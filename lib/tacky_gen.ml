open Batteries
open Cnums

module Ast = struct
  include Ast.Typed
end

module T = struct
  include Tacky
end

let break_label id = "break." ^ id
let continue_label id = "continue." ^ id

let create_tmp t =
  let name = Unique_ids.make_temporary () in
  Symbols.add_automatic_var name ~t;
  name

let mk_const t i =
  let as_int = Const.ConstInt (Int32.of_int i) in
  Const_convert.const_convert t as_int

let mk_ast_const t i = Ast.{ e = Constant (mk_const t i); t }

let get_ptr_scale = function
  | Types.Pointer referenced -> Type_utils.get_size referenced
  | t ->
      failwith
        ("Internal error: tried to get scale of non-pointer type: "
        ^ Types.show t) [@coverage off]

let convert_op = function
  | Ast.Complement -> T.Complement
  | Ast.Negate -> T.Negate
  | Ast.Not -> T.Not
  | Incr | Decr -> failwith "internal error: shouldn't handle these here"

let convert_binop = function
  | Ast.Add -> T.Add
  | Ast.Subtract -> T.Subtract
  | Ast.Multiply -> T.Multiply
  | Ast.Divide -> T.Divide
  | Ast.Mod -> T.Mod
  | Ast.BitshiftLeft -> T.BitshiftLeft
  | Ast.BitshiftRight -> T.BitshiftRight
  | Ast.BitwiseAnd -> T.BitwiseAnd
  | Ast.BitwiseOr -> T.BitwiseOr
  | Ast.BitwiseXor -> T.BitwiseXor
  | Ast.Equal -> T.Equal
  | Ast.NotEqual -> T.NotEqual
  | Ast.LessThan -> T.LessThan
  | Ast.LessOrEqual -> T.LessOrEqual
  | Ast.GreaterThan -> T.GreaterThan
  | Ast.GreaterOrEqual -> T.GreaterOrEqual
  | And | Or ->
      failwith "Internal error, cannot convert these directly to TACKY binops"
      [@coverage off]

(* an expression result that may or may not be lvalue converted *)
type exp_result =
  | PlainOperand of T.tacky_val
  | DereferencedPointer of T.tacky_val

(* return list of instructions to evaluate expression and resulting exp_result value as a pair *)
let rec emit_tacky_for_exp Ast.{ e; t } =
  match e with
  (* don't need any instructions to calculate a constant or variable  *)
  | Ast.Constant c -> ([], PlainOperand (T.Constant c))
  | Ast.Var v -> ([], PlainOperand (T.Var v))
  | Ast.String s ->
      let str_id = Symbols.add_string s in
      ([], PlainOperand (T.Var str_id))
  | Ast.Unary (Incr, v) ->
      let const_t = if Type_utils.is_pointer t then Types.Long else t in
      emit_compound_expression ~op:Ast.Add ~lhs:v ~rhs:(mk_ast_const const_t 1)
        ~result_t:t
  | Ast.Unary (Decr, v) ->
      let const_t = if Type_utils.is_pointer t then Types.Long else t in
      emit_compound_expression ~op:Ast.Subtract ~lhs:v
        ~rhs:(mk_ast_const const_t 1) ~result_t:t
  | Ast.Cast { target_type; e } -> emit_cast_expression target_type e
  | Ast.Unary (op, inner) -> emit_unary_expression t op inner
  | Ast.Binary (And, e1, e2) -> emit_and_expression e1 e2
  | Ast.Binary (Or, e1, e2) -> emit_or_expression e1 e2
  | Ast.Binary (Add, e1, e2) when Type_utils.is_pointer t ->
      emit_pointer_addition t e1 e2
  | Ast.Binary (Subtract, ptr, index) when Type_utils.is_pointer t ->
      emit_subtraction_from_pointer t ptr index
  | Ast.Binary (Subtract, e1, e2) when Type_utils.is_pointer e1.t ->
      (* at least one operand is pointer but result isn't, must be subtracting one pointer from another *)
      emit_pointer_diff t e1 e2
  | Ast.Binary (op, e1, e2) -> emit_binary_expression t op e1 e2
  | Ast.Assignment (lhs, rhs) -> emit_assignment lhs rhs
  | Ast.CompoundAssignment { op; lhs; rhs; result_t } ->
      emit_compound_expression ~op ~lhs ~rhs ~result_t
  | Ast.PostfixDecr v -> emit_postfix Ast.Subtract v
  | PostfixIncr v -> emit_postfix Ast.Add v
  | Ast.Conditional { condition; then_result; else_result } ->
      emit_conditional_expression t condition then_result else_result
  | Ast.FunCall { f; args } -> emit_fun_call t f args
  | Ast.Dereference inner -> emit_dereference inner
  | Ast.AddrOf inner -> emit_addr_of t inner
  | Ast.Subscript { ptr; index } -> emit_subscript t ptr index

(* helper functions for individual expression *)
and emit_unary_expression t op inner =
  let eval_inner, v = emit_tacky_and_convert inner in
  (* define a temporary variable to hold result of this expression *)
  let dst_name = create_tmp t in
  let dst = T.Var dst_name in
  let tacky_op = convert_op op in
  let instructions = eval_inner @ [ T.Unary { op = tacky_op; src = v; dst } ] in
  (instructions, PlainOperand dst)

and emit_postfix op inner =
  (* If LHS is a variable:
   *   dst = lhs
   *   lhs = lhs <op> 1
   * If LHS is a pointer:
   *   dst = load(ptr)
   *   tmp = dst <op> 1
   *   store(tmp, ptr)
   *
   * If LHS has a pointer type, we implement <op> with AddPtr
   * otherwise with Binary instruction
   *)
  (* define var for result - i.e. value of lval BEFORE incr or decr *)
  let dst = T.Var (create_tmp inner.t) in
  (* evaluate inner to get exp_result *)
  let instrs, lval = emit_tacky_for_exp inner in
  (* Helper to construct Binary or AddPtr instruction from operands, depending on type
   * Note that dst is destination of this instruction rather than the whole expression
   * (i.e. it's the lvalue we're updating, or a temporary we'll then store to that lvalue
   *)
  let do_op op src dst =
    let index =
      match op with
      | Ast.Add -> T.Constant (mk_const Long 1)
      | Subtract -> T.Constant (mk_const Long (-1))
      | _ -> failwith "Internal error"
    in
    if Type_utils.is_pointer inner.t then
      T.AddPtr { ptr = src; index; scale = get_ptr_scale inner.t; dst }
    else
      let one = T.Constant (mk_const inner.t 1) in
      T.Binary { op = convert_binop op; src1 = src; src2 = one; dst }
  in

  (* copy result to dst and perform incr or decr *)
  let oper_instrs =
    match lval with
    | PlainOperand (Var v) ->
        (* dst = v
         * v = v + 1 // or v - 1
         *)
        [ T.Copy { src = Var v; dst }; do_op op (Var v) (Var v) ]
    | DereferencedPointer p ->
        (* dst = Load(p)
         * tmp = dst + 1 // or dst - 1
         * store(tmp, p)
         *)
        let tmp = T.Var (create_tmp inner.t) in
        [
          T.Load { src_ptr = p; dst };
          do_op op dst tmp;
          T.Store { src = tmp; dst_ptr = p };
        ]
    | PlainOperand _ -> failwith "Invalid lvalue in postfix incr/decr"
  in
  (instrs @ oper_instrs, PlainOperand dst)

and emit_cast_expression target_type inner =
  let eval_inner, result = emit_tacky_and_convert inner in
  let src_type = Type_utils.get_type inner in
  if src_type = target_type then (eval_inner, PlainOperand result)
  else
    let dst_name = create_tmp target_type in
    let dst = T.Var dst_name in
    let cast_instruction =
      get_cast_instruction result dst src_type target_type
    in
    (eval_inner @ [ cast_instruction ], PlainOperand dst)

and get_cast_instruction src dst src_t dst_t =
  match (dst_t, src_t) with
  | Double, _ ->
      if Type_utils.is_signed src_t then T.IntToDouble { src; dst }
      else T.UIntToDouble { src; dst }
  | _, Double ->
      if Type_utils.is_signed dst_t then T.DoubleToInt { src; dst }
      else T.DoubleToUInt { src; dst }
  | _ ->
      (* Cast between int types. NOTE: assumes src and dst have different types *)
      if Type_utils.get_size dst_t = Type_utils.get_size src_t then
        T.Copy { src; dst }
      else if Type_utils.get_size dst_t < Type_utils.get_size src_t then
        Truncate { src; dst }
      else if Type_utils.is_signed src_t then T.SignExtend { src; dst }
      else T.ZeroExtend { src; dst }

and emit_pointer_addition t e1 e2 =
  let eval_v1, v1 = emit_tacky_and_convert e1 in
  let eval_v2, v2 = emit_tacky_and_convert e2 in
  let dst_name = create_tmp t in
  let dst = T.Var dst_name in
  let ptr, index = if t = e1.t then (v1, v2) else (v2, v1) in
  let scale = get_ptr_scale t in
  let instructions =
    eval_v1 @ eval_v2 @ [ AddPtr { ptr; index; scale; dst } ]
  in
  (instructions, PlainOperand dst)

and emit_subscript t e1 e2 =
  let instructions, result = emit_pointer_addition (Types.Pointer t) e1 e2 in
  match result with
  | PlainOperand dst -> (instructions, DereferencedPointer dst)
  | _ ->
      failwith
        "Internal error: expected result of pointer addition to be lvalue \
         converted" [@coverage off]

and emit_subtraction_from_pointer t ptr_e idx_e =
  let eval_v1, ptr = emit_tacky_and_convert ptr_e in
  let eval_v2, index = emit_tacky_and_convert idx_e in
  let dst_name = create_tmp t in
  let dst = T.Var dst_name in
  let negated_index = T.Var (create_tmp Types.Long) in
  let scale = get_ptr_scale t in
  ( eval_v1
    @ eval_v2
    @ [
        Unary { op = Negate; src = index; dst = negated_index };
        AddPtr { ptr; index = negated_index; scale; dst };
      ],
    PlainOperand dst )

and emit_pointer_diff t e1 e2 =
  let eval_v1, v1 = emit_tacky_and_convert e1 in
  let eval_v2, v2 = emit_tacky_and_convert e2 in
  let ptr_diff = T.Var (create_tmp Types.Long) in
  let dst_name = create_tmp t in
  let dst = T.Var dst_name in
  let scale =
    T.Constant (Const.ConstLong ((Int64.of_int % get_ptr_scale) e1.t))
  in
  ( eval_v1
    @ eval_v2
    @ [
        Binary { op = Subtract; src1 = v1; src2 = v2; dst = ptr_diff };
        Binary { op = Divide; src1 = ptr_diff; src2 = scale; dst };
      ],
    PlainOperand dst )

and emit_binary_expression t op e1 e2 =
  let eval_v1, v1 = emit_tacky_and_convert e1 in
  let eval_v2, v2 = emit_tacky_and_convert e2 in
  let dst_name = create_tmp t in
  let dst = T.Var dst_name in
  let tacky_op = convert_binop op in
  let instructions =
    eval_v1
    @ eval_v2
    @ [ T.Binary { op = tacky_op; src1 = v1; src2 = v2; dst } ]
  in
  (instructions, PlainOperand dst)

and emit_compound_expression ~op ~lhs ~rhs ~result_t =
  (*
  if LHS is var w/ same type as result:
    lhs = lhs <op> rval
  if LHS is a var w/ different type:
    tmp = cast(lhs)
    tmp = tmp <op> rval
    lhs = cast(tmp)
  if LHS is pointer w/ same type:
    tmp = load(lhs_ptr)
    tmp = tmp <op> rval
    store(tmp, lhs_ptr)
  if LHS is pointer w/ diff type:
    tmp = load(lhs_ptr)
    tmp2 = cast(tmp)
    tmp2 = tmp2 <op> rval
    tmp = cast(tmp2)
    store(tmp, rhs_ptr)
   *)
  let lhs_t = lhs.t in
  (* evaluate LHS *)
  let eval_lhs, lhs = emit_tacky_for_exp lhs in
  (* evaluate RHS - type checker already added conversion to common type if one is needed *)
  let eval_rhs, rhs = emit_tacky_and_convert rhs in
  (* If LHS is a variable, we can update it directly. If it's a dereferenced pointer,
   * we need to load it into a temporary variable, operate on that, and then store it
   *)
  let dst, load_instr, store_instr =
    match lhs with
    | PlainOperand dst -> (dst, [], [])
    | DereferencedPointer p ->
        let dst = T.Var (create_tmp lhs_t) in
        ( dst,
          [ T.Load { src_ptr = p; dst } ],
          [ T.Store { src = dst; dst_ptr = p } ] )
  in
  (* If LHS type and result type are the same, we can operate on dst directly. Otherwise
   * we need to cast dst to correct type before operation, then cast result back and assign
   * to dst.
   *)
  let result_var, cast_to, cast_from =
    if lhs_t = result_t then (dst, [], [])
    else
      let tmp = T.Var (create_tmp result_t) in
      let cast_lhs_to_tmp = get_cast_instruction dst tmp lhs_t result_t in
      let cast_tmp_to_lhs = get_cast_instruction tmp dst result_t lhs_t in
      (tmp, [ cast_lhs_to_tmp ], [ cast_tmp_to_lhs ])
  in

  let do_operation =
    if Type_utils.is_pointer result_t then
      let scale = get_ptr_scale result_t in
      match op with
      | Add ->
          [
            T.AddPtr { ptr = result_var; index = rhs; scale; dst = result_var };
          ]
      | Subtract ->
          let negated_index = T.Var (create_tmp Types.Long) in
          [
            Unary { op = Negate; src = rhs; dst = negated_index };
            AddPtr
              {
                ptr = result_var;
                index = negated_index;
                scale;
                dst = result_var;
              };
          ]
      | _ -> failwith "Internal error in compound assignment"
    else
      [
        T.Binary
          {
            op = convert_binop op;
            src1 = result_var;
            src2 = rhs;
            dst = result_var;
          };
      ]
  in
  let instructions =
    eval_lhs
    @ eval_rhs
    @ load_instr
    @ cast_to
    @ do_operation
    @ cast_from
    @ store_instr
  in
  (instructions, PlainOperand dst)

and emit_and_expression e1 e2 =
  let eval_v1, v1 = emit_tacky_and_convert e1 in
  let eval_v2, v2 = emit_tacky_and_convert e2 in
  let false_label = Unique_ids.make_label "and_false" in
  let end_label = Unique_ids.make_label "and_end" in
  let dst_name = create_tmp Types.Int in
  let dst = T.Var dst_name in
  let instructions =
    eval_v1
    @ [ T.JumpIfZero (v1, false_label) ]
    @ eval_v2
    @ [
        T.JumpIfZero (v2, false_label);
        T.Copy { src = Constant Const.int_one; dst };
        T.Jump end_label;
        T.Label false_label;
        T.Copy { src = Constant Const.int_zero; dst };
        T.Label end_label;
      ]
  in
  (instructions, PlainOperand dst)

and emit_or_expression e1 e2 =
  let eval_v1, v1 = emit_tacky_and_convert e1 in
  let eval_v2, v2 = emit_tacky_and_convert e2 in
  let true_label = Unique_ids.make_label "or_true" in
  let end_label = Unique_ids.make_label "or_end" in
  let dst_name = create_tmp Types.Int in
  let dst = T.Var dst_name in
  let instructions =
    eval_v1
    @ (T.JumpIfNotZero (v1, true_label) :: eval_v2)
    @ T.JumpIfNotZero (v2, true_label)
      :: T.Copy { src = Constant Const.int_zero; dst }
      :: T.Jump end_label
      :: T.Label true_label
      :: T.Copy { src = Constant Const.int_one; dst }
      :: [ T.Label end_label ]
  in
  (instructions, PlainOperand dst)

and emit_assignment lhs rhs =
  let lhs_instructions, lval = emit_tacky_for_exp lhs in
  let rhs_instructions, rval = emit_tacky_and_convert rhs in
  let instructions = lhs_instructions @ rhs_instructions in
  match lval with
  | PlainOperand o -> (instructions @ [ T.Copy { src = rval; dst = o } ], lval)
  | DereferencedPointer ptr ->
      ( instructions @ [ T.Store { src = rval; dst_ptr = ptr } ],
        PlainOperand rval )

and emit_conditional_expression t condition e1 e2 =
  let eval_cond, c = emit_tacky_and_convert condition in
  let eval_v1, v1 = emit_tacky_and_convert e1 in
  let eval_v2, v2 = emit_tacky_and_convert e2 in
  let e2_label = Unique_ids.make_label "conditional_else" in
  let end_label = Unique_ids.make_label "conditional_end" in
  let dst_name = create_tmp t in
  let dst = T.Var dst_name in
  let instructions =
    eval_cond
    @ (T.JumpIfZero (c, e2_label) :: eval_v1)
    @ T.Copy { src = v1; dst }
      :: T.Jump end_label
      :: T.Label e2_label
      :: eval_v2
    @ (T.Copy { src = v2; dst } :: [ T.Label end_label ])
  in
  (instructions, PlainOperand dst)

and emit_fun_call t f args =
  let dst_name = create_tmp t in
  let dst = T.Var dst_name in
  let arg_instructions, arg_vals =
    List.split (List.map emit_tacky_and_convert args)
  in
  let instructions =
    List.flatten arg_instructions @ [ T.FunCall { f; args = arg_vals; dst } ]
  in
  (instructions, PlainOperand dst)

and emit_dereference inner =
  let instructions, result = emit_tacky_and_convert inner in
  (instructions, DereferencedPointer result)

and emit_addr_of t inner =
  let instructions, result = emit_tacky_for_exp inner in
  match result with
  | PlainOperand o ->
      let dst = T.Var (create_tmp t) in
      (instructions @ [ T.GetAddress { src = o; dst } ], PlainOperand dst)
  | DereferencedPointer ptr -> (instructions, PlainOperand ptr)

and emit_tacky_and_convert e =
  let instructions, result = emit_tacky_for_exp e in
  match result with
  | PlainOperand o -> (instructions, o)
  | DereferencedPointer ptr ->
      let dst = T.Var (create_tmp e.t) in
      (instructions @ [ T.Load { src_ptr = ptr; dst } ], dst)

let rec emit_string_init dst offset s =
  let len = Bytes.length s in
  if len = 0 then []
  else if len >= 8 then
    let l = Bytes.get_int64_le s 0 in
    let instr =
      Tacky.CopyToOffset { src = Constant (ConstLong l); dst; offset }
    in
    let rest = Bytes.sub s 8 (len - 8) in
    instr :: emit_string_init dst (offset + 8) rest
  else if len >= 4 then
    let i = Bytes.get_int32_le s 0 in
    let instr =
      Tacky.CopyToOffset { src = Constant (ConstInt i); dst; offset }
    in
    let rest = Bytes.sub s 4 (len - 4) in
    instr :: emit_string_init dst (offset + 4) rest
  else
    let c = Int8.of_int (Bytes.get_int8 s 0) in
    let instr =
      Tacky.CopyToOffset { src = Constant (ConstChar c); dst; offset }
    in
    let rest = Bytes.sub s 1 (len - 1) in
    instr :: emit_string_init dst (offset + 1) rest

let rec emit_compound_init name offset = function
  | Ast.SingleInit { e = String s; t = Array { size; _ } } ->
      let str_bytes = Bytes.of_string s in
      let padding_bytes = Bytes.make (size - String.length s) (Char.chr 0) in
      emit_string_init name offset (Bytes.cat str_bytes padding_bytes)
  | Ast.SingleInit e ->
      let eval_init, v = emit_tacky_and_convert e in
      eval_init @ [ CopyToOffset { src = v; dst = name; offset } ]
  | Ast.CompoundInit (Array { elem_type; _ }, inits) ->
      let handle_init idx elem_init =
        let new_offset = offset + (idx * Type_utils.get_size elem_type) in
        emit_compound_init name new_offset elem_init
      in
      List.flatten (List.mapi handle_init inits)
  | Ast.CompoundInit (_, _) ->
      failwith "Internal error: compound init has non-array type!"
      [@coverage off]

let rec emit_tacky_for_statement = function
  | Ast.Return e ->
      let eval_exp, v = emit_tacky_and_convert e in
      eval_exp @ [ T.Return v ]
  | Ast.Expression e ->
      (* evaluate expression but don't use result *)
      let eval_exp, _ = emit_tacky_for_exp e in
      eval_exp
  | Ast.If { condition; then_clause; else_clause } ->
      emit_tacky_for_if_statement condition then_clause else_clause
  | Ast.LabeledStatement (lbl, stmt) ->
      T.Label lbl :: emit_tacky_for_statement stmt
  | Ast.Goto lbl -> [ T.Jump lbl ]
  | Ast.Compound (Block items) ->
      List.concat_map emit_tacky_for_block_item items
  | Ast.Break id -> [ T.Jump (break_label id) ]
  | Ast.Continue id -> [ T.Jump (continue_label id) ]
  | Ast.DoWhile { body; condition; id } ->
      emit_tacky_for_do_loop body condition id
  | Ast.While { condition; body; id } ->
      emit_tacky_for_while_loop condition body id
  | Ast.For { init; condition; post; body; id } ->
      emit_tacky_for_for_loop init condition post body id
  | Case (_, stmt, id) -> T.Label id :: emit_tacky_for_statement stmt
  | Default (stmt, id) -> T.Label id :: emit_tacky_for_statement stmt
  | Switch { control; body; id; cases } ->
      emit_tacky_for_switch control body id cases
  | Ast.Null -> []

and emit_tacky_for_block_item = function
  | Ast.S s -> emit_tacky_for_statement s
  | Ast.D d -> emit_local_declaration d

and emit_local_declaration = function
  | Ast.VarDecl { storage_class = Some _; _ } -> []
  | Ast.VarDecl vd -> emit_var_declaration vd
  | Ast.FunDecl _ -> []

and emit_var_declaration = function
  | {
      name;
      init =
        Some (Ast.SingleInit { e = Ast.String _; t = Array _ } as string_init);
      _;
    } ->
      emit_compound_init name 0 string_init
  | { name; init = Some (Ast.SingleInit e); var_type; _ } ->
      (* treat declaration with initializer like an assignment expression *)
      let eval_assignment, _assign_result =
        emit_assignment { e = Ast.Var name; t = var_type } e
      in
      eval_assignment
  | { name; init = Some compound_init; _ } ->
      emit_compound_init name 0 compound_init
  | { init = None; _ } ->
      (* don't generate instructions for declaration without initializer *) []

and emit_tacky_for_if_statement condition then_clause = function
  | None ->
      (* no else clause *)
      let end_label = Unique_ids.make_label "if_end" in
      let eval_condition, c = emit_tacky_and_convert condition in
      eval_condition
      @ (T.JumpIfZero (c, end_label) :: emit_tacky_for_statement then_clause)
      @ [ T.Label end_label ]
  | Some else_clause ->
      let else_label = Unique_ids.make_label "else" in
      let end_label = Unique_ids.make_label "" in
      let eval_condition, c = emit_tacky_and_convert condition in
      eval_condition
      @ (T.JumpIfZero (c, else_label) :: emit_tacky_for_statement then_clause)
      @ T.Jump end_label
        :: T.Label else_label
        :: emit_tacky_for_statement else_clause
      @ [ T.Label end_label ]

and emit_tacky_for_do_loop body condition id =
  let start_label = Unique_ids.make_label "do_loop_start" in
  let cont_label = continue_label id in
  let br_label = break_label id in
  let eval_condition, c = emit_tacky_and_convert condition in
  (T.Label start_label :: emit_tacky_for_statement body)
  @ (T.Label cont_label :: eval_condition)
  @ [ T.JumpIfNotZero (c, start_label); T.Label br_label ]

and emit_tacky_for_while_loop condition body id =
  let cont_label = continue_label id in
  let br_label = break_label id in
  let eval_condition, c = emit_tacky_and_convert condition in
  (T.Label cont_label :: eval_condition)
  @ (T.JumpIfZero (c, br_label) :: emit_tacky_for_statement body)
  @ [ T.Jump cont_label; T.Label br_label ]

(* c = <evaluate condition>
 * cmp = c == case1
 * JumpIfNotZero(cmp, label1)
 * cmp = c == case2
 * JumpIfNotZero(cmp, label2)
 * ...
 * Jump(default_label) // if there's a default
 * Jump(break_label) // so we jump over body if there's no matching case or default
 * <statement>
 * Label(break_label)
 *
 * Could improve on this by adding a JumpIfEqual(v1, v2, label) TACKY instruction,
 * but I want to avoid changing TACKY here
 *)
and emit_tacky_for_switch control body id cases =
  let br_label = break_label id in
  let eval_control, c = emit_tacky_and_convert control in
  let cmp_result = T.Var (create_tmp control.t) in
  let emit_tacky_for_case (key, id) =
    match key with
    (* case statement - emit tacky now *)
    | Some i ->
        [
          T.Binary { op = Equal; src1 = Constant i; src2 = c; dst = cmp_result };
          JumpIfNotZero (cmp_result, id);
        ]
        (* default statement - handle later*)
    | None -> []
  in
  let jump_to_cases = List.concat_map emit_tacky_for_case cases in
  let default_tacky =
    if List.mem_assoc None cases then
      let default_id = List.assoc None cases in
      [ T.Jump default_id ]
    else []
  in
  eval_control
  @ jump_to_cases
  @ default_tacky
  @ [ T.Jump br_label ]
  @ emit_tacky_for_statement body
  @ [ Label br_label ]

and emit_tacky_for_for_loop init condition post body id =
  (* generate some labels *)
  let start_label = Unique_ids.make_label "for_start" in
  let cont_label = continue_label id in
  let br_label = break_label id in
  let for_init_instructions =
    match init with
    | InitDecl d -> emit_var_declaration d
    | InitExp e -> (
        match Option.map emit_tacky_for_exp e with
        | Some (instrs, _) -> instrs
        | None -> [])
  in
  let test_condition =
    match Option.map emit_tacky_and_convert condition with
    | Some (instrs, v) -> instrs @ [ T.JumpIfZero (v, br_label) ]
    | None -> []
  in
  let post_instructions =
    match Option.map emit_tacky_for_exp post with
    | Some (instrs, _post_result) -> instrs
    | None -> []
  in
  for_init_instructions
  @ (T.Label start_label :: test_condition)
  @ emit_tacky_for_statement body
  @ (T.Label cont_label :: post_instructions)
  @ [ T.Jump start_label; T.Label br_label ]

let emit_fun_declaration = function
  | Ast.FunDecl { name; params; body = Some (Block block_items); _ } ->
      (* Use the tacky_instructions queue to accumulate instructions as we go *)
      let global = Symbols.is_global name in
      let body_instructions =
        List.concat_map emit_tacky_for_block_item block_items
      in
      let extra_return = T.(Return (Constant Const.int_zero)) in
      Some
        (T.Function
           { name; global; params; body = body_instructions @ [ extra_return ] })
  | _ -> None

let convert_symbols_to_tacky all_symbols =
  let to_var (name, entry) =
    match entry.Symbols.attrs with
    | Symbols.StaticAttr { init; global } -> (
        match init with
        | Initial i ->
            Some (T.StaticVariable { name; t = entry.t; global; init = i })
        | Tentative ->
            Some
              (T.StaticVariable
                 { name; t = entry.t; global; init = Initializers.zero entry.t })
        | NoInitializer -> None)
    | Symbols.ConstAttr init ->
        Some (T.StaticConstant { name; t = entry.t; init })
    | _ -> None
  in
  List.filter_map to_var all_symbols

let gen (Ast.Program decls) =
  let tacky_fn_defs = List.filter_map emit_fun_declaration decls in
  let tacky_var_defs = convert_symbols_to_tacky (Symbols.bindings ()) in
  Tacky.Program (tacky_var_defs @ tacky_fn_defs)
