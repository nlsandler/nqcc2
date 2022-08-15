open Batteries

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

let convert_op = function
  | Ast.Complement -> T.Complement
  | Ast.Negate -> T.Negate
  | Ast.Not -> T.Not

let convert_binop = function
  | Ast.Add -> T.Add
  | Ast.Subtract -> T.Subtract
  | Ast.Multiply -> T.Multiply
  | Ast.Divide -> T.Divide
  | Ast.Mod -> T.Mod
  | Ast.Equal -> T.Equal
  | Ast.NotEqual -> T.NotEqual
  | Ast.LessThan -> T.LessThan
  | Ast.LessOrEqual -> T.LessOrEqual
  | Ast.GreaterThan -> T.GreaterThan
  | Ast.GreaterOrEqual -> T.GreaterOrEqual
  | And | Or ->
      failwith "Internal error, cannot convert these directly to TACKY binops"
      [@coverage off]

(* return list of instructions to evaluate expression a and resulting TACKY value as a pair *)
let rec emit_tacky_for_exp Ast.{ e; t } =
  match e with
  (* don't need any instructions to calculate a constant or variable  *)
  | Ast.Constant c -> ([], T.Constant c)
  | Ast.Var v -> ([], T.Var v)
  | Ast.Cast { target_type; e } -> emit_cast_expression target_type e
  | Ast.Unary (op, inner) -> emit_unary_expression t op inner
  | Ast.Binary (And, e1, e2) -> emit_and_expression e1 e2
  | Ast.Binary (Or, e1, e2) -> emit_or_expression e1 e2
  | Ast.Binary (op, e1, e2) -> emit_binary_expression t op e1 e2
  | Ast.Assignment ({ e = Var v; _ }, rhs) -> emit_assignment v rhs
  | Ast.Assignment _ -> failwith "Internal error: bad lvalue" [@coverage off]
  | Ast.Conditional { condition; then_result; else_result } ->
      emit_conditional_expression t condition then_result else_result
  | Ast.FunCall { f; args } -> emit_fun_call t f args

(* helper functions for individual expression *)
and emit_unary_expression t op inner =
  let eval_inner, v = emit_tacky_for_exp inner in
  (* define a temporary variable to hold result of this expression *)
  let dst_name = create_tmp t in
  let dst = T.Var dst_name in
  let tacky_op = convert_op op in
  let instructions = eval_inner @ [ T.Unary { op = tacky_op; src = v; dst } ] in
  (instructions, dst)

and emit_cast_expression target_type inner =
  let eval_inner, result = emit_tacky_for_exp inner in
  if Type_utils.get_type inner = target_type then (eval_inner, result)
  else
    let dst_name = create_tmp target_type in
    let dst = T.Var dst_name in
    let cast_instruction =
      match target_type with
      | Types.Long -> T.SignExtend { src = result; dst }
      | Types.Int -> T.Truncate { src = result; dst }
      | _ -> failwith "Internal error: cast to function type" [@coverage off]
    in
    let instructions = eval_inner @ [ cast_instruction ] in
    (instructions, dst)

and emit_binary_expression t op e1 e2 =
  let eval_v1, v1 = emit_tacky_for_exp e1 in
  let eval_v2, v2 = emit_tacky_for_exp e2 in
  let dst_name = create_tmp t in
  let dst = T.Var dst_name in
  let tacky_op = convert_binop op in
  let instructions =
    eval_v1
    @ eval_v2
    @ [ T.Binary { op = tacky_op; src1 = v1; src2 = v2; dst } ]
  in
  (instructions, dst)

and emit_and_expression e1 e2 =
  let eval_v1, v1 = emit_tacky_for_exp e1 in
  let eval_v2, v2 = emit_tacky_for_exp e2 in
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
  (instructions, dst)

and emit_or_expression e1 e2 =
  let eval_v1, v1 = emit_tacky_for_exp e1 in
  let eval_v2, v2 = emit_tacky_for_exp e2 in
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
  (instructions, dst)

and emit_assignment v rhs =
  let rhs_instructions, rhs_result = emit_tacky_for_exp rhs in
  let instructions =
    rhs_instructions @ [ T.Copy { src = rhs_result; dst = Var v } ]
  in
  (instructions, Var v)

and emit_conditional_expression t condition e1 e2 =
  let eval_cond, c = emit_tacky_for_exp condition in
  let eval_v1, v1 = emit_tacky_for_exp e1 in
  let eval_v2, v2 = emit_tacky_for_exp e2 in
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
  (instructions, dst)

and emit_fun_call t f args =
  let dst_name = create_tmp t in
  let dst = T.Var dst_name in
  let arg_instructions, arg_vals =
    List.split (List.map emit_tacky_for_exp args)
  in
  let instructions =
    List.flatten arg_instructions @ [ T.FunCall { f; args = arg_vals; dst } ]
  in
  (instructions, dst)

let rec emit_tacky_for_statement = function
  | Ast.Return e ->
      let eval_exp, v = emit_tacky_for_exp e in
      eval_exp @ [ T.Return v ]
  | Ast.Expression e ->
      (* evaluate expression but don't use result *)
      let eval_exp, _exp_result = emit_tacky_for_exp e in
      eval_exp
  | Ast.If { condition; then_clause; else_clause } ->
      emit_tacky_for_if_statement condition then_clause else_clause
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
  | Ast.Null -> []

and emit_tacky_for_block_item = function
  | Ast.S s -> emit_tacky_for_statement s
  | Ast.D d -> emit_local_declaration d

and emit_local_declaration = function
  | Ast.VarDecl { storage_class = Some _; _ } -> []
  | Ast.VarDecl vd -> emit_var_declaration vd
  | Ast.FunDecl _ -> []

and emit_var_declaration = function
  | { name; init = Some e; _ } ->
      (* treat declaration with initializer like an assignment expression *)
      let eval_assignment, _assign_result = emit_assignment name e in
      eval_assignment
  | { init = None; _ } ->
      (* don't generate instructions for declaration without initializer *) []

and emit_tacky_for_if_statement condition then_clause = function
  | None ->
      (* no else clause *)
      let end_label = Unique_ids.make_label "if_end" in
      let eval_condition, c = emit_tacky_for_exp condition in
      eval_condition
      @ (T.JumpIfZero (c, end_label) :: emit_tacky_for_statement then_clause)
      @ [ T.Label end_label ]
  | Some else_clause ->
      let else_label = Unique_ids.make_label "else" in
      let end_label = Unique_ids.make_label "" in
      let eval_condition, c = emit_tacky_for_exp condition in
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
  let eval_condition, c = emit_tacky_for_exp condition in
  (T.Label start_label :: emit_tacky_for_statement body)
  @ (T.Label cont_label :: eval_condition)
  @ [ T.JumpIfNotZero (c, start_label); T.Label br_label ]

and emit_tacky_for_while_loop condition body id =
  let cont_label = continue_label id in
  let br_label = break_label id in
  let eval_condition, c = emit_tacky_for_exp condition in
  (T.Label cont_label :: eval_condition)
  @ (T.JumpIfZero (c, br_label) :: emit_tacky_for_statement body)
  @ [ T.Jump cont_label; T.Label br_label ]

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
    match Option.map emit_tacky_for_exp condition with
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
    | _ -> None
  in
  List.filter_map to_var all_symbols

let gen (Ast.Program decls) =
  let tacky_fn_defs = List.filter_map emit_fun_declaration decls in
  let tacky_var_defs = convert_symbols_to_tacky (Symbols.bindings ()) in
  Tacky.Program (tacky_var_defs @ tacky_fn_defs)
