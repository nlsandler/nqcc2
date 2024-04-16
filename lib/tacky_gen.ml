open Batteries

module T = struct
  include Tacky
end

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

let emit_decr op v =
  let dst = T.Var (Unique_ids.make_temporary ()) in
  let instrs =
    [
      T.Copy { src = Var v; dst };
      T.Binary
        { op = convert_binop op; src1 = Var v; src2 = Constant 1; dst = Var v };
    ]
  in
  (instrs, dst)

(* return list of instructions to evaluate expression a and resulting TACKY value as a pair *)
let rec emit_tacky_for_exp = function
  (* don't need any instructions to calculate a constant or variable  *)
  | Ast.Constant c -> ([], T.Constant c)
  | Ast.Var v -> ([], T.Var v)
  | Ast.Unary (Incr, Var v) ->
      emit_compound_expression Ast.Add v (Ast.Constant 1)
  | Ast.Unary (Decr, Var v) ->
      emit_compound_expression Ast.Subtract v (Ast.Constant 1)
  | Ast.Unary (op, inner) -> emit_unary_expression op inner
  | Ast.Binary (And, e1, e2) -> emit_and_expression e1 e2
  | Ast.Binary (Or, e1, e2) -> emit_or_expression e1 e2
  | Ast.Binary (op, e1, e2) -> emit_binary_expression op e1 e2
  | Ast.Assignment (Var v, rhs) ->
      let rhs_instructions, rhs_result = emit_tacky_for_exp rhs in
      let instructions =
        rhs_instructions @ [ T.Copy { src = rhs_result; dst = Var v } ]
      in
      (instructions, Var v)
  | Ast.CompoundAssignment (op, Var v, rhs) -> emit_compound_expression op v rhs
  | Ast.PostfixDecr (Var v) -> emit_decr Ast.Subtract v
  | PostfixIncr (Var v) -> emit_decr Ast.Add v
  | Ast.Conditional { condition; then_result; else_result } ->
      emit_conditional_expression condition then_result else_result
  | Ast.Assignment _ | Ast.CompoundAssignment _ | Ast.PostfixDecr _
  | Ast.PostfixIncr _ ->
      failwith "Internal error: bad lvalue" [@coverage off]

(* helper functions for individual expression *)
and emit_unary_expression op inner =
  let eval_inner, v = emit_tacky_for_exp inner in
  (* define a temporary variable to hold result of this expression *)
  let dst_name = Unique_ids.make_temporary () in
  let dst = T.Var dst_name in
  let tacky_op = convert_op op in
  let instructions = eval_inner @ [ T.Unary { op = tacky_op; src = v; dst } ] in
  (instructions, dst)

and emit_binary_expression op e1 e2 =
  let eval_v1, v1 = emit_tacky_for_exp e1 in
  let eval_v2, v2 = emit_tacky_for_exp e2 in
  let dst_name = Unique_ids.make_temporary () in
  let dst = T.Var dst_name in
  let tacky_op = convert_binop op in
  let instructions =
    eval_v1
    @ eval_v2
    @ [ T.Binary { op = tacky_op; src1 = v1; src2 = v2; dst } ]
  in
  (instructions, dst)

and emit_compound_expression op v rhs =
  let eval_rhs, rhs = emit_tacky_for_exp rhs in
  let dst = T.Var v in
  let tacky_op = convert_binop op in
  let instructions =
    eval_rhs @ [ T.Binary { op = tacky_op; src1 = dst; src2 = rhs; dst } ]
  in
  (instructions, dst)

and emit_and_expression e1 e2 =
  let eval_v1, v1 = emit_tacky_for_exp e1 in
  let eval_v2, v2 = emit_tacky_for_exp e2 in
  let false_label = Unique_ids.make_label "and_false" in
  let end_label = Unique_ids.make_label "and_end" in
  let dst_name = Unique_ids.make_temporary () in
  let dst = T.Var dst_name in
  let instructions =
    eval_v1
    @ [ T.JumpIfZero (v1, false_label) ]
    @ eval_v2
    @ [
        T.JumpIfZero (v2, false_label);
        T.Copy { src = Constant 1; dst };
        T.Jump end_label;
        T.Label false_label;
        T.Copy { src = Constant 0; dst };
        T.Label end_label;
      ]
  in
  (instructions, dst)

and emit_or_expression e1 e2 =
  let eval_v1, v1 = emit_tacky_for_exp e1 in
  let eval_v2, v2 = emit_tacky_for_exp e2 in
  let true_label = Unique_ids.make_label "or_true" in
  let end_label = Unique_ids.make_label "or_end" in
  let dst_name = Unique_ids.make_temporary () in
  let dst = T.Var dst_name in
  let instructions =
    eval_v1
    @ (T.JumpIfNotZero (v1, true_label) :: eval_v2)
    @ T.JumpIfNotZero (v2, true_label)
      :: T.Copy { src = Constant 0; dst }
      :: T.Jump end_label
      :: T.Label true_label
      :: T.Copy { src = Constant 1; dst }
      :: [ T.Label end_label ]
  in
  (instructions, dst)

and emit_conditional_expression condition e1 e2 =
  let eval_cond, c = emit_tacky_for_exp condition in
  let eval_v1, v1 = emit_tacky_for_exp e1 in
  let eval_v2, v2 = emit_tacky_for_exp e2 in
  let e2_label = Unique_ids.make_label "conditional_else" in
  let end_label = Unique_ids.make_label "conditional_end" in
  let dst_name = Unique_ids.make_temporary () in
  let dst = Tacky.Var dst_name in
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
  | Ast.Null -> []

and emit_tacky_for_block_item = function
  | Ast.S s -> emit_tacky_for_statement s
  | Ast.D (Declaration { name; init = Some e }) ->
      (* treat declaration with initializer like an assignment expression *)
      let eval_assignment, _assign_result =
        emit_tacky_for_exp (Ast.Assignment (Var name, e))
      in
      eval_assignment
  | Ast.D (Declaration { init = None; _ }) ->
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

let emit_tacky_for_function (Ast.Function { name; body }) =
  (* Use the tacky_instructions queue to accumulate instructions as we go *)
  let body_instructions = List.concat_map emit_tacky_for_block_item body in
  let extra_return = T.(Return (Constant 0)) in
  Tacky.Function { name; body = body_instructions @ [ extra_return ] }

let gen (Ast.Program fn_def) = T.Program (emit_tacky_for_function fn_def)
