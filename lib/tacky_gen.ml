open Batteries

module T = struct
  include Tacky
end

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
let rec emit_tacky_for_exp = function
  (* don't need any instructions to calculate a constant *)
  | Ast.Constant c -> ([], T.Constant c)
  | Ast.Unary (op, inner) -> emit_unary_expression op inner
  | Ast.Binary (And, e1, e2) -> emit_and_expression e1 e2
  | Ast.Binary (Or, e1, e2) -> emit_or_expression e1 e2
  | Ast.Binary (op, e1, e2) -> emit_binary_expression op e1 e2

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

let emit_tacky_for_statement (Ast.Return e) =
  let eval_exp, v = emit_tacky_for_exp e in
  eval_exp @ [ T.Return v ]

let emit_tacky_for_function (Ast.Function { name; body }) =
  let instructions = emit_tacky_for_statement body in
  T.Function { name; body = instructions }

let gen (Ast.Program fn_def) = T.Program (emit_tacky_for_function fn_def)
