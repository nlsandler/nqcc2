module T = struct
  include Tacky
end

(* convenience function: append instruction to end of list *)
let ( @::: ) is i = is @ [ i ]

let convert_op = function
  | Ast.Complement -> T.Complement
  | Ast.Negate -> T.Negate

let convert_binop = function
  | Ast.Add -> Tacky.Add
  | Ast.Subtract -> Tacky.Subtract
  | Ast.Multiply -> Tacky.Multiply
  | Ast.Divide -> Tacky.Divide
  | Ast.Mod -> Tacky.Mod

(* return list of instructions to evaluate expression a and resulting TACKY value as a pair *)
let rec emit_tacky_for_exp = function
  (* don't need any instructions to calculate a constant *)
  | Ast.Constant c -> ([], T.Constant c)
  | Ast.Unary (op, inner) -> emit_unary_expression op inner
  | Ast.Binary (op, e1, e2) -> emit_binary_expression op e1 e2

(* helper functions for individual expression *)
and emit_unary_expression op inner =
  let eval_inner, v = emit_tacky_for_exp inner in
  (* define a temporary variable to hold result of this expression *)
  let dst_name = Unique_ids.make_temporary () in
  let dst = T.Var dst_name in
  let tacky_op = convert_op op in
  let instructions = eval_inner @::: T.Unary { op = tacky_op; src = v; dst } in
  (instructions, dst)

and emit_binary_expression op e1 e2 =
  let eval_v1, v1 = emit_tacky_for_exp e1 in
  let eval_v2, v2 = emit_tacky_for_exp e2 in
  let dst_name = Unique_ids.make_temporary () in
  let dst = Tacky.Var dst_name in
  let tacky_op = convert_binop op in
  let instructions =
    eval_v1
    @ eval_v2
    @::: Tacky.Binary { op = tacky_op; src1 = v1; src2 = v2; dst }
  in
  (instructions, dst)

let emit_tacky_for_statement (Ast.Return e) =
  let eval_exp, v = emit_tacky_for_exp e in
  eval_exp @::: T.Return v

let emit_tacky_for_function (Ast.Function { name; body }) =
  let instructions = emit_tacky_for_statement body in
  T.Function { name; body = instructions }

let gen (Ast.Program fn_def) = T.Program (emit_tacky_for_function fn_def)
