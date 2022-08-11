let convert_exp (Ast.Constant i) = Assembly.Imm i

let convert_statement (Ast.Return e) =
  let v = convert_exp e in
  Assembly.[ Mov (v, Register); Ret ]

let convert_function (Ast.Function { name; body }) =
  Assembly.Function { name; instructions = convert_statement body }

let gen (Ast.Program fn_def) = Assembly.Program (convert_function fn_def)
