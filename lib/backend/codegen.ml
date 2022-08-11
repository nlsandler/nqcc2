let convert_val = function
  | Tacky.Constant i -> Assembly.Imm i
  | Tacky.Var v -> Assembly.Pseudo v

let convert_op = function
  | Tacky.Complement -> Assembly.Not
  | Tacky.Negate -> Assembly.Neg

let convert_instruction = function
  | Tacky.Return tacky_val ->
      let asm_val = convert_val tacky_val in
      Assembly.[ Mov (asm_val, Reg AX); Ret ]
  | Tacky.Unary { op; src; dst } ->
      let asm_op = convert_op op in
      let asm_src = convert_val src in
      let asm_dst = convert_val dst in
      [ Mov (asm_src, asm_dst); Unary (asm_op, asm_dst) ]

let convert_function (Tacky.Function { name; body }) =
  let instructions = List.concat_map convert_instruction body in
  Assembly.Function { name; instructions }

let gen (Tacky.Program fn_def) = Assembly.Program (convert_function fn_def)
