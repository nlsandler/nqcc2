let convert_val = function
  | Tacky.Constant i -> Assembly.Imm i
  | Tacky.Var v -> Assembly.Pseudo v

let convert_unop = function
  | Tacky.Complement -> Assembly.Not
  | Tacky.Negate -> Assembly.Neg

let convert_binop = function
  | Tacky.Add -> Assembly.Add
  | Tacky.Subtract -> Assembly.Sub
  | Tacky.Multiply -> Assembly.Mult
  | Tacky.(Divide | Mod) ->
      failwith
        "Internal error: shouldn't handle division like other binary operators"
      [@coverage off]

let convert_instruction = function
  | Tacky.Return tacky_val ->
      let asm_val = convert_val tacky_val in
      Assembly.[ Mov (asm_val, Reg AX); Ret ]
  | Tacky.Unary { op; src; dst } ->
      let asm_op = convert_unop op in
      let asm_src = convert_val src in
      let asm_dst = convert_val dst in
      [ Mov (asm_src, asm_dst); Unary (asm_op, asm_dst) ]
  | Tacky.Binary { op; src1; src2; dst } -> (
      let asm_src1 = convert_val src1 in
      let asm_src2 = convert_val src2 in
      let asm_dst = convert_val dst in
      match op with
      (* Division/modulo *)
      | Divide | Mod ->
          let result_reg = if op = Divide then Assembly.AX else DX in
          [
            Mov (asm_src1, Reg AX);
            Cdq;
            Idiv asm_src2;
            Mov (Reg result_reg, asm_dst);
          ]
          (* Addition/subtraction/mutliplication*)
      | _ ->
          let asm_op = convert_binop op in
          [
            Mov (asm_src1, asm_dst);
            Binary { op = asm_op; src = asm_src2; dst = asm_dst };
          ])

let convert_function (Tacky.Function { name; body }) =
  let instructions = List.concat_map convert_instruction body in
  Assembly.Function { name; instructions }

let gen (Tacky.Program fn_def) = Assembly.Program (convert_function fn_def)
