let convert_val = function
  | Tacky.Constant i -> Assembly.Imm i
  | Tacky.Var v -> Assembly.Pseudo v

let convert_unop = function
  | Tacky.Complement -> Assembly.Not
  | Tacky.Negate -> Assembly.Neg
  | Tacky.Not ->
      failwith "Internal error, can't convert TACKY not directly to assembly"
      [@coverage off]

let convert_binop = function
  | Tacky.Add -> Assembly.Add
  | Tacky.Subtract -> Assembly.Sub
  | Tacky.Multiply -> Assembly.Mult
  | Tacky.(
      ( Divide | Mod | Equal | NotEqual | GreaterOrEqual | LessOrEqual
      | GreaterThan | LessThan )) ->
      failwith "Internal error: not a binary assembly instruction"
      [@coverage off]
  | Tacky.BitwiseAnd -> Assembly.And
  | Tacky.BitwiseOr -> Assembly.Or
  | Tacky.BitwiseXor -> Assembly.Xor
  | Tacky.BitshiftLeft -> Assembly.Sal
  | Tacky.BitshiftRight -> Assembly.Sar

let convert_cond_code = function
  | Tacky.Equal -> Assembly.E
  | Tacky.NotEqual -> Assembly.NE
  | Tacky.GreaterThan -> Assembly.G
  | Tacky.GreaterOrEqual -> Assembly.GE
  | Tacky.LessThan -> Assembly.L
  | Tacky.LessOrEqual -> Assembly.LE
  | _ -> failwith "Internal error: not a condition code" [@coverage off]

let convert_instruction = function
  | Tacky.Copy { src; dst } ->
      let asm_src = convert_val src in
      let asm_dst = convert_val dst in
      Assembly.[ Mov (asm_src, asm_dst) ]
  | Tacky.Return tacky_val ->
      let asm_val = convert_val tacky_val in
      [ Mov (asm_val, Reg AX); Ret ]
  | Tacky.Unary { op = Not; src; dst } ->
      let asm_src = convert_val src in
      let asm_dst = convert_val dst in
      [ Cmp (Imm 0, asm_src); Mov (Imm 0, asm_dst); SetCC (E, asm_dst) ]
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
      (* Relational operator *)
      | Equal | NotEqual | GreaterThan | GreaterOrEqual | LessThan | LessOrEqual
        ->
          let cond_code = convert_cond_code op in
          [
            Cmp (asm_src2, asm_src1);
            Mov (Imm 0, asm_dst);
            SetCC (cond_code, asm_dst);
          ]
      (* Division/modulo *)
      | Divide | Mod ->
          let result_reg = if op = Divide then Assembly.AX else DX in
          [
            Mov (asm_src1, Reg AX);
            Cdq;
            Idiv asm_src2;
            Mov (Reg result_reg, asm_dst);
          ]
      | BitshiftLeft | BitshiftRight -> (
          let asm_op = convert_binop op in
          match asm_src2 with
          | Imm _ ->
              [
                Mov (asm_src1, asm_dst);
                Binary { op = asm_op; src = asm_src2; dst = asm_dst };
              ]
          | _ ->
              [
                Mov (asm_src1, asm_dst);
                Mov (asm_src2, Reg CX);
                Binary { op = asm_op; src = Reg CX; dst = asm_dst };
              ])
      (* Addition/subtraction/mutliplication/and/or/xor*)
      | _ ->
          let asm_op = convert_binop op in
          [
            Mov (asm_src1, asm_dst);
            Binary { op = asm_op; src = asm_src2; dst = asm_dst };
          ])
  | Tacky.Jump target -> [ Jmp target ]
  | Tacky.JumpIfZero (cond, target) ->
      let asm_cond = convert_val cond in
      [ Cmp (Imm 0, asm_cond); JmpCC (E, target) ]
  | Tacky.JumpIfNotZero (cond, target) ->
      let asm_cond = convert_val cond in
      [ Cmp (Imm 0, asm_cond); JmpCC (NE, target) ]
  | Tacky.Label l -> [ Label l ]

let convert_function (Tacky.Function { name; body }) =
  let instructions = List.concat_map convert_instruction body in
  Assembly.Function { name; instructions }

let gen (Tacky.Program fn_def) = Assembly.Program (convert_function fn_def)
