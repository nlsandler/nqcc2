open Batteries

let param_passing_regs = Assembly.[ DI; SI; DX; CX; R8; R9 ]
let zero = Assembly.Imm Int64.zero

let convert_val = function
  | Tacky.Constant (ConstInt i) -> Assembly.Imm (Int32.to_int64 i)
  | Tacky.Constant (ConstLong l) -> Imm l
  | Tacky.Var v -> Assembly.Pseudo v

let convert_type = function
  | Types.Int -> Assembly.Longword
  | Long -> Quadword
  | FunType _ ->
      failwith "Internal error, converting function type to assembly"
      [@coverage off]

let asm_type = function
  | Tacky.Constant (ConstLong _) -> Assembly.Quadword
  | Tacky.Constant (ConstInt _) -> Longword
  | Tacky.Var v -> convert_type (Symbols.get v).t

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

let convert_cond_code = function
  | Tacky.Equal -> Assembly.E
  | Tacky.NotEqual -> Assembly.NE
  | Tacky.GreaterThan -> Assembly.G
  | Tacky.GreaterOrEqual -> Assembly.GE
  | Tacky.LessThan -> Assembly.L
  | Tacky.LessOrEqual -> Assembly.LE
  | _ -> failwith "Internal error: not a condition code" [@coverage off]

let convert_function_call f args dst =
  let reg_args, stack_args = List.takedrop 6 args in
  (* adjust stack alignment *)
  let stack_padding = if List.length stack_args mod 2 = 0 then 0 else 8 in
  let instructions =
    if stack_padding = 0 then []
    else
      [
        Assembly.Binary
          {
            op = Sub;
            t = Quadword;
            src = Imm (Int64.of_int stack_padding);
            dst = Reg SP;
          };
      ]
  in
  (* pass args in registers *)
  let pass_reg_arg idx arg =
    let r = List.at param_passing_regs idx in
    let assembly_arg = convert_val arg in
    Assembly.Mov (asm_type arg, assembly_arg, Reg r)
  in
  let instructions = instructions @ List.mapi pass_reg_arg reg_args in

  (* pass args on the stack*)
  let pass_stack_arg arg =
    let assembly_arg = convert_val arg in
    match assembly_arg with
    | Assembly.Imm _ | Reg _ -> [ Assembly.Push assembly_arg ]
    | _ ->
        let assembly_type = asm_type arg in
        if assembly_type = Quadword then [ Assembly.Push assembly_arg ]
        else
          (* copy into a register before pushing *)
          [ Mov (assembly_type, assembly_arg, Reg AX); Push (Reg AX) ]
  in
  let instructions =
    instructions @ List.concat (List.rev_map pass_stack_arg stack_args)
  in

  (* adjust stack pointer *)
  let instructions = instructions @ [ Assembly.Call f ] in

  (* adjust stack pointer *)
  let bytes_to_remove = (8 * List.length stack_args) + stack_padding in
  let dealloc =
    if bytes_to_remove = 0 then []
    else
      [
        Assembly.Binary
          {
            op = Add;
            t = Quadword;
            src = Imm (Int64.of_int bytes_to_remove);
            dst = Reg SP;
          };
      ]
  in
  let instructions = instructions @ dealloc in

  (* retrieve return value *)
  let assembly_dst = convert_val dst in
  instructions @ [ Mov (asm_type dst, Reg AX, assembly_dst) ]

let convert_instruction = function
  | Tacky.Copy { src; dst } ->
      let t = asm_type src in
      let asm_src = convert_val src in
      let asm_dst = convert_val dst in
      Assembly.[ Mov (t, asm_src, asm_dst) ]
  | Tacky.Return tacky_val ->
      let t = asm_type tacky_val in
      let asm_val = convert_val tacky_val in
      [ Mov (t, asm_val, Reg AX); Ret ]
  | Tacky.Unary { op = Not; src; dst } ->
      let src_t = asm_type src in
      let dst_t = asm_type dst in

      let asm_src = convert_val src in
      let asm_dst = convert_val dst in
      [
        Cmp (src_t, zero, asm_src);
        Mov (dst_t, zero, asm_dst);
        SetCC (E, asm_dst);
      ]
  | Tacky.Unary { op; src; dst } ->
      let t = asm_type src in
      let asm_op = convert_unop op in
      let asm_src = convert_val src in
      let asm_dst = convert_val dst in
      [ Mov (t, asm_src, asm_dst); Unary (asm_op, t, asm_dst) ]
  | Tacky.Binary { op; src1; src2; dst } -> (
      let src_t = asm_type src1 in
      let dst_t = asm_type dst in
      let asm_src1 = convert_val src1 in
      let asm_src2 = convert_val src2 in
      let asm_dst = convert_val dst in
      match op with
      (* Relational operator *)
      | Equal | NotEqual | GreaterThan | GreaterOrEqual | LessThan | LessOrEqual
        ->
          let cond_code = convert_cond_code op in
          [
            Cmp (src_t, asm_src2, asm_src1);
            Mov (dst_t, zero, asm_dst);
            SetCC (cond_code, asm_dst);
          ]
      (* Division/modulo *)
      | Divide | Mod ->
          let result_reg = if op = Divide then Assembly.AX else DX in
          [
            Mov (src_t, asm_src1, Reg AX);
            Cdq src_t;
            Idiv (src_t, asm_src2);
            Mov (src_t, Reg result_reg, asm_dst);
          ]
          (* Addition/subtraction/mutliplication*)
      | _ ->
          let asm_op = convert_binop op in
          [
            Mov (src_t, asm_src1, asm_dst);
            Binary { op = asm_op; t = src_t; src = asm_src2; dst = asm_dst };
          ])
  | Tacky.Jump target -> [ Jmp target ]
  | Tacky.JumpIfZero (cond, target) ->
      let t = asm_type cond in
      let asm_cond = convert_val cond in
      [ Cmp (t, zero, asm_cond); JmpCC (E, target) ]
  | Tacky.JumpIfNotZero (cond, target) ->
      let t = asm_type cond in
      let asm_cond = convert_val cond in
      [ Cmp (t, zero, asm_cond); JmpCC (NE, target) ]
  | Tacky.Label l -> [ Label l ]
  | Tacky.FunCall { f; args; dst } -> convert_function_call f args dst
  | Tacky.SignExtend { src; dst } ->
      let asm_src = convert_val src in
      let asm_dst = convert_val dst in
      [ Movsx (asm_src, asm_dst) ]
  | Tacky.Truncate { src; dst } ->
      let asm_src = convert_val src in
      let asm_dst = convert_val dst in
      [ Mov (Longword, asm_src, asm_dst) ]

let pass_params param_list =
  let register_params, stack_params = List.takedrop 6 param_list in
  (* pass parameter in register *)
  let pass_in_register idx param =
    let r = List.at param_passing_regs idx in
    let param_t = asm_type (Var param) in
    Assembly.Mov (param_t, Reg r, Pseudo param)
  in
  let pass_on_stack idx param =
    (* first param passed on stack has idx 0 and is passed at Stack(16) *)
    let stk = Assembly.Stack (16 + (8 * idx)) in
    let param_t = asm_type (Var param) in
    Assembly.Mov (param_t, stk, Pseudo param)
  in
  List.mapi pass_in_register register_params
  @ List.mapi pass_on_stack stack_params

let convert_top_level = function
  | Tacky.Function { name; global; body; params } ->
      let instructions =
        pass_params params @ List.concat_map convert_instruction body
      in
      Assembly.Function { name; global; instructions }
  | Tacky.StaticVariable { name; global; t; init } ->
      Assembly.StaticVariable
        { name; global; alignment = Type_utils.get_alignment t; init }

(* convert each symbol table entry to assembly symbol table equivalent*)
let convert_symbol name = function
  (* if a function is defined (not just declared) in this translation unit, add it to the assembly symbol table *)
  | Symbols.{ t = Types.FunType _; attrs = FunAttr { defined; _ } } ->
      Assembly_symbols.add_fun name defined
  | Symbols.{ t; attrs = StaticAttr _; _ } ->
      Assembly_symbols.add_var name (convert_type t) true
  | Symbols.{ t; _ } -> Assembly_symbols.add_var name (convert_type t) false

let gen (Tacky.Program top_levels) =
  Symbols.iter convert_symbol;
  Assembly.Program (List.map convert_top_level top_levels)
