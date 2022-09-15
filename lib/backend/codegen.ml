open Batteries
open Cnums

let int_param_passing_regs = Assembly.[ DI; SI; DX; CX; R8; R9 ]

let dbl_param_passing_regs =
  Assembly.[ XMM0; XMM1; XMM2; XMM3; XMM4; XMM5; XMM6; XMM7 ]

let zero = Assembly.Imm Int64.zero
let constants = Hashtbl.create 10

let add_constant ?(alignment = 8) dbl =
  let key = Int64.bits_of_float dbl in
  (* see if we've defined this double already *)
  try
    let name, old_alignment = Hashtbl.find constants key in
    (* update alignment to max of current and new *)
    Hashtbl.replace constants key (name, max alignment old_alignment);
    name
  with Not_found ->
    (* we haven't defined it yet, add it to the table *)
    let name = Unique_ids.make_label "dbl" in
    Hashtbl.replace constants key (name, alignment);
    name

let convert_val = function
  | Tacky.Constant (ConstChar c) -> Assembly.Imm (Int8.to_int64 c)
  | Tacky.Constant (ConstUChar uc) -> Assembly.Imm (UInt8.to_int64 uc)
  | Tacky.Constant (ConstInt i) -> Assembly.Imm (Int32.to_int64 i)
  | Tacky.Constant (ConstLong l) -> Imm l
  | Tacky.Constant (ConstUInt u) -> Imm (UInt32.to_int64 u)
  | Tacky.Constant (ConstULong ul) -> Imm (UInt64.to_int64 ul)
  | Tacky.Constant (ConstDouble d) -> Data (add_constant d)
  | Tacky.Var v ->
      if Type_utils.is_array (Symbols.get v).t then PseudoMem (v, 0)
      else Assembly.Pseudo v

let convert_type = function
  | Types.Int | UInt -> Assembly.Longword
  | Long | ULong | Pointer _ -> Quadword
  | Char | SChar | UChar -> Byte
  | Double -> Double
  | Array _ as t ->
      ByteArray
        { size = Type_utils.get_size t; alignment = Type_utils.get_alignment t }
  | (FunType _ | Void) as t ->
      failwith
        ("Internal error, converting type to assembly: " ^ Types.show t)
      [@coverage off]

let tacky_type = function
  (* note: this reports the type of ConstChar as SChar instead of Char, doesn't matter in this context *)
  | Tacky.Constant c -> Const.type_of_const c
  | Tacky.Var v -> (
      try (Symbols.get v).t
      with Not_found ->
        failwith ("Internal error: " ^ v ^ " not in symbol table"))

let asm_type = convert_type % tacky_type

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
  | Tacky.Divide ->
      DivDouble (* NB should only be called for operands on doubles *)
  | Tacky.(
      ( Mod | Equal | NotEqual | GreaterOrEqual | LessOrEqual | GreaterThan
      | LessThan | BitshiftLeft | BitshiftRight )) ->
      failwith
        "Internal error: shouldn't be converted to binary assembly instruction \
         here" [@coverage off]
  | Tacky.BitwiseAnd -> Assembly.And
  | Tacky.BitwiseOr -> Assembly.Or
  | Tacky.BitwiseXor -> Assembly.Xor

let convert_shift_op signed = function
  (* NOTE: Sal/Shl are actually the same operation;
   * we use different mnemonics for symmetry with Sar/Shr, which are distinct
   *)
  | Tacky.BitshiftLeft -> if signed then Assembly.Sal else Assembly.Shl
  | Tacky.BitshiftRight -> if signed then Assembly.Sar else Assembly.Shr
  | _ ->
      failwith "Internal error: not a bitwise shift operation" [@coverage off]

let convert_cond_code signed = function
  | Tacky.Equal -> Assembly.E
  | Tacky.NotEqual -> Assembly.NE
  | Tacky.GreaterThan -> if signed then Assembly.G else Assembly.A
  | Tacky.GreaterOrEqual -> if signed then Assembly.GE else Assembly.AE
  | Tacky.LessThan -> if signed then Assembly.L else Assembly.B
  | Tacky.LessOrEqual -> if signed then Assembly.LE else Assembly.BE
  | _ -> failwith "Internal error: not a condition code" [@coverage off]

(* Helper function for double comparisons w/ support for NaN *)
let convert_dbl_comparison op dst_t asm_src1 asm_src2 asm_dst =
  let cond_code = convert_cond_code false op in
  (* If op is A or AE, can perform usual comparisons;
     * these are true only if some flags are 0, so they'll be false for unordered results.
       * If op is B or BE, just flip operands and use A or AE instead.
     * If op is E or NE, need to check for parity afterwards *)
  let cond_code, asm_src1, asm_src2 =
    match cond_code with
    | B -> (Assembly.A, asm_src2, asm_src1)
    | BE -> (AE, asm_src2, asm_src1)
    | _ -> (cond_code, asm_src1, asm_src2)
  in
  let instrs =
    Assembly.
      [
        Cmp (Double, asm_src2, asm_src1);
        Mov (dst_t, zero, asm_dst);
        SetCC (cond_code, asm_dst);
      ]
  in
  let parity_instrs =
    match cond_code with
    | Assembly.E ->
        (* zero out destination if parity flag is set,
         * indicating unordered result
         *)
        Assembly.
          [
            Mov (dst_t, zero, Reg R9);
            SetCC (NP, Reg R9);
            Binary { op = And; t = dst_t; src = Reg R9; dst = asm_dst };
          ]
    | Assembly.NE ->
        (* set destination to 1 if parity flag is set, indicating ordered result *)
        Assembly.
          [
            Mov (dst_t, zero, Reg R9);
            SetCC (P, Reg R9);
            Binary { op = Or; t = dst_t; src = Reg R9; dst = asm_dst };
          ]
    | _ -> []
  in
  instrs @ parity_instrs

let classify_parameters tacky_vals =
  let process_one_param (int_reg_args, dbl_reg_args, stack_args) v =
    let operand = convert_val v in
    let t = asm_type v in
    let typed_operand = (t, operand) in
    match t with
    | Double ->
        if List.length dbl_reg_args < 8 then
          (int_reg_args, operand :: dbl_reg_args, stack_args)
        else (int_reg_args, dbl_reg_args, typed_operand :: stack_args)
    | _ ->
        if List.length int_reg_args < 6 then
          (typed_operand :: int_reg_args, dbl_reg_args, stack_args)
        else (int_reg_args, dbl_reg_args, typed_operand :: stack_args)
  in
  let reversed_int, reversed_dbl, reversed_stack =
    List.fold_left process_one_param ([], [], []) tacky_vals
  in
  (List.rev reversed_int, List.rev reversed_dbl, List.rev reversed_stack)

let convert_function_call f args dst =
  let int_reg_args, dbl_reg_args, stack_args = classify_parameters args in
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
  let pass_int_reg_arg idx (arg_t, arg) =
    let r = List.at int_param_passing_regs idx in
    Assembly.Mov (arg_t, arg, Reg r)
  in
  let instructions = instructions @ List.mapi pass_int_reg_arg int_reg_args in

  (* pass args in registers *)
  let pass_dbl_reg_arg idx arg =
    let r = List.at dbl_param_passing_regs idx in
    Assembly.Mov (Double, arg, Reg r)
  in
  let instructions = instructions @ List.mapi pass_dbl_reg_arg dbl_reg_args in

  (* pass args on the stack*)
  let pass_stack_arg (arg_t, arg) =
    match arg with
    | Assembly.Imm _ | Reg _ -> [ Assembly.Push arg ]
    | _ ->
        if arg_t = Assembly.Quadword || arg_t = Double then
          [ Assembly.Push arg ]
        else
          (* copy into a register before pushing *)
          [ Mov (arg_t, arg, Reg AX); Push (Reg AX) ]
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
  let retrieve_result =
    match dst with
    | Some d ->
        let assembly_dst = convert_val d in
        let return_reg = if asm_type d = Double then Assembly.XMM0 else AX in
        [ Assembly.Mov (asm_type d, Reg return_reg, assembly_dst) ]
    | None -> []
  in
  instructions @ retrieve_result

let convert_instruction = function
  | Tacky.Copy { src; dst } ->
      let t = asm_type src in
      let asm_src = convert_val src in
      let asm_dst = convert_val dst in
      Assembly.[ Mov (t, asm_src, asm_dst) ]
  | Tacky.Return (Some tacky_val) ->
      let t = asm_type tacky_val in
      let asm_val = convert_val tacky_val in
      let ret_reg = if t = Assembly.Double then Assembly.XMM0 else AX in
      [ Mov (t, asm_val, Reg ret_reg); Ret ]
  | Tacky.Return None -> [ Ret ]
  | Tacky.Unary { op = Not; src; dst } ->
      let src_t = asm_type src in
      let dst_t = asm_type dst in

      let asm_src = convert_val src in
      let asm_dst = convert_val dst in
      if src_t = Double then
        [
          Assembly.Binary
            { op = Xor; t = Double; src = Reg XMM0; dst = Reg XMM0 };
          Cmp (src_t, asm_src, Reg XMM0);
          Mov (dst_t, zero, asm_dst);
          SetCC (E, asm_dst);
        ]
      else
        [
          Cmp (src_t, zero, asm_src);
          Mov (dst_t, zero, asm_dst);
          SetCC (E, asm_dst);
        ]
  | Tacky.Unary { op = Negate; src; dst } when tacky_type src = Double ->
      let asm_src = convert_val src in
      let asm_dst = convert_val dst in
      let negative_zero = add_constant ~alignment:16 (-0.0) in
      [
        Mov (Double, asm_src, asm_dst);
        Binary { op = Xor; t = Double; src = Data negative_zero; dst = asm_dst };
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
          if
            src_t = Double && List.mem Settings.Nan !Settings.extra_credit_flags
          then convert_dbl_comparison op dst_t asm_src1 asm_src2 asm_dst
          else
            let signed =
              if src_t = Double then false
              else Type_utils.is_signed (tacky_type src1)
            in
            let cond_code = convert_cond_code signed op in
            [
              Cmp (src_t, asm_src2, asm_src1);
              Mov (dst_t, zero, asm_dst);
              SetCC (cond_code, asm_dst);
            ]
      (* Division/modulo *)
      | (Divide | Mod) when src_t <> Double ->
          let result_reg = if op = Divide then Assembly.AX else DX in
          if Type_utils.is_signed (tacky_type src1) then
            [
              Mov (src_t, asm_src1, Reg AX);
              Cdq src_t;
              Assembly.Idiv (src_t, asm_src2);
              Mov (src_t, Reg result_reg, asm_dst);
            ]
          else
            [
              Mov (src_t, asm_src1, Reg AX);
              Mov (src_t, zero, Reg DX);
              Assembly.Div (src_t, asm_src2);
              Mov (src_t, Reg result_reg, asm_dst);
            ]
      | BitshiftLeft | BitshiftRight -> (
          let is_signed = Type_utils.is_signed (tacky_type src1) in
          let asm_op = convert_shift_op is_signed op in
          let asm_t = asm_type src1 in
          match asm_src2 with
          | Imm _ ->
              [
                Mov (asm_t, asm_src1, asm_dst);
                Binary { op = asm_op; t = asm_t; src = asm_src2; dst = asm_dst };
              ]
          | _ ->
              [
                Mov (asm_t, asm_src1, asm_dst);
                (* NOTE: only lower byte of CX is used. *)
                Mov (Byte, asm_src2, Reg CX);
                Binary { op = asm_op; t = asm_t; src = Reg CX; dst = asm_dst };
              ])
      (* Addition/subtraction/mutliplication/and/or/xor*)
      | _ ->
          let asm_op = convert_binop op in
          [
            Mov (src_t, asm_src1, asm_dst);
            Binary { op = asm_op; t = src_t; src = asm_src2; dst = asm_dst };
          ])
  | Tacky.Load { src_ptr; dst } ->
      let asm_src_ptr = convert_val src_ptr in
      let asm_dst = convert_val dst in
      let t = asm_type dst in
      [ Mov (Quadword, asm_src_ptr, Reg R9); Mov (t, Memory (R9, 0), asm_dst) ]
  | Tacky.Store { src; dst_ptr } ->
      let asm_src = convert_val src in
      let t = asm_type src in
      let asm_dst_ptr = convert_val dst_ptr in
      [ Mov (Quadword, asm_dst_ptr, Reg R9); Mov (t, asm_src, Memory (R9, 0)) ]
  | Tacky.GetAddress { src; dst } ->
      let asm_src = convert_val src in
      let asm_dst = convert_val dst in
      [ Lea (asm_src, asm_dst) ]
  | Tacky.Jump target -> [ Jmp target ]
  | Tacky.JumpIfZero (cond, target) ->
      let t = asm_type cond in
      let asm_cond = convert_val cond in
      if t = Double then
        [
          Assembly.Binary
            { op = Xor; t = Double; src = Reg XMM0; dst = Reg XMM0 };
          Cmp (t, asm_cond, Reg XMM0);
          JmpCC (E, target);
        ]
      else [ Cmp (t, zero, asm_cond); JmpCC (E, target) ]
  | Tacky.JumpIfNotZero (cond, target) ->
      let t = asm_type cond in
      let asm_cond = convert_val cond in
      if t = Double then
        [
          Assembly.Binary
            { op = Xor; t = Double; src = Reg XMM0; dst = Reg XMM0 };
          Cmp (t, asm_cond, Reg XMM0);
          JmpCC (NE, target);
        ]
      else [ Cmp (t, zero, asm_cond); JmpCC (NE, target) ]
  | Tacky.Label l -> [ Label l ]
  | Tacky.FunCall { f; args; dst } -> convert_function_call f args dst
  | Tacky.SignExtend { src; dst } ->
      let asm_src = convert_val src in
      let asm_dst = convert_val dst in
      [
        Movsx
          {
            src_type = asm_type src;
            dst_type = asm_type dst;
            src = asm_src;
            dst = asm_dst;
          };
      ]
  | Tacky.Truncate { src; dst } ->
      let asm_src = convert_val src in
      let asm_dst = convert_val dst in
      [ Mov (asm_type dst, asm_src, asm_dst) ]
  | Tacky.ZeroExtend { src; dst } ->
      let asm_src = convert_val src in
      let asm_dst = convert_val dst in
      [
        MovZeroExtend
          {
            src_type = asm_type src;
            dst_type = asm_type dst;
            src = asm_src;
            dst = asm_dst;
          };
      ]
  | Tacky.IntToDouble { src; dst } ->
      let asm_src = convert_val src in
      let asm_dst = convert_val dst in
      let t = asm_type src in
      if t = Byte then
        [
          Movsx
            {
              src_type = Byte;
              dst_type = Longword;
              src = asm_src;
              dst = Reg R9;
            };
          Cvtsi2sd (Longword, Reg R9, asm_dst);
        ]
      else [ Cvtsi2sd (t, asm_src, asm_dst) ]
  | Tacky.DoubleToInt { src; dst } ->
      let asm_src = convert_val src in
      let asm_dst = convert_val dst in
      let t = asm_type dst in
      if t = Byte then
        [ Cvttsd2si (Longword, asm_src, Reg R9); Mov (Byte, Reg R9, asm_dst) ]
      else [ Cvttsd2si (t, asm_src, asm_dst) ]
  | Tacky.UIntToDouble { src; dst } ->
      let asm_src = convert_val src in
      let asm_dst = convert_val dst in
      if tacky_type src = Types.UChar then
        [
          MovZeroExtend
            {
              src_type = Byte;
              dst_type = Longword;
              src = asm_src;
              dst = Reg R9;
            };
          Cvtsi2sd (Longword, Reg R9, asm_dst);
        ]
      else if tacky_type src = Types.UInt then
        [
          MovZeroExtend
            {
              src_type = Longword;
              dst_type = Quadword;
              src = asm_src;
              dst = Reg R9;
            };
          Cvtsi2sd (Quadword, Reg R9, asm_dst);
        ]
      else
        let out_of_bounds = Unique_ids.make_label "ulong2dbl.oob" in
        let end_lbl = Unique_ids.make_label "ulong2dbl.end" in
        let r1, r2 = Assembly.(Reg R8, Reg R9) in
        [
          (* check whether asm_src is w/in range of long *)
          Cmp (Quadword, zero, asm_src);
          JmpCC (L, out_of_bounds);
          (* it's in range, just use normal cvtsi2sd then jump to end *)
          Cvtsi2sd (Quadword, asm_src, asm_dst);
          Jmp end_lbl;
          (* it's out of bounds *)
          Label out_of_bounds;
          (* halve source and round to dd*)
          Mov (Quadword, asm_src, r1);
          Mov (Quadword, r1, r2);
          Unary (ShrOneOp, Quadword, r2);
          Binary { op = And; t = Quadword; src = Imm Int64.one; dst = r1 };
          Binary { op = Or; t = Quadword; src = r1; dst = r2 };
          (* convert to double, then double it *)
          Cvtsi2sd (Quadword, r2, asm_dst);
          Binary { op = Add; t = Double; src = asm_dst; dst = asm_dst };
          Label end_lbl;
        ]
  | Tacky.DoubleToUInt { src; dst } ->
      let asm_src = convert_val src in
      let asm_dst = convert_val dst in
      if tacky_type dst = Types.UChar then
        [ Cvttsd2si (Longword, asm_src, Reg R9); Mov (Byte, Reg R9, asm_dst) ]
      else if tacky_type dst = Types.UInt then
        Assembly.
          [
            Cvttsd2si (Quadword, asm_src, Reg R9);
            Mov (Longword, Reg R9, asm_dst);
          ]
      else
        let out_of_bounds = Unique_ids.make_label "dbl2ulong.oob" in
        let end_lbl = Unique_ids.make_label "dbl2ulong.end" in
        let upper_bound = add_constant 9223372036854775808.0 in
        let upper_bound_as_int =
          (* interpreted as signed integer, upper bound wraps around to become minimum int *)
          Assembly.Imm Int64.min_int
        in
        let r, x = Assembly.(Reg R9, Reg XMM7) in
        [
          Cmp (Double, Data upper_bound, asm_src);
          JmpCC (AE, out_of_bounds);
          Cvttsd2si (Quadword, asm_src, asm_dst);
          Jmp end_lbl;
          Label out_of_bounds;
          Mov (Double, asm_src, x);
          Binary { op = Sub; t = Double; src = Data upper_bound; dst = x };
          Cvttsd2si (Quadword, x, asm_dst);
          Mov (Quadword, upper_bound_as_int, r);
          Binary { op = Add; t = Quadword; src = r; dst = asm_dst };
          Label end_lbl;
        ]
  | CopyToOffset { src; dst; offset } ->
      [ Mov (asm_type src, convert_val src, PseudoMem (dst, offset)) ]
  | AddPtr { ptr; index = Constant (Const.ConstLong c); scale; dst } ->
      (* note that typechecker converts index to long
       * QUESTION: what's the largest offset we should support? *)
      let i = Int64.to_int c in
      [
        Mov (Quadword, convert_val ptr, Reg R9);
        Lea (Memory (R9, i * scale), convert_val dst);
      ]
  | AddPtr { ptr; index; scale; dst } ->
      if scale = 1 || scale = 2 || scale = 4 || scale = 8 then
        [
          Mov (Quadword, convert_val ptr, Reg R8);
          Mov (Quadword, convert_val index, Reg R9);
          Lea (Indexed { base = R8; index = R9; scale }, convert_val dst);
        ]
      else
        [
          Mov (Quadword, convert_val ptr, Reg R8);
          Mov (Quadword, convert_val index, Reg R9);
          Binary
            {
              op = Mult;
              t = Quadword;
              src = Imm (Int64.of_int scale);
              dst = Reg R9;
            };
          Lea (Indexed { base = R8; index = R9; scale = 1 }, convert_val dst);
        ]

let pass_params param_list =
  let int_reg_params, dbl_reg_params, stack_params =
    classify_parameters param_list
  in
  (* pass parameter in register *)
  let pass_in_int_register idx (param_t, param) =
    let r = List.at int_param_passing_regs idx in
    Assembly.Mov (param_t, Reg r, param)
  in
  let pass_in_dbl_register idx param =
    let r = List.at dbl_param_passing_regs idx in
    Assembly.Mov (Double, Reg r, param)
  in
  let pass_on_stack idx (param_t, param) =
    (* first param passed on stack has idx 0 and is passed at 16(%rbp) *)
    let stk = Assembly.Memory (BP, 16 + (8 * idx)) in
    Assembly.Mov (param_t, stk, param)
  in
  List.mapi pass_in_int_register int_reg_params
  @ List.mapi pass_in_dbl_register dbl_reg_params
  @ List.mapi pass_on_stack stack_params

(* Special-case logic to get type/alignment of array; array variables w/ size >=16 bytes have alignment of 16 *)
let get_var_alignment = function
  | Types.Array _ as t when Type_utils.get_size t >= 16 -> 16
  | t -> Type_utils.get_alignment t

let convert_var_type = function
  | Types.Array _ as t ->
      Assembly.ByteArray
        { size = Type_utils.get_size t; alignment = get_var_alignment t }
  | other -> convert_type other

let convert_top_level = function
  | Tacky.Function { name; global; body; params } ->
      let params_as_tacky = List.map (fun name -> Tacky.Var name) params in
      let instructions =
        pass_params params_as_tacky @ List.concat_map convert_instruction body
      in
      Assembly.Function { name; global; instructions }
  | Tacky.StaticVariable { name; global; t; init } ->
      Assembly.StaticVariable
        { name; global; alignment = get_var_alignment t; init }
  | Tacky.StaticConstant { name; t; init } ->
      Assembly.StaticConstant
        { name; alignment = Type_utils.get_alignment t; init }

let convert_constant (key, (name, alignment)) =
  let dbl = Int64.float_of_bits key in
  Assembly_symbols.add_constant name Double;
  Assembly.StaticConstant
    { name; alignment; init = Initializers.DoubleInit dbl }

(* convert each symbol table entry to assembly symbol table equivalent*)
let convert_symbol name = function
  | Symbols.{ t = Types.FunType _; attrs = FunAttr { defined; _ } } ->
      Assembly_symbols.add_fun name defined
  | { t; attrs = ConstAttr _ } ->
      Assembly_symbols.add_constant name (convert_type t)
  | Symbols.{ t; attrs = StaticAttr _; _ } ->
      Assembly_symbols.add_var name (convert_var_type t) true
  | Symbols.{ t; _ } -> Assembly_symbols.add_var name (convert_var_type t) false

let gen (Tacky.Program top_levels) =
  (* clear the hashtable (necessary if we're compiling multiple source) *)
  Hashtbl.clear constants;
  let tls = List.map convert_top_level top_levels in
  let constants = List.map convert_constant (Hashtbl.bindings constants) in

  let prog = Assembly.Program (constants @ tls) in
  let _ = Symbols.iter convert_symbol in
  prog
