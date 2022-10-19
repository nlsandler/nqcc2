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

(* Get the operand type we should use to move an eightbyte of a struct.
   * If it contains exactly 8, 4, or 1 bytes, use the corresponding type (note that all but the last
   * eightbyte of a struct are exactly 8 bytes). If it's an ueven size s *)
let get_eightbyte_type ~eightbyte_idx ~total_var_size =
  let bytes_left = total_var_size - (eightbyte_idx * 8) in
  match bytes_left with
  | x when x >= 8 -> Assembly.Quadword (* *)
  | 4 -> Longword
  | 1 -> Byte
  | x -> ByteArray { size = x; alignment = 8 }

let add_offset n = function
  | Assembly.PseudoMem (base, off) -> Assembly.PseudoMem (base, off + n)
  | Memory (r, off) -> Memory (r, off + n)
  (* you could do pointer arithmetic w/ indexed or data operands but we don't need to *)
  | Imm _ | Reg _ | Pseudo _ | Indexed _ | Data _ ->
      failwith
        "Internal error: trying to copy data to or from non-memory operand"
      [@coverage off]

let rec copy_bytes src_val dst_val ~byte_count =
  if byte_count = 0 then []
  else
    let operand_type, operand_size =
      if byte_count < 4 then (Assembly.Byte, 1)
      else if byte_count < 8 then (Longword, 4)
      else (Quadword, 8)
    in

    let next_src = add_offset operand_size src_val in
    let next_dst = add_offset operand_size dst_val in
    let bytes_left = byte_count - operand_size in
    Assembly.Mov (operand_type, src_val, dst_val)
    :: copy_bytes next_src next_dst ~byte_count:bytes_left

(* copy an uneven, smaller-than-quadword eightbyte from memory into a register:
 * repeatedly copy byte into register and shift left, starting w/ highest byte and working down to lowest *)
let copy_bytes_to_reg src_val dst_reg ~byte_count =
  let copy_byte i =
    let mv = Assembly.Mov (Byte, add_offset i src_val, Reg dst_reg) in
    if i = 0 then [ mv ]
    else
      [ mv; Binary { op = Shl; t = Quadword; src = Imm 8L; dst = Reg dst_reg } ]
  in
  let byte_counts = List.range (byte_count - 1) `Downto 0 in
  List.concat_map copy_byte byte_counts

(* copy an uneven, smaller-than-quadword eightbyte from a register into memory;
 * repeatedly copy byte into register and shift right, starting w/ byte 0  and working up *)
let copy_bytes_from_reg src_reg dst_val ~byte_count =
  let copy_byte i =
    let mv = Assembly.Mov (Byte, Reg src_reg, add_offset i dst_val) in
    if i < byte_count - 1 then
      Assembly.
        [
          mv; Binary { op = Shr; t = Quadword; src = Imm 8L; dst = Reg src_reg };
        ]
    else [ mv ]
  in
  List.concat (List.init byte_count copy_byte)

let convert_val = function
  | Tacky.Constant (ConstChar c) -> Assembly.Imm (Int8.to_int64 c)
  | Tacky.Constant (ConstUChar uc) -> Assembly.Imm (UInt8.to_int64 uc)
  | Tacky.Constant (ConstInt i) -> Assembly.Imm (Int32.to_int64 i)
  | Tacky.Constant (ConstLong l) -> Imm l
  | Tacky.Constant (ConstUInt u) -> Imm (UInt32.to_int64 u)
  | Tacky.Constant (ConstULong ul) -> Imm (UInt64.to_int64 ul)
  | Tacky.Constant (ConstDouble d) -> Data (add_constant d, 0)
  | Tacky.Var v ->
      if Type_utils.is_scalar (Symbols.get v).t then Pseudo v
      else Assembly.PseudoMem (v, 0)

let convert_type = function
  | Types.Int | UInt -> Assembly.Longword
  | Long | ULong | Pointer _ -> Quadword
  | Char | SChar | UChar -> Byte
  | Double -> Double
  | (Array _ | Structure _ | Union _) as t ->
      ByteArray
        { size = Type_utils.get_size t; alignment = Type_utils.get_alignment t }
  | (FunType _ | Void) as t ->
      failwith
        ("Internal error, converting type to assembly: " ^ Types.show t)
      [@coverage off]

let asm_type = convert_type % Tacky.type_of_val

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

type cls = Mem | SSE | Integer

let classify_new_type tag =
  let size = Type_table.get_size tag in
  if size > 16 then
    let eightbyte_count = (size / 8) + if size mod 8 = 0 then 0 else 1 in
    List.make eightbyte_count Mem
  else
    (* Helper function to determine class of each eightbyte *)
    let rec classify_eightbytes offset (first_eightbyte, second_eightbyte) =
      function
      | Types.Double -> (* this is default *) (first_eightbyte, second_eightbyte)
      | t when Type_utils.is_scalar t ->
          if offset < 8 then (Integer, second_eightbyte)
          else (first_eightbyte, Integer)
      | Union tag ->
          (* fold over members *)
          let member_types = Type_table.get_member_types tag in
          List.fold_left
            (classify_eightbytes offset)
            (first_eightbyte, second_eightbyte)
            member_types
      | Structure tag ->
          (* fold over members, updating offsets*)
          let members = Type_table.get_members tag in
          let f (one, two) (_, member_info) =
            classify_eightbytes
              (offset + member_info.Type_table.offset)
              (one, two) member_info.member_type
          in
          List.fold_left f (first_eightbyte, second_eightbyte) members
      | Array { elem_type; size } ->
          let elem_size = Type_utils.get_size elem_type in
          let rec f (one, two) idx =
            if idx = size then (one, two)
            else
              let one', two' =
                classify_eightbytes
                  (offset + (idx * elem_size))
                  (one, two) elem_type
              in
              f (one', two') (idx + 1)
          in
          f (first_eightbyte, second_eightbyte) 0
      | _ -> failwith "Internal error"
    in
    let class1, class2 =
      classify_eightbytes 0 (SSE, SSE) (Type_table.get_type tag)
    in
    if size > 8 then [ class1; class2 ] else [ class1 ]

(* memoize results of classify_type *)
let classified_types = Hashtbl.create 10

let classify_type tag =
  match Hashtbl.find_option classified_types tag with
  | Some classes -> classes
  | None ->
      let classes = classify_new_type tag in
      Hashtbl.add classified_types tag classes;
      classes

let classify_tacky_val v =
  match Tacky.type_of_val v with
  | Structure tag -> classify_type tag
  | Union tag -> classify_type tag
  | _ ->
      failwith "Internal error: trying to classify non-structure or union type"
      [@coverage off]

let classify_parameters tacky_vals return_on_stack =
  let int_regs_available = if return_on_stack then 5 else 6 in
  let process_one_param (int_reg_args, dbl_reg_args, stack_args) v =
    let operand = convert_val v in
    let t = asm_type v in
    let typed_operand = (t, operand) in
    match t with
    | Double ->
        if List.length dbl_reg_args < 8 then
          (int_reg_args, operand :: dbl_reg_args, stack_args)
        else (int_reg_args, dbl_reg_args, typed_operand :: stack_args)
    | Byte | Longword | Quadword ->
        if List.length int_reg_args < int_regs_available then
          (typed_operand :: int_reg_args, dbl_reg_args, stack_args)
        else (int_reg_args, dbl_reg_args, typed_operand :: stack_args)
    | ByteArray _ ->
        (* it's a structure or union *)
        let var_name =
          match v with
          | Tacky.Var n -> n
          | Constant _ ->
              failwith "Internal error: constant byte array" [@coverage off]
        in
        let var_size = Type_utils.get_size (Tacky.type_of_val v) in
        let classes = classify_tacky_val v in
        let updated_int, updated_dbl, use_stack =
          if List.hd classes = Mem then
            (* all eightbytes go on the stack*)
            (int_reg_args, dbl_reg_args, true)
          else
            (* tentative assign eigthbytes to registers *)
            let process_one_eightbyte (tentative_ints, tentative_dbls) i cls =
              let operand = Assembly.PseudoMem (var_name, i * 8) in
              match cls with
              | SSE -> (tentative_ints, operand :: tentative_dbls)
              | Integer ->
                  let eightbyte_type =
                    get_eightbyte_type ~eightbyte_idx:i ~total_var_size:var_size
                  in
                  ((eightbyte_type, operand) :: tentative_ints, tentative_dbls)
              | Mem ->
                  failwith
                    "Internal error: found eightbyte in Mem class, but first \
                     eighbyte wasn't Mem" [@coverage off]
            in
            let tentative_ints, tentative_dbs =
              List.fold_lefti process_one_eightbyte
                (int_reg_args, dbl_reg_args)
                classes
            in
            if
              List.length tentative_ints <= int_regs_available
              && List.length tentative_dbs <= 8
            then
              (* assignment to regs succeeded *)
              (tentative_ints, tentative_dbs, false)
            else (int_reg_args, dbl_reg_args, true)
        in
        let add_stack_args stk i _ =
          let eightbyte_type =
            get_eightbyte_type ~eightbyte_idx:i ~total_var_size:var_size
          in
          Assembly.(eightbyte_type, PseudoMem (var_name, i * 8)) :: stk
        in
        let updated_stack_args =
          if use_stack then
            (* add each eighbyte of structure to stack_args s*)
            List.fold_lefti add_stack_args stack_args classes
          else stack_args
        in
        (updated_int, updated_dbl, updated_stack_args)
  in

  let reversed_int, reversed_dbl, reversed_stack =
    List.fold_left process_one_param ([], [], []) tacky_vals
  in
  (List.rev reversed_int, List.rev reversed_dbl, List.rev reversed_stack)

let classify_return_value retval =
  let open Assembly in
  let retval_type = Tacky.type_of_val retval in
  let classify_return_val_helper tag =
    let classes = classify_type tag in
    let var_name =
      match retval with
      | Tacky.Var n -> n
      | Constant _ ->
          failwith "Internal error: constant with structure type"
          [@coverage off]
    in
    if List.hd classes = Mem then ([], [], true)
    else
      (* return in registers, can move everything w/ quadword operands *)
      let process_quadword (ints, dbls) i cls =
        let operand = PseudoMem (var_name, i * 8) in
        match cls with
        | SSE -> (ints, dbls @ [ operand ])
        | Integer ->
            let eightbyte_type =
              get_eightbyte_type ~eightbyte_idx:i
                ~total_var_size:(Type_utils.get_size retval_type)
            in
            (ints @ [ (eightbyte_type, operand) ], dbls)
        | Mem ->
            failwith
              "Internal error: found eightbyte in Mem class, but first \
               eighbyte wasn't Mem" [@coverage off]
      in
      let i, d = List.fold_lefti process_quadword ([], []) classes in
      (i, d, false)
  in
  match retval_type with
  | Types.Structure tag -> classify_return_val_helper tag
  | Types.Union tag -> classify_return_val_helper tag
  | Double ->
      let asm_val = convert_val retval in
      ([], [ asm_val ], false)
  | _ ->
      let typed_operand = (asm_type retval, convert_val retval) in
      ([ typed_operand ], [], false)

let convert_function_call f args dst =
  let int_retvals, dbl_retvals, return_on_stack =
    Option.map_default classify_return_value ([], [], false) dst
  in
  (* load address of dest into DI  *)
  let load_dst_instruction, first_intreg_idx =
    if return_on_stack then
      ([ Assembly.Lea (convert_val (Option.get dst), Reg DI) ], 1)
    else ([], 0)
  in

  let int_reg_args, dbl_reg_args, stack_args =
    classify_parameters args return_on_stack
  in

  (* adjust stack alignment *)
  let stack_padding = if List.length stack_args mod 2 = 0 then 0 else 8 in
  let alignment_instruction =
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
  let instructions = load_dst_instruction @ alignment_instruction in
  (* pass args in registers *)
  let pass_int_reg_arg idx (arg_t, arg) =
    let r = List.at int_param_passing_regs (idx + first_intreg_idx) in
    match arg_t with
    | Assembly.ByteArray { size; _ } ->
        copy_bytes_to_reg arg r
          ~byte_count:size (* copy_thru_redzone arg r size *)
    | _ -> [ Assembly.Mov (arg_t, arg, Reg r) ]
  in
  let instructions =
    instructions @ List.concat (List.mapi pass_int_reg_arg int_reg_args)
  in

  (* pass args in registers *)
  let pass_dbl_reg_arg idx arg =
    let r = List.at dbl_param_passing_regs idx in
    Assembly.Mov (Double, arg, Reg r)
  in
  let instructions = instructions @ List.mapi pass_dbl_reg_arg dbl_reg_args in

  (* pass args on the stack*)
  let pass_stack_arg (arg_t, arg) =
    match (arg, arg_t) with
    | (Assembly.Imm _ | Reg _), _ | _, Assembly.(Quadword | Double) ->
        [ Assembly.Push arg ]
    | _, ByteArray { size; _ } ->
        Binary
          { op = Sub; t = Quadword; src = Imm (Int64.of_int 8); dst = Reg SP }
        :: copy_bytes arg (Memory (SP, 0)) ~byte_count:size
    | _ ->
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
  let int_ret_regs = [ Assembly.AX; DX ] in
  let dbl_ret_regs = [ Assembly.XMM0; XMM1 ] in
  let retrieve_result =
    match (dst, return_on_stack) with
    | Some _, false ->
        let get_int i (t, op) =
          let r = List.at int_ret_regs i in
          match t with
          | Assembly.ByteArray { size; _ } ->
              copy_bytes_from_reg r op ~byte_count:size
          | _ -> [ Assembly.Mov (t, Reg r, op) ]
        in
        let get_dbl i op =
          let r = List.at dbl_ret_regs i in
          Assembly.Mov (Double, Reg r, op)
        in
        List.concat (List.mapi get_int int_retvals)
        @ List.mapi get_dbl dbl_retvals
    | _ -> []
  in
  instructions @ retrieve_result

let convert_return_instruction = function
  | None -> [ Assembly.Ret ]
  | Some v ->
      let int_retvals, dbl_retvals, return_on_stack = classify_return_value v in
      if return_on_stack then
        let byte_count = Type_utils.get_size (Tacky.type_of_val v) in
        let get_ptr = Assembly.Mov (Quadword, Memory (BP, -8), Reg AX) in
        let copy_into_ptr =
          copy_bytes (convert_val v) (Assembly.Memory (AX, 0)) ~byte_count
        in
        (get_ptr :: copy_into_ptr) @ [ Ret ]
      else
        let open Assembly in
        let return_ints =
          List.concat
            (List.mapi
               (fun i (t, op) ->
                 let dst_reg = List.at [ AX; DX ] i in
                 match t with
                 | ByteArray { size; _ } ->
                     copy_bytes_to_reg op dst_reg
                       ~byte_count:size (* copy_thru_redzone op dst_reg size *)
                 | _ -> [ Mov (t, op, Reg dst_reg) ])
               int_retvals)
        in
        let return_dbls =
          List.mapi
            (fun i op -> Mov (Double, op, Reg (List.at [ XMM0; XMM1 ] i)))
            dbl_retvals
        in
        return_ints @ return_dbls @ [ Ret ]

let convert_instruction = function
  | Tacky.Copy { src; dst } when Type_utils.is_scalar (Tacky.type_of_val src) ->
      let t = asm_type src in
      let asm_src = convert_val src in
      let asm_dst = convert_val dst in
      Assembly.[ Mov (t, asm_src, asm_dst) ]
  | Tacky.Copy { src; dst } ->
      let asm_src = convert_val src in
      let asm_dst = convert_val dst in
      let byte_count = Type_utils.get_size (Tacky.type_of_val src) in
      copy_bytes asm_src asm_dst ~byte_count
  | Tacky.Return maybe_val -> convert_return_instruction maybe_val
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
  | Tacky.Unary { op = Negate; src; dst } when Tacky.type_of_val src = Double ->
      let asm_src = convert_val src in
      let asm_dst = convert_val dst in
      let negative_zero = add_constant ~alignment:16 (-0.0) in
      [
        Mov (Double, asm_src, asm_dst);
        Binary
          { op = Xor; t = Double; src = Data (negative_zero, 0); dst = asm_dst };
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
              else Type_utils.is_signed (Tacky.type_of_val src1)
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
          if Type_utils.is_signed (Tacky.type_of_val src1) then
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
          let is_signed = Type_utils.is_signed (Tacky.type_of_val src1) in
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
  | Tacky.Load { src_ptr; dst }
    when Type_utils.is_scalar (Tacky.type_of_val dst) ->
      let asm_src_ptr = convert_val src_ptr in
      let asm_dst = convert_val dst in
      let t = asm_type dst in
      [ Mov (Quadword, asm_src_ptr, Reg R9); Mov (t, Memory (R9, 0), asm_dst) ]
  | Tacky.Load { src_ptr; dst } ->
      let asm_src_ptr = convert_val src_ptr in
      let asm_dst = convert_val dst in
      let byte_count = Type_utils.get_size (Tacky.type_of_val dst) in
      Mov (Quadword, asm_src_ptr, Reg R9)
      :: copy_bytes (Memory (R9, 0)) asm_dst ~byte_count
  | Tacky.Store { src; dst_ptr }
    when Type_utils.is_scalar (Tacky.type_of_val src) ->
      let asm_src = convert_val src in
      let t = asm_type src in
      let asm_dst_ptr = convert_val dst_ptr in
      [ Mov (Quadword, asm_dst_ptr, Reg R9); Mov (t, asm_src, Memory (R9, 0)) ]
  | Tacky.Store { src; dst_ptr } ->
      let asm_src = convert_val src in
      let asm_dst_ptr = convert_val dst_ptr in
      let byte_count = Type_utils.get_size (Tacky.type_of_val src) in
      Mov (Quadword, asm_dst_ptr, Reg R9)
      :: copy_bytes asm_src (Memory (R9, 0)) ~byte_count
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
      if Tacky.type_of_val src = Types.UChar then
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
      else if Tacky.type_of_val src = Types.UInt then
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
      if Tacky.type_of_val dst = Types.UChar then
        [ Cvttsd2si (Longword, asm_src, Reg R9); Mov (Byte, Reg R9, asm_dst) ]
      else if Tacky.type_of_val dst = Types.UInt then
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
        let x = Assembly.(Reg XMM7) in
        [
          Cmp (Double, Data (upper_bound, 0), asm_src);
          JmpCC (AE, out_of_bounds);
          Cvttsd2si (Quadword, asm_src, asm_dst);
          Jmp end_lbl;
          Label out_of_bounds;
          Mov (Double, asm_src, x);
          Binary { op = Sub; t = Double; src = Data (upper_bound, 0); dst = x };
          Cvttsd2si (Quadword, x, asm_dst);
          Binary
            { op = Add; t = Quadword; src = upper_bound_as_int; dst = asm_dst };
          Label end_lbl;
        ]
  | CopyToOffset { src; dst; offset }
    when Type_utils.is_scalar (Tacky.type_of_val src) ->
      [ Mov (asm_type src, convert_val src, PseudoMem (dst, offset)) ]
  | CopyToOffset { src; dst; offset } ->
      let asm_src = convert_val src in
      let asm_dst = Assembly.PseudoMem (dst, offset) in
      let byte_count = Type_utils.get_size (Tacky.type_of_val src) in
      copy_bytes asm_src asm_dst ~byte_count
  | CopyFromOffset { src; dst; offset }
    when Type_utils.is_scalar (Tacky.type_of_val dst) ->
      [ Mov (asm_type dst, PseudoMem (src, offset), convert_val dst) ]
  | CopyFromOffset { src; dst; offset } ->
      let asm_src = Assembly.PseudoMem (src, offset) in
      let asm_dst = convert_val dst in
      let byte_count = Type_utils.get_size (Tacky.type_of_val dst) in
      copy_bytes asm_src asm_dst ~byte_count
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

let pass_params param_list return_on_stack =
  let int_reg_params, dbl_reg_params, stack_params =
    classify_parameters param_list return_on_stack
  in
  let copy_dst_ptr, remaining_int_regs =
    if return_on_stack then
      ( [ Assembly.Mov (Quadword, Reg DI, Memory (BP, -8)) ],
        List.tl int_param_passing_regs )
    else ([], int_param_passing_regs)
  in
  (* pass parameter in register *)
  let pass_in_int_register idx (param_t, param) =
    let r = List.at remaining_int_regs idx in
    match param_t with
    | Assembly.ByteArray { size; _ } ->
        copy_bytes_from_reg r param ~byte_count:size
    | _ -> [ Assembly.Mov (param_t, Reg r, param) ]
  in
  let pass_in_dbl_register idx param =
    let r = List.at dbl_param_passing_regs idx in
    Assembly.Mov (Double, Reg r, param)
  in
  let pass_on_stack idx (param_t, param) =
    (* first param passed on stack has idx 0 and is passed at 16(%rbp) *)
    let stk = Assembly.Memory (BP, 16 + (8 * idx)) in
    match param_t with
    | Assembly.ByteArray { size; _ } -> copy_bytes stk param ~byte_count:size
    | _ -> [ Assembly.Mov (param_t, stk, param) ]
  in
  copy_dst_ptr
  @ List.concat (List.mapi pass_in_int_register int_reg_params)
  @ List.mapi pass_in_dbl_register dbl_reg_params
  @ List.concat (List.mapi pass_on_stack stack_params)

let returns_on_stack fn_name =
  match (Symbols.get fn_name).t with
  | Types.FunType { ret_type = Structure tag; _ } -> (
      match classify_type tag with Mem :: _ -> true | _ -> false)
  | Types.FunType { ret_type = Union tag; _ } -> (
      match classify_type tag with Mem :: _ -> true | _ -> false)
  | FunType _ -> false
  | _ -> failwith "Internal error: not a function name" [@coverage off]

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
      let return_on_stack = returns_on_stack name in
      let params_as_tacky = List.map (fun name -> Tacky.Var name) params in
      let instructions =
        pass_params params_as_tacky return_on_stack
        @ List.concat_map convert_instruction body
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
  | Symbols.
      { t = Types.FunType { ret_type; _ }; attrs = FunAttr { defined; _ } } ->
      (* If this function has incomplete return type (implying we don't define or call it in this translation unit)
       * use a dummy value for fun_returns_on_stack *)
      let fun_returns_on_stack =
        if Type_utils.is_complete ret_type || ret_type = Void then
          returns_on_stack name
        else false
      in
      Assembly_symbols.add_fun name defined fun_returns_on_stack
  | { t; attrs = ConstAttr _ } ->
      Assembly_symbols.add_constant name (convert_type t)
  (* use dummy type for static variables of incomplete type *)
  | Symbols.{ t; attrs = StaticAttr _ } when not (Type_utils.is_complete t) ->
      Assembly_symbols.add_var name Byte true
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
