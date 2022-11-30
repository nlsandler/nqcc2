open Batteries
open Assembly
open Cnums

let suffix = function
  | Byte -> "b"
  | Longword -> "l"
  | Quadword -> "q"
  | Double -> "sd"
  | ByteArray _ ->
      failwith "Internal error: found instruction w/ non-scalar operand type"
      [@coverage off]

let align_directive =
  match !Settings.platform with OS_X -> ".balign" | Linux -> ".align"

let show_label name =
  match !Settings.platform with OS_X -> "_" ^ name | Linux -> name

let show_local_label label =
  match !Settings.platform with OS_X -> "L" ^ label | Linux -> ".L" ^ label

let show_fun_name f =
  match !Settings.platform with
  | OS_X -> "_" ^ f
  | Linux -> if Assembly_symbols.is_defined f then f else f ^ "@PLT"

let show_long_reg = function
  | AX -> "%eax"
  | BX -> "%ebx"
  | CX -> "%ecx"
  | DX -> "%edx"
  | DI -> "%edi"
  | SI -> "%esi"
  | R8 -> "%r8d"
  | R9 -> "%r9d"
  | R10 -> "%r10d"
  | R11 -> "%r11d"
  | R12 -> "%r12d"
  | R13 -> "%r13d"
  | R14 -> "%r14d"
  | R15 -> "%r15d"
  | SP -> failwith "Internal error: no 32-bit RSP" [@coverage off]
  | BP -> failwith "Internal error: no 32-bit RBP" [@coverage off]
  | _ ->
      failwith "Internal error: can't store longword type in XMM register"
      [@coverage off]

let show_quadword_reg = function
  | AX -> "%rax"
  | BX -> "%rbx"
  | CX -> "%rcx"
  | DX -> "%rdx"
  | DI -> "%rdi"
  | SI -> "%rsi"
  | R8 -> "%r8"
  | R9 -> "%r9"
  | R10 -> "%r10"
  | R11 -> "%r11"
  | R12 -> "%r12"
  | R13 -> "%r13"
  | R14 -> "%r14"
  | R15 -> "%r15"
  | SP -> "%rsp"
  | BP -> "%rbp"
  | _ ->
      failwith "Internal error: can't store quadword type in XMM register"
      [@coverage off]

let show_double_reg = function
  | XMM0 -> "%xmm0"
  | XMM1 -> "%xmm1"
  | XMM2 -> "%xmm2"
  | XMM3 -> "%xmm3"
  | XMM4 -> "%xmm4"
  | XMM5 -> "%xmm5"
  | XMM6 -> "%xmm6"
  | XMM7 -> "%xmm7"
  | XMM8 -> "%xmm8"
  | XMM9 -> "%xmm9"
  | XMM10 -> "%xmm10"
  | XMM11 -> "%xmm11"
  | XMM12 -> "%xmm12"
  | XMM13 -> "%xmm13"
  | XMM14 -> "%xmm14"
  | XMM15 -> "%xmm15"
  | _ ->
      failwith
        "Internal error: can't store double type in general-purpose register"
      [@coverage off]

let show_byte_reg = function
  | AX -> "%al"
  | BX -> "%bl"
  | CX -> "%cl"
  | DX -> "%dl"
  | DI -> "%dil"
  | SI -> "%sil"
  | R8 -> "%r8b"
  | R9 -> "%r9b"
  | R10 -> "%r10b"
  | R11 -> "%r11b"
  | R12 -> "%r12b"
  | R13 -> "%r13b"
  | R14 -> "%r14b"
  | R15 -> "%r15b"
  | SP -> failwith "Internal error: no one-byte RSP" [@coverage off]
  | BP -> failwith "Internal error: no one-byte RBP" [@coverage off]
  | _ ->
      failwith "Internal error: can't store byte type in XMM register"
      [@coverage off]

let show_operand t = function
  | Reg r -> (
      match t with
      | Byte -> show_byte_reg r
      | Longword -> show_long_reg r
      | Quadword -> show_quadword_reg r
      | Double -> show_double_reg r
      | ByteArray _ ->
          failwith "Internal error: can't store non-scalar operand in register"
          [@coverage off])
  | Imm i -> Printf.sprintf "$%s" (Int64.to_string i)
  | Memory (r, 0) -> Printf.sprintf "(%s)" (show_quadword_reg r)
  | Memory (r, i) -> Printf.sprintf "%d(%s)" i (show_quadword_reg r)
  | Data (name, offset) ->
      let lbl =
        if Assembly_symbols.is_constant name then show_local_label name
        else show_label name
      in

      if offset = 0 then Printf.sprintf "%s(%%rip)" lbl
      else Printf.sprintf "%s+%d(%%rip)" lbl offset
  | Indexed { base; index; scale } ->
      Printf.sprintf "(%s, %s, %d)" (show_quadword_reg base)
        (show_quadword_reg index) scale
  (* printing out pseudoregisters is only for debugging *)
  | Pseudo name -> Printf.sprintf "%%%s" name [@coverage off]
  | PseudoMem (name, offset) ->
      Printf.sprintf "%d(%%%s)" offset name [@coverage off]

let show_byte_operand = function
  | Reg r -> show_byte_reg r
  | other -> show_operand Longword other

let show_unary_instruction = function
  | Neg -> "neg"
  | Not -> "not"
  | ShrOneOp -> "shr"

let show_binary_instruction = function
  | Add -> "add"
  | Sub -> "sub"
  | Mult -> "imul"
  | DivDouble -> "div"
  | And -> "and"
  | Or -> "or"
  | Xor -> "xor" (* int xor only; double xor is a special case *)
  | Sal -> "sal"
  | Sar -> "sar"
  | Shl -> "shl"
  | Shr -> "shr"

let show_cond_code = function
  | E -> "e"
  | NE -> "ne"
  | G -> "g"
  | GE -> "ge"
  | L -> "l"
  | LE -> "le"
  | A -> "a"
  | AE -> "ae"
  | B -> "b"
  | BE -> "be"
  | P -> "p"
  | NP -> "np"

let emit_instruction chan = function
  | Mov (t, src, dst) ->
      Printf.fprintf chan "\tmov%s %s, %s\n" (suffix t) (show_operand t src)
        (show_operand t dst)
  | Unary (operator, t, dst) ->
      Printf.fprintf chan "\t%s%s %s\n"
        (show_unary_instruction operator)
        (suffix t) (show_operand t dst)
  (* special logic: emit CX reg as %cl *)
  | Binary { op = (Sal | Sar | Shl | Shr) as op; t; src; dst } ->
      Printf.fprintf chan "\t%s%s %s, %s\n"
        (show_binary_instruction op)
        (suffix t) (show_byte_operand src) (show_operand t dst)
  | Binary { op = Xor; t = Double; src; dst } ->
      Printf.fprintf chan "\txorpd %s, %s\n" (show_operand Double src)
        (show_operand Double dst)
  | Binary { op = Mult; t = Double; src; dst } ->
      Printf.fprintf chan "\tmulsd %s, %s\n" (show_operand Double src)
        (show_operand Double dst)
  | Binary { op; t; src; dst } ->
      Printf.fprintf chan "\t%s%s %s, %s\n"
        (show_binary_instruction op)
        (suffix t) (show_operand t src) (show_operand t dst)
  | Cmp (Double, src, dst) ->
      Printf.fprintf chan "\tcomisd %s, %s\n" (show_operand Double src)
        (show_operand Double dst)
  | Cmp (t, src, dst) ->
      Printf.fprintf chan "\tcmp%s %s, %s\n" (suffix t) (show_operand t src)
        (show_operand t dst)
  | Idiv (t, operand) ->
      Printf.fprintf chan "\tidiv%s %s\n" (suffix t) (show_operand t operand)
  | Div (t, operand) ->
      Printf.fprintf chan "\tdiv%s %s\n" (suffix t) (show_operand t operand)
  | Lea (src, dst) ->
      Printf.fprintf chan "\tleaq %s, %s\n"
        (show_operand Quadword src)
        (show_operand Quadword dst)
  | Cdq Longword -> Printf.fprintf chan "\tcdq\n"
  | Cdq Quadword -> Printf.fprintf chan "\tcqo\n"
  | Jmp lbl -> Printf.fprintf chan "\tjmp %s\n" (show_local_label lbl)
  | JmpCC (code, lbl) ->
      Printf.fprintf chan "\tj%s %s\n" (show_cond_code code)
        (show_local_label lbl)
  | SetCC (code, operand) ->
      Printf.fprintf chan "\tset%s %s\n" (show_cond_code code)
        (show_byte_operand operand)
  | Label lbl -> Printf.fprintf chan "%s:\n" (show_local_label lbl)
  | Push op -> (
      try Printf.fprintf chan "\tpushq %s\n" (show_operand Quadword op)
      with Failure _ ->
        (* For intermediate/debug output only *)
        Printf.fprintf chan "\tpushq %s\n" (show_operand Double op))
  | Pop r -> Printf.fprintf chan "\tpopq %s\n" (show_quadword_reg r)
  | Call f -> Printf.fprintf chan "\tcall %s\n" (show_fun_name f)
  | Movsx { src_type; dst_type; src; dst } ->
      Printf.fprintf chan "\tmovs%s%s %s, %s\n" (suffix src_type)
        (suffix dst_type)
        (show_operand src_type src)
        (show_operand dst_type dst)
  | MovZeroExtend { src_type; dst_type; src; dst } ->
      Printf.fprintf chan "\tmovz%s%s %s, %s\n" (suffix src_type)
        (suffix dst_type)
        (show_operand src_type src)
        (show_operand dst_type dst)
  | Cvtsi2sd (t, src, dst) ->
      Printf.fprintf chan "\tcvtsi2sd%s %s, %s\n" (suffix t)
        (show_operand t src) (show_operand Double dst)
  | Cvttsd2si (t, src, dst) ->
      Printf.fprintf chan "\tcvttsd2si%s %s, %s\n" (suffix t)
        (show_operand Double src) (show_operand t dst)
  | Ret ->
      Printf.fprintf chan {|
    movq %%rbp, %%rsp
    popq %%rbp
    ret
|}
  | Cdq (Double | Byte | ByteArray _) ->
      failwith "Internal error: can't apply cdq to a byte or non-integer type"
      [@coverage off]

let emit_global_directive chan global label =
  if global then Printf.fprintf chan "\t.globl %s\n" label else ()

let escape s =
  let escape_char c =
    if Char.is_digit c || Char.is_letter c then String.of_char c
      (* use octal escape for everything except alphanumeric values
       * make sure to pad out octal escapes to 3 digits so we don't, e.g.
       * escape "hello 1" as "hello\401" *)
    else Printf.sprintf {|\%03o|} (Char.code c)
  in
  String.concat "" (List.map escape_char (String.explode s))

let emit_init chan = function
  | Initializers.IntInit i ->
      Printf.fprintf chan "\t.long %s\n" (Int32.to_string i)
  | LongInit l -> Printf.fprintf chan "\t.quad %Ld\n" l
  | UIntInit u -> Printf.fprintf chan "\t.long %s\n" (UInt32.to_string u)
  | ULongInit l -> Printf.fprintf chan "\t.quad %s\n" (UInt64.to_string l)
  | CharInit c -> Printf.fprintf chan "\t.byte %s\n" (Int8.to_string c)
  | UCharInit uc -> Printf.fprintf chan "\t.byte %s\n" (UInt8.to_string uc)
  | DoubleInit d -> Printf.fprintf chan "\t.quad %Ld\n" (Int64.bits_of_float d)
  (* a partly-initialized array can include a mix of zero and non-zero initializaers *)
  | ZeroInit byte_count -> Printf.fprintf chan "\t.zero %d\n" byte_count
  | StringInit (s, true) -> Printf.fprintf chan "\t.asciz \"%s\"\n" (escape s)
  | StringInit (s, false) -> Printf.fprintf chan "\t.ascii \"%s\"\n" (escape s)
  | PointerInit lbl -> Printf.fprintf chan "\t.quad %s\n" (show_local_label lbl)

let emit_constant chan name alignment init =
  let constant_section_name =
    match (!Settings.platform, init) with
    | Linux, _ -> ".section .rodata"
    | OS_X, Initializers.StringInit _ -> ".cstring"
    | OS_X, _ ->
        if alignment = 8 then ".literal8"
        else if alignment = 16 then ".literal16"
        else
          failwith "Internal error: found constant with bad alignment"
          [@coverage off]
  in
  Printf.fprintf chan
    {|
    %s
    %s %d
  %s:
|}
    constant_section_name align_directive alignment (show_local_label name);
  emit_init chan init;
  (* macOS linker gets cranky if you write only 8 bytes to .literal16 section *)
  if constant_section_name = ".literal16" then
    emit_init chan (Initializers.LongInit Int64.zero)
  else ()

let emit_tl chan = function
  | Function { name; global; instructions } ->
      let label = show_label name in
      emit_global_directive chan global label;
      Printf.fprintf chan
        {|
    .text
%s:
    pushq %%rbp
    movq %%rsp, %%rbp
|}
        label;
      List.iter (emit_instruction chan) instructions
  | StaticVariable { name; global; init; alignment }
  (* is_zero is false for all doubles*)
    when List.for_all Initializers.is_zero init ->
      let label = show_label name in
      emit_global_directive chan global label;
      Printf.fprintf chan
        {|
    .bss
    %s %d
%s:
|}
        align_directive alignment label;
      List.iter (emit_init chan) init
  | StaticVariable { name; global; init; alignment } ->
      let label = show_label name in
      emit_global_directive chan global label;
      Printf.fprintf chan {|
  .data
  %s %d
%s:
|} align_directive alignment
        label;
      List.iter (emit_init chan) init
  | StaticConstant { name; alignment; init } ->
      emit_constant chan name alignment init

let emit_stack_note chan =
  match !Settings.platform with
  | OS_X -> ()
  | Linux -> Printf.fprintf chan "\t.section .note.GNU-stack,\"\",@progbits\n"

let emit assembly_file (Program tls) =
  let output_channel = open_out assembly_file in
  List.iter (emit_tl output_channel) tls;
  emit_stack_note output_channel;
  close_out output_channel
