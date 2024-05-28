open Assembly
open Cnums

let suffix = function Longword -> "l" | Quadword -> "q" | Double -> "sd"

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
  | CX -> "%ecx"
  | DX -> "%edx"
  | DI -> "%edi"
  | SI -> "%esi"
  | R8 -> "%r8d"
  | R9 -> "%r9d"
  | R10 -> "%r10d"
  | R11 -> "%r11d"
  | SP -> failwith "Internal error: no 32-bit RSP" [@coverage off]
  | _ ->
      failwith "Internal error: can't store longword type in XMM register"
      [@coverage off]

let show_quadword_reg = function
  | AX -> "%rax"
  | CX -> "%rcx"
  | DX -> "%rdx"
  | DI -> "%rdi"
  | SI -> "%rsi"
  | R8 -> "%r8"
  | R9 -> "%r9"
  | R10 -> "%r10"
  | R11 -> "%r11"
  | SP -> "%rsp"
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
  | XMM14 -> "%xmm14"
  | XMM15 -> "%xmm15"
  | _ ->
      failwith
        "Internal error: can't store double type in general-purpose register"
      [@coverage off]

let show_operand t = function
  | Reg r -> (
      match t with
      | Longword -> show_long_reg r
      | Quadword -> show_quadword_reg r
      | Double -> show_double_reg r)
  | Imm i -> Printf.sprintf "$%s" (Int64.to_string i)
  | Stack i -> Printf.sprintf "%d(%%rbp)" i
  | Data name ->
      let lbl =
        if Assembly_symbols.is_constant name then show_local_label name
        else show_label name
      in
      Printf.sprintf "%s(%%rip)" lbl
  (* printing out pseudoregisters is only for debugging *)
  | Pseudo name -> Printf.sprintf "%%%s" name [@coverage off]

let show_byte_reg = function
  | AX -> "%al"
  | CX -> "%cl"
  | DX -> "%dl"
  | DI -> "%dil"
  | SI -> "%sil"
  | R8 -> "%r8b"
  | R9 -> "%r9b"
  | R10 -> "%r10b"
  | R11 -> "%r11b"
  | SP -> failwith "Internal error: no one-byte RSP" [@coverage off]
  | _ ->
      failwith "Internal error: can't store byte type in XMM register"
      [@coverage off]

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
  | Call f -> Printf.fprintf chan "\tcall %s\n" (show_fun_name f)
  | Movsx (src, dst) ->
      Printf.fprintf chan "\tmovslq %s, %s\n"
        (show_operand Longword src)
        (show_operand Quadword dst)
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
  | Cdq Double ->
      failwith "Internal error: can't apply cdq to double type" [@coverage off]
  | MovZeroExtend _ ->
      failwith
        "Internal error: MovZeroExtend should have been removed in instruction \
         rewrite pass" [@coverage off]

let emit_global_directive chan global label =
  if global then Printf.fprintf chan "\t.globl %s\n" label else ()

let emit_zero_init chan = function
  | Initializers.IntInit _ | UIntInit _ -> Printf.fprintf chan "\t.zero 4\n"
  | LongInit _ | ULongInit _ -> Printf.fprintf chan "\t.zero 8\n"
  | DoubleInit _ ->
      failwith "internal error: shoud never use zeroinit for doubles"
      [@coverage off]

let emit_init chan = function
  | Initializers.IntInit i ->
      Printf.fprintf chan "\t.long %s\n" (Int32.to_string i)
  | LongInit l -> Printf.fprintf chan "\t.quad %Ld\n" l
  | UIntInit u -> Printf.fprintf chan "\t.long %s\n" (UInt32.to_string u)
  | ULongInit l -> Printf.fprintf chan "\t.quad %s\n" (UInt64.to_string l)
  | DoubleInit d -> Printf.fprintf chan "\t.quad %Ld\n" (Int64.bits_of_float d)

let emit_constant chan name alignment init =
  let constant_section_name =
    match !Settings.platform with
    | Linux -> ".section .rodata"
    | OS_X ->
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
    when Initializers.is_zero init ->
      let label = show_label name in
      emit_global_directive chan global label;
      Printf.fprintf chan
        {|
    .bss
    %s %d
%s:
|}
        align_directive alignment label;
      emit_zero_init chan init
  | StaticVariable { name; global; init; alignment } ->
      let label = show_label name in
      emit_global_directive chan global label;
      Printf.fprintf chan {|
  .data
  %s %d
%s:
|} align_directive alignment
        label;
      emit_init chan init
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
