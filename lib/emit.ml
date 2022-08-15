open Assembly

let suffix = function Longword -> "l" | Quadword -> "q"

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

let show_operand t = function
  | Reg r -> (
      match t with
      | Longword -> show_long_reg r
      | Quadword -> show_quadword_reg r)
  | Imm i -> Printf.sprintf "$%s" (Int64.to_string i)
  | Stack i -> Printf.sprintf "%d(%%rbp)" i
  | Data name -> Printf.sprintf "%s(%%rip)" (show_label name)
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

let show_byte_operand = function
  | Reg r -> show_byte_reg r
  | other -> show_operand Longword other

let show_unary_instruction = function Neg -> "neg" | Not -> "not"

let show_binary_instruction = function
  | Add -> "add"
  | Sub -> "sub"
  | Mult -> "imul"

let show_cond_code = function
  | E -> "e"
  | NE -> "ne"
  | G -> "g"
  | GE -> "ge"
  | L -> "l"
  | LE -> "le"

let emit_instruction chan = function
  | Mov (t, src, dst) ->
      Printf.fprintf chan "\tmov%s %s, %s\n" (suffix t) (show_operand t src)
        (show_operand t dst)
  | Unary (operator, t, dst) ->
      Printf.fprintf chan "\t%s%s %s\n"
        (show_unary_instruction operator)
        (suffix t) (show_operand t dst)
  | Binary { op; t; src; dst } ->
      Printf.fprintf chan "\t%s%s %s, %s\n"
        (show_binary_instruction op)
        (suffix t) (show_operand t src) (show_operand t dst)
  | Cmp (t, src, dst) ->
      Printf.fprintf chan "\tcmp%s %s, %s\n" (suffix t) (show_operand t src)
        (show_operand t dst)
  | Idiv (t, operand) ->
      Printf.fprintf chan "\tidiv%s %s\n" (suffix t) (show_operand t operand)
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
  | Push op -> Printf.fprintf chan "\tpushq %s\n" (show_operand Quadword op)
  | Call f -> Printf.fprintf chan "\tcall %s\n" (show_fun_name f)
  | Movsx (src, dst) ->
      Printf.fprintf chan "\tmovslq %s, %s\n"
        (show_operand Longword src)
        (show_operand Quadword dst)
  | Ret ->
      Printf.fprintf chan {|
    movq %%rbp, %%rsp
    popq %%rbp
    ret
|}

let emit_global_directive chan global label =
  if global then Printf.fprintf chan "\t.globl %s\n" label else ()

let emit_zero_init chan = function
  | Initializers.IntInit _ -> Printf.fprintf chan "\t.zero 4\n"
  | LongInit _ -> Printf.fprintf chan "\t.zero 8\n"

let emit_init chan = function
  | Initializers.IntInit i ->
      Printf.fprintf chan "\t.long %s\n" (Int32.to_string i)
  | LongInit l -> Printf.fprintf chan "\t.quad %s\n" (Int64.to_string l)

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

let emit_stack_note chan =
  match !Settings.platform with
  | OS_X -> ()
  | Linux -> Printf.fprintf chan "\t.section .note.GNU-stack,\"\",@progbits\n"

let emit assembly_file (Program tls) =
  let output_channel = open_out assembly_file in
  List.iter (emit_tl output_channel) tls;
  emit_stack_note output_channel;
  close_out output_channel
