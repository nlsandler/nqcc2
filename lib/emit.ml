open Assembly

let align_directive =
  match !Settings.platform with OS_X -> ".balign" | Linux -> ".align"

let show_label name =
  match !Settings.platform with OS_X -> "_" ^ name | Linux -> name

let show_local_label label =
  match !Settings.platform with OS_X -> "L" ^ label | Linux -> ".L" ^ label

let show_fun_name f =
  match !Settings.platform with
  | OS_X -> "_" ^ f
  | Linux -> if Symbols.is_defined f then f else f ^ "@PLT"

let show_reg = function
  | AX -> "%eax"
  | CX -> "%ecx"
  | DX -> "%edx"
  | DI -> "%edi"
  | SI -> "%esi"
  | R8 -> "%r8d"
  | R9 -> "%r9d"
  | R10 -> "%r10d"
  | R11 -> "%r11d"

let show_operand = function
  | Reg r -> show_reg r
  | Imm i -> Printf.sprintf "$%d" i
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

let show_byte_operand = function
  | Reg r -> show_byte_reg r
  | other -> show_operand other

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

let show_quadword_operand = function
  | Reg r -> show_quadword_reg r
  | other -> show_operand other

let show_unary_instruction = function Neg -> "negl" | Not -> "notl"

let show_binary_instruction = function
  | Add -> "addl"
  | Sub -> "subl"
  | Mult -> "imull"

let show_cond_code = function
  | E -> "e"
  | NE -> "ne"
  | G -> "g"
  | GE -> "ge"
  | L -> "l"
  | LE -> "le"

let emit_instruction chan = function
  | Mov (src, dst) ->
      Printf.fprintf chan "\tmovl %s, %s\n" (show_operand src)
        (show_operand dst)
  | Unary (operator, dst) ->
      Printf.fprintf chan "\t%s %s\n"
        (show_unary_instruction operator)
        (show_operand dst)
  | Binary { op; src; dst } ->
      Printf.fprintf chan "\t%s %s, %s\n"
        (show_binary_instruction op)
        (show_operand src) (show_operand dst)
  | Cmp (src, dst) ->
      Printf.fprintf chan "\tcmpl %s, %s\n" (show_operand src)
        (show_operand dst)
  | Idiv operand -> Printf.fprintf chan "\tidivl %s\n" (show_operand operand)
  | Cdq -> Printf.fprintf chan "\tcdq\n"
  | Jmp lbl -> Printf.fprintf chan "\tjmp %s\n" (show_local_label lbl)
  | JmpCC (code, lbl) ->
      Printf.fprintf chan "\tj%s %s\n" (show_cond_code code)
        (show_local_label lbl)
  | SetCC (code, operand) ->
      Printf.fprintf chan "\tset%s %s\n" (show_cond_code code)
        (show_byte_operand operand)
  | Label lbl -> Printf.fprintf chan "%s:\n" (show_local_label lbl)
  | AllocateStack i -> Printf.fprintf chan "\tsubq $%d, %%rsp\n" i
  | DeallocateStack i -> Printf.fprintf chan "\taddq $%d, %%rsp\n" i
  | Push op -> Printf.fprintf chan "\tpushq %s\n" (show_quadword_operand op)
  | Call f -> Printf.fprintf chan "\tcall %s\n" (show_fun_name f)
  | Ret ->
      Printf.fprintf chan {|
    movq %%rbp, %%rsp
    popq %%rbp
    ret
|}

let emit_global_directive chan global label =
  if global then Printf.fprintf chan "\t.globl %s\n" label else ()

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
  | StaticVariable { name; global; init = 0 } ->
      let label = show_label name in
      emit_global_directive chan global label;
      Printf.fprintf chan
        {|
    .bss
    %s 4
%s:
    .zero 4
|}
        align_directive label
  | StaticVariable { name; global; init } ->
      let label = show_label name in
      emit_global_directive chan global label;
      Printf.fprintf chan {|
  .data
  %s 4
%s:
  .long %d
|} align_directive
        label init

let emit_stack_note chan =
  match !Settings.platform with
  | OS_X -> ()
  | Linux -> Printf.fprintf chan "\t.section .note.GNU-stack,\"\",@progbits\n"

let emit assembly_file (Program tls) =
  let output_channel = open_out assembly_file in
  List.iter (emit_tl output_channel) tls;
  emit_stack_note output_channel;
  close_out output_channel
