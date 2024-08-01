open Assembly

let show_operand = function
  | Reg AX -> "%eax"
  | Reg DX -> "%edx"
  | Reg R10 -> "%r10d"
  | Reg R11 -> "%r11d"
  | Imm i -> Printf.sprintf "$%d" i
  | Stack i -> Printf.sprintf "%d(%%rbp)" i
  (* printing out pseudoregisters is only for debugging *)
  | Pseudo name -> Printf.sprintf "%%%s" name [@coverage off]

let show_label name =
  match !Settings.platform with OS_X -> "_" ^ name | Linux -> name

let show_unary_instruction = function Neg -> "negl" | Not -> "notl"

let show_binary_instruction = function
  | Add -> "addl"
  | Sub -> "subl"
  | Mult -> "imull"

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
  | Idiv operand -> Printf.fprintf chan "\tidivl %s\n" (show_operand operand)
  | Cdq -> Printf.fprintf chan "\tcdq\n"
  | AllocateStack i -> Printf.fprintf chan "\tsubq $%d, %%rsp\n" i
  | Ret ->
      Printf.fprintf chan {|
    movq %%rbp, %%rsp
    popq %%rbp
    ret
|}

let emit_function chan (Function { name; instructions }) =
  let label = show_label name in
  Printf.fprintf chan
    {|
    .globl %s
%s:
    pushq %%rbp
    movq %%rsp, %%rbp
|}
    label label;
  List.iter (emit_instruction chan) instructions

let emit_stack_note chan =
  match !Settings.platform with
  | OS_X -> ()
  | Linux -> Printf.fprintf chan "\t.section .note.GNU-stack,\"\",@progbits\n"

let emit assembly_file (Program function_def) =
  let output_channel = open_out assembly_file in
  emit_function output_channel function_def;
  emit_stack_note output_channel;
  close_out output_channel
