open Assembly

let fixup_instruction = function
  | Mov ((Stack _ as src), (Stack _ as dst)) ->
      [ Mov (src, Reg R10); Mov (Reg R10, dst) ]
  | other -> [ other ]

let fixup_function last_stack_slot (Function { name; instructions }) =
  Function
    {
      name;
      instructions =
        AllocateStack (-last_stack_slot)
        :: List.concat_map fixup_instruction instructions;
    }

let fixup_program last_stack_slot (Program fn_def) =
  Program (fixup_function last_stack_slot fn_def)
