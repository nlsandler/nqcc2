open Assembly

let fixup_instruction = function
  (* Mov can't move a value from one memory address to another *)
  | Mov ((Stack _ as src), (Stack _ as dst)) ->
      [ Mov (src, Reg R10); Mov (Reg R10, dst) ]
  (* Idiv can't operate on constants *)
  | Idiv (Imm i) -> [ Mov (Imm i, Reg R10); Idiv (Reg R10) ]
  (* Add/Sub can't use memory addresses for both operands *)
  | Binary
      { op = (Add | Sub) as op; src = Stack _ as src; dst = Stack _ as dst } ->
      [ Mov (src, Reg R10); Binary { op; src = Reg R10; dst } ]
  (* Destination of Mult can't be in memory *)
  | Binary { op = Mult; src; dst = Stack _ as dst } ->
      [
        Mov (dst, Reg R11);
        Binary { op = Mult; src; dst = Reg R11 };
        Mov (Reg R11, dst);
      ]
  (* Both operands of cmp can't be in memory *)
  | Cmp ((Stack _ as src), (Stack _ as dst)) ->
      [ Mov (src, Reg R10); Cmp (Reg R10, dst) ]
  (* Second operand of cmp can't be a constant *)
  | Cmp (src, Imm i) -> [ Mov (Imm i, Reg R11); Cmp (src, Reg R11) ]
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
