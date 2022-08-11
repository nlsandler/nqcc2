type operand = Imm of int | Register
type instruction = Mov of operand * operand | Ret

type function_definition =
  | Function of { name : string; instructions : instruction list }

type t = Program of function_definition
