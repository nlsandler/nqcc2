type reg = AX | DX | R10 | R11
type operand = Imm of int | Reg of reg | Pseudo of string | Stack of int
type unary_operator = Neg | Not
type binary_operator = Add | Sub | Mult

type instruction =
  | Mov of operand * operand
  | Unary of unary_operator * operand
  | Binary of { op : binary_operator; src : operand; dst : operand }
  | Idiv of operand
  | Cdq
  | AllocateStack of int
  | Ret

type function_definition =
  | Function of { name : string; instructions : instruction list }

type t = Program of function_definition
