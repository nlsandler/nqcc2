type reg = AX | CX | DX | DI | SI | R8 | R9 | R10 | R11
type operand = Imm of int | Reg of reg | Pseudo of string | Stack of int
type unary_operator = Neg | Not
type binary_operator = Add | Sub | Mult
type cond_code = E | NE | G | GE | L | LE

type instruction =
  | Mov of operand * operand
  | Unary of unary_operator * operand
  | Binary of { op : binary_operator; src : operand; dst : operand }
  | Cmp of operand * operand
  | Idiv of operand
  | Cdq
  | Jmp of string
  | JmpCC of cond_code * string
  | SetCC of cond_code * operand
  | Label of string
  | AllocateStack of int
  | DeallocateStack of int
  | Push of operand
  | Call of string
  | Ret

type function_definition =
  | Function of { name : string; instructions : instruction list }

type t = Program of function_definition list
