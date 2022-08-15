type reg = AX | CX | DX | DI | SI | R8 | R9 | R10 | R11

type operand =
  | Imm of int
  | Reg of reg
  | Pseudo of string
  | Stack of int
  | Data of string

type unary_operator = Neg | Not
type binary_operator = Add | Sub | Mult | And | Or | Xor | Sal | Sar
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

type top_level =
  | Function of {
      name : string;
      global : bool;
      instructions : instruction list;
    }
  | StaticVariable of { name : string; global : bool; init : int }

type t = Program of top_level list
