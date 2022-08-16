type reg = AX | CX | DX | DI | SI | R8 | R9 | R10 | R11 | SP

type operand =
  | Imm of int64
  | Reg of reg
  | Pseudo of string
  | Stack of int
  | Data of string

type unary_operator = Neg | Not
type binary_operator = Add | Sub | Mult
type cond_code = E | NE | G | GE | L | LE | A | AE | B | BE
type asm_type = Longword | Quadword

type instruction =
  | Mov of asm_type * operand * operand
  | Movsx of operand * operand
  | MovZeroExtend of operand * operand
  | Unary of unary_operator * asm_type * operand
  | Binary of {
      op : binary_operator;
      t : asm_type;
      src : operand;
      dst : operand;
    }
  | Cmp of asm_type * operand * operand
  | Idiv of asm_type * operand
  | Div of asm_type * operand
  | Cdq of asm_type
  | Jmp of string
  | JmpCC of cond_code * string
  | SetCC of cond_code * operand
  | Label of string
  | Push of operand
  | Call of string
  | Ret

type top_level =
  | Function of {
      name : string;
      global : bool;
      instructions : instruction list;
    }
  | StaticVariable of {
      name : string;
      alignment : int;
      global : bool;
      init : Initializers.static_init;
    }

type t = Program of top_level list
