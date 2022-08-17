type reg =
  | AX
  | CX
  | DX
  | DI
  | SI
  | R8
  | R9
  | R10
  | R11
  | SP
  | BP
  | XMM0
  | XMM1
  | XMM2
  | XMM3
  | XMM4
  | XMM5
  | XMM6
  | XMM7
  | XMM14
  | XMM15

type operand =
  | Imm of int64
  | Reg of reg
  | Pseudo of string
  | Memory of reg * int
  | Data of string

type unary_operator = Neg | Not | ShrOneOp

type binary_operator =
  | Add
  | Sub
  | Mult
  | DivDouble
  | And
  | Or
  | Xor
  | Sal
  | Sar
  | Shr
  | Shl

type cond_code = E | NE | G | GE | L | LE | A | AE | B | BE | P | NP
type asm_type = Longword | Quadword | Double

type instruction =
  | Mov of asm_type * operand * operand
  | Movsx of operand * operand
  | MovZeroExtend of operand * operand
  | Lea of operand * operand
  | Cvttsd2si of asm_type * operand * operand
  | Cvtsi2sd of asm_type * operand * operand
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
  | StaticConstant of {
      name : string;
      alignment : int;
      init : Initializers.static_init;
    }

type t = Program of top_level list
