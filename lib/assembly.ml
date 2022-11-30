[@@@coverage exclude_file]

type reg =
  | AX
  | BX
  | CX
  | DX
  | DI
  | SI
  | R8
  | R9
  | R10
  | R11
  | R12
  | R13
  | R14
  | R15
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
  | XMM8
  | XMM9
  | XMM10
  | XMM11
  | XMM12
  | XMM13
  | XMM14
  | XMM15
[@@deriving show { with_path = false }]

type operand =
  | Imm of int64
  | Reg of reg
  | Pseudo of string
  | Memory of reg * int
  | Data of string * int
  | PseudoMem of string * int
  | Indexed of { base : reg; index : reg; scale : int }
[@@deriving show { with_path = false }]

type unary_operator = Neg | Not | Shr [@@deriving show { with_path = false }]

type binary_operator =
  | Add
  | Sub
  | Mult
  | DivDouble
  | And
  | Or
  | Xor
  | Shl
  | ShrBinop
[@@deriving show { with_path = false }]

type cond_code = E | NE | G | GE | L | LE | A | AE | B | BE
[@@deriving show { with_path = false }]

type asm_type =
  | Byte
  | Longword
  | Quadword
  | Double
  | ByteArray of { size : int; alignment : int }
[@@deriving show { with_path = false }]

type instruction =
  | Mov of asm_type * operand * operand
  | Movsx of {
      src_type : asm_type;
      dst_type : asm_type;
      src : operand;
      dst : operand;
    }
  | MovZeroExtend of {
      src_type : asm_type;
      dst_type : asm_type;
      src : operand;
      dst : operand;
    }
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
  | Pop of reg
  | Call of string
  | Ret
[@@deriving show { with_path = false }]

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
      init : Initializers.static_init list;
    }
  | StaticConstant of {
      name : string;
      alignment : int;
      init : Initializers.static_init;
    }

type t = Program of top_level list
