[@@@coverage exclude_file]

type unary_operator = Complement | Negate | Not [@@deriving show]

type binary_operator =
  | Add
  | Subtract
  | Multiply
  | Divide
  | Mod
  | BitwiseAnd
  | BitwiseOr
  | BitwiseXor
  | BitshiftLeft
  | BitshiftRight
  | Equal
  | NotEqual
  | LessThan
  | LessOrEqual
  | GreaterThan
  | GreaterOrEqual
[@@deriving show]

type tacky_val = Constant of int | Var of string [@@deriving show]

type instruction =
  | Return of tacky_val
  | Unary of { op : unary_operator; src : tacky_val; dst : tacky_val }
  | Binary of {
      op : binary_operator;
      src1 : tacky_val;
      src2 : tacky_val;
      dst : tacky_val;
    }
  | Copy of { src : tacky_val; dst : tacky_val }
  | Jump of string
  | JumpIfZero of tacky_val * string
  | JumpIfNotZero of tacky_val * string
  | Label of string
[@@deriving show]

type function_definition =
  | Function of { name : string; body : instruction list }
[@@deriving show]

type t = Program of function_definition [@@deriving show]
