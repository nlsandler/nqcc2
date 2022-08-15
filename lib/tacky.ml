[@@@coverage exclude_file]

type unary_operator = Complement | Negate | Not [@@deriving show]

type binary_operator =
  | Add
  | Subtract
  | Multiply
  | Divide
  | Mod
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
  | FunCall of { f : string; args : tacky_val list; dst : tacky_val }
[@@deriving show]

type top_level =
  | Function of {
      name : string;
      global : bool;
      params : string list;
      body : instruction list;
    }
  | StaticVariable of { name : string; global : bool; init : int }
[@@deriving show]

type t = Program of top_level list [@@deriving show]
