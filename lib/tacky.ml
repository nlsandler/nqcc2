[@@@coverage exclude_file]

type unary_operator = Complement | Negate [@@deriving show]

type binary_operator = Add | Subtract | Multiply | Divide | Mod
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
[@@deriving show]

type function_definition =
  | Function of { name : string; body : instruction list }
[@@deriving show]

type t = Program of function_definition [@@deriving show]
