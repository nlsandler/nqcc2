[@@@coverage exclude_file]

type unary_operator = Complement | Negate [@@deriving show]
type tacky_val = Constant of int | Var of string [@@deriving show]

type instruction =
  | Return of tacky_val
  | Unary of { op : unary_operator; src : tacky_val; dst : tacky_val }
[@@deriving show]

type function_definition =
  | Function of { name : string; body : instruction list }
[@@deriving show]

type t = Program of function_definition [@@deriving show]
