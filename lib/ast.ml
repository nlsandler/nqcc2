type unary_operator = Complement | Negate | Not [@@deriving show]

type binary_operator =
  | Add
  | Subtract
  | Multiply
  | Divide
  | Mod
  | And
  | Or
  | Equal
  | NotEqual
  | LessThan
  | LessOrEqual
  | GreaterThan
  | GreaterOrEqual
[@@deriving show]

type exp =
  | Constant of int
  | Unary of unary_operator * exp
  | Binary of binary_operator * exp * exp
[@@deriving show]

type statement = Return of exp [@@deriving show]

type function_definition = Function of { name : string; body : statement }
[@@deriving show]

type t = Program of function_definition [@@deriving show]
