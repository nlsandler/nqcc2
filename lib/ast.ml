[@@@coverage exclude_file]

type unary_operator = Complement | Negate | Not | Incr | Decr
[@@deriving show]

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
  | Var of string
  | Unary of unary_operator * exp
  | Binary of binary_operator * exp * exp
  | Assignment of exp * exp
  | CompoundAssignment of binary_operator * exp * exp
  | PostfixIncr of exp
  | PostfixDecr of exp
[@@deriving show]

type declaration = Declaration of { name : string; init : exp option }
[@@deriving show]

type statement = Return of exp | Expression of exp | Null [@@deriving show]
type block_item = S of statement | D of declaration [@@deriving show]

type function_definition =
  | Function of { name : string; body : block_item list }
[@@deriving show]

type t = Program of function_definition [@@deriving show]
