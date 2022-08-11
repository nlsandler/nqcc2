type exp = Constant of int [@@deriving show]
type statement = Return of exp [@@deriving show]

type function_definition = Function of { name : string; body : statement }
[@@deriving show]

type t = Program of function_definition [@@deriving show]
