[@@@coverage exclude_file]

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
  | Var of string
  | Unary of unary_operator * exp
  | Binary of binary_operator * exp * exp
  | Assignment of exp * exp
  | Conditional of { condition : exp; then_result : exp; else_result : exp }
  | FunCall of { f : string; args : exp list }
[@@deriving show]

type variable_declaration = { name : string; init : exp option }
[@@deriving show]

type for_init = InitDecl of variable_declaration | InitExp of exp option
[@@deriving show]

type statement =
  | Return of exp
  | Expression of exp
  | If of {
      condition : exp;
      then_clause : statement;
      else_clause : statement option;
    }
  | Compound of block
  | Break of string
  | Continue of string
  | While of { condition : exp; body : statement; id : string }
  | DoWhile of { body : statement; condition : exp; id : string }
  | For of {
      init : for_init;
      condition : exp option;
      post : exp option;
      body : statement;
      id : string;
    }
  | Null
[@@deriving show]

and block_item = S of statement | D of declaration [@@deriving show]
and block = Block of block_item list

and function_declaration = {
  name : string;
  params : string list;
  body : block option;
}
[@@deriving show]

and declaration =
  | FunDecl of function_declaration
  | VarDecl of variable_declaration

type t = Program of function_declaration list [@@deriving show]
