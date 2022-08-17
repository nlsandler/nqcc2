[@@@coverage exclude_file]

module CommonAst = struct
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

  type storage_class = Static | Extern [@@deriving show]

  type 'exp_t variable_declaration = {
    name : string;
    var_type : Types.t;
    init : 'exp_t option;
    storage_class : storage_class option;
  }
  [@@deriving show]

  type 'exp_t for_init =
    | InitDecl of 'exp_t variable_declaration
    | InitExp of 'exp_t option
  [@@deriving show]

  type 'exp_t statement =
    | Return of 'exp_t
    | Expression of 'exp_t
    | If of {
        condition : 'exp_t;
        then_clause : 'exp_t statement;
        else_clause : 'exp_t statement option;
      }
    | Compound of 'exp_t block
    | Break of string
    | Continue of string
    | While of { condition : 'exp_t; body : 'exp_t statement; id : string }
    | DoWhile of { body : 'exp_t statement; condition : 'exp_t; id : string }
    | For of {
        init : 'exp_t for_init;
        condition : 'exp_t option;
        post : 'exp_t option;
        body : 'exp_t statement;
        id : string;
      }
    | Null
    | LabeledStatement of string * 'exp_t statement
    | Goto of string
    | Switch of {
        control : 'exp_t;
        body : 'exp_t statement;
        id : string;
        cases : (Const.t option * string) list;
      }
    | Case of
        'exp_t
        (* exp must be constant; validate during semantic analysis *)
        * 'exp_t statement
        * string
    | Default of 'exp_t statement * string
  [@@deriving show]

  and 'exp_t block_item = S of 'exp_t statement | D of 'exp_t declaration
  [@@deriving show]

  and 'exp_t block = Block of 'exp_t block_item list

  and 'exp_t function_declaration = {
    name : string;
    fun_type : Types.t;
    params : string list;
    body : 'exp_t block option;
    storage_class : storage_class option;
  }
  [@@deriving show]

  and 'exp_t declaration =
    | FunDecl of 'exp_t function_declaration
    | VarDecl of 'exp_t variable_declaration

  type 'exp_t prog_t = Program of 'exp_t declaration list [@@deriving show]
end

module Untyped = struct
  include CommonAst

  type exp =
    | Constant of Const.t
    | Var of string
    | Cast of { target_type : Types.t; e : exp }
    | Unary of unary_operator * exp
    | Binary of binary_operator * exp * exp
    | Assignment of exp * exp
    | CompoundAssignment of binary_operator * exp * exp
    | PostfixIncr of exp
    | PostfixDecr of exp
    | Conditional of { condition : exp; then_result : exp; else_result : exp }
    | FunCall of { f : string; args : exp list }
    | Dereference of exp
    | AddrOf of exp
  [@@deriving show]

  type t = exp CommonAst.prog_t
end

module Typed = struct
  include CommonAst

  type inner_exp =
    | Constant of Const.t
    | Var of string
    | Cast of { target_type : Types.t; e : exp }
    | Unary of unary_operator * exp
    | Binary of binary_operator * exp * exp
    | Assignment of exp * exp
    | CompoundAssignment of {
        op : binary_operator;
        lhs : exp;
        rhs : exp;
        (* Type of lhs op rhs;
         * may need to convert lhs to tihs type before op, and convert result back
         * to lhs type before assignment
         *)
        result_t : Types.t;
      }
    | PostfixIncr of exp
    | PostfixDecr of exp
    | Conditional of { condition : exp; then_result : exp; else_result : exp }
    | FunCall of { f : string; args : exp list }
    | Dereference of exp
    | AddrOf of exp
  [@@deriving show]

  and exp = { e : inner_exp; t : Types.t }

  type t = exp CommonAst.prog_t
end
