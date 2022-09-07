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

  type 'init_t variable_declaration = {
    name : string;
    var_type : Types.t;
    init : 'init_t option;
    storage_class : storage_class option;
  }
  [@@deriving show]

  type ('init_t, 'exp_t) for_init =
    | InitDecl of 'init_t variable_declaration
    | InitExp of 'exp_t option
  [@@deriving show]

  type ('init_t, 'exp_t) statement =
    | Return of 'exp_t
    | Expression of 'exp_t
    | If of {
        condition : 'exp_t;
        then_clause : ('init_t, 'exp_t) statement;
        else_clause : ('init_t, 'exp_t) statement option;
      }
    | Compound of ('init_t, 'exp_t) block
    | Break of string
    | Continue of string
    | While of {
        condition : 'exp_t;
        body : ('init_t, 'exp_t) statement;
        id : string;
      }
    | DoWhile of {
        body : ('init_t, 'exp_t) statement;
        condition : 'exp_t;
        id : string;
      }
    | For of {
        init : ('init_t, 'exp_t) for_init;
        condition : 'exp_t option;
        post : 'exp_t option;
        body : ('init_t, 'exp_t) statement;
        id : string;
      }
    | Null
    | LabeledStatement of string * ('init_t, 'exp_t) statement
    | Goto of string
    | Switch of {
        control : 'exp_t;
        body : ('init_t, 'exp_t) statement;
        id : string;
        cases : (Const.t option * string) list;
      }
    | Case of
        'exp_t
        (* exp must be constant; validate during semantic analysis *)
        * ('init_t, 'exp_t) statement
        * string
    | Default of ('init_t, 'exp_t) statement * string
  [@@deriving show]

  and ('init_t, 'exp_t) block_item =
    | S of ('init_t, 'exp_t) statement
    | D of ('init_t, 'exp_t) declaration
  [@@deriving show]

  and ('init_t, 'exp_t) block = Block of ('init_t, 'exp_t) block_item list

  and ('init_t, 'exp_t) function_declaration = {
    name : string;
    fun_type : Types.t;
    params : string list;
    body : ('init_t, 'exp_t) block option;
    storage_class : storage_class option;
  }
  [@@deriving show]

  and ('init_t, 'exp_t) declaration =
    | FunDecl of ('init_t, 'exp_t) function_declaration
    | VarDecl of 'init_t variable_declaration

  type ('init_t, 'exp_t) prog_t =
    | Program of ('init_t, 'exp_t) declaration list
  [@@deriving show]
end

module Untyped = struct
  include CommonAst

  type exp =
    | Constant of Const.t
    | Var of string
    | String of string
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
    | Subscript of { ptr : exp; index : exp }
  [@@deriving show]

  type initializr = SingleInit of exp | CompoundInit of initializr list
  [@@deriving show]

  type t = (initializr, exp) CommonAst.prog_t
end

module Typed = struct
  include CommonAst

  type inner_exp =
    | Constant of Const.t
    | Var of string
    | String of string
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
    | Subscript of { ptr : exp; index : exp }
  [@@deriving show]

  and exp = { e : inner_exp; t : Types.t }

  type initializr =
    | SingleInit of exp
    | CompoundInit of Types.t * initializr list
  [@@deriving show]

  type t = (initializr, exp) CommonAst.prog_t [@@deriving show]
end

[@@@coverage exclude_file]
