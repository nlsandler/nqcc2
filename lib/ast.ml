[@@@coverage exclude_file]

(** Unary and binary operators; used in exp AST nodes both with and without type
    information *)
module Ops = struct
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
end

(** Exp AST definition without type info *)
module UntypedExp = struct
  include Ops

  type exp =
    | Constant of Const.t
    | Var of string
    | Cast of { target_type : Types.t; e : exp }
    | Unary of unary_operator * exp
    | Binary of binary_operator * exp * exp
    | Assignment of exp * exp
    | Conditional of { condition : exp; then_result : exp; else_result : exp }
    | FunCall of { f : string; args : exp list }
    | Dereference of exp
    | AddrOf of exp
  [@@deriving show]
end

(** Exp AST definition with type info - see Listing 11-9 *)
module TypedExp = struct
  include Ops

  type inner_exp =
    | Constant of Const.t
    | Var of string
    | Cast of { target_type : Types.t; e : exp }
    | Unary of unary_operator * exp
    | Binary of binary_operator * exp * exp
    | Assignment of exp * exp
    | Conditional of { condition : exp; then_result : exp; else_result : exp }
    | FunCall of { f : string; args : exp list }
    | Dereference of exp
    | AddrOf of exp
  [@@deriving show]

  and exp = { e : inner_exp; t : Types.t } [@@deriving show]
end

(** [EXP] is an interface that [TypedExp] and [UntypedExp] both satisfy - we use
    it below to construct AST definitions for block items that can accept either
    typed or untyped expressions *)
module type EXP = sig
  type exp
  (** The AST definition of an expression *)

  val pp_exp : Format.formatter -> exp -> unit
  (** A pretty printer for exp, produced by [@@deriving show]. We need this to
      automatically derive pretty printers for other types that use [exp] *)
end

(** We put storage_class in its own module instead of defining it in
    [BlockItems] below so OCaml can figure out that [Typed] and [Untyped] are
    using the same [storage_class] type *)
module StorageClass = struct
  type storage_class = Static | Extern [@@deriving show]
end

(** Everything else in the AST - statements/blocks/declarations/etc.

    This functor takes a module that defines an exp type (i.e. TypedExp or
    UntypedExp) and defines another module with all the AST constructs that
    depend on that type. *)
module BlockItems (Exp : EXP) = struct
  (* "open Exp" lets us use the Exp.exp type but doesn't re-export it as part of
     this module. Using "include Exp" would re-export it, but only expose what's
     in the EXP type signature and not the concrete definitions of exp or
     inner_exp. We need those concrete type definitions to be visible, so we
     include them in the Typed and Untyped modules below instead of including
     Exp here. *)
  open Exp

  (* Re-export the storage_class type as part of this module. This doesn't have
     the same problem that "include Exp" would because StorageClass is a
     concrete module, not a functor argument. *)
  include StorageClass

  type variable_declaration = {
    name : string;
    var_type : Types.t;
    init : exp option;
    storage_class : storage_class option;
  }
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
    fun_type : Types.t;
    params : string list;
    body : block option;
    storage_class : storage_class option;
  }
  [@@deriving show]

  and declaration =
    | FunDecl of function_declaration
    | VarDecl of variable_declaration

  type t = Program of declaration list [@@deriving show]
end

(** The complete untyped AST *)
module Untyped = struct
  (* Re-export the definitions of unary/binary ops and exp from UntypedExp *)
  include UntypedExp

  (* Instantiate definitions of statements/declarations/etc using untyped
     expressions and re-export them *)
  include BlockItems (UntypedExp)
end

(** The complete typed AST *)
module Typed = struct
  (* Re-export the definitions of unary/binary ops and exp from TypedExp *)
  include TypedExp

  (* Instantiate definitions of statements/declarations/etc using typed
     expressions and re-export them *)
  include BlockItems (TypedExp)
end
