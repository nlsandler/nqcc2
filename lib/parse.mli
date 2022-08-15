(** Functions to parse individual symbols. These are not accessed by the rest of
    the compiler and are exported only for testing. *)
module Private : sig
  val parse_id : Tok_stream.t -> string
  val parse_unop : Tok_stream.t -> Ast.Untyped.unary_operator
  val parse_exp : int -> Tok_stream.t -> Ast.Untyped.exp
  val parse_statement : Tok_stream.t -> Ast.Untyped.statement
  val parse_program : Tok_stream.t -> Ast.Untyped.t
end

exception ParseError of string
(** Raised when we hit an unexpected token or end of file *)

val parse : Tokens.t list -> Ast.Untyped.t
(** Convert a list of tokens to an AST
    @raise ParseError if the program is syntactically invalid. *)
