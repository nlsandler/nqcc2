(** Functions to parse individual symbols. These are not accessed by the rest of
    the compiler and are exported only for testing. *)
module Private : sig
  val parse_id : Tok_stream.t -> string
  val parse_int : Tok_stream.t -> Ast.exp
  val parse_exp : Tok_stream.t -> Ast.exp
  val parse_statement : Tok_stream.t -> Ast.statement
  val parse_function_definition : Tok_stream.t -> Ast.function_definition
  val parse_program : Tok_stream.t -> Ast.t
end

exception ParseError of string
(** Raised when we hit an unexpected token or end of file *)

val parse : Tokens.t list -> Ast.t
(** Convert a list of tokens to an AST
    @raise ParseError if the program is syntactically invalid. *)
