(** The lexer *)

exception LexError of string
(** Throw this exception if we hit something that isn't a valid token or
    whitespace *)

val lex : string -> Tokens.t list
(** Convert C source code from a string to a list of tokens. Raise [LexError] if
    we encounter input that doesn't match a token. *)
