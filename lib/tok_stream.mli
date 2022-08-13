(** Basic functions for reading streams of tokens. This is basically a thin
    wrapper around the [Stream] library to provide an API closer to what we use
    in the book. *)

type t
(** A stream of tokens *)

exception End_of_stream
(** Raised when we try to get the next token from an empty stream *)

val take_token : t -> Tokens.t
(** Remove the next token from the stream and return it
    @raise End_of_stream if the stream is empty *)

val peek : t -> Tokens.t
(** Return the next token but don't remove it from the stream
    @raise End_of_stream if the stream is empty *)

val npeek : int -> t -> Tokens.t list
(** [npeek n stream] returns a list of the next [n] elements of [stream], or all
    the elements if there are fewer than [n] *)

val is_empty : t -> bool
(** Return [true] if the stream is empty, [false] otherwise. *)

val of_list : Tokens.t list -> t
(** Convert a list of tokens into a stream of tokens *)
