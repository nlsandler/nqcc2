(** A signed, one-byte integer type. Neither ocaml nor the 'integers' library
    defines this type so we have to implement it ourselves *)

type t

val pp : Format.formatter -> t -> unit
val show : t -> string
val equal : t -> t -> bool
val compare : t -> t -> int
val zero : t
val of_int : int -> t
val to_int : t -> int
val of_int64 : int64 -> t
val to_int64 : t -> int64
val to_string : t -> string
