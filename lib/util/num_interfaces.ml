(* basic interface supporting integer conversions,
 * satisfied by both our types and the ones provided by batteries *)

module type NumLike = sig
  type t

  val zero : t

  (* conversions *)
  val of_int32 : int32 -> t
  val to_int32 : t -> int32
  val of_int64 : int64 -> t
  val to_int64 : t -> int64
  val to_int : t -> int
  val to_float : t -> float

  (* string conversions/formatting *)
  val of_string : string -> t
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
end
