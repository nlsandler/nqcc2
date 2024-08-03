module type Infix = sig
  type t

  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( / ) : t -> t -> t
  val ( * ) : t -> t -> t
end

module type Compare = sig
  type t

  val ( = ) : t -> t -> bool
  val ( <> ) : t -> t -> bool
  val ( < ) : t -> t -> bool
  val ( <= ) : t -> t -> bool
  val ( > ) : t -> t -> bool
  val ( >= ) : t -> t -> bool
end

(* basic interface supporting integer conversions,
 * satisfied by both our types and the ones provided by batteries *)

module type BasicNumLike = sig
  type t

  module Infix : Infix with type t := t
  module Compare : Compare with type t := t

  val zero : t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val lognot : t -> t
  val neg : t -> t
  val rem : t -> t -> t
end

module type NumLike = sig
  include BasicNumLike

  (* conversions *)

  (* these operations take modulo of source if it's outside range of dest type *)
  val of_int32 : int32 -> t
  val to_int32 : t -> int32
  val of_int64 : int64 -> t
  val to_int64 : t -> int64
  val of_int : int -> t
  val to_int : t -> int
  val to_float : t -> float

  (* raise exception if z is outside range of t *)
  val of_z : Z.t -> t

  (* string conversions/formatting *)
  val of_string : string -> t
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
end
