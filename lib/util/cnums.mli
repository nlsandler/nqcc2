(* extend existing modules *)
module Float : sig
  include module type of Float

  val pp : Format.formatter -> t -> unit
end

(* define new modules *)
module Int8 : Num_interfaces.NumLike
module UInt8 : Num_interfaces.NumLike
module UInt32 : Num_interfaces.NumLike
module UInt64 : Num_interfaces.NumLike

module MakeCompare : functor (C : sig
  type t
  val compare: t -> t -> int
end) ->  Num_interfaces.Compare with type t = C.t