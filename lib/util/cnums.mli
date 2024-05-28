(* extend existing modules *)
module Float : sig
  include module type of Batteries.Float

  val pp : Format.formatter -> t -> unit
end

(* define new modules *)
module UInt32 : Num_interfaces.NumLike
module UInt64 : Num_interfaces.NumLike
