module type S = sig
  type t
  type elt

  val init : t
  val union : elt -> elt -> t -> t
  val find : elt -> t -> elt
  val is_empty : t -> bool
end

module Make : functor (Ord : Map.OrderedType) -> S with type elt = Ord.t
