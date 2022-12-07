module type S = sig
  type t
  type elt

  val init : t
  val union : elt -> elt -> t -> t
  val find : elt -> t -> elt
  val is_empty : t -> bool
end

module Make (Ord : Map.OrderedType) = struct
  module M = Map.Make (Ord)

  type t = Ord.t M.t
  type elt = Ord.t

  let init = M.empty
  let union x y disj_sets = M.add x y disj_sets

  let rec find x disj_sets =
    if M.mem x disj_sets then
      let mapped_to = M.find x disj_sets in
      find mapped_to disj_sets
    else x

  let is_empty = M.is_empty
end
