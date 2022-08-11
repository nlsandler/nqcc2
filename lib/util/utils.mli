(** Miscellaneous utility functions *)
module ListUtil : sig
  val max : ('a -> 'a -> int) -> 'a list -> 'a
  (** [max cmp l] returns the maximum element in list [l] according to
      comparison function [cmp]
      @raise Failure if the list is empty *)
end

module StringUtil : sig
  val drop : int -> string -> string
end
