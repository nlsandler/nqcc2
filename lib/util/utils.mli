(** Miscellaneous utility functions *)
module ListUtil : sig
  val max : ('a -> 'a -> int) -> 'a list -> 'a
  (** [max cmp l] returns the maximum element in list [l] according to
      comparison function [cmp]
      @raise Failure if the list is empty *)

  val make_list : int -> 'a -> 'a list
  (** [make_list n v] returns a list of [n] elements with value [v] *)

  val take_drop : int -> 'a list -> 'a list * 'a list
  (** [take_drop n l] returns one list containing the first [n] elements of [l]
      and another list containing any remaining elements. Returns [(l, [])] if
      [l] has [n] or fewer elements. *)
end

module StringUtil : sig
  val drop : int -> string -> string
  val chop_suffix : ?n:int -> string -> string
  val of_list : char list -> string
  val is_alnum : char -> bool
end
