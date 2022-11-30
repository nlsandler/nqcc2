val last: 'a list -> 'a
val take: int -> 'a list -> 'a list
val take_drop : int -> 'a list -> 'a list * 'a list
val make_list: int -> 'a -> 'a list
val max : ('a -> 'a -> int) -> 'a list -> 'a
val min : ('a -> 'a -> int) -> 'a list -> 'a