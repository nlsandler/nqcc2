type 'a t

val init : 'a t
val union : 'a -> 'a -> 'a t -> 'a t
val find : 'a -> 'a t -> 'a
val is_empty : 'a t -> bool
