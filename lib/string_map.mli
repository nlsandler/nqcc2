type key = string
type 'a t = 'a Map.Make(String).t
val empty : 'a t
val add : key -> 'a -> 'a t -> 'a t
val add_to_list : key -> 'a -> 'a list t -> 'a list t
val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
val singleton : key -> 'a -> 'a t
val remove : key -> 'a t -> 'a t
val merge :
  (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
val cardinal : 'a t -> int
val bindings : 'a t -> (key * 'a) list
val min_binding : 'a t -> key * 'a
val min_binding_opt : 'a t -> (key * 'a) option
val max_binding : 'a t -> key * 'a
val max_binding_opt : 'a t -> (key * 'a) option
val choose : 'a t -> key * 'a
val choose_opt : 'a t -> (key * 'a) option
val find : key -> 'a t -> 'a
val find_opt : key -> 'a t -> 'a option
val find_first : (key -> bool) -> 'a t -> key * 'a
val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
val find_last : (key -> bool) -> 'a t -> key * 'a
val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option
val iter : (key -> 'a -> unit) -> 'a t -> unit
val fold : (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
val map : ('a -> 'b) -> 'a t -> 'b t
val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
val filter : (key -> 'a -> bool) -> 'a t -> 'a t
val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t
val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
val split : key -> 'a t -> 'a t * 'a option * 'a t
val is_empty : 'a t -> bool
val mem : key -> 'a t -> bool
val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
val for_all : (key -> 'a -> bool) -> 'a t -> bool
val exists : (key -> 'weak15 -> bool) -> 'weak15 t -> bool
val to_list : 'a t -> (key * 'a) list
val of_list : (key * 'a) list -> 'a t
val to_seq : 'a t -> (key * 'a) Seq.t
val to_rev_seq : 'a t -> (key * 'a) Seq.t
val to_seq_from : key -> 'a t -> (key * 'a) Seq.t
val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t
val of_seq : (key * 'a) Seq.t -> 'a t
