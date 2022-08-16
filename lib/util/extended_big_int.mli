include module type of Batteries.Big_int

val pp_big_int : Format.formatter -> big_int -> unit
val uint32_of_big_int : big_int -> Cnums.UInt32.t
val uint32_of_big_int_opt : big_int -> Cnums.UInt32.t option
val uint64_of_big_int : big_int -> Cnums.UInt64.t
val uint64_of_big_int_opt : big_int -> Cnums.UInt64.t option