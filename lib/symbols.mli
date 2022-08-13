(* note: is_defined is only used for functions *)
type entry = { t : Types.t; is_defined : bool; stack_frame_size : int }

val add_var : string -> t:Types.t -> unit
val add_fun : string -> t:Types.t -> is_defined:bool -> unit
val get : string -> entry
val get_opt : string -> entry option
val is_defined : string -> bool
val set_bytes_required : string -> int -> unit
