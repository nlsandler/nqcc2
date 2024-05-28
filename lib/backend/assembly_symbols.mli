val add_fun : string -> bool -> unit
val add_var : string -> Assembly.asm_type -> bool -> unit
val add_constant : string -> Assembly.asm_type -> unit
val set_bytes_required : string -> int -> unit
val get_bytes_required : string -> int
val get_size : string -> int
val get_alignment : string -> int
val is_defined : string -> bool
val is_constant : string -> bool
val is_static : string -> bool
