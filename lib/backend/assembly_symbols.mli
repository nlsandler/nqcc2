val add_fun :
  string -> bool -> bool -> Assembly.reg list -> Assembly.reg list -> unit

val add_var : string -> Assembly.asm_type -> bool -> unit
val add_constant : string -> Assembly.asm_type -> unit
val set_bytes_required : string -> int -> unit
val get_bytes_required : string -> int
val add_callee_saved_regs_used : string -> Reg_set.t -> unit
val get_callee_saved_regs_used : string -> Reg_set.t
val get_type : string -> Assembly.asm_type
val get_size : string -> int
val get_alignment : string -> int
val is_defined : string -> bool
val is_constant : string -> bool
val is_static : string -> bool
val returns_on_stack : string -> bool
val param_regs_used : string -> Assembly.reg list
val return_regs_used : string -> Assembly.reg list
