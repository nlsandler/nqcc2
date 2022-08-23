val get_type : Ast.Typed.exp -> Types.t
val set_type : Ast.Typed.inner_exp -> Types.t -> Ast.Typed.exp
val get_alignment : Types.t -> int
val get_size : Types.t -> int
val is_signed : Types.t -> bool
val is_pointer : Types.t -> bool
val is_integer : Types.t -> bool
val is_arithmetic : Types.t -> bool
val is_array : Types.t -> bool
