type initial_value = Tentative | Initial of int | NoInitializer

type identifier_attrs =
  | FunAttr of { defined : bool; global : bool; stack_frame_size : int }
  | StaticAttr of { init : initial_value; global : bool }
  | LocalAttr

type entry = { t : Types.t; attrs : identifier_attrs }

val add_automatic_var : string -> t:Types.t -> unit

val add_static_var :
  string -> t:Types.t -> global:bool -> init:initial_value -> unit

val add_fun : string -> t:Types.t -> global:bool -> defined:bool -> unit
val get : string -> entry
val get_opt : string -> entry option
val is_global : string -> bool
val is_static : string -> bool
val bindings : unit -> (string * entry) list
val is_defined : string -> bool
val set_bytes_required : string -> int -> unit
val get_bytes_required : string -> int
