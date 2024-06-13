type member_entry = { member_type : Types.t; offset : int }

type type_def = {
  alignment : int;
  size : int;
  members : (string * member_entry) list;
}

type type_entry = Ast.CommonAst.which * type_def option

val add_type_definition : string -> type_entry -> unit
val mem : string -> bool
val find : string -> type_entry
val find_opt : string -> type_entry option
val get_members : string -> (string * member_entry) list
val get_member_types : string -> Types.t list
val get_size : string -> int
val get_type : string -> Types.t
