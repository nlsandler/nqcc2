type member_entry = { member_type : Types.t; offset : int }

type struct_entry = {
  alignment : int;
  size : int;
  members : member_entry Map.Make(String).t;
}

val add_struct_definition : string -> struct_entry -> unit
val mem : string -> bool
val find : string -> struct_entry
val get_members : string -> member_entry list
val get_member_types : string -> Types.t list
