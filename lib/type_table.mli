open Batteries

type member_entry = { member_type : Types.t; offset : int }

type struct_entry = {
  alignment : int;
  size : int;
  members : (string, member_entry) Map.t;
}

val add_struct_definition : string -> struct_entry -> unit
val mem : string -> bool
val find : string -> struct_entry
val get_members : string -> member_entry list
val get_member_types : string -> Types.t list
