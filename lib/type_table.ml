(* structure type definitions *)
type member_entry = { member_type : Types.t; offset : int }

type struct_entry = {
  alignment : int;
  size : int;
  members : member_entry String_map.t;
}

let (type_table : (string, struct_entry) Hashtbl.t) = Hashtbl.create 20

let add_struct_definition tag struct_def =
  Hashtbl.replace type_table tag struct_def

let mem tag = Hashtbl.mem type_table tag
let find tag = Hashtbl.find type_table tag

let get_members tag =
  let struct_def = find tag in
  let compare_offset m1 m2 = compare m1.offset m2.offset in
  String_map.bindings struct_def.members
  |> List.split
  |> snd
  |> List.sort compare_offset

let get_member_types tag = List.map (fun m -> m.member_type) (get_members tag)
