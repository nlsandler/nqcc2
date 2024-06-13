open Batteries

(* structure and union type definitions *)

type member_entry = { member_type : Types.t; offset : int }

type type_def = {
  alignment : int;
  size : int;
  members : (string * member_entry) list; (* in declaration order *)
}

type type_entry = Ast.CommonAst.which * type_def option

let (type_table : (string, type_entry) Hashtbl.t) = Hashtbl.create 20
let add_type_definition tag type_def = Hashtbl.replace type_table tag type_def
let mem tag = Hashtbl.mem type_table tag
let find tag = Hashtbl.find type_table tag
let find_opt tag = Hashtbl.find_option type_table tag

let get_members tag =
  let _, type_def = find tag in
  match type_def with
  | Some td -> td.members
  | None -> failwith "No member definitions"

let get_member_types tag =
  List.map (fun (_, m) -> m.member_type) (get_members tag)

let get_size tag =
  let _, type_def = find tag in
  match type_def with Some td -> td.size | None -> failwith "Incomplete type"

(* Helper function to reconstruct Types.t from a tag*)
let get_type tag =
  match find tag with
  | Ast.CommonAst.Struct, _ -> Types.Structure tag
  | Union, _ -> Types.Union tag
