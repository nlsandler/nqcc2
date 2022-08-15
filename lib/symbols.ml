type initial_value = Tentative | Initial of int | NoInitializer

type identifier_attrs =
  | FunAttr of { defined : bool; global : bool; stack_frame_size : int }
  | StaticAttr of { init : initial_value; global : bool }
  | LocalAttr

type entry = { t : Types.t; attrs : identifier_attrs }

let (symbol_table : (string, entry) Hashtbl.t) = Hashtbl.create 20

(* Apply f to value at k in Hashtbl *)
let modify k f =
  let v = Hashtbl.find symbol_table k in
  let v' = f v in
  Hashtbl.replace symbol_table k v'

(* always use replace instead of add; Hashtbl.add creates new binding for a name without remvoing the old one,
 * but we want to remove old binding when we add a new one*)

let add_automatic_var name ~t =
  Hashtbl.replace symbol_table name { t; attrs = LocalAttr }

let add_static_var name ~t ~global ~init =
  Hashtbl.replace symbol_table name { t; attrs = StaticAttr { init; global } }

let add_fun name ~t ~global ~defined =
  Hashtbl.replace symbol_table name
    { t; attrs = FunAttr { global; defined; stack_frame_size = 0 } }

let get name = Hashtbl.find symbol_table name
let get_opt name = Hashtbl.find_opt symbol_table name

let is_global name =
  match (get name).attrs with
  | LocalAttr -> false
  | StaticAttr { global; _ } -> global
  | FunAttr { global; _ } -> global

let is_static name =
  try
    match (get name).attrs with
    | LocalAttr -> false
    | StaticAttr _ -> true
    | FunAttr _ ->
        failwith "Internal error: functions don't have storage duration"
        [@coverage off]
  with Not_found ->
    (* If it's not in the symbol table, it's a TACKY temporary, so not static *)
    false

let bindings () = Hashtbl.to_seq symbol_table |> List.of_seq
let is_defined = Hashtbl.mem symbol_table

let set_bytes_required name bytes_required =
  let update_entry = function
    | { t; attrs = FunAttr a } ->
        { t; attrs = FunAttr { a with stack_frame_size = bytes_required } }
    | _ -> failwith "Internal error: not a function" [@coverage off]
  in
  modify name update_entry

let get_bytes_required name =
  match Hashtbl.find symbol_table name with
  | { attrs = FunAttr a; _ } -> a.stack_frame_size
  | _ -> failwith "Internal error: not a function" [@coverage off]
