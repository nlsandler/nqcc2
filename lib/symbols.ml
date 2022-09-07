type initial_value =
  | Tentative
  | Initial of Initializers.static_init list
  | NoInitializer

type identifier_attrs =
  | FunAttr of { defined : bool; global : bool }
  | StaticAttr of { init : initial_value; global : bool }
  | ConstAttr of Initializers.static_init
  | LocalAttr

type entry = { t : Types.t; attrs : identifier_attrs }

let (symbol_table : (string, entry) Hashtbl.t) = Hashtbl.create 20

(* always use replace instead of add; Hashtbl.add creates new binding for a name without remvoing the old one,
 * but we want to remove old binding when we add a new one*)

let add_automatic_var name ~t =
  Hashtbl.replace symbol_table name { t; attrs = LocalAttr }

let add_static_var name ~t ~global ~init =
  Hashtbl.replace symbol_table name { t; attrs = StaticAttr { init; global } }

let add_fun name ~t ~global ~defined =
  Hashtbl.replace symbol_table name { t; attrs = FunAttr { global; defined } }

let get name = Hashtbl.find symbol_table name
let get_opt name = Hashtbl.find_opt symbol_table name

let add_string s =
  let str_id = Unique_ids.make_named_temporary "string" in
  let t = Types.Array { elem_type = Char; size = String.length s + 1 } in
  Hashtbl.replace symbol_table str_id
    { t; attrs = ConstAttr (StringInit (s, true)) };
  str_id

let is_global name =
  match (get name).attrs with
  | LocalAttr | ConstAttr _ -> false
  | StaticAttr { global; _ } -> global
  | FunAttr { global; _ } -> global

let bindings () = Hashtbl.to_seq symbol_table |> List.of_seq
let iter f = Hashtbl.iter f symbol_table
