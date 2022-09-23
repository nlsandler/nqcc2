type entry =
  | Fun of { defined : bool; bytes_required : int; return_on_stack : bool }
  | Obj of { t : Assembly.asm_type; is_static : bool; constant : bool }

let (symbol_table : (string, entry) Hashtbl.t) = Hashtbl.create 20

let add_fun fun_name defined return_on_stack =
  Hashtbl.replace symbol_table fun_name
    (Fun { defined; bytes_required = 0; return_on_stack })

let add_var var_name t is_static =
  Hashtbl.replace symbol_table var_name (Obj { t; is_static; constant = false })

let add_constant const_name t =
  Hashtbl.replace symbol_table const_name
    (Obj { t; is_static = true; constant = true })

let set_bytes_required fun_name bytes_required =
  let entry' =
    match Hashtbl.find symbol_table fun_name with
    | Fun f -> Fun { f with bytes_required }
    | Obj _ -> failwith "Internal error: not a function" [@coverage off]
  in
  Hashtbl.add symbol_table fun_name entry'

let get_bytes_required fun_name =
  match Hashtbl.find symbol_table fun_name with
  | Fun f -> f.bytes_required
  | Obj _ -> failwith "Internal error: not a function" [@coverage off]

let get_size var_name =
  match Hashtbl.find symbol_table var_name with
  | Obj { t = Byte; _ } -> 1
  | Obj { t = Longword; _ } -> 4
  | Obj { t = Quadword | Double; _ } -> 8
  | Obj { t = ByteArray { size; _ }; _ } -> size
  | Fun _ ->
      failwith "Internal error: this is a function, not an object"
      [@coverage off]

let get_type var_name =
  match Hashtbl.find symbol_table var_name with
  | Obj { t; _ } -> t
  | Fun _ ->
      failwith "Internal error: this is a function, not an object"
      [@coverage off]

let get_alignment var_name =
  match Hashtbl.find symbol_table var_name with
  | Obj { t = Byte; _ } -> 1
  | Obj { t = Longword; _ } -> 4
  | Obj { t = Quadword | Double; _ } -> 8
  | Obj { t = ByteArray { alignment; _ }; _ } -> alignment
  | Fun _ ->
      failwith "Internal error: this is a function, not an object"
      [@coverage off]

let is_defined fun_name =
  match Hashtbl.find symbol_table fun_name with
  | Fun { defined; _ } -> defined
  | _ -> failwith "Internal error: not a function" [@coverage off]

let is_static var_name =
  match Hashtbl.find symbol_table var_name with
  | Obj o -> o.is_static
  | Fun _ ->
      failwith "Internal error: functions don't have storage duration"
      [@coverage off]

let is_constant name =
  match Hashtbl.find symbol_table name with
  | Obj { constant = true; _ } -> true
  | Obj _ -> false
  | Fun _ ->
      failwith "Internal error: is_constant doesn't make sense for functions"
      [@coverage off]

let returns_on_stack fun_name =
  match Hashtbl.find symbol_table fun_name with
  | Fun f -> f.return_on_stack
  | Obj _ ->
      failwith "Internal error: this is an object, not a function"
      [@coverage off]
