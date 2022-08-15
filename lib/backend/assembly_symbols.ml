type entry =
  | Fun of { defined : bool; bytes_required : int }
  | Obj of { t : Assembly.asm_type; is_static : bool }

let (symbol_table : (string, entry) Hashtbl.t) = Hashtbl.create 20

let add_fun fun_name defined =
  Hashtbl.replace symbol_table fun_name (Fun { defined; bytes_required = 0 })

let add_var var_name t is_static =
  Hashtbl.replace symbol_table var_name (Obj { t; is_static })

let set_bytes_required fun_name bytes_required =
  if Hashtbl.mem symbol_table fun_name then
    Hashtbl.replace symbol_table fun_name
      (* note: we only set bytes_required if function is defined in this
         translation unit *)
      (Fun { defined = true; bytes_required })
  else failwith "Internal error: function is not defined" [@coverage off]

let get_bytes_required fun_name =
  match Hashtbl.find symbol_table fun_name with
  | Fun f -> f.bytes_required
  | Obj _ -> failwith "Internal error: not a function" [@coverage off]

let get_size var_name =
  match Hashtbl.find symbol_table var_name with
  | Obj { t = Longword; _ } -> 4
  | Obj { t = Quadword; _ } -> 8
  | Fun _ ->
      failwith "Internal error: this is a function, not an object"
      [@coverage off]

let get_alignment var_name =
  match Hashtbl.find symbol_table var_name with
  | Obj { t = Longword; _ } -> 4
  | Obj { t = Quadword; _ } -> 8
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
