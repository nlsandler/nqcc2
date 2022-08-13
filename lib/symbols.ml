(* note: is_defined is only used for functions *)
type entry = { t : Types.t; is_defined : bool; stack_frame_size : int }

let (symbol_table : (string, entry) Hashtbl.t) = Hashtbl.create 20

(* Apply f to value at k in Hashtbl *)
let modify k f =
  let v = Hashtbl.find symbol_table k in
  let v' = f v in
  Hashtbl.replace symbol_table k v'

let add_var name ~t =
  Hashtbl.add symbol_table name { t; is_defined = false; stack_frame_size = 0 }

let add_fun name ~t ~is_defined =
  Hashtbl.add symbol_table name { t; is_defined; stack_frame_size = 0 }

let get name = Hashtbl.find symbol_table name
let get_opt name = Hashtbl.find_opt symbol_table name
let is_defined = Hashtbl.mem symbol_table

let set_bytes_required name bytes_required =
  modify name (fun f -> { f with stack_frame_size = bytes_required })
