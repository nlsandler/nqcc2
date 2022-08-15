open Types

let get_type Ast.Typed.{ t; _ } = t
let set_type e new_type = Ast.Typed.{ e; t = new_type }

let get_alignment = function
  | Int -> 4
  | Long -> 8
  | FunType _ ->
      failwith "Internal error: function type doesn't have alignment."
      [@coverage off]
