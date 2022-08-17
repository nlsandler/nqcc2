open Types

let get_type Ast.Typed.{ t; _ } = t
let set_type e new_type = Ast.Typed.{ e; t = new_type }

let get_size = function
  | Int | UInt -> 4
  | Long | ULong | Double | Pointer _ -> 8
  | FunType _ ->
      failwith "Internal error: function type doesn't have size" [@coverage off]

let get_alignment = function
  | Int | UInt -> 4
  | Long | ULong | Double | Pointer _ -> 8
  | FunType _ ->
      failwith "Internal error: function type doesn't have alignment."
      [@coverage off]

let is_signed = function
  | Int | Long -> true
  | UInt | ULong | Pointer _ -> false
  | (Double | FunType _) as t ->
      failwith
        ("Internal error: signedness doesn't make sense for type " ^ show t)
      [@coverage off]
