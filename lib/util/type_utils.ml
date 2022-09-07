open Types

let get_type Ast.Typed.{ t; _ } = t
let set_type e new_type = Ast.Typed.{ e; t = new_type }

let rec get_size = function
  | Char | SChar | UChar -> 1
  | Int | UInt -> 4
  | Long | ULong | Double | Pointer _ -> 8
  | Array { elem_type; size } -> size * get_size elem_type
  | FunType _ ->
      failwith "Internal error: function type doesn't have size" [@coverage off]

let rec get_alignment = function
  | Char | SChar | UChar -> 1
  | Int | UInt -> 4
  | Long | ULong | Double | Pointer _ -> 8
  | Array { elem_type; _ } -> get_alignment elem_type
  | FunType _ ->
      failwith "Internal error: function type doesn't have alignment."
      [@coverage off]

let is_signed = function
  | Int | Long | Char | SChar -> true
  | UInt | ULong | Pointer _ | UChar -> false
  | (Double | FunType _ | Array _) as t ->
      failwith
        ("Internal error: signedness doesn't make sense for type " ^ show t)
      [@coverage off]

let is_pointer = function Pointer _ -> true | _ -> false

let is_integer = function
  | Char | UChar | SChar | Int | UInt | Long | ULong -> true
  | Double | Array _ | Pointer _ | FunType _ -> false

let is_array = function Array _ -> true | _ -> false
let is_character t = get_size t = 1

let is_arithmetic = function
  | Int | UInt | Long | ULong | Char | UChar | SChar | Double -> true
  | FunType _ | Pointer _ | Array _ -> false
