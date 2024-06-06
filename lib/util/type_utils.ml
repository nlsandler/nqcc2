open Types

let get_type Ast.Typed.{ t; _ } = t
let set_type e new_type = Ast.Typed.{ e; t = new_type }

let rec get_size = function
  | Char | SChar | UChar -> 1
  | Int | UInt -> 4
  | Long | ULong | Double | Pointer _ -> 8
  | Array { elem_type; size } -> size * get_size elem_type
  | Structure tag -> Type_table.(find tag).size
  | (FunType _ | Void) as t ->
      failwith
        ("Internal error: type doesn't have size: " ^ show t) [@coverage off]

let rec get_alignment = function
  | Char | SChar | UChar -> 1
  | Int | UInt -> 4
  | Long | ULong | Double | Pointer _ -> 8
  | Array { elem_type; _ } -> get_alignment elem_type
  | Structure tag -> (Type_table.find tag).alignment
  | (FunType _ | Void) as t ->
      failwith
        ("Internal error: type doesn't have alignment: " ^ show t)
      [@coverage off]

let is_signed = function
  | Int | Long | Char | SChar -> true
  | UInt | ULong | Pointer _ | UChar -> false
  | (Double | FunType _ | Array _ | Void | Structure _) as t ->
      failwith
        ("Internal error: signedness doesn't make sense for non-integral type "
        ^ show t) [@coverage off]

let is_pointer = function Pointer _ -> true | _ -> false

let is_integer = function
  | Char | UChar | SChar | Int | UInt | Long | ULong -> true
  | Double | Array _ | Pointer _ | FunType _ | Void | Structure _ -> false

let is_array = function Array _ -> true | _ -> false
let is_character = function Char | SChar | UChar -> true | _ -> false

let is_arithmetic = function
  | Int | UInt | Long | ULong | Char | UChar | SChar | Double -> true
  | FunType _ | Pointer _ | Array _ | Void | Structure _ -> false

let is_scalar = function
  | Array _ | Void | FunType _ | Structure _ -> false
  | Int | UInt | Long | ULong | Char | UChar | SChar | Double | Pointer _ ->
      true

let is_complete = function
  | Void -> false
  | Structure tag -> Type_table.mem tag
  | _ -> true

let is_complete_pointer = function Pointer t -> is_complete t | _ -> false
