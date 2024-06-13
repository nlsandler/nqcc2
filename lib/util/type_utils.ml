open Types

let get_type Ast.Typed.{ t; _ } = t
let set_type e new_type = Ast.Typed.{ e; t = new_type }

(* Helper to get definition from type table by tag, or throw error if not defined *)
let get_type_def tag =
  match Type_table.find_opt tag with
  | None | Some (_, None) ->
      failwith (Printf.sprintf "No definition found for %s" tag)
  | Some (_which, Some td) -> td

let rec get_size = function
  | Char | SChar | UChar -> 1
  | Int | UInt -> 4
  | Long | ULong | Double | Pointer _ -> 8
  | Array { elem_type; size } -> size * get_size elem_type
  | Structure tag -> (get_type_def tag).size
  | Union tag -> (get_type_def tag).size
  | (FunType _ | Void) as t ->
      failwith
        ("Internal error: type doesn't have size: " ^ show t) [@coverage off]

let rec get_alignment = function
  | Char | SChar | UChar -> 1
  | Int | UInt -> 4
  | Long | ULong | Double | Pointer _ -> 8
  | Array { elem_type; _ } -> get_alignment elem_type
  | Structure tag -> (get_type_def tag).alignment
  | Union tag -> (get_type_def tag).alignment
  | (FunType _ | Void) as t ->
      failwith
        ("Internal error: type doesn't have alignment: " ^ show t)
      [@coverage off]

let is_signed = function
  | Int | Long | Char | SChar -> true
  | UInt | ULong | Pointer _ | UChar -> false
  | (Double | FunType _ | Array _ | Void | Structure _ | Union _) as t ->
      failwith
        ("Internal error: signedness doesn't make sense for non-integral type "
        ^ show t) [@coverage off]

let is_pointer = function Pointer _ -> true | _ -> false

let is_integer = function
  | Char | UChar | SChar | Int | UInt | Long | ULong -> true
  | Double | Array _ | Pointer _ | FunType _ | Void | Structure _ | Union _ ->
      false

let is_array = function Array _ -> true | _ -> false
let is_character = function Char | SChar | UChar -> true | _ -> false

let is_arithmetic = function
  | Int | UInt | Long | ULong | Char | UChar | SChar | Double -> true
  | FunType _ | Pointer _ | Array _ | Void | Structure _ | Union _ -> false

let is_scalar = function
  (* NOTE: unions are neither scalar nor aggregate *)
  | Array _ | Void | FunType _ | Structure _ | Union _ -> false
  | Int | UInt | Long | ULong | Char | UChar | SChar | Double | Pointer _ ->
      true

let is_complete t =
  (* Helper to check whether tag has type table entry w/ member info *)
  let tag_complete tag =
    match Type_table.find_opt tag with
    | Some (_which, Some _td) -> true
    (* Otherwise, either type isn't in type table, or it's only declared, not defined *)
    | _ -> false
  in
  match t with
  | Void -> false
  | Structure tag -> tag_complete tag
  | Union tag -> tag_complete tag
  | _ -> true

let is_complete_pointer = function Pointer t -> is_complete t | _ -> false
