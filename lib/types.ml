[@@@coverage exclude_file]

type t =
  | Char
  | SChar
  | UChar
  | Int
  | Long
  | UInt
  | ULong
  | Double
  | Void
  | Pointer of t
  | Array of { elem_type : t; size : int }
  | FunType of { param_types : t list; ret_type : t }
  | Structure of string (* tag *)
[@@deriving show]
