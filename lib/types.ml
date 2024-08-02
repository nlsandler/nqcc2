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
  | Pointer of t [@printer fun fmt typ -> Format.fprintf fmt "%a*" pp typ]
  | Array of { elem_type : t; size : int }
      [@printer
        fun fmt elem_type size ->
          Format.fprintf fmt "(%a, %d)" pp elem_type size]
  | FunType of { param_types : t list; ret_type : t }
[@@deriving show { with_path = false }]
