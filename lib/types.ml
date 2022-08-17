[@@@coverage exclude_file]

type t =
  | Int
  | Long
  | UInt
  | ULong
  | Double
  | Pointer of t [@printer fun fmt typ -> Format.fprintf fmt "%a*" pp typ]
  | FunType of { param_types : t list; ret_type : t }
[@@deriving show { with_path = false }]
