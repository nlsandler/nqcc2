[@@@coverage exclude_file]

type t =
  | Int
  | Long
  | UInt
  | ULong
  | Double
  | FunType of { param_types : t list; ret_type : t }
[@@deriving show { with_path = false }]
