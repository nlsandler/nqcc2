[@@@coverage off]

type static_init =
  | IntInit of int32
      [@printer fun fmt i -> Format.pp_print_string fmt (Int32.to_string i)]
  | LongInit of int64
      [@printer
        fun fmt l -> Format.pp_print_string fmt (Int64.to_string l ^ "l")]
[@@deriving show]

[@@@coverage on]

let zero = function
  | Types.Int -> IntInit Int32.zero
  | Long -> LongInit Int64.zero
  | FunType _ ->
      failwith "Internal error: zero doesn't make sense for function type"
      [@coverage off]

let is_zero = function
  | IntInit i -> i = Int32.zero
  | LongInit l -> l = Int64.zero
