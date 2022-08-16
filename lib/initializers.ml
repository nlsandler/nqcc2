open Unsigned

[@@@coverage off]

(* TODO handle custom printing in cnums, not here *)
type static_init =
  | IntInit of Int32.t
      [@printer fun fmt i -> Format.pp_print_string fmt (Int32.to_string i)]
  | LongInit of Int64.t
      [@printer
        fun fmt l -> Format.pp_print_string fmt (Int64.to_string l ^ "l")]
  | UIntInit of UInt32.t
      [@printer
        fun fmt u -> Format.pp_print_string fmt (UInt32.to_string u ^ "u")]
  | ULongInit of UInt64.t
      [@printer
        fun fmt ul -> Format.pp_print_string fmt (UInt64.to_string ul ^ "ul")]
  | DoubleInit of Float.t
      [@printer fun fmt dbl -> Format.pp_print_string fmt (Float.to_string dbl)]
[@@deriving show]

[@@@coverage on]

let zero = function
  | Types.Int -> IntInit Int32.zero
  | Long -> LongInit Int64.zero
  | UInt -> UIntInit UInt32.zero
  | ULong -> ULongInit UInt64.zero
  | Double -> DoubleInit Float.zero
  | FunType _ ->
      failwith "Internal error: zero doesn't make sense for function type"
      [@coverage off]

let is_zero = function
  | IntInit i -> i = Int32.zero
  | LongInit l -> l = Int64.zero
  | UIntInit u -> u = UInt32.zero
  | ULongInit ul -> ul = UInt64.zero
  (* NOTE: consider all doubles non-zero since we don't know if it's zero or
     negative zero *)
  | DoubleInit _ -> false
