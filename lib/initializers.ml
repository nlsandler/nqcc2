open Cnums

[@@@coverage off]

type static_init =
  | IntInit of Int32.t
  | LongInit of Int64.t
  | UIntInit of UInt32.t
  | ULongInit of UInt64.t
  | DoubleInit of Float.t
  (* zero out arbitrary number of bytes *)
  | ZeroInit of int
[@@deriving show]

[@@@coverage on]

let zero t = [ ZeroInit (Type_utils.get_size t) ]

let is_zero = function
  | IntInit i -> i = Int32.zero
  | LongInit l -> l = Int64.zero
  | UIntInit u -> u = UInt32.zero
  | ULongInit ul -> ul = UInt64.zero
  (* NOTE: consider all doubles non-zero since we don't know if it's zero or negative zero *)
  | DoubleInit _ -> false
  | ZeroInit _ -> true
