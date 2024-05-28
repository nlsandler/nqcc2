open Cnums

[@@@coverage off]

type t =
  | ConstInt of Int32.t
  | ConstLong of Int64.t
  | ConstUInt of UInt32.t
  | ConstULong of UInt64.t
  | ConstDouble of Float.t
[@@deriving show]

[@@@coverage on]

let int_zero = ConstInt Int32.zero
let int_one = ConstInt Int32.one

let type_of_const = function
  | ConstInt _ -> Types.Int
  | ConstLong _ -> Types.Long
  | ConstUInt _ -> Types.UInt
  | ConstULong _ -> Types.ULong
  | ConstDouble _ -> Types.Double
