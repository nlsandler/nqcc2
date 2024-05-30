open Cnums

[@@@coverage off]

type t =
  | ConstInt of Int32.t
  | ConstLong of Int64.t
  | ConstUInt of UInt32.t
  | ConstULong of UInt64.t
  | ConstDouble of Float.t

(* print functions for debugging *)
let show = function
  | ConstInt i -> Int32.to_string i
  | ConstLong l -> Int64.to_string l ^ "L"
  | ConstUInt u -> UInt32.to_string u ^ "U"
  | ConstULong ul -> UInt64.to_string ul ^ "UL"
  | ConstDouble d -> Float.to_string d

let pp fmt cnst = Format.pp_print_string fmt (show cnst)

[@@@coverage on]

let int_zero = ConstInt Int32.zero
let int_one = ConstInt Int32.one

let type_of_const = function
  | ConstInt _ -> Types.Int
  | ConstLong _ -> Types.Long
  | ConstUInt _ -> Types.UInt
  | ConstULong _ -> Types.ULong
  | ConstDouble _ -> Types.Double
