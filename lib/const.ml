open Cnums

[@@@coverage off]

type t =
  | ConstChar of Int8.t
  | ConstUChar of UInt8.t
  | ConstInt of Int32.t
  | ConstLong of Int64.t
  | ConstUInt of UInt32.t
  | ConstULong of UInt64.t
  | ConstDouble of Float.t
[@@deriving eq, ord]

(* print functions for debugging *)
let show = function
  | ConstChar c -> Int8.to_string c
  | ConstUChar c -> UInt8.to_string c
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
  | ConstChar _ -> Types.SChar
  | ConstUChar _ -> Types.UChar
  | ConstInt _ -> Types.Int
  | ConstLong _ -> Types.Long
  | ConstUInt _ -> Types.UInt
  | ConstULong _ -> Types.ULong
  | ConstDouble _ -> Types.Double

let to_int = function
  | ConstChar c -> Int8.to_int c
  | ConstUChar c -> UInt8.to_int c
  | ConstInt i -> Int32.to_int i
  | ConstLong l -> Int64.to_int l
  | ConstUInt u -> UInt32.to_int u
  | ConstULong ul -> UInt64.to_int ul
  | ConstDouble d -> Float.to_int d
