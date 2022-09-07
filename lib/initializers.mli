open Cnums

type static_init =
  | CharInit of Int8.t
  | UCharInit of UInt8.t
  | IntInit of Int32.t
  | LongInit of Int64.t
  | UIntInit of UInt32.t
  | ULongInit of UInt64.t
  | DoubleInit of Float.t
  | ZeroInit of int
  | StringInit of
      string * bool (* flag indicates whether the string is null terminated *)
  | PointerInit of string (* pointer to static variable *)

val pp_static_init : Format.formatter -> static_init -> unit
val show_static_init : static_init -> string
val zero : Types.t -> static_init list
val is_zero : static_init -> bool
