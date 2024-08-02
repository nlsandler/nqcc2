open Cnums

[@@@coverage off]

(* TODO handle custom printing in cnums, not here *)
type static_init =
  | CharInit of Int8.t
      [@printer fun fmt c -> Format.pp_print_string fmt (Int8.to_string c)]
  | UCharInit of UInt8.t
      [@printer fun fmt uc -> Format.pp_print_string fmt (UInt8.to_string uc)]
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
  | DoubleInit of Float.t [@printer fun fmt dbl -> Float.pp fmt dbl]
  (* zero out arbitrary number of bytes *)
  | ZeroInit of int [@printer fun fmt i -> Format.fprintf fmt "zero[%d]" i]
  | StringInit of
      string * bool (* flag indicates whether the string is null terminated *)
      [@printer
        fun fmt (s, b) ->
          Format.pp_print_string fmt "\"";
          Format.pp_print_string fmt s;
          if b then Format.pp_print_string fmt "\\0";
          Format.pp_print_string fmt "\""]
  | PointerInit of string
      (* pointer to static variable *)
      [@printer fun fmt s -> Format.fprintf fmt "&%s" s]
[@@deriving show]

[@@@coverage on]

let zero t = [ ZeroInit (Type_utils.get_size t) ]

let is_zero = function
  | CharInit c -> c = Int8.zero
  | IntInit i -> i = Int32.zero
  | LongInit l -> l = Int64.zero
  | UCharInit c -> c = UInt8.zero
  | UIntInit u -> u = UInt32.zero
  | ULongInit ul -> ul = UInt64.zero
  (* NOTE: consider all doubles non-zero since we don't know if it's zero or negative zero *)
  | DoubleInit _ -> false
  | ZeroInit _ -> true
  | PointerInit _ | StringInit _ -> false
