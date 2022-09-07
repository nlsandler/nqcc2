(* Represent signed chars internally with the int32 type *)
type t = int32 [@@deriving eq, ord]

let pp fmt i8 = Format.fprintf fmt "%ld" i8
let show = Int32.to_string
let zero = Int32.zero

(* internal function to sign-or-zero-extend into upper bytes *)
let reset_upper_bytes x =
  if Int32.logand x 128l = Int32.zero then
    (* result is positive, zero out upper bits*)
    let bitmask = 0x000000ffl in
    Int32.logand x bitmask
  else
    (* result is negative, set upper bits to 1*)
    let bitmask = 0xffffff00l in
    Int32.logor x bitmask

let of_int i = reset_upper_bytes (Int32.of_int i)
let to_int = Int32.to_int
let of_int64 i = reset_upper_bytes (Int64.to_int32 i)
let to_int64 = Int64.of_int32
let to_string = Int32.to_string
