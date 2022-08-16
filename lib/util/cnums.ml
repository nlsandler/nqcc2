include Num_interfaces
open Batteries

module Float = struct
  include Batteries.Float

  [@@@coverage off]

  type t = float [@@deriving show]

  [@@@coverage on]
end

module UInt32 : NumLike = struct
  [@@@coverage off]

  type t = int32 [@@deriving show]

  [@@@coverage on]

  let zero = Int32.zero

  let to_int x =
    match Int32.unsigned_to_int x with
    | Some i -> i
    | None -> failwith "this should never happen!" [@coverage off]

  let to_float = Int.to_float % to_int
  let of_int32 x = x
  let to_int32 x = x
  let of_int64 = Int32.of_int64

  let to_int64 x =
    let i64 = Int32.to_int64 x in
    let bitmask = 0x00000000ffffffffL in
    Int64.logand i64 bitmask

  let of_string s = Int32.of_string ("0u" ^ s)
  let to_string x = Printf.sprintf "%lu" x
end

let%test "uint_type_conversion" =
  UInt32.of_int64 4294967295L |> UInt32.to_int32 = -1l

module UInt64 : NumLike = struct
  [@@@coverage off]

  type t = int64 [@@deriving show]

  [@@@coverage on]

  let zero = Int64.zero

  let to_int x =
    match Int64.unsigned_to_int x with
    | Some i -> i
    | None -> failwith "out of range"

  let to_float x =
    if Int64.compare x Int64.zero >= 0 then Int64.to_float x
    else
      let open Big_int.Infix in
      let bi = Big_int.big_int_of_int64 x in
      let bigint_as_uint = bi + (Big_int.of_int 2 ** Big_int.of_int 64) in
      Big_int.float_of_big_int bigint_as_uint

  let of_int32 = Int64.of_int32
  let to_int32 = Int64.to_int32
  let of_int64 x = x
  let to_int64 x = x
  let of_string s = Int64.of_string ("0u" ^ s)
  let to_string x = Printf.sprintf "%Lu" x
end
