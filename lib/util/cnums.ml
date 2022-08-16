include Num_interfaces
open Batteries

module UInt32 : NumLike = struct
  [@@@coverage off]

  type t = int32 [@@deriving show]

  [@@@coverage on]

  let zero = Int32.zero
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
  let of_int32 = Int64.of_int32
  let to_int32 = Int64.to_int32
  let of_int64 x = x
  let to_int64 x = x
  let of_string s = Int64.of_string ("0u" ^ s)
  let to_string x = Printf.sprintf "%Lu" x
end
