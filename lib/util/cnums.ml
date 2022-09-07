include Num_interfaces

module Float = struct
  include Float

  [@@@coverage off]

  type t = float [@@deriving show]

  [@@@coverage on]
end

module Int8 : NumLike = struct
  [@@@coverage off]

  type t = int32 [@@deriving show]

  [@@@coverage on]

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

  let check_range x =
    if x > 127l || x < -128l then failwith "Out of range" else x

  let of_int i =
    let x = Int32.of_int i in
    check_range x

  let to_int = Int32.to_int
  let of_int32 x = reset_upper_bytes x
  let to_int32 x = x
  let of_int64 x = reset_upper_bytes (Int64.to_int32 x)
  let to_int64 = Int64.of_int32
  let to_float = Int32.to_float

  let of_z x =
    if Z.(geq x ~- (~$128) && lt x ~$128) then Z.to_int32 x
    else raise Z.Overflow

  let of_string x =
    let result = Int32.of_string x in
    check_range result

  let to_string = Int32.to_string
end

module UInt8 : NumLike = struct
  [@@@coverage off]

  type t = int32 [@@deriving show]

  [@@@coverage on]

  let zero = Int32.zero

  (* internal function to sign-or-zero-extend into upper bytes *)
  let reset_upper_bytes x =
    let bitmask = 0x000000ffl in
    Int32.logand x bitmask

  let of_int i =
    let x = Int32.of_int i in
    reset_upper_bytes x

  let to_int = Int32.to_int
  let of_int32 x = reset_upper_bytes x
  let to_int32 x = x
  let of_int64 x = reset_upper_bytes (Int64.to_int32 x)
  let to_int64 = Int64.of_int32
  let to_float = Int32.to_float

  let of_z x =
    if Z.(geq x zero && lt x ~$256) then Z.to_int32 x else raise Z.Overflow

  let of_string x =
    let result = Int32.of_string x in
    if result > 255l || result < 0l then failwith "Out of range" else result

  let to_string = Int32.to_string
end

module UInt32 : NumLike = struct
  [@@@coverage off]

  type t = int32 [@@deriving show]

  [@@@coverage on]

  let zero = Int32.zero
  let of_int = Int32.of_int

  let to_int x =
    match Int32.unsigned_to_int x with
    | Some i -> i
    | None -> failwith "this should never happen!" [@coverage off]

  let to_float x = Int.to_float (to_int x)
  let of_int32 x = x
  let to_int32 x = x
  let of_int64 x = Int64.to_int32 x

  let of_z x =
    if Z.(gt x (of_int32 Int32.max_int) && lt x (~$2 ** 32)) then
      (* it's w/in unsigned range but not signed range *)
      of_int64 (Z.to_int64_unsigned x)
      (* it's either in range of int (so this will work) or out of range (so we'll get overflow) *)
    else Z.to_int32_unsigned x

  let to_int64 x =
    let i64 = Int64.of_int32 x in
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
  let of_int = Int64.of_int

  let to_int x =
    match Int64.unsigned_to_int x with
    | Some i -> i
    | None -> failwith "out of range"

  let to_float x =
    if Int64.compare x Int64.zero >= 0 then Int64.to_float x
    else Z.to_float (Z.of_int64_unsigned x)

  let of_int32 = Int64.of_int32
  let to_int32 = Int64.to_int32
  let of_int64 x = x
  let to_int64 x = x
  let of_z x = Z.to_int64_unsigned x
  let of_string s = Int64.of_string ("0u" ^ s)
  let to_string x = Printf.sprintf "%Lu" x
end
