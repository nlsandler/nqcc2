include Num_interfaces
open Batteries

module Float = struct
  include Batteries.Float

  [@@@coverage off]

  type t = float [@@deriving show, eq]

  (* Override Compare to handle NaN correctly *)
  module Compare = struct
    (* Ugly batteries thing *)
    type bat__compare_t = float

    let ( = ) = Stdlib.( = )
    let ( > ) = Stdlib.( > )
    let ( >= ) = Stdlib.( >= )
    let ( < ) = Stdlib.( < )
    let ( <= ) = Stdlib.( <= )
    let ( <> ) = Stdlib.( <> )
  end

  [@@@coverage on]
end

module Int8 : NumLike = struct
  [@@@coverage off]

  type t = int32 [@@deriving show, eq, ord]

  [@@@coverage on]

  let zero = Int32.zero
  let lognot = Int32.lognot
  let rem = Int32.rem

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

  let neg = reset_upper_bytes % Int32.neg

  module Infix = struct
    let ( + ) x y = reset_upper_bytes Int32.Infix.(x + y)
    let ( - ) x y = reset_upper_bytes Int32.Infix.(x - y)

    (* don't need to explicitly wrap division; if x and y are in range of int8, result will be too *)
    let ( / ) = Int32.Infix.( / )
    let ( * ) x y = reset_upper_bytes Int32.Infix.(x * y)
  end

  module Compare = Int32.Compare

  let logand x y = reset_upper_bytes (Int32.logand x y)
  let logor x y = reset_upper_bytes (Int32.logor x y)
  let logxor x y = reset_upper_bytes (Int32.logxor x y)
  let shift_left x y = reset_upper_bytes (Int32.shift_left x y)
  let shift_right x y = reset_upper_bytes (Int32.shift_right x y)

  let check_range x =
    if x > 127l || x < -128l then failwith "Out of range" else x

  let of_int i =
    let x = Int32.of_int i in
    check_range x

  let to_int = Int32.to_int
  let of_int32 x = reset_upper_bytes x
  let to_int32 x = x
  let of_int64 = reset_upper_bytes % Int32.of_int64
  let to_int64 = Int32.to_int64
  let to_float = Int32.to_float

  let of_string x =
    let result = Int32.of_string x in
    check_range result

  let to_string = Int32.to_string
end

module MakeCompare (C : sig
  type t

  val compare : t -> t -> int
end) : Compare with type t = C.t = struct
  type t = C.t

  let ( = ) x y = C.compare x y = 0
  let ( <> ) x y = C.compare x y <> 0
  let ( > ) x y = C.compare x y > 0
  let ( >= ) x y = C.compare x y >= 0
  let ( < ) x y = C.compare x y < 0
  let ( <= ) x y = C.compare x y <= 0
end

module UInt8 : NumLike = struct
  [@@@coverage off]

  type t = int32 [@@deriving show, eq]

  [@@@coverage on]

  let zero = Int32.zero
  let lognot = Int32.lognot
  let compare = Int32.unsigned_compare
  let rem = Int32.unsigned_rem

  (* internal function to sign-or-zero-extend into upper bytes *)
  let reset_upper_bytes x =
    let bitmask = 0x000000ffl in
    Int32.logand x bitmask

  let neg = reset_upper_bytes % Int32.neg

  module Infix = struct
    let ( + ) x y = reset_upper_bytes Int32.Infix.(x + y)
    let ( - ) x y = reset_upper_bytes Int32.Infix.(x - y)
    let ( / ) x y = Int32.unsigned_div x y
    let ( * ) x y = reset_upper_bytes Int32.Infix.(x * y)
  end

  module Compare = MakeCompare (struct
    type t = int32

    let compare = Int32.unsigned_compare
  end)

  let logand x y = reset_upper_bytes (Int32.logand x y)
  let logor x y = reset_upper_bytes (Int32.logor x y)
  let logxor x y = reset_upper_bytes (Int32.logxor x y)
  let shift_left x y = reset_upper_bytes (Int32.shift_left x y)
  let shift_right x y = reset_upper_bytes (Int32.shift_right_logical x y)

  let of_int i =
    let x = Int32.of_int i in
    reset_upper_bytes x

  let to_int = Int32.to_int
  let of_int32 x = reset_upper_bytes x
  let to_int32 x = x
  let of_int64 = reset_upper_bytes % Int32.of_int64
  let to_int64 = Int32.to_int64
  let to_float = Int32.to_float

  let of_string x =
    let result = Int32.of_string x in
    if result > 255l || result < 0l then failwith "Out of range" else result

  let to_string = Int32.to_string
end

module UInt32 : NumLike = struct
  [@@@coverage off]

  type t = int32 [@@deriving show, eq]

  [@@@coverage on]

  let zero = Int32.zero
  let compare = Int32.unsigned_compare
  let rem = Int32.unsigned_rem
  let neg = Int32.neg
  let lognot = Int32.lognot
  let shift_left x y = Int32.shift_left x y
  let shift_right x y = Int32.shift_right_logical x y

  module Infix = struct
    let ( + ) x y = Int32.Infix.(x + y)
    let ( - ) x y = Int32.Infix.(x - y)
    let ( / ) = Int32.unsigned_div
    let ( * ) x y = Int32.Infix.(x * y)
  end

  module Compare = MakeCompare (struct
    type t = int32

    let compare = Int32.unsigned_compare
  end)

  let logand x y = Int32.logand x y
  let logor x y = Int32.logor x y
  let logxor x y = Int32.logxor x y
  let of_int = Int32.of_int

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

  type t = int64 [@@deriving show, eq]

  [@@@coverage on]

  let zero = Int64.zero
  let compare = Int64.unsigned_compare
  let neg = Int64.neg
  let lognot = Int64.lognot
  let rem = Int64.unsigned_rem

  module Infix = struct
    let ( + ) x y = Int64.Infix.(x + y)
    let ( - ) x y = Int64.Infix.(x - y)
    let ( / ) = Int64.unsigned_div
    let ( * ) x y = Int64.Infix.(x * y)
  end

  module Compare = MakeCompare (struct
    type t = int64

    let compare = Int64.unsigned_compare
  end)

  let logand x y = Int64.logand x y
  let logor x y = Int64.logor x y
  let logxor x y = Int64.logxor x y
  let shift_left x y = Int64.shift_left x y
  let shift_right x y = Int64.shift_right_logical x y
  let of_int = Int64.of_int

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
