[@@@coverage off]

type t = ConstInt of int32 | ConstLong of Int64.t [@@deriving show]

[@@@coverage on]

let int_zero = ConstInt Int32.zero
let int_one = ConstInt Int32.one
