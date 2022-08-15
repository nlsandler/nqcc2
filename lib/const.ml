[@@@coverage off]

type t = ConstInt of Int32.t | ConstLong of Int64.t [@@deriving show]

[@@@coverage on]

let int_zero = ConstInt Int32.zero
let int_one = ConstInt Int32.one
