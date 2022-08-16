open Unsigned
module C = Const
module T = Types
module Ast = Ast.Typed

(** Convert constant to an int64. If constant is smaller than int64 it will be
    zero- or sign-extended to preserve value; if it's the same size we preserve
    its representation. *)
let const_to_int64 = function
  | C.ConstInt i -> Int64.of_int32 i
  | C.ConstUInt ui -> UInt32.to_int64 ui
  | C.ConstLong l -> l
  | C.ConstULong ul -> UInt64.to_int64 ul

(** Convert int64 to a constant. Preserve the value if possible and wrap modulo
    the size of the target type otherwise. *)
let const_of_int64 v = function
  | T.Int -> C.ConstInt (Int64.to_int32 v)
  | T.Long -> C.ConstLong v
  | T.UInt -> C.ConstUInt (UInt32.of_int64 v)
  | T.ULong -> C.ConstULong (UInt64.of_int64 v)
  | T.FunType _ ->
      failwith "Internal error: can't convert constant to function type"
      [@coverage off]

let const_convert target_type c =
  if C.type_of_const c = target_type then c
  else
    (* Convert c to int64, then to target type, to avoid exponential explosion
       of different cases. Conversion to int64 preserves value (except when
       converting from out-of-range ulong, where it preserves representation).
       Conversion from int64 to const wraps modulo const size. *)
    let as_int64 = const_to_int64 c in
    const_of_int64 as_int64 target_type
