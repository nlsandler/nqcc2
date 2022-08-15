module C = Const
module T = Types
module Ast = Ast.Typed

let const_convert target_type c =
  match (target_type, c) with
  (* Int64.of_int32 preserves value *)
  | T.Long, C.ConstInt i -> C.ConstLong (Int64.of_int32 i)
  (* Int64.to_int32 wraps module 2**32 *)
  | T.Int, C.ConstLong l -> C.ConstInt (Int64.to_int32 l)
  (* Otherwise c already has the correct type *)
  | _ -> c
