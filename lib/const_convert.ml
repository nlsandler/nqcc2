open Batteries
open Cnums

module C = struct
  include Const
end

module T = struct
  include Types
end

module Ast = struct
  include Ast.Typed
end

module Big_int = struct
  include Extended_big_int
end

module type Castable = sig
  type t

  val to_int32 : t -> int32
  val to_int64 : t -> int64
end

(* Cast any integer to another type *)
module IntCastEvaluator (C : Castable) = struct
  let cast v = function
    | T.Int -> Const.ConstInt (C.to_int32 v)
    | UInt -> ConstUInt (v |> C.to_int64 |> UInt32.of_int64)
    | Long -> ConstLong (C.to_int64 v)
    | ULong -> ConstULong (v |> C.to_int64 |> UInt64.of_int64)
    | FunType _ ->
        failwith "Internal error: cannot cast constant to function type"
        [@coverage off]
end

module UIntCaster = IntCastEvaluator (UInt32)
module ULongCaster = IntCastEvaluator (UInt64)

module IntCaster = IntCastEvaluator (struct
  include Int32

  let to_int32 x = x
end)

module LongCaster = IntCastEvaluator (struct
  include Int64

  let to_int64 x = x
end)

let const_convert target_type = function
  | Const.ConstInt i -> IntCaster.cast i target_type
  | ConstUInt ui -> UIntCaster.cast ui target_type
  | ConstLong l -> LongCaster.cast l target_type
  | ConstULong ul -> ULongCaster.cast ul target_type
