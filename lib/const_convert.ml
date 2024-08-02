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

module type Castable = sig
  type t

  val to_float : t -> float
  val to_int32 : t -> int32
  val to_int64 : t -> int64
end

(* Cast any integer to another type *)
module IntCastEvaluator (C : Castable) = struct
  let cast v = function
    | T.Char | SChar -> Const.ConstChar (v |> C.to_int32 |> Int8.of_int32)
    | UChar -> ConstUChar (v |> C.to_int32 |> UInt8.of_int32)
    | Int -> Const.ConstInt (C.to_int32 v)
    | UInt -> ConstUInt (v |> C.to_int64 |> UInt32.of_int64)
    | Long -> ConstLong (C.to_int64 v)
    | ULong | Pointer _ -> ConstULong (v |> C.to_int64 |> UInt64.of_int64)
    | Double -> ConstDouble (C.to_float v)
    | (FunType _ | Array _ | Void | Structure _) as t ->
        failwith
          ("Internal error: cannot cast constant to non-scalar " ^ T.show t)
        [@coverage off]
end

module CharCaster = IntCastEvaluator (Int8)
module UCharCaster = IntCastEvaluator (UInt8)
module UIntCaster = IntCastEvaluator (UInt32)
module ULongCaster = IntCastEvaluator (UInt64)

module IntCaster = IntCastEvaluator (struct
  include Int32

  let to_int32 x = x
  let to_int64 x = Int64.of_int32 x
end)

module LongCaster = IntCastEvaluator (struct
  include Int64

  let to_int64 x = x
end)

let const_convert target_type = function
  | Const.ConstChar c -> CharCaster.cast c target_type
  | ConstUChar uc -> UCharCaster.cast uc target_type
  | ConstInt i -> IntCaster.cast i target_type
  | ConstUInt ui -> UIntCaster.cast ui target_type
  | ConstLong l -> LongCaster.cast l target_type
  | ConstULong ul -> ULongCaster.cast ul target_type
  | ConstDouble d -> (
      match target_type with
      | Double -> ConstDouble d
      | ULong -> ConstULong (UInt64.of_z (Z.of_float d))
      | Pointer _ ->
          failwith "Internal error: cannot convert double to pointer"
          [@coverage off]
      | _ ->
          let i64 = Int64.of_float d in
          LongCaster.cast i64 target_type)
