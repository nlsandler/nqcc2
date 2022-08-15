module C = struct
  include Const
end

module T = struct
  include Types
end

module Ast = struct
  include Ast.Typed
end

let const_convert target_type = function
  | C.ConstInt i ->
      if target_type = T.Int then C.ConstInt i
      else C.ConstLong (Int64.of_int32 i)
  | C.ConstLong i ->
      (* note that to_int32 reduces modulo 2**32 *)
      if target_type = T.Long then C.ConstLong i
      else C.ConstInt (Int64.to_int32 i)