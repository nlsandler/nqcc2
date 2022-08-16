include Batteries.Big_int
open Cnums

let pp_big_int fmt bi = Format.pp_print_string fmt (to_string bi)

let uint32_of_big_int bi =
  let uint32_max = power_int_positive_int 2 32 - one in
  if Big_int.gt_big_int bi uint32_max then
    raise (Failure "Out of range of uint32")
  else UInt32.of_int64 (int64_of_big_int bi)

let uint32_of_big_int_opt bi =
  try Some (uint32_of_big_int bi) with Failure _ -> None

let uint64_of_big_int bi =
  let uint64_max = power_int_positive_int 2 64 - one in
  if Big_int.gt_big_int bi uint64_max then
    raise (Failure "Out of range of uint64")
  else UInt64.of_string (to_string bi)

let uint64_of_big_int_opt bi =
  try Some (uint64_of_big_int bi) with Failure _ -> None
