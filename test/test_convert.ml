open Nqcc

let print_const c = Format.printf "%a" Const.pp c

let%expect_test "preserve int value" =
  let i = Const.ConstInt 1000l in
  let l = Const_convert.const_convert Types.Long i in
  print_const l;
  [%expect "(Const.ConstLong 1000L)"]

let%expect_test "preserve negative int value" =
  let i = Const.ConstInt (-1000l) in
  let l = Const_convert.const_convert Types.Long i in
  print_const l;
  [%expect "(Const.ConstLong -1000L)"]

let%expect_test "preserve long value" =
  let l = Const.ConstLong 200000L in
  let i = Const_convert.const_convert Types.Int l in
  print_const i;
  [%expect "(Const.ConstInt 200000l)"]

let%expect_test "truncate positive long" =
  (* l is 2^52 + 5 *)
  let l = Const.ConstLong 4503599627370501L in
  let i = Const_convert.const_convert Types.Int l in
  print_const i;
  [%expect "(Const.ConstInt 5l)"]

let%expect_test "truncate positive long to negative" =
  (* l is 2^52 - 5 *)
  let l = Const.ConstLong 4503599627370491L in
  let i = Const_convert.const_convert Types.Int l in
  print_const i;
  [%expect "(Const.ConstInt -5l)"]

let%expect_test "truncate negative long to zero" =
  (* l is -2^33 *)
  let l = Const.ConstLong (-8589934592L) in
  let i = Const_convert.const_convert Types.Int l in
  print_const i;
  [%expect "(Const.ConstInt 0l)"]

let%expect_test "truncate negative long to negative" =
  (* l is -2^33 - 100 *)
  let l = Const.ConstLong (-8589934692L) in
  let i = Const_convert.const_convert Types.Int l in
  print_const i;
  [%expect "(Const.ConstInt -100l)"]
