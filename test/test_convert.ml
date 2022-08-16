open Nqcc
open Unsigned

let print_const c = Format.printf "%a" Const.pp c

let convert_and_print ~target_type c =
  let converted = Const_convert.const_convert target_type c in
  Format.printf "%a" Const.pp converted

let%expect_test "preserve int value" =
  let i = Const.ConstInt 1000l in
  convert_and_print ~target_type:Types.Long i;
  [%expect "(Const.ConstLong 1000L)"]

let%expect_test "preserve negative int value" =
  let i = Const.ConstInt (-1000l) in
  convert_and_print ~target_type:Types.Long i;
  [%expect "(Const.ConstLong -1000L)"]

let%expect_test "preserve long value" =
  let l = Const.ConstLong 200000L in
  convert_and_print ~target_type:Types.Int l;
  [%expect "(Const.ConstInt 200000l)"]

let%expect_test "truncate positive long" =
  (* l is 2^52 + 5 *)
  let l = Const.ConstLong 4503599627370501L in
  convert_and_print ~target_type:Types.Int l;
  [%expect "(Const.ConstInt 5l)"]

let%expect_test "truncate positive long to negative" =
  (* l is 2^52 - 5 *)
  let l = Const.ConstLong 4503599627370491L in
  convert_and_print ~target_type:Types.Int l;
  [%expect "(Const.ConstInt -5l)"]

let%expect_test "truncate negative long to zero" =
  (* l is -2^33 *)
  let l = Const.ConstLong (-8589934592L) in
  convert_and_print ~target_type:Types.Int l;
  [%expect "(Const.ConstInt 0l)"]

let%expect_test "truncate negative long to negative" =
  (* l is -2^33 - 100 *)
  let l = Const.ConstLong (-8589934692L) in
  convert_and_print ~target_type:Types.Int l;
  [%expect "(Const.ConstInt -100l)"]

let%expect_test "trivial uint to int" =
  let ui = Const.ConstUInt (UInt32.of_int64 100L) in
  convert_and_print ~target_type:Types.Int ui;
  [%expect "(Const.ConstInt 100l)"]

let%expect_test "wrapping uint to int" =
  let ui = Const.ConstUInt (UInt32.of_int64 4294967200L) in
  convert_and_print ~target_type:Types.Int ui;
  [%expect "(Const.ConstInt -96l)"]

let%expect_test "trivial int to uint" =
  let i = Const.ConstInt 1000l in
  convert_and_print ~target_type:Types.UInt i;
  [%expect "(Const.ConstUInt 1000)"]

let%expect_test "wrapping int to uint" =
  let i = Const.ConstInt (-1000l) in
  convert_and_print ~target_type:Types.UInt i;
  [%expect "(Const.ConstUInt 4294966296)"]

let%expect_test "int to ulong" =
  let i = Const.ConstInt (-10l) in
  convert_and_print ~target_type:Types.ULong i;
  [%expect "(Const.ConstULong 18446744073709551606)"]

let%expect_test "uint to long" =
  let ui = Const.ConstUInt (UInt32.of_int64 4294967200L) in
  convert_and_print ~target_type:Types.Long ui;
  [%expect "(Const.ConstLong 4294967200L)"]

let%expect_test "long to uint" =
  let l = Const.ConstLong (-9223372036854774574L) in
  convert_and_print ~target_type:Types.UInt l;
  [%expect "(Const.ConstUInt 1234)"]

let%expect_test "ulong to int" =
  let ul = Const.ConstULong (UInt64.of_string "4294967200") in
  convert_and_print ~target_type:Types.Int ul;
  [%expect "(Const.ConstInt -96l)"]

let%expect_test "ulong to uint" =
  let ul = Const.ConstULong (UInt64.of_string "1152921506754330624") in
  convert_and_print ~target_type:Types.UInt ul;
  [%expect "(Const.ConstUInt 2147483648)"]
