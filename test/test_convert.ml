open Nqcc
open Unsigned

let print_const c = Format.printf "%a" Const.pp c

let convert_and_print ~target_type c =
  let converted = Const_convert.const_convert target_type c in
  Format.printf "%a" Const.pp converted

let%expect_test "preserve int value" =
  let i = Const.ConstInt 1000l in
  convert_and_print ~target_type:Types.Long i;
  [%expect "1000L"]

let%expect_test "preserve negative int value" =
  let i = Const.ConstInt (-1000l) in
  convert_and_print ~target_type:Types.Long i;
  [%expect "-1000L"]

let%expect_test "preserve long value" =
  let l = Const.ConstLong 200000L in
  convert_and_print ~target_type:Types.Int l;
  [%expect "200000"]

let%expect_test "truncate positive long" =
  (* l is 2^52 + 5 *)
  let l = Const.ConstLong 4503599627370501L in
  convert_and_print ~target_type:Types.Int l;
  [%expect "5"]

let%expect_test "truncate positive long to negative" =
  (* l is 2^52 - 5 *)
  let l = Const.ConstLong 4503599627370491L in
  convert_and_print ~target_type:Types.Int l;
  [%expect "-5"]

let%expect_test "truncate negative long to zero" =
  (* l is -2^33 *)
  let l = Const.ConstLong (-8589934592L) in
  convert_and_print ~target_type:Types.Int l;
  [%expect "0"]

let%expect_test "truncate negative long to negative" =
  (* l is -2^33 - 100 *)
  let l = Const.ConstLong (-8589934692L) in
  convert_and_print ~target_type:Types.Int l;
  [%expect "-100"]

let%expect_test "trivial uint to int" =
  let ui = Const.ConstUInt (UInt32.of_int64 100L) in
  convert_and_print ~target_type:Types.Int ui;
  [%expect "100"]

let%expect_test "wrapping uint to int" =
  let ui = Const.ConstUInt (UInt32.of_int64 4294967200L) in
  convert_and_print ~target_type:Types.Int ui;
  [%expect "-96"]

let%expect_test "trivial int to uint" =
  let i = Const.ConstInt 1000l in
  convert_and_print ~target_type:Types.UInt i;
  [%expect "1000U"]

let%expect_test "wrapping int to uint" =
  let i = Const.ConstInt (-1000l) in
  convert_and_print ~target_type:Types.UInt i;
  [%expect "4294966296U"]

let%expect_test "int to ulong" =
  let i = Const.ConstInt (-10l) in
  convert_and_print ~target_type:Types.ULong i;
  [%expect "18446744073709551606UL"]

let%expect_test "uint to long" =
  let ui = Const.ConstUInt (UInt32.of_int64 4294967200L) in
  convert_and_print ~target_type:Types.Long ui;
  [%expect "4294967200L"]

let%expect_test "long to uint" =
  let l = Const.ConstLong (-9223372036854774574L) in
  convert_and_print ~target_type:Types.UInt l;
  [%expect "1234U"]

let%expect_test "ulong to int" =
  let ul = Const.ConstULong (UInt64.of_string "4294967200") in
  convert_and_print ~target_type:Types.Int ul;
  [%expect "-96"]

let%expect_test "ulong to uint" =
  let ul = Const.ConstULong (UInt64.of_string "1152921506754330624") in
  convert_and_print ~target_type:Types.UInt ul;
  [%expect "2147483648U"]

let%expect_test "double to long" =
  let d = Const.ConstDouble 2148429099.3 in
  convert_and_print ~target_type:Types.Long d;
  [%expect "2148429099L"]

let%expect_test "double to int" =
  let d = Const.ConstDouble (-200000.9999) in
  convert_and_print ~target_type:Types.Long d;
  [%expect "-200000L"]

let%expect_test "double to uint" =
  let d = Const.ConstDouble 2147483750.5 in
  convert_and_print ~target_type:Types.UInt d;
  [%expect "2147483750U"]

let%expect_test "double to ulong" =
  let d = Const.ConstDouble 3458764513821589504.0 in
  convert_and_print ~target_type:Types.ULong d;
  [%expect "3458764513821589504UL"]

let%expect_test "int to double" =
  let i = Const.ConstInt (-1000l) in
  convert_and_print ~target_type:Types.Double i;
  [%expect "-1000"]

let%expect_test "long to double" =
  let l = Const.ConstLong (-9007199254751227L) in
  convert_and_print ~target_type:Types.Double l;
  [%expect "-9007199254751228"]

let%expect_test "uint to double" =
  let ui = Const.ConstUInt (UInt32.of_int64 4294967200L) in
  convert_and_print ~target_type:Types.Double ui;
  [%expect "4294967200"]

let%expect_test "ulong to double" =
  let ul = Const.ConstULong (UInt64.of_string "138512825844") in
  convert_and_print ~target_type:Types.Double ul;
  [%expect "138512825844"]

let%expect_test "ulong to double inexact" =
  let ul = Const.ConstULong (UInt64.of_string "10223372036854775816") in
  convert_and_print ~target_type:Types.Double ul;
  [%expect "10223372036854775808"]

let%expect_test "ulong to double round to odd" =
  let ul = Const.ConstULong (UInt64.of_string "9223372036854776832") in
  convert_and_print ~target_type:Types.Double ul;
  [%expect "9223372036854775808"]
