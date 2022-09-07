open Nqcc

let print_int8 i8 = Format.printf "%a" Int8.pp i8

let%expect_test "int to int8" =
  let i8 = Int8.of_int 100 in
  print_int8 i8;
  [%expect "100"]

let%expect_test "wrapped int to int8" =
  let i8 = Int8.of_int 128 in
  print_int8 i8;
  [%expect "-128"]

let%expect_test "int64 to int8" =
  let i8 = Int8.of_int64 (-110L) in
  print_int8 i8;
  [%expect "-110"]

let%expect_test "wrapped int64 to int8" =
  let i8 = Int8.of_int64 1239235325L in
  print_int8 i8;
  [%expect "-3"]

let%test "compare int8 values" =
  let twelve = Int8.of_int 268 in
  let fourteen = Int8.of_int (-4082) in
  compare twelve fourteen = -1
