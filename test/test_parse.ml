(** Parser tests *)

open Nqcc
module Ast = Ast.Untyped

let print_const c = Format.printf "%a" Const.pp c

let%expect_test "signed long constant" =
  let tok_list =
    Tok_stream.of_list [ Tokens.ConstLong Z.(of_int64 4611686018427387904L) ]
  in
  let c = Parse.Private.parse_const tok_list in
  print_const c;
  [%expect "(Const.ConstLong 4611686018427387904L)"]

let%expect_test "unsigned int constant" =
  let tok_list =
    Tok_stream.of_list [ Tokens.ConstUInt Z.(of_string "4294967291") ]
  in
  let c = Parse.Private.parse_const tok_list in
  print_const c;
  [%expect "(Const.ConstUInt 4294967291)"]

let%expect_test "unsigned long constant" =
  let tok_list =
    Tok_stream.of_list
      [ Tokens.ConstULong Z.(of_string "18446744073709551611") ]
  in
  let c = Parse.Private.parse_const tok_list in
  print_const c;
  [%expect "(Const.ConstULong 18446744073709551611)"]

let%test "expression" =
  Parse.Private.parse_exp 40
    (Tok_stream.of_list [ Tokens.ConstInt Z.(~$100); Tokens.Semicolon ])
  = Ast.Constant (Const.ConstInt (Int32.of_int 100))

let%test "statement" =
  Parse.Private.parse_statement
    (Tok_stream.of_list
       [ Tokens.KWReturn; Tokens.ConstInt Z.(~$4); Tokens.Semicolon ])
  = Ast.Return (Ast.Constant (Const.ConstInt (Int32.of_int 4)))

let%test "error" =
  match Parse.parse [ Tokens.KWInt ] with
  | exception Parse.ParseError _ -> true
  | _ -> false
