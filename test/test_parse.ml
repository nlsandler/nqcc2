(** Parser tests *)

open Nqcc
module Ast = Ast.Untyped

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
