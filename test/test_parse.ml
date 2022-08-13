(** Parser tests *)

open Nqcc

let%test "expression" =
  Parse.Private.parse_exp 40
    (Tok_stream.of_list [ Tokens.Constant 100; Tokens.Semicolon ])
  = Ast.Constant 100

let%test "statement" =
  Parse.Private.parse_statement
    (Tok_stream.of_list
       [ Tokens.KWReturn; Tokens.Constant 4; Tokens.Semicolon ])
  = Ast.Return (Ast.Constant 4)

let%test "error" =
  match Parse.parse [ Tokens.KWInt ] with
  | exception Parse.ParseError _ -> true
  | _ -> false
