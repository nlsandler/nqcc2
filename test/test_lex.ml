(** Lexer tests *)

open Nqcc

let%test "leading whitespace" = Lex.lex "   return" = [ KWReturn ]
let%test "trailing whitespace" = Lex.lex "0;\t\n" = [ Constant 0; Semicolon ]

let%test "a full program" =
  Lex.lex "int main(void){return 0;}"
  = [
      KWInt;
      Identifier "main";
      OpenParen;
      KWVoid;
      CloseParen;
      OpenBrace;
      KWReturn;
      Constant 0;
      Semicolon;
      CloseBrace;
    ]
