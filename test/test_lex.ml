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

let%test "two hyphens" = Lex.lex "- -" = [ Hyphen; Hyphen ]
let%test "double hyphen" = Lex.lex "a--" = [ Identifier "a"; DoubleHyphen ]
let%test "two tildes" = Lex.lex "~~" = [ Tilde; Tilde ]
