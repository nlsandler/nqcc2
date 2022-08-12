[@@@coverage exclude_file]

type t =
  (* tokens with contents *)
  | Identifier of string
  | Constant of int
  (* Keywords *)
  | KWInt
  | KWReturn
  | KWVoid
  (* punctuation *)
  | OpenParen
  | CloseParen
  | OpenBrace
  | CloseBrace
  | Semicolon
  | Hyphen
  | DoubleHyphen
  | Tilde
  | Plus
  | Star
  | Slash
  | Percent
[@@deriving show]
