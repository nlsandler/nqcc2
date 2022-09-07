[@@@coverage exclude_file]

type t =
  (* tokens with contents *)
  | Identifier of string
  | StringLiteral of string
  | ConstChar of string
  | ConstInt of (Z.t[@equal Z.equal] [@printer Z.pp_print])
  | ConstLong of (Z.t[@equal Z.equal] [@printer Z.pp_print])
  | ConstUInt of (Z.t[@equal Z.equal] [@printer Z.pp_print])
  | ConstULong of (Z.t[@equal Z.equal] [@printer Z.pp_print])
  | ConstDouble of Float.t
      [@printer fun fmt dbl -> Format.pp_print_string fmt (Float.to_string dbl)]
  (* Keywords *)
  | KWInt
  | KWLong
  | KWChar
  | KWSigned
  | KWUnsigned
  | KWDouble
  | KWReturn
  | KWVoid
  | KWIf
  | KWElse
  | KWDo
  | KWWhile
  | KWFor
  | KWBreak
  | KWContinue
  | KWStatic
  | KWExtern
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
  | Bang (* ! *)
  | LogicalAnd (* && *)
  | LogicalOr (* || *)
  | DoubleEqual (* == *)
  | NotEqual (* != *)
  | LessThan
  | GreaterThan
  | LessOrEqual
  | GreaterOrEqual
  | EqualSign (* = *)
  | QuestionMark
  | Colon
  | Comma
  | Ampersand
  | OpenBracket
  | CloseBracket
[@@deriving show, ord]
