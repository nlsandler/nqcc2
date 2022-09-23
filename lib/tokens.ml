[@@@coverage exclude_file]

module Big_int = struct
  include Extended_big_int
end

type t =
  (* tokens with contents *)
  | Identifier of string
  | StringLiteral of string
  | ConstChar of string
  | ConstInt of (Big_int.big_int[@equal Big_int.eq_big_int])
  | ConstLong of (Big_int.big_int[@equal Big_int.eq_big_int])
  | ConstUInt of (Big_int.big_int[@equal Big_int.eq_big_int])
  | ConstULong of (Big_int.big_int[@equal Big_int.eq_big_int])
  | ConstDouble of Cnums.Float.t
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
  | KWSizeOf
  | KWStruct
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
  | Dot
  | Arrow
[@@deriving show, eq, ord]
