[@@@coverage exclude_file]

module Big_int = struct
  include Extended_big_int
end

type t =
  (* tokens with contents *)
  | Identifier of string
  | StringLiteral of string
  | ConstChar of string
  | ConstInt of Big_int.big_int
  | ConstLong of Big_int.big_int
  | ConstUInt of Big_int.big_int
  | ConstULong of Big_int.big_int
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
  | KWGoto
  | KWDo
  | KWWhile
  | KWFor
  | KWBreak
  | KWContinue
  | KWSwitch
  | KWCase
  | KWDefault
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
  | DoublePlus
  | Tilde
  | Plus
  | Star
  | Slash
  | Percent
  | Ampersand
  | Caret
  | Pipe
  | DoubleLeftBracket
  | DoubleRightBracket
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
  | PlusEqual (* += *)
  | HyphenEqual (* -= *)
  | StarEqual (* *= *)
  | SlashEqual (* /= *)
  | PercentEqual (* %= *)
  | AmpersandEqual (* &= *)
  | PipeEqual (* |= *)
  | CaretEqual (* ^= *)
  | DoubleLeftBracketEqual (* <<= *)
  | DoubleRightBracketEqual (* >>= *)
  | QuestionMark
  | Colon
  | Comma
  | OpenBracket
  | CloseBracket
[@@deriving show, ord]
