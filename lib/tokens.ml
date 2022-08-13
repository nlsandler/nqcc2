[@@@coverage exclude_file]

type t =
  (* tokens with contents *)
  | Identifier of string
  | Constant of int
  (* Keywords *)
  | KWInt
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
[@@deriving show]
