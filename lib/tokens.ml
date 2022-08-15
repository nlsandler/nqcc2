[@@@coverage exclude_file]

module Big_int = struct
  include Batteries.Big_int

  let pp_big_int fmt bi =
    Format.pp_print_string fmt (Big_int.string_of_big_int bi)
end

type t =
  (* tokens with contents *)
  | Identifier of string
  | ConstInt of (Big_int.big_int[@equal Big_int.eq_big_int])
  | ConstLong of (Big_int.big_int[@equal Big_int.eq_big_int])
  (* Keywords *)
  | KWInt
  | KWLong
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
[@@deriving show]