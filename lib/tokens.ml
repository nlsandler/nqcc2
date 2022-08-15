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
[@@deriving show]