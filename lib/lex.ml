open Tokens

(* regular expressions for tokens *)
let id_regexp = Str.regexp {|[A-Za-z_][A-Za-z0-9_]*\b|}
let int_regexp = Str.regexp {|\([0-9]+\)[^A-Za-z0-9_.]|}
let long_regexp = Str.regexp {|\([0-9]+[lL]\)[^A-Za-z0-9_.]|}
let uint_regexp = Str.regexp {|\([0-9]+[uU]\)[^A-Za-z0-9_.]|}
let ulong_regexp = Str.regexp {|\([0-9]+\([uU][lL]\|[lL][uU]\)\)[^A-Za-z0-9_.]|}

let double_regexp =
  Str.regexp
    {|\(\([0-9]*\.[0-9]+\|[0-9]+\.?\)[Ee][+-]?[0-9]+\|[0-9]*\.[0-9]+\|[0-9]+\.\)[^A-Za-z0-9_.]|}

(* because we're using quoted string literals, we need a literal newline in these regexes instead of \n *)
let char_regexp = Str.regexp {|'\([^'\\
]\|\\['"?\\abfnrtv]\)'|}

let string_regexp = Str.regexp {|"\([^"\\
]\|\\['"\\?abfnrtv]\)*"|}

let id_to_tok = function
  | "int" -> KWInt
  | "return" -> KWReturn
  | "void" -> KWVoid
  | "if" -> KWIf
  | "else" -> KWElse
  | "do" -> KWDo
  | "while" -> KWWhile
  | "for" -> KWFor
  | "break" -> KWBreak
  | "continue" -> KWContinue
  | "static" -> KWStatic
  | "extern" -> KWExtern
  | "long" -> KWLong
  | "unsigned" -> KWUnsigned
  | "signed" -> KWSigned
  | "double" -> KWDouble
  | "char" -> KWChar
  | "sizeof" -> KWSizeOf
  | "struct" -> KWStruct
  | other -> Identifier other

let rec lex_helper chars =
  if chars = String.empty (* we've processed the whole input *) then []
  else
    match String_utils.prefix chars 2 with
    | '&' :: '&' :: _ -> LogicalAnd :: lex_helper (String_utils.drop 2 chars)
    | '|' :: '|' :: _ -> LogicalOr :: lex_helper (String_utils.drop 2 chars)
    | '=' :: '=' :: _ -> DoubleEqual :: lex_helper (String_utils.drop 2 chars)
    | '!' :: '=' :: _ -> NotEqual :: lex_helper (String_utils.drop 2 chars)
    | '<' :: '=' :: _ -> LessOrEqual :: lex_helper (String_utils.drop 2 chars)
    | '>' :: '=' :: _ ->
        GreaterOrEqual :: lex_helper (String_utils.drop 2 chars)
    | '<' :: _ -> LessThan :: lex_helper (String_utils.drop_first chars)
    | '>' :: _ -> GreaterThan :: lex_helper (String_utils.drop_first chars)
    | '!' :: _ -> Bang :: lex_helper (String_utils.drop_first chars)
    | '=' :: _ -> EqualSign :: lex_helper (String_utils.drop_first chars)
    | '{' :: _ -> OpenBrace :: lex_helper (String_utils.drop_first chars)
    | '}' :: _ -> CloseBrace :: lex_helper (String_utils.drop_first chars)
    | '(' :: _ -> OpenParen :: lex_helper (String_utils.drop_first chars)
    | ')' :: _ -> CloseParen :: lex_helper (String_utils.drop_first chars)
    | ';' :: _ -> Semicolon :: lex_helper (String_utils.drop_first chars)
    | [ '-'; '-' ] -> DoubleHyphen :: lex_helper (String_utils.drop 2 chars)
    | '-' :: '>' :: _ -> Arrow :: lex_helper (String_utils.drop 2 chars)
    | '-' :: _ -> Hyphen :: lex_helper (String_utils.drop_first chars)
    | '~' :: _ -> Tilde :: lex_helper (String_utils.drop_first chars)
    | '+' :: _ -> Plus :: lex_helper (String_utils.drop_first chars)
    | '*' :: _ -> Star :: lex_helper (String_utils.drop_first chars)
    | '/' :: _ -> Slash :: lex_helper (String_utils.drop_first chars)
    | '%' :: _ -> Percent :: lex_helper (String_utils.drop_first chars)
    | '?' :: _ -> QuestionMark :: lex_helper (String_utils.drop_first chars)
    | ':' :: _ -> Colon :: lex_helper (String_utils.drop_first chars)
    | ',' :: _ -> Comma :: lex_helper (String_utils.drop_first chars)
    | '&' :: _ -> Ampersand :: lex_helper (String_utils.drop_first chars)
    | '[' :: _ -> OpenBracket :: lex_helper (String_utils.drop_first chars)
    | ']' :: _ -> CloseBracket :: lex_helper (String_utils.drop_first chars)
    | '\'' :: _ -> lex_character chars
    | '"' :: _ -> lex_string chars
    | '.' :: c :: _ when not (String_utils.is_digit c) ->
        Dot :: lex_helper (String_utils.drop_first chars)
    | c :: _ when String_utils.is_whitespace c ->
        lex_helper (String_utils.drop_first chars)
    | c :: _ when String_utils.is_digit c || c = '.' -> lex_constant chars
    | _ -> lex_identifier chars

and lex_character input =
  if Str.string_match char_regexp input 0 then
    (* remove open and close quotes from matched input *)
    let ch =
      Str.matched_string input
      |> String_utils.chop_suffix
      |> String_utils.drop_first
    in
    let tok = ConstChar ch in
    let remaining = Str.string_after input (Str.match_end ()) in
    tok :: lex_helper remaining
  else
    failwith ("Input starts with ' but isn't a valid character token: " ^ input)

and lex_string input =
  if Str.string_match string_regexp input 0 then
    (* remove open and close quotes from matched input *)
    let str =
      Str.matched_string input
      |> String_utils.chop_suffix
      |> String_utils.drop_first
    in
    let tok = StringLiteral str in
    let remaining = Str.string_after input (Str.match_end ()) in
    tok :: lex_helper remaining
  else
    failwith ("Input starts with \" but isn't a valid string literal: " ^ input)

and lex_constant input =
  let tok =
    if Str.string_match long_regexp input 0 then
      (* extract the portion of the string that matched the input, except the l suffix, and convert it to a Constant token *)
      let const_str = String_utils.chop_suffix (Str.matched_group 1 input) in
      ConstLong (Z.of_string const_str)
    else if Str.string_match int_regexp input 0 then
      (* extract the portion of the string that matched the input, and convert it to a Constant token *)
      let const_str = Str.matched_group 1 input in
      ConstInt (Z.of_string const_str)
    else if Str.string_match uint_regexp input 0 then
      (* remove "u" suffix *)
      let const_str = String_utils.chop_suffix (Str.matched_group 1 input) in
      ConstUInt (Z.of_string const_str)
    else if Str.string_match ulong_regexp input 0 then
      (* remove ul/lu suffix *)
      let const_str =
        String_utils.chop_suffix ~n:2 (Str.matched_group 1 input)
      in
      ConstULong (Z.of_string const_str)
    else if Str.string_match double_regexp input 0 then
      (* remove ul/lu suffix *)
      let const_str = Str.matched_group 1 input in
      ConstDouble (Float.of_string const_str)
    else
      failwith
        ("Lexer failure: input starts with a digit but isn't a constant: "
        ^ input)
  in
  (* remaining is the rest of the input after the match group in the regex *)
  let remaining = Str.string_after input (Str.group_end 1) in
  tok :: lex_helper remaining

and lex_identifier input =
  if Str.string_match id_regexp input 0 then
    (* extract the portion of the string that matched the input *)
    let id_str = Str.matched_string input in
    let tok = id_to_tok id_str in
    let remaining = Str.string_after input (Str.match_end ()) in
    tok :: lex_helper remaining
  else failwith ("Lexer failure: input doesn't match id_regexp: " ^ input)

let lex input =
  let input = String.trim input in
  lex_helper input
