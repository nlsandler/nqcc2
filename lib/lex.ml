open Batteries
open Tokens

(* reject disabled extra-credit features (some here, some in parser)*)
let check_extra_credit tok =
  let flag_enabled flag = List.mem flag !Settings.extra_credit_flags in
  match tok with
  | (Pipe | Caret | DoubleLeftBracket | DoubleRightBracket)
    when not (flag_enabled Settings.Bitwise) ->
      failwith "Unsupported extra-credit feature: bitwise operations"
  | (DoublePlus | DoubleHyphen) when not (flag_enabled Settings.Increment) ->
      failwith "Unsupported extra-credit feature: increment/decrement"
  | (PlusEqual | HyphenEqual | StarEqual | PercentEqual | SlashEqual)
    when not (flag_enabled Settings.Compound) ->
      failwith "Unsupported extra-credit feature: compound assignment"
  | AmpersandEqual | PipeEqual | CaretEqual | DoubleLeftBracketEqual
  | DoubleRightBracketEqual
    when not (flag_enabled Settings.Compound && flag_enabled Settings.Bitwise)
    ->
      failwith "Unsupported extra-credit feature"
  | KWGoto when not (flag_enabled Settings.Goto) ->
      failwith "Unsupported extra-credit feature: goto"
  | (KWSwitch | KWCase | KWDefault) when not (flag_enabled Settings.Switch) ->
      failwith "Unsupported extra-credit feature: switch"
  | _ -> ()

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
  | "goto" -> KWGoto
  | "do" -> KWDo
  | "while" -> KWWhile
  | "for" -> KWFor
  | "break" -> KWBreak
  | "continue" -> KWContinue
  | "switch" -> KWSwitch
  | "case" -> KWCase
  | "default" -> KWDefault
  | "static" -> KWStatic
  | "extern" -> KWExtern
  | "long" -> KWLong
  | "unsigned" -> KWUnsigned
  | "signed" -> KWSigned
  | "double" -> KWDouble
  | "char" -> KWChar
  | "sizeof" -> KWSizeOf
  | other -> Identifier other

let rec lex_helper chars =
  match chars with
  | [] -> [] (* we've processed the whole input *)
  | '<' :: '<' :: '=' :: rest -> DoubleLeftBracketEqual :: lex_helper rest
  | '>' :: '>' :: '=' :: rest -> DoubleRightBracketEqual :: lex_helper rest
  | '&' :: '&' :: rest -> LogicalAnd :: lex_helper rest
  | '|' :: '|' :: rest -> LogicalOr :: lex_helper rest
  | '=' :: '=' :: rest -> DoubleEqual :: lex_helper rest
  | '!' :: '=' :: rest -> NotEqual :: lex_helper rest
  | '<' :: '=' :: rest -> LessOrEqual :: lex_helper rest
  | '>' :: '=' :: rest -> GreaterOrEqual :: lex_helper rest
  | '<' :: '<' :: rest -> DoubleLeftBracket :: lex_helper rest
  | '>' :: '>' :: rest -> DoubleRightBracket :: lex_helper rest
  | '+' :: '=' :: rest -> PlusEqual :: lex_helper rest
  | '-' :: '=' :: rest -> HyphenEqual :: lex_helper rest
  | '/' :: '=' :: rest -> SlashEqual :: lex_helper rest
  | '*' :: '=' :: rest -> StarEqual :: lex_helper rest
  | '%' :: '=' :: rest -> PercentEqual :: lex_helper rest
  | '&' :: '=' :: rest -> AmpersandEqual :: lex_helper rest
  | '|' :: '=' :: rest -> PipeEqual :: lex_helper rest
  | '^' :: '=' :: rest -> CaretEqual :: lex_helper rest
  | '<' :: rest -> LessThan :: lex_helper rest
  | '>' :: rest -> GreaterThan :: lex_helper rest
  | '!' :: rest -> Bang :: lex_helper rest
  | '=' :: rest -> EqualSign :: lex_helper rest
  | '{' :: rest -> OpenBrace :: lex_helper rest
  | '}' :: rest -> CloseBrace :: lex_helper rest
  | '(' :: rest -> OpenParen :: lex_helper rest
  | ')' :: rest -> CloseParen :: lex_helper rest
  | ';' :: rest -> Semicolon :: lex_helper rest
  | '-' :: '-' :: rest -> DoubleHyphen :: lex_helper rest
  | '+' :: '+' :: rest -> DoublePlus :: lex_helper rest
  | '-' :: rest -> Hyphen :: lex_helper rest
  | '~' :: rest -> Tilde :: lex_helper rest
  | '+' :: rest -> Plus :: lex_helper rest
  | '*' :: rest -> Star :: lex_helper rest
  | '/' :: rest -> Slash :: lex_helper rest
  | '%' :: rest -> Percent :: lex_helper rest
  | '&' :: rest -> Ampersand :: lex_helper rest
  | '^' :: rest -> Caret :: lex_helper rest
  | '|' :: rest -> Pipe :: lex_helper rest
  | '?' :: rest -> QuestionMark :: lex_helper rest
  | ':' :: rest -> Colon :: lex_helper rest
  | ',' :: rest -> Comma :: lex_helper rest
  | '[' :: rest -> OpenBracket :: lex_helper rest
  | ']' :: rest -> CloseBracket :: lex_helper rest
  | '\'' :: _ -> lex_character chars
  | '"' :: _ -> lex_string chars
  | c :: rest when Char.is_whitespace c -> lex_helper rest
  | c :: _ when Char.is_digit c || c = '.' -> lex_constant chars
  | _ -> lex_identifier chars

and lex_character input_chars =
  let input = String.implode input_chars in
  if Str.string_match char_regexp input 0 then
    (* remove open and close quotes from matched input *)
    let ch = Str.matched_string input |> String.rchop |> String.lchop in
    let tok = ConstChar ch in
    let remaining = Str.string_after input (Str.match_end ()) in
    tok :: lex_helper (String.explode remaining)
  else
    failwith ("Input starts with ' but isn't a valid character token: " ^ input)

and lex_string input_chars =
  let input = String.implode input_chars in
  if Str.string_match string_regexp input 0 then
    (* remove open and close quotes from matched input *)
    let str = Str.matched_string input |> String.rchop |> String.lchop in
    let tok = StringLiteral str in
    let remaining = Str.string_after input (Str.match_end ()) in
    tok :: lex_helper (String.explode remaining)
  else
    failwith ("Input starts with \" but isn't a valid string literal: " ^ input)

and lex_constant input_chars =
  let input = String.implode input_chars in
  let tok =
    if Str.string_match long_regexp input 0 then
      (* extract the portion of the string that matched the input, except the l suffix, and convert it to a Constant token *)
      let const_str = String.rchop (Str.matched_group 1 input) in
      ConstLong (Big_int.of_string const_str)
    else if Str.string_match int_regexp input 0 then
      (* extract the portion of the string that matched the input, and convert it to a Constant token *)
      let const_str = Str.matched_group 1 input in
      ConstInt (Big_int.of_string const_str)
    else if Str.string_match uint_regexp input 0 then
      (* remove "u" suffix *)
      let const_str = String.rchop (Str.matched_group 1 input) in
      ConstUInt (Big_int.of_string const_str)
    else if Str.string_match ulong_regexp input 0 then
      (* remove ul/lu suffix *)
      let const_str = String.rchop ~n:2 (Str.matched_group 1 input) in
      ConstULong (Big_int.of_string const_str)
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
  tok :: lex_helper (String.explode remaining)

and lex_identifier input_chars =
  let input = String.implode input_chars in
  if Str.string_match id_regexp input 0 then
    (* extract the portion of the string that matched the input *)
    let id_str = Str.matched_string input in
    let tok = id_to_tok id_str in
    let remaining = Str.string_after input (Str.match_end ()) in
    tok :: lex_helper (String.explode remaining)
  else failwith ("Lexer failure: input doesn't match id_regexp: " ^ input)

let lex input =
  let input = String.trim input in
  let toks = lex_helper (String.explode input) in
  List.iter check_extra_credit toks;
  toks
