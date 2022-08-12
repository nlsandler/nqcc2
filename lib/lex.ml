open Tokens

(* regular expressions for tokens *)
let id_regexp = Str.regexp {|[A-Za-z_][A-Za-z0-9_]*\b|}
let const_regexp = Str.regexp {|[0-9]+\b|}

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
  | other -> Identifier other

(* whitespace characters: space, tab, newline, vertical tab, form feed *)
let is_whitespace c = String.contains " \t\n\x0b\x0c" c
let is_digit c = String.contains "0123456789" c
let drop n s = String.sub s n (String.length s - n)
let drop_first = drop 1

(* Get the first n characters of s as a list;
 * if s has n or fewer characters, return the whole string *)
let prefix s n =
  let pref = if String.length s > n then String.sub s 0 n else s in
  pref |> String.to_seq |> List.of_seq

let rec lex_helper chars =
  if chars = String.empty (* we've processed the whole input *) then []
  else
    match prefix chars 2 with
    | '&' :: '&' :: _ -> LogicalAnd :: lex_helper (drop 2 chars)
    | '|' :: '|' :: _ -> LogicalOr :: lex_helper (drop 2 chars)
    | '=' :: '=' :: _ -> DoubleEqual :: lex_helper (drop 2 chars)
    | '!' :: '=' :: _ -> NotEqual :: lex_helper (drop 2 chars)
    | '<' :: '=' :: _ -> LessOrEqual :: lex_helper (drop 2 chars)
    | '>' :: '=' :: _ -> GreaterOrEqual :: lex_helper (drop 2 chars)
    | '<' :: _ -> LessThan :: lex_helper (drop_first chars)
    | '>' :: _ -> GreaterThan :: lex_helper (drop_first chars)
    | '!' :: _ -> Bang :: lex_helper (drop_first chars)
    | '=' :: _ -> EqualSign :: lex_helper (drop_first chars)
    | '{' :: _ -> OpenBrace :: lex_helper (drop_first chars)
    | '}' :: _ -> CloseBrace :: lex_helper (drop_first chars)
    | '(' :: _ -> OpenParen :: lex_helper (drop_first chars)
    | ')' :: _ -> CloseParen :: lex_helper (drop_first chars)
    | ';' :: _ -> Semicolon :: lex_helper (drop_first chars)
    | [ '-'; '-' ] -> DoubleHyphen :: lex_helper (drop 2 chars)
    | '-' :: _ -> Hyphen :: lex_helper (drop_first chars)
    | '~' :: _ -> Tilde :: lex_helper (drop_first chars)
    | '+' :: _ -> Plus :: lex_helper (drop_first chars)
    | '*' :: _ -> Star :: lex_helper (drop_first chars)
    | '/' :: _ -> Slash :: lex_helper (drop_first chars)
    | '%' :: _ -> Percent :: lex_helper (drop_first chars)
    | '?' :: _ -> QuestionMark :: lex_helper (drop_first chars)
    | ':' :: _ -> Colon :: lex_helper (drop_first chars)
    | c :: _ when is_whitespace c -> lex_helper (drop_first chars)
    | c :: _ when is_digit c -> lex_constant chars
    | _ -> lex_identifier chars

and lex_constant input =
  if Str.string_match const_regexp input 0 then
    (* extract the portion of the string that matched the input, and convert it to a Constant token *)
    let const_str = Str.matched_string input in
    let tok = Constant (int_of_string const_str) in
    (* remaining is the rest of the input after the substring that matched the regex *)
    let remaining = Str.string_after input (Str.match_end ()) in
    tok :: lex_helper remaining
  else
    failwith
      ("Lexer failure: input starts with a digit but isn't a constant: " ^ input)

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
