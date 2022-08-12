open Batteries
open Tokens

(* reject disabled extra-credit features (some here, some in parser)*)
let check_extra_credit = function
  | (Ampersand | Pipe | Caret | DoubleLeftBracket | DoubleRightBracket)
    when not (List.mem Settings.Bitwise !Settings.extra_credit_flags) ->
      failwith "Unsupported extra-credit feature"
  | _ -> ()

(* regular expressions for tokens *)
let id_regexp = Str.regexp {|[A-Za-z_][A-Za-z0-9_]*\b|}
let const_regexp = Str.regexp {|[0-9]+\b|}

let id_to_tok = function
  | "int" -> KWInt
  | "return" -> KWReturn
  | "void" -> KWVoid
  | other -> Identifier other

let rec lex_helper chars =
  match chars with
  | [] -> [] (* we've processed the whole input *)
  | '{' :: rest -> OpenBrace :: lex_helper rest
  | '}' :: rest -> CloseBrace :: lex_helper rest
  | '(' :: rest -> OpenParen :: lex_helper rest
  | ')' :: rest -> CloseParen :: lex_helper rest
  | ';' :: rest -> Semicolon :: lex_helper rest
  | '-' :: '-' :: rest -> DoubleHyphen :: lex_helper rest
  | '-' :: rest -> Hyphen :: lex_helper rest
  | '~' :: rest -> Tilde :: lex_helper rest
  | '+' :: rest -> Plus :: lex_helper rest
  | '*' :: rest -> Star :: lex_helper rest
  | '/' :: rest -> Slash :: lex_helper rest
  | '%' :: rest -> Percent :: lex_helper rest
  | '&' :: rest -> Ampersand :: lex_helper rest
  | '^' :: rest -> Caret :: lex_helper rest
  | '|' :: rest -> Pipe :: lex_helper rest
  | '<' :: '<' :: rest -> DoubleLeftBracket :: lex_helper rest
  | '>' :: '>' :: rest -> DoubleRightBracket :: lex_helper rest
  | c :: rest when Char.is_whitespace c -> lex_helper rest
  | c :: _ when Char.is_digit c -> lex_constant chars
  | _ -> lex_identifier chars

and lex_constant input_chars =
  let input = String.implode input_chars in
  if Str.string_match const_regexp input 0 then
    (* extract the portion of the string that matched the input, and convert it to a Constant token *)
    let const_str = Str.matched_string input in
    let tok = Constant (Int.of_string const_str) in
    (* remaining is the rest of the input after the substring that matched the regex *)
    let remaining = Str.string_after input (Str.match_end ()) in
    tok :: lex_helper (String.explode remaining)
  else
    failwith
      ("Lexer failure: input starts with a digit but isn't a constant: " ^ input)

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
