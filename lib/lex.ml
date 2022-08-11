open Tokens

(* regular expressions for tokens *)
let id_regexp = Str.regexp {|[A-Za-z_][A-Za-z0-9_]*\b|}
let const_regexp = Str.regexp {|[0-9]+\b|}

let id_to_tok = function
  | "int" -> KWInt
  | "return" -> KWReturn
  | "void" -> KWVoid
  | other -> Identifier other

(* whitespace characters: space, tab, newline, vertical tab, form feed *)
let is_whitespace c = String.contains " \t\n\x0b\x0c" c

let is_digit c = String.contains "0123456789" c

let drop_first s = String.sub s 1 (String.length s - 1)

let rec lex_helper chars = if chars = String.empty  (* we've processed the whole input *)then [] else
  match chars.[0] with
  | '{'  -> OpenBrace :: lex_helper (drop_first chars)
  | '}'  -> CloseBrace :: lex_helper (drop_first chars)
  | '('  -> OpenParen :: lex_helper (drop_first chars)
  | ')'  -> CloseParen :: lex_helper (drop_first chars)
  | ';'  -> Semicolon :: lex_helper (drop_first chars)
  | c when is_whitespace c -> lex_helper (drop_first chars)
  | c  when is_digit c -> lex_constant chars
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
