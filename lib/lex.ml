open Utils

module T = struct
  include Tokens
end

exception LexError of string

type token_def = {
  re : Re.re;  (** The regular expression to recognize a token *)
  converter : string -> Tokens.t;
      (** A function to convert the matched substring into a token *)
}
(** Define how to recognize a token and convert it to a Token.t *)

type match_def = {
  matched_substring : string;
      (** Substring matching the regex specified in token_def *)
  matching_token : token_def;  (** Which token it matched *)
}
(** A substring at the start of the input that matches a token *)

(** Functions to convert individual tokens from string to Tok.t **)

(* For tokens that match fixed strings, e.g. "{" and ";"

   Once we get a match we can just return the token without processing the
   string further. *)
let literal tok _s = tok

(* Check whether it's a keyword - otherwise it's an identifier *)
let convert_identifier = function
  | "int" -> T.KWInt
  | "return" -> T.KWReturn
  | "void" -> T.KWVoid
  | "if" -> T.KWIf
  | "else" -> T.KWElse
  | "do" -> T.KWDo
  | "while" -> T.KWWhile
  | "for" -> T.KWFor
  | "break" -> T.KWBreak
  | "continue" -> T.KWContinue
  | other -> T.Identifier other

let convert_int s = T.Constant (int_of_string s)

(** List of token definitions

    NOTE: we use OCaml quoted string literals like
    [{_|Here's my special string|_}], which are interpreted literally without
    escape sequences, so we can write e.g. \b instead of \\b. *)
let token_defs =
  (* Smart constructor to compile regex for token defs *)
  let def re_str converter =
    (* `ANCHORED flag means only match at start of string *)
    { re = Re.Pcre.regexp ~flags:[ `ANCHORED ] re_str; converter }
  in
  [
    (* all identifiers, including keywords *)
    def {_|[A-Za-z_][A-Za-z0-9_]*\b|_} convert_identifier;
    (* constants *)
    def {_|[0-9]+\b|_} convert_int;
    (* punctuation *)
    def {_|\(|_} (literal T.OpenParen);
    def {_|\)|_} (literal T.CloseParen);
    (* NOTE: The regexes for { and } are not escaped in Table 1-1 in the book;
       but Re.Pcre requires them to be escaped. Using them unescaped to match
       literal "{" and "}" characters is legal but deprecated in Perl; see
       https://github.com/ocaml/ocaml-re/issues/200 *)
    def {_|\{|_} (literal T.OpenBrace);
    def {_|\}|_} (literal T.CloseBrace);
    def ";" (literal T.Semicolon);
    def "-" (literal T.Hyphen);
    def "--" (literal T.DoubleHyphen);
    def "~" (literal T.Tilde);
    def {_|\+|_} (literal T.Plus);
    def {_|\*|_} (literal T.Star);
    def "/" (literal T.Slash);
    def "%" (literal T.Percent);
    def "!" (literal T.Bang);
    def "&&" (literal T.LogicalAnd);
    def {_|\|\||_} (literal T.LogicalOr);
    def "==" (literal T.DoubleEqual);
    def "!=" (literal T.NotEqual);
    def "<" (literal T.LessThan);
    def ">" (literal T.GreaterThan);
    def "<=" (literal T.LessOrEqual);
    def ">=" (literal T.GreaterOrEqual);
    def "=" (literal T.EqualSign);
    def {_|\?|_} (literal T.QuestionMark);
    def ":" (literal T.Colon);
    def "," (literal T.Comma);
  ]

(** Check whether this string starts with this token; if so, return a match_def *)
let find_match s tok_def =
  let re = tok_def.re in
  let maybe_match = Re.exec_opt re s in
  match maybe_match with
  | Some m ->
      (* It matched! Now extract the matching substring.

         [Re.Group.get m 0] returns group 0, i.e. the match for the whole
         regex *)
      Some { matched_substring = Re.Group.get m 0; matching_token = tok_def }
  | None -> None

(** Count number of leading whitespace characters in a string; return None if it
    doesn't start with whitespace *)
let count_leading_ws s =
  let ws_matcher = Re.Pcre.regexp ~flags:[ `ANCHORED ] {|\s+|} in
  let ws_match = Re.exec_opt ws_matcher s in
  match ws_match with
  | None -> None
  | Some mtch ->
      let _, match_end = Re.Group.offset mtch 0 in
      Some match_end

(* The main lexing function *)
let rec lex input =
  (* If input is the empty string, we're done *)
  if input = "" then []
  else
    match count_leading_ws input with
    (* If input starts with whitespace, trim it *)
    | Some ws_count -> lex (StringUtil.drop ws_count input)
    (* Otherwise, lex next token *)
    | None ->
        (* Run each regex in token_defs against start of input and return all
           matches *)
        let matches = List.filter_map (find_match input) token_defs in
        if matches = [] then raise (LexError input)
        else
          (* Find longest match*)
          let compare_match_lengths m1 m2 =
            Int.compare
              (String.length m1.matched_substring)
              (String.length m2.matched_substring)
          in
          let longest_match = ListUtil.max compare_match_lengths matches in
          (* Convert longest match to a token *)
          let converter = longest_match.matching_token.converter in
          let matching_substring = longest_match.matched_substring in
          let next_tok = converter matching_substring in
          (* Remove longest substring from input *)
          let remaining =
            StringUtil.drop
              (String.length longest_match.matched_substring)
              input
          in
          (* Lex the remainder *)
          next_tok :: lex remaining
