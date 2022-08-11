module T = struct
  include Tokens
end

open Ast

(* exceptions *)
exception ParseError of string

type expected = Tok of T.t | Name of string

let pp_expected fmt = function
  | Tok tk -> T.pp fmt tk
  | Name s -> Format.pp_print_string fmt s

let raise_error ~expected ~actual =
  let msg =
    Format.asprintf "Expected %a but found %a" pp_expected expected T.pp actual
  in
  raise (ParseError msg)

(* helper functions *)

let peek tokens =
  match Stream.peek tokens with
  (* non-empty stream *)
  | Some t -> t
  (* empty stream - raise an exception, we'll catch it at top level *)
  | None -> raise Stream.Failure

let expect expected tokens =
  let actual = Stream.next tokens in
  if actual <> expected then raise_error ~expected:(Tok expected) ~actual
  else ()

let expect_empty tokens =
  try Stream.empty tokens
  with Stream.Failure ->
    (* Stream.empty raises this error if stream isn't empty *)
    let bad_token = Stream.next tokens in
    raise_error ~expected:(Name "end of file") ~actual:bad_token

(* parsing grammar symbols *)

(* <identifier> ::= ? An identifier token ? *)
let parse_id tokens =
  match Stream.next tokens with
  | T.Identifier x -> x
  | other -> raise_error ~expected:(Name "an identifier") ~actual:other

(* <int> ::= ? A constant token ? *)
let parse_constant tokens =
  match Stream.next tokens with
  | T.Constant c -> Constant c
  (* we only call this when we know the next token is a constant *)
  | _ ->
      raise (ParseError "Internal error when parsing constant") [@coverage off]

(* <unop> ::= "-" | "~" *)
let parse_unop tokens =
  match Stream.next tokens with
  | T.Tilde -> Complement
  | T.Hyphen -> Negate
  (* we only call this when we know the next token is a unop *)
  | _ ->
      raise (ParseError "Internal error when parsing unary operator")
      [@coverage off]

(* <exp> ::= <int> | <unop> <exp> | "(" <exp> ")" *)
let rec parse_expression tokens =
  let next_token = peek tokens in
  match next_token with
  (* constant *)
  | T.Constant _ -> parse_constant tokens
  (* unary expression *)
  | T.Hyphen | T.Tilde ->
      let operator = parse_unop tokens in
      let inner_exp = parse_expression tokens in
      Unary (operator, inner_exp)
  (* parenthesized expression *)
  | T.OpenParen ->
      (* Stream.junk consumes open paren *)
      let _ = Stream.junk tokens in
      let e = parse_expression tokens in
      let _ = expect T.CloseParen tokens in
      e
  (* errors *)
  | t -> raise_error ~expected:(Name "an expression") ~actual:t

(* <statement> ::= "return" <exp> ";" *)
let parse_statement tokens =
  let _ = expect T.KWReturn tokens in
  let exp = parse_expression tokens in
  let _ = expect T.Semicolon tokens in
  Return exp

(* <function> ::= "int" <identifier> "(" ")" "{" <statement> "}" *)
let parse_function_definition tokens =
  let _ = expect KWInt tokens in
  let fun_name = parse_id tokens in
  let _ =
    expect T.OpenParen tokens;
    expect T.KWVoid tokens;
    expect T.CloseParen tokens;
    expect T.OpenBrace tokens
  in
  let statement = parse_statement tokens in
  let _ = expect T.CloseBrace tokens in
  Function { name = fun_name; body = statement }

(* <program> ::= <function> *)
let parse tokens =
  try
    let token_stream = Stream.of_list tokens in
    let fun_def = parse_function_definition token_stream in
    let _ = expect_empty token_stream in
    Program fun_def
  with Stream.Failure -> raise (ParseError "Unexpected end of file")
