module T = struct
  include Tokens
end

(* Raised when we hit an unexpected token or end of file *)
exception ParseError of string

module Private = struct
  (*** Utilities ***)

  (* Type used to build ParseError message in raise_error utility below. We can
     either specify the exact token we expected or use a string description of
     the type of token we expected (e.g. "an integer") *)
  type expected = Tok of T.t | Name of string

  (* Pretty printer for expected values, used to construct ParseError
     messages *)
  let pp_expected fmt = function
    | Tok tk -> T.pp fmt tk
    | Name s -> Format.pp_print_string fmt s

  (* Build parse error messages *)
  let raise_error ~expected ~actual =
    let msg =
      Format.asprintf "Expected %a but found %a" pp_expected expected T.pp
        actual
    in
    raise (ParseError msg)

  (* Remove next token and verify it's what we expect; if not, raise an error.
     See Listing 1-7 *)
  let expect expected tokens =
    let actual = Tok_stream.take_token tokens in
    if actual <> expected then raise_error ~expected:(Tok expected) ~actual
    else ()

  (*** Parsing functions for grammar symbols ***)

  (* <identifier> ::= ? An identifier token ? *)
  let parse_id tokens =
    match Tok_stream.take_token tokens with
    | T.Identifier x -> x
    | other -> raise_error ~expected:(Name "an identifier") ~actual:other

  (* <int> ::= ? A constant token ? *)
  let parse_int tokens =
    match Tok_stream.take_token tokens with
    | T.Constant c -> Ast.Constant c
    | other -> raise_error ~expected:(Name "a constant") ~actual:other

  (* <exp> ::= <int> *)
  let parse_exp tokens = parse_int tokens

  (* <statement> ::= "return" <exp> ";"
   * See Listing 1-7 *)
  let parse_statement tokens =
    expect T.KWReturn tokens;
    let exp = parse_exp tokens in
    expect T.Semicolon tokens;
    Ast.Return exp

  (* <function> ::= "int" <identifier> "(" "void" ")" "{" <statement> "}" *)
  let parse_function_definition tokens =
    expect T.KWInt tokens;
    let fun_name = parse_id tokens in
    expect T.OpenParen tokens;
    expect T.KWVoid tokens;
    expect T.CloseParen tokens;
    expect T.OpenBrace tokens;
    let statement = parse_statement tokens in
    expect T.CloseBrace tokens;
    Ast.Function { name = fun_name; body = statement }

  (* <program> ::= <function> *)
  let parse_program tokens =
    let fun_def = parse_function_definition tokens in
    if Tok_stream.is_empty tokens then Ast.Program fun_def
    else raise (ParseError "Unexpected tokens after function definition")
end

(* Main parsing function - convert a list of tokens to an AST *)
let parse tokens =
  try
    (* We convert our list of tokens to a Tok_stream.t type that supports the
       imperative operations we use in the book (like "take_token") in order to
       follow the code in the book as closely as possible, but having the parser
       operate directly on a list would work too. *)
    let token_stream = Tok_stream.of_list tokens in
    Private.parse_program token_stream
  with Tok_stream.End_of_stream -> raise (ParseError "Unexpected end of file")
