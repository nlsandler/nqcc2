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

(* return Some prec if token represents a binary operator, None otherwise*)
let get_precedence = function
  | T.Star | T.Slash | T.Percent -> Some 50
  | T.Plus | T.Hyphen -> Some 45
  | T.(LessThan | LessOrEqual | GreaterThan | GreaterOrEqual) -> Some 35
  | T.(DoubleEqual | NotEqual) -> Some 30
  | T.LogicalAnd -> Some 10
  | T.LogicalOr -> Some 5
  | T.QuestionMark -> Some 3
  | T.EqualSign -> Some 1
  | _ -> None

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
  | T.Bang -> Not
  (* we only call this when we know the next token is a unop *)
  | _ ->
      raise (ParseError "Internal error when parsing unary operator")
      [@coverage off]

(* <binop> ::= "-" | "+" | "*" | "/" | "%" *)
let parse_binop tokens =
  match Stream.next tokens with
  | T.Plus -> Add
  | T.Hyphen -> Subtract
  | T.Star -> Multiply
  | T.Slash -> Divide
  | T.Percent -> Mod
  | T.LogicalAnd -> And
  | T.LogicalOr -> Or
  | T.DoubleEqual -> Equal
  | T.NotEqual -> NotEqual
  | T.LessThan -> LessThan
  | T.LessOrEqual -> LessOrEqual
  | T.GreaterThan -> GreaterThan
  | T.GreaterOrEqual -> GreaterOrEqual
  | _ ->
      raise (ParseError "Internal error when parsing binary operator")
      [@coverage off]

(* <factor> ::= <int> | <identifier> <unop> <factor> | "(" <exp> ")" *)
let rec parse_factor tokens =
  let next_token = peek tokens in
  match next_token with
  (* constant *)
  | T.Constant _ -> parse_constant tokens
  (* identifier *)
  | T.Identifier _ -> Var (parse_id tokens)
  (* unary expression *)
  | T.Hyphen | T.Tilde | T.Bang ->
      let operator = parse_unop tokens in
      let inner_exp = parse_factor tokens in
      Unary (operator, inner_exp)
  (* parenthesized expression *)
  | T.OpenParen ->
      (* Stream.junk consumes open paren *)
      let _ = Stream.junk tokens in
      let e = parse_expression 0 tokens in
      let _ = expect T.CloseParen tokens in
      e
  (* errors *)
  | t -> raise_error ~expected:(Name "a factor") ~actual:t

(* "?" <exp> ":" *)
and parse_conditional_middle tokens =
  let _ = expect QuestionMark tokens in
  let e = parse_expression 0 tokens in
  let _ = expect Colon tokens in
  e

(* <exp> ::= <factor> | <exp> <binop> <exp> | <exp> "?" <exp> ":" <exp> *)
and parse_expression min_prec tokens =
  let initial_factor = parse_factor tokens in
  let next_token = peek tokens in
  let rec parse_exp_loop left next =
    match get_precedence next with
    | Some prec when prec >= min_prec -> (
        match next with
        | T.EqualSign ->
            let _ = Stream.junk tokens in
            let right = parse_expression prec tokens in
            let left = Assignment (left, right) in
            parse_exp_loop left (peek tokens)
        | T.QuestionMark ->
            let middle = parse_conditional_middle tokens in
            let right = parse_expression prec tokens in
            let left =
              Conditional
                { condition = left; then_result = middle; else_result = right }
            in
            parse_exp_loop left (peek tokens)
        | _ ->
            let operator = parse_binop tokens in
            let right = parse_expression (prec + 1) tokens in
            let left = Binary (operator, left, right) in
            parse_exp_loop left (peek tokens))
    | _ -> left
  in
  parse_exp_loop initial_factor next_token

(* <declaration> ::= "int" <identifier> [ "=" <exp> ] ";" *)
let parse_declaration tokens =
  let _ = expect T.KWInt tokens in
  let var_name = parse_id tokens in
  match Stream.next tokens with
  | T.Semicolon -> Declaration { name = var_name; init = None }
  | T.EqualSign ->
      let init = parse_expression 0 tokens in
      expect T.Semicolon tokens;
      Declaration { name = var_name; init = Some init }
  | other ->
      raise_error ~expected:(Name "An initializer or semicolon") ~actual:other

(* <statement> ::= "return" <exp> ";"
                 | <exp> ";"
                 | "if" "(" <exp> ")" <statement> [ "else" <statement> ]
                 | ";"
*)
let rec parse_statement tokens =
  match peek tokens with
  | T.KWIf -> parse_if_statement tokens
  | T.KWReturn ->
      (* consume return keyword *)
      let _ = Stream.junk tokens in
      let exp = parse_expression 0 tokens in
      let _ = expect T.Semicolon tokens in
      Return exp
  | T.Semicolon ->
      (* consume semicolon *)
      let _ = Stream.junk tokens in
      Null
  | _ ->
      let exp = parse_expression 0 tokens in
      expect T.Semicolon tokens;
      Expression exp

(* helper functions to parse individual statements *)
and parse_if_statement tokens =
  let _ = expect KWIf tokens in
  let _ = expect OpenParen tokens in
  let condition = parse_expression 0 tokens in
  let _ = expect CloseParen tokens in
  let then_clause = parse_statement tokens in
  let else_clause =
    match peek tokens with
    | KWElse ->
        (* there is an else clause - consume the else keyword *)
        let _ = Stream.junk tokens in
        Some (parse_statement tokens)
    | _ -> None
  in
  If { condition; then_clause; else_clause }

(* <block-item> ::= <statement> | <declaration> *)
let parse_block_item tokens =
  match peek tokens with
  | T.KWInt -> D (parse_declaration tokens)
  | _ -> S (parse_statement tokens)

(* helper function to parse list of block items, stopping when we hit a close brace *)
let rec parse_block_item_list tokens =
  match peek tokens with
  | T.CloseBrace -> []
  | _ ->
      let next_block_item = parse_block_item tokens in
      next_block_item :: parse_block_item_list tokens

(* <function> ::= "int" <identifier> "(" ")" "{" { <block-item> } "}" *)
let parse_function_definition tokens =
  let _ = expect KWInt tokens in
  let fun_name = parse_id tokens in
  let _ =
    expect T.OpenParen tokens;
    expect T.KWVoid tokens;
    expect T.CloseParen tokens;
    expect T.OpenBrace tokens
  in
  let body = parse_block_item_list tokens in
  let _ = expect T.CloseBrace tokens in
  Function { name = fun_name; body }

(* <program> ::= <function> *)
let parse tokens =
  try
    let token_stream = Stream.of_list tokens in
    let fun_def = parse_function_definition token_stream in
    let _ = expect_empty token_stream in
    Program fun_def
  with Stream.Failure -> raise (ParseError "Unexpected end of file")
