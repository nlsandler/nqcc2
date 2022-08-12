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

  (*** Expressions ***)

  (* <int> ::= ? A constant token ? *)
  let parse_int tokens =
    match Tok_stream.take_token tokens with
    | T.Constant c -> Ast.Constant c
    | other -> raise_error ~expected:(Name "a constant") ~actual:other

  (* return Some prec if token represents a binary operator, None otherwise*)
  let get_precedence = function
    | T.Star | T.Slash | T.Percent -> Some 50
    | T.Plus | T.Hyphen -> Some 45
    | T.LessThan | T.LessOrEqual | T.GreaterThan | T.GreaterOrEqual -> Some 35
    | T.DoubleEqual | T.NotEqual -> Some 30
    | T.LogicalAnd -> Some 10
    | T.LogicalOr -> Some 5
    | T.QuestionMark -> Some 3
    | T.EqualSign -> Some 1
    | _ -> None

  (* <unop> ::= "-" | "~" | "!" *)
  let parse_unop tokens =
    match Tok_stream.take_token tokens with
    | T.Tilde -> Ast.Complement
    | T.Hyphen -> Ast.Negate
    | T.Bang -> Ast.Not
    | other -> raise_error ~expected:(Name "a unary operator") ~actual:other

  (* <binop> ::= "-" | "+" | "*" | "/" | "%" | "&&" | "||"
   *           | "==" | "!=" | "<" | "<=" | ">" | ">=" | "="
   * but we parse "=" in parse_exp, not here.
   *)
  let parse_binop tokens =
    match Tok_stream.take_token tokens with
    | T.Plus -> Ast.Add
    | T.Hyphen -> Ast.Subtract
    | T.Star -> Ast.Multiply
    | T.Slash -> Ast.Divide
    | T.Percent -> Ast.Mod
    | T.LogicalAnd -> Ast.And
    | T.LogicalOr -> Ast.Or
    | T.DoubleEqual -> Ast.Equal
    | T.NotEqual -> Ast.NotEqual
    | T.LessThan -> Ast.LessThan
    | T.LessOrEqual -> Ast.LessOrEqual
    | T.GreaterThan -> Ast.GreaterThan
    | T.GreaterOrEqual -> Ast.GreaterOrEqual
    | other -> raise_error ~expected:(Name "a binary operator") ~actual:other

  (* <factor> ::= <int> | <identifier> | <unop> <factor> | "(" <exp> ")" *)
  let rec parse_factor tokens =
    let next_token = Tok_stream.peek tokens in
    match next_token with
    (* constant *)
    | T.Constant _ -> parse_int tokens
    (* identifier *)
    | T.Identifier _ -> Ast.Var (parse_id tokens)
    (* unary expression *)
    | T.Hyphen | T.Tilde | T.Bang ->
        let operator = parse_unop tokens in
        let inner_exp = parse_factor tokens in
        Ast.Unary (operator, inner_exp)
    (* parenthesized expression *)
    | T.OpenParen ->
        (* Consume open paren *)
        let _ = Tok_stream.take_token tokens in
        let e = parse_exp 0 tokens in
        expect T.CloseParen tokens;
        e
    (* errors *)
    | t -> raise_error ~expected:(Name "a factor") ~actual:t

  (* Helper function to parse the middle of a conditional expression:
   * "?" <exp> ":"
   *)
  and parse_conditional_middle tokens =
    expect QuestionMark tokens;
    let e = parse_exp 0 tokens in
    expect Colon tokens;
    e

  (* <exp> ::= <factor> | <exp> <binop> <exp> | <exp> "?" <exp> ":" <exp>
   * Precedence parsing algorithm (see Listing 6-9) *)
  and parse_exp min_prec tokens =
    let initial_factor = parse_factor tokens in
    let next_token = Tok_stream.peek tokens in
    let rec parse_exp_loop left next =
      match get_precedence next with
      | Some prec when prec >= min_prec ->
          let left =
            if next = T.EqualSign then
              let _ = Tok_stream.take_token tokens in
              let right = parse_exp prec tokens in
              Ast.Assignment (left, right)
            else if next = T.QuestionMark then
              let middle = parse_conditional_middle tokens in
              let right = parse_exp prec tokens in
              Ast.Conditional
                { condition = left; then_result = middle; else_result = right }
            else
              let operator = parse_binop tokens in
              let right = parse_exp (prec + 1) tokens in
              Ast.Binary (operator, left, right)
          in
          parse_exp_loop left (Tok_stream.peek tokens)
      | _ -> left
    in
    parse_exp_loop initial_factor next_token

  (*** Declarations ***)

  (* <declaration> ::= "int" <identifier> [ "=" <exp> ] ";" *)
  let parse_declaration tokens =
    expect T.KWInt tokens;
    let var_name = parse_id tokens in
    let init =
      match Tok_stream.take_token tokens with
      (* No initializer *)
      | T.Semicolon -> None
      (* There is an initializer *)
      | T.EqualSign ->
          let init_exp = parse_exp 0 tokens in
          expect T.Semicolon tokens;
          Some init_exp
      (* Malformed declaration *)
      | other ->
          raise_error ~expected:(Name "An initializer or semicolon")
            ~actual:other
    in
    Ast.Declaration { name = var_name; init }

  (*** Statements and blocks ***)

  (* <statement> ::= "return" <exp> ";"
   *               | "if" "(" <exp> ")" <statement> [ "else" <statement> ]
   *               | <block>
   *               | <exp> ";"
   *               | ";"
   *)
  let rec parse_statement tokens =
    match Tok_stream.peek tokens with
    (* "return" <exp> ";" *)
    | T.KWReturn ->
        (* consume return keyword *)
        let _ = Tok_stream.take_token tokens in
        let exp = parse_exp 0 tokens in
        expect T.Semicolon tokens;
        Ast.Return exp
    (* ";" *)
    | T.Semicolon ->
        (* consume semicolon *)
        let _ = Tok_stream.take_token tokens in
        Ast.Null
        (* "if" "(" <exp> ")" <statement> [ "else" <statement> ] *)
    | T.KWIf ->
        (* if statement - consume if keyword *)
        let _ = Tok_stream.take_token tokens in
        expect T.OpenParen tokens;
        let condition = parse_exp 0 tokens in
        expect T.CloseParen tokens;
        let then_clause = parse_statement tokens in
        let else_clause =
          if Tok_stream.peek tokens = T.KWElse then
            (* there is an else clause - consume the else keyword *)
            let _ = Tok_stream.take_token tokens in

            Some (parse_statement tokens)
          else (* there's no else clause *)
            None
        in
        Ast.If { condition; then_clause; else_clause }
    | T.OpenBrace -> Ast.Compound (parse_block tokens)
    (* <exp> ";" *)
    | _ ->
        let exp = parse_exp 0 tokens in
        expect T.Semicolon tokens;
        Ast.Expression exp

  (* <block-item> ::= <statement> | <declaration> *)
  and parse_block_item tokens =
    if Tok_stream.peek tokens = T.KWInt then Ast.D (parse_declaration tokens)
    else Ast.S (parse_statement tokens)

  (* <block> ::= "{" { <block-item> } "}" *)
  and parse_block tokens =
    expect T.OpenBrace tokens;
    let rec parse_block_item_loop () =
      if Tok_stream.peek tokens = T.CloseBrace then []
      else
        let next_block_item = parse_block_item tokens in
        next_block_item :: parse_block_item_loop ()
    in
    let block = parse_block_item_loop () in
    expect T.CloseBrace tokens;
    Ast.Block block

  (*** Top Level ***)

  (* <function> ::= "int" <identifier> "(" "void" ")" <block> *)
  let parse_function_definition tokens =
    expect T.KWInt tokens;
    let fun_name = parse_id tokens in
    expect T.OpenParen tokens;
    expect T.KWVoid tokens;
    expect T.CloseParen tokens;
    let body = parse_block tokens in
    Ast.Function { name = fun_name; body }

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
