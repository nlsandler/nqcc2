module T = struct
  include Tokens
end

module Ast = Ast.Untyped

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

  (*** Specifiers ***)

  (* Helper function to check whether a token is a type specifier *)
  let is_type_specifier = function T.KWInt | T.KWLong -> true | _ -> false

  (* <type-specifier> ::= "int" | "long" *)
  let parse_type_specifier tokens =
    let spec = Tok_stream.take_token tokens in
    if is_type_specifier spec then spec
    else raise_error ~expected:(Name "a type specifier") ~actual:spec

  (* Helper to consume a list of type specifiers from start of token stream:
   * { <type-specifier> }+ *)
  let rec parse_type_specifier_list tokens =
    let spec = parse_type_specifier tokens in
    if is_type_specifier (Tok_stream.peek tokens) then
      spec :: parse_type_specifier_list tokens
    else [ spec ]

  (* Helper function to check whether a token is a specifier *)
  let is_specifier = function
    | T.KWStatic | T.KWExtern -> true
    | other -> is_type_specifier other

  (* <specifier> ::= <type-specifier> | "static" | "extern" *)
  let parse_specifier tokens =
    let spec = Tok_stream.take_token tokens in
    if is_specifier spec then spec
    else
      raise_error ~expected:(Name "a type or storage-class specifier")
        ~actual:spec

  (* Helper to consume a list of specifiers from start of token stream:
   * { <specifier> }+ *)
  let rec parse_specifier_list tokens =
    let spec = parse_specifier tokens in
    if is_specifier (Tok_stream.peek tokens) then
      spec :: parse_specifier_list tokens
    else [ spec ]

  (* Convert a single token to a storage class *)
  let parse_storage_class = function
    | T.KWExtern -> Ast.Extern
    | T.KWStatic -> Ast.Static
    | other ->
        raise_error ~expected:(Name "a storage class specifier") ~actual:other

  (* Convert list of specifiers to a type (Listing 11-4) *)
  let parse_type specifier_list =
    match specifier_list with
    | [ T.KWInt ] -> Types.Int
    | [ T.KWInt; T.KWLong ] | [ T.KWLong; T.KWInt ] | [ T.KWLong ] -> Types.Long
    | _ -> raise (ParseError "Invalid type specifier")

  (* Convert list of specifiers to type and storage class (Listing 11-5) *)
  let parse_type_and_storage_class specifier_list =
    let types, storage_classes =
      List.partition is_type_specifier specifier_list
    in
    let typ = parse_type types in
    let storage_class =
      match storage_classes with
      | [] -> None
      | [ sc ] -> Some (parse_storage_class sc)
      | _ :: _ -> failwith "Internal error - not a storage class"
    in
    (typ, storage_class)

  (*** Constants ***)

  (* <const> ::= <int> | <long>

     Convert a single token into constant AST node (Listing 11-6). Note that
     this is slightly different than most parse_* functions because it takes one
     token instead of a token stream. *)
  let parse_constant token =
    let v, is_int =
      match token with
      | T.ConstInt i -> (i, true)
      | T.ConstLong l -> (l, false)
      | other -> raise_error ~expected:(Name "a constant") ~actual:other
    in
    (* ~$2, etc are literals of type Z.t (an arbitrary-precision type) *)
    if Z.(gt v ((~$2 ** 63) - ~$1)) then
      raise (ParseError "Constant is too large to represent as an int or long")
    else if is_int && Z.(leq v ((~$2 ** 31) - ~$1)) then
      Const.ConstInt (Z.to_int32 v)
    else Const.ConstLong (Z.to_int64 v)

  (*** Expressions ***)

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

  (* <factor> ::= <const> | <identifier>
   *            | "(" { <type-specifier }+ ")" <factor>
   *            | <unop> <factor> | "(" <exp> ")"
   *            | <identifier> "(" [ <argument-list> ] ")" *)
  let rec parse_factor tokens =
    let next_token = Tok_stream.peek tokens in
    match next_token with
    (* constant *)
    | T.ConstInt _ | T.ConstLong _ ->
        let tok = Tok_stream.take_token tokens in
        Ast.Constant (parse_constant tok)
    (* variable or function call *)
    | T.Identifier _ ->
        let id = parse_id tokens in
        if Tok_stream.peek tokens = T.OpenParen then
          (* It's a function call - consume open paren, then parse args *)
          let _ = Tok_stream.take_token tokens in
          let args =
            if Tok_stream.peek tokens = T.CloseParen then []
            else parse_argument_list tokens
          in
          let _ = expect T.CloseParen tokens in
          Ast.FunCall { f = id; args }
        else (* It's a variable *) Ast.Var id
    (* unary expression *)
    | T.Hyphen | T.Tilde | T.Bang ->
        let operator = parse_unop tokens in
        let inner_exp = parse_factor tokens in
        Ast.Unary (operator, inner_exp)
    (* cast or parenthesized expression *)
    | T.OpenParen ->
        (* Consume open paren *)
        let _ = Tok_stream.take_token tokens in
        if is_type_specifier (Tok_stream.peek tokens) then
          (* It's a cast expression *)
          let type_specifiers = parse_type_specifier_list tokens in
          let target_type = parse_type type_specifiers in
          let _ = expect T.CloseParen tokens in
          let inner_exp = parse_factor tokens in
          Ast.Cast { target_type; e = inner_exp }
        else
          (* It's parenthesized *)
          let e = parse_exp 0 tokens in
          expect T.CloseParen tokens;
          e
    (* errors *)
    | t -> raise_error ~expected:(Name "a factor") ~actual:t

  (* <argument-list> ::= <exp> { "," <exp> } *)
  and parse_argument_list tokens =
    let arg = parse_exp 0 tokens in
    if Tok_stream.peek tokens = T.Comma then
      let _ = Tok_stream.take_token tokens in
      arg :: parse_argument_list tokens
    else [ arg ]

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

  (* parse an optional expression followed by a delimiter *)
  let parse_optional_exp delim tokens =
    if Tok_stream.peek tokens = delim then
      let _ = Tok_stream.take_token tokens in
      None
    else
      let e = parse_exp 0 tokens in
      expect delim tokens;
      Some e

  (*** Declarations ***)

  (* <param-list> ::= "void"
   *                | { <type-specifier> }+ <identifier> { "," { <type-specifier> }+ <identifier> } *)
  let parse_param_list tokens =
    if Tok_stream.peek tokens = T.KWVoid then
      (* no params - return empty list *)
      let _ = Tok_stream.take_token tokens in
      []
    else
      let rec param_loop () =
        let next_param_t = parse_type (parse_type_specifier_list tokens) in
        let next_param_name = parse_id tokens in
        let next_param = (next_param_t, next_param_name) in
        if Tok_stream.peek tokens = T.Comma then
          (* there are more params *)
          let _ = Tok_stream.take_token tokens in
          next_param :: param_loop ()
        else [ next_param ]
      in
      param_loop ()

  (* <function-declaration> ::= { <specifier> }+ <identifier> "(" <param-list> ")" ( <block> | ";" )
   * <variable-declaration> ::= { <specifier> }+ <identifier> [ "=" <exp> ] ";"
   * Use a common function to parse both symbols
   *)
  let rec parse_declaration tokens =
    let specifiers = parse_specifier_list tokens in
    let typ, storage_class = parse_type_and_storage_class specifiers in
    let name = parse_id tokens in
    match Tok_stream.peek tokens with
    (* It's a function declaration *)
    | T.OpenParen ->
        let _ = Tok_stream.take_token tokens in
        let params_with_types = parse_param_list tokens in
        let param_types, param_names = List.split params_with_types in
        let fun_type = Types.FunType { param_types; ret_type = typ } in
        expect T.CloseParen tokens;
        let body =
          match Tok_stream.peek tokens with
          | T.Semicolon ->
              let _ = Tok_stream.take_token tokens in
              None
          | _ -> Some (parse_block tokens)
        in
        Ast.FunDecl
          { name; fun_type; storage_class; params = param_names; body }
    (* It's a variable *)
    | tok ->
        let init =
          if tok = T.EqualSign then
            let _ = Tok_stream.take_token tokens in
            Some (parse_exp 0 tokens)
          else None
        in
        expect T.Semicolon tokens;
        Ast.VarDecl { name; var_type = typ; storage_class; init }

  (*** Statements and blocks ***)

  (* <for-init> ::= <variable-declaration> | [ <exp> ] ";" *)
  and parse_for_init tokens =
    if is_specifier (Tok_stream.peek tokens) then
      match parse_declaration tokens with
      | Ast.VarDecl vd -> Ast.InitDecl vd
      | _ ->
          raise (ParseError "Found a function declaration in a for loop header")
    else
      let opt_e = parse_optional_exp T.Semicolon tokens in
      Ast.InitExp opt_e

  (* <statement> ::= "return" <exp> ";"
   *               | "if" "(" <exp> ")" <statement> [ "else" <statement> ]
   *               | <block>
   *               | "break" ";"
   *               | "continue" ";"
   *               | "while" "(" <exp> ")" <statement>
   *               | "do" <statement> "while" "(" <exp> ")" ";"
   *               | "for" "(" <for-init> [ <exp> ] ";" [ <exp> ] ")" <statement>
   *               | <exp> ";"
   *               | ";"
   *)
  and parse_statement tokens =
    match Tok_stream.peek tokens with
    (* "return" <exp> ";" *)
    | T.KWReturn ->
        (* consume return keyword *)
        let _ = Tok_stream.take_token tokens in
        let exp = parse_exp 0 tokens in
        expect T.Semicolon tokens;
        Ast.Return exp
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
    (* "break" *)
    | T.KWBreak ->
        (* consume break keyword *)
        let _ = Tok_stream.take_token tokens in
        expect T.Semicolon tokens;
        Ast.Break ""
    (* "continue" *)
    | T.KWContinue ->
        (* consume continue keyword *)
        let _ = Tok_stream.take_token tokens in
        expect T.Semicolon tokens;
        Ast.Continue ""
    (* "while" "(" <exp> ")" <statement> *)
    | T.KWWhile ->
        (* consume while keyword *)
        let _ = Tok_stream.take_token tokens in
        expect OpenParen tokens;
        let condition = parse_exp 0 tokens in
        expect CloseParen tokens;
        let body = parse_statement tokens in
        Ast.While { condition; body; id = "" }
    (* "do" <statement> "while" "(" <exp> ")" ";" *)
    | T.KWDo ->
        expect KWDo tokens;
        let body = parse_statement tokens in
        expect KWWhile tokens;
        expect OpenParen tokens;
        let condition = parse_exp 0 tokens in
        expect CloseParen tokens;
        expect Semicolon tokens;
        Ast.DoWhile { body; condition; id = "" }
    (* "for" "(" <for-init> [ <exp> ] ";" [ <exp> ] ")" <statement> *)
    | T.KWFor ->
        expect KWFor tokens;
        expect OpenParen tokens;
        let init = parse_for_init tokens in
        let condition = parse_optional_exp T.Semicolon tokens in
        let post = parse_optional_exp T.CloseParen tokens in
        let body = parse_statement tokens in
        Ast.For { init; condition; post; body; id = "" }
    (* <exp> ";" | ";" *)
    | _ -> (
        let opt_exp = parse_optional_exp T.Semicolon tokens in
        match opt_exp with Some e -> Ast.Expression e | None -> Ast.Null)

  (* <block-item> ::= <statement> | <declaration> *)
  and parse_block_item tokens =
    if is_specifier (Tok_stream.peek tokens) then
      Ast.D (parse_declaration tokens)
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

  (* <program> ::= { <declaration> } *)
  let parse_program tokens =
    let rec parse_decl_loop () =
      if Tok_stream.is_empty tokens then []
      else
        let next_decl = parse_declaration tokens in
        next_decl :: parse_decl_loop ()
    in
    let fun_decls = parse_decl_loop () in
    Ast.Program fun_decls
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
