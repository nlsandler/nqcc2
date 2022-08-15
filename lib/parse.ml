module T = struct
  include Tokens
end

module Ast = struct
  include Ast.Untyped
end

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

(* getting a list of specifiers *)

let rec parse_type_specifer_list tokens =
  match peek tokens with
  | T.KWInt | T.KWLong ->
      let spec = Stream.next tokens in
      spec :: parse_type_specifer_list tokens
  | _ -> []

let rec parse_specifier_list tokens =
  match peek tokens with
  | T.KWInt | KWLong | KWStatic | KWExtern ->
      let spec = Stream.next tokens in
      spec :: parse_specifier_list tokens
  | _ -> []

let parse_storage_class = function
  | T.KWExtern -> Ast.Extern
  | KWStatic -> Static
  | _ -> failwith "Internal error: bad storage class" [@coverage off]

let parse_type = function
  | [ T.KWInt ] -> Types.Int
  | [ KWInt; KWLong ] | [ KWLong; KWInt ] | [ KWLong ] -> Long
  | _ -> failwith "Invalid type specifier"

let parse_type_and_storage_class specifier_list =
  let types, storage_classes =
    List.partition (fun tok -> tok = T.KWInt || tok = KWLong) specifier_list
  in
  let typ = parse_type types in
  let storage_class =
    match storage_classes with
    | [] -> None
    | [ sc ] -> Some (parse_storage_class sc)
    | _ :: _ -> failwith "Invalid storage class"
  in
  (typ, storage_class)

(* parsing grammar symbols *)

(* <identifier> ::= ? An identifier token ? *)
let parse_id tokens =
  match Stream.next tokens with
  | T.Identifier x -> x
  | other -> raise_error ~expected:(Name "an identifier") ~actual:other

(* <int> ::= ? A constant token ? *)
let parse_constant tokens =
  try
    match Stream.next tokens with
    | T.ConstInt c -> (
        match Big_int.int32_of_big_int_opt c with
        | Some i32 -> Const.ConstInt i32
        | None -> ConstLong (Big_int.int64_of_big_int c))
    | T.ConstLong c -> ConstLong (Big_int.int64_of_big_int c)
    (* we only call this when we know the next token is a constant *)
    | _ ->
        raise (ParseError "Internal error when parsing constant")
        [@coverage off]
  with Failure _ ->
    (* int64_of_big_int raises failure when value is out of bounds *)
    raise (ParseError "Constant is too large to fit in an int or long")

(* <unop> ::= "-" | "~" *)
let parse_unop tokens =
  match Stream.next tokens with
  | T.Tilde -> Ast.Complement
  | T.Hyphen -> Negate
  | T.Bang -> Not
  (* we only call this when we know the next token is a unop *)
  | _ ->
      raise (ParseError "Internal error when parsing unary operator")
      [@coverage off]

(* <binop> ::= "-" | "+" | "*" | "/" | "%" *)
let parse_binop tokens =
  match Stream.next tokens with
  | T.Plus -> Ast.Add
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
  | T.ConstInt _ | T.ConstLong _ -> Ast.Constant (parse_constant tokens)
  (* identifier *)
  | T.Identifier _ -> (
      let id = parse_id tokens in
      (* look at next token to figure out whether this is a variable or function call *)
      match peek tokens with
      | T.OpenParen ->
          let args = parse_optional_arg_list tokens in
          FunCall { f = id; args }
      | _ -> Var id)
  (* unary expression *)
  | T.Hyphen | T.Tilde | T.Bang ->
      let operator = parse_unop tokens in
      let inner_exp = parse_factor tokens in
      Unary (operator, inner_exp)
  (* parenthesized expression *)
  | T.OpenParen -> (
      (* this is either a cast or a parenthesized expression *)
      (* Stream.junk consumes open paren *)
      let _ = Stream.junk tokens in
      match peek tokens with
      | KWInt | KWLong ->
          (* it's a cast*)
          let type_specifiers = parse_type_specifer_list tokens in
          let target_type = parse_type type_specifiers in
          expect T.CloseParen tokens;
          let inner_exp = parse_factor tokens in
          Cast { target_type; e = inner_exp }
      | _ ->
          let e = parse_expression 0 tokens in
          let _ = expect T.CloseParen tokens in
          e)
  (* errors *)
  | t -> raise_error ~expected:(Name "a factor") ~actual:t

(* "(" [ <argument-list> ] ")" ]*)
and parse_optional_arg_list tokens =
  expect OpenParen tokens;
  let args =
    match peek tokens with T.CloseParen -> [] | _ -> parse_arg_list tokens
  in
  expect T.CloseParen tokens;
  args

(* <argument-list> ::= <exp> { "," <exp> } *)
and parse_arg_list tokens =
  let arg = parse_expression 0 tokens in
  match peek tokens with
  | T.Comma ->
      Stream.junk tokens;
      arg :: parse_arg_list tokens
  | _ -> [ arg ]

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
            let left = Ast.Assignment (left, right) in
            parse_exp_loop left (peek tokens)
        | T.QuestionMark ->
            let middle = parse_conditional_middle tokens in
            let right = parse_expression prec tokens in
            let left =
              Ast.Conditional
                { condition = left; then_result = middle; else_result = right }
            in
            parse_exp_loop left (peek tokens)
        | _ ->
            let operator = parse_binop tokens in
            let right = parse_expression (prec + 1) tokens in
            let left = Ast.Binary (operator, left, right) in
            parse_exp_loop left (peek tokens))
    | _ -> left
  in
  parse_exp_loop initial_factor next_token

(* parse an optional expression followed by a delimiter *)
let parse_optional_expression delim tokens =
  if peek tokens = delim then (
    Stream.junk tokens;
    None)
  else
    let e = parse_expression 0 tokens in
    expect delim tokens;
    Some e

(* <statement> ::= "return" <exp> ";"
                 | <exp> ";"
                 | "if" "(" <exp> ")" <statement> [ "else" <statement> ]
                 | <block>
                 | "break" ";"
                 | "continue" ";"
                 | "while" "(" <exp> ")" <statement>
                 | "do" <statement> "while" "(" <exp> ")" ";"
                 | "for" "(" <for-init> [ <exp> ] ";" [ <exp> ] ")" <statement>
                 | ";"
*)
let rec parse_statement tokens =
  match peek tokens with
  | T.KWIf -> parse_if_statement tokens
  | T.OpenBrace -> Ast.Compound (parse_block tokens)
  | T.KWDo -> parse_do_loop tokens
  | T.KWWhile -> parse_while_loop tokens
  | T.KWFor -> parse_for_loop tokens
  | T.KWBreak ->
      Stream.junk tokens;
      expect T.Semicolon tokens;
      Break ""
  | T.KWContinue ->
      Stream.junk tokens;
      expect T.Semicolon tokens;
      Continue ""
  | T.KWReturn ->
      (* consume return keyword *)
      let _ = Stream.junk tokens in
      let exp = parse_expression 0 tokens in
      let _ = expect T.Semicolon tokens in
      Return exp
  | _ -> (
      let opt_exp = parse_optional_expression T.Semicolon tokens in
      match opt_exp with Some exp -> Expression exp | None -> Null)

(* "if" "(" <exp> ")" <statement> [ "else" <statement> ] *)
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

(* "do" <statement> "while" "(" <exp> ")" ";" *)
and parse_do_loop tokens =
  expect KWDo tokens;
  let body = parse_statement tokens in
  expect KWWhile tokens;
  expect OpenParen tokens;
  let condition = parse_expression 0 tokens in
  expect CloseParen tokens;
  expect Semicolon tokens;
  DoWhile { body; condition; id = "" }

(* "while" "(" <exp> ")" <statement> *)
and parse_while_loop tokens =
  expect KWWhile tokens;
  expect OpenParen tokens;
  let condition = parse_expression 0 tokens in
  expect CloseParen tokens;
  let body = parse_statement tokens in
  While { condition; body; id = "" }

(* "for" "(" <for-init> [ <exp> ] ";" [ <exp> ] ")" <statement> *)
and parse_for_loop tokens =
  expect KWFor tokens;
  expect OpenParen tokens;
  let init = parse_for_init tokens in
  let condition = parse_optional_expression T.Semicolon tokens in
  let post = parse_optional_expression T.CloseParen tokens in
  let body = parse_statement tokens in
  For { init; condition; post; body; id = "" }

(* <block-item> ::= <statement> | <declaration> *)
and parse_block_item tokens =
  match peek tokens with
  | T.(KWInt | KWLong | KWStatic | KWExtern) -> Ast.D (parse_declaration tokens)
  | _ -> S (parse_statement tokens)

(* helper function to parse list of block items, stopping when we hit a close brace *)
and parse_block_item_list tokens =
  match peek tokens with
  | T.CloseBrace -> []
  | _ ->
      let next_block_item = parse_block_item tokens in
      next_block_item :: parse_block_item_list tokens

(* <block> ::= "{" { <block-item> } "}" *)
and parse_block tokens =
  expect T.OpenBrace tokens;
  let block_items = parse_block_item_list tokens in
  expect T.CloseBrace tokens;
  Block block_items

(*
   <function-declaration> ::= "int" <identifier> "(" "void" | <param-list> ")" ( <block> | ";")
   we've already parsed "int" <identifier>
*)
and finish_parsing_function_declaration ret_type storage_class name tokens =
  expect T.OpenParen tokens;
  let params_with_types =
    match peek tokens with
    | T.KWVoid ->
        Stream.junk tokens;
        []
    | _ -> parse_param_list tokens
  in
  let param_types, params = List.split params_with_types in
  expect T.CloseParen tokens;
  let body =
    match peek tokens with
    | T.OpenBrace -> Some (parse_block tokens)
    | T.Semicolon ->
        Stream.junk tokens;
        None
    | other ->
        raise_error ~expected:(Name "function body or semicolon") ~actual:other
  in
  let fun_type = Types.FunType { param_types; ret_type } in
  Ast.{ name; fun_type; storage_class; params; body }

(* <param-list> ::= "int" <identifier> { "," "int" <identifier> } *)
and parse_param_list tokens =
  let specifiers = parse_type_specifer_list tokens in
  let param_type = parse_type specifiers in
  let next_param = parse_id tokens in
  match peek tokens with
  | T.Comma ->
      (* parse the rest of the param list *)
      Stream.junk tokens;
      (param_type, next_param) :: parse_param_list tokens
  | _ -> [ (param_type, next_param) ]

(* <variable-declaration> ::= "int" <identifier> [ "=" <exp> ] ";"
   we've already parsed "int" <identifer> *)
and finish_parsing_variable_declaration var_type storage_class name tokens =
  match Stream.next tokens with
  | T.Semicolon -> Ast.{ name; var_type; storage_class; init = None }
  | T.EqualSign ->
      let init = parse_expression 0 tokens in
      expect T.Semicolon tokens;
      { name; var_type; storage_class; init = Some init }
  | other ->
      raise_error ~expected:(Name "An initializer or semicolon") ~actual:other

(* <declaration> ::= <variable-declaration> | <function-declaration>
   parse "int" <identifier>, then call appropriate function to finish parsing
*)
and parse_declaration tokens =
  let specifiers = parse_specifier_list tokens in
  let typ, storage_class = parse_type_and_storage_class specifiers in
  let name = parse_id tokens in
  match peek tokens with
  | T.OpenParen ->
      FunDecl
        (finish_parsing_function_declaration typ storage_class name tokens)
  | _ ->
      VarDecl
        (finish_parsing_variable_declaration typ storage_class name tokens)

(* helper function to accept variable declarations and reject function declarations *)
and parse_variable_declaration tokens =
  match parse_declaration tokens with
  | VarDecl vd -> vd
  | FunDecl _ ->
      raise
        (ParseError
           "Expected variable declaration but found function declaration")

(* <for-init> ::= <declaration> | [ <exp> ] ";" *)
and parse_for_init tokens =
  match peek tokens with
  (* note that a static or extern keyword here is invalid, but we'll catch that in semantic analysis *)
  | T.(KWInt | KWLong | KWStatic | KWExtern) ->
      InitDecl (parse_variable_declaration tokens)
  | _ ->
      let opt_e = parse_optional_expression T.Semicolon tokens in
      InitExp opt_e

(* { <function-declaration> } *)
let rec parse_declaration_list tokens =
  match Stream.peek tokens with
  | None -> (* we've reached the end of the input *) []
  | Some _ ->
      let next_decl = parse_declaration tokens in

      next_decl :: parse_declaration_list tokens

(* <program> ::= { <function-declaration> } *)
let parse tokens =
  try
    let token_stream = Stream.of_list tokens in
    let declarations = parse_declaration_list token_stream in
    Ast.Program declarations
  with Stream.Failure -> raise (ParseError "Unexpected end of file")
