open Batteries

module T = struct
  include Tokens
end

module Ast = struct
  include Ast.Untyped
end

module Big_int = struct
  include Extended_big_int
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
  | T.DoubleLeftBracket | T.DoubleRightBracket -> Some 40
  | T.(LessThan | LessOrEqual | GreaterThan | GreaterOrEqual) -> Some 35
  | T.(DoubleEqual | NotEqual) -> Some 30
  | T.Ampersand -> Some 25
  | T.Caret -> Some 20
  | T.Pipe -> Some 15
  | T.LogicalAnd -> Some 10
  | T.LogicalOr -> Some 5
  | T.QuestionMark -> Some 3
  | T.(
      ( EqualSign | PlusEqual | HyphenEqual | StarEqual | SlashEqual
      | PercentEqual | AmpersandEqual | CaretEqual | PipeEqual
      | DoubleLeftBracketEqual | DoubleRightBracketEqual )) ->
      Some 1
  | _ -> None

let get_compound_operator = function
  | T.EqualSign -> None
  | T.PlusEqual -> Some Ast.Add
  | T.HyphenEqual -> Some Subtract
  | T.SlashEqual -> Some Divide
  | T.StarEqual -> Some Multiply
  | T.PercentEqual -> Some Mod
  | T.AmpersandEqual -> Some BitwiseAnd
  | T.PipeEqual -> Some BitwiseOr
  | T.CaretEqual -> Some BitwiseXor
  | T.DoubleLeftBracketEqual -> Some BitshiftLeft
  | T.DoubleRightBracketEqual -> Some BitshiftRight
  | t -> failwith ("Internal error: not an assignment operator: " ^ T.show t)

let is_assignment = function
  | T.(
      ( EqualSign | PlusEqual | HyphenEqual | StarEqual | SlashEqual
      | PercentEqual | AmpersandEqual | CaretEqual | PipeEqual
      | DoubleLeftBracketEqual | DoubleRightBracketEqual )) ->
      true
  | _ -> false

(* getting a list of specifiers *)

let is_type_specifier = function
  | T.KWInt | KWLong | KWUnsigned | KWSigned | KWDouble -> true
  | _ -> false

let is_specifier = function
  | T.KWStatic | KWExtern -> true
  | other -> is_type_specifier other

let rec parse_type_specifer_list tokens =
  if is_type_specifier (peek tokens) then
    let spec = Stream.next tokens in
    spec :: parse_type_specifer_list tokens
  else []

let rec parse_specifier_list tokens =
  if is_specifier (peek tokens) then
    let spec = Stream.next tokens in
    spec :: parse_specifier_list tokens
  else []

let parse_storage_class = function
  | T.KWExtern -> Ast.Extern
  | KWStatic -> Static
  | _ -> failwith "Internal error: bad storage class" [@coverage off]

let parse_type specifier_list =
  if specifier_list = [ T.KWDouble ] then Types.Double
  else if
    specifier_list = []
    || List.unique specifier_list <> specifier_list
    || List.mem T.KWDouble specifier_list
    || List.mem T.KWSigned specifier_list
       && List.mem T.KWUnsigned specifier_list
  then failwith "Invalid type specifier"
  else if
    List.mem T.KWUnsigned specifier_list && List.mem T.KWLong specifier_list
  then Types.ULong
  else if List.mem T.KWUnsigned specifier_list then Types.UInt
  else if List.mem T.KWLong specifier_list then Types.Long
  else Types.Int

let parse_type_and_storage_class specifier_list =
  let types, storage_classes =
    List.partition (fun tok -> is_type_specifier tok) specifier_list
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

(* Parsing declarators *)

(* first parse declarators to this type, then convert to AST  *)
type declarator =
  | Ident of string
  | PointerDeclarator of declarator
  | FunDeclarator of param_info list * declarator

and param_info = Param of Types.t * declarator

(* <simple-declarator> ::= <identifier> | "(" <declarator> ")" *)
let rec parse_simple_declarator tokens =
  let next_tok = Stream.next tokens in
  match next_tok with
  | T.OpenParen ->
      let decl = parse_declarator tokens in
      expect T.CloseParen tokens;
      decl
  | Identifier id -> Ident id
  | other -> raise_error ~expected:(Name "a simple declarator") ~actual:other

(* <declarator> ::= "*" <declarator> | <direct-declarator> *)
and parse_declarator tokens =
  match peek tokens with
  | T.Star ->
      Stream.junk tokens;
      let inner = parse_declarator tokens in
      PointerDeclarator inner
  | _ -> parse_direct_declarator tokens

(* <direct-declarator> ::= <simple-declarator> [ <param-list> ] *)
and parse_direct_declarator tokens =
  let simple_dec = parse_simple_declarator tokens in
  match peek tokens with
  | T.OpenParen ->
      let params = parse_param_list tokens in
      FunDeclarator (params, simple_dec)
  | _ -> simple_dec

(* <param-list> ::= "(" <param> { "," <param> } ")" | "(" "void" ")" *)
and parse_param_list tokens =
  expect T.OpenParen tokens;
  let params =
    match peek tokens with
    | T.KWVoid ->
        Stream.junk tokens;
        []
    | _ -> param_loop tokens
  in
  expect T.CloseParen tokens;
  params

and param_loop tokens =
  let p = parse_param tokens in
  match peek tokens with
  | T.Comma ->
      (* parse the rest of the param list *)
      Stream.junk tokens;
      p :: param_loop tokens
  | _ -> [ p ]

(* <param> ::= { <type-specifier> }+ <declarator> *)
and parse_param tokens =
  let specifiers = parse_type_specifer_list tokens in
  let param_type = parse_type specifiers in
  let param_decl = parse_declarator tokens in
  Param (param_type, param_decl)

let rec process_declarator decl base_type =
  match decl with
  | Ident s -> (s, base_type, [])
  | PointerDeclarator d ->
      let derived_type = Types.Pointer base_type in
      process_declarator d derived_type
  | FunDeclarator (params, Ident s) ->
      let process_param (Param (p_base_type, p_decl)) =
        let param_name, param_t, _ = process_declarator p_decl p_base_type in
        (match param_t with
        | Types.FunType _ ->
            raise
              (ParseError "Function pointers in parameters are not supported")
        | _ -> ());
        (param_name, param_t)
      in
      let param_names, param_types =
        List.split (List.map process_param params)
      in
      let fun_type = Types.FunType { param_types; ret_type = base_type } in
      (s, fun_type, param_names)
  | FunDeclarator _ ->
      raise
        (ParseError
           "can't apply additional type derivations to a function declarator")

(* abstract declarators*)
type abstract_declarator =
  | AbstractPointer of abstract_declarator
  | AbstractBase

(* <abstract-declarator> ::= "*" [ <abstract-declarator> ]
                        | <direct-abstract-declarator>
*)

let rec parse_abstract_declarator tokens =
  match peek tokens with
  | T.Star ->
      (* it's a pointer declarator *)
      Stream.junk tokens;
      let inner =
        match peek tokens with
        | T.Star | T.OpenParen ->
            (* there's an inner declarator *)
            parse_abstract_declarator tokens
        | T.CloseParen -> AbstractBase
        | other ->
            raise_error ~expected:(Name "an abstract declarator") ~actual:other
      in
      AbstractPointer inner
  | _ -> parse_direct_abstract_declarator tokens

(* <direct-abstract-declarator ::= "(" <abstract-declarator> ")" *)
and parse_direct_abstract_declarator tokens =
  expect T.OpenParen tokens;
  let decl = parse_abstract_declarator tokens in
  expect T.CloseParen tokens;
  decl

let rec process_abstract_declarator decl base_type =
  match decl with
  | AbstractBase -> base_type
  | AbstractPointer inner ->
      let derived_type = Types.Pointer base_type in
      process_abstract_declarator inner derived_type

(* <int> ::= ? A constant token ? *)
let parse_constant tokens =
  try
    match Stream.next tokens with
    | T.ConstDouble d -> Const.ConstDouble d
    | T.ConstInt c -> (
        match Big_int.int32_of_big_int_opt c with
        | Some i32 -> Const.ConstInt i32
        | None -> ConstLong (Big_int.int64_of_big_int c))
    | T.ConstLong c -> ConstLong (Big_int.int64_of_big_int c)
    | T.ConstUInt c -> (
        match Big_int.uint32_of_big_int_opt c with
        | Some ui32 -> Const.ConstUInt ui32
        | None -> ConstULong (Big_int.uint64_of_big_int c))
    | T.ConstULong c -> ConstULong (Big_int.uint64_of_big_int c)
    (* we only call this when we know the next token is a constant *)
    | _ ->
        raise (ParseError "Internal error when parsing constant")
        [@coverage off]
  with Failure _ ->
    (* int64_of_big_int raises failure when value is out of bounds *)
    raise
      (ParseError
         "Constant is too large to fit in an int or long with given signedness")

(* <unop> ::= "-" | "~" *)
let parse_unop tokens =
  match Stream.next tokens with
  | T.Tilde -> Ast.Complement
  | T.Hyphen -> Negate
  | T.Bang -> Not
  | T.DoublePlus -> Incr
  | T.DoubleHyphen -> Decr
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
  | T.Ampersand ->
      (* Check this here b/c we can't check during lexing - & token is also address operator *)
      if List.mem Settings.Bitwise !Settings.extra_credit_flags then BitwiseAnd
      else failwith "bitwise & operator not enabled "
  | T.Caret -> BitwiseXor
  | T.Pipe -> BitwiseOr
  | T.DoubleLeftBracket -> BitshiftLeft
  | T.DoubleRightBracket -> BitshiftRight
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

(* <primary-exp> ::= <int> | <identifier> | <identifier> "(" [ <argument-list> ] ")" | "(" <exp> ")" *)
let rec parse_primary_expression tokens =
  let next_token = peek tokens in
  match next_token with
  (* constant *)
  | T.ConstInt _ | T.ConstLong _ | T.ConstUInt _ | T.ConstULong _
  | T.ConstDouble _ ->
      Ast.Constant (parse_constant tokens)
  (* identifier *)
  | T.Identifier _ -> (
      let id = parse_id tokens in
      (* look at next token to figure out whether this is a variable or function call *)
      match peek tokens with
      | T.OpenParen ->
          let args = parse_optional_arg_list tokens in
          FunCall { f = id; args }
      | _ -> Var id)
  (* parenthesized expression. NOTE: we know this isn't a cast b/c we would have already consumed that in parse_factor *)
  | T.OpenParen ->
      (* consume open paren *)
      let _ = Stream.junk tokens in
      let e = parse_expression 0 tokens in
      let _ = expect T.CloseParen tokens in
      e
  (* errors *)
  | t -> raise_error ~expected:(Name "a factor") ~actual:t

(* <postfix-exp> ::= <primary-exp> { "++" | "--" } *)
and parse_postfix_exp tokens =
  let primary = parse_primary_expression tokens in
  postfix_helper primary tokens

and postfix_helper primary tokens =
  match peek tokens with
  | T.DoubleHyphen ->
      Stream.junk tokens;
      let decr_exp = Ast.PostfixDecr primary in
      postfix_helper decr_exp tokens
  | T.DoublePlus ->
      Stream.junk tokens;
      let incr_exp = Ast.PostfixIncr primary in
      postfix_helper incr_exp tokens
  | _ -> primary

(* <factor> ::= <unop> <factor> | "(" { <type-specifier> }+ ")" | factor | <postfix-exp> *)
and parse_factor tokens =
  let next_tokens = Stream.npeek 2 tokens in
  match next_tokens with
  (* unary expression *)
  | (T.Hyphen | T.Tilde | T.Bang | T.DoublePlus | T.DoubleHyphen) :: _ ->
      let operator = parse_unop tokens in
      let inner_exp = parse_factor tokens in
      Ast.Unary (operator, inner_exp)
  | T.Star :: _ ->
      Stream.junk tokens;
      let inner_exp = parse_factor tokens in
      Dereference inner_exp
  | T.Ampersand :: _ ->
      Stream.junk tokens;
      let inner_exp = parse_factor tokens in
      AddrOf inner_exp
  | T.OpenParen :: t :: _ when is_type_specifier t ->
      (* it's a cast - consume open paren, then parse type specifiers *)
      let _ = Stream.junk tokens in
      let type_specifiers = parse_type_specifer_list tokens in
      let base_type = parse_type type_specifiers in
      (* check for optional abstract declarator *)
      let target_type =
        match peek tokens with
        | T.CloseParen -> base_type
        | _ ->
            let abstract_decl = parse_abstract_declarator tokens in
            process_abstract_declarator abstract_decl base_type
      in
      expect T.CloseParen tokens;
      let inner_exp = parse_factor tokens in
      Cast { target_type; e = inner_exp }
  | _ -> parse_postfix_exp tokens

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
    | Some prec when prec >= min_prec ->
        if is_assignment next then
          let _ = Stream.junk tokens in
          let right = parse_expression prec tokens in
          let left =
            match get_compound_operator next with
            | None -> Ast.Assignment (left, right)
            | Some op -> CompoundAssignment (op, left, right)
          in
          parse_exp_loop left (peek tokens)
        else if next = T.QuestionMark then
          let middle = parse_conditional_middle tokens in
          let right = parse_expression prec tokens in
          let left =
            Ast.Conditional
              { condition = left; then_result = middle; else_result = right }
          in
          parse_exp_loop left (peek tokens)
        else
          let operator = parse_binop tokens in
          let right = parse_expression (prec + 1) tokens in
          let left = Ast.Binary (operator, left, right) in
          parse_exp_loop left (peek tokens)
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
                 | <label> ":" <statement>
                 | "goto" <label> ";"
                 | <block>
                 | "break" ";"
                 | "continue" ";"
                 | "while" "(" <exp> ")" <statement>
                 | "do" <statement> "while" "(" <exp> ")" ";"
                 | "for" "(" <for-init> [ <exp> ] ";" [ <exp> ] ")" <statement>
                 | ";"
*)
let rec parse_statement tokens =
  match Stream.npeek 2 tokens with
  | T.KWIf :: _ -> parse_if_statement tokens
  | T.OpenBrace :: _ -> Ast.Compound (parse_block tokens)
  | T.KWDo :: _ -> parse_do_loop tokens
  | T.KWWhile :: _ -> parse_while_loop tokens
  | T.KWFor :: _ -> parse_for_loop tokens
  | T.KWBreak :: _ ->
      Stream.junk tokens;
      expect T.Semicolon tokens;
      Break ""
  | T.KWContinue :: _ ->
      Stream.junk tokens;
      expect T.Semicolon tokens;
      Continue ""
  | T.KWReturn :: _ ->
      (* consume return keyword *)
      let _ = Stream.junk tokens in
      let exp = parse_expression 0 tokens in
      let _ = expect T.Semicolon tokens in
      Return exp
  | KWGoto :: _ ->
      Stream.junk tokens;
      let lbl = parse_id tokens in
      expect T.Semicolon tokens;
      Goto lbl
  | KWSwitch :: _ -> parse_switch_statement tokens
  | KWCase :: _ ->
      Stream.junk tokens;
      let case_val = parse_expression 0 tokens in
      expect T.Colon tokens;
      let stmt = parse_statement tokens in
      Case (case_val, stmt, "")
  | KWDefault :: _ ->
      Stream.junk tokens;
      expect T.Colon tokens;
      let stmt = parse_statement tokens in
      Default (stmt, "")
  | [ T.Identifier lbl; T.Colon ] ->
      (* consume label and colon *)
      Stream.junk tokens;
      Stream.junk tokens;
      let stmt = parse_statement tokens in
      LabeledStatement (lbl, stmt)
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

(* "switch" "(" <exp> ")" <statement> *)
and parse_switch_statement tokens =
  expect KWSwitch tokens;
  expect OpenParen tokens;
  let control = parse_expression 0 tokens in
  expect CloseParen tokens;
  let body = parse_statement tokens in
  Switch { control; body; id = ""; cases = [] }

(* <block-item> ::= <statement> | <declaration> *)
and parse_block_item tokens =
  if is_specifier (peek tokens) then Ast.D (parse_declaration tokens)
  else S (parse_statement tokens)

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
   <function-declaration> ::= { <specifier> }+ <declarator> ( <block> | ";")
   we've already parsed { <specifier> }+ <declarator>
*)
and finish_parsing_function_declaration fun_type storage_class name params
    tokens =
  let body =
    match peek tokens with
    | T.OpenBrace -> Some (parse_block tokens)
    | T.Semicolon ->
        Stream.junk tokens;
        None
    | other ->
        raise_error ~expected:(Name "function body or semicolon") ~actual:other
  in
  Ast.{ name; fun_type; storage_class; params; body }

(* <variable-declaration> ::= { <specifier> }+ <declarator> [ "=" <exp> ] ";"
   we've already parsed { <specifier> }+ <declarator> *)
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
   parse until declarator, then call appropriate function to finish parsing
*)
and parse_declaration tokens =
  let specifiers = parse_specifier_list tokens in
  let base_typ, storage_class = parse_type_and_storage_class specifiers in
  let declarator = parse_declarator tokens in
  let name, typ, params = process_declarator declarator base_typ in
  match typ with
  | Types.FunType _ ->
      FunDecl
        (finish_parsing_function_declaration typ storage_class name params
           tokens)
  | _ ->
      if params = [] then
        VarDecl
          (finish_parsing_variable_declaration typ storage_class name tokens)
      else
        failwith "Internal error: declarator has parameters but object type"
        [@coverage off]

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
  if is_specifier (peek tokens) then
    (* note that a static or extern keyword here is invalid, but we'll catch that in semantic analysis *)
    InitDecl (parse_variable_declaration tokens)
  else
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
