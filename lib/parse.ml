module T = struct
  include Tokens
end

module Ast = struct
  include Ast.Untyped
end

open Cnums

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

(* unescape a string - have to deal with escape sequences individually
 * b/c OCaml lexical convention doesn't recognize all of them *)
let unescape s =
  let rec f s' =
    match String_utils.prefix s' 2 with
    | [] -> []
    | [ '\\'; '\'' ] -> '\'' :: f (String_utils.drop 2 s')
    | [ '\\'; '"' ] -> '"' :: f (String_utils.drop 2 s')
    | [ '\\'; '?' ] -> '?' :: f (String_utils.drop 2 s')
    | [ '\\'; '\\' ] -> '\\' :: f (String_utils.drop 2 s')
    | [ '\\'; 'a' ] -> Char.chr 7 :: f (String_utils.drop 2 s')
    | [ '\\'; 'b' ] -> '\b' :: f (String_utils.drop 2 s')
    | [ '\\'; 'f' ] -> Char.chr 12 :: f (String_utils.drop 2 s')
    | [ '\\'; 'n' ] -> '\n' :: f (String_utils.drop 2 s')
    | [ '\\'; 'r' ] -> '\r' :: f (String_utils.drop 2 s')
    | [ '\\'; 't' ] -> '\t' :: f (String_utils.drop 2 s')
    | [ '\\'; 'v' ] -> Char.chr 11 :: f (String_utils.drop 2 s')
    | '\\' :: _ ->
        failwith
          "Internal error: not a valid escape sequence; should have been \
           rejected during lexing" [@coverage off]
    | x :: _ -> x :: f (String_utils.drop_first s')
  in
  String.of_seq (List.to_seq (f s))

(* getting a list of specifiers *)

let is_type_specifier = function
  | T.KWInt | KWLong | KWUnsigned | KWSigned | KWDouble | KWChar | KWVoid ->
      true
  | _ -> false

let is_specifier = function
  | T.KWStatic | KWExtern -> true
  | other -> is_type_specifier other

let rec parse_type_specifier_list tokens =
  if is_type_specifier (peek tokens) then
    let spec = Stream.next tokens in
    spec :: parse_type_specifier_list tokens
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
  (* sort specifiers so we don't need to check for different
   * orderings of same specifiers *)
  let specifier_list = List.sort Tokens.compare specifier_list in
  match specifier_list with
  | [ T.KWVoid ] -> Types.Void
  | [ T.KWDouble ] -> Types.Double
  | [ T.KWChar ] -> Types.Char
  | [ T.KWChar; T.KWSigned ] -> Types.SChar
  | [ T.KWChar; T.KWUnsigned ] -> Types.UChar
  | _ ->
      if
        specifier_list = []
        || List.sort_uniq compare specifier_list
           <> List.sort compare specifier_list
        || List.mem T.KWDouble specifier_list
        || List.mem T.KWChar specifier_list
        || List.mem T.KWVoid specifier_list
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

(* parsing constants *)

(* <int> ::= ? A constant token ?
   <char> :: ? A char token ?
   <long> ::= ? an int or long token ?
   <uint> ::= ? An unsigned int token ?
   <ulong> ::= ? An unsigned int or unsigned long token ?
   <double ::= ? A floating-point constant token ?
*)
let parse_constant tokens =
  try
    match Stream.next tokens with
    | T.ConstChar s ->
        let s' = unescape s in
        if String.length s' = 1 then
          Const.ConstInt (Int32.of_int (Char.code s'.[0]))
        else
          failwith
            "Internal error: Character token contains multiple characters, \
             lexer should have rejected this" [@coverage off]
    | T.ConstDouble d -> Const.ConstDouble d
    | T.ConstInt c -> (
        try Const.ConstInt (Z.to_int32 c)
        with Z.Overflow -> ConstLong (Z.to_int64 c))
    | T.ConstLong c -> ConstLong (Z.to_int64 c)
    | T.ConstUInt c -> (
        try Const.ConstUInt (UInt32.of_z c)
        with Z.Overflow -> Const.ConstULong (UInt64.of_z c))
    | T.ConstULong c -> ConstULong (UInt64.of_z c)
    (* we only call this when we know the next token is a constant *)
    | _ ->
        raise (ParseError "Internal error when parsing constant")
        [@coverage off]
  with Failure _ ->
    (* int64_of_big_int raises failure when value is out of bounds *)
    raise
      (ParseError
         "Constant is too large to fit in an int or long with given signedness")

(* Parsing declarators *)

(* first parse declarators to this type, then convert to AST  *)
type declarator =
  | Ident of string
  | PointerDeclarator of declarator
  | ArrayDeclarator of declarator * Const.t
  | FunDeclarator of param_info list * declarator

and param_info = Param of Types.t * declarator

(* { "[" <const> "]" }+ *)
let rec parse_array_dimensions tokens =
  match peek tokens with
  | T.OpenBracket ->
      Stream.junk tokens;
      let dim = parse_constant tokens in
      expect T.CloseBracket tokens;
      dim :: parse_array_dimensions tokens
  | _ -> []

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

(* <direct-declarator> ::= <simple-declarator> [ <declarator-suffix> ]
   <declarator-suffix> ::= <param-list> | { "[" <const> "]" }+
*)
and parse_direct_declarator tokens =
  let simple_dec = parse_simple_declarator tokens in
  match peek tokens with
  | T.OpenBracket ->
      let array_dimensions = parse_array_dimensions tokens in
      List.fold_left
        (fun decl dim -> ArrayDeclarator (decl, dim))
        simple_dec array_dimensions
  | T.OpenParen ->
      let params = parse_param_list tokens in
      FunDeclarator (params, simple_dec)
  | _ -> simple_dec

(* <param-list> ::= "(" <param> { "," <param> } ")" | "(" "void" ")" *)
and parse_param_list tokens =
  expect T.OpenParen tokens;
  let params =
    match Stream.npeek 2 tokens with
    | [ T.KWVoid; T.CloseParen ] ->
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
  let specifiers = parse_type_specifier_list tokens in
  let param_type = parse_type specifiers in
  let param_decl = parse_declarator tokens in
  Param (param_type, param_decl)

(* convert constant to int and check that it's a valid array dimension: must be an integer > 0 *)
let const_to_dim c =
  let i =
    match c with
    | Const.ConstInt i -> Int32.to_int i
    | ConstLong l -> Int64.to_int l
    | ConstUInt u -> UInt32.to_int u
    | ConstULong ul -> UInt64.to_int ul
    | ConstDouble _ -> failwith "Array dimensions must have integer type"
    | ConstChar _ | ConstUChar _ ->
        failwith "Internal error, we're not using these yet" [@coverage off]
  in
  if i > 0 then i else failwith "Array dimension must be greater than zero"

let rec process_declarator decl base_type =
  match decl with
  | Ident s -> (s, base_type, [])
  | PointerDeclarator d ->
      let derived_type = Types.Pointer base_type in
      process_declarator d derived_type
  | ArrayDeclarator (inner, cnst) ->
      let size = const_to_dim cnst in
      let derived_type = Types.Array { elem_type = base_type; size } in
      process_declarator inner derived_type
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
  | AbstractArray of abstract_declarator * Const.t
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
        | T.Star | T.OpenParen | T.OpenBracket ->
            (* there's an inner declarator *)
            parse_abstract_declarator tokens
        | T.CloseParen -> AbstractBase
        | other ->
            raise_error ~expected:(Name "an abstract declarator") ~actual:other
      in
      AbstractPointer inner
  | _ -> parse_direct_abstract_declarator tokens

(* <direct-abstract-declarator ::= "(" <abstract-declarator> ")" { "[" <const> "]" }
                                | { "[" <const> "]" }+
*)
and parse_direct_abstract_declarator tokens =
  match peek tokens with
  | T.OpenParen ->
      Stream.junk tokens;
      let abstr_decl = parse_abstract_declarator tokens in
      expect T.CloseParen tokens;
      (* inner declarator is followed by possibly-empty list of aray dimensions *)
      let array_dimensions = parse_array_dimensions tokens in
      List.fold_left
        (fun decl dim -> AbstractArray (decl, dim))
        abstr_decl array_dimensions
  | T.OpenBracket ->
      let array_dimensions = parse_array_dimensions tokens in
      List.fold_left
        (fun decl dim -> AbstractArray (decl, dim))
        AbstractBase array_dimensions
  | other ->
      raise_error ~expected:(Name "an abstract direct declarator") ~actual:other

let rec process_abstract_declarator decl base_type =
  match decl with
  | AbstractBase -> base_type
  | AbstractArray (inner, cnst) ->
      let dim = const_to_dim cnst in
      let derived_type = Types.Array { elem_type = base_type; size = dim } in
      process_abstract_declarator inner derived_type
  | AbstractPointer inner ->
      let derived_type = Types.Pointer base_type in
      process_abstract_declarator inner derived_type

(* <type-name> ::= { <type-specifier> }+ [ <abstract-declarator> ] *)
let parse_type_name tokens =
  let type_specifiers = parse_type_specifier_list tokens in
  let base_type = parse_type type_specifiers in
  (* check for optional abstract declarator
   * note that <type-name> is always followed by close paren,
   * although that's not part of the grammar rule
   *)
  match peek tokens with
  | T.CloseParen -> base_type
  | _ ->
      let abstract_decl = parse_abstract_declarator tokens in
      process_abstract_declarator abstract_decl base_type

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

let rec parse_string_literals tokens =
  match peek tokens with
  | T.StringLiteral s ->
      Stream.junk tokens;
      unescape s ^ parse_string_literals tokens
  | _ -> ""

(*
   <primary-exp> ::= <const> | <identifier> | "(" <exp> ")" | { <string> }+
                   | <identifier> "(" [ <argument-list> ] ")"
*)
let rec parse_primary_expression tokens =
  let next_token = peek tokens in
  match next_token with
  (* constant *)
  | T.ConstChar _ | T.ConstInt _ | T.ConstLong _ | T.ConstUInt _
  | T.ConstULong _ | T.ConstDouble _ ->
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
  | T.StringLiteral _ ->
      let string_exp = parse_string_literals tokens in
      Ast.String string_exp
  | T.OpenParen ->
      Stream.junk tokens;
      let e = parse_expression 0 tokens in
      expect T.CloseParen tokens;
      e
  | other -> raise_error ~expected:(Name "a primary expression") ~actual:other

(* <postfix-exp> ::= <primary-exp> { "[" <exp> "]" } *)
and parse_postfix_expression tokens =
  let primary = parse_primary_expression tokens in
  postfix_helper primary tokens

and postfix_helper primary tokens =
  if peek tokens = T.OpenBracket then (
    Stream.junk tokens;
    let index = parse_expression 0 tokens in
    expect T.CloseBracket tokens;
    let subscript_exp = Ast.Subscript { ptr = primary; index } in
    postfix_helper subscript_exp tokens)
  else primary

(* <cast-exp> ::= "(" <type-name> ")" <cast-exp>
               | <unary-exp>
*)
and parse_cast_expression tokens =
  match Stream.npeek 2 tokens with
  | T.OpenParen :: t :: _ when is_type_specifier t ->
      (* this is a cast expression *)
      (* Stream.junk consumes open paren *)
      Stream.junk tokens;
      let target_type = parse_type_name tokens in
      expect T.CloseParen tokens;
      let inner_exp = parse_cast_expression tokens in
      Ast.Cast { target_type; e = inner_exp }
  | _ -> parse_unary_expression tokens

(*
   <unary-exp> ::= <unop> <cast-exp>
               | "sizeof" <unary-exp>
               | "sizeof" "(" <type-name> ")"
               | <postfix-exp>
*)
and parse_unary_expression tokens =
  (* look ahead 2 tokens to distinguish between cast and parenthesized exp *)
  match Stream.npeek 3 tokens with
  (* unary expressions *)
  | T.Star :: _ ->
      Stream.junk tokens;
      let inner_exp = parse_cast_expression tokens in
      Ast.Dereference inner_exp
  | T.Ampersand :: _ ->
      Stream.junk tokens;
      let inner_exp = parse_cast_expression tokens in
      AddrOf inner_exp
  | (T.Hyphen | T.Tilde | T.Bang) :: _ ->
      let operator = parse_unop tokens in
      let inner_exp = parse_cast_expression tokens in
      Unary (operator, inner_exp)
  | T.KWSizeOf :: T.OpenParen :: t :: _ when is_type_specifier t ->
      (* this is the size of a type name *)
      Stream.junk tokens;
      Stream.junk tokens;
      let target_type = parse_type_name tokens in
      expect T.CloseParen tokens;
      SizeOfT target_type
  | T.KWSizeOf :: _ ->
      (*size of an expression*)
      Stream.junk tokens;
      let inner_exp = parse_unary_expression tokens in
      SizeOf inner_exp
  | _ -> parse_postfix_expression tokens

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
  let initial_factor = parse_cast_expression tokens in
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

(* <initializer> ::= <exp> | "{" <initializer> { "," <initializer> } [ "," ] "}" *)
let rec parse_initializer tokens =
  if peek tokens = T.OpenBrace then (
    Stream.junk tokens;
    let init_list = parse_init_list tokens in
    expect T.CloseBrace tokens;
    Ast.CompoundInit init_list)
  else
    let e = parse_expression 0 tokens in
    Ast.SingleInit e

and parse_init_list tokens =
  let next_init = parse_initializer tokens in
  match Stream.npeek 2 tokens with
  (* trailing comma - consume it and return *)
  | T.Comma :: T.CloseBrace :: _ ->
      Stream.junk tokens;
      [ next_init ]
  (* comma that isn't followed by a brace means there's one more element *)
  | T.Comma :: _ ->
      Stream.junk tokens;
      next_init :: parse_init_list tokens
  | _ -> [ next_init ]

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
      let exp = parse_optional_expression T.Semicolon tokens in
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
      let init = parse_initializer tokens in
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
