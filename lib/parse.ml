open Unsigned
open Utils

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

  (* Unescape a string - see Table 16-1 *)
  let unescape s =
    (* Map from escape sequences to corresponding unescaped characters;
     * use Ocaml quoted string literals to represent escape sequences so we
     * don't need to escape the backslashes *)
    let escapes =
      [
        (* Single quote *)
        ({|\'|}, Char.chr 39);
        (* Double quote *)
        ({|\"|}, Char.chr 34);
        (* Question mark *)
        ({|\?|}, Char.chr 63);
        (* Backslash *)
        ({|\\|}, Char.chr 92);
        (* Audible alert *)
        ({|\a|}, Char.chr 7);
        (* Backspace *)
        ({|\b|}, Char.chr 8);
        (* Form feed *)
        ({|\f|}, Char.chr 12);
        (* New line *)
        ({|\n|}, Char.chr 10);
        (* Carriage return*)
        ({|\r|}, Char.chr 13);
        (* Horizontal tab*)
        ({|\t|}, Char.chr 9);
        (* Vertical tab *)
        ({|\v|}, Char.chr 11);
      ]
    in
    let rec unescape_next remaining =
      if remaining = "" then []
      else
        let find_matching_escape (escape_seq, _) =
          String.starts_with ~prefix:escape_seq remaining
        in
        match List.find_opt find_matching_escape escapes with
        (* It's an escape sequence - add unescaped character to list *)
        | Some (escape_seq, unescaped) ->
            unescaped
            :: unescape_next
                 (StringUtil.drop (String.length escape_seq) remaining)
        | None -> remaining.[0] :: unescape_next (StringUtil.drop 1 remaining)
    in
    let unescaped_list = unescape_next s in
    StringUtil.of_list unescaped_list

  (*** Parsing functions for grammar symbols ***)

  (* <identifier> ::= ? An identifier token ? *)
  let parse_id tokens =
    match Tok_stream.take_token tokens with
    | T.Identifier x -> x
    | other -> raise_error ~expected:(Name "an identifier") ~actual:other

  (*** Specifiers ***)

  (* Helper function to check whether a token is a type specifier *)
  let is_type_specifier = function
    | T.KWInt | T.KWLong | T.KWUnsigned | T.KWSigned | T.KWDouble | T.KWChar
    | T.KWVoid ->
        true
    | _ -> false

  (* <type-specifier> ::= "int" | "long" | "unsigned" | "signed" | "double"
   *                    | "char" | "void" *)
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

  (* Convert list of specifiers to a type (Listing 13-10). *)
  let parse_type specifier_list =
    (* sort specifiers so we don't need to check for different * orderings of
       same specifiers *)
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
        then raise (ParseError "Invalid type specifier")
        else if
          List.mem T.KWUnsigned specifier_list
          && List.mem T.KWLong specifier_list
        then Types.ULong
        else if List.mem T.KWUnsigned specifier_list then Types.UInt
        else if List.mem T.KWLong specifier_list then Types.Long
        else Types.Int

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

  (*** Constants/Literals ***)

  (* Convert a signed constant token to a constant AST node (Listing 11-6). *)
  let parse_signed_constant token =
    let v, is_int =
      match token with
      | T.ConstInt i -> (i, true)
      | T.ConstLong l -> (l, false)
      | other ->
          raise_error ~expected:(Name "a signed integer constant") ~actual:other
    in
    (* ~$2, etc are literals of type Z.t (an arbitrary-precision type) *)
    if Z.(gt v ((~$2 ** 63) - ~$1)) then
      raise (ParseError "Constant is too large to represent as an int or long")
    else if is_int && Z.(leq v ((~$2 ** 31) - ~$1)) then
      Const.ConstInt (Z.to_int32 v)
    else Const.ConstLong (Z.to_int64 v)

  (* Convert an unsigned constant token to a Const.t. (Analogous to Listing
     11-6.) *)
  let parse_unsigned_constant token =
    let v, is_uint =
      match token with
      | T.ConstUInt ui -> (ui, true)
      | T.ConstULong ul -> (ul, false)
      | other ->
          raise_error ~expected:(Name "an unsigned integer  constant")
            ~actual:other
    in
    (* ~$2, etc are literals of type Z.t (an arbitrary-precision type) *)
    if Z.(gt v ((~$2 ** 64) - ~$1)) then
      raise
        (ParseError
           "Constant is too large to represent as an unsigned int or unsigned \
            long")
    else if is_uint && Z.(leq v ((~$2 ** 32) - ~$1)) then
      Const.ConstUInt (UInt32.of_int32 (Z.to_int32_unsigned v))
    else Const.ConstULong (UInt64.of_int64 (Z.to_int64_unsigned v))

  (* Convert a char token to a Const.t *)
  let parse_char token =
    let unescaped = unescape token in
    if String.length unescaped = 1 then
      let ch_code = Char.code unescaped.[0] in
      Const.ConstInt (Int32.of_int ch_code)
    else raise (ParseError "multi-character constant tokens not supported")

  (* <const> ::= <int> | <long> | <uint> | <ulong> | <double> | <char>

     Just remove the next token from the stream and pass it off to the
     appropriate helper function to convert it to a Const.t *)
  let parse_const tokens =
    let const_tok = Tok_stream.take_token tokens in
    match const_tok with
    | T.ConstInt _ | T.ConstLong _ -> parse_signed_constant const_tok
    | T.ConstUInt _ | T.ConstULong _ -> parse_unsigned_constant const_tok
    | T.ConstDouble d -> Const.ConstDouble d
    | T.ConstChar c -> parse_char c
    | other -> raise_error ~expected:(Name "a constant token") ~actual:other

  (* Parse an array declarator dimension of the form "[" <const> "]" to an
     integer *)
  let parse_dim tokens =
    expect T.OpenBracket tokens;
    let dim =
      match parse_const tokens with
      | Const.ConstDouble _ ->
          raise (ParseError "Floating-point array dimensions not allowed")
      | Const.ConstChar c -> Int8.to_int c
      | Const.ConstInt i -> Int32.to_int i
      | Const.ConstLong l -> Int64.to_int l
      | Const.ConstUChar uc -> UInt8.to_int uc
      | Const.ConstUInt u -> UInt32.to_int u
      | Const.ConstULong ul -> UInt64.to_int ul
    in
    expect T.CloseBracket tokens;
    dim

  (* <string> ::= ? a string token ? *)
  let parse_string tokens =
    match Tok_stream.take_token tokens with
    | T.StringLiteral s -> unescape s
    | other -> raise_error ~expected:(Name "a string literal") ~actual:other

  (*** Abstract declarators ***)

  type abstract_declarator =
    | AbstractPointer of abstract_declarator
    | AbstractArray of abstract_declarator * int
    | AbstractBase

  (* Helper function to process sequence of array dimension suffixes of the form
     "[" <const> "]" on an abstract declarator; expects at least one array
     dimension *)
  let rec parse_abstract_array_decl_suffix base_decl tokens =
    (* Get next array dimension [const] *)
    let dim = parse_dim tokens in
    let new_decl = AbstractArray (base_decl, dim) in
    if Tok_stream.peek tokens = T.OpenBracket then
      (* There's another dimension *)
      parse_abstract_array_decl_suffix new_decl tokens
    else (* We're done *) new_decl

  (* <abstract-declarator> ::= "*" [ <abstract-declarator> ]
   *                         | <direct-abstract-declarator>
   *)

  let rec parse_abstract_declarator tokens =
    match Tok_stream.peek tokens with
    | T.Star ->
        (* it's a pointer declarator *)
        let _ = Tok_stream.take_token tokens in
        let inner =
          match Tok_stream.peek tokens with
          (* there's an inner declarator *)
          | T.Star | T.OpenParen | T.OpenBracket ->
              parse_abstract_declarator tokens
          (* We've parsed the whole abstract declarator *)
          | _ -> AbstractBase
        in
        AbstractPointer inner
    | _ -> parse_direct_abstract_declarator tokens

  (* <direct-abstract-declarator ::= "(" <abstract-declarator> ")" { "[" <const> "]" }
   *                               | { "[" <const> "]" }+
   *)
  and parse_direct_abstract_declarator tokens =
    if Tok_stream.peek tokens = T.OpenParen then (
      (* "(" <abstract-declarator> ")" { "[" <const> "]" } *)
      let _ = Tok_stream.take_token tokens in
      let decl = parse_abstract_declarator tokens in
      expect T.CloseParen tokens;
      if Tok_stream.peek tokens = T.OpenBracket then
        parse_abstract_array_decl_suffix decl tokens
      else decl)
    else
      (* { "[" <const> "]" }+ *)
      parse_abstract_array_decl_suffix AbstractBase tokens

  (* Convert an abstract declarator + base type to a derived type (analogous to
     Listing 14-6) *)
  let rec process_abstract_declarator decl base_type =
    match decl with
    | AbstractBase -> base_type
    | AbstractPointer inner ->
        let derived_type = Types.Pointer base_type in
        process_abstract_declarator inner derived_type
    | AbstractArray (inner, size) ->
        let derived_type = Types.Array { elem_type = base_type; size } in
        process_abstract_declarator inner derived_type

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

  (* <unop> ::= "-" | "~" | "!" | "*" | "&"
   * but we parse "*" and "&" in parse_unary_exp, not here. *)
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

  (* <type-name> ::= { <type-specifier> }+ [ <abstract-declarator> ] *)
  let parse_type_name tokens =
    let type_specifiers = parse_type_specifier_list tokens in
    let base_type = parse_type type_specifiers in
    (* check for optional abstract declarator *)
    match Tok_stream.peek tokens with
    | T.CloseParen -> base_type
    | _ ->
        let abstract_decl = parse_abstract_declarator tokens in
        process_abstract_declarator abstract_decl base_type

  (* <primary-exp> ::= <const> | <identifier> | "(" <exp> ")" | { <string> }+
   *                 | <identifier> "(" [ <argument-list> ] ")"
   *)
  let rec parse_primary_exp tokens =
    let next_token = Tok_stream.peek tokens in
    match next_token with
    (* constant *)
    | T.ConstInt _ | T.ConstLong _ | T.ConstUInt _ | T.ConstULong _
    | T.ConstDouble _ | T.ConstChar _ ->
        Ast.Constant (parse_const tokens)
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
    (* parenthesized expression *)
    | T.OpenParen ->
        (* Consume open paren *)
        let _ = Tok_stream.take_token tokens in
        let e = parse_exp 0 tokens in
        expect T.CloseParen tokens;
        e
    (* string literal *)
    | T.StringLiteral _ ->
        (* Consume and concatenate strings literals *)
        let rec parse_string_loop () =
          let s = parse_string tokens in
          match Tok_stream.peek tokens with
          (* Recursively process remaining string literals, then concatenate s
             with them *)
          | T.StringLiteral _ -> s ^ parse_string_loop ()
          (* s is last string literal, return as-is *)
          | _ -> s
        in
        Ast.String (parse_string_loop ())
    (* errors *)
    | t -> raise_error ~expected:(Name "a primary expression") ~actual:t

  (* <argument-list> ::= <exp> { "," <exp> } *)
  and parse_argument_list tokens =
    let arg = parse_exp 0 tokens in
    if Tok_stream.peek tokens = T.Comma then
      let _ = Tok_stream.take_token tokens in
      arg :: parse_argument_list tokens
    else [ arg ]

  (* <postfix-exp> ::= <primary-exp> { "[" <exp> "]" } *)
  and parse_postfix_exp tokens =
    let primary = parse_primary_exp tokens in
    (* Use postfix_loop to recursively consume subscript operators *)
    let rec postfix_loop e =
      if Tok_stream.peek tokens = T.OpenBracket then
        let _ = Tok_stream.take_token tokens in
        let subscript = parse_exp 0 tokens in
        let () = expect T.CloseBracket tokens in
        let subscript_exp = Ast.Subscript { ptr = e; index = subscript } in
        (* Consume next subscript operator if there is one *)
        postfix_loop subscript_exp
      else e
    in
    postfix_loop primary

  (* <unary-exp> ::= <unop> <cast-exp>
   *               | "sizeof" <unary-exp>
   *               | "sizeof" "(" <type-name> ")"
   *               | <postfix-exp>
   *)
  and parse_unary_exp tokens =
    match Tok_stream.npeek 3 tokens with
    (* unary expressions *)
    | T.Star :: _ ->
        let _ = Tok_stream.take_token tokens in
        let inner_exp = parse_cast_exp tokens in
        Ast.Dereference inner_exp
    | T.Ampersand :: _ ->
        let _ = Tok_stream.take_token tokens in
        let inner_exp = parse_cast_exp tokens in
        Ast.AddrOf inner_exp
    | (T.Hyphen | T.Tilde | T.Bang) :: _ ->
        let operator = parse_unop tokens in
        let inner_exp = parse_cast_exp tokens in
        Ast.Unary (operator, inner_exp)
    (* "sizeof" "(" <type-name> ")"

       Need to look ahead three tokens to tell whether operand is a type name or
       parenthesized expression *)
    | [ T.KWSizeOf; T.OpenParen; t ] when is_type_specifier t ->
        (* consume sizeof keyword and open paren *)
        let _ = Tok_stream.take_token tokens in
        let _ = Tok_stream.take_token tokens in
        let typ = parse_type_name tokens in
        expect T.CloseParen tokens;
        Ast.SizeOfT typ
    (* "sizeof" <unary-exp> *)
    | T.KWSizeOf :: _ ->
        let _ = Tok_stream.take_token tokens in
        let inner_exp = parse_unary_exp tokens in
        Ast.SizeOf inner_exp
    (* Postfix expression *)
    | _ -> parse_postfix_exp tokens

  (* <cast-exp> ::= "(" <type-name> ")" <cast-exp>
   *              | <unary-exp> *)
  and parse_cast_exp tokens =
    match Tok_stream.npeek 2 tokens with
    (* Cast expression - need to look ahead two tokens to distinguish this from
       parenthesized expressions *)
    | [ T.OpenParen; t ] when is_type_specifier t ->
        let _ = Tok_stream.take_token tokens in
        let target_type = parse_type_name tokens in
        let _ = expect T.CloseParen tokens in
        let inner_exp = parse_cast_exp tokens in
        Ast.Cast { target_type; e = inner_exp }
    | _ -> parse_unary_exp tokens

  (* Helper function to parse the middle of a conditional expression:
   * "?" <exp> ":"
   *)
  and parse_conditional_middle tokens =
    expect QuestionMark tokens;
    let e = parse_exp 0 tokens in
    expect Colon tokens;
    e

  (* <exp> ::= <cast-exp> | <exp> <binop> <exp> | <exp> "?" <exp> ":" <exp>
   * Precedence parsing algorithm (see Listing 6-9) *)
  and parse_exp min_prec tokens =
    let initial_factor = parse_cast_exp tokens in
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

  type declarator =
    | Ident of string
    | PointerDeclarator of declarator
    | ArrayDeclarator of declarator * int
    | FunDeclarator of param_info list * declarator

  and param_info = Param of Types.t * declarator

  (* Helper function to process sequence of array dimension suffixes of the form
     "[" <const> "]" on a declarator; expects at least one array dimension *)
  let rec parse_array_decl_suffix base_decl tokens =
    (* Get next array dimension [const] *)
    let dim = parse_dim tokens in
    let new_decl = ArrayDeclarator (base_decl, dim) in
    if Tok_stream.peek tokens = T.OpenBracket then
      (* There's another dimension *)
      parse_array_decl_suffix new_decl tokens
    else (* We're done *) new_decl

  (* <declarator> ::= "*" <declarator> | <direct-declarator> *)
  let rec parse_declarator tokens =
    match Tok_stream.peek tokens with
    | T.Star ->
        let _ = Tok_stream.take_token tokens in
        let inner = parse_declarator tokens in
        PointerDeclarator inner
    | _ -> parse_direct_declarator tokens

  (* <direct-declarator> ::= <simple-declarator> [ <declarator-suffix> ]
   * <declarator-suffix> ::= <param-list> | { "[" <const> "]" }+
   *)
  and parse_direct_declarator tokens =
    let simple_dec = parse_simple_declarator tokens in
    match Tok_stream.peek tokens with
    (* There's a suffix of form <param-list> *)
    | T.OpenParen ->
        let params = parse_param_list tokens in
        FunDeclarator (params, simple_dec)
    (* There's a suffix of form { "[" <const> "]" }+ *)
    | T.OpenBracket -> parse_array_decl_suffix simple_dec tokens
    (* No declarator suffix *)
    | _ -> simple_dec

  (* <param-list> ::= "(" "void" ")"
   *                | "(" <param> { "," <param> } ")"
   *)
  and parse_param_list tokens =
    if Tok_stream.npeek 3 tokens = [ T.OpenParen; T.KWVoid; T.CloseParen ] then
      (* No params - consume these three tokens and return empty list *)
      let _ = Tok_stream.take_token tokens in
      let _ = Tok_stream.take_token tokens in
      let _ = Tok_stream.take_token tokens in
      []
    else
      let _ = expect T.OpenParen tokens in
      let rec param_loop () =
        let next_param = parse_param tokens in
        if Tok_stream.peek tokens = T.Comma then
          (* there are more params *)
          let _ = Tok_stream.take_token tokens in
          next_param :: param_loop ()
        else [ next_param ]
      in
      let params = param_loop () in
      let _ = expect T.CloseParen tokens in
      params

  (* <param> ::= { <type-specifier> }+ <declarator> *)
  and parse_param tokens =
    let param_t = parse_type (parse_type_specifier_list tokens) in
    let param_decl = parse_declarator tokens in
    Param (param_t, param_decl)

  (* <simple-declarator> ::= <identifier> | "(" <declarator> ")" *)
  and parse_simple_declarator tokens =
    let next_tok = Tok_stream.take_token tokens in
    match next_tok with
    | T.OpenParen ->
        let decl = parse_declarator tokens in
        expect T.CloseParen tokens;
        decl
    | Identifier id -> Ident id
    | other -> raise_error ~expected:(Name "a simple declarator") ~actual:other

  (* Derive type and identifier information from a base type and declarator
     (Listing 14-7 and LIsting 15-5). *)
  let rec process_declarator decl base_type =
    match decl with
    | Ident s -> (s, base_type, [])
    | PointerDeclarator d ->
        let derived_type = Types.Pointer base_type in
        process_declarator d derived_type
    | ArrayDeclarator (inner, size) ->
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

  (* <initializer> ::= <exp>
   *                 | "{" <initializer> { "," <initializer> } [ "," ] "}"
   *)
  let rec parse_initializer tokens =
    if Tok_stream.peek tokens = T.OpenBrace then
      (* Compound initializer *)
      let _ = Tok_stream.take_token tokens in
      let rec parse_init_loop () =
        let next_init = parse_initializer tokens in
        match Tok_stream.npeek 2 tokens with
        (* Trailing comma followed by close brace - we're done *)
        | [ T.Comma; T.CloseBrace ] ->
            (* Consume comma before returning (but not close brace, we get that
               below) *)
            let _ = Tok_stream.take_token tokens in
            [ next_init ]
        (* Comma not followed by close brace means more elements *)
        | T.Comma :: _ ->
            let _ = Tok_stream.take_token tokens in
            next_init :: parse_init_loop ()
        (* Otherwise we're done (next token should be close brace, we'll
           validate that below) *)
        | _ -> [ next_init ]
      in
      let init_list = parse_init_loop () in
      let () = expect T.CloseBrace tokens in
      Ast.CompoundInit init_list
    else Ast.SingleInit (parse_exp 0 tokens)

  (* Single expression*)
  (* <function-declaration> ::= { <specifier> }+ <declarator> ( <block> | ";" )
   * <variable-declaration> ::= { <specifier> }+ <declarator> [ "=" <initializer> ] ";"
   * Use a common function to parse both symbols (Listing 14-8).
   *)
  let rec parse_declaration tokens =
    let specifiers = parse_specifier_list tokens in
    let base_type, storage_class = parse_type_and_storage_class specifiers in
    let decl = parse_declarator tokens in
    let name, typ, params = process_declarator decl base_type in
    match typ with
    (* It's a function declaration *)
    | Types.FunType _ ->
        let body =
          match Tok_stream.peek tokens with
          | T.Semicolon ->
              let _ = Tok_stream.take_token tokens in
              None
          | _ -> Some (parse_block tokens)
        in
        Ast.FunDecl { name; fun_type = typ; storage_class; params; body }
    (* It's a variable *)
    | _ ->
        let init =
          if Tok_stream.peek tokens = T.EqualSign then
            let _ = Tok_stream.take_token tokens in
            Some (parse_initializer tokens)
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

  (* <statement> ::= "return" [ <exp> ] ";"
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
    (* "return" [ <exp> ] ";" *)
    | T.KWReturn ->
        (* consume return keyword *)
        let _ = Tok_stream.take_token tokens in
        let opt_exp = parse_optional_exp T.Semicolon tokens in
        Ast.Return opt_exp
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
