open Batteries
open Type_utils
open Cnums

module U = struct
  include Ast.Untyped
end

module T = struct
  include Ast.Typed
end

let rec is_lvalue T.{ e; _ } =
  match e with
  | T.Dereference _ | T.Subscript _ | T.Var _ | T.String _ | T.Arrow _ -> true
  | T.Dot { strct; _ } -> is_lvalue strct
  | _ -> false

let rec validate_type = function
  | Types.Array { elem_type; _ } ->
      if is_complete elem_type then validate_type elem_type
      else failwith "Array of incomplete type"
  | Types.Pointer t -> validate_type t
  | FunType { param_types; ret_type } ->
      List.iter validate_type param_types;
      validate_type ret_type
  | Char | SChar | UChar | Int | Long | UInt | ULong | Double | Void
  | Structure _ ->
      ()

let validate_struct_definition U.{ tag; members } =
  (* make sure it's not already in the type table *)
  if Type_table.mem tag then failwith "Structure was already declared"
  else
    (* check for duplicate number names *)
    let member_names = ref Set.empty in
    let validate_member U.{ member_name; member_type } =
      if Set.mem member_name !member_names then
        failwith
          ("Duplicate declaration of member "
          ^ member_name
          ^ " in structure "
          ^ tag)
      else member_names := Set.add member_name !member_names;
      (* validate member type *)
      validate_type member_type;
      match member_type with
      | Types.FunType _ ->
          (* this is redundant, we'd already reject this in parser *)
          failwith "Can't declare structure member with function type"
      | _ ->
          if is_complete member_type then ()
          else failwith "Cannot declare structure member with incomplete type"
    in
    List.iter validate_member members

let typecheck_struct_decl (U.{ tag; members } as sd) =
  if List.is_empty members then (* ignore forward declarations *) ()
  else (
    (* validate the definition, then add it to the type table *)
    validate_struct_definition sd;
    let build_member_def (current_size, current_alignment, current_members)
        U.{ member_name; member_type } =
      let member_alignment = get_alignment member_type in
      let offset =
        Rounding.round_away_from_zero member_alignment current_size
      in
      let member_entry = Type_table.{ member_type; offset } in
      let new_alignment = max current_alignment member_alignment in
      let new_size = offset + get_size member_type in
      let new_members = Map.(current_members <-- (member_name, member_entry)) in
      (new_size, new_alignment, new_members)
    in
    let unpadded_size, alignment, member_defs =
      List.fold_left build_member_def (0, 1, Map.empty) members
    in
    let size = Rounding.round_away_from_zero alignment unpadded_size in
    let struct_def = Type_table.{ alignment; size; members = member_defs } in
    Type_table.add_struct_definition tag struct_def);

  (* actual conversion to new type is trivial  *)
  T.{ tag; members }

let convert_to e target_type =
  let cast = T.Cast { target_type; e } in
  set_type cast target_type

let get_common_type t1 t2 =
  let t1 = if is_character t1 then Types.Int else t1 in
  let t2 = if is_character t2 then Types.Int else t2 in
  if t1 = t2 then t1
  else if t1 = Types.Double || t2 = Double then Double
  else if get_size t1 = get_size t2 then if is_signed t1 then t2 else t1
  else if get_size t1 > get_size t2 then t1
  else t2

let is_zero_int = function
  | Const.ConstInt i when i = Int32.zero -> true
  | ConstLong l when l = Int64.zero -> true
  | ConstUInt u when u = UInt32.zero -> true
  | ConstULong ul when ul = UInt64.zero -> true
  | _ -> false

let is_null_pointer_constant = function
  | T.Constant c -> is_zero_int c
  | _ -> false

let get_common_pointer_type e1 e2 =
  if T.(e1.t = e2.t) then e1.t
  else if is_null_pointer_constant e1.e then e2.t
  else if is_null_pointer_constant e2.e then e1.t
  else if
    (e1.t = Pointer Void && is_pointer e2.t)
    || (e2.t = Pointer Void && is_pointer e1.t)
  then Pointer Void
  else failwith "Expressions have incompatible types"

let convert_by_assignment e target_type =
  if e.T.t = target_type then e
  else if is_arithmetic e.t && is_arithmetic target_type then
    convert_to e target_type
  else if is_null_pointer_constant e.e && is_pointer target_type then
    convert_to e target_type
  else if
    (target_type = Pointer Void && is_pointer e.t)
    || (is_pointer target_type && e.t = Pointer Void)
  then convert_to e target_type
  else failwith "Cannot convert type for asignment"

let typecheck_var v =
  let v_type = (Symbols.get v).t in
  let e = T.Var v in
  match v_type with
  | FunType _ -> failwith "Tried to use function name as variable "
  | _ -> set_type e v_type

let typecheck_const c =
  let e = T.Constant c in
  set_type e (Const.type_of_const c)

let typecheck_string s =
  let e = T.String s in
  let t = Types.Array { elem_type = Char; size = String.length s + 1 } in
  set_type e t

let rec typecheck_exp = function
  | U.Var v -> typecheck_var v
  | Constant c -> typecheck_const c
  | String s -> typecheck_string s
  | Cast { target_type; e = inner } -> typecheck_cast target_type inner
  | Unary (Not, inner) -> typecheck_not inner
  | Unary (Complement, inner) -> typecheck_complement inner
  | Unary (Negate, inner) -> typecheck_negate inner
  | Unary (op, inner) -> typecheck_incr op inner
  | Binary (op, e1, e2) -> (
      match op with
      | And | Or -> typecheck_logical op e1 e2
      | Add -> typecheck_addition e1 e2
      | Subtract -> typecheck_subtraction e1 e2
      | Multiply | Divide | Mod -> typecheck_multiplicative op e1 e2
      | Equal | NotEqual -> typecheck_equality op e1 e2
      | GreaterThan | GreaterOrEqual | LessThan | LessOrEqual ->
          typecheck_comparison op e1 e2
      | BitwiseAnd | BitwiseOr | BitwiseXor -> typecheck_bitwise op e1 e2
      | BitshiftLeft | BitshiftRight -> typecheck_bitshift op e1 e2)
  | Assignment (lhs, rhs) -> typecheck_assignment lhs rhs
  | CompoundAssignment (op, lhs, rhs) ->
      typecheck_compound_assignment op lhs rhs
  | PostfixDecr e -> typecheck_postfix_decr e
  | PostfixIncr e -> typecheck_postfix_incr e
  | Conditional { condition; then_result; else_result } ->
      typecheck_conditional condition then_result else_result
  | FunCall { f; args } -> typecheck_fun_call f args
  | Dereference inner -> typecheck_dereference inner
  | AddrOf inner -> typecheck_addr_of inner
  | Subscript { ptr; index } -> typecheck_subscript ptr index
  | SizeOfT t -> typecheck_size_of_t t
  | SizeOf e -> typecheck_size_of e
  | Dot { strct; member } -> typecheck_dot_operator strct member
  | Arrow { strct; member } -> typecheck_arrow_operator strct member

and typecheck_cast target_type inner =
  validate_type target_type;
  let typed_inner = typecheck_and_convert inner in
  match (target_type, typed_inner.T.t) with
  | Types.Double, Types.Pointer _ | Pointer _, Double ->
      failwith "Cannot cast between pointer and double"
  | Void, _ ->
      let cast_exp = T.Cast { target_type = Void; e = typed_inner } in
      set_type cast_exp Void
  | _ ->
      if not (is_scalar target_type) then
        failwith "Can only cast to scalar types or void"
      else if not (is_scalar typed_inner.T.t) then
        failwith "Can only cast scalar expressions to non-void type"
      else
        let cast_exp = T.Cast { target_type; e = typed_inner } in
        set_type cast_exp target_type

(* convenience function to type check an expression and validate that it's scalar*)
and typecheck_scalar e =
  let typed_e = typecheck_and_convert e in
  if is_scalar typed_e.t then typed_e
  else failwith "A scalar operand is required"

and typecheck_not inner =
  let typed_inner = typecheck_scalar inner in
  let not_exp = T.Unary (Not, typed_inner) in
  set_type not_exp Int

and typecheck_complement inner =
  let typed_inner = typecheck_and_convert inner in
  if not (is_integer typed_inner.t) then
    failwith "Bitwise complement only valid for integer types"
  else
    (* promote character types to int *)
    let typed_inner =
      if is_character typed_inner.t then convert_to typed_inner Int
      else typed_inner
    in
    let complement_exp = T.Unary (Complement, typed_inner) in
    set_type complement_exp typed_inner.t

and typecheck_negate inner =
  let typed_inner = typecheck_and_convert inner in
  if is_arithmetic typed_inner.t then
    (* promote character types to int *)
    let typed_inner =
      if is_character typed_inner.t then convert_to typed_inner Int
      else typed_inner
    in
    let negate_exp = T.Unary (Negate, typed_inner) in
    set_type negate_exp typed_inner.t
  else failwith "Can only negate arithmetic types"

and typecheck_incr op inner =
  let typed_inner = typecheck_and_convert inner in
  if
    is_lvalue typed_inner
    && (is_arithmetic typed_inner.t || is_complete_pointer typed_inner.t)
  then
    let typed_exp = T.Unary (op, typed_inner) in
    set_type typed_exp typed_inner.t
  else
    failwith
      "Operand of ++/-- must be an lvalue with arithmetic or pointer type"

and typecheck_postfix_decr e =
  let typed_e = typecheck_and_convert e in
  if
    is_lvalue typed_e
    && (is_arithmetic typed_e.t || is_complete_pointer typed_e.t)
  then
    (* result has same value as e; no conversions required.
     * (We need to convert integer "1" and to their common type, but that will
     * always be the same type as e, at least w/ types we've added so far *)
    let result_type = get_type typed_e in
    set_type (PostfixDecr typed_e) result_type
  else
    failwith
      "operand of postfix -- must be an lvalue with arithmetic or pointer type"

and typecheck_postfix_incr e =
  let typed_e = typecheck_and_convert e in
  if
    is_lvalue typed_e
    && (is_arithmetic typed_e.t || is_complete_pointer typed_e.t)
  then
    (* Same deal as postfix decrement *)
    let result_type = get_type typed_e in
    set_type (PostfixIncr typed_e) result_type
  else
    failwith
      "operand of postfix ++ must be an lvalue with arithmetic or pointer type"

and typecheck_logical op e1 e2 =
  let typed_e1 = typecheck_scalar e1 in
  let typed_e2 = typecheck_scalar e2 in
  let typed_binexp = T.Binary (op, typed_e1, typed_e2) in
  set_type typed_binexp Int

and typecheck_addition e1 e2 =
  let typed_e1 = typecheck_and_convert e1 in
  let typed_e2 = typecheck_and_convert e2 in
  if is_arithmetic typed_e1.t && is_arithmetic typed_e2.t then
    let common_type = get_common_type typed_e1.t typed_e2.t in
    let converted_e1 = convert_to typed_e1 common_type in
    let converted_e2 = convert_to typed_e2 common_type in
    let add_exp = T.Binary (Add, converted_e1, converted_e2) in
    set_type add_exp common_type
  else if is_complete_pointer typed_e1.t && is_integer typed_e2.t then
    let converted_e2 = convert_to typed_e2 Types.Long in
    let add_exp = T.Binary (Add, typed_e1, converted_e2) in
    set_type add_exp typed_e1.t
  else if is_complete_pointer typed_e2.t && is_integer typed_e1.t then
    let converted_e1 = convert_to typed_e1 Types.Long in
    let add_exp = T.Binary (Add, converted_e1, typed_e2) in
    set_type add_exp typed_e2.t
  else failwith "invalid operands for addition"

and typecheck_subtraction e1 e2 =
  let typed_e1 = typecheck_and_convert e1 in
  let typed_e2 = typecheck_and_convert e2 in
  if is_arithmetic typed_e1.t && is_arithmetic typed_e2.t then
    let common_type = get_common_type typed_e1.t typed_e2.t in
    let converted_e1 = convert_to typed_e1 common_type in
    let converted_e2 = convert_to typed_e2 common_type in
    let sub_exp = T.Binary (Subtract, converted_e1, converted_e2) in
    set_type sub_exp common_type
  else if is_complete_pointer typed_e1.t && is_integer typed_e2.t then
    let converted_e2 = convert_to typed_e2 Types.Long in
    let sub_exp = T.Binary (Subtract, typed_e1, converted_e2) in
    set_type sub_exp typed_e1.t
  else if is_complete_pointer typed_e1.t && typed_e1.t = typed_e2.t then
    let sub_exp = T.Binary (Subtract, typed_e1, typed_e2) in
    set_type sub_exp Types.Long
  else failwith "Invalid operands for subtraction"

and typecheck_multiplicative op e1 e2 =
  let typed_e1 = typecheck_and_convert e1 in
  let typed_e2 = typecheck_and_convert e2 in
  if is_arithmetic typed_e1.t && is_arithmetic typed_e2.t then
    let common_type = get_common_type typed_e1.t typed_e2.t in
    let converted_e1 = convert_to typed_e1 common_type in
    let converted_e2 = convert_to typed_e2 common_type in
    let binary_exp = T.Binary (op, converted_e1, converted_e2) in
    match op with
    | Mod when common_type = Double -> failwith "Can't apply % to double"
    | Multiply | Divide | Mod -> set_type binary_exp common_type
    | _ ->
        failwith
          ("Internal error: "
          ^ T.show_binary_operator op
          ^ "isn't a multiplicative operator") [@coverage off]
  else failwith "Can only multiply arithmetic types"

and typecheck_equality op e1 e2 =
  let typed_e1 = typecheck_and_convert e1 in
  let typed_e2 = typecheck_and_convert e2 in
  let common_type =
    if is_pointer typed_e1.t || is_pointer typed_e2.t then
      get_common_pointer_type typed_e1 typed_e2
    else if is_arithmetic typed_e1.t && is_arithmetic typed_e2.t then
      get_common_type typed_e1.t typed_e2.t
    else failwith "Invalid operands for equality"
  in
  let converted_e1 = convert_to typed_e1 common_type in
  let converted_e2 = convert_to typed_e2 common_type in
  let binary_exp = T.Binary (op, converted_e1, converted_e2) in
  set_type binary_exp Int

and typecheck_bitshift op e1 e2 =
  let typed_e1 = typecheck_and_convert e1 in
  let typed_e2 = typecheck_and_convert e2 in
  if not (is_integer (get_type typed_e1) && is_integer (get_type typed_e2)) then
    failwith "Both operands of bit shift operation must be integers"
  else
    (* promote both operands from character to int type *)
    let typed_e1 =
      if is_character typed_e1.t then convert_to typed_e1 Types.Int
      else typed_e1
    in

    let typed_e2 =
      if is_character typed_e2.t then convert_to typed_e2 Types.Int
      else typed_e2
    in

    (* Don't perform usual arithmetic conversions; result has type of left operand *)
    let typed_binexp = T.Binary (op, typed_e1, typed_e2) in
    set_type typed_binexp (get_type typed_e1)

and typecheck_comparison op e1 e2 =
  let typed_e1 = typecheck_and_convert e1 in
  let typed_e2 = typecheck_and_convert e2 in
  let common_type =
    if is_arithmetic typed_e1.t && is_arithmetic typed_e2.t then
      get_common_type typed_e1.t typed_e2.t
    else if is_pointer typed_e1.t && typed_e1.t = typed_e2.t then typed_e1.t
    else failwith "invalid types for comparions"
  in
  let converted_e1 = convert_to typed_e1 common_type in
  let converted_e2 = convert_to typed_e2 common_type in
  let binary_exp = T.Binary (op, converted_e1, converted_e2) in
  set_type binary_exp Int

and typecheck_bitwise op e1 e2 =
  let typed_e1 = typecheck_and_convert e1 in
  let typed_e2 = typecheck_and_convert e2 in
  if not (is_integer (get_type typed_e1) && is_integer (get_type typed_e2)) then
    failwith "Both operands of bitwise operation must be integers"
  else
    let common_type = get_common_type typed_e1.t typed_e2.t in
    let converted_e1 = convert_to typed_e1 common_type in
    let converted_e2 = convert_to typed_e2 common_type in
    let binary_exp = T.Binary (op, converted_e1, converted_e2) in
    set_type binary_exp common_type

and typecheck_assignment lhs rhs =
  let typed_lhs = typecheck_and_convert lhs in
  if is_lvalue typed_lhs then
    let lhs_type = get_type typed_lhs in
    let typed_rhs = typecheck_and_convert rhs in
    let converted_rhs = convert_by_assignment typed_rhs lhs_type in
    let assign_exp = T.Assignment (typed_lhs, converted_rhs) in
    set_type assign_exp lhs_type
  else failwith "left hand side of assignment is invalid lvalue"

and typecheck_compound_assignment op lhs rhs =
  let typed_lhs = typecheck_and_convert lhs in
  if is_lvalue typed_lhs then
    let lhs_type = get_type typed_lhs in
    let typed_rhs = typecheck_and_convert rhs in
    let rhs_type = get_type typed_rhs in
    let _ =
      match op with
      (* %= and compound bitwise ops only permit integer types *)
      | Mod | BitwiseAnd | BitwiseOr | BitwiseXor | BitshiftLeft | BitshiftRight
        when (not (is_integer lhs_type)) || not (is_integer rhs_type) ->
          failwith
            (Printf.sprintf "Operand %s only supports integer operands"
               (U.show_binary_operator op))
      (* *= and /= only support arithmetic types *)
      | (Multiply | Divide)
        when (not (is_arithmetic lhs_type)) || not (is_arithmetic rhs_type) ->
          failwith
            (Printf.sprintf "Operand %s only supports arithmetic operands"
               (U.show_binary_operator op))
      (* += and -= require either two arithmetic operators, or pointer on LHS and integer on RHS *)
      | (Add | Subtract)
        when not
               ((is_arithmetic rhs_type && is_arithmetic lhs_type)
               || (is_complete_pointer lhs_type && is_integer rhs_type)) ->
          failwith "Invalid types for +=/-="
      | _ -> ()
    in

    let result_t, converted_rhs =
      (* Apply integer type promotions to >>= and <<=, but don't convert to common type *)
      if op = BitshiftLeft || op = BitshiftRight then
        let lhs_type = if is_character lhs_type then Types.Int else lhs_type in
        let converted_rhs =
          if is_character typed_rhs.t then convert_to typed_rhs Int
          else typed_rhs
        in
        (lhs_type, converted_rhs)
        (* For += and -= with pointers, convert RHS to Long and leave LHS type as result type *)
      else if is_pointer lhs_type then (lhs_type, convert_to typed_rhs Long)
        (* Otherwise perform usual arithmetic conversions on both operands *)
      else
        let common_type = get_common_type lhs_type rhs_type in
        (common_type, convert_to typed_rhs common_type)
    in
    (* IMPORTANT: this may involve several implicit casts:
     * - from RHS type to common type (represented w/ explicit convert_to)
     * - from LHS type to common type (NOT directly represented in AST)
     * - from common type back to LHS type on assignment (NOT directly represented in AST)
     * We can't add Cast expressions for the last two because LHS should be evaluated only once,
     * so we don't have two separate places to put Cast expressiosn in this AST node. But we have
     * enough type information to allow us to insert these casts during TACKY generation
     *)
    let compound_assign_exp =
      T.CompoundAssignment
        { op; lhs = typed_lhs; rhs = converted_rhs; result_t }
    in
    set_type compound_assign_exp lhs_type
  else failwith "Left-hand side of compound assignment must be an lvalue "

and typecheck_conditional condition then_exp else_exp =
  let typed_conditon = typecheck_scalar condition in
  let typed_then = typecheck_and_convert then_exp in
  let typed_else = typecheck_and_convert else_exp in
  let result_type =
    if typed_then.t = Void && typed_else.t = Void then Types.Void
    else if is_pointer typed_then.t || is_pointer typed_else.t then
      get_common_pointer_type typed_then typed_else
    else if is_arithmetic typed_then.t && is_arithmetic typed_else.t then
      get_common_type typed_then.t typed_else.t
      (* only other option is structure typess, this is fine if they're identical
       * (typecheck_and_convert already validated that they're complete) *)
    else if typed_then.t = typed_else.t then typed_then.t
    else failwith "Invalid operands for conditional"
  in
  let converted_then = convert_to typed_then result_type in
  let converted_else = convert_to typed_else result_type in
  let conditional_exp =
    T.Conditional
      {
        condition = typed_conditon;
        then_result = converted_then;
        else_result = converted_else;
      }
  in
  set_type conditional_exp result_type

and typecheck_fun_call f args =
  let f_type = (Symbols.get f).t in

  match f_type with
  | FunType { param_types; ret_type } ->
      if List.length param_types <> List.length args then
        failwith "Function called with wrong number of arguments"
      else ();
      let process_arg arg param_t =
        convert_by_assignment (typecheck_and_convert arg) param_t
      in
      let converted_args = List.map2 process_arg args param_types in
      let call_exp = T.FunCall { f; args = converted_args } in
      set_type call_exp ret_type
  | _ -> failwith "Tried to use variable as function name"

and typecheck_dereference inner =
  let typed_inner = typecheck_and_convert inner in
  match get_type typed_inner with
  | Pointer Void -> failwith "Can't dereference pointer to void"
  | Pointer referenced_t ->
      let deref_exp = T.Dereference typed_inner in
      set_type deref_exp referenced_t
  | _ -> failwith "Tried to dereference non-pointer"

and typecheck_addr_of inner =
  let typed_inner = typecheck_exp inner in
  if is_lvalue typed_inner then
    let inner_t = get_type typed_inner in
    let addr_exp = T.AddrOf typed_inner in
    set_type addr_exp (Pointer inner_t)
  else failwith "Cannot take address of non-lvalue"

and typecheck_subscript e1 e2 =
  let typed_e1 = typecheck_and_convert e1 in
  let typed_e2 = typecheck_and_convert e2 in
  let ptr_type, converted_e1, converted_e2 =
    if is_complete_pointer typed_e1.t && is_integer typed_e2.t then
      (typed_e1.t, typed_e1, convert_to typed_e2 Types.Long)
    else if is_complete_pointer typed_e2.t && is_integer typed_e1.t then
      (typed_e2.t, convert_to typed_e1 Long, typed_e2)
    else failwith "Invalid types for subscript operation"
  in
  let result_type =
    match ptr_type with
    | Pointer referenced -> referenced
    | _ -> failwith "Internal error typechecking subscript" [@coverage off]
  in
  let subscript_exp =
    T.Subscript { ptr = converted_e1; index = converted_e2 }
  in
  set_type subscript_exp result_type

and typecheck_size_of_t t =
  validate_type t;
  if is_complete t then
    let sizeof_exp = T.SizeOfT t in
    set_type sizeof_exp ULong
  else failwith "Can't apply sizeof to incomplete type"

and typecheck_size_of inner =
  let typed_inner = typecheck_exp inner in
  if is_complete typed_inner.t then
    let sizeof_exp = T.SizeOf typed_inner in
    set_type sizeof_exp ULong
  else failwith "Can't apply sizeof to incomplete type"

and typecheck_and_convert e =
  let typed_e = typecheck_exp e in
  match typed_e.t with
  | Types.Structure _ when not (is_complete typed_e.t) ->
      failwith "Incomplete structure type not permitted here"
  | Types.Array { elem_type; _ } ->
      let addr_exp = T.AddrOf typed_e in
      set_type addr_exp (Pointer elem_type)
  | _ -> typed_e

and typecheck_dot_operator strct member =
  let typed_strct = typecheck_and_convert strct in
  match typed_strct.t with
  | Types.Structure tag ->
      (* typecheck_and_convert already validated that this structure type is complete *)
      let struct_def = Type_table.find tag in
      let member_typ =
        try Map.(struct_def.members --> member).member_type
        with Not_found ->
          failwith ("Struct type " ^ tag ^ " has no member " ^ member)
      in
      let dot_exp = T.Dot { strct = typed_strct; member } in
      set_type dot_exp member_typ
  | _ ->
      failwith
        "Dot operator can only be applied to expressions with structure type"

and typecheck_arrow_operator strct_ptr member =
  let typed_strct_ptr = typecheck_and_convert strct_ptr in
  match typed_strct_ptr.t with
  | Types.Pointer (Structure tag) ->
      let struct_def = Type_table.find tag in
      let member_typ =
        try Map.(struct_def.members --> member).member_type
        with Not_found ->
          failwith
            ("Struct type " ^ tag ^ " is incomplete or has no member " ^ member)
      in
      let arrow_exp = T.Arrow { strct = typed_strct_ptr; member } in
      set_type arrow_exp member_typ
  | _ -> failwith "Arrow operator can only be applied to pointers to structure"

let rec static_init_helper var_type init =
  match (var_type, init) with
  | Types.Array { elem_type; size }, U.SingleInit (U.String s) ->
      if is_character elem_type then
        match size - String.length s with
        | 0 -> [ Initializers.StringInit (s, false) ]
        | 1 -> [ Initializers.StringInit (s, true) ]
        | n when n > 0 ->
            [ Initializers.StringInit (s, true); ZeroInit (n - 1) ]
        | _ -> failwith "string is too long for initializer"
      else
        failwith
          "Can't initialize array of non-character type with string literal"
  | Types.Array _, U.SingleInit _ ->
      failwith "Can't initialize array from scalar value"
  | Types.Pointer Char, U.SingleInit (U.String s) ->
      let str_id = Symbols.add_string s in
      [ PointerInit str_id ]
  | _, U.SingleInit (U.String _) ->
      failwith "String literal can only initialize char *"
  | Structure tag, U.CompoundInit inits ->
      let struct_def = Type_table.find tag in
      let members = Type_table.get_members tag in
      if List.length inits > List.length members then
        failwith "Too many elements in struct initializer"
      else
        let handle_member (current_offset, current_inits) memb init =
          let padding =
            if current_offset < memb.Type_table.offset then
              [ Initializers.ZeroInit (memb.offset - current_offset) ]
            else []
          in
          let more_static_inits = static_init_helper memb.member_type init in
          let new_inits = current_inits @ padding @ more_static_inits in
          let new_offset = memb.offset + get_size memb.member_type in
          (new_offset, new_inits)
        in
        let initialized_members = List.take (List.length inits) members in
        let initialized_size, explicit_initializers =
          List.fold_left2 handle_member (0, []) initialized_members inits
        in
        let trailing_padding =
          if initialized_size < struct_def.size then
            [ Initializers.ZeroInit (struct_def.size - initialized_size) ]
          else []
        in
        explicit_initializers @ trailing_padding
  | Structure _, SingleInit _ ->
      failwith " Can't initialize static structure with scalar value"
  | _, U.SingleInit (U.Constant c) when is_zero_int c ->
      Initializers.zero var_type
  | Types.Pointer _, _ -> failwith "invalid static initializer for pointer"
  | _, U.SingleInit (U.Constant c) ->
      if is_arithmetic var_type then
        let init_val =
          match Const_convert.const_convert var_type c with
          | Const.ConstChar c -> Initializers.CharInit c
          | Const.ConstInt i -> Initializers.IntInit i
          | Const.ConstLong l -> Initializers.LongInit l
          | Const.ConstUChar uc -> Initializers.UCharInit uc
          | Const.ConstUInt ui -> UIntInit ui
          | Const.ConstULong ul -> ULongInit ul
          | Const.ConstDouble d -> DoubleInit d
        in
        [ init_val ]
      else
        (* we already dealt with pointers (can only initialize w/ null constant or string literal)
         * and already rejected any declarations with type void and any arrays or structs
         * initialized with scalar expressions *)
        failwith
          ("Internal error: should have already rejected initializer with type "
          ^ Types.show var_type) [@coverage off]
  | _, U.SingleInit _ -> failwith "non-constant initializer"
  | Array { elem_type; size }, U.CompoundInit inits ->
      let static_inits = List.concat_map (static_init_helper elem_type) inits in
      let padding =
        match size - List.length inits with
        | 0 -> []
        | n when n > 0 ->
            let zero_bytes = get_size elem_type * n in
            [ Initializers.ZeroInit zero_bytes ]
        | _ -> failwith "Too many values in static initializer"
      in
      static_inits @ padding
  | _, U.CompoundInit _ ->
      failwith "Can't use compound initializer for object with scalar type"

let to_static_init var_type init =
  let init_list = static_init_helper var_type init in
  Symbols.Initial init_list

let rec make_zero_init t =
  let scalar c = T.SingleInit { e = Constant c; t } in
  match t with
  | Types.Array { elem_type; size } as t ->
      T.CompoundInit (t, List.make size (make_zero_init elem_type))
  | Structure tag ->
      let members = Type_table.get_members tag in
      T.CompoundInit
        (t, List.map (fun m -> make_zero_init m.Type_table.member_type) members)
  | Char | SChar -> scalar (Const.ConstChar Int8.zero)
  | Int -> scalar (Const.ConstInt Int32.zero)
  | UChar -> scalar (Const.ConstUChar UInt8.zero)
  | UInt -> scalar (Const.ConstUInt UInt32.zero)
  | Long -> scalar (Const.ConstLong Int64.zero)
  | ULong | Pointer _ -> scalar (Const.ConstULong UInt64.zero)
  | Double -> scalar (Const.ConstDouble Float.zero)
  | (FunType _ | Void) as t ->
      failwith
        ("Internal error: can't create zero initializer with type"
        ^ Types.show t) [@coverage off]

let rec typecheck_init target_type init =
  match (target_type, init) with
  | Types.Array { elem_type; size }, U.SingleInit (String s) ->
      if not (is_character elem_type) then
        failwith "Can't initialize non-character type with string literal"
      else if String.length s > size then
        failwith "Too many characters in string literal"
      else T.SingleInit (set_type (T.String s) target_type)
  | Types.Structure tag, CompoundInit init_list ->
      let members = Type_table.get_members tag in
      if List.length init_list > List.length members then
        failwith "Too many elements in structure initializer"
      else
        let initialized_members, uninitialized_members =
          List.takedrop (List.length init_list) members
        in
        let typechecked_members =
          List.map2
            (fun memb init -> typecheck_init memb.Type_table.member_type init)
            initialized_members init_list
        in
        let padding =
          List.map
            (fun m -> make_zero_init m.Type_table.member_type)
            uninitialized_members
        in

        T.CompoundInit (target_type, typechecked_members @ padding)
  | _, U.SingleInit e ->
      let typechecked_e = typecheck_and_convert e in
      let cast_exp = convert_by_assignment typechecked_e target_type in
      T.SingleInit cast_exp
  | Array { elem_type; size }, CompoundInit inits ->
      if List.length inits > size then
        failwith "too many values in initializer "
      else
        let typechecked_inits = List.map (typecheck_init elem_type) inits in
        let padding =
          List.make (size - List.length inits) (make_zero_init elem_type)
        in
        T.CompoundInit (target_type, typechecked_inits @ padding)
  | _ -> failwith "Can't initializer scalar value from compound initializer"

let rec typecheck_block ret_type (U.Block b) =
  T.Block (List.map (typecheck_block_item ret_type) b)

and typecheck_block_item ret_type = function
  | S s -> S (typecheck_statement ret_type s)
  | D d -> D (typecheck_local_decl d)

and typecheck_statement ret_type = function
  | Return (Some e) ->
      if ret_type = Types.Void then
        failwith "function with void return type cannot return a value"
      else
        let typed_e =
          convert_by_assignment (typecheck_and_convert e) ret_type
        in
        Return (Some typed_e)
  | Return None ->
      if ret_type = Void then Return None
      else failwith "Function with non-void return type must return a value"
  | Expression e -> Expression (typecheck_and_convert e)
  | If { condition; then_clause; else_clause } ->
      If
        {
          condition = typecheck_scalar condition;
          then_clause = typecheck_statement ret_type then_clause;
          else_clause = Option.map (typecheck_statement ret_type) else_clause;
        }
  | LabeledStatement (l, s) ->
      LabeledStatement (l, typecheck_statement ret_type s)
  | Case (e, s, id) ->
      let typed_e = typecheck_and_convert e in
      if get_type typed_e = Double then
        failwith "Case expression cannot be double"
      else
        (* NOTE: e must be converted to type of controlling expression in enclosing switch; we'll do that during collect_switch_cases pass *)
        Case (typecheck_and_convert e, typecheck_statement ret_type s, id)
  | Default (s, id) -> Default (typecheck_statement ret_type s, id)
  | Switch s ->
      let typed_control = typecheck_and_convert s.control in
      if not (is_integer (get_type typed_control)) then
        failwith "Controlling expression in switch must have integer type"
      else
        (* Perform integer promotions on controlling expression *)
        let typed_control =
          if is_character typed_control.t then convert_to typed_control Int
          else typed_control
        in

        Switch
          {
            control = typed_control;
            body = typecheck_statement ret_type s.body;
            id = s.id;
            cases = s.cases;
          }
  | Compound block -> Compound (typecheck_block ret_type block)
  | While { condition; body; id } ->
      While
        {
          condition = typecheck_scalar condition;
          body = typecheck_statement ret_type body;
          id;
        }
  | DoWhile { body; condition; id } ->
      DoWhile
        {
          body = typecheck_statement ret_type body;
          condition = typecheck_scalar condition;
          id;
        }
  | For { init; condition; post; body; id } ->
      let typechecked_for_init =
        match init with
        | InitDecl { storage_class = Some _; _ } ->
            failwith
              "Storage class not permitted on declaration in for loop header"
        | InitDecl d -> T.InitDecl (typecheck_local_var_decl d)
        | InitExp e -> InitExp (Option.map typecheck_and_convert e)
      in
      For
        {
          init = typechecked_for_init;
          condition = Option.map typecheck_scalar condition;
          post = Option.map typecheck_and_convert post;
          body = typecheck_statement ret_type body;
          id;
        }
  | (Null | Break _ | Continue _ | Goto _) as s -> s

and typecheck_local_decl = function
  | VarDecl vd -> VarDecl (typecheck_local_var_decl vd)
  | FunDecl fd -> FunDecl (typecheck_fn_decl fd)
  | StructDecl sd -> StructDecl (typecheck_struct_decl sd)

and typecheck_local_var_decl ({ name; init; storage_class; var_type } as vd) =
  if var_type = Void then failwith "No void declarations"
  else validate_type var_type;
  match storage_class with
  | Some Extern ->
      if Option.is_some init then
        failwith "initializer on local extern declaration"
      else ();
      (match Symbols.get_opt name with
      | Some { t; _ } ->
          (* If an external local var is already in the symbol table, don't need to add it *)
          if t <> var_type then
            failwith "Variable redeclared with different type"
          else ()
      | None ->
          Symbols.add_static_var name ~t:var_type ~init:NoInitializer
            ~global:true);
      T.{ name; init = None; storage_class; var_type }
  | _ when not (is_complete var_type) ->
      (* can't define a variable with an incomplete type *)
      failwith "Cannot define a variable with an incomplete type"
  | Some Static ->
      let zero_init = Symbols.Initial (Initializers.zero var_type) in
      let static_init =
        Option.map_default (to_static_init var_type) zero_init init
      in

      Symbols.add_static_var name ~t:var_type ~init:static_init ~global:false;
      (* NOTE: we won't actually use init in subsequent passes so we can drop it*)
      T.{ name; init = None; storage_class; var_type }
  | None ->
      Symbols.add_automatic_var name ~t:var_type;
      { vd with init = Option.map (typecheck_init var_type) init }

and typecheck_fn_decl { name; fun_type; params; body; storage_class } =
  validate_type fun_type;
  (* Note: we do this _before_ adjusting param types *)
  let adjust_param_type = function
    | Types.Array { elem_type; _ } -> Types.Pointer elem_type
    | Void -> failwith "No void params allowed"
    | t -> t
  in
  let param_ts, return_t, fun_type =
    match fun_type with
    | Types.FunType { ret_type = Array _; _ } ->
        failwith "A function cannot return an array"
    | Types.FunType { param_types; ret_type } ->
        let param_types = List.map adjust_param_type param_types in
        (param_types, ret_type, Types.FunType { param_types; ret_type })
    | _ ->
        failwith "Internal error, function has non-function type"
        [@coverage off]
  in
  let has_body = Option.is_some body in
  (* can't define a function with incomplete return or param type *)
  if
    has_body
    && not
         ((return_t = Void || is_complete return_t)
         && List.for_all is_complete param_ts)
  then
    failwith
      "Can't define a function with incomplete return type or parameter type"
  else
    let global = storage_class <> Some Static in
    (* helper function to reconcile current and previous declarations *)
    let check_against_previous Symbols.{ t = prev_t; attrs } =
      if prev_t <> fun_type then
        failwith ("Redeclared function " ^ name ^ " with a different type")
      else
        match attrs with
        | Symbols.FunAttr { global = prev_global; defined = prev_defined } ->
            if prev_defined && has_body then
              failwith ("Defined body of function " ^ name ^ "twice")
            else if prev_global && storage_class = Some Static then
              failwith "Static function declaration follows non-static"
            else
              let defined = has_body || prev_defined in
              (defined, prev_global)
        | _ ->
            failwith
              "Internal error: symbol has function type but not function \
               attributes" [@coverage off]
    in
    let old_decl = Symbols.get_opt name in
    let defined, global =
      Option.map_default check_against_previous (has_body, global) old_decl
    in

    Symbols.add_fun name ~t:fun_type ~defined ~global;

    if has_body then
      List.iter2 (fun p t -> Symbols.add_automatic_var p ~t) params param_ts
    else ();
    let body = Option.map (typecheck_block return_t) body in
    T.{ name; fun_type; params; body; storage_class }

let typecheck_file_scope_var_decl U.{ name; var_type; init; storage_class } =
  if var_type = Void then failwith "void variables not allowed"
  else validate_type var_type;
  let default_init =
    if storage_class = Some Extern then Symbols.NoInitializer else Tentative
  in
  let static_init =
    Option.map_default (to_static_init var_type) default_init init
  in
  if not (is_complete var_type || static_init = NoInitializer) then
    (* note: some compilers permit tentative definition with incomplete type, if it's completed later in the file. we don't. *)
    failwith "Can't define a variable with an incomplete type "
  else
    let current_global = storage_class <> Some Static in
    let old_decl = Symbols.get_opt name in
    let check_against_previous Symbols.{ t; attrs } =
      if t <> var_type then failwith "Variable redeclared with different type"
      else
        match attrs with
        | StaticAttr { global = prev_global; init = prev_init } ->
            let global =
              if storage_class = Some Extern then prev_global
              else if current_global = prev_global then current_global
              else failwith "Conflicting variable linkage"
            in
            let init =
              match (prev_init, static_init) with
              | Initial _, Initial _ ->
                  failwith "Conflicting global variable definition"
              | Initial _, _ -> prev_init
              | Tentative, (Tentative | NoInitializer) -> Tentative
              | _, Initial _ | NoInitializer, _ -> static_init
            in
            (global, init)
        | _ ->
            failwith
              "Internal error, file-scope variable previously declared as \
               local variable or function" [@coverage off]
    in
    let global, init =
      Option.map_default check_against_previous
        (current_global, static_init)
        old_decl
    in
    Symbols.add_static_var name ~t:var_type ~global ~init;
    (* Okay to drop initializer b/c it's never used after this pass *)
    T.{ name; var_type; init = None; storage_class }

let typecheck_global_decl = function
  | U.FunDecl fd -> T.FunDecl (typecheck_fn_decl fd)
  | VarDecl vd -> VarDecl (typecheck_file_scope_var_decl vd)
  | StructDecl sd -> StructDecl (typecheck_struct_decl sd)

let typecheck (U.Program decls) =
  T.Program (List.map typecheck_global_decl decls)
