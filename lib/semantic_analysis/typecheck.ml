open Batteries
open Type_utils
open Cnums

module U = struct
  include Ast.Untyped
end

module T = struct
  include Ast.Typed
end

let is_lvalue T.{ e; _ } =
  match e with T.Dereference _ | T.Subscript _ | T.Var _ -> true | _ -> false

let convert_to e target_type =
  let cast = T.Cast { target_type; e } in
  set_type cast target_type

let get_common_type t1 t2 =
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
  else failwith "Expressions have incompatible types"

let convert_by_assignment e target_type =
  if e.T.t = target_type then e
  else if is_arithmetic e.t && is_arithmetic target_type then
    convert_to e target_type
  else if is_null_pointer_constant e.e && is_pointer target_type then
    convert_to e target_type
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

let rec typecheck_exp = function
  | U.Var v -> typecheck_var v
  | Constant c -> typecheck_const c
  | Cast { target_type; e = inner } -> typecheck_cast target_type inner
  | Unary (Not, inner) -> typecheck_not inner
  | Unary (Complement, inner) -> typecheck_complement inner
  | Unary (Negate, inner) -> typecheck_negate inner
  | Binary (op, e1, e2) -> (
      match op with
      | And | Or -> typecheck_logical op e1 e2
      | Add -> typecheck_addition e1 e2
      | Subtract -> typecheck_subtraction e1 e2
      | Multiply | Divide | Mod -> typecheck_multiplicative op e1 e2
      | Equal | NotEqual -> typecheck_equality op e1 e2
      | GreaterThan | GreaterOrEqual | LessThan | LessOrEqual ->
          typecheck_comparison op e1 e2)
  | Assignment (lhs, rhs) -> typecheck_assignment lhs rhs
  | Conditional { condition; then_result; else_result } ->
      typecheck_conditional condition then_result else_result
  | FunCall { f; args } -> typecheck_fun_call f args
  | Dereference inner -> typecheck_dereference inner
  | AddrOf inner -> typecheck_addr_of inner
  | Subscript { ptr; index } -> typecheck_subscript ptr index

and typecheck_cast target_type inner =
  match target_type with
  | Array _ -> failwith "Cannot cast to array type"
  | _ -> (
      let typed_inner = typecheck_and_convert inner in
      match (target_type, typed_inner.T.t) with
      | Types.Double, Types.Pointer _ | Pointer _, Double ->
          failwith "Cannot cast between pointer and double"
      | _ ->
          let cast_exp =
            T.Cast { target_type; e = typecheck_and_convert inner }
          in
          set_type cast_exp target_type)

and typecheck_not inner =
  let typed_inner = typecheck_and_convert inner in
  let not_exp = T.Unary (Not, typed_inner) in
  set_type not_exp Int

and typecheck_complement inner =
  let typed_inner = typecheck_and_convert inner in
  if typed_inner.t = Double || is_pointer typed_inner.t then
    failwith "Bitwise complement only valid for integer types"
  else
    let complement_exp = T.Unary (Complement, typed_inner) in
    set_type complement_exp typed_inner.t

and typecheck_negate inner =
  let typed_inner = typecheck_and_convert inner in
  match typed_inner.t with
  | Pointer _ -> failwith "Can't negate a pointer"
  | _ ->
      let negate_exp = T.Unary (Negate, typed_inner) in
      set_type negate_exp typed_inner.t

and typecheck_logical op e1 e2 =
  let typed_e1 = typecheck_and_convert e1 in
  let typed_e2 = typecheck_and_convert e2 in
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
  else if is_pointer typed_e1.t && is_integer typed_e2.t then
    let converted_e2 = convert_to typed_e2 Types.Long in
    let add_exp = T.Binary (Add, typed_e1, converted_e2) in
    set_type add_exp typed_e1.t
  else if is_pointer typed_e2.t && is_integer typed_e1.t then
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
  else if is_pointer typed_e1.t && is_integer typed_e2.t then
    let converted_e2 = convert_to typed_e2 Types.Long in
    let sub_exp = T.Binary (Subtract, typed_e1, converted_e2) in
    set_type sub_exp typed_e1.t
  else if is_pointer typed_e1.t && typed_e1.t = typed_e2.t then
    let sub_exp = T.Binary (Subtract, typed_e1, typed_e2) in
    set_type sub_exp Types.Long
  else failwith "Invalid operands for subtraction"

and typecheck_multiplicative op e1 e2 =
  let typed_e1 = typecheck_and_convert e1 in
  let typed_e2 = typecheck_and_convert e2 in
  if is_pointer typed_e1.t || is_pointer typed_e2.t then
    failwith "multiplicative operations not permitted on pointers"
  else
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

and typecheck_equality op e1 e2 =
  let typed_e1 = typecheck_and_convert e1 in
  let typed_e2 = typecheck_and_convert e2 in
  let common_type =
    if is_pointer typed_e1.t || is_pointer typed_e2.t then
      get_common_pointer_type typed_e1 typed_e2
    else get_common_type typed_e1.t typed_e2.t
  in
  let converted_e1 = convert_to typed_e1 common_type in
  let converted_e2 = convert_to typed_e2 common_type in
  let binary_exp = T.Binary (op, converted_e1, converted_e2) in
  set_type binary_exp Int

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

and typecheck_assignment lhs rhs =
  let typed_lhs = typecheck_and_convert lhs in
  if is_lvalue typed_lhs then
    let lhs_type = get_type typed_lhs in
    let typed_rhs = typecheck_and_convert rhs in
    let converted_rhs = convert_by_assignment typed_rhs lhs_type in
    let assign_exp = T.Assignment (typed_lhs, converted_rhs) in
    set_type assign_exp lhs_type
  else failwith "left hand side of assignment is invalid lvalue"

and typecheck_conditional condition then_exp else_exp =
  let typed_conditon = typecheck_and_convert condition in
  let typed_then = typecheck_and_convert then_exp in
  let typed_else = typecheck_and_convert else_exp in
  let common_type =
    if is_pointer typed_then.t || is_pointer typed_else.t then
      get_common_pointer_type typed_then typed_else
    else get_common_type typed_then.t typed_else.t
  in
  let converted_then = convert_to typed_then common_type in
  let converted_else = convert_to typed_else common_type in
  let conditional_exp =
    T.Conditional
      {
        condition = typed_conditon;
        then_result = converted_then;
        else_result = converted_else;
      }
  in
  set_type conditional_exp common_type

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
  else failwith "Cannot take address of non-value"

and typecheck_subscript e1 e2 =
  let typed_e1 = typecheck_and_convert e1 in
  let typed_e2 = typecheck_and_convert e2 in
  let ptr_type, converted_e1, converted_e2 =
    if is_pointer typed_e1.t && is_integer typed_e2.t then
      (typed_e1.t, typed_e1, convert_to typed_e2 Types.Long)
    else if is_pointer typed_e2.t && is_integer typed_e1.t then
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

and typecheck_and_convert e =
  let typed_e = typecheck_exp e in
  match typed_e.t with
  | Types.Array { elem_type; _ } ->
      let addr_exp = T.AddrOf typed_e in
      set_type addr_exp (Pointer elem_type)
  | _ -> typed_e

let rec static_init_helper var_type init =
  match (var_type, init) with
  | Types.Array _, U.SingleInit _ ->
      failwith "Can't initialize array from scalar value"
  | _, U.SingleInit (U.Constant c) when is_zero_int c ->
      Initializers.zero var_type
  | Types.Pointer _, _ -> failwith "invalid static initializer for pointer"
  | _, U.SingleInit (U.Constant c) ->
      let init_val =
        match Const_convert.const_convert var_type c with
        | Const.ConstInt i -> Initializers.IntInit i
        | Const.ConstLong l -> Initializers.LongInit l
        | Const.ConstUInt ui -> UIntInit ui
        | Const.ConstULong ul -> ULongInit ul
        | Const.ConstDouble d -> DoubleInit d
      in
      [ init_val ]
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
  | Types.Int -> scalar (Const.ConstInt Int32.zero)
  | UInt -> scalar (Const.ConstUInt UInt32.zero)
  | Long -> scalar (Const.ConstLong Int64.zero)
  | ULong | Pointer _ -> scalar (Const.ConstULong UInt64.zero)
  | Double -> scalar (Const.ConstDouble Float.zero)
  | FunType _ ->
      failwith
        "Internal error: can't create zero initializer with function type"
      [@coverage off]

let rec typecheck_init target_type init =
  match (target_type, init) with
  | _, U.SingleInit e ->
      let typechecked_e = typecheck_and_convert e in
      let cast_exp = convert_by_assignment typechecked_e target_type in
      T.SingleInit cast_exp
  | Types.Array { elem_type; size }, CompoundInit inits ->
      if List.length inits > size then
        failwith "too mahy values in initializer "
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
  | Return e ->
      let typed_e = typecheck_and_convert e in
      Return (convert_by_assignment typed_e ret_type)
  | Expression e -> Expression (typecheck_and_convert e)
  | If { condition; then_clause; else_clause } ->
      If
        {
          condition = typecheck_and_convert condition;
          then_clause = typecheck_statement ret_type then_clause;
          else_clause = Option.map (typecheck_statement ret_type) else_clause;
        }
  | Compound block -> Compound (typecheck_block ret_type block)
  | While { condition; body; id } ->
      While
        {
          condition = typecheck_and_convert condition;
          body = typecheck_statement ret_type body;
          id;
        }
  | DoWhile { body; condition; id } ->
      DoWhile
        {
          body = typecheck_statement ret_type body;
          condition = typecheck_and_convert condition;
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
          condition = Option.map typecheck_and_convert condition;
          post = Option.map typecheck_and_convert post;
          body = typecheck_statement ret_type body;
          id;
        }
  | (Null | Break _ | Continue _) as s -> s

and typecheck_local_decl = function
  | VarDecl vd -> VarDecl (typecheck_local_var_decl vd)
  | FunDecl fd -> FunDecl (typecheck_fn_decl fd)

and typecheck_local_var_decl ({ name; init; storage_class; var_type } as vd) =
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
  let adjust_param_type = function
    | Types.Array { elem_type; _ } -> Types.Pointer elem_type
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
  let global = storage_class <> Some Static in
  (* helper function to reconcile current and previous declarations *)
  let check_against_previous Symbols.{ t = prev_t; attrs; _ } =
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
  let default_init =
    if storage_class = Some Extern then Symbols.NoInitializer else Tentative
  in
  let static_init =
    Option.map_default (to_static_init var_type) default_init init
  in
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
            "Internal error, file-scope variable previously declared as local \
             variable or function" [@coverage off]
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

let typecheck (U.Program decls) =
  T.Program (List.map typecheck_global_decl decls)
