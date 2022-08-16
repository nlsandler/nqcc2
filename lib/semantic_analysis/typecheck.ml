open Type_utils

module U = struct
  include Ast.Untyped
end

module T = struct
  include Ast.Typed
end

let convert_to e target_type =
  let cast = T.Cast { target_type; e } in
  set_type cast target_type

let get_common_type t1 t2 =
  if t1 = t2 then t1
  else if t1 = Types.Double || t2 = Double then Double
  else if get_size t1 = get_size t2 then if is_signed t1 then t2 else t1
  else if get_size t1 > get_size t2 then t1
  else t2

let typecheck_var v =
  let v_type = (Symbols.get v).t in
  let e = T.Var v in
  match v_type with
  | FunType _ -> failwith "Tried to use function name as variable "
  | _ -> set_type e v_type

let typecheck_const c =
  let e = T.Constant c in
  set_type e (Const.type_of_const c)

(* tiny helper function to apply typecheck function to AST node option *)
let opt_typecheck typecheck_fn = function
  | Some ast_node -> Some (typecheck_fn ast_node)
  | None -> None

let rec typecheck_exp = function
  | U.Var v -> typecheck_var v
  | Constant c -> typecheck_const c
  | Cast { target_type; e = inner } ->
      let cast_exp = T.Cast { target_type; e = typecheck_exp inner } in
      set_type cast_exp target_type
  | Unary (op, inner) -> typecheck_unary op inner
  | Binary (op, e1, e2) -> typecheck_binary op e1 e2
  | Assignment (lhs, rhs) -> typecheck_assignment lhs rhs
  | Conditional { condition; then_result; else_result } ->
      typecheck_conditional condition then_result else_result
  | FunCall { f; args } -> typecheck_fun_call f args

and typecheck_unary op inner =
  let typed_inner = typecheck_exp inner in
  let unary_exp = T.Unary (op, typed_inner) in
  match op with
  | Not -> set_type unary_exp Int
  | Complement when get_type typed_inner = Double ->
      failwith "Can't apply bitwise complement to double"
  | _ -> set_type unary_exp (get_type typed_inner)

and typecheck_binary op e1 e2 =
  let typed_e1 = typecheck_exp e1 in
  let typed_e2 = typecheck_exp e2 in
  match op with
  | And | Or ->
      let typed_binexp = T.Binary (op, typed_e1, typed_e2) in
      set_type typed_binexp Int
  | _ -> (
      let t1 = get_type typed_e1 in
      let t2 = get_type typed_e2 in
      let common_type = get_common_type t1 t2 in
      let converted_e1 = convert_to typed_e1 common_type in
      let converted_e2 = convert_to typed_e2 common_type in
      let binary_exp = T.Binary (op, converted_e1, converted_e2) in
      match op with
      | Mod when common_type = Double -> failwith "Can't apply % to double"
      | Add | Subtract | Multiply | Divide | Mod ->
          set_type binary_exp common_type
      | _ -> set_type binary_exp Int)

and typecheck_assignment lhs rhs =
  let typed_lhs = typecheck_exp lhs in
  let lhs_type = get_type typed_lhs in
  let typed_rhs = typecheck_exp rhs in
  let converted_rhs = convert_to typed_rhs lhs_type in
  let assign_exp = T.Assignment (typed_lhs, converted_rhs) in
  set_type assign_exp lhs_type

and typecheck_conditional condition then_exp else_exp =
  let typed_conditon = typecheck_exp condition in
  let typed_then = typecheck_exp then_exp in
  let typed_else = typecheck_exp else_exp in
  let common_type =
    get_common_type (get_type typed_then) (get_type typed_else)
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
      let process_arg arg param_t = convert_to (typecheck_exp arg) param_t in
      let converted_args = List.map2 process_arg args param_types in
      let call_exp = T.FunCall { f; args = converted_args } in
      set_type call_exp ret_type
  | _ -> failwith "Tried to use variable as function name"

(* convert a constant to a static initializer, performing type conversion if needed *)
let to_static_init var_type = function
  | U.Constant c ->
      let init_val =
        match Const_convert.const_convert var_type c with
        | ConstInt i -> Initializers.IntInit i
        | ConstLong l -> Initializers.LongInit l
        | ConstUInt u -> Initializers.UIntInit u
        | ConstULong ul -> Initializers.ULongInit ul
        | ConstDouble d -> Initializers.DoubleInit d
      in
      Symbols.Initial init_val
  | _ -> failwith "Non-constant initializer on static variable"

let rec typecheck_block ret_type (U.Block b) =
  T.Block (List.map (typecheck_block_item ret_type) b)

and typecheck_block_item ret_type = function
  | S s -> S (typecheck_statement ret_type s)
  | D d -> D (typecheck_local_decl d)

and typecheck_statement ret_type = function
  | Return e ->
      let typed_e = typecheck_exp e in
      Return (convert_to typed_e ret_type)
  | Expression e -> Expression (typecheck_exp e)
  | If { condition; then_clause; else_clause } ->
      If
        {
          condition = typecheck_exp condition;
          then_clause = typecheck_statement ret_type then_clause;
          else_clause = Option.map (typecheck_statement ret_type) else_clause;
        }
  | Compound block -> Compound (typecheck_block ret_type block)
  | While { condition; body; id } ->
      While
        {
          condition = typecheck_exp condition;
          body = typecheck_statement ret_type body;
          id;
        }
  | DoWhile { body; condition; id } ->
      DoWhile
        {
          body = typecheck_statement ret_type body;
          condition = typecheck_exp condition;
          id;
        }
  | For { init; condition; post; body; id } ->
      let typechecked_for_init =
        match init with
        | InitDecl { storage_class = Some _; _ } ->
            failwith
              "Storage class not permitted on declaration in for loop header"
        | InitDecl d -> T.InitDecl (typecheck_local_var_decl d)
        | InitExp e -> InitExp (opt_typecheck typecheck_exp e)
      in
      For
        {
          init = typechecked_for_init;
          condition = opt_typecheck typecheck_exp condition;
          post = opt_typecheck typecheck_exp post;
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
        match init with
        | Some i -> to_static_init var_type i
        | None -> zero_init
      in

      Symbols.add_static_var name ~t:var_type ~init:static_init ~global:false;
      (* NOTE: we won't actually use init in subsequent passes so we can ddrop it*)
      T.{ name; init = None; storage_class; var_type }
  | None ->
      Symbols.add_automatic_var name ~t:var_type;
      let convert_init e = convert_to (typecheck_exp e) var_type in
      { vd with init = opt_typecheck convert_init init }

and typecheck_fn_decl { name; fun_type; params; body; storage_class } =
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
    match old_decl with
    | Some old_d -> check_against_previous old_d
    | None -> (has_body, global)
  in

  Symbols.add_fun name ~t:fun_type ~defined ~global;
  let param_ts, return_t =
    match fun_type with
    | Types.FunType { param_types; ret_type } -> (param_types, ret_type)
    | _ ->
        failwith "Internal error, function has non-function type"
        [@coverage off]
  in
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
    match init with Some i -> to_static_init var_type i | None -> default_init
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
    match old_decl with
    | Some old_d -> check_against_previous old_d
    | None -> (current_global, static_init)
  in
  Symbols.add_static_var name ~t:var_type ~global ~init;
  (* Okay to drop initializer b/c it's never used after this pass *)
  T.{ name; var_type; init = None; storage_class }

let typecheck_global_decl = function
  | U.FunDecl fd -> T.FunDecl (typecheck_fn_decl fd)
  | VarDecl vd -> VarDecl (typecheck_file_scope_var_decl vd)

let typecheck (U.Program decls) =
  T.Program (List.map typecheck_global_decl decls)
