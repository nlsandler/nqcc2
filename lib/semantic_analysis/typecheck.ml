open Batteries
open Ast
open Types

let rec typecheck_exp = function
  | FunCall { f; args } -> (
      let t = (Symbols.get f).t in
      match t with
      | Int -> failwith "Tried to use variable as function name"
      | FunType { param_count } ->
          if List.length args <> param_count then
            failwith "Function called with wrong number of arguments"
          else List.iter typecheck_exp args)
  | Var v -> (
      let t = (Symbols.get v).t in
      match t with
      | Int -> ()
      | FunType _ -> failwith "Tried to use function name as variable ")
  | Unary (_, inner) -> typecheck_exp inner
  | Binary (_, e1, e2) ->
      typecheck_exp e1;
      typecheck_exp e2
  | Assignment (lhs, rhs) ->
      typecheck_exp lhs;
      typecheck_exp rhs
  | CompoundAssignment (_, lhs, rhs) ->
      typecheck_exp lhs;
      typecheck_exp rhs
  | PostfixDecr e -> typecheck_exp e
  | PostfixIncr e -> typecheck_exp e
  | Conditional { condition; then_result; else_result } ->
      typecheck_exp condition;
      typecheck_exp then_result;
      typecheck_exp else_result
  | Constant _ -> ()

let rec typecheck_block (Block b) = List.iter typecheck_block_item b

and typecheck_block_item = function
  | S s -> typecheck_statement s
  | D d -> typecheck_local_decl d

and typecheck_statement = function
  | Return e -> typecheck_exp e
  | Expression e -> typecheck_exp e
  | If { condition; then_clause; else_clause } ->
      typecheck_exp condition;
      typecheck_statement then_clause;
      Option.may typecheck_statement else_clause
  | LabeledStatement (_, s) -> typecheck_statement s
  | Case (_, s, _) -> typecheck_statement s
  | Default (s, _) -> typecheck_statement s
  | Switch s ->
      typecheck_exp s.control;
      typecheck_statement s.body
  | Compound block -> typecheck_block block
  | While { condition; body; _ } ->
      typecheck_exp condition;
      typecheck_statement body
  | DoWhile { body; condition; _ } ->
      typecheck_statement body;
      typecheck_exp condition
  | For { init; condition; post; body; _ } ->
      let typecheck_for_init = function
        | InitDecl { storage_class = Some _; _ } ->
            failwith
              "Storage class not permitted on declaration in for loop header"
        | InitDecl d -> typecheck_local_var_decl d
        | InitExp e -> Option.may typecheck_exp e
      in
      typecheck_for_init init;
      Option.may typecheck_exp condition;
      Option.may typecheck_exp post;
      typecheck_statement body
  | Null | Break _ | Continue _ | Goto _ -> ()

and typecheck_local_decl = function
  | VarDecl vd -> typecheck_local_var_decl vd
  | FunDecl fd -> typecheck_fn_decl fd

and typecheck_local_var_decl { name; init; storage_class } =
  match storage_class with
  | Some Extern -> (
      if Option.is_some init then
        failwith "initializer on local extern declaration"
      else ();
      match Symbols.get_opt name with
      | Some { t; _ } ->
          (* If an external local var is already in the symbol table, don't need to add it *)
          if t <> Int then failwith "Function redelcared as variable" else ()
      | None ->
          Symbols.add_static_var name ~t:Int ~init:NoInitializer ~global:true)
  | Some Static ->
      let ini =
        match init with
        | Some (Constant i) -> Symbols.Initial i
        | None -> Symbols.Initial 0
        | Some _ -> failwith "non-constant initializer on local static variable"
      in
      Symbols.add_static_var name ~t:Int ~init:ini ~global:false
  | None ->
      Symbols.add_automatic_var name ~t:Int;
      Option.may typecheck_exp init

and typecheck_fn_decl { name; params; body; storage_class } =
  let fun_type = Types.FunType { param_count = List.length params } in
  let has_body = Option.is_some body in
  let global = storage_class <> Some Static in
  (* helper function to reconcile current and previous declarations *)
  let check_against_previous Symbols.{ t = prev_t; attrs; _ } =
    if prev_t <> fun_type then
      failwith ("Redeclared function " ^ name ^ " with a different type")
    else
      match attrs with
      | Symbols.FunAttr { global = prev_global; defined = prev_defined; _ } ->
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
    List.iter (fun p -> Symbols.add_automatic_var p ~t:Types.Int) params;
  Option.may typecheck_block body;
  ()

let typecheck_file_scope_var_decl { name; init; storage_class } =
  let current_init =
    match init with
    | Some (Constant c) -> Symbols.Initial c
    | None -> if storage_class = Some Extern then NoInitializer else Tentative
    | Some _ -> failwith "File scope variable has non-constant initializer"
  in
  let current_global = storage_class <> Some Static in
  let old_decl = Symbols.get_opt name in
  let check_against_previous Symbols.{ t; attrs } =
    if t <> Int then failwith "Function redeclared as variable"
    else
      match attrs with
      | StaticAttr { global = prev_global; init = prev_init } ->
          let global =
            if storage_class = Some Extern then prev_global
            else if current_global = prev_global then current_global
            else failwith "Conflicting variable linkage"
          in
          let init =
            match (prev_init, current_init) with
            | Initial _, Initial _ ->
                failwith "Conflicting global variable definition"
            | Initial _, _ -> prev_init
            | Tentative, (Tentative | NoInitializer) -> Tentative
            | _, Initial _ | NoInitializer, _ -> current_init
          in
          (global, init)
      | _ ->
          failwith
            "Internal error, file-scope variable previously declared as local \
             variable or function" [@coverage off]
  in
  let global, init =
    Option.map_default check_against_previous
      (current_global, current_init)
      old_decl
  in
  Symbols.add_static_var name ~t:Int ~global ~init

let typecheck_global_decl = function
  | FunDecl fd -> typecheck_fn_decl fd
  | VarDecl vd -> typecheck_file_scope_var_decl vd

let typecheck (Ast.Program decls) = List.iter typecheck_global_decl decls
