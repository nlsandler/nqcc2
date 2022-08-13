open Ast
open Types

(* tiny helper function to apply typecheck function to AST node option *)
let opt_typecheck typecheck_fn = function
  | Some ast_node -> typecheck_fn ast_node
  | None -> ()

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
  | Conditional { condition; then_result; else_result } ->
      typecheck_exp condition;
      typecheck_exp then_result;
      typecheck_exp else_result
  | Constant _ -> ()

let rec typecheck_block (Block b) = List.iter typecheck_block_item b

and typecheck_block_item = function
  | S s -> typecheck_statement s
  | D d -> typecheck_decl d

and typecheck_statement = function
  | Return e -> typecheck_exp e
  | Expression e -> typecheck_exp e
  | If { condition; then_clause; else_clause } ->
      typecheck_exp condition;
      typecheck_statement then_clause;
      opt_typecheck typecheck_statement else_clause
  | Compound block -> typecheck_block block
  | While { condition; body; _ } ->
      typecheck_exp condition;
      typecheck_statement body
  | DoWhile { body; condition; _ } ->
      typecheck_statement body;
      typecheck_exp condition
  | For { init; condition; post; body; _ } ->
      let typecheck_for_init = function
        | InitDecl d -> typecheck_var_decl d
        | InitExp e -> opt_typecheck typecheck_exp e
      in
      typecheck_for_init init;
      opt_typecheck typecheck_exp condition;
      opt_typecheck typecheck_exp post;
      typecheck_statement body
  | Null | Break _ | Continue _ -> ()

and typecheck_decl = function
  | VarDecl vd -> typecheck_var_decl vd
  | FunDecl fd -> typecheck_fn_decl fd

and typecheck_var_decl { name; init } =
  Symbols.add_var name ~t:Int;
  opt_typecheck typecheck_exp init

and typecheck_fn_decl { name; params; body } =
  let fun_type = Types.FunType { param_count = List.length params } in
  let has_body = Option.is_some body in
  (* helper function to validate that current declaration is compatible with prior in*)
  let check_against_previous Symbols.{ t = prev_t; is_defined; _ } =
    if prev_t <> fun_type then
      failwith ("Redeclared function " ^ name ^ " with a different type")
    else if is_defined && has_body then
      failwith ("Defined body of function " ^ name ^ "twice")
    else ()
  in
  let old_decl = Symbols.get_opt name in
  opt_typecheck check_against_previous old_decl;
  let already_defined =
    match old_decl with Some { is_defined; _ } -> is_defined | None -> false
  in
  Symbols.add_fun name ~t:fun_type ~is_defined:(already_defined || has_body);
  if has_body then List.iter (fun p -> Symbols.add_var p ~t:Types.Int) params;
  opt_typecheck typecheck_block body;
  ()

let typecheck (Ast.Program fn_decls) = List.iter typecheck_fn_decl fn_decls
