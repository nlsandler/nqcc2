open Ast

type var_entry = {
  unique_name : string;
  from_current_scope : bool;
  has_linkage : bool;
}

let copy_identifier_map m =
  (* return a copy of the map with from_current_block set to false for every entry *)
  String_map.map (fun entry -> { entry with from_current_scope = false }) m

let rec resolve_exp id_map = function
  | Assignment (left, right) ->
      (* validate that lhs is an lvalue *)
      let _ =
        match left with
        | Var _ -> ()
        | _ ->
            failwith
              (Format.asprintf
                 "Expected expression on left-hand side of assignment \
                  statement, found %a"
                 pp_exp left)
      in
      (* recursively process lhs and rhs *)
      Assignment (resolve_exp id_map left, resolve_exp id_map right)
  | Var v -> (
      (* rename var from map  *)
      try Var (String_map.find v id_map).unique_name
      with Not_found -> failwith (Printf.sprintf "Undeclared variable %s" v))
  (* recursively process operands for unary, binary, and conditional *)
  | Unary (op, e) -> Unary (op, resolve_exp id_map e)
  | Binary (op, e1, e2) ->
      Binary (op, resolve_exp id_map e1, resolve_exp id_map e2)
  | Conditional { condition; then_result; else_result } ->
      Conditional
        {
          condition = resolve_exp id_map condition;
          then_result = resolve_exp id_map then_result;
          else_result = resolve_exp id_map else_result;
        }
  | FunCall { f; args } -> (
      try
        let fn_name = (String_map.find f id_map).unique_name in

        FunCall { f = fn_name; args = List.map (resolve_exp id_map) args }
      with Not_found -> failwith "Undeclared function!")
  (* Nothing to do for constant *)
  | Constant _ as c -> c

let resolve_optional_exp id_map = Option.map (resolve_exp id_map)

(* helper for resolving local variables and parameters; deals with validation and updating the variable map*)
let resolve_local_var_helper id_map name =
  match String_map.find_opt name id_map with
  | Some { from_current_scope = true; _ } ->
      (* variable is present in the map and was defined in the current block *)
      failwith "Duplicate variable declaration"
  | _ ->
      (* variable isn't in the map, or was defined in an outer scope;
       * generate a unique name and add it to the map *)
      let unique_name = Unique_ids.make_named_temporary name in
      let new_map =
        String_map.add name
          { unique_name; from_current_scope = true; has_linkage = false }
          id_map
      in
      (new_map, unique_name)

let resolve_local_var_declaration id_map { name; init } =
  let new_map, unique_name = resolve_local_var_helper id_map name in

  let resolved_init = Option.map (resolve_exp new_map) init in
  (* return new map and resolved declaration *)
  (new_map, { name = unique_name; init = resolved_init })

let resolve_for_init id_map = function
  | InitExp e -> (id_map, InitExp (resolve_optional_exp id_map e))
  | InitDecl d ->
      let new_map, resolved_decl = resolve_local_var_declaration id_map d in
      (new_map, InitDecl resolved_decl)

let rec resolve_statement id_map = function
  | Return e -> Return (resolve_exp id_map e)
  | Expression e -> Expression (resolve_exp id_map e)
  | If { condition; then_clause; else_clause } ->
      If
        {
          condition = resolve_exp id_map condition;
          then_clause = resolve_statement id_map then_clause;
          else_clause = Option.map (resolve_statement id_map) else_clause;
        }
  | While { condition; body; id } ->
      While
        {
          condition = resolve_exp id_map condition;
          body = resolve_statement id_map body;
          id;
        }
  | DoWhile { body; condition; id } ->
      DoWhile
        {
          body = resolve_statement id_map body;
          condition = resolve_exp id_map condition;
          id;
        }
  | For { init; condition; post; body; id } ->
      let id_map1 = copy_identifier_map id_map in
      let id_map2, resolved_init = resolve_for_init id_map1 init in
      For
        {
          init = resolved_init;
          condition = resolve_optional_exp id_map2 condition;
          post = resolve_optional_exp id_map2 post;
          body = resolve_statement id_map2 body;
          id;
        }
  | Compound block ->
      let new_variable_map = copy_identifier_map id_map in
      Compound (resolve_block new_variable_map block)
  | (Null | Break _ | Continue _) as s -> s

and resolve_block_item id_map = function
  | S s ->
      (* resolving a statement doesn't change the variable map *)
      let resolved_s = resolve_statement id_map s in
      (id_map, S resolved_s)
  | D d ->
      (* resolving a declaration does change the variable map *)
      let new_map, resolved_d = resolve_local_declaration id_map d in
      (new_map, D resolved_d)

and resolve_block id_map (Block items) =
  let _final_map, resolved_items =
    List.fold_left_map resolve_block_item id_map items
  in
  Block resolved_items

and resolve_local_declaration id_map = function
  | VarDecl vd ->
      let new_map, resolved_vd = resolve_local_var_declaration id_map vd in
      (new_map, VarDecl resolved_vd)
  | FunDecl { body = Some _; _ } ->
      failwith "nested function definitions are not allowed"
  | FunDecl fd ->
      let new_map, resolved_fd = resolve_function_declaration id_map fd in
      (new_map, FunDecl resolved_fd)

and resolve_params id_map = List.fold_left_map resolve_local_var_helper id_map

and resolve_function_declaration id_map fn =
  match String_map.find_opt fn.name id_map with
  | Some { from_current_scope = true; has_linkage = false; _ } ->
      failwith "Duplicate declaration"
  | _ ->
      ();
      let new_entry =
        { unique_name = fn.name; from_current_scope = true; has_linkage = true }
      in
      let new_map = String_map.add fn.name new_entry id_map in
      let inner_map = copy_identifier_map new_map in
      let inner_map1, resolved_params = resolve_params inner_map fn.params in
      let resolved_body = Option.map (resolve_block inner_map1) fn.body in
      (new_map, { fn with params = resolved_params; body = resolved_body })

let resolve (Program fn_decls) =
  let _, resolved_decls =
    List.fold_left_map resolve_function_declaration String_map.empty fn_decls
  in
  Program resolved_decls
