open Ast.Untyped
open Types

type var_entry = {
  unique_name : string;
  from_current_scope : bool;
  has_linkage : bool;
}

type struct_entry = { unique_tag : string; struct_from_current_scope : bool }

let copy_identifier_map (m : var_entry String_map.t) =
  (* return a copy of the map with from_current_block set to false for every entry *)
  String_map.map (fun entry -> { entry with from_current_scope = false }) m

let copy_struct_map m =
  (* return a copy of the map with from_current_block set to false for every entry *)
  String_map.map
    (fun entry -> { entry with struct_from_current_scope = false })
    m

(* replace structure tags in type specifiers *)
let rec resolve_type struct_map = function
  | Structure tag -> (
      try
        let unique_tag = (String_map.find tag struct_map).unique_tag in
        Structure unique_tag
      with Not_found -> failwith "specified undeclared structure type")
  | Pointer referenced_t -> Pointer (resolve_type struct_map referenced_t)
  | Array { elem_type; size } ->
      let resolved_elem_type = resolve_type struct_map elem_type in
      Array { elem_type = resolved_elem_type; size }
  | FunType { param_types; ret_type } ->
      let resolved_param_types =
        List.map (resolve_type struct_map) param_types
      in
      let resolved_ret_type = resolve_type struct_map ret_type in
      FunType
        { param_types = resolved_param_types; ret_type = resolved_ret_type }
  | t -> t

let rec resolve_exp struct_map id_map = function
  | Assignment (left, right) ->
      (* recursively process lhs and rhs *)
      Assignment
        (resolve_exp struct_map id_map left, resolve_exp struct_map id_map right)
  | Var v -> (
      (* rename var from map  *)
      try Var (String_map.find v id_map).unique_name
      with Not_found -> failwith (Printf.sprintf "Undeclared variable %s" v))
  (* recursively process operands of other expressions *)
  | Cast { target_type; e } ->
      let resolved_type = resolve_type struct_map target_type in
      Cast { target_type = resolved_type; e = resolve_exp struct_map id_map e }
  | Unary (op, e) -> Unary (op, resolve_exp struct_map id_map e)
  | Binary (op, e1, e2) ->
      Binary
        (op, resolve_exp struct_map id_map e1, resolve_exp struct_map id_map e2)
  | Conditional { condition; then_result; else_result } ->
      Conditional
        {
          condition = resolve_exp struct_map id_map condition;
          then_result = resolve_exp struct_map id_map then_result;
          else_result = resolve_exp struct_map id_map else_result;
        }
  | FunCall { f; args } -> (
      try
        let fn_name = (String_map.find f id_map).unique_name in

        FunCall
          { f = fn_name; args = List.map (resolve_exp struct_map id_map) args }
      with Not_found -> failwith "Undeclared function!")
  | Dereference inner -> Dereference (resolve_exp struct_map id_map inner)
  | AddrOf inner -> AddrOf (resolve_exp struct_map id_map inner)
  | Subscript { ptr; index } ->
      Subscript
        {
          ptr = resolve_exp struct_map id_map ptr;
          index = resolve_exp struct_map id_map index;
        }
  | SizeOf e -> SizeOf (resolve_exp struct_map id_map e)
  | SizeOfT t -> SizeOfT (resolve_type struct_map t)
  | Dot { strct; member } ->
      Dot { strct = resolve_exp struct_map id_map strct; member }
  | Arrow { strct; member } ->
      Arrow { strct = resolve_exp struct_map id_map strct; member }
  (* Nothing to do for expressions without subexpressions *)
  | (Constant _ | String _) as c -> c

let resolve_optional_exp struct_map id_map =
  Option.map (resolve_exp struct_map id_map)

(* helper for resolving local variables and parameters; deals with validation and updating the variable map*)
let resolve_local_var_helper id_map name storage_class =
  (match String_map.find_opt name id_map with
  | Some { from_current_scope = true; has_linkage; _ } ->
      if not (has_linkage && storage_class = Some Extern) then
        (* variable is present in the map and was defined in the current block *)
        failwith "Duplicate variable declaration"
      else ()
  | _ -> ());
  let entry =
    if storage_class = Some Extern then
      { unique_name = name; from_current_scope = true; has_linkage = true }
    else
      (* variable isn't in the map, or was defined in an outer scope;
       * generate a unique name and add it to the map *)
      let unique_name = Unique_ids.make_named_temporary name in
      { unique_name; from_current_scope = true; has_linkage = false }
  in
  let new_map = String_map.add name entry id_map in
  (new_map, entry.unique_name)

let rec resolve_initializer struct_map id_map = function
  | SingleInit e -> SingleInit (resolve_exp struct_map id_map e)
  | CompoundInit inits ->
      CompoundInit (List.map (resolve_initializer struct_map id_map) inits)

let resolve_local_var_declaration struct_map id_map
    { name; var_type; init; storage_class } =
  let new_id_map, unique_name =
    resolve_local_var_helper id_map name storage_class
  in
  let resolved_type = resolve_type struct_map var_type in
  let resolved_init =
    Option.map (resolve_initializer struct_map new_id_map) init
  in
  (* return new map and resolved declaration *)
  ( new_id_map,
    {
      name = unique_name;
      var_type = resolved_type;
      init = resolved_init;
      storage_class;
    } )

let resolve_for_init struct_map id_map = function
  | InitExp e -> (id_map, InitExp (resolve_optional_exp struct_map id_map e))
  | InitDecl d ->
      let new_id_map, resolved_decl =
        resolve_local_var_declaration struct_map id_map d
      in
      (new_id_map, InitDecl resolved_decl)

let rec resolve_statement struct_map id_map = function
  | Return e ->
      let resolved_e = Option.map (resolve_exp struct_map id_map) e in
      Return resolved_e
  | Expression e -> Expression (resolve_exp struct_map id_map e)
  | If { condition; then_clause; else_clause } ->
      If
        {
          condition = resolve_exp struct_map id_map condition;
          then_clause = resolve_statement struct_map id_map then_clause;
          else_clause =
            Option.map (resolve_statement struct_map id_map) else_clause;
        }
  | While { condition; body; id } ->
      While
        {
          condition = resolve_exp struct_map id_map condition;
          body = resolve_statement struct_map id_map body;
          id;
        }
  | DoWhile { body; condition; id } ->
      DoWhile
        {
          body = resolve_statement struct_map id_map body;
          condition = resolve_exp struct_map id_map condition;
          id;
        }
  | For { init; condition; post; body; id } ->
      let id_map1 = copy_identifier_map id_map in
      let struct_map1 = copy_struct_map struct_map in
      let id_map2, resolved_init = resolve_for_init struct_map1 id_map1 init in
      For
        {
          init = resolved_init;
          condition = resolve_optional_exp struct_map1 id_map2 condition;
          post = resolve_optional_exp struct_map1 id_map2 post;
          body = resolve_statement struct_map1 id_map2 body;
          id;
        }
  | Compound block ->
      let new_variable_map = copy_identifier_map id_map in
      let new_struct_map = copy_struct_map struct_map in
      Compound (resolve_block new_struct_map new_variable_map block)
  | (Null | Break _ | Continue _) as s -> s

and resolve_block_item (struct_map, id_map) = function
  | S s ->
      (* resolving a statement doesn't change the struct or variable map *)
      let resolved_s = resolve_statement struct_map id_map s in
      ((struct_map, id_map), S resolved_s)
  | D d ->
      (* resolving a declaration can change the structure or variable map *)
      let new_maps, resolved_d =
        resolve_local_declaration struct_map id_map d
      in
      (new_maps, D resolved_d)

and resolve_block struct_map id_map (Block items) =
  let _final_maps, resolved_items =
    List.fold_left_map resolve_block_item (struct_map, id_map) items
  in
  Block resolved_items

and resolve_local_declaration struct_map id_map = function
  | VarDecl vd ->
      let new_id_map, resolved_vd =
        resolve_local_var_declaration struct_map id_map vd
      in
      ((struct_map, new_id_map), VarDecl resolved_vd)
  | FunDecl { body = Some _; _ } ->
      failwith "nested function definitions are not allowed"
  | FunDecl { storage_class = Some Static; _ } ->
      failwith "static keyword not allowed on local function declarations"
  | FunDecl fd ->
      let new_id_map, resolved_fd =
        resolve_function_declaration struct_map id_map fd
      in
      ((struct_map, new_id_map), FunDecl resolved_fd)
  | StructDecl sd ->
      let new_struct_map, resolved_sd =
        resolve_structure_declaration struct_map sd
      in
      ((new_struct_map, id_map), StructDecl resolved_sd)

and resolve_params id_map =
  let fold_param new_map param_name =
    resolve_local_var_helper new_map param_name None
  in
  List.fold_left_map fold_param id_map

and resolve_function_declaration struct_map id_map fn =
  match String_map.find_opt fn.name id_map with
  | Some { from_current_scope = true; has_linkage = false; _ } ->
      failwith "Duplicate declaration"
  | _ ->
      let resolved_type = resolve_type struct_map fn.fun_type in
      let new_entry =
        { unique_name = fn.name; from_current_scope = true; has_linkage = true }
      in
      let new_id_map = String_map.add fn.name new_entry id_map in
      let inner_id_map = copy_identifier_map new_id_map in
      let inner_id_map1, resolved_params =
        resolve_params inner_id_map fn.params
      in
      let inner_struct_map = copy_struct_map struct_map in
      let resolved_body =
        Option.map (resolve_block inner_struct_map inner_id_map1) fn.body
      in
      ( new_id_map,
        {
          fn with
          fun_type = resolved_type;
          params = resolved_params;
          body = resolved_body;
        } )

and resolve_structure_declaration struct_map { tag; members } =
  let prev_entry = String_map.find_opt tag struct_map in
  let new_map, resolved_tag =
    match prev_entry with
    | Some { unique_tag; struct_from_current_scope = true } ->
        (* this refers to the same struc we've already declared, don't update the map *)
        (struct_map, unique_tag)
    | _ ->
        (* this declare a new type, generate a tag and update the map *)
        let unique_tag = Unique_ids.make_named_temporary tag in
        let entry = { unique_tag; struct_from_current_scope = true } in
        (String_map.add tag entry struct_map, unique_tag)
  in
  let resolve_member m =
    (* note that we need to use new structure map here in case member type is derived from this structure type  *)
    { m with member_type = resolve_type new_map m.member_type }
  in
  let resolved_members = List.map resolve_member members in
  (new_map, { tag = resolved_tag; members = resolved_members })

let resolve_file_scope_variable_declaration struct_map id_map
    ({ name; var_type; _ } as vd : initializr variable_declaration) =
  let resolved_vd = { vd with var_type = resolve_type struct_map var_type } in
  let new_map =
    String_map.add name
      { unique_name = name; from_current_scope = true; has_linkage = true }
      id_map
  in
  (new_map, resolved_vd)

let resolve_global_declaration (struct_map, id_map) = function
  | FunDecl fd ->
      let id_map1, fd = resolve_function_declaration struct_map id_map fd in
      ((struct_map, id_map1), FunDecl fd)
  | VarDecl vd ->
      let id_map1, resolved_vd =
        resolve_file_scope_variable_declaration struct_map id_map vd
      in
      ((struct_map, id_map1), VarDecl resolved_vd)
  | StructDecl sd ->
      let struct_map1, resolved_sd =
        resolve_structure_declaration struct_map sd
      in
      ((struct_map1, id_map), StructDecl resolved_sd)

let resolve (Program decls) =
  let _, resolved_decls =
    List.fold_left_map resolve_global_declaration
      (String_map.empty, String_map.empty)
      decls
  in
  Program resolved_decls
