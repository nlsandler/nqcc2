open Batteries
open Ast

let rec resolve_exp var_map = function
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
      Assignment (resolve_exp var_map left, resolve_exp var_map right)
  | Var v -> (
      (* rename var from map  *)
      try Var (Map.find v var_map)
      with Not_found -> failwith (Printf.sprintf "Undeclared variable %s" v))
  (* recursively process operands for unary and binary *)
  | Unary (op, e) -> Unary (op, resolve_exp var_map e)
  | Binary (op, e1, e2) ->
      Binary (op, resolve_exp var_map e1, resolve_exp var_map e2)
  (* Nothing to do for constant *)
  | Constant _ as c -> c

let resolve_declaration var_map (Declaration { name; init }) =
  if Map.mem name var_map then failwith "Duplicate variable declaration"
  else
    (* generate a unique name and add it to the map *)
    let unique_name = Unique_ids.make_named_temporary name in
    let new_map = Map.add name unique_name var_map in
    (* resolve initializer if there is one *)
    let resolved_init = Option.map (resolve_exp new_map) init in
    (* return new map and resolved declaration *)
    (new_map, Declaration { name = unique_name; init = resolved_init })

let resolve_statement var_map = function
  | Return e -> Return (resolve_exp var_map e)
  | Expression e -> Expression (resolve_exp var_map e)
  | Null -> Null

let resolve_block_item var_map = function
  | S s ->
      (* resolving a statement doesn't change the variable map *)
      let resolved_s = resolve_statement var_map s in
      (var_map, S resolved_s)
  | D d ->
      (* resolving a declaration does change the variable map *)
      let new_map, resolved_d = resolve_declaration var_map d in
      (new_map, D resolved_d)

let resolve_function_def (Function { name; body }) =
  let var_map = Map.empty in
  let _final_map, resolved_body =
    List.fold_left_map resolve_block_item var_map body
  in
  Function { name; body = resolved_body }

let resolve (Program fn_def) = Program (resolve_function_def fn_def)