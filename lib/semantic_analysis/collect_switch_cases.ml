(* Traverse AST and collect case and default statements associated w/ each switch.
 * We annotate each case and default statement with a unique ID, and annotate each
 * switch with a (int option --> string) map, where the keys are:
 * - Some i to represent case i
 * - None to represent default
 * and the values are the IDs of the corresponding case/default statements.
 * (We represent this map with as an association list - i.e. a list of pairs - rather than with the Map type)
 * We pass around an optional map as we traverse the AST: the map is present when we're in a
 * switch statement and absent otherwise.
 *)
open Ast
open Batteries

(* common logic for case and default statements *)
let rec analyze_case_or_default key opt_case_map lbl inner_statement =
  (* First make sure we're in a switch statement *)
  let case_map =
    match opt_case_map with
    | Some s -> s
    | None -> failwith "Found case statement outside of switch"
  in
  (* Check for duplicates *)
  if List.mem_assoc key case_map then
    failwith
      (Option.map_default
         (fun i -> "Duplicate case in switch statement: " ^ Int.to_string i)
         "Duplicate default in switch statement" key);
  (* Generate new ID - lbl should be "case" or "default" *)
  let case_id = Unique_ids.make_label lbl in
  let updated_map = Some ((key, case_id) :: case_map) in
  (* Analyze inner statement *)
  let final_map, new_inner_statement =
    analyze_statement updated_map inner_statement
  in
  (final_map, new_inner_statement, case_id)

and analyze_statement opt_case_map = function
  | Default (stmt, _) ->
      let new_map, new_stmt, default_id =
        analyze_case_or_default None opt_case_map "default" stmt
      in
      (new_map, Default (new_stmt, default_id))
  | Case (v, stmt, _) ->
      (* Get integer value of this case *)
      let key =
        match v with
        | Constant c -> Some c
        | _ -> failwith "Non-constant label in case statement"
      in
      let new_map, new_stmt, case_id =
        analyze_case_or_default key opt_case_map "case" stmt
      in
      (new_map, Case (v, new_stmt, case_id))
  | Switch s ->
      (* Use fresh map when traversing body *)
      let new_case_map, new_body = analyze_statement (Some []) s.body in
      (* annotate switch with new case map; don't pass new case map to caller *)
      ( opt_case_map,
        Switch { s with body = new_body; cases = Option.get new_case_map } )
      (* Just pass case_map through to substatements *)
  | If { condition; then_clause; else_clause } ->
      let case_map1, new_then_clause =
        analyze_statement opt_case_map then_clause
      in
      let case_map2, new_else_clause =
        match else_clause with
        | Some c -> Tuple2.map2 Option.some (analyze_statement case_map1 c)
        | None -> (case_map1, None)
      in
      ( case_map2,
        If
          {
            condition;
            then_clause = new_then_clause;
            else_clause = new_else_clause;
          } )
  | Compound block ->
      let new_case_map, new_block = analyze_block opt_case_map block in
      (new_case_map, Compound new_block)
  | While w ->
      let new_map, new_body = analyze_statement opt_case_map w.body in
      (new_map, While { w with body = new_body })
  | DoWhile d ->
      let new_map, new_body = analyze_statement opt_case_map d.body in
      (new_map, DoWhile { d with body = new_body })
  | For f ->
      let new_map, new_body = analyze_statement opt_case_map f.body in
      (new_map, For { f with body = new_body })
  | LabeledStatement (lbl, stmt) ->
      let new_case_map, new_stmt = analyze_statement opt_case_map stmt in
      (new_case_map, LabeledStatement (lbl, new_stmt))
  (* These don't include sub-statements *)
  | (Return _ | Null | Expression _ | Break _ | Continue _ | Goto _) as s ->
      (opt_case_map, s)

and analyze_block_item opt_case_map = function
  | S stmt ->
      let new_opt_case_map, new_stmt = analyze_statement opt_case_map stmt in
      (new_opt_case_map, S new_stmt)
  | decl ->
      (opt_case_map, decl (* don't need to change or traverse declaration *))

and analyze_block opt_case_map (Block b) =
  let new_opt_case_map, new_block_items =
    List.fold_left_map analyze_block_item opt_case_map b
  in
  (new_opt_case_map, Block new_block_items)

let analyze_function_def fun_decl =
  match fun_decl.body with
  | Some b ->
      let _, blk = analyze_block None b in
      { fun_decl with body = Some blk }
  | None -> fun_decl

let analyze_decl = function
  | FunDecl fd -> FunDecl (analyze_function_def fd)
  | vd -> vd

let analyze_switches (Program decls) = Program (List.map analyze_decl decls)
