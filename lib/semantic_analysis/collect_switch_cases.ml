(* Traverse AST and collect case and default statements associated w/ each switch.
 * We annotate each case and default statement with a unique ID, and annotate each
 * switch with a (int option --> string) map, where the keys are:
 * - Some i to represent case i
 * - None to represent default
 * and the values are the IDs of the corresponding case/default statements.
 * (We represent this map with as an association list - i.e. a list of pairs - rather than with the Map type)
 * We pass around an optional (type, map) pair as we traverse the AST, containing this map and the type of the
 * switch statement's controlling experssion.
 * : the map is present when we're in a
 * switch statement and absent otherwise.
 *)
open Ast.Typed
open Batteries

(* common logic for case and default statements *)
let rec analyze_case_or_default key opt_switch_ctx lbl inner_statement =
  (* First make sure we're in a switch statement *)
  let switch_t, case_map =
    match opt_switch_ctx with
    | Some (t, s) -> (t, s)
    | None -> failwith "Found case statement outside of switch"
  in
  (* Convert key to type of controlling switch statement *)
  let key = Option.map (Const_convert.const_convert switch_t) key in
  (* Check for duplicates *)
  if List.mem_assoc key case_map then
    failwith
      (Option.map_default
         (fun i -> "Duplicate case in switch statement: " ^ Const.show i)
         "Duplicate default in switch statement" key);
  (* Generate new ID - lbl should be "case" or "default" *)
  let case_id = Unique_ids.make_label lbl in
  let updated_ctx = Some (switch_t, (key, case_id) :: case_map) in
  (* Analyze inner statement *)
  let final_ctx, new_inner_statement =
    analyze_statement updated_ctx inner_statement
  in
  (final_ctx, new_inner_statement, case_id)

and analyze_statement opt_switch_ctx = function
  | Default (stmt, _) ->
      let new_ctx, new_stmt, default_id =
        analyze_case_or_default None opt_switch_ctx "default" stmt
      in
      (new_ctx, Default (new_stmt, default_id))
  | Case (v, stmt, _) ->
      (* Get integer value of this case *)
      let key =
        match v with
        | { e = Constant c; _ } -> Some c
        | _ -> failwith "Non-constant label in case statement"
      in
      let new_ctx, new_stmt, case_id =
        analyze_case_or_default key opt_switch_ctx "case" stmt
      in
      (new_ctx, Case (v, new_stmt, case_id))
  | Switch s ->
      (* Use fresh map/expression type when traversing body *)
      let switch_t = Type_utils.get_type s.control in
      let new_ctx, new_body = analyze_statement (Some (switch_t, [])) s.body in
      (* annotate switch with new case map; don't pass new case map to caller *)
      ( opt_switch_ctx,
        (* first item in new_ctx is controlling switch_t, which we don't need at this point *)
        Switch { s with body = new_body; cases = snd (Option.get new_ctx) } )
      (* Just pass switch context through to substatements in remaining statements*)
  | If { condition; then_clause; else_clause } ->
      let opt_switch_ctx1, new_then_clause =
        analyze_statement opt_switch_ctx then_clause
      in
      let opt_switch_ctx2, new_else_clause =
        match else_clause with
        | Some c ->
            Tuple2.map2 Option.some (analyze_statement opt_switch_ctx1 c)
        | None -> (opt_switch_ctx1, None)
      in
      ( opt_switch_ctx2,
        If
          {
            condition;
            then_clause = new_then_clause;
            else_clause = new_else_clause;
          } )
  | Compound block ->
      let new_ctx, new_block = analyze_block opt_switch_ctx block in
      (new_ctx, Compound new_block)
  | While w ->
      let new_ctx, new_body = analyze_statement opt_switch_ctx w.body in
      (new_ctx, While { w with body = new_body })
  | DoWhile d ->
      let new_ctx, new_body = analyze_statement opt_switch_ctx d.body in
      (new_ctx, DoWhile { d with body = new_body })
  | For f ->
      let new_ctx, new_body = analyze_statement opt_switch_ctx f.body in
      (new_ctx, For { f with body = new_body })
  | LabeledStatement (lbl, stmt) ->
      let new_ctx, new_stmt = analyze_statement opt_switch_ctx stmt in
      (new_ctx, LabeledStatement (lbl, new_stmt))
  (* These don't include sub-statements *)
  | (Return _ | Null | Expression _ | Break _ | Continue _ | Goto _) as s ->
      (opt_switch_ctx, s)

and analyze_block_item opt_switch_ctx = function
  | S stmt ->
      let new_opt_switch_ctx, new_stmt =
        analyze_statement opt_switch_ctx stmt
      in
      (new_opt_switch_ctx, S new_stmt)
  | decl ->
      (opt_switch_ctx, decl (* don't need to change or traverse declaration *))

and analyze_block opt_switch_ctx (Block b) =
  let new_opt_switch_ctx, new_block_items =
    List.fold_left_map analyze_block_item opt_switch_ctx b
  in
  (new_opt_switch_ctx, Block new_block_items)

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
