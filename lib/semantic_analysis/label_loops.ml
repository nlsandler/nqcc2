open Ast.Untyped

let rec label_statement (current_break_id, current_continue_id) = function
  | Break _ -> (
      match current_break_id with
      | Some l -> Break l
      | None -> failwith "Break outside of loop or switch")
  | Continue _ -> (
      match current_continue_id with
      | Some l -> Continue l
      | None -> failwith "Continue outside of loop")
  | While while_loop ->
      let new_id = Unique_ids.make_label "while" in
      While
        {
          while_loop with
          body = label_statement (Some new_id, Some new_id) while_loop.body;
          id = new_id;
        }
  | DoWhile do_loop ->
      let new_id = Unique_ids.make_label "do_while" in
      DoWhile
        {
          do_loop with
          body = label_statement (Some new_id, Some new_id) do_loop.body;
          id = new_id;
        }
  | For for_loop ->
      let new_id = Unique_ids.make_label "for" in
      For
        {
          for_loop with
          body = label_statement (Some new_id, Some new_id) for_loop.body;
          id = new_id;
        }
  | Compound blk ->
      Compound (label_block (current_break_id, current_continue_id) blk)
  | If if_statement ->
      If
        {
          if_statement with
          then_clause =
            label_statement
              (current_break_id, current_continue_id)
              if_statement.then_clause;
          else_clause =
            Option.map
              (label_statement (current_break_id, current_continue_id))
              if_statement.else_clause;
        }
  | LabeledStatement (lbl, stmt) ->
      LabeledStatement
        (lbl, label_statement (current_break_id, current_continue_id) stmt)
  | Default (stmt, id) ->
      Default (label_statement (current_break_id, current_continue_id) stmt, id)
  | Case (v, stmt, id) ->
      Case (v, label_statement (current_break_id, current_continue_id) stmt, id)
  | Switch s ->
      let new_break_id = Unique_ids.make_label "switch" in
      let labeled_body =
        label_statement (Some new_break_id, current_continue_id) s.body
      in
      Switch { s with body = labeled_body; id = new_break_id }
  | (Null | Return _ | Expression _ | Goto _) as s -> s

and label_block_item current_ids = function
  | S s -> S (label_statement current_ids s)
  | decl -> decl

and label_block current_ids (Block b) =
  Block (List.map (label_block_item current_ids) b)

let label_decl = function
  | FunDecl fn ->
      FunDecl { fn with body = Option.map (label_block (None, None)) fn.body }
  | var_decl -> var_decl

let label_loops (Program decls) = Program (List.map label_decl decls)
