open Ast

let rec label_statement current_label = function
  | Break _ -> (
      match current_label with
      | Some l -> Break l
      | None -> failwith "Break outside of loop")
  | Continue _ -> (
      match current_label with
      | Some l -> Continue l
      | None -> failwith "Continue outside of loop")
  | While while_loop ->
      let new_id = Unique_ids.make_label "while" in
      While
        {
          while_loop with
          body = label_statement (Some new_id) while_loop.body;
          id = new_id;
        }
  | DoWhile do_loop ->
      let new_id = Unique_ids.make_label "do_while" in
      DoWhile
        {
          do_loop with
          body = label_statement (Some new_id) do_loop.body;
          id = new_id;
        }
  | For for_loop ->
      let new_id = Unique_ids.make_label "for" in
      For
        {
          for_loop with
          body = label_statement (Some new_id) for_loop.body;
          id = new_id;
        }
  | Compound blk -> Compound (label_block current_label blk)
  | If if_statement ->
      If
        {
          if_statement with
          then_clause = label_statement current_label if_statement.then_clause;
          else_clause =
            Option.map (label_statement current_label) if_statement.else_clause;
        }
  | (Null | Return _ | Expression _) as s -> s

and label_block_item current_label = function
  | S s -> S (label_statement current_label s)
  | decl -> decl

and label_block current_label (Block b) =
  Block (List.map (label_block_item current_label) b)

let label_function_def fn =
  { fn with body = Option.map (label_block None) fn.body }

let label_loops (Program fn_defs) =
  Program (List.map label_function_def fn_defs)
