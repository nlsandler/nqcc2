open Ast
open Batteries

let rec collect_labels_from_statement (defined, used) = function
  | Ast.Goto lbl -> (defined, Set.add lbl used)
  | LabeledStatement (lbl, stmt) ->
      if Set.mem lbl defined then failwith ("Duplicate label: " ^ lbl);
      let defined' = Set.add lbl defined in
      collect_labels_from_statement (defined', used) stmt
  | If { then_clause; else_clause; _ } -> (
      let defined', used' =
        collect_labels_from_statement (defined, used) then_clause
      in
      match else_clause with
      | Some stmt -> collect_labels_from_statement (defined', used') stmt
      | None -> (defined', used'))
  | Compound (Block block_items) ->
      List.fold_left collect_labels_from_block_item (defined, used) block_items
  | While { body; _ } -> collect_labels_from_statement (defined, used) body
  | DoWhile { body; _ } -> collect_labels_from_statement (defined, used) body
  | For { body; _ } -> collect_labels_from_statement (defined, used) body
  | Return _ | Null | Expression _ | Break _ | Continue _ -> (defined, used)

and collect_labels_from_block_item lbls = function
  | Ast.S stmt -> collect_labels_from_statement lbls stmt
  | Ast.D _ -> lbls

let validate_labels_in_fun (Function { body = Block block_items; _ }) =
  let labels_defined, labels_used =
    (* traverse AST to find all labels that are defined or used in goto statements;
       * collect_labels will also throw an error if it finds a duplicate label
    *)
    List.fold_left collect_labels_from_block_item (Set.empty, Set.empty)
      block_items
  in
  let undefined = Set.diff labels_used labels_defined in
  if not (Set.is_empty undefined) then
    failwith
      ("Found labels that are used but not defined: "
      ^ String.join ", " (Set.to_list undefined))

let validate_labels (Program fn_def) = validate_labels_in_fun fn_def
