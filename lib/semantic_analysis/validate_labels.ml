open Ast
open Batteries

let rec collect_labels_from_statement transform_lbl (defined, used) = function
  | Ast.Goto lbl -> ((defined, Set.add lbl used), Ast.Goto (transform_lbl lbl))
  | LabeledStatement (lbl, stmt) ->
      if Set.mem lbl defined then failwith ("Duplicate label: " ^ lbl);
      let defined' = Set.add lbl defined in
      let lbls, renamed_stmt =
        collect_labels_from_statement transform_lbl (defined', used) stmt
      in
      (lbls, LabeledStatement (transform_lbl lbl, renamed_stmt))
  | If { then_clause; else_clause; condition } ->
      let (defined', used'), then_clause' =
        collect_labels_from_statement transform_lbl (defined, used) then_clause
      in
      let (defined'', used''), else_clause' =
        match else_clause with
        | Some stmt ->
            Tuple2.map2 Option.some
              (collect_labels_from_statement transform_lbl (defined', used')
                 stmt)
        | None -> ((defined', used'), None)
      in
      ( (defined'', used''),
        If { then_clause = then_clause'; else_clause = else_clause'; condition }
      )
  | Compound (Block block_items) ->
      let lbls, block_items' =
        List.fold_left_map
          (collect_labels_from_block_item transform_lbl)
          (defined, used) block_items
      in
      (lbls, Compound (Block block_items'))
  | While w ->
      let lbls, body' =
        collect_labels_from_statement transform_lbl (defined, used) w.body
      in
      (lbls, While { w with body = body' })
  | DoWhile dw ->
      let lbls, body' =
        collect_labels_from_statement transform_lbl (defined, used) dw.body
      in
      (lbls, DoWhile { dw with body = body' })
  | For fr ->
      let lbls, body' =
        collect_labels_from_statement transform_lbl (defined, used) fr.body
      in
      (lbls, For { fr with body = body' })
  | Switch sw ->
      let lbls, body' =
        collect_labels_from_statement transform_lbl (defined, used) sw.body
      in
      (lbls, Switch { sw with body = body' })
  | Case (v, stmt, id) ->
      let lbls, stmt' =
        collect_labels_from_statement transform_lbl (defined, used) stmt
      in
      (lbls, Case (v, stmt', id))
  | Default (stmt, id) ->
      let lbls, stmt' =
        collect_labels_from_statement transform_lbl (defined, used) stmt
      in
      (lbls, Default (stmt', id))
  | (Return _ | Null | Expression _ | Break _ | Continue _) as s ->
      ((defined, used), s)

and collect_labels_from_block_item transform_lbl lbls = function
  | Ast.S stmt ->
      let lbls, stmt' = collect_labels_from_statement transform_lbl lbls stmt in
      (lbls, S stmt')
  | Ast.D _ as decl -> (lbls, decl)

let validate_labels_in_fun fn_decl =
  let transform_lbl lbl = fn_decl.name ^ "." ^ lbl in
  match fn_decl.body with
  | Some (Block block_items) ->
      let (labels_defined, labels_used), renamed_block_items =
        (* traverse AST to find all labels that are defined or used in goto statements;
           * collect_labels will also throw an error if it finds a duplicate label
        *)
        List.fold_left_map
          (collect_labels_from_block_item transform_lbl)
          (Set.empty, Set.empty) block_items
      in
      let undefined = Set.diff labels_used labels_defined in
      if not (Set.is_empty undefined) then
        failwith
          ("Found labels that are used but not defined: "
          ^ String.join ", " (Set.to_list undefined));
      { fn_decl with body = Some (Block renamed_block_items) }
  | None -> fn_decl

let validate_labels_in_decl = function
  | FunDecl fd -> FunDecl (validate_labels_in_fun fd)
  | vd -> vd

let validate_labels (Program decls) =
  Program (List.map validate_labels_in_decl decls)
