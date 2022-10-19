module G = struct
  include Cfg.TackyCfg
end

module NodeSet = Set.Make (struct
  type t = G.node_id

  let compare = compare
end)

let eliminate_unreachable_blocks (cfg : unit G.t) =
  let rec dfs explored node_id =
    (* we've already explored this node *)
    if NodeSet.mem node_id explored then explored
    else
      let explored' = NodeSet.add node_id explored in
      let succs = G.get_succs node_id cfg in
      List.fold_left dfs explored' succs
  in
  let reachable_block_ids = dfs NodeSet.empty G.Entry in

  let f (_, (blk : unit G.basic_block)) =
    if NodeSet.mem blk.id reachable_block_ids then true
    else (
      (* Not reachable - remove edges and filter it out *)
      List.iter (fun p -> G.remove_edge p blk.id cfg) blk.preds;
      List.iter (fun s -> G.remove_edge blk.id s cfg) blk.succs;
      false)
  in

  let updated_blocks = List.filter f cfg.basic_blocks in
  G.{ cfg with basic_blocks = updated_blocks }

let eliminate_useless_jumps (cfg : unit G.t) =
  let drop_last lst = Utils.take (List.length lst - 1) lst in
  let update idx ((n, (blk : unit G.basic_block)) as numbered_block) =
    if idx = List.length cfg.basic_blocks - 1 then
      (* don't modify last block *)
      numbered_block
    else
      match Utils.last blk.instructions with
      | _, Tacky.(Jump _ | JumpIfZero _ | JumpIfNotZero _) ->
          let _, default_succ = List.nth cfg.basic_blocks (idx + 1) in
          if List.for_all (fun nd -> nd = default_succ.id) blk.succs then
            (* jump instruction is useless, we can drop it *)
            (n, { blk with instructions = drop_last blk.instructions })
          else numbered_block
      | _ -> numbered_block
  in

  { cfg with basic_blocks = List.mapi update cfg.basic_blocks }

let eliminate_useless_labels (cfg : unit G.t) =
  let update idx ((n, (blk : unit G.basic_block)) as numbered_block) =
    match blk.instructions with
    | (_, Tacky.Label _) :: rest ->
        let default_pred =
          if idx = 0 then G.Entry
          else (snd (List.nth cfg.basic_blocks (idx - 1))).id
        in
        if List.for_all (fun nd -> nd = default_pred) blk.preds then
          (n, { blk with instructions = rest })
        else numbered_block
    | _ -> numbered_block
  in
  { cfg with basic_blocks = List.mapi update cfg.basic_blocks }

let remove_empty_blocks (cfg : unit G.t) =
  let remove (_, (blk : unit G.basic_block)) =
    if blk.instructions = [] then
      (* block is empty, remove it
         but first update edges
      *)
      match (blk.preds, blk.succs) with
      | [ pred ], [ succ ] ->
          (* add edge from predecessor to successor, remove edges from either to blk *)
          G.remove_edge pred blk.id cfg;
          G.remove_edge blk.id succ cfg;
          G.add_edge pred succ cfg;
          false
      | _ ->
          failwith
            "Empty block should have exactly one predecessor and one successor"
          [@coverage off]
    else true
  in
  { cfg with basic_blocks = List.filter remove cfg.basic_blocks }

(* Print CFG if debug flag is on *)
let debug_print cfg =
  if !Settings.debug then
    (* no annotations, so annotation printer is no-op*)
    let nop_printer _fmt () = () in
    let lbl = cfg.G.debug_label ^ "_unreachable" in
    G.(print_graphviz nop_printer { cfg with debug_label = lbl })

let optimize cfg =
  debug_print cfg;
  cfg
  |> eliminate_unreachable_blocks
  |> eliminate_useless_jumps
  |> eliminate_useless_labels
  |> remove_empty_blocks
