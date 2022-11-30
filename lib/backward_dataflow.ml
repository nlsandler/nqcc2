open Batteries

module Dataflow (G : Cfg.CFG) = struct
  type 'var annotation = 'var Batteries.Set.t
  type 'var annotated_block = 'var annotation G.basic_block
  type 'var annotated_graph = 'var annotation G.t

  let analyze debug_printer meet_fn transfer_fn cfg =
    let starting_cfg = G.initialize_annotation cfg Set.empty in
    let rec process_worklist current_cfg
        (worklist : (int * 'var annotated_block) list) =
      debug_printer current_cfg;
      match worklist with
      | [] -> current_cfg (* we're done*)
      | (block_idx, blk) :: rest ->
          let old_annotation = blk.value in
          let live_vars_at_exit = meet_fn current_cfg blk in
          let block' = transfer_fn blk live_vars_at_exit in
          let updated_cfg =
            G.
              {
                current_cfg with
                basic_blocks =
                  List.modify block_idx
                    (fun _ -> block')
                    current_cfg.basic_blocks;
              }
          in
          let new_worklist =
            if Set.equal old_annotation block'.value then rest
            else
              List.fold
                (fun wklist -> function
                  | G.Entry -> wklist
                  | Exit ->
                      failwith "Internal error: malformed CFG" [@coverage off]
                  | Block n ->
                      if List.mem_assoc n wklist then wklist
                      else (n, List.assoc n updated_cfg.basic_blocks) :: wklist)
                rest block'.preds
          in
          process_worklist updated_cfg new_worklist
    in
    process_worklist starting_cfg (List.rev starting_cfg.basic_blocks)
end
