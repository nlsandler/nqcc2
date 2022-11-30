open Batteries

module Dataflow (G : Cfg.CFG) = struct
  type 'var annotation = 'var Batteries.Set.t
  type 'var annotated_block = 'var annotation G.basic_block
  type 'var annotated_graph = 'var annotation G.t

  let debug_print ?(extra_tag = "") pp_var cfg =
    if !Settings.debug then
      let livevar_printer fmt live_vars =
        Format.pp_open_box fmt 0;
        Format.pp_print_list
          ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
          pp_var fmt (Set.to_list live_vars);
        Format.pp_close_box fmt ()
      in
      let lbl = cfg.G.debug_label ^ "_dse" ^ extra_tag in
      G.(print_graphviz livevar_printer { cfg with debug_label = lbl })

  let analyze pp_var meet_fn transfer_fn cfg =
    let starting_cfg = G.initialize_annotation cfg Set.empty in
    let rec process_worklist current_cfg
        (worklist : (int * 'var annotated_block) list) =
      debug_print ~extra_tag:"_in_progress_" pp_var current_cfg;
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
