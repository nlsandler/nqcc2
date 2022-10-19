open Batteries

module G = struct
  include Cfg.TackyCfg
end

type annotated_block = string Set.t G.basic_block

let transfer static_and_aliased_vars (block : annotated_block)
    (end_live_variables : string Set.t) =
  let remove_var var var_set =
    match var with
    | Tacky.Var v -> Set.remove v var_set
    | Tacky.Constant _ -> failwith "Internal error" [@coverage off]
  in

  let add_var v var_set =
    match v with
    | Tacky.Constant _ -> var_set
    | Var name -> Set.add name var_set
  in
  let add_vars used_vals var_set = List.fold_right add_var used_vals var_set in

  let process_instr current_live_vars (_, i) =
    let annotated_instr = (current_live_vars, i) in
    let new_live_vars =
      match i with
      | Tacky.Binary b ->
          current_live_vars |> remove_var b.dst |> add_vars [ b.src1; b.src2 ]
      | Tacky.Unary u -> current_live_vars |> remove_var u.dst |> add_var u.src
      | JumpIfZero (v, _) -> add_var v current_live_vars
      | JumpIfNotZero (v, _) -> add_var v current_live_vars
      | Copy c -> current_live_vars |> remove_var c.dst |> add_var c.src
      | Return (Some v) -> current_live_vars |> add_var v
      | FunCall f ->
          let live_vars' =
            match f.dst with
            | Some d -> remove_var d current_live_vars
            | None -> current_live_vars
          in
          let live_vars'' = add_vars f.args live_vars' in
          Set.union live_vars'' static_and_aliased_vars
      (* part II instructions *)
      | SignExtend sx ->
          current_live_vars |> remove_var sx.dst |> add_var sx.src
      | ZeroExtend zx ->
          current_live_vars |> remove_var zx.dst |> add_var zx.src
      | DoubleToInt d2i ->
          current_live_vars |> remove_var d2i.dst |> add_var d2i.src
      | IntToDouble i2d ->
          current_live_vars |> remove_var i2d.dst |> add_var i2d.src
      | DoubleToUInt d2u ->
          current_live_vars |> remove_var d2u.dst |> add_var d2u.src
      | UIntToDouble u2d ->
          current_live_vars |> remove_var u2d.dst |> add_var u2d.src
      | Truncate t -> current_live_vars |> remove_var t.dst |> add_var t.src
      | AddPtr { ptr; index; dst; _ } ->
          current_live_vars |> remove_var dst |> add_vars [ ptr; index ]
      | GetAddress { dst; _ } -> remove_var dst current_live_vars
      | Load { src_ptr; dst } ->
          current_live_vars
          |> remove_var dst
          |> add_var src_ptr
          |> Set.union static_and_aliased_vars
      | Store { src; dst_ptr } -> add_vars [ src; dst_ptr ] current_live_vars
      | CopyToOffset { src; _ } -> add_var src current_live_vars
      | CopyFromOffset { src; dst; _ } ->
          current_live_vars |> remove_var dst |> add_var (Var src)
      (* instructions with no impact *)
      | Jump _ | Label _ | Return None -> current_live_vars
    in
    (new_live_vars, annotated_instr)
  in
  let incoming_live_vars, annotated_reversed_instructions =
    block.instructions
    |> List.rev
    |> List.fold_left_map process_instr end_live_variables
  in
  {
    block with
    instructions = List.rev annotated_reversed_instructions;
    value = incoming_live_vars;
  }

let meet static_vars cfg (block : annotated_block) =
  let update_live live = function
    | G.Entry -> failwith "Internal error: malformed CFG" [@coverage off]
    | Exit -> Set.union live static_vars
    | Block n -> Set.union live (G.get_block_value n cfg)
  in
  List.fold update_live Set.empty block.succs

let find_live_variables static_vars aliased_vars (cfg : unit G.t) =
  let starting_cfg = G.initialize_annotation cfg Set.empty in
  let static_and_aliased_vars = Set.union static_vars aliased_vars in
  let rec process_worklist current_cfg (worklist : (int * annotated_block) list)
      =
    match worklist with
    | [] -> current_cfg (* we're done*)
    | (block_idx, blk) :: rest ->
        let old_annotation = blk.value in
        let live_vars_at_exit = meet static_vars current_cfg blk in
        let block' = transfer static_and_aliased_vars blk live_vars_at_exit in
        let updated_cfg =
          G.
            {
              current_cfg with
              basic_blocks =
                List.modify block_idx (fun _ -> block') current_cfg.basic_blocks;
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
  process_worklist starting_cfg starting_cfg.basic_blocks

let is_dead_store (live_vars, i) =
  match i with
  | Tacky.FunCall _ -> false
  | Tacky.Store _ -> false
  | _ -> (
      match Optimize_utils.get_dst i with
      | Some (Var v) when not (Set.mem v live_vars) -> true
      | _ -> false)

let optimize aliased_vars cfg =
  let entry_is_static = function
    | v, Symbols.{ attrs = StaticAttr _; _ } -> Some v
    | _ -> None
  in
  let static_vars =
    Symbols.bindings () |> List.filter_map entry_is_static |> Set.of_list
  in
  let annotated_cfg = find_live_variables static_vars aliased_vars cfg in
  let rewrite_block (idx, block) =
    ( idx,
      G.
        {
          block with
          instructions = List.filter (not % is_dead_store) block.instructions;
        } )
  in
  let transformed_cfg =
    {
      annotated_cfg with
      basic_blocks = List.map rewrite_block annotated_cfg.basic_blocks;
    }
  in
  G.strip_annotations transformed_cfg
