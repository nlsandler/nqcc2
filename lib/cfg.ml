open Batteries

(* a simplified instruction type that can represent both TACKY and assembly instructions *)
type simple_instr =
  | Label of string
  | ConditionalJump of string
  | UnconditionalJump of string
  | Return
  | Other

module type INSTR = sig
  type instr

  (* functions for classifying basic blocks *)
  val simplify : instr -> simple_instr
  val show_instr : instr -> string
end

module Cfg (Instr : INSTR) = struct
  type node_id = Entry | Block of int | Exit

  (* Cfg is parameterized by type of val we compute and type of instruction (we use both Tacky and assembly instructions )*)

  type 'v basic_block = {
    id : node_id;
    instructions : ('v * Instr.instr) list;
    mutable preds : node_id list;
    mutable succs : node_id list;
    value : 'v;
  }

  type 'v t = {
    (* store basic blocks in association list, indexed by block # *)
    basic_blocks : (int * 'v basic_block) list;
    mutable entry_succs : node_id list;
    mutable exit_preds : node_id list;
  }

  (* useful functions *)

  let get_succs nd_id cfg =
    match nd_id with
    | Entry -> cfg.entry_succs
    | Block n ->
        let nd = List.assoc n cfg.basic_blocks in
        nd.succs
    | Exit -> []

  let get_block_value blocknum cfg =
    let nd = List.assoc blocknum cfg.basic_blocks in
    nd.value

  let update_successors ~f nd_id g =
    match nd_id with
    | Entry -> g.entry_succs <- f g.entry_succs
    | Block n ->
        let blk = List.assoc n g.basic_blocks in
        blk.succs <- f blk.succs
    | Exit -> failwith "Internal error: malformed CFG" [@coverage off]

  let update_predecessors ~f nd_id g =
    match nd_id with
    | Entry -> failwith "Internal error: malformed CFG" [@coverage off]
    | Block n ->
        let blk = List.assoc n g.basic_blocks in
        blk.preds <- f blk.preds
    | Exit -> g.exit_preds <- f g.exit_preds

  let add_edge pred succ g =
    let add_id nd_id id_list =
      if List.mem nd_id id_list then id_list else nd_id :: id_list
    in
    update_successors ~f:(add_id succ) pred g;
    update_predecessors ~f:(add_id pred) succ g

  let remove_edge pred succ g =
    let remove_id nd_id id_list = List.remove id_list nd_id in
    update_successors ~f:(remove_id succ) pred g;
    update_predecessors ~f:(remove_id pred) succ g

  (* constructing the CFG *)
  let partition_into_basic_blocks instructions =
    let f (finished_blocks, current_block) i =
      match Instr.simplify i with
      | Label _ ->
          let finished_blocks' =
            if List.is_empty current_block then finished_blocks
            else List.rev current_block :: finished_blocks
          in
          (finished_blocks', [ i ])
      | ConditionalJump _ | UnconditionalJump _ | Return ->
          let finished = List.rev (i :: current_block) in
          (finished :: finished_blocks, [])
      | Other -> (finished_blocks, i :: current_block)
    in
    let finished, last = List.fold_left f ([], []) instructions in
    let all_blocks =
      if List.is_empty last then finished else List.rev last :: finished
    in
    List.rev all_blocks

  let add_all_edges g =
    (* build map from labels to the IDS of the blocks that they start with *)
    let label_map =
      List.fold
        (fun lbl_map (_, blk) ->
          match Instr.simplify (snd (List.hd blk.instructions)) with
          | Label lbl -> Map.add lbl blk.id lbl_map
          | _ -> lbl_map)
        Map.empty g.basic_blocks
    in

    (* add outgoing edges from a single basic block *)
    let process_node (id_num, block) =
      let next_block =
        if id_num = fst (List.last g.basic_blocks) then Exit
        else Block (id_num + 1)
      in
      let (), last_instr = List.last block.instructions in

      match Instr.simplify last_instr with
      | Return -> add_edge block.id Exit g
      | UnconditionalJump target ->
          let target_id = Map.find target label_map in
          add_edge block.id target_id g
      | ConditionalJump target ->
          let target_id = Map.find target label_map in
          add_edge block.id next_block g;
          add_edge block.id target_id g
      | _ -> add_edge block.id next_block g
    in
    add_edge Entry (Block 0) g;
    List.iter process_node g.basic_blocks

  let instructions_to_cfg instructions =
    let to_node idx instructions =
      let ann x = ((), x) in
      ( idx,
        {
          id = Block idx;
          instructions = List.map ann instructions;
          preds = [];
          succs = [];
          value = ();
        } )
    in
    let cfg =
      {
        basic_blocks =
          List.mapi to_node (partition_into_basic_blocks instructions);
        entry_succs = [];
        exit_preds = [];
      }
    in

    add_all_edges cfg;
    cfg

  (* converting back to instructions *)
  let cfg_to_instructions g =
    let blk_to_instrs (_, { instructions; _ }) = List.map snd instructions in
    List.concat_map blk_to_instrs g.basic_blocks

  (* working with annotations *)
  let initialize_annotation cfg dummy_val =
    let initialize_instruction (_, i) = (dummy_val, i) in
    let initialize_block (idx, b) =
      ( idx,
        {
          b with
          instructions = List.map initialize_instruction b.instructions;
          value = dummy_val;
        } )
    in
    { cfg with basic_blocks = List.map initialize_block cfg.basic_blocks }

  let strip_annotations cfg = initialize_annotation cfg ()

  (* debugging *)
  let print_graphviz filename val_printer cfg =
    let path = Filename.concat (Sys.getcwd ()) filename in
    let chan = open_out path in
    let node_id_to_label = function
      | Exit -> "exit"
      | Entry -> "entry"
      | Block n -> Printf.sprintf "block%d" n
    in
    let instruction_to_label (v, i) =
      Printf.sprintf "%s (%s)\n" (Instr.show_instr i) (val_printer v)
    in
    let block_to_label blk =
      String.join "\\n" (List.map instruction_to_label blk.instructions)
    in
    let print_block (lbl, b) =
      Printf.fprintf chan "\tblock%d [label=\"%s\"]\n" lbl (block_to_label b)
    in
    let add_entry_edge lbl =
      Printf.fprintf chan "\tentry -> %s\n" (node_id_to_label lbl)
    in
    let add_edge i succ =
      Printf.fprintf chan "\tblock%d  -> %s\n" i (node_id_to_label succ)
    in
    let add_edges ((lbl : int), (blk : 'a basic_block)) =
      List.iter (add_edge lbl) blk.succs
    in
    Printf.fprintf chan "digraph {\n";
    Printf.fprintf chan "\tnode[shape=\"box\"]\n";
    Printf.fprintf chan "\tentry [label=\"ENTRY\"]\n";
    Printf.fprintf chan "\texit [label=\"EXIT\"]\n";
    List.iter print_block cfg.basic_blocks;
    List.iter add_entry_edge cfg.entry_succs;
    List.iter add_edges cfg.basic_blocks;
    Printf.fprintf chan "\t}\n";
    close_out chan
    [@@coverage off]
end

module TackyCfg = Cfg (struct
  type instr = Tacky.instruction

  let simplify = function
    | Tacky.Label l -> Label l
    | Jump target -> UnconditionalJump target
    | JumpIfZero (_, target) -> ConditionalJump target
    | JumpIfNotZero (_, target) -> ConditionalJump target
    | Return _ -> Return
    | _ -> Other

  let show_instr i =
    i |> Tacky.show_instruction |> String.filter (fun x -> x <> '\"')
    [@@coverage off]
end)
