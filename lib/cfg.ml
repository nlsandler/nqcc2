open Utils
module StringMap = Map.Make (String)

(* a simplified instruction type that can represent both TACKY and assembly
   instructions *)
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
  val pp_instr : Format.formatter -> instr -> unit
end

module Cfg (Instr : INSTR) = struct
  type node_id = Entry | Block of int | Exit [@deriving ord]

  (* Cfg is parameterized by type of val we compute and type of instruction (we
     use both Tacky and assembly instructions )*)

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
    (* label (e.g. fun name/param list) for debugging *)
    debug_label : string;
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
    let remove_id nd_id id_list = List.filter (fun i -> i <> nd_id) id_list in
    update_successors ~f:(remove_id succ) pred g;
    update_predecessors ~f:(remove_id pred) succ g

  (* replace block with given block ID *)
  let update_basic_block block_idx new_block g =
    let new_blocks =
      List.map
        (fun ((i, _) as blk) -> if i = block_idx then (i, new_block) else blk)
        g.basic_blocks
    in
    { g with basic_blocks = new_blocks }

  (* constructing the CFG *)
  let partition_into_basic_blocks instructions =
    let f (finished_blocks, current_block) i =
      match Instr.simplify i with
      | Label _ ->
          let finished_blocks' =
            if current_block = [] then finished_blocks
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
      if last = [] then finished else List.rev last :: finished
    in
    List.rev all_blocks

  let add_all_edges g =
    (* build map from labels to the IDS of the blocks that they start with *)
    let label_map =
      List.fold_left
        (fun lbl_map (_, blk) ->
          match Instr.simplify (snd (List.hd blk.instructions)) with
          | Label lbl -> StringMap.add lbl blk.id lbl_map
          | _ -> lbl_map)
        StringMap.empty g.basic_blocks
    in

    (* add outgoing edges from a single basic block *)
    let process_node (id_num, block) =
      let next_block =
        if id_num = fst (ListUtil.last g.basic_blocks) then Exit
        else Block (id_num + 1)
      in
      let (), last_instr = ListUtil.last block.instructions in

      match Instr.simplify last_instr with
      | Return -> add_edge block.id Exit g
      | UnconditionalJump target ->
          let target_id = StringMap.find target label_map in
          add_edge block.id target_id g
      | ConditionalJump target ->
          let target_id = StringMap.find target label_map in
          add_edge block.id next_block g;
          add_edge block.id target_id g
      | _ -> add_edge block.id next_block g
    in
    add_edge Entry (Block 0) g;
    List.iter process_node g.basic_blocks

  let instructions_to_cfg debug_label instructions =
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
        debug_label;
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
  let print_graphviz pp_val cfg =
    let filename = Unique_ids.make_label cfg.debug_label ^ ".dot" in
    let path =
      if Filename.is_relative filename then
        Filename.concat (Sys.getcwd ()) filename
      else filename
    in
    let chan = open_out path in
    let formatter = Format.formatter_of_out_channel chan in
    let pp_node_id out = function
      | Exit -> Format.pp_print_string out "exit"
      | Entry -> Format.pp_print_string out "entry"
      | Block n -> Format.fprintf out "block%d" n
    in
    let pp_annotated_instruction out (v, i) =
      Format.pp_open_box out 0;
      Format.fprintf out
        "@{<tr>@,\
         @[<h><td align=\"left\">%a</td>@]@,\
         <td align=\"left\">%a</td>@}@,"
        Instr.pp_instr i pp_val v;
      Format.pp_close_box out ()
    in
    let pp_block_instructions out blk =
      Format.pp_open_vbox out 0;
      Format.(pp_open_stag out (String_tag "table"));
      Format.fprintf out "@{<tr><td colspan=\"2\">@{<b>%a@}</td>@}@," pp_node_id
        blk.id;
      Format.pp_print_list pp_annotated_instruction out blk.instructions;
      (* print block annotations *)
      Format.fprintf out "@{<tr><td colspan=\"2\">%a</td>@}@," pp_val blk.value;
      Format.pp_close_stag out ();
      Format.pp_close_box out ()
    in
    let pp_block out (lbl, b) =
      Format.fprintf out "block%d[label=<%a>]" lbl pp_block_instructions b
    in
    let pp_entry_edge out lbl =
      Format.fprintf out "entry -> %a" pp_node_id lbl
    in
    let pp_edge i out succ =
      Format.fprintf out "block%d -> %a" i pp_node_id succ
    in
    let pp_edges out ((lbl : int), (blk : 'a basic_block)) =
      Format.open_vbox 0;
      Format.pp_print_list (pp_edge lbl) out blk.succs;
      Format.close_box ()
    in
    Format.pp_set_tags formatter true;
    Format.pp_open_vbox formatter 0;
    Format.pp_print_string formatter "digraph {";
    Format.pp_print_break formatter 0 2;
    Format.pp_open_vbox formatter 0;
    Format.pp_print_string formatter "labeljust=l";
    Format.pp_print_cut formatter ();
    Format.pp_print_string formatter "node[shape=\"box\"]";
    Format.pp_print_cut formatter ();
    Format.pp_print_string formatter "entry[label=\"ENTRY\"]";
    Format.pp_print_cut formatter ();
    Format.pp_print_string formatter "exit[label=\"EXIT\"]";
    Format.pp_print_cut formatter ();
    Format.pp_print_list pp_block formatter cfg.basic_blocks;
    Format.pp_print_cut formatter ();
    Format.pp_print_list pp_entry_edge formatter cfg.entry_succs;
    Format.pp_print_cut formatter ();
    Format.pp_print_list pp_edges formatter cfg.basic_blocks;
    Format.pp_close_box formatter ();
    Format.pp_print_cut formatter ();
    Format.pp_print_string formatter "}";
    Format.pp_close_box formatter ();
    Format.pp_print_newline formatter ();
    close_out chan;
    let cmd =
      Printf.sprintf "dot -Tpng %s -o %s" filename
        (Filename.chop_extension filename ^ ".png")
    in
    if Sys.command cmd <> 0 then failwith ("graphviz fail: " ^ cmd)
    else if Sys.command ("rm " ^ filename) <> 0 then
      failwith "failed to remove DOT file"
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

  let pp_instr = Tacky_print.pp_instruction ~escape_brackets:true
  [@@coverage off]
end)
