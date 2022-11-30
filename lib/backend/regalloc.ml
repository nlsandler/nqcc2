open Batteries
open Assembly

module AsmCfg = struct
  include Cfg.AsmCfg
end

(* extract all operands from an instruction.
 * NOTE: don't need to include implicit operands (e.g. ax/dx for cdq)
 * because we only use this to find pseudos
 *)
let get_operands = function
  | Mov (_, src, dst) -> [ src; dst ]
  | Movsx i -> [ i.src; i.dst ]
  | MovZeroExtend zx -> [ zx.src; zx.dst ]
  | Lea (src, dst) -> [ src; dst ]
  | Cvttsd2si (_, src, dst) -> [ src; dst ]
  | Cvtsi2sd (_, src, dst) -> [ src; dst ]
  | Unary (_, _, op) -> [ op ]
  | Binary b -> [ b.src; b.dst ]
  | Cmp (_, v1, v2) -> [ v1; v2 ]
  | Idiv (_, op) -> [ op ]
  | Div (_, op) -> [ op ]
  | SetCC (_, op) -> [ op ]
  | Push op -> [ op ]
  | Label _ | Call _ | Ret | Cdq _ | JmpCC _ | Jmp _ -> []
  | Pop _ -> failwith "Internal error" [@coverage off]

let replace_ops f i =
  match i with
  | Mov (t, src, dst) -> Mov (t, f src, f dst)
  | Movsx sx -> Movsx { sx with dst = f sx.dst; src = f sx.src }
  | MovZeroExtend zx -> MovZeroExtend { zx with dst = f zx.dst; src = f zx.src }
  | Lea (src, dst) -> Lea (f src, f dst)
  | Cvttsd2si (t, src, dst) -> Cvttsd2si (t, f src, f dst)
  | Cvtsi2sd (t, src, dst) -> Cvtsi2sd (t, f src, f dst)
  | Unary (operator, t, operand) -> Unary (operator, t, f operand)
  | Binary b -> Binary { b with dst = f b.dst; src = f b.src }
  | Cmp (code, v1, v2) -> Cmp (code, f v1, f v2)
  | Idiv (t, v) -> Idiv (t, f v)
  | Div (t, v) -> Div (t, f v)
  | SetCC (code, dst) -> SetCC (code, f dst)
  | Push v -> Push (f v)
  | Label _ | Call _ | Ret | Cdq _ | Jmp _ | JmpCC _ -> i
  | Pop _ -> failwith "We shouldn't use this yet" [@coverage off]

let cleanup_movs instructions =
  let is_redundant_mov = function
    | Mov (_, src, dst) when src = dst -> true
    | _ -> false
  in
  List.filter (not % is_redundant_mov) instructions

module type REG_TYPE = sig
  val suffix : string
  val all_hardregs : Assembly.reg list
  val caller_saved_regs : Assembly.reg list
  val pseudo_is_current_type : string -> bool
  val debug_settings : unit -> Settings.regalloc_debug_options
end

module Allocator (R : REG_TYPE) = struct
  (* convenience function : convert set of regs to set of operands *)
  let regs_to_operands = Set.map (fun r -> Reg r)

  (* values derived from R *)
  let all_hardregs = R.all_hardregs |> Set.of_list |> regs_to_operands
  let caller_saved_regs = R.caller_saved_regs |> Set.of_list |> regs_to_operands

  (* Interference graph definition and helpers for manipulating graph *)
  type node_id = Assembly.operand

  type node = {
    id : node_id;
    mutable neighbors : node_id Set.t;
    spill_cost : Float.t;
    color : int option;
    pruned : bool;
  }

  type graph = (node_id, node) Map.t

  let show_node_id nd =
    let s =
      match nd with
      | Reg r -> show_reg r
      | Pseudo p -> p
      | _ ->
          failwith "Internal error: malformed interference graph"
          [@coverage off]
    in
    String.replace_chars (function '.' -> "_" | c -> String.of_char c) s

  let k = Set.cardinal all_hardregs

  let add_edge g nd_id1 nd_id2 =
    let nd1 = Map.find nd_id1 g in
    let nd2 = Map.find nd_id2 g in
    nd1.neighbors <- Set.add nd_id2 nd1.neighbors;
    nd2.neighbors <- Set.add nd_id1 nd2.neighbors

  (* utility function *)

  let regs_used_and_written i =
    let ops_used, ops_written =
      match i with
      | Mov (_, src, dst) -> ([ src ], [ dst ])
      | MovZeroExtend zx -> ([ zx.src ], [ zx.dst ])
      | Movsx sx -> ([ sx.src ], [ sx.dst ])
      | Cvtsi2sd (_, src, dst) -> ([ src ], [ dst ])
      | Cvttsd2si (_, src, dst) -> ([ src ], [ dst ])
      (* dst of binary or unary instruction is both read and written *)
      | Binary b -> ([ b.src; b.dst ], [ b.dst ])
      | Unary (_, _, op) -> ([ op ], [ op ])
      | Cmp (_, v1, v2) -> ([ v1; v2 ], [])
      | SetCC (_, op) -> ([], [ op ])
      | Push v -> ([ v ], [])
      | Idiv (_, op) -> ([ op; Reg AX; Reg DX ], [ Reg AX; Reg DX ])
      | Div (_, op) -> ([ op; Reg AX; Reg DX ], [ Reg AX; Reg DX ])
      | Cdq _ -> ([ Reg AX ], [ Reg DX ])
      | Call f ->
          (* function call updates caller-saved regs, uses param-passing registers *)
          let used =
            Assembly_symbols.param_regs_used f
            |> List.filter (fun r -> List.mem r R.all_hardregs)
            |> List.map (fun r -> Reg r)
          in
          (used, Set.to_list caller_saved_regs)
      (* if src is a pseudo, lea won't actually generate it,
       * but we've excluded it from the graph anyway
       * if it's a memory address or indexed operand, we _do_ want to generate
       * hardregs used in address calculations
       *)
      | Lea (src, dst) -> ([ src ], [ dst ])
      | Jmp _ | JmpCC _ | Label _ | Ret -> ([], [])
      | Pop _ -> failwith "Internal error" [@coverage off]
    in
    (* convert list of operands read into list of hard/pseudoregs read *)
    let regs_used_to_read opr =
      match opr with
      | Pseudo _ | Reg _ -> [ opr ]
      | Memory (r, _) -> [ Reg r ]
      | Indexed x -> [ Reg x.base; Reg x.index ]
      | Imm _ | Data _ | PseudoMem _ -> []
    in
    let regs_read1 = List.concat_map regs_used_to_read ops_used in
    (* now convert list of operands written into lists of hard/pseudoregs
     * read _or_ written, accounting for the fact that writing to a memory address
     * may require reading a pointer *)
    let regs_used_to_update opr =
      match opr with
      | Pseudo _ | Reg _ -> ([], [ opr ])
      | Memory (r, _) -> ([ Reg r ], [])
      | Indexed x -> ([ Reg x.base; Reg x.index ], [])
      | Imm _ | Data _ | PseudoMem _ -> ([], [])
    in
    let regs_read2, regs_written =
      List.map regs_used_to_update ops_written
      |> List.split
      |> Tuple2.mapn List.concat
    in
    (Set.of_list (regs_read1 @ regs_read2), Set.of_list regs_written)

  (* Functions to dump the interference graph *)
  module DumpGraph = struct
    (* common logic for printing in ncol and graphviz format *)
    let dump_helper ?start_graph ?end_graph file_ext edge_printer post_processor
        ctx g =
      let filename =
        Debug.mk_filename (R.suffix ^ ".interference") ctx file_ext
      in
      let path = Filename.concat (Sys.getcwd ()) filename in
      let chan = open_out path in
      let print_edges nd { neighbors; pruned; _ } =
        if pruned then ()
        else
          let print_edge nghbor_id =
            edge_printer chan (show_node_id nd) (show_node_id nghbor_id)
          in
          let _, later_neighbors = Set.split_le nd neighbors in
          (* todo avoid copypasta of not_pruned helper*)
          let not_pruned nd_id = not (Map.find nd_id g).pruned in
          let unpruned_later_neighbors =
            Set.filter not_pruned later_neighbors
          in
          Set.iter print_edge unpruned_later_neighbors
      in
      Option.may (Printf.fprintf chan "%s") start_graph;
      Map.iter print_edges g;
      Option.may (Printf.fprintf chan "%s") end_graph;
      close_out chan;
      post_processor filename

    let dump_graphviz ctx g =
      if (R.debug_settings ()).interference_graphviz && Debug.is_dump_target ctx
      then
        let start_graph = "graph {\n" in
        let end_graph = "\t}\n" in
        let edge_printer chan = Printf.fprintf chan "\t%s -- %s\n" in
        let post_processor filename =
          (* convert DOT file to png*)
          let cmd =
            Printf.sprintf "circo -Tpng %s -o %s" filename
              (Filename.chop_extension filename ^ ".png")
          in
          if Sys.command cmd <> 0 then failwith ("graphviz fail: " ^ cmd)
          else if Sys.command ("rm " ^ filename) <> 0 then
            failwith "failed to remove DOT file"
        in
        dump_helper ~start_graph ~end_graph ".dot" edge_printer post_processor
          ctx g

    let dump_ncol ctx g =
      if (R.debug_settings ()).interference_ncol && Debug.is_dump_target ctx
      then
        let edge_printer chan = Printf.fprintf chan "%s %s\n" in
        let post_processor _ = () in
        dump_helper ".ncol" edge_printer post_processor ctx g
  end

  module LivenessAnalysis = struct
    open AsmCfg
    module Iterative = Backward_dataflow.Dataflow (AsmCfg)

    let debug_print_cfg extra_tag cfg =
      if (R.debug_settings ()).liveness && Debug.is_dump_target AsmCfg.(cfg.ctx)
      then
        let livevar_printer fmt live_vars =
          Format.pp_open_box fmt 0;
          Format.pp_print_list
            ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
            Assembly.pp_operand fmt (Set.to_list live_vars);
          Format.pp_close_box fmt ()
        in
        let tag = "regalloc_liveness." ^ extra_tag in
        AsmCfg.(print_graphviz tag livevar_printer cfg)

    let meet cfg block =
      let live_at_exit =
        let all_return_regs =
          Assembly_symbols.return_regs_used AsmCfg.(cfg.ctx.fun_name)
          |> Set.of_list
          |> regs_to_operands
        in
        let return_regs = Set.intersect all_hardregs all_return_regs in
        return_regs
      in

      let update_live live = function
        | Entry ->
            failwith "Internal error: malformed interference graph"
            [@coverage off]
        | Exit -> Set.union live live_at_exit
        | Block n -> Set.union live (get_block_value n cfg)
      in
      List.fold update_live Set.empty block.succs

    let transfer block end_live_regs =
      let process_instr current_live_regs (_, i) =
        let annotated_instr = (current_live_regs, i) in
        let new_live_regs =
          let regs_used, regs_written = regs_used_and_written i in
          let without_killed = Set.diff current_live_regs regs_written in
          Set.union without_killed regs_used
        in
        (new_live_regs, annotated_instr)
      in
      let incoming_live_regs, annotated_reversed_instrutions =
        block.instructions
        |> List.rev
        |> List.fold_left_map process_instr end_live_regs
      in
      {
        block with
        instructions = List.rev annotated_reversed_instrutions;
        value = incoming_live_regs;
      }

    let analyze =
      Iterative.analyze (debug_print_cfg "in_progress") meet transfer
  end

  let mk_base_graph () =
    let add_node g r =
      Map.add r
        {
          id = r;
          neighbors = Set.remove r all_hardregs;
          spill_cost = Float.infinity;
          color = None;
          pruned = false;
        }
        g
    in
    List.fold add_node Map.empty (Set.to_list all_hardregs)

  let get_pseudo_nodes aliased_pseudos instructions =
    (* convert list of operands to list of pseudoregisters - note that we don't need to include hardregs because they're already in the base graph
       * and some operands e.g. constants, pseudos with static storage duration, and regs of the wrong type) should be excluded *)
    let operands_to_pseudos = function
      | Assembly.Pseudo r ->
          if
            R.pseudo_is_current_type r
            && not (Assembly_symbols.is_static r || Set.mem r aliased_pseudos)
          then Some r
          else None
      | _ -> None
    in
    let get_pseudos i = get_operands i |> List.filter_map operands_to_pseudos in
    let initialize_node pseudo =
      {
        id = Pseudo pseudo;
        neighbors = Set.empty;
        spill_cost = 0.0;
        color = None;
        pruned = false;
      }
    in
    List.concat_map get_pseudos instructions
    |> List.unique
    |> List.map initialize_node

  let add_pseudo_nodes aliased_pseudos graph instructions =
    let nds = get_pseudo_nodes aliased_pseudos instructions in
    let add_node g nd = Map.add nd.id nd g in
    List.fold add_node graph nds

  let add_edges liveness_cfg interference_graph =
    let handle_instr (live_after_instr, i) =
      let _, updated_regs = regs_used_and_written i in

      let handle_livereg l =
        match i with
        | Mov (_, src, _) when src = l -> ()
        | _ ->
            let handle_update u =
              if
                u <> l
                && Map.mem l interference_graph
                && Map.mem u interference_graph
              then add_edge interference_graph l u
              else ()
            in
            Set.iter handle_update updated_regs
      in
      Set.iter handle_livereg live_after_instr
    in

    let all_instructions =
      let open AsmCfg in
      List.concat_map
        (fun (_, blk) -> blk.instructions)
        liveness_cfg.basic_blocks
    in
    List.iter handle_instr all_instructions

  let build_interference_graph ctx aliased_pseudos instructions =
    let base_graph = mk_base_graph () in
    let graph = add_pseudo_nodes aliased_pseudos base_graph instructions in
    let cfg = AsmCfg.instructions_to_cfg ctx instructions in
    let liveness_cfg = LivenessAnalysis.analyze cfg in
    LivenessAnalysis.debug_print_cfg "annotated" liveness_cfg;
    add_edges liveness_cfg graph;
    graph

  let add_spill_costs graph instructions =
    (* given map from pseudo names to counts, incremene map entry for pseudo, or set to 1 if not already present  *)
    let incr_count (counts : (string, int) Map.t) pseudo =
      Map.modify_def 0 pseudo (( + ) 1) counts
    in
    (* get list of all operands in the function, filter out all but pseudoregs *)
    let operands = List.concat_map get_operands instructions in
    let get_pseudo = function Assembly.Pseudo r -> Some r | _ -> None in
    let pseudos = List.filter_map get_pseudo operands in
    (* create map from pseudoregs to counts - note that this may include pseudos that aren't in the interference graph, we'll ignore them *)
    let count_map = List.fold incr_count Map.empty pseudos in
    (* set each node's spill cost to the count from count_map *)
    let set_spill_cost nd =
      match nd.id with
      | Pseudo r -> { nd with spill_cost = Float.of_int (Map.find r count_map) }
      | _ -> nd
    in
    Map.map set_spill_cost graph

  let rec color_graph ctx graph =
    let remaining =
      graph
      |> Map.bindings
      |> List.split
      |> snd
      |> List.filter (fun nd -> not nd.pruned)
    in
    match remaining with
    | [] ->
        (* we've pruned the whole graph, so we're done *)
        graph
    | _ -> (
        let not_pruned nd_id = not (Map.find nd_id graph).pruned in
        (* find next node to prune *)
        let degree nd =
          let unpruned_neighbors = Set.filter not_pruned nd.neighbors in
          Set.cardinal unpruned_neighbors
        in
        let is_low_degree nd = degree nd < k in
        let next_node =
          try List.find is_low_degree remaining
          with Not_found ->
            (* Need to choose a spill candidat! *)

            (* choose next node by spill metric *)
            let spill_metric nd = nd.spill_cost /. Float.of_int (degree nd) in
            let cmp nd1 nd2 =
              Float.compare (spill_metric nd1) (spill_metric nd2)
            in
            let spilled = List.min ~cmp remaining in

            (* debug printing *)
            let spill_info_printer fmt =
              Printf.ksprintf
                (fun msg ->
                  if
                    (R.debug_settings ()).spill_info && Debug.is_dump_target ctx
                  then Printf.printf "%s" msg)
                fmt
            in
            spill_info_printer "================================\n";
            List.iter
              (fun nd ->
                spill_info_printer
                  "Node %s has degree %d, spill cost %f, and spill metric %f\n"
                  (show_node_id nd.id) (degree nd) nd.spill_cost
                  (spill_metric nd))
              (List.sort cmp remaining);

            spill_info_printer "Spill candidate: %s\n" (show_node_id spilled.id);
            (* return the spill candidate *)
            spilled
        in
        let pruned_graph =
          Map.modify next_node.id (fun nd -> { nd with pruned = true }) graph
        in
        let partly_colored = color_graph ctx pruned_graph in
        let all_colors = List.range 0 `To (k - 1) in
        let remove_neighbor_color neighbor_id remaining_colors =
          let neighbor_nd = Map.find neighbor_id partly_colored in
          match neighbor_nd.color with
          | Some c -> List.remove remaining_colors c
          | None -> remaining_colors
        in
        let available_colors =
          Set.fold remove_neighbor_color next_node.neighbors all_colors
        in
        match available_colors with
        (* no available colors, leave this node uncolored *)
        | [] -> partly_colored
        (* we found an available color! *)
        | _ :: _ ->
            (* If this is a callee-saved reg, give it the highest-numbered color; otherwise give it the lowest (implementation tip)*)
            let c =
              match next_node.id with
              | Reg r when not (List.mem r R.caller_saved_regs) ->
                  List.max available_colors
              | _ -> List.min available_colors
            in
            Map.modify next_node.id
              (fun nd -> { nd with pruned = false; color = Some c })
              partly_colored)

  let make_register_map ctx graph =
    (* first build map from colors to hardregs *)
    let add_color nd_id { color; _ } color_map =
      match nd_id with
      | Reg r -> Map.add (Option.get color) r color_map
      | _ -> color_map
    in
    let colors_to_regs = Map.foldi add_color graph Map.empty in

    (* then build map from pseudoregisters to hard registers *)
    let add_mapping nd (used_callee_saved, reg_map) =
      match nd with
      | { id = Pseudo p; color = Some c; _ } ->
          let hardreg = Map.find c colors_to_regs in
          let used_callee_saved =
            if List.mem hardreg R.caller_saved_regs then used_callee_saved
            else Set.add hardreg used_callee_saved
          in
          (used_callee_saved, Map.add p hardreg reg_map)
      | _ -> (used_callee_saved, reg_map)
    in
    let callee_saved_regs_used, reg_map =
      Map.fold add_mapping graph (Set.empty, Map.empty)
    in

    let fn_name = Context.(ctx.fun_name) in
    Assembly_symbols.add_callee_saved_regs_used fn_name callee_saved_regs_used;
    reg_map

  let replace_pseudoregs instructions reg_map =
    let f = function
      (* replace pseudoregister w/ corresponding hardreg. If operand isn't a pseudo or isn't colored, don't replace it*)
      | Assembly.Pseudo p as op -> (
          try Reg (Map.find p reg_map) with Not_found -> op)
      | op -> op
    in
    cleanup_movs (List.map (replace_ops f) instructions)

  let allocate ctx aliased_pseudos instructions =
    let graph : graph =
      build_interference_graph ctx aliased_pseudos instructions
    in
    DumpGraph.dump_graphviz ctx graph;
    DumpGraph.dump_ncol ctx graph;
    let graph_with_spill_costs = add_spill_costs graph instructions in
    let colored_graph = color_graph ctx graph_with_spill_costs in

    let register_map = make_register_map ctx colored_graph in
    replace_pseudoregs instructions register_map
end

module GP = Allocator (struct
  let suffix = "gp"
  let all_hardregs = [ AX; BX; CX; DX; DI; SI; R8; R9; R12; R13; R14; R15 ]
  let caller_saved_regs = [ AX; CX; DX; DI; SI; R8; R9 ]
  let pseudo_is_current_type p = Assembly_symbols.get_type p <> Double
  let debug_settings () = !Settings.debug.dump_gp_regalloc
end)

module XMM = Allocator (struct
  let suffix = "xmm"

  let all_hardregs =
    [
      XMM0;
      XMM1;
      XMM2;
      XMM3;
      XMM4;
      XMM5;
      XMM6;
      XMM7;
      XMM8;
      XMM9;
      XMM10;
      XMM11;
      XMM12;
      XMM13;
    ]

  let caller_saved_regs = all_hardregs
  let pseudo_is_current_type p = Assembly_symbols.get_type p = Double
  let debug_settings () = !Settings.debug.dump_xmm_regalloc
end)

let allocate_registers src_file aliased_pseudos (Program tls) =
  let allocate_regs_for_fun fn_name instructions =
    instructions
    |> GP.allocate fn_name aliased_pseudos
    |> XMM.allocate fn_name aliased_pseudos
  in
  let alloc_in_tl = function
    | Function f ->
        let ctx =
          Context.{ filename = src_file; fun_name = f.name; params = [] }
        in
        Function
          { f with instructions = allocate_regs_for_fun ctx f.instructions }
    | tl -> tl
  in
  Program (List.map alloc_in_tl tls)
