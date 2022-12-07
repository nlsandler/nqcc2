open Batteries
open Assembly

module AsmCfg = struct
  include Cfg.AsmCfg
end

let debug_print fmt =
  Printf.ksprintf
    (fun msg -> if !Settings.debug then Printf.printf "%s" msg)
    fmt

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

(* map functon f over all the operands in an instruction *)
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
  | Pop _ -> failwith "Shouldn't use this yet" [@coverage off]

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
end

module Allocator (R : REG_TYPE) = struct
  (* convenience function : convert set of regs to set of operands *)
  let regs_to_operands = Set.map (fun r -> Reg r)

  (* values dervied from R *)
  let all_hardregs = R.all_hardregs |> Set.of_list |> regs_to_operands
  let caller_saved_regs = R.caller_saved_regs |> Set.of_list |> regs_to_operands

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

  (* for debugging *)
  let _print_graphviz fn_name g =
    if !Settings.debug then (
      let filename = fn_name ^ "_" ^ R.suffix ^ ".dot" in
      let path = Filename.concat (Sys.getcwd ()) filename in
      let chan = open_out path in
      let print_edges nd { neighbors; pruned; _ } =
        if pruned then ()
        else
          let print_edge nghbor_id =
            Printf.fprintf chan "\t%s -- %s\n" (show_node_id nd)
              (show_node_id nghbor_id)
          in
          let _, later_neighbors = Set.split_le nd neighbors in
          (* todo avoid copypasta of not_pruned helper*)
          let not_pruned nd_id = not (Map.find nd_id g).pruned in
          let unpruned_later_neighbors =
            Set.filter not_pruned later_neighbors
          in
          Set.iter print_edge unpruned_later_neighbors
      in
      Printf.fprintf chan "graph {\n";
      Map.iter print_edges g;
      Printf.fprintf chan "\t}\n";
      close_out chan)
  [@@coverage off]

  let k = Set.cardinal all_hardregs
  let get_node_by_id graph node_id = Map.find node_id graph

  let add_edge g nd_id1 nd_id2 =
    let nd1 = Map.find nd_id1 g in
    let nd2 = Map.find nd_id2 g in
    nd1.neighbors <- Set.add nd_id2 nd1.neighbors;
    nd2.neighbors <- Set.add nd_id1 nd2.neighbors

  let remove_edge g nd_id1 nd_id2 =
    let nd1, nd2 = (get_node_by_id g nd_id1, get_node_by_id g nd_id2) in
    nd1.neighbors <- Set.remove nd_id2 nd1.neighbors;
    nd2.neighbors <- Set.remove nd_id1 nd2.neighbors

  let degree graph nd_id =
    let nd = get_node_by_id graph nd_id in
    Set.cardinal nd.neighbors

  let are_neighbors g nd_id1 nd_id2 =
    let nd1 = Map.find nd_id1 g in
    Set.mem nd_id2 nd1.neighbors

  module LivenessAnalysis = struct
    open AsmCfg
    module Iterative = Backward_dataflow.Dataflow (AsmCfg)

    let meet fn_name cfg block =
      let live_at_exit =
        let all_return_regs =
          Assembly_symbols.return_regs_used fn_name
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

    let analyze fn_name = Iterative.analyze pp_operand (meet fn_name) transfer
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

  let build_interference_graph fn_name aliased_pseudos instructions =
    let base_graph = mk_base_graph () in
    let graph = add_pseudo_nodes aliased_pseudos base_graph instructions in
    let cfg = AsmCfg.instructions_to_cfg fn_name instructions in
    let liveness_cfg = LivenessAnalysis.analyze fn_name cfg in
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

  let george_test graph ~hardreg ~pseudo =
    let pseudoreg_neighbors = (get_node_by_id graph pseudo).neighbors in
    let neighbor_is_ok neighbor_id =
      (* a neighbor of the pseudo won't interfere with coalescing
       * if it has insignificant degree or it already interferes with hardreg *)
      are_neighbors graph neighbor_id hardreg || degree graph neighbor_id < k
    in
    Set.for_all neighbor_is_ok pseudoreg_neighbors

  let briggs_test graph x y =
    let x_nd = get_node_by_id graph x in
    let y_nd = get_node_by_id graph y in
    let neighbors = Set.union x_nd.neighbors y_nd.neighbors in
    let has_significant_degree neighbor_id =
      let deg = degree graph neighbor_id in
      let adjusted_deg =
        if
          are_neighbors graph x neighbor_id && are_neighbors graph y neighbor_id
        then deg - 1
        else deg
      in
      adjusted_deg >= k
    in
    let count_significant neighbor cnt =
      if has_significant_degree neighbor then cnt + 1 else cnt
    in
    let significant_neighbor_count = Set.fold count_significant neighbors 0 in
    significant_neighbor_count < k

  let conservative_coalescable graph src dst =
    if briggs_test graph src dst then true
    else
      match (src, dst) with
      | Reg _, _ -> george_test graph ~hardreg:src ~pseudo:dst
      | _, Reg _ -> george_test graph ~hardreg:dst ~pseudo:src
      | _ -> false

  let update_graph g ~to_merge ~to_keep =
    let update_neighbor neighbor_id =
      add_edge g neighbor_id to_keep;
      remove_edge g neighbor_id to_merge
    in
    Set.iter update_neighbor (get_node_by_id g to_merge).neighbors;
    Map.remove to_merge g

  let coalesce graph instructions =
    let process_instr (g, reg_map) = function
      | Mov (_, src, dst) ->
          let src' = Disjoint_sets.find src reg_map in
          let dst' = Disjoint_sets.find dst reg_map in
          if
            Map.mem src' g
            && Map.mem dst' g
            && src' <> dst'
            && (not (are_neighbors g src' dst'))
            && conservative_coalescable g src' dst'
          then
            match src' with
            | Reg _ ->
                ( update_graph g ~to_merge:dst' ~to_keep:src',
                  Disjoint_sets.union dst' src' reg_map )
            | _ ->
                ( update_graph g ~to_merge:src' ~to_keep:dst',
                  Disjoint_sets.union src' dst' reg_map )
          else (g, reg_map)
      | _ -> (g, reg_map)
    in
    let _updated_graph, new_instructions =
      List.fold process_instr (graph, Disjoint_sets.init) instructions
    in
    new_instructions

  let rewrite_coalesced instructions coalesced_regs =
    let f r = Disjoint_sets.find r coalesced_regs in
    let rewrite_instruction = function
      | Mov (t, src, dst) ->
          let new_src = f src in
          let new_dst = f dst in
          if new_src = new_dst then None else Some (Mov (t, new_src, new_dst))
      | i -> Some (replace_ops f i)
    in
    List.filter_map rewrite_instruction instructions

  let rec color_graph graph =
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
            (* choose next node by spill metric *)
            let spill_metric nd = nd.spill_cost /. Float.of_int (degree nd) in
            let cmp nd1 nd2 =
              Float.compare (spill_metric nd1) (spill_metric nd2)
            in

            let print_spill_info nd =
              debug_print "Node %s has degree %d and spill cost %f\n"
                (show_node_id nd.id) (degree nd) nd.spill_cost
            in
            debug_print "================================\n";
            List.iter print_spill_info remaining;

            let spilled = List.min ~cmp remaining in
            debug_print "Spill candidate: %s\n" (show_node_id spilled.id);
            spilled
        in
        let pruned_graph =
          Map.modify next_node.id (fun nd -> { nd with pruned = true }) graph
        in
        let partly_colored = color_graph pruned_graph in
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

  let make_register_map fn_name graph =
    let _graph = Array.of_enum (Map.values graph) in
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

  let allocate fn_name aliased_pseudos instructions =
    let rec coalesce_loop current_instructions =
      let graph : graph =
        build_interference_graph fn_name aliased_pseudos current_instructions
      in
      let coalesced_regs = coalesce graph current_instructions in
      if Disjoint_sets.is_empty coalesced_regs then (graph, current_instructions)
      else
        let new_instructions =
          rewrite_coalesced current_instructions coalesced_regs
        in
        coalesce_loop new_instructions
    in
    let coalesced_graph, coalesced_instructions = coalesce_loop instructions in
    let graph_with_spill_costs =
      add_spill_costs coalesced_graph coalesced_instructions
    in
    let colored_graph = color_graph graph_with_spill_costs in

    let register_map = make_register_map fn_name colored_graph in
    replace_pseudoregs coalesced_instructions register_map
end

module GP = Allocator (struct
  let suffix = "gp"
  let all_hardregs = [ AX; BX; CX; DX; DI; SI; R8; R9; R12; R13; R14; R15 ]
  let caller_saved_regs = [ AX; CX; DX; DI; SI; R8; R9 ]
  let pseudo_is_current_type p = Assembly_symbols.get_type p <> Double
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
end)

let allocate_registers aliased_pseudos (Program tls) =
  let allocate_regs_for_fun fn_name instructions =
    instructions
    |> GP.allocate fn_name aliased_pseudos
    |> XMM.allocate fn_name aliased_pseudos
  in
  let alloc_in_tl = function
    | Function f ->
        Function
          { f with instructions = allocate_regs_for_fun f.name f.instructions }
    | tl -> tl
  in
  Program (List.map alloc_in_tl tls)
