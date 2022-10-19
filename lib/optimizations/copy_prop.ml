module G = struct
  include Cfg.TackyCfg
end

module StringSet = Set.Make (String)

(* represent a reaching copy as pair of tacky vals *)
type cp = { src : Tacky.tacky_val; dst : Tacky.tacky_val } [@@deriving ord]

let pp_cp fmt copy =
  Format.fprintf fmt "%a = %a" Tacky_print.pp_tacky_val copy.dst
    Tacky_print.pp_tacky_val copy.src

(* be careful to always use our own eq/compare instead of built-in one b/c 0.0 and -0.0 compare equal, and NaN doesn't equal itself, using (=) *)
module ReachingCopies = Set.Make (struct
  type t = cp

  let compare = compare_cp
end)

let eq = Tacky.equal_tacky_val

let debug_print ?(extra_tag = "") cfg =
  if !Settings.debug then
    let copies_printer fmt copies =
      Format.pp_open_box fmt 0;
      Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
        pp_cp fmt
        (ReachingCopies.to_list copies);
      Format.pp_close_box fmt ()
    in
    let lbl = cfg.G.debug_label ^ "_copy_prop" ^ extra_tag in
    G.(print_graphviz copies_printer { cfg with debug_label = lbl })

(* check whether two operands of Copy are, for our purposes, the same type *)
let same_type v1 v2 =
  let t1, t2 = Tacky.(type_of_val v1, type_of_val v2) in
  t1 = t2 || Type_utils.is_signed t1 = Type_utils.is_signed t2

let var_is_aliased aliased_vars = function
  | Tacky.Constant _ -> false
  (* all static variables are potentially aliased *)
  | Var v when StringSet.mem v aliased_vars || Optimize_utils.is_static v ->
      true
  | _ -> false

let filter_updated copies updated =
  (* A copy is killed if we've updated its src or destination *)
  let is_killed cp = eq cp.src updated || eq cp.dst updated in
  ReachingCopies.filter (fun x -> not (is_killed x)) copies

let transfer aliased_vars (block : ReachingCopies.t G.basic_block)
    (initial_reaching_copies : ReachingCopies.t) =
  let is_aliased = var_is_aliased aliased_vars in
  let process_instr current_copies (_, i) =
    let annotated_instr = (current_copies, i) in
    let new_copies =
      match i with
      | Tacky.Copy { src; dst } ->
          if ReachingCopies.mem { src = dst; dst = src } current_copies then
            (* dst and src already have the same value, so there's no effect *)
            current_copies
          else if same_type src dst then
            filter_updated current_copies dst |> ReachingCopies.add { src; dst }
            (* if types are the same, kill dst but don't count this as a reaching copy *)
          else filter_updated current_copies dst
      | FunCall { dst; _ } ->
          (* first filter out copies killed by dst *)
          let copies' =
            match dst with
            | Some d -> filter_updated current_copies d
            | None -> current_copies
          in

          (* then filter out copies that are static *)
          ReachingCopies.filter
            (fun cp -> not (is_aliased cp.src || is_aliased cp.dst))
            copies'
      | Store _ ->
          let not_killed cp = not (is_aliased cp.src || is_aliased cp.dst) in
          ReachingCopies.filter not_killed current_copies
      | i -> (
          match Optimize_utils.get_dst i with
          | Some dst -> filter_updated current_copies dst
          | None -> current_copies)
    in
    (new_copies, annotated_instr)
  in
  let final_reaching_copies, annotated_instructions =
    List.fold_left_map process_instr initial_reaching_copies block.instructions
  in
  {
    block with
    instructions = annotated_instructions;
    value = final_reaching_copies;
  }

let meet ident cfg (block : ReachingCopies.t G.basic_block) =
  (* arbitrartily choose value *)
  let update_incoming incoming = function
    | G.Entry -> ReachingCopies.empty
    | Exit -> failwith "Internal error" [@coverage off]
    | Block n ->
        (* get current value for this block *)
        let v = G.get_block_value n cfg in
        ReachingCopies.inter v incoming
  in

  List.fold_left update_incoming ident block.preds

let collect_all_copies cfg =
  let f = function
    | Tacky.Copy { src; dst } when same_type src dst -> Some { src; dst }
    | _ -> None
  in
  cfg |> G.cfg_to_instructions |> List.filter_map f |> ReachingCopies.of_list

let find_reaching_copies aliased_vars cfg =
  let ident = collect_all_copies cfg in
  let starting_cfg = G.initialize_annotation cfg ident in

  let rec process_worklist current_cfg
      (worklist : (int * ReachingCopies.t G.basic_block) list) =
    debug_print ~extra_tag:"_in_progress_" current_cfg;
    match worklist with
    | [] -> current_cfg (* we're done *)
    | (block_idx, blk) :: rest ->
        let old_annotation = blk.value in
        let incoming_copies = meet ident current_cfg blk in
        let block' = transfer aliased_vars blk incoming_copies in
        let updated_cfg = G.update_basic_block block_idx block' current_cfg in
        let new_worklist =
          if ReachingCopies.equal old_annotation block'.value then rest
          else
            (* add successors to worklist *)
            List.fold_left
              (fun wklist -> function
                | G.Exit -> wklist
                | Entry ->
                    failwith "Internal error: malformed CFG" [@coverage off]
                | Block n ->
                    if List.mem_assoc n wklist then wklist
                    else wklist @ [ (n, List.assoc n updated_cfg.basic_blocks) ])
              rest block'.succs
        in
        process_worklist updated_cfg new_worklist
  in
  process_worklist starting_cfg starting_cfg.basic_blocks

let rewrite_instruction (reaching_copies, i) =
  (* filter out useless copies *)
  match i with
  | Tacky.Copy { src; dst }
    when ReachingCopies.mem { src; dst } reaching_copies
         || ReachingCopies.mem { src = dst; dst = src } reaching_copies ->
      None
  | _ ->
      (* we're not filtering it out, so replace src instead *)
      let replace op =
        match op with
        (* we never replace constants *)
        | Tacky.Constant _ -> op
        | Tacky.Var _ -> (
            try
              (* find th reaching copy whose destination is op *)
              let matching_copy =
                reaching_copies
                |> ReachingCopies.to_list
                |> List.find (fun cp -> eq cp.dst op)
              in
              matching_copy.src
              (* didn't find one, just return op itself *)
            with Not_found -> op)
      in

      let new_i =
        match i with
        | Tacky.Copy { src; dst } -> Tacky.Copy { src = replace src; dst }
        | Unary u -> Unary { u with src = replace u.src }
        | Binary b ->
            Binary { b with src1 = replace b.src1; src2 = replace b.src2 }
        | Return v -> Return (Option.map replace v)
        | JumpIfZero (v, target) -> JumpIfZero (replace v, target)
        | JumpIfNotZero (v, target) -> JumpIfNotZero (replace v, target)
        | FunCall f -> FunCall { f with args = List.map replace f.args }
        | SignExtend sx -> SignExtend { sx with src = replace sx.src }
        | ZeroExtend zx -> ZeroExtend { zx with src = replace zx.src }
        | DoubleToInt d2i -> DoubleToInt { d2i with src = replace d2i.src }
        | IntToDouble i2d -> IntToDouble { i2d with src = replace i2d.src }
        | DoubleToUInt d2u -> DoubleToUInt { d2u with src = replace d2u.src }
        | UIntToDouble u2d -> UIntToDouble { u2d with src = replace u2d.src }
        | Truncate t -> Truncate { t with src = replace t.src }
        | Load l -> Load { l with src_ptr = replace l.src_ptr }
        | Store s -> Store { s with src = replace s.src }
        | AddPtr ap ->
            AddPtr { ap with ptr = replace ap.ptr; index = replace ap.index }
        | CopyToOffset c2o -> CopyToOffset { c2o with src = replace c2o.src }
        | CopyFromOffset cfo -> (
            match replace (Var cfo.src) with
            | Var replaced -> CopyFromOffset { cfo with src = replaced }
            | Constant _ -> failwith "internal error" [@coverage off])
        | Label _ | Jump _ | GetAddress _ -> i
      in
      Some (reaching_copies, new_i)

let optimize aliased_vars (cfg : 'a Cfg.TackyCfg.t) : 'b Cfg.TackyCfg.t =
  let annotated_cfg = find_reaching_copies aliased_vars cfg in
  let _ = debug_print annotated_cfg in
  let rewrite_block (idx, block) =
    ( idx,
      G.
        {
          block with
          instructions = List.filter_map rewrite_instruction block.instructions;
        } )
  in
  let transformed_cfg =
    {
      annotated_cfg with
      basic_blocks = List.map rewrite_block annotated_cfg.basic_blocks;
    }
  in
  (* remove annotations since we no longer need them *)
  G.strip_annotations transformed_cfg
