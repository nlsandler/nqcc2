open Assembly
module StringMap = Map.Make (String)

(* Structure to keep track of what stack slots we've assigned so far *)
type replacement_state = {
  current_offset : int; (* last used stack slot *)
  offset_map : int StringMap.t; (* map from pseudoregister to stack slots *)
}

let replace_operand state = function
  (* if it's a pseudoregister, replace it with a stack slot *)
  | Pseudo s -> (
      if Assembly_symbols.is_static s then (state, Data s)
      else
        match StringMap.find_opt s state.offset_map with
        (* We've already assigned this operand a stack slot *)
        | Some offset -> (state, Memory (BP, offset))
        (* We haven't already assigned it a stack slot;
         * assign it and update state *)
        | None ->
            let size = Assembly_symbols.get_size s in
            let alignment = Assembly_symbols.get_alignment s in
            let new_offset =
              Rounding.round_away_from_zero alignment
                (state.current_offset - size)
            in
            let new_state =
              {
                current_offset = new_offset;
                offset_map = StringMap.add s new_offset state.offset_map;
              }
            in
            (new_state, Memory (BP, new_offset)))
  (* not a pseudo, so nothing to do *)
  | other -> (state, other)

let replace_pseudos_in_instruction state = function
  (* Replace src and dst of mov instruction *)
  | Mov (t, src, dst) ->
      let state1, new_src = replace_operand state src in
      let state2, new_dst = replace_operand state1 dst in
      let new_mov = Mov (t, new_src, new_dst) in
      (state2, new_mov)
  | Movsx (src, dst) ->
      let state1, new_src = replace_operand state src in
      let state2, new_dst = replace_operand state1 dst in
      let new_movsx = Movsx (new_src, new_dst) in
      (state2, new_movsx)
  | MovZeroExtend (src, dst) ->
      let state1, new_src = replace_operand state src in
      let state2, new_dst = replace_operand state1 dst in
      let new_movzx = MovZeroExtend (new_src, new_dst) in
      (state2, new_movzx)
  | Lea (src, dst) ->
      let state1, new_src = replace_operand state src in
      let state2, new_dst = replace_operand state1 dst in
      let new_lea = Lea (new_src, new_dst) in
      (state2, new_lea)
      (* Replace dst of unary instruction *)
  | Unary (t, op, dst) ->
      let state1, new_dst = replace_operand state dst in
      let new_unary = Unary (t, op, new_dst) in
      (state1, new_unary)
  | Binary { op; t; src; dst } ->
      let state1, new_src = replace_operand state src in
      let state2, new_dst = replace_operand state1 dst in
      let new_binary = Binary { op; t; src = new_src; dst = new_dst } in
      (state2, new_binary)
  | Cmp (t, op1, op2) ->
      let state1, new_op1 = replace_operand state op1 in
      let state2, new_op2 = replace_operand state1 op2 in
      let new_cmp = Cmp (t, new_op1, new_op2) in
      (state2, new_cmp)
  | Idiv (t, op) ->
      let state1, new_op = replace_operand state op in
      (state1, Idiv (t, new_op))
  | Div (t, op) ->
      let state1, new_op = replace_operand state op in
      (state1, Div (t, new_op))
  (* Ret instruction has no operands, doesn't need to be rewritten *)
  | SetCC (code, op) ->
      let state1, new_op = replace_operand state op in
      (state1, SetCC (code, new_op))
  | Push op ->
      let state1, new_op = replace_operand state op in
      (state1, Push new_op)
  | Cvttsd2si (t, src, dst) ->
      let state1, new_src = replace_operand state src in
      let state2, new_dst = replace_operand state1 dst in
      let new_cvt = Cvttsd2si (t, new_src, new_dst) in
      (state2, new_cvt)
  | Cvtsi2sd (t, src, dst) ->
      let state1, new_src = replace_operand state src in
      let state2, new_dst = replace_operand state1 dst in
      let new_cvt = Cvtsi2sd (t, new_src, new_dst) in
      (state2, new_cvt)
  | (Ret | Cdq _ | Label _ | JmpCC _ | Jmp _ | Call _) as other -> (state, other)

let replace_pseudos_in_tl = function
  | Function { name; global; instructions } ->
      let init_state = { current_offset = 0; offset_map = StringMap.empty } in
      let final_state, fixed_instructions =
        (* rewrite each instruction, tracking current offset/stack slot map as
           we go *)
        List.fold_left_map replace_pseudos_in_instruction init_state
          instructions
      in
      Assembly_symbols.set_bytes_required name final_state.current_offset;
      Function { name; global; instructions = fixed_instructions }
  | static_var -> static_var

let replace_pseudos (Program tls) =
  let fixed_defs = List.map replace_pseudos_in_tl tls in
  Program fixed_defs
