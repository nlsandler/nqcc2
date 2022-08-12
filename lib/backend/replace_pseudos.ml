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
      match StringMap.find_opt s state.offset_map with
      (* We've already assigned this operand a stack slot *)
      | Some offset -> (state, Stack offset)
      (* We haven't already assigned it a stack slot;
       * assign it and update state *)
      | None ->
          let new_offset = state.current_offset - 4 in
          let new_state =
            {
              current_offset = new_offset;
              offset_map = StringMap.add s new_offset state.offset_map;
            }
          in
          (new_state, Stack new_offset))
  (* not a pseudo, so nothing to do *)
  | other -> (state, other)

let replace_pseudos_in_instruction state = function
  (* Replace src and dst of mov instruction *)
  | Mov (src, dst) ->
      let state1, new_src = replace_operand state src in
      let state2, new_dst = replace_operand state1 dst in
      let new_mov = Mov (new_src, new_dst) in
      (state2, new_mov)
      (* Replace dst of unary instruction *)
  | Unary (op, dst) ->
      let state1, new_dst = replace_operand state dst in
      let new_unary = Unary (op, new_dst) in
      (state1, new_unary)
  | Binary { op; src; dst } ->
      let state1, new_src = replace_operand state src in
      let state2, new_dst = replace_operand state1 dst in
      let new_binary = Binary { op; src = new_src; dst = new_dst } in
      (state2, new_binary)
  | Cmp (op1, op2) ->
      let state1, new_op1 = replace_operand state op1 in
      let state2, new_op2 = replace_operand state1 op2 in
      let new_cmp = Cmp (new_op1, new_op2) in
      (state2, new_cmp)
  | Idiv op ->
      let state1, new_op = replace_operand state op in
      (state1, Idiv new_op)
  (* Ret instruction has no operands, doesn't need to be rewritten *)
  | SetCC (code, op) ->
      let state1, new_op = replace_operand state op in
      (state1, SetCC (code, new_op))
  | (Ret | Cdq | Label _ | JmpCC _ | Jmp _) as other -> (state, other)
  | AllocateStack _ ->
      failwith
        "Internal error: AllocateStack shouldn't be present at this point"
      [@coverage off]

let replace_pseudos_in_function (Function { name; instructions }) =
  let init_state = { current_offset = 0; offset_map = StringMap.empty } in
  let final_state, fixed_instructions =
    (* rewrite each instruction, tracking current offset/stack slot map as we go *)
    List.fold_left_map replace_pseudos_in_instruction init_state instructions
  in
  ( Function { name; instructions = fixed_instructions },
    final_state.current_offset )

let replace_pseudos (Program fn_def) =
  let fixed_def, last_stack_slot = replace_pseudos_in_function fn_def in
  (Program fixed_def, last_stack_slot)
