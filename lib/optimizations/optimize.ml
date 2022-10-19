open Batteries
open Cfg
(* main optimization pipeline *)

let rec optimize_fun ctx (opts : Settings.optimizations) = function
  | [] -> []
  | instructions ->
      let aliased_vars = Address_taken.analyze instructions in
      (* run the optimization pipeline *)
      let constant_folded =
        if opts.constant_folding then Constant_folding.optimize ctx instructions
        else instructions
      in
      let cfg = TackyCfg.instructions_to_cfg ctx constant_folded in
      let cfg1 =
        if opts.unreachable_code_elimination then
          Unreachable_code_elim.optimize cfg
        else cfg
      in
      let cfg2 =
        if opts.copy_propagation then Copy_prop.optimize aliased_vars cfg1
        else cfg1
      in
      let cfg3 =
        if opts.dead_store_elimination then
          Dead_store_elim.optimize aliased_vars cfg2
        else cfg2
      in
      (* once we're done optimization, convert back to instruction list *)
      let optimized_instructions = TackyCfg.cfg_to_instructions cfg3 in

      if List.equal Tacky.equal_instruction instructions optimized_instructions
      then optimized_instructions
        (* If we found any optimizations, run it again! *)
      else optimize_fun ctx opts optimized_instructions

let optimize opts src_file tacky_program =
  let open Tacky in
  let (Program tls) = tacky_program in
  let handle_tl = function
    | Function f ->
        let ctx =
          Context.{ filename = src_file; fun_name = f.name; params = f.params }
        in
        Function { f with body = optimize_fun ctx opts f.body }
    | other_tl -> other_tl
  in
  Program (List.map handle_tl tls)
