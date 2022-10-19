open Batteries

let compile stage optimizations src_file =
  (* read in the file - TODO use streams? *)
  let source_lines = File.lines_of src_file in
  (* concatenate all the lines *)
  let source =
    Enum.reduce (fun line1 line2 -> line1 ^ "\n" ^ line2) source_lines
  in
  (* Lex it *)
  let tokens = Lex.lex source in
  if stage = Settings.Lex then ()
  else
    let ast = Parse.parse tokens in
    if stage = Settings.Parse then ()
    else
      (* Semantic analysis has three steps:
       * 1. resolve identifiers *)
      let resolved_ast = Identifier_resolution.resolve ast in
      (* 2. validate labels and goto statements *)
      let validated_ast = Validate_labels.validate_labels resolved_ast in

      (* 3. annotate loops and break/continue statements *)
      let validated_ast2 = Label_loops.label_loops validated_ast in

      (* 4. typecheck definitions and uses of functions and variables *)
      let typed_ast = Typecheck.typecheck validated_ast2 in
      (* 5. Collect cases in switch statements (need to type check first) *)
      let validated_ast3 = Collect_switch_cases.analyze_switches typed_ast in
      if stage = Settings.Validate then ()
      else
        (* Convert the AST to TACKY *)
        let tacky = Tacky_gen.gen validated_ast3 in
        (* print to file (src filename with .debug.tacky extension) if debug is enabled*)
        Tacky_print.debug_print_tacky ~tag:"pre_opt" ~file:src_file tacky;
        (* optimize it! *)
        let optimized_tacky = Optimize.optimize optimizations src_file tacky in
        Tacky_print.debug_print_tacky ~tag:"post_opt" ~file:src_file
          optimized_tacky;
        if stage = Settings.Tacky then ()
        else
          (* Assembly generation has three steps:
           * 1. convert TACKY to assembly *)
          let asm_ast = Codegen.gen optimized_tacky in
          (* print pre-pseudoreg-allocation assembly if debug enabled *)
          (if !Settings.debug.dump_asm then
             let prealloc_filename =
               Filename.chop_extension src_file ^ ".prealloc.debug.s"
             in
             Emit.emit prealloc_filename asm_ast);
          (* replace pseudoregisters with Stack operands *)
          let asm_ast1 = Replace_pseudos.replace_pseudos asm_ast in
          (* fix up instructions *)
          let asm_ast2 = Instruction_fixup.fixup_program asm_ast1 in
          if stage = Settings.Codegen then ()
          else
            let asm_filename = Filename.chop_extension src_file ^ ".s" in
            Emit.emit asm_filename asm_ast2
