let compile stage src_file =
  (* read in the file - TODO use streams? *)
  let ic = open_in src_file in
  let source = In_channel.input_all ic in
  close_in ic;
  (* Lex it *)
  let tokens = Lex.lex source in
  if stage = Settings.Lex then ()
  else
    let ast = Parse.parse tokens in
    if stage = Settings.Parse then ()
    else
      (* Convert the AST to TACKY *)
      let tacky = Tacky_gen.gen ast in
      (* print to file (src filename with .debug.tacky extension) if debug is enabled*)
      Tacky_print.debug_print_tacky src_file tacky;
      if stage = Settings.Tacky then ()
      else
        (* Assembly generation has three steps:
         * 1. convert TACKY to assembly *)
        let asm_ast = Codegen.gen tacky in
        (* print pre-pseudoreg-allocation assembly if debug enabled *)
        (if !Settings.debug then
           let prealloc_filename =
             Filename.chop_extension src_file ^ ".prealloc.debug.s"
           in
           Emit.emit prealloc_filename asm_ast);
        (* replace pseudoregisters with Stack operands *)
        let asm_ast1, stack_size = Replace_pseudos.replace_pseudos asm_ast in
        (* fix up instructions *)
        let asm_ast2 = Instruction_fixup.fixup_program stack_size asm_ast1 in
        if stage = Settings.Codegen then ()
        else
          let asm_filename = Filename.chop_extension src_file ^ ".s" in
          Emit.emit asm_filename asm_ast2
