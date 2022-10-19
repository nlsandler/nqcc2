let compile stage optimizations src_file =
  (* read in the file - TODO use streams? *)
  let source = In_channel.with_open_text src_file In_channel.input_all in
  (* Lex it *)
  let tokens = Lex.lex source in
  if stage = Settings.Lex then ()
  else
    let ast = Parse.parse tokens in
    if stage = Settings.Parse then Format.printf "%a" Ast.Untyped.pp ast
    else
      (* Semantic analysis has three steps:
       * 1. resolve identifiers *)
      let resolved_ast = Resolve.resolve ast in
      (* 2. annotate loops and break/continue statements *)
      let annotated_ast = Label_loops.label_loops resolved_ast in
      (* 3. typecheck definitions and uses of functions adn variables *)
      let typed_ast = Typecheck.typecheck annotated_ast in
      if stage = Settings.Validate then ()
      else
        (* Convert the AST to TACKY *)
        let tacky = Tacky_gen.gen typed_ast in
        (* print to file (src filename with .debug.tacky extension) if debug is
           enabled *)
        Tacky_print.debug_print_tacky src_file tacky;
        (* optimize it! *)
        let optimized_tacky = Optimize.optimize optimizations src_file tacky in
        if stage = Settings.Tacky then ()
        else
          (* Assembly generation has three steps:
           * 1. convert TACKY to assembly *)
          let asm_ast = Codegen.gen optimized_tacky in
          (* print pre-pseudoreg-allocation assembly if debug enabled *)
          (if !Settings.debug then
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
