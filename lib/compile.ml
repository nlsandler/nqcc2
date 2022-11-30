open Batteries

let compile target stage optimizations src_file =
  let _ = Settings.platform := target in
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
      (* 2. annotate loops and break/continue statements *)
      let annotated_ast = Label_loops.label_loops resolved_ast in
      (* 3. typecheck definitions and uses of functions adn variables *)
      let typed_ast = Typecheck.typecheck annotated_ast in
      if stage = Settings.Validate then ()
      else
        (* Convert the AST to TACKY *)
        let tacky = Tacky_gen.gen typed_ast in
        (* optimize it! *)
        let optimized_tacky = Optimize.optimize optimizations tacky in
        if stage = Settings.Tacky then ()
        else
          (* start by getting all aliased vars, we'll need them for register allocation *)
          let aliased_vars = Address_taken.analyze_program optimized_tacky in
          (* Assembly generation has three steps:
           * 1. convert TACKY to assembly *)
          let asm_ast = Codegen.gen optimized_tacky in
          let pre_asm_filename = Filename.chop_extension src_file ^ "_pre.s" in
          let _ = Emit.emit pre_asm_filename asm_ast in
          (* register allocation *)
          let asm_ast1 = Regalloc.allocate_registers aliased_vars asm_ast in
          (* replace remaining pseudoregisters with Data/Stack operands *)
          let asm_ast2 = Replace_pseudos.replace_pseudos asm_ast1 in
          (* fix up instructions *)
          let asm_ast3 = Instruction_fixup.fixup_program asm_ast2 in
          if stage = Settings.Codegen then ()
          else
            let asm_filename = Filename.chop_extension src_file ^ ".s" in
            Emit.emit asm_filename asm_ast3
