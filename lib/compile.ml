open Batteries

let compile target stage src_file =
  let _ = Settings.platform := target in
  (* read in the file - TODO use streams? *)
  let source_lines = File.lines_of src_file in
  (* concatenate all the lines *)
  let source =
    Enum.reduce (fun line1 line2 -> line1 ^ " " ^ line2) source_lines
  in
  (* Lex it *)
  let tokens = Lex.lex source in
  if stage = Settings.Lex then ()
  else
    let ast = Parse.parse tokens in
    if stage = Settings.Parse then ()
    else
      (* Semantic analysis has two steps:
       * 1. resolve variables *)
      let resolved_ast = Var_resolution.resolve ast in
      (* 2. annotate loops and break/continue statements *)
      let validated_ast = Label_loops.label_loops resolved_ast in
      if stage = Settings.Validate then ()
      else
        (* Convert the AST to TACKY *)
        let tacky = Tacky_gen.gen validated_ast in
        if stage = Settings.Tacky then ()
        else
          (* Assembly generation has three steps:
           * 1. convert TACKY to assembly *)
          let asm_ast = Codegen.gen tacky in
          (* replace pseudoregisters with Stack operands *)
          let asm_ast1, stack_size = Replace_pseudos.replace_pseudos asm_ast in
          (* fix up instructions *)
          let asm_ast2 = Instruction_fixup.fixup_program stack_size asm_ast1 in
          if stage = Settings.Codegen then ()
          else
            let asm_filename = Filename.chop_extension src_file ^ ".s" in
            Emit.emit asm_filename asm_ast2
