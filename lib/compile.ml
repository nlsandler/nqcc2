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
      let asm_ast = Codegen.gen ast in
      if stage = Settings.Codegen then ()
      else
        let asm_filename = Filename.chop_extension src_file ^ ".s" in
        Emit.emit asm_filename asm_ast
