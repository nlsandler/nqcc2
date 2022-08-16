open Cmdliner
open Nqcc

(* what platform are we on? *)
let current_platform =
  let uname_output =
    let ic = Unix.open_process_in "uname" in
    let result = In_channel.input_all ic in
    let _ = Unix.close_process_in ic in
    String.lowercase_ascii result
  in
  if String.starts_with ~prefix:"darwin" uname_output then Settings.OS_X
  else Settings.Linux

(* Utilities *)
let validate_extension filename =
  let ext = Filename.extension filename in
  if ext = ".c" || ext = ".h" then ()
  else failwith "Expected C source file with .c or .h extension"

let replace_extension filename new_extension =
  let base = Filename.chop_extension filename in
  base ^ new_extension

let run_command cmd args =
  if Sys.command (Filename.quote_command cmd args) <> 0 then
    failwith ("Command failed: " ^ cmd)

(* main driver *)
let preprocess src =
  let _ = validate_extension src in
  let output = replace_extension src ".i" in
  let _ = run_command "gcc" [ "-E"; "-P"; src; "-o"; output ] in
  output

let compile stage preprocessed_src =
  let _ = Compile.compile stage preprocessed_src in
  (* remove preprocessed src *)
  run_command "rm" [ preprocessed_src ];
  replace_extension preprocessed_src ".s"

let assemble_and_link ?(link = true) ?(cleanup = true) ?(libs = []) src =
  let link_option = if link then [] else [ "-c" ] in
  let lib_options = List.map (fun l -> "-l" ^ l) libs in
  let assembly_file = replace_extension src ".s" in
  let output_file =
    if link then Filename.chop_extension src else replace_extension src ".o"
  in
  let _ =
    run_command "gcc"
      (link_option @ [ assembly_file ] @ lib_options @ [ "-o"; output_file ])
  in
  (* cleanup .s files *)
  if cleanup then run_command "rm" [ assembly_file ]

let driver target debug libs stage src =
  let _ =
    Settings.platform := target;
    Settings.debug := debug
  in
  let preprocessed_name = preprocess src in
  let assembly_name = compile stage preprocessed_name in
  match stage with
  | Settings.Executable ->
      assemble_and_link ~link:true ~cleanup:(not debug) ~libs assembly_name
  | Settings.Obj ->
      assemble_and_link ~link:false ~cleanup:(not debug) assembly_name
  | _ -> ()

(* Command-line options *)
let stage =
  let lex =
    let doc = "Run the lexer" in
    (Settings.Lex, Arg.info [ "lex" ] ~doc)
  in
  let parse =
    let doc = "Run the lexer and parser" in
    (Settings.Parse, Arg.info [ "parse" ] ~doc)
  in
  let validate =
    let doc = "Run the lexer, parser, and semantic analysis " in
    (Settings.Validate, Arg.info [ "validate" ] ~doc)
  in
  let tacky =
    let doc = "Run the lexer, parser, semantic analysis, and tacky generator" in
    (Settings.Tacky, Arg.info [ "tacky" ] ~doc)
  in
  let codegen =
    let doc = "Run through code generation but stop before emitting assembly" in
    (Settings.Codegen, Arg.info [ "codegen" ] ~doc)
  in
  let assembly =
    let doc = "Stop before assembling (keep .s file)" in
    (Settings.Assembly, Arg.info [ "s"; "S" ] ~doc)
  in
  let obj =
    let doc = "Stop before invoking linker (keep .o file)" in
    (Settings.Obj, Arg.info [ "c" ] ~doc)
  in
  Arg.(
    value
    & vflag Settings.Executable
        [ lex; parse; validate; tacky; obj; codegen; assembly ])

let libs =
  let doc = "Link against library (passed through to assemble/link command)" in
  Arg.(value & opt_all Arg.string [] & info [ "l" ] ~doc)

let target =
  let doc = "Choose target platform" in
  let target = Arg.enum [ ("linux", Settings.Linux); ("osx", Settings.OS_X) ] in
  Arg.(value & opt target current_platform & info [ "t"; "target" ] ~doc)

let debug =
  let doc =
    "Write out pre- and post-register-allocation assembly and DOT files of \
     interference graphs."
  in
  Arg.(value & flag & info [ "d" ] ~doc)

let src_file =
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"files")

let cmd =
  let doc = "A not-quite-C compiler" in
  let info = Cmd.info "nqcc" ~doc in
  Cmd.v info Term.(const driver $ target $ debug $ libs $ stage $ src_file)

let main () = exit (Cmd.eval cmd)
let () = main ()
