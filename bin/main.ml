open Cmdliner
open Nqcc

(* what platform are we on? *)
let current_platform =
  let uname_output =
    String.lowercase_ascii (snd (Batteries.Unix.run_and_read "uname"))
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

let run_command cmd =
  if Sys.command cmd <> 0 then failwith ("Command failed: " ^ cmd)

(* main driver *)
let preprocess src =
  let _ = validate_extension src in
  let output = replace_extension src ".i" in
  let preprocess_cmd = Printf.sprintf "gcc -E -P %s -o %s" src output in
  let _ = run_command preprocess_cmd in
  output

let compile stage preprocessed_src =
  let _ = Compile.compile stage preprocessed_src in
  (* remove preprocessed src *)
  let cleanup_preprocessed = Printf.sprintf "rm %s" preprocessed_src in
  run_command cleanup_preprocessed;
  replace_extension preprocessed_src ".s"

let assemble_and_link ?(cleanup = true) src =
  let assembly_file = replace_extension src ".s" in
  let output_file = Filename.chop_extension src in
  let assemble_cmd = Printf.sprintf "gcc %s -o %s" assembly_file output_file in
  let _ = run_command assemble_cmd in
  (* cleanup .s files *)
  if cleanup then
    let cleanup_cmd = Printf.sprintf "rm %s" assembly_file in
    run_command cleanup_cmd

let driver target debug stage src =
  let _ = Settings.platform := target in
  let preprocessed_name = preprocess src in
  let assembly_name = compile stage preprocessed_name in
  if stage = Settings.Executable then
    assemble_and_link ~cleanup:(not debug) assembly_name
  else ()

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
  let codegen =
    let doc = "Run through code generation but stop before emitting assembly" in
    (Settings.Codegen, Arg.info [ "codegen" ] ~doc)
  in
  let assembly =
    let doc = "Stop before assembling (keep .s file)" in
    (Settings.Assembly, Arg.info [ "s"; "S" ] ~doc)
  in
  Arg.(value & vflag Settings.Executable [ lex; parse; codegen; assembly ])

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
  Cmd.v info Term.(const driver $ target $ debug $ stage $ src_file)

let main () = exit (Cmd.eval cmd)
let () = main ()
