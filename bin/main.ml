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

let compile stage optimizations preprocessed_src =
  let _ = Compile.compile stage optimizations preprocessed_src in
  (* remove preprocessed src *)
  let cleanup_preprocessed = Printf.sprintf "rm %s" preprocessed_src in
  run_command cleanup_preprocessed;
  replace_extension preprocessed_src ".s"

let assemble_and_link ?(link = true) ?(cleanup = true) ?(libs = []) src =
  let link_option = if link then "" else "-c" in
  let lib_options = List.map (fun l -> "-l" ^ l) libs |> String.concat " " in
  let assembly_file = replace_extension src ".s" in
  let output_file =
    if link then Filename.chop_extension src else replace_extension src ".o"
  in
  let assemble_cmd =
    Printf.sprintf "gcc %s %s %s -o %s" link_option assembly_file lib_options
      output_file
  in
  let _ = run_command assemble_cmd in
  (* cleanup .s files *)
  if cleanup then
    let cleanup_cmd = Printf.sprintf "rm %s" assembly_file in
    run_command cleanup_cmd

let driver target debug libs stage optimizations src =
  let _ =
    Settings.platform := target;
    Settings.debug := debug
  in
  let preprocessed_name = preprocess src in
  let assembly_name = compile stage optimizations preprocessed_name in
  match stage with
  | Settings.Executable ->
      assemble_and_link ~link:true ~cleanup:(not debug.dump_asm) ~libs
        assembly_name
  | Settings.Obj ->
      assemble_and_link ~link:false ~cleanup:(not debug.dump_asm) assembly_name
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
  let dump_tacky_opt =
    let doc = "Dump TACKY at start and end of optimization phase." in
    Arg.(value & flag & info [ "dump-tacky" ] ~doc)
  in
  let dump_asm_opt =
    let doc = "Dump assembly before and after pseudoreg allocation" in
    Arg.(value & flag & info [ "dump-asm" ] ~doc)
  in
  let dump_const_fold_opt =
    let doc =
      "Dump TACKY immediately before and after each constant folding pass. \
       Ignored if this optimization is not enabled."
    in
    Arg.(value & flag & info [ "dump-const-fold" ] ~doc)
  in
  let dump_dse_opt =
    let doc =
      "Dump debug info (e.g. control flow graph) during dead store \
       elimination. Ignored if this optimization is not enabled."
    in
    Arg.(value & flag & info [ "dump-dse" ] ~doc)
  in
  let dump_copy_prop_opt =
    let doc =
      "Dump debug info (e.g. control flow graph) during copy pro. Ignored if \
       this optimization is not enabled."
    in
    Arg.(value & flag & info [ "dump-copy-prop" ] ~doc)
  in
  let dump_unreachable_elim_opt =
    let doc =
      "Dump debug info (e.g. control flow graph) during unreachable code elim. \
       Ignored if this optimization is not enabled."
    in
    Arg.(value & flag & info [ "dump-unreachable-elim" ] ~doc)
  in
  let dump_gp_regalloc_opt =
    let doc = "Verbosity level when allocating general-purpose registers" in
    Arg.(value & opt ~vopt:1 int 0 & info [ "dump-gp-regalloc" ] ~doc)
  in
  let dump_xmm_regalloc_opt =
    let doc = "Verbosity level when allocating XMM registers" in
    Arg.(value & opt ~vopt:1 int 0 & info [ "dump-xmm-regalloc" ] ~doc)
  in
  let dump_fun_opt =
    let doc =
      "Function to dump extra optimization details for (if not specified, dump \
       info for all functions). Affects optimization-specific --dump* options \
       but not --dump-tacky or --dump-asm."
    in
    Arg.(value & opt (some' string) None & info [ "dump-fun" ] ~doc)
  in
  let set_debug_options dump_asm dump_tacky constant_folding
      dead_store_elimination copy_propagation unreachable_code_elimination
      dump_gp_regalloc_lvl dump_xmm_regalloc_lvl dump_fun =
    let mk_regalloc_opts dump_lvl =
      Settings.
        {
          debug_msg = dump_lvl >= 1;
          interference_ncol = dump_lvl >= 2;
          interference_graphviz = dump_lvl >= 3;
          liveness = dump_lvl >= 4;
        }
    in
    Settings.
      {
        dump_asm;
        dump_tacky;
        dump_optimizations =
          {
            constant_folding;
            dead_store_elimination;
            unreachable_code_elimination;
            copy_propagation;
          };
        dump_gp_regalloc = mk_regalloc_opts dump_gp_regalloc_lvl;
        dump_xmm_regalloc = mk_regalloc_opts dump_xmm_regalloc_lvl;
        dump_fun;
      }
  in
  Cmdliner.Term.(
    const set_debug_options
    $ dump_asm_opt
    $ dump_tacky_opt
    $ dump_const_fold_opt
    $ dump_dse_opt
    $ dump_copy_prop_opt
    $ dump_unreachable_elim_opt
    $ dump_gp_regalloc_opt
    $ dump_xmm_regalloc_opt
    $ dump_fun_opt)

let src_file =
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"files")

(* optimization options *)
let optimization_options =
  let fold_constants =
    let doc = "Enable constant folding" in
    Arg.(value & flag & info [ "fold-constants" ] ~doc)
  in
  let eliminate_dead_stores =
    let doc = "Enable dead store elimination" in
    Arg.(value & flag & info [ "eliminate-dead-stores" ] ~doc)
  in
  let propagate_copies =
    let doc = "Enable copy-propagation" in
    Arg.(value & flag & info [ "propagate-copies" ] ~doc)
  in
  let eliminate_unreachable_code =
    let doc = "Enable unreachable code eliminaiton" in
    Arg.(value & flag & info [ "eliminate-unreachable-code" ] ~doc)
  in
  let optimize =
    let doc = "enable optimizations" in
    Arg.(value & flag & info [ "o"; "optimize" ] ~doc)
  in
  let set_options optimize constant_folding dead_store_elimination
      copy_propagation unreachable_code_elimination =
    if optimize then
      (* TODO maybe warn if both --optimize and any of the other options are set, since those options will be redundant? *)
      Settings.
        {
          constant_folding = true;
          dead_store_elimination = true;
          unreachable_code_elimination = true;
          copy_propagation = true;
        }
    else
      Settings.
        {
          constant_folding;
          dead_store_elimination;
          copy_propagation;
          unreachable_code_elimination;
        }
  in
  Cmdliner.Term.(
    const set_options
    $ optimize
    $ fold_constants
    $ eliminate_dead_stores
    $ propagate_copies
    $ eliminate_unreachable_code)

let cmd =
  let doc = "A not-quite-C compiler" in
  let info = Cmd.info "nqcc" ~doc in
  Cmd.v info
    Term.(
      const driver
      $ target
      $ debug
      $ libs
      $ stage
      $ optimization_options
      $ src_file)

let main () = exit (Cmd.eval cmd)
let () = main ()
