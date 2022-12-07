type stage = Lex | Parse | Validate | Tacky | Codegen | Assembly | Obj | Executable
type target = OS_X | Linux
type extra_credit = Bitwise | Compound | Increment | Goto | Switch | Nan | Union
type optimizations = {
  constant_folding : bool;
  dead_store_elimination : bool;
  unreachable_code_elimination : bool;
  copy_propagation : bool;
}

type regalloc_debug_options = {
  debug_msg : bool;
  interference_ncol : bool;
  interference_graphviz : bool;
  liveness : bool;
}

type debug_options = {
  (* dumping intermediate representations *)
  dump_tacky : bool;
  dump_asm : bool;
  (* dumping extra info about specific optimizations*)
  dump_optimizations : optimizations;
    (* dumping extra info during register allocation *)
    dump_gp_regalloc : regalloc_debug_options;
    dump_xmm_regalloc : regalloc_debug_options;
  (* If specified, we dump optimization info only for this function;
   * otherwise dump for all functions
   * doesn't impact dump_tacky/dump_asm, which always dump the whole program *)
  dump_fun : string option;
}
val platform : target ref
val extra_credit_flags : extra_credit list ref
val int_only: bool ref
val debug : debug_options ref
