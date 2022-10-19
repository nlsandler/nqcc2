type stage =
  | Lex
  | Parse
  | Validate
  | Tacky
  | Codegen
  | Assembly
  | Obj
  | Executable

type target = OS_X | Linux

let platform = ref OS_X (* default to OS_X *)
let debug = ref false

type optimizations = {
  constant_folding : bool;
  dead_store_elimination : bool;
  unreachable_code_elimination : bool;
  copy_propagation : bool;
}
