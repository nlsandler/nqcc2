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

(* Control which extra-credit features are enabled (to test the test suite) *)
type extra_credit =
  | Bitwise
  | Compound
  | Increment
  | Goto
  | Switch
  | Nan
  | Union

let platform = ref OS_X (* default to OS X *)
let extra_credit_flags = ref []
let int_only = ref false
let debug = ref false
