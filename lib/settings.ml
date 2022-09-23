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

let platform = ref OS_X (* default to OS X *)
