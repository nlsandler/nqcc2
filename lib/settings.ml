type stage = Lex | Parse | Validate | Tacky | Codegen | Obj | Executable
type target = OS_X | Linux

let platform = ref OS_X (* default to OS X *)
