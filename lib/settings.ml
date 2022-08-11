type stage = Lex | Parse | Tacky | Codegen | Assembly | Executable
type target = OS_X | Linux

let platform = ref OS_X (* default to OS X *)
let debug = ref false
