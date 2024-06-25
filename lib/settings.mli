type stage = Lex | Parse | Validate | Tacky | Codegen | Assembly | Obj | Executable
type target = OS_X | Linux
type extra_credit = Bitwise | Compound | Increment | Goto | Switch | Nan | Union
val platform : target ref
val extra_credit_flags : extra_credit list ref
val debug : bool ref
val int_only: bool ref
