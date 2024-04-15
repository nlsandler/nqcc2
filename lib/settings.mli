type stage = Lex | Parse | Validate | Tacky | Codegen | Assembly | Executable
type target = OS_X | Linux
type extra_credit = Bitwise | Compound | Increment
val platform : target ref
val extra_credit_flags : extra_credit list ref
val debug : bool ref
