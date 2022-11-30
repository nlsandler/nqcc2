(* Extra context about a TACKY or assembly function;
 * used for debugging, looking things up in symbol table, etc *)
type t = { filename : string; fun_name : string; params : string list }
