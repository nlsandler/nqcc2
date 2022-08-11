let counter = ref 0

let make_temporary () =
  let n = !counter in
  counter := n + 1;
  (* Note: including "." ensures that this label won't conflict
   * with real function or variable names in the symbol table,
   * when we start tracking symbols in later chapters *)
  "tmp." ^ Int.to_string n
