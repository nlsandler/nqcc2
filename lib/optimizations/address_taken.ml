module StringSet = Set.Make (String)

let analyze instrs =
  let addr_taken = function
    | Tacky.GetAddress { src = Var v; _ } -> Some v
    | _ -> None
  in

  StringSet.of_list (List.filter_map addr_taken instrs)

let analyze_program (Tacky.Program tls) =
  let analyze_tl = function
    | Tacky.Function f -> analyze f.body
    | _ -> StringSet.empty
  in
  let aliased_vars_per_fun = List.map analyze_tl tls in
  List.fold_left StringSet.union StringSet.empty aliased_vars_per_fun
