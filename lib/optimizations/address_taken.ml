module StringSet = Set.Make (String)

let analyze instrs =
  let addr_taken = function
    | Tacky.GetAddress { src = Var v; _ } -> Some v
    | _ -> None
  in

  StringSet.of_list (List.filter_map addr_taken instrs)
