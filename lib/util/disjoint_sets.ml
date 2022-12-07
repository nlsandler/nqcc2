open Batteries

type 'a t = ('a, 'a) Map.t

let init = Map.empty
let union x y disj_sets = Map.add x y disj_sets

let rec find x disj_sets =
  if Map.mem x disj_sets then
    let mapped_to = Map.find x disj_sets in
    find mapped_to disj_sets
  else x

let is_empty = Map.is_empty
