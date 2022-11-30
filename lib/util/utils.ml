let last l = List.hd (List.rev l)

let rec take n = function
  | [] -> []
  | _ :: _ when n <= 0 -> []
  | h :: t -> h :: take (n - 1) t

let%test "take_zero" = take 0 [ 1; 2; 3; 4; 5 ] = []
let%test "take_all" = take 10 [ 1; 2; 3; 4; 5 ] = [ 1; 2; 3; 4; 5 ]
let%test "take_some" = take 3 [ 1; 2; 3; 4; 5 ] = [ 1; 2; 3 ]

let rec take_drop n = function
  | h :: t when n > 0 ->
      let l1, l2 = take_drop (n - 1) t in
      (h :: l1, l2)
  | l -> ([], l)

let max cmp l = List.(hd (rev (sort cmp l)))
let min cmp l = List.(hd (sort cmp l))

(* Construct a list with len elements of value v*)
let make_list len v = List.init len (fun _ -> v)
let%test "empty_0" = take_drop 0 [] = ([], [])
let%test "empty_n" = take_drop 10 [] = ([], [])
let%test "one" = take_drop 1 [ 'a'; 'b'; 'c' ] = ([ 'a' ], [ 'b'; 'c' ])

let%test "three" =
  take_drop 3 [ 1; 2; 3; 4; 5; 6; 7 ] = ([ 1; 2; 3 ], [ 4; 5; 6; 7 ])

let%test "n_gt_len" = take_drop 10 [ 'a'; 'b'; 'c' ] = ([ 'a'; 'b'; 'c' ], [])
let%test "all" = take_drop 3 [ 'a'; 'b'; 'c' ] = ([ 'a'; 'b'; 'c' ], [])
