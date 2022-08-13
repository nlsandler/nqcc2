let rec take_drop n = function
  | h :: t when n > 0 ->
      let l1, l2 = take_drop (n - 1) t in
      (h :: l1, l2)
  | l -> ([], l)

let%test "empty_0" = take_drop 0 [] = ([], [])
let%test "empty_n" = take_drop 10 [] = ([], [])
let%test "one" = take_drop 1 [ 'a'; 'b'; 'c' ] = ([ 'a' ], [ 'b'; 'c' ])

let%test "three" =
  take_drop 3 [ 1; 2; 3; 4; 5; 6; 7 ] = ([ 1; 2; 3 ], [ 4; 5; 6; 7 ])

let%test "n_gt_len" = take_drop 10 [ 'a'; 'b'; 'c' ] = ([ 'a'; 'b'; 'c' ], [])
let%test "all" = take_drop 3 [ 'a'; 'b'; 'c' ] = ([ 'a'; 'b'; 'c' ], [])
