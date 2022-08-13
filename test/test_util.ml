module ListUtilTest = struct
  open Nqcc.Utils.ListUtil

  let%test "empty_0" = take_drop 0 [] = ([], [])
  let%test "empty_n" = take_drop 10 [] = ([], [])
  let%test "one" = take_drop 1 [ 'a'; 'b'; 'c' ] = ([ 'a' ], [ 'b'; 'c' ])

  let%test "three" =
    take_drop 3 [ 1; 2; 3; 4; 5; 6; 7 ] = ([ 1; 2; 3 ], [ 4; 5; 6; 7 ])

  let%test "n_gt_len" = take_drop 10 [ 'a'; 'b'; 'c' ] = ([ 'a'; 'b'; 'c' ], [])
  let%test "all" = take_drop 3 [ 'a'; 'b'; 'c' ] = ([ 'a'; 'b'; 'c' ], [])
end
