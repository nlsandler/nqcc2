open Nqcc

let print_instructions instr_list =
  Format.printf "%a"
    (Tacky_print.pp_tl ~escape_brackets:false)
    (Tacky.Function
       { global = false; name = "test"; params = []; body = instr_list })

let%expect_test "not_zero" =
  let optimized =
    Constant_folding.optimize "test"
      [
        Tacky.Unary
          { op = Not; src = Tacky.Constant Const.int_zero; dst = Var "test" };
      ]
  in
  print_instructions optimized;
  [%expect {|
    test():
        test = 1
    |}]

let%expect_test "not_zero" =
  let optimized =
    Constant_folding.optimize "test"
      [
        Tacky.Unary
          { op = Not; src = Tacky.Constant Const.int_one; dst = Var "test" };
      ]
  in
  print_instructions optimized;
  [%expect {|
    test():
        test = 0
    |}]
