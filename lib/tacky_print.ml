open Tacky

let pp_unary_operator out = function
  | Complement -> Format.pp_print_string out "~"
  | Negate -> Format.pp_print_string out "-"

let pp_binary_operator out = function
  | Add -> Format.pp_print_string out "+"
  | Subtract -> Format.pp_print_string out "-"
  | Multiply -> Format.pp_print_string out "*"
  | Divide -> Format.pp_print_string out "/"
  | Mod -> Format.pp_print_string out "%"
  | BitshiftLeft -> Format.pp_print_string out "<<"
  | BitshiftRight -> Format.pp_print_string out ">>"
  | BitwiseAnd -> Format.pp_print_string out "&"
  | BitwiseOr -> Format.pp_print_string out "|"
  | BitwiseXor -> Format.pp_print_string out "^"

let pp_tacky_val out = function
  | Constant i -> Format.pp_print_int out i
  | Var s -> Format.pp_print_string out s

let pp_instruction out = function
  | Return v -> Format.fprintf out "Return(%a)" pp_tacky_val v
  | Unary { op; src; dst } ->
      Format.fprintf out "%a = %a%a" pp_tacky_val dst pp_unary_operator op
        pp_tacky_val src
  | Binary { op; src1; src2; dst } ->
      Format.fprintf out "%a = %a %a %a" pp_tacky_val dst pp_tacky_val src1
        pp_binary_operator op pp_tacky_val src2

let pp_function_definition out (Function { name; body }) =
  (* Format.pp_set_margin out 40; *)
  Format.pp_open_vbox out 4;
  Format.fprintf out "%s:" name;
  Format.pp_print_cut out ();
  Format.pp_open_vbox out 0;
  Format.(pp_print_list pp_instruction) out body;
  Format.pp_close_box out ();
  Format.pp_close_box out ();
  Format.pp_print_newline out () (* flush *)

let pp_program out (Program f) = pp_function_definition out f

let debug_print_tacky src_filename tacky_prog =
  if !Settings.debug then (
    let tacky_file = Filename.chop_extension src_filename ^ ".debug.tacky" in
    let chan = open_out tacky_file in
    let formatter = Format.formatter_of_out_channel chan in
    pp_program formatter tacky_prog;
    close_out chan)
