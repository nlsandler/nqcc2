open Unsigned
open Tacky

let comma_sep out () = Format.fprintf out ", "

let pp_init_list out init_list =
  Format.open_box 0;
  Format.pp_print_string out "{";
  Format.pp_print_list ~pp_sep:comma_sep Initializers.pp_static_init out
    init_list;
  Format.pp_print_string out "}";
  Format.close_box ()

let pp_unary_operator out = function
  | Complement -> Format.pp_print_string out "~"
  | Negate -> Format.pp_print_string out "-"
  | Not -> Format.pp_print_string out "!"

let pp_binary_operator out = function
  | Add -> Format.pp_print_string out "+"
  | Subtract -> Format.pp_print_string out "-"
  | Multiply -> Format.pp_print_string out "*"
  | Divide -> Format.pp_print_string out "/"
  | Mod -> Format.pp_print_string out "%"
  | Equal -> Format.pp_print_string out "=="
  | NotEqual -> Format.pp_print_string out "!="
  | LessThan -> Format.pp_print_string out "<"
  | LessOrEqual -> Format.pp_print_string out "<="
  | GreaterThan -> Format.pp_print_string out ">"
  | GreaterOrEqual -> Format.pp_print_string out "<="

let const_to_string = function
  | Const.ConstInt i -> Int32.to_string i
  | ConstLong l -> Int64.to_string l ^ "l"
  | ConstUInt ui -> UInt32.to_string ui ^ "u"
  | ConstULong ul -> UInt64.to_string ul ^ "ul"
  | ConstDouble d -> Float.to_string d

let pp_tacky_val out = function
  | Constant i -> Format.pp_print_string out (const_to_string i)
  | Var s -> Format.pp_print_string out s

let pp_instruction out = function
  | Return v -> Format.fprintf out "Return(%a)" pp_tacky_val v
  | Unary { op; src; dst } ->
      Format.fprintf out "%a = %a%a" pp_tacky_val dst pp_unary_operator op
        pp_tacky_val src
  | Binary { op; src1; src2; dst } ->
      Format.fprintf out "%a = %a %a %a" pp_tacky_val dst pp_tacky_val src1
        pp_binary_operator op pp_tacky_val src2
  | Copy { src; dst } ->
      Format.fprintf out "%a = %a" pp_tacky_val dst pp_tacky_val src
  | Jump s -> Format.fprintf out "Jump(%s)" s
  | JumpIfZero (cond, target) ->
      Format.fprintf out "JumpIfZero(%a, %s)" pp_tacky_val cond target
  | JumpIfNotZero (cond, target) ->
      Format.fprintf out "JumpIfNotZero(%a, %s)" pp_tacky_val cond target
  | Label s ->
      Format.pp_print_break out 0 (-2);
      Format.fprintf out "%s:" s
  | FunCall { f; args; dst } ->
      Format.fprintf out "%a = %s(%a)" pp_tacky_val dst f
        Format.(pp_print_list ~pp_sep:comma_sep pp_tacky_val)
        args
  | SignExtend { src; dst } ->
      Format.fprintf out "%a = SignExtend(%a)" pp_tacky_val dst pp_tacky_val src
  | ZeroExtend { src; dst } ->
      Format.fprintf out "%a = ZeroExtend(%a)" pp_tacky_val dst pp_tacky_val src
  | Truncate { src; dst } ->
      Format.fprintf out "%a = Truncate(%a)" pp_tacky_val dst pp_tacky_val src
  | DoubleToInt { src; dst } ->
      Format.fprintf out "%a = DoubleToInt(%a)" pp_tacky_val dst pp_tacky_val
        src
  | DoubleToUInt { src; dst } ->
      Format.fprintf out "%a = DoubleToUInt(%a)" pp_tacky_val dst pp_tacky_val
        src
  | IntToDouble { src; dst } ->
      Format.fprintf out "%a = IntToDouble(%a)" pp_tacky_val dst pp_tacky_val
        src
  | UIntToDouble { src; dst } ->
      Format.fprintf out "%a = UIntToDouble(%a)" pp_tacky_val dst pp_tacky_val
        src
  | GetAddress { src; dst } ->
      Format.fprintf out "%a = GetAddress(%a)" pp_tacky_val dst pp_tacky_val src
  | Load { src_ptr; dst } ->
      Format.fprintf out "%a = Load(%a)" pp_tacky_val dst pp_tacky_val src_ptr
  | Store { src; dst_ptr } ->
      Format.fprintf out "*(%a) = %a" pp_tacky_val dst_ptr pp_tacky_val src
  | AddPtr { ptr; index; scale; dst } ->
      Format.fprintf out "%a = %a + %a * %d" pp_tacky_val dst pp_tacky_val ptr
        pp_tacky_val index scale
  | CopyToOffset { src; dst; offset } ->
      Format.fprintf out "%s[offset = %d] = %a" dst offset pp_tacky_val src

let pp_function_definition global name params out body =
  Format.pp_open_vbox out 0;
  if global then Format.pp_print_string out "global ";
  Format.fprintf out "%s(%a):" name
    Format.(pp_print_list ~pp_sep:comma_sep pp_print_string)
    params;
  Format.pp_print_break out 0 4;
  Format.pp_open_vbox out 0;
  Format.pp_print_list pp_instruction out body;
  Format.pp_close_box out ();
  Format.pp_close_box out ()

let pp_tl out = function
  | Function { global; name; params; body } ->
      pp_function_definition global name params out body
  | StaticVariable { global; name; init; t } ->
      Format.pp_open_vbox out 0;
      if global then Format.pp_print_string out "global ";
      Format.fprintf out "%a %s = %a" Types.pp t name pp_init_list init;
      Format.pp_close_box out ()

let pp_program out (Program tls) =
  Format.pp_open_vbox out 0;
  Format.pp_print_list
    ~pp_sep:(fun out () ->
      (* print _two_ newlines b/t top levels *)
      Format.pp_print_cut out ();
      Format.pp_print_cut out ())
    pp_tl out tls;
  Format.pp_close_box out ();
  Format.pp_print_newline out () (* flush *)

let debug_print_tacky src_filename tacky_prog =
  if !Settings.debug then (
    let tacky_file = Filename.chop_extension src_filename ^ ".debug.tacky" in
    let chan = open_out tacky_file in
    let formatter = Format.formatter_of_out_channel chan in
    pp_program formatter tacky_prog;
    close_out chan)
