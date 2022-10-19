[@@@coverage exclude_file]

open Cnums

type unary_operator = Complement | Negate | Not [@@deriving show, eq, ord]

type binary_operator =
  | Add
  | Subtract
  | Multiply
  | Divide
  | Mod
  | Equal
  | NotEqual
  | LessThan
  | LessOrEqual
  | GreaterThan
  | GreaterOrEqual
[@@deriving show, eq, ord]

(* we need a custom comparison function for constants to make sure that 0.0 and -0.0 don't compare equal *)
let const_compare a b =
  match (a, b) with
  | Const.ConstDouble d1, Const.ConstDouble d2 when d1 = d2 ->
      Float.compare (Float.copysign 1. d1) (Float.copysign 1. d2)
  | _ -> Const.compare a b

type tacky_val =
  | Constant of (Const.t[@compare fun a b -> const_compare a b])
  | Var of string
[@@deriving eq, ord]

let show_tacky_val = function Constant c -> Const.show c | Var v -> v
let pp_tacky_val fmt v = Format.pp_print_string fmt (show_tacky_val v)

(* TODO maybe this should be in a separate module? *)
let type_of_val = function
  (* note: this reports the type of ConstChar as SChar instead of Char, doesn't matter in this context *)
  | Constant c -> Const.type_of_const c
  | Var v -> (Symbols.get v).t

type instruction =
  | Return of tacky_val option
  | SignExtend of { src : tacky_val; dst : tacky_val }
  | ZeroExtend of { src : tacky_val; dst : tacky_val }
  | DoubleToInt of { src : tacky_val; dst : tacky_val }
  | IntToDouble of { src : tacky_val; dst : tacky_val }
  | DoubleToUInt of { src : tacky_val; dst : tacky_val }
  | UIntToDouble of { src : tacky_val; dst : tacky_val }
  | Truncate of { src : tacky_val; dst : tacky_val }
  | Unary of { op : unary_operator; src : tacky_val; dst : tacky_val }
  | Binary of {
      op : binary_operator;
      src1 : tacky_val;
      src2 : tacky_val;
      dst : tacky_val;
    }
  | Copy of { src : tacky_val; dst : tacky_val }
  | GetAddress of { src : tacky_val; dst : tacky_val }
  | Load of { src_ptr : tacky_val; dst : tacky_val }
  | Store of { src : tacky_val; dst_ptr : tacky_val }
  | AddPtr of {
      ptr : tacky_val;
      index : tacky_val;
      scale : int;
      dst : tacky_val;
    }
  | CopyToOffset of { src : tacky_val; dst : string; offset : int }
  | CopyFromOffset of { src : string; offset : int; dst : tacky_val }
  | Jump of string
  | JumpIfZero of tacky_val * string
  | JumpIfNotZero of tacky_val * string
  | Label of string
  | FunCall of { f : string; args : tacky_val list; dst : tacky_val option }
[@@deriving show { with_path = false }, eq, ord]

type top_level =
  | Function of {
      name : string;
      global : bool;
      params : string list;
      body : instruction list;
    }
  | StaticVariable of {
      name : string;
      t : Types.t;
      global : bool;
      init : Initializers.static_init list;
    }
  | StaticConstant of {
      name : string;
      t : Types.t;
      init : Initializers.static_init;
    }
[@@deriving show]

type t = Program of top_level list [@@deriving show]

[@@@coverage exclude_file]
