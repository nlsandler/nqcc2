open Unsigned

let evaluate_cast src_const dst =
  let dst_type = Tacky.type_of_val dst in
  let converted_src =
    try Const_convert.const_convert dst_type src_const
    with Z.Overflow | Failure _ ->
      (* Undefined behavior (e.g. out of range conversion) so just use 0;
       * program behavior is undefined if this cast is ever executed, but
        * well-defined it's in is dead code *)
      Const_convert.const_convert dst_type Const.int_zero
  in
  Some (Tacky.Copy { src = Constant converted_src; dst })

let int_of_bool b = if b then Const.int_one else Const.int_zero

module type Evaluatable = sig
  type t

  val zero : t

  (* comparison function - used to derive <, <=, etc *)
  val compare : t -> t -> int

  (* unary arithmetic expression *)
  val lognot : t -> t
  val neg : t -> t

  (* Binary arithmetic operations *)
  val add : t -> t -> t
  val sub : t -> t -> t
  val div : t -> t -> t
  val mul : t -> t -> t
  val rem : t -> t -> t
  val to_const : t -> Const.t
end

module ConstEvaluator (E : Evaluatable) = struct
  let eq v1 v2 = E.compare v1 v2 = 0
  let neq v1 v2 = E.compare v1 v2 <> 0
  let gt v1 v2 = E.compare v1 v2 > 0
  let ge v1 v2 = E.compare v1 v2 >= 0
  let lt v1 v2 = E.compare v1 v2 < 0
  let le v1 v2 = E.compare v1 v2 <= 0

  let eval_unop v = function
    | Tacky.Not -> int_of_bool (eq v E.zero)
    | Tacky.Complement -> E.to_const (E.lognot v)
    | Tacky.Negate -> E.to_const (E.neg v)

  let eval_binop v1 v2 = function
    (* result is same type as source values *)
    | Tacky.Add -> E.(to_const (add v1 v2))
    | Subtract -> E.(to_const (sub v1 v2))
    | Multiply -> E.(to_const (mul v1 v2))
    | Divide -> (
        (* don't fail on division by zero; it may not actually be executed at
           runtime *)
        try E.(to_const (div v1 v2))
        with Division_by_zero -> E.to_const E.zero)
    | Mod -> (
        try E.(to_const (rem v1 v2))
        with Division_by_zero -> E.to_const E.zero)
    | Equal -> int_of_bool (eq v1 v2)
    | NotEqual -> int_of_bool (neq v1 v2)
    | GreaterThan -> int_of_bool (gt v1 v2)
    | GreaterOrEqual -> int_of_bool (ge v1 v2)
    | LessThan -> int_of_bool (lt v1 v2)
    | LessOrEqual -> int_of_bool (le v1 v2)
end

module IntEvaluator = ConstEvaluator (struct
  include Int32

  let to_const i = Const.ConstInt i
end)

module LongEvaluator = ConstEvaluator (struct
  include Int64

  let to_const l = Const.ConstLong l
end)

module UIntEvaluator = ConstEvaluator (struct
  include UInt32

  let neg x = sub zero x
  let to_const u = Const.ConstUInt u
end)

module ULongEvaluator = ConstEvaluator (struct
  include UInt64

  let neg x = sub zero x
  let to_const ul = Const.ConstULong ul
end)

module DoubleEvaluator = ConstEvaluator (struct
  include Float

  let to_const d = Const.ConstDouble d
  let rem _ = (failwith "Remainder of double not supported" [@coverage off])

  let lognot _ =
    (failwith "Bitwise complement of double not supported" [@coverage off])
end)

let evaluate_unop op = function
  | Const.ConstInt i -> IntEvaluator.eval_unop i op
  | ConstUInt u -> UIntEvaluator.eval_unop u op
  | ConstLong l -> LongEvaluator.eval_unop l op
  | ConstULong ul -> ULongEvaluator.eval_unop ul op
  | ConstDouble d -> DoubleEvaluator.eval_unop d op
  | Const.ConstChar c when op = Tacky.Not -> int_of_bool (c = Int8.zero)
  | ConstUChar uc when op = Not -> int_of_bool (uc = UInt8.zero)
  | ConstChar _ | ConstUChar _ ->
      failwith "~ and - operations on chars should be integer promoted"

let evaluate_binop op v1 v2 =
  match (v1, v2) with
  | Const.ConstInt i1, Const.ConstInt i2 -> IntEvaluator.eval_binop i1 i2 op
  | ConstUInt i1, ConstUInt i2 -> UIntEvaluator.eval_binop i1 i2 op
  | ConstLong l1, ConstLong l2 -> LongEvaluator.eval_binop l1 l2 op
  | ConstULong l1, ConstULong l2 -> ULongEvaluator.eval_binop l1 l2 op
  | ConstDouble d1, ConstDouble d2 -> DoubleEvaluator.eval_binop d1 d2 op
  | ConstChar _, ConstChar _ | ConstUChar _, ConstUChar _ ->
      failwith "binary operations on chars should be integer promoted"
  | _ -> failwith "Internal error:\n   mismatched types" [@coverage off]

let is_zero c = evaluate_unop Not c = Const.ConstInt Int32.one

let optimize_instruction = function
  | Tacky.Unary { op; src = Constant c; dst } ->
      let new_src = evaluate_unop op c in
      Some (Tacky.Copy { src = Constant new_src; dst })
  | Binary { op; src1 = Constant c1; src2 = Constant c2; dst } ->
      let new_src = evaluate_binop op c1 c2 in
      Some (Copy { src = Constant new_src; dst })
  | JumpIfZero (Constant c, target) ->
      if is_zero c then Some (Jump target) else None
  | JumpIfNotZero (Constant c, target) ->
      if is_zero c then None else Some (Jump target)
  (* type conversions *)
  | Truncate { src = Constant c; dst } -> evaluate_cast c dst
  | SignExtend { src = Constant c; dst } -> evaluate_cast c dst
  | ZeroExtend { src = Constant c; dst } -> evaluate_cast c dst
  | DoubleToInt { src = Constant c; dst } -> evaluate_cast c dst
  | DoubleToUInt { src = Constant c; dst } -> evaluate_cast c dst
  | IntToDouble { src = Constant c; dst } -> evaluate_cast c dst
  | UIntToDouble { src = Constant c; dst } -> evaluate_cast c dst
  (* if this copies b/t values of different types, we'll replace it with a copy using
   * src constant w/ same type as dst *)
  | Copy { src = Constant c; dst } -> evaluate_cast c dst
  (* other instructions can't be constnat folded *)
  | i -> Some i

let debug_print debug_label instructions =
  let filename = debug_label ^ "_const_fold" in
  let dummy_prog =
    Tacky.Program
      [
        Function
          {
            global = false;
            name = debug_label;
            params = [ "UNKNOWN" ];
            body = instructions;
          };
      ]
  in
  Tacky_print.debug_print_tacky filename dummy_prog

let optimize debug_label instructions =
  debug_print debug_label instructions;
  List.filter_map optimize_instruction instructions
