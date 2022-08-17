open Assembly

let int32_max = Int64.of_int32 Int32.max_int
let int32_min = Int64.of_int32 Int32.min_int
let is_large imm = imm > int32_max || imm < int32_min

let is_larger_than_uint imm =
  (* use unsigned upper-bound for positives *)
  let max_i = Int64.of_int 4294967295 (* 2^32 - 1*) in
  (* use signed 32-bit lower bound for negatives *)
  imm > max_i || imm < int32_min

let is_constant = function Imm _ -> true | _ -> false
let is_memory = function Memory _ | Data _ -> true | _ -> false

let is_xmm = function
  | XMM0 | XMM1 | XMM2 | XMM3 | XMM4 | XMM5 | XMM6 | XMM7 | XMM14 | XMM15 ->
      true
  | _ -> false

let fixup_instruction = function
  (* Mov can't move a value from one memory address to another *)
  | Mov (t, ((Memory _ | Data _) as src), ((Memory _ | Data _) as dst)) ->
      let scratch = if t = Double then Reg XMM14 else Reg R10 in
      [ Mov (t, src, scratch); Mov (t, scratch, dst) ]
  (* Mov can't move a large constant to a memory address *)
  | Mov (Quadword, (Imm i as src), ((Memory _ | Data _) as dst)) when is_large i
    ->
      [ Mov (Quadword, src, Reg R10); Mov (Quadword, Reg R10, dst) ]
  (* Moving a quadword-size constant with a longword operand size produces assembler warning *)
  | Mov (Longword, Imm i, dst) when is_larger_than_uint i ->
      (* reduce modulo 2^32 by zeroing out upper 32 bits
       * NOTE: can't use Int64.modulo b/c it just calculates remainder *)
      let bitmask = Int64.of_string "0xffffffff" in
      let reduced = Int64.logand i bitmask in
      [ Mov (Longword, Imm reduced, dst) ]
  (* Movsx can't handle immediate source or memory dst *)
  | Movsx ((Imm _ as src), ((Memory _ | Data _) as dst)) ->
      [
        Mov (Longword, src, Reg R10);
        Movsx (Reg R10, Reg R11);
        Mov (Quadword, Reg R11, dst);
      ]
  | Movsx ((Imm _ as src), dst) ->
      [ Mov (Longword, src, Reg R10); Movsx (Reg R10, dst) ]
  | Movsx (src, ((Memory _ | Data _) as dst)) ->
      [ Movsx (src, Reg R11); Mov (Quadword, Reg R11, dst) ]
  (* rewrite MovZerOExtend as one or two Mov instructions *)
  | MovZeroExtend (src, dst) -> (
      match dst with
      | Reg _ -> [ Mov (Longword, src, dst) ]
      | _ -> [ Mov (Longword, src, Reg R11); Mov (Quadword, Reg R11, dst) ])
  (* Idiv can't operate on constants *)
  | Idiv (t, Imm i) -> [ Mov (t, Imm i, Reg R10); Idiv (t, Reg R10) ]
  | Div (t, Imm i) -> [ Mov (t, Imm i, Reg R10); Div (t, Reg R10) ]
  (* dst of lea must be a register *)
  | Lea (src, dst) when is_memory dst ->
      [ Lea (src, Reg R11); Mov (Quadword, Reg R11, dst) ]
  (* Binary operations on double require register as destination *)
  | Binary { t = Double; dst = Reg _; _ } as i -> [ i ]
  | Binary { op; t = Double; src; dst } ->
      [
        Mov (Double, dst, Reg XMM15);
        Binary { op; t = Double; src; dst = Reg XMM15 };
        Mov (Double, Reg XMM15, dst);
      ]
  (* Add/Sub/And/Or can't take large immediates as source operands *)
  | Binary
      {
        op = (Add | Sub | And | Or) as op;
        t = Quadword;
        src = Imm i as src;
        dst;
      }
    when is_large i ->
      [
        Mov (Quadword, src, Reg R10);
        Binary { op; t = Quadword; src = Reg R10; dst };
      ]
  (* Add/Sub can't use memory addresses for both operands *)
  | Binary
      {
        op = (Add | Sub | And | Or) as op;
        t;
        src = (Memory _ | Data _) as src;
        dst = (Memory _ | Data _) as dst;
      } ->
      [ Mov (t, src, Reg R10); Binary { op; t; src = Reg R10; dst } ]
  (* Destination of Mult can't be in memory; src can't be a big operand  *)
  | Binary
      {
        op = Mult;
        t = Quadword;
        src = Imm i as src;
        dst = (Memory _ | Data _) as dst;
      }
    when is_large i ->
      (* rewrite both operands *)
      [
        Mov (Quadword, src, Reg R10);
        Mov (Quadword, dst, Reg R11);
        Binary { op = Mult; t = Quadword; src = Reg R10; dst = Reg R11 };
        Mov (Quadword, Reg R11, dst);
      ]
  | Binary { op = Mult; t = Quadword; src = Imm i as src; dst } when is_large i
    ->
      (* just rewrite src *)
      [
        Mov (Quadword, src, Reg R10);
        Binary { op = Mult; t = Quadword; src = Reg R10; dst };
      ]
  | Binary { op = Mult; t; src; dst = (Memory _ | Data _) as dst } ->
      [
        Mov (t, dst, Reg R11);
        Binary { op = Mult; t; src; dst = Reg R11 };
        Mov (t, Reg R11, dst);
      ]
  (* destination of comisd must be a register *)
  | Cmp (Double, _, Reg _) as i -> [ i ]
  | Cmp (Double, src, dst) ->
      [ Mov (Double, dst, Reg XMM15); Cmp (Double, src, Reg XMM15) ]
  (* Both operands of cmp can't be in memory *)
  | Cmp (t, ((Memory _ | Data _) as src), ((Memory _ | Data _) as dst)) ->
      [ Mov (t, src, Reg R10); Cmp (t, Reg R10, dst) ]
  (* first operand of Cmp can't be a large constant, second can't be a constant at all *)
  | Cmp (Quadword, (Imm i as src), (Imm _ as dst)) when is_large i ->
      [
        Mov (Quadword, src, Reg R10);
        Mov (Quadword, dst, Reg R11);
        Cmp (Quadword, Reg R10, Reg R11);
      ]
  | Cmp (Quadword, (Imm i as src), dst) when is_large i ->
      [ Mov (Quadword, src, Reg R10); Cmp (Quadword, Reg R10, dst) ]
  | Cmp (t, src, Imm i) -> [ Mov (t, Imm i, Reg R11); Cmp (t, src, Reg R11) ]
  | Push (Reg r) when is_xmm r ->
      [
        Binary
          { op = Sub; t = Quadword; src = Imm (Int64.of_int 8); dst = Reg SP };
        Mov (Double, Reg r, Memory (SP, 0));
      ]
  | Push (Imm i as src) when is_large i ->
      [ Mov (Quadword, src, Reg R10); Push (Reg R10) ]
      (* destination of cvttsd2si must be a register *)
  | Cvttsd2si (t, src, ((Memory _ | Data _) as dst)) ->
      [ Cvttsd2si (t, src, Reg R11); Mov (t, Reg R11, dst) ]
  | Cvtsi2sd (t, src, dst) as i ->
      if is_constant src && is_memory dst then
        [
          Mov (t, src, Reg R10);
          Cvtsi2sd (t, Reg R10, Reg XMM15);
          Mov (Double, Reg XMM15, dst);
        ]
      else if is_constant src then
        [ Mov (t, src, Reg R10); Cvtsi2sd (t, Reg R10, dst) ]
      else if is_memory dst then
        [ Cvtsi2sd (t, src, Reg XMM15); Mov (Double, Reg XMM15, dst) ]
      else [ i ]
  | other -> [ other ]

let fixup_tl = function
  | Function { name; global; instructions } ->
      let stack_bytes =
        Rounding.round_away_from_zero 16
          (-Assembly_symbols.get_bytes_required name)
      in
      let stack_byte_op = Imm (Int64.of_int stack_bytes) in
      Function
        {
          name;
          global;
          instructions =
            Binary { op = Sub; t = Quadword; src = stack_byte_op; dst = Reg SP }
            :: List.concat_map fixup_instruction instructions;
        }
  | static_var -> static_var

let fixup_program (Program tls) =
  let fixed_functions = List.map fixup_tl tls in
  Program fixed_functions
