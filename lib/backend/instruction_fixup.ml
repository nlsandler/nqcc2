open Assembly

let int32_max = Int64.of_int32 Int32.max_int
let int32_min = Int64.of_int32 Int32.min_int
let is_large imm = imm > int32_max || imm < int32_min

let is_larger_than_uint imm =
  (* use unsigned upper-bound for positives *)
  let max_i = Int64.of_int 4294967295 (* 2^32 - 1*) in
  (* use signed 32-bit lower bound for negatives *)
  imm > max_i || imm < int32_min

let fixup_instruction = function
  (* Mov can't move a value from one memory address to another *)
  | Mov (t, ((Stack _ | Data _) as src), ((Stack _ | Data _) as dst)) ->
      [ Mov (t, src, Reg R10); Mov (t, Reg R10, dst) ]
  (* Mov can't move a large constant to a memory address *)
  | Mov (Quadword, (Imm i as src), ((Stack _ | Data _) as dst)) when is_large i
    ->
      [ Mov (Quadword, src, Reg R10); Mov (Quadword, Reg R10, dst) ]
  (* Moving a quadword-size constant with a longword operand size produces
     assembler warning *)
  | Mov (Longword, Imm i, dst) when is_larger_than_uint i ->
      (* reduce modulo 2^32 by zeroing out upper 32 bits
       * NOTE: can't use Int64.modulo b/c it just calculates remainder *)
      let bitmask = Int64.of_string "0xffffffff" in
      let reduced = Int64.logand i bitmask in
      [ Mov (Longword, Imm reduced, dst) ]
  (* Movsx can't handle immediate source or memory dst *)
  | Movsx ((Imm _ as src), ((Stack _ | Data _) as dst)) ->
      [
        Mov (Longword, src, Reg R10);
        Movsx (Reg R10, Reg R11);
        Mov (Quadword, Reg R11, dst);
      ]
  | Movsx ((Imm _ as src), dst) ->
      [ Mov (Longword, src, Reg R10); Movsx (Reg R10, dst) ]
  | Movsx (src, ((Stack _ | Data _) as dst)) ->
      [ Movsx (src, Reg R11); Mov (Quadword, Reg R11, dst) ]
  (* Idiv can't operate on constants *)
  | Idiv (t, Imm i) -> [ Mov (t, Imm i, Reg R10); Idiv (t, Reg R10) ]
  (* Add/Sub can't take large immediates as source operands *)
  | Binary { op = (Add | Sub) as op; t = Quadword; src = Imm i as src; dst }
    when is_large i ->
      [
        Mov (Quadword, src, Reg R10);
        Binary { op; t = Quadword; src = Reg R10; dst };
      ]
  (* Add/Sub can't use memory addresses for both operands *)
  | Binary
      {
        op = (Add | Sub) as op;
        t;
        src = (Stack _ | Data _) as src;
        dst = (Stack _ | Data _) as dst;
      } ->
      [ Mov (t, src, Reg R10); Binary { op; t; src = Reg R10; dst } ]
  (* Destination of Mult can't be in memory; src can't be a big operand *)
  | Binary
      {
        op = Mult;
        t = Quadword;
        src = Imm i as src;
        dst = (Stack _ | Data _) as dst;
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
  | Binary { op = Mult; t; src; dst = (Stack _ | Data _) as dst } ->
      [
        Mov (t, dst, Reg R11);
        Binary { op = Mult; t; src; dst = Reg R11 };
        Mov (t, Reg R11, dst);
      ]
  (* Both operands of cmp can't be in memory *)
  | Cmp (t, ((Stack _ | Data _) as src), ((Stack _ | Data _) as dst)) ->
      [ Mov (t, src, Reg R10); Cmp (t, Reg R10, dst) ]
  (* first operand of Cmp can't be a large constant, second can't be a constant
     at all *)
  | Cmp (Quadword, (Imm i as src), (Imm _ as dst)) when is_large i ->
      [
        Mov (Quadword, src, Reg R10);
        Mov (Quadword, dst, Reg R11);
        Cmp (Quadword, Reg R10, Reg R11);
      ]
  | Cmp (Quadword, (Imm i as src), dst) when is_large i ->
      [ Mov (Quadword, src, Reg R10); Cmp (Quadword, Reg R10, dst) ]
  | Cmp (t, src, Imm i) -> [ Mov (t, Imm i, Reg R11); Cmp (t, src, Reg R11) ]
  | Push (Imm i as src) when is_large i ->
      [ Mov (Quadword, src, Reg R10); Push (Reg R10) ]
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
