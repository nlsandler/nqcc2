module type CFG = sig
  type instr
  type node_id = Entry | Block of int | Exit

  (* Cfg is parameterized by type of val we compute and type of instruction (we use both Tacky and assembly instructions )*)

  type 'v basic_block = {
    id : node_id;
    instructions : ('v * instr) list;
    mutable preds : node_id list;
    mutable succs : node_id list;
    value : 'v;
  }

  type 'v t = {
    (* store basic blocks in association list, indexed by block # *)
    basic_blocks : (int * 'v basic_block) list;
    mutable entry_succs : node_id list;
    mutable exit_preds : node_id list;
    ctx : Context.t;
  }

  val instructions_to_cfg : Context.t -> instr list -> unit t
  val cfg_to_instructions : 'v t -> instr list
  val get_succs : node_id -> 'v t -> node_id list
  val get_block_value : int -> 'v t -> 'v
  val add_edge : node_id -> node_id -> 'v t -> unit
  val remove_edge : node_id -> node_id -> 'v t -> unit
  val initialize_annotation : 'a t -> 'b -> 'b t
  val strip_annotations : 'a t -> unit t

  (* debugging *)
  val print_graphviz :
    string -> (Format.formatter -> 'v -> unit) -> 'v t -> unit
end

module TackyCfg : CFG with type instr = Tacky.instruction
module AsmCfg : CFG with type instr = Assembly.instruction
