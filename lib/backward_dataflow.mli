module Dataflow : functor (G : Cfg.CFG) (VarSet : Set.S) -> sig
  type annotation = VarSet.t
  type annotated_block = annotation G.basic_block
  type annotated_graph = annotation G.t

  val analyze :
    (Format.formatter -> VarSet.elt -> unit) ->
    (annotated_graph -> annotated_block -> annotation) ->
    (annotated_block -> annotation -> annotated_block) ->
    unit G.t ->
    annotated_graph
end
