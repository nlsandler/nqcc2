module Dataflow : functor (G : Cfg.CFG) -> sig
  type 'var annotation = 'var Batteries.Set.t
  type 'var annotated_block = 'var annotation G.basic_block
  type 'var annotated_graph = 'var annotation G.t

  val analyze :
    ('var annotated_graph -> 'var annotated_block -> 'var annotation) ->
    ('var annotated_block -> 'var annotation -> 'var annotated_block) ->
    unit G.t ->
    'var annotated_graph
end
