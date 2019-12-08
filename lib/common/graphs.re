module NodeId = Id.MkInt ()
module EdgeId = Id.MkInt ()
module G = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(NodeId, EdgeId)
