
open Graphs
module NodeId = MkIntId ()
module EdgeId = MkIntId ()
module ListGraph = MkListGraph(NodeId, EdgeId)
type posListGraph = ListGraph.t(Position.t, unit);

type t = {
    graph: posListGraph,
    selectedNode: option(Node.t(NodeId.t, Position.t))
};