
open Graphs
type posListGraph = ListGraph.t(Position.t, unit);

type t = {
    graph: posListGraph,
    selectedNode: option(Node.t(Position.t))
};