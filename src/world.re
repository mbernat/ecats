module ListGraph = Graphs.ListBased(Graphs.IntId, Graphs.IntId);
type posListGraph = ListGraph.t(Position.t, unit);

type t = {
    graph: posListGraph,
    selectedNode: option((Graphs.IntId.t, Graphs.Node.t(Position.t)))
};