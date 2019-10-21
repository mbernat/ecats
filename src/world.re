module G = Graph.ListGraph(Graph.IntId, Graph.IntId);

type t = {
    graph: G.t(Position.t, unit)
};