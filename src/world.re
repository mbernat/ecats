module G = Graph.ListGraph(Graph.IntId, Graph.IntId);

type t = {
    positions: list(Position.t),
    graph: G.t(Position.t, unit)
};