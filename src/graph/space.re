open Graphs
module NodeId = MkIntId ()
module EdgeId = MkIntId ()
module ListGraph = MkListGraph(NodeId, EdgeId)
type listGraph = ListGraph.t(Position.t, unit);

let getNodeAtPos = (pos, graph) => {
    let nodes = ListGraph.extract(graph).nodes;
    let nearby = n => Position.dist(pos, n.Graphs.Node.data) < 100.0
    List.find_opt(nearby, nodes);
}