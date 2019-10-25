open Graphs
module NodeId = MkIntId ()
module EdgeId = MkIntId ()
module ListGraph = MkListGraph(NodeId, EdgeId)

type with_pos('a) = {
    pos: Position.t,
    data: 'a
}

type listGraph = ListGraph.t(with_pos(unit), unit);

let getNodeAtPos = (pos, graph) => {
    let nodes = ListGraph.extract(graph).nodes;
    let nearby = n => Position.dist(pos, n.Graphs.Node.data.pos) < 100.0
    List.find_opt(nearby, nodes);
}