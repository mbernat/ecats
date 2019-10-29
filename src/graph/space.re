type with_pos('a) = {
    pos: Position.t,
    data: 'a
}

open Graphs
module MkSpace(NodeId: Id, EdgeId: Id, G: WithIds(NodeId, EdgeId).Graph) = {
    type listGraph = G.t(with_pos(unit), unit);

    let getNodeAtPos = (pos, graph) => {
        let nodes = G.extract(graph).nodes;
        let nearby = n => Position.dist(pos, n.Graphs.Node.data.pos) < 100.0
        List.find_opt(nearby, nodes);
    }
}