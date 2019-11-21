open Common
open Graphs

module MkSpace(NodeId: Id, EdgeId: Id, G: WithIds(NodeId, EdgeId).Graph) = {
    type listGraph = G.t(Vec.with_pos(unit), unit);

    let getNodeAtPos = (pos, graph) => {
        let nodes = G.extract(graph).nodes;
        let nearby = n => Vec.dist(pos, n.Graphs.Node.data.Vec.pos) < 100.0
        List.find_opt(nearby, nodes);
    }
}