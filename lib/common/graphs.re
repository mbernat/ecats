module NodeId = Id.MkInt ()
module EdgeId = Id.MkInt ()
module G = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(NodeId, EdgeId)
module O = Graph.Oper.P(G)

module NodeMap = Map.Make(NodeId)
module EdgeMap = Map.Make(EdgeId)

module Data = {
    type t('a, 'b) = {
        graph: G.t,
        nodes: NodeMap.t('a),
        edges: EdgeMap.t('b)
    }

    let empty = {
        graph: G.empty,
        nodes: NodeMap.empty,
        edges: EdgeMap.empty
    }

    let add_node = (id, node, d) => {
        let graph = G.add_vertex(d.graph, id);
        let nodes = NodeMap.add(id, node, d.nodes);
        {...d, graph, nodes}
    }

    let add_edge = (id, edge, src, dest, d) => {
        let graph = G.add_edge_e(d.graph, (src, id, dest));
        let edges = EdgeMap.add(id, edge, d.edges);
        {...d, graph, edges}
    }

    exception NodesNotDisjoint(NodeId.t)
    exception EdgesNotDisjoint(EdgeId.t)
    let disjoint_union = (d1, d2) => {
        graph: O.union(d1.graph, d2.graph),
        nodes: NodeMap.union((k, n1, n2) => raise(NodesNotDisjoint(k)), d1.nodes, d2.nodes),
        edges: EdgeMap.union((k, e1, e2) => raise(EdgesNotDisjoint(k)), d1.edges, d2.edges)
    }

    let get_nodes = d => G.fold_vertex(List.cons, d.graph, [])
    let get_edges = d => G.fold_edges_e(List.cons, d.graph, [])
}