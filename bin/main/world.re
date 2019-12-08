open Common
open Graph
open Etymology
open Physics

module Node = {
    type t = {
        word: string,
        pos: Vec.t
    }
}

module NodeMap = Map.Make(NodeId)
module EdgeMap = Map.Make(EdgeId)

module G = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(NodeId, EdgeId)

type t = {
    engine: Engine.t(NodeId.t),
    graph: G.t,
    nodes: NodeMap.t(Node.t),
    edges: EdgeMap.t(unit),
    selectedNode: option((NodeId.t, Node.t))
};

let mk_point = (id, pos) =>
    Point.{
        id: id,
        pos: pos,
        vel: Vec.zero,
        forces: [],
        mass: Some(1.)
    };

let random_pos = () => {
    let top_left = 200.;
    let extent = 1000.;
    let x = top_left +. Random.float(extent);
    let y = top_left +. Random.float(extent);
    Vec.{x: x, y: y}
}

let mk_random_point = n => mk_point(n, random_pos())

let mk_bear_point = n => {
    let point = mk_random_point(n);
    if (n == root)
        {...point, mass: None}
    else
        point
};

let get_nodes = g => G.fold_vertex(List.cons, g, [])
let get_edges = g => G.fold_edges_e(List.cons, g, [])

let from_node_list = l => {
    let f = (m, (k, v)) => NodeMap.add(k, v, m);
    List.fold_left(f, NodeMap.empty, l)
}

let from_edge_list = l => {
    let f = (m, (k, v)) => EdgeMap.add(k, v, m);
    List.fold_left(f, EdgeMap.empty, l)
}

module ResolvedEdge = {
    type t = {
        id: EdgeId.t,
        src_id: NodeId.t,
        src: Node.t,
        dest_id: NodeId.t,
        dest: Node.t
    }

    let resolve = (w, (src_id, id, dest_id)) => {
        let src = NodeMap.find(src_id, w.nodes)
        let dest = NodeMap.find(dest_id, w.nodes);
        {id, src_id, src, dest_id, dest}
    }
}

let to_node = n => Point.(n.id, Node.{word: id_to_word(n.id), pos: n.pos})

let node_ids = get_nodes(bear_graph)
let points = List.map(mk_bear_point, node_ids)
let nodes = List.map(to_node, points) |> from_node_list

let initial = {
    engine: Engine.init(points),
    graph: bear_graph,
    edges: get_edges(bear_graph) |> List.map(((_, e, _)) => (e, ())) |> from_edge_list,
    nodes: nodes,
    selectedNode: None
}