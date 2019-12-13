open Common
open Graphs
open Graph
open Physics

module Node = Lambda.Graph.Node

type t = {
    data: Data.t(Node.t, Lambda.Graph.Order.t),
    root: NodeId.t,
    engine: Engine.t(NodeId.t),
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
    let top_left = 100.;
    let extent = 500.;
    let x = top_left +. Random.float(extent);
    let y = top_left +. Random.float(extent);
    Vec.{x: x, y: y}
}

let mk_random_point = n => mk_point(n, random_pos())

let mk_root_point = (root, (id, n)) => {
    let point = mk_point(id, n.Node.pos);
    if (id == root)
        {...point, mass: None}
    else
        point
};

module ResolvedEdge = {
    type t = {
        id: EdgeId.t,
        src_id: NodeId.t,
        src: Node.t,
        dest_id: NodeId.t,
        dest: Node.t
    }

    let resolve = (w, (src_id, id, dest_id)) => {
        let src = NodeMap.find(src_id, w.data.nodes)
        let dest = NodeMap.find(dest_id, w.data.nodes);
        {id, src_id, src, dest_id, dest}
    }
}

let prepare = (root, data) => {
    open Data
    let nodes = NodeMap.bindings(data.nodes)
    let points = List.map(mk_root_point(root), nodes);
    {
        data,
        root,
        engine: Engine.init(points),
        selectedNode: None
    }
}

let box = Lambda.Graph.Box.{
    top_left: Vec.{x: 100., y: 100.},
    bottom_right: Vec.{x: 1000., y: 1000.}
}

// TODO abstract away the dependency on concrete graphs (etymology/lambda)
//let (root, my_graph) = (Etymology.root, Etymology.bear_graph)
open Lambda
let g = Graph.of_term(box, Term.ex3)
let initial = prepare(g.root, g.graph)

let step_lambda = state => {
    open Data
    // TODO compute boxes and use them to reposition nodes after substitutions
    let g = Lambda.Graph.Rooted.{graph: state.data, root: state.root}
        |> Graph.view
        |> Term.step
        |> Graph.of_annotated_term;
    prepare(g.root, g.graph)
}