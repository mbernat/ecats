open Graphs
open Etymology
open Physics

module L = Layout.WithGraph(NodeId, EdgeId, ListGraph);

type t('a, 'b) = {
    engine: Engine.t(NodeId.t),
    graph: ListGraph.t(Vec.with_pos('a), 'b),
    selectedNode: option(Node.t(NodeId.t, Vec.with_pos('a)))
};

module Space = Space.MkSpace(NodeId, EdgeId, ListGraph);

let mk_point = (id, pos) =>
    Point.{
        id: id,
        pos: pos,
        vel: Vec.zero,
        forces: [],
        mass: Some(1.)
    };

let mk_random_point = n => mk_point(n.Node.id, Vec.random(1000.))

let mk_bear_point = n => {
    let point = mk_random_point(n);
    if (n.id == root)
        {...point, mass: None}
    else
        point
};

let points = List.map(mk_bear_point, ListGraph.extract(bear_graph).nodes);

let initial = {
    engine: Engine.init(points),
    graph: L.Random.layout((), bear_graph),
    selectedNode: None
}