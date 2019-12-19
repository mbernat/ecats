open Common
open Graphs
open Graph
open Physics

module Node = Lambda.Graph.Node

type foo('f) = {
    a: app(int, 'f)
}

module L = {
    type t('a) = list('a)
}
module F = Newtype1(L)

type t = {
    data: Data.t(Node.t, Lambda.Graph.Order.t),
    root: NodeId.t,
    engine: Engine.t(NodeId.t),
    selectedNode: option((NodeId.t, Node.t))
};

/*
Current data:
graph (node ids + edges ~ (edge id, source, dest))
node pos
node name ~ lambda term info
edge ordering
engine: forall nodes: id, pos, vel, forces, mass
selected id (this should really be a component, for when multiple entities are selected!!!!!!!!!!)
*/

/*
components:
position
node
edge
lambda term info
lambda child ordering info
physical (mass, vel)
forces
selected

we'll also need a couple others for subsystems:
lambda modification
rendering
charges
physics constants?
*/

module Id = Id.MkInt ()

module Physical = {
    type t = {
        mass: float,
        vel: Vec.t
    }
}

module Forces = {
    type t = list(Vec.t)
}

module Edge = {
    type t = {
        source: Id.t,
        target: Id.t
    }
}


module Components2 = {
    type t('f) = {
        position: app(Vec.t, 'f),
        node: app(unit, 'f),
        edge: app(Edge.t, 'f),
        lambda_term: app(Lambda.Graph.Name.t, 'f),
        lambda_child: app(Lambda.Graph.Order.t, 'f),
        physical: app(Physical.t, 'f),
        forces: app(Forces.t, 'f),
        selected: app(unit, 'f)
    }
}

module Mappy = Newtype1({ type t('a) = EntityMap.t('a) })

module Components = {
    type t = {
        position: Vec.t,
        node: unit,
        edge: Edge.t,
        lambda_term: Lambda.Graph.Name.t,
        lambda_child: Lambda.Graph.Order.t,
        physical: Physical.t,
        forces: Forces.t,
        selected: unit
    }
}

module EntityMap = Map.Make(Id)

module World = {
    type t = {
        position: EntityMap.t(Vec.t),
        node: EntityMap.t(unit),
        edge: EntityMap.t(Edge.t),
        lambda_term: EntityMap.t(Lambda.Graph.Name.t),
        lambda_child: EntityMap.t(Lambda.Graph.Order.t),
        physical: EntityMap.t(Physical.t),
        forces: EntityMap.t(Forces.t),
        selected: EntityMap.t(unit)
    }
}

module Entity = {
    type t = {
        position: option(Vec.t),
        node: option(unit),
        edge: option(Edge.t),
        lambda_term: option(Lambda.Graph.Name.t),
        lambda_child: option(Lambda.Graph.Order.t),
        physical: option(Physical.t),
        forces: option(Forces.t),
        selected: option(unit)
    }
}

module Update = {
    type t('a) = Set('a) | Keep | Unset
}

module EntityUpdate = {
    type t = {
        position: Update.t(Vec.t),
        node: Update.t(unit),
        edge: Update.t(Edge.t),
        lambda_term: Update.t(Lambda.Graph.Name.t),
        lambda_child: Update.t(Lambda.Graph.Order.t),
        physical: Update.t(Physical.t),
        forces: Update.t(Forces.t),
        selected: Update.t(unit)
    }
}

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