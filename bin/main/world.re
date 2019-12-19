open Common
open Graphs
open Graph
open Physics

module Node = Lambda.Graph.Node

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

module Edge = {
    type t = {
        source: Id.t,
        target: Id.t
    }
}

module Physical = {
    type t = {
        mass: float,
        velocity: Vec.t
    }
}

/*
polymorphic components using higher library
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
module World = Components2.t(Mappy.t)

module Maybe = Newtype1({ type t('a) = option('a) })
module Entity = Component2.t(Maybe.t)
*/

module Generic = {
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

module EntityMaps = {
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

    let empty = {
        position: EntityMap.empty,
        node: EntityMap.empty,
        edge: EntityMap.empty,
        lambda_term: EntityMap.empty,
        lambda_child: EntityMap.empty,
        physical: EntityMap.empty,
        forces: EntityMap.empty,
        selected: EntityMap.empty
    }
}

type t = {
    components: EntityMaps.t,
    entities: list(Id.t)
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

    let default = {
        position: None,
        node: None,
        edge: None,
        lambda_term: None,
        lambda_child: None,
        physical: None,
        forces: None,
        selected: None
    }
}

module Update = {
    type t('a) = Set('a) | Keep | Unset
}

let get_entity_comp = (id, cs:EntityMaps.t) => {
    let entity = Entity.{
        position: EntityMap.find_opt(id, cs.position),
        node: EntityMap.find_opt(id, cs.node),
        edge: EntityMap.find_opt(id, cs.edge),
        lambda_term: EntityMap.find_opt(id, cs.lambda_term),
        lambda_child: EntityMap.find_opt(id, cs.lambda_child),
        physical: EntityMap.find_opt(id, cs.physical),
        forces: EntityMap.find_opt(id, cs.forces),
        selected: EntityMap.find_opt(id, cs.selected)
    };
    (id, entity)
}

let get_entity = (id, w) => get_entity_comp(id, w.components)

let efilter = (p, w) => {
    List.map(id => get_entity(id, w), w.entities)
    |> List.filter(((_, e)) => p(e))
}

let maybe_add = (id, c, cm) => switch (c) {
    | Some (c) => EntityMap.add(id, c, cm)
    | None => cm
}

let add_components = (id, e, cs) => {
    EntityMaps.{
        position: maybe_add(id, e.Entity.position, cs.position),
        node: maybe_add(id, e.Entity.node, cs.node),
        edge: maybe_add(id, e.Entity.edge, cs.edge),
        lambda_term: maybe_add(id, e.Entity.lambda_term, cs.lambda_term),
        lambda_child: maybe_add(id, e.Entity.lambda_child, cs.lambda_child),
        physical: maybe_add(id, e.Entity.physical, cs.physical),
        forces: maybe_add(id, e.Entity.forces, cs.forces),
        selected: maybe_add(id, e.Entity.selected, cs.selected)
    }
}

let add_entity = (e, w) => {
    let id = Id.allocate();
    {
        components: add_components(id, e, w.components),
        entities: List.cons(id, w.entities)
    }
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

    let default = {
        position: Keep,
        node: Keep,
        edge: Keep,
        lambda_term: Keep,
        lambda_child: Keep,
        physical: Keep,
        forces: Keep,
        selected: Keep
    }
}

let update = (id, up, cm) => switch(up) {
    | Update.Set(c) => EntityMap.add(id, c, cm)
    | Keep => cm
    | Unset => EntityMap.remove(id, cm)
}

let update_components = (id, e, cs) => {
    EntityMaps.{
        position: update(id, e.EntityUpdate.position, cs.position),
        node: update(id, e.EntityUpdate.node, cs.node),
        edge: update(id, e.EntityUpdate.edge, cs.edge),
        lambda_term: update(id, e.EntityUpdate.lambda_term, cs.lambda_term),
        lambda_child: update(id, e.EntityUpdate.lambda_child, cs.lambda_child),
        physical: update(id, e.EntityUpdate.physical, cs.physical),
        forces: update(id, e.EntityUpdate.forces, cs.forces),
        selected: update(id, e.EntityUpdate.selected, cs.selected)
    }
}

let update_entity = (id, e, w) => {...w, components: update_components(id, e, w.components)}


let emap = (f, w) => {
    let update = (cs, id) => update_components(id, f(snd(get_entity_comp(id, cs))), cs);
    {...w, components: List.fold_left(update, w.components, w.entities)}
}

let random_pos = () => {
    let top_left = 100.;
    let extent = 500.;
    let x = top_left +. Random.float(extent);
    let y = top_left +. Random.float(extent);
    Vec.{x: x, y: y}
}

let box = Lambda.Graph.Box.{
    top_left: Vec.{x: 100., y: 100.},
    bottom_right: Vec.{x: 1000., y: 1000.}
}

// TODO abstract away the dependency on concrete graphs (etymology/lambda)
//let (root, my_graph) = (Etymology.root, Etymology.bear_graph)
open Lambda

let step_lambda = g => {
    open Data
    // TODO compute boxes and use them to reposition nodes after substitutions
    g
        |> Graph.view
        |> Term.step
        |> Graph.of_annotated_term;
}
