//open Revery;
open Revery.UI;
open Revery.UI.Components;
open Common
open Graphs
open Etymology
open Graph
open Data

let fps = 1.;
let tick = 1000. /. fps;

let debug = world => List.length(world.World.entities) |> string_of_int |> print_endline;


/*

what should systems do?
they should be run every tick, in some order

how should input be handled?
just register handlers in Revery and treat them as systems

should the state be stored in revery or elsewhere?
is it reasonable to decouple the rendering and treat revery just as a backend?

*/

module Main {
    type action =
        | Click(Vec.t)
        | Step
        | Tick;

    // needs: selection
    // modifies: selection
    // creates entity with: edge, lambda_child
    let add_edge = (node_id, world) => {
        print_endline("add_edge")
        open World
        let k = 5e-5;
        let l = 1e2;
        let c = 5e1;
        let selected = efilter(Entity.(e => e.node != None && e.selected != None), world);
        let world' = switch(selected) {
            | [(sel_id, _)] => {
                let entity = {
                    ...Entity.default,
                    edge: Some(Edge.{ source: sel_id, target: node_id }),
                    lambda_child: Some(Lambda.Graph.Order.First),
                    forces: Some(
                        Physics.Forces.{
                            one_body: [],
                            central: Central.[
                                // TODO the coulomb interaction shouldn't be attached to an edge
                                coulomb(c),
                                spring(k, l)
                            ],
                            concrete: Vec.zero
                    })
                }
                add_entity(entity, world)
            }
            | _ => world
        };
        let update = EntityUpdate.{...default, selected: Set(())}
        update_entity(node_id, update, world')
    }

    // needs: nothing
    // modifies: selection
    // creates entity with: node, lambda_term, position, physical, forces
    let mk_node = pos => {
        print_endline("mk_node");
        open World
        open Vec
        let id = NodeId.allocate();
        let var_id = Lambda.Term.Id.Free(NodeId.string_of(id))
        let term = Lambda.Graph.Name.Var(var_id);
        let d = 5e-3;
        let g = 1e-3;
        // TODO maybe we should attach hidden edges to all other nodes to model coloumb forces?
        let drag_force = p => Vec.scale(p.Physics.Point.velocity, -. d);
        {
            ...Entity.default,
            node: Some(()),
            position: Some(pos),
            physical: Some({
                mass: 1.,
                velocity: Vec.zero
            }),
            lambda_term: Some(term),
            forces: Some(Physics.Forces.{
                one_body: [
                    OneBody.uniform({x: 0., y: g}),
                    drag_force

                ],
                central: [],
                concrete: Vec.zero
            }),
            selected: Some(())
        }
    }

    // TODO get rid of this
    exception GotNone
    let from_option = o => switch(o) {
        | Some(x) => x
        | None => raise(GotNone)
    }

    let add_central_forces = world => {
        open World
        let es = efilter(e => e.Entity.edge != None && e.Entity.forces != None, world);
        List.fold_left((w, (_, e)) => {
            open Entity
            switch (e.edge, e.forces) {
                | (Some(edge), Some(forces)) => {
                    open Physics.Forces
                    // TODO add edge resolver
                    let source = get_entity(edge.source, w)
                    let target = get_entity(edge.target, w)
                    let source_pos = from_option(source.position);
                    let source_forces = from_option(source.forces);
                    let target_pos = from_option(target.position);
                    let target_forces = from_option(target.forces);
                    let add_force = f => Central.apply(f, source_pos, target_pos)
                    let force = List.fold_left((f, cf) => Vec.add(f, add_force(cf)), Vec.zero, forces.central);
                    let source_update = EntityUpdate.{
                        ...default,
                        forces: Set({...source_forces, concrete: Vec.add(source_forces.concrete, force)})
                    };
                    let op_force = Vec.scale(force, -1.)
                    let target_update = EntityUpdate.{
                        ...default,
                        forces: Set({...target_forces, concrete: Vec.add(source_forces.concrete, op_force)})
                    };
                    w
                        |> update_entity(edge.source, source_update)
                        |> update_entity(edge.target, target_update)
                }
                | _ => w
            }
        }, world, es)
/*
TODO convert edge forces into concrete point forces

how do we handle multiple edges between two nodes?
for now just add multiple forces; it's suboptimal but we don't care
*/


    }

    // only uses forces.one_body & forces.concrete and updates forces.concrete
    let add_one_body_forces = e => {
        open World
        open Entity

        switch((e.position, e.physical, e.forces)) {
            | (Some(position), Some(physical), Some(forces)) => {
                let p = Physics.Point.{
                    position: position,
                    mass: physical.mass,
                    velocity: physical.velocity
                }
                let add = (f, ob) => Vec.add(f, ob(p));
                let concrete = List.fold_left(add, forces.concrete, forces.one_body);
                {...EntityUpdate.default, forces: Set({...forces, concrete: concrete})}
            }
            | _ => EntityUpdate.default
        }
    }

    // only uses forces.concrete
    let step_point = e => {
        open World
        open Entity
        switch((e.position, e.physical, e.forces)) {
            | (Some(position), Some(physical), Some(forces)) => {
                let p = Physics.Point.{
                    position: position,
                    mass: physical.mass,
                    velocity: physical.velocity
                }
                let p' = Physics.step_point(tick, p, forces.concrete)
                EntityUpdate.{
                    ...default,
                    position: Set(p'.position),
                    physical: Set({
                        mass: p'.mass,
                        velocity: p'.velocity
                    })
                }
            }
            | _ => EntityUpdate.default
        }
    }

    let step_physics = world => {
        open World
        world
            |> emap(add_one_body_forces)
            |> add_central_forces
            |> emap(step_point)
    }

    let unit_option_to_bool = o => switch(o) {
        | Some(()) => true
        | None => false
    }

    let draw_nodes = world => {
        print_endline("draw_nodes");
        open World
        let nodes = efilter(e => e.node != None && e.position != None, world);
        List.map(((id, node)) => {
            print_endline("drawing a node")
            open Entity
            let position = from_option(node.position);
            let label = Shared.Id.string_of(id);
            Draw.node(position, label, unit_option_to_bool(node.selected))
        }, nodes)
    }

    let draw_edges = world => {
        open World
        let edges = efilter(e => e.Entity.edge != None, world);
        List.map(((id, e)) => {
            open Entity
            let edge = from_option(e.edge)
            let source = get_entity(edge.source, world)
            let target = get_entity(edge.target, world)
            let resolved = Draw.ResolvedEdge.{
                label: Shared.Id.string_of(id),
                src_id: edge.source,
                dest_id: edge.target,
                src_pos: from_option(source.position),
                dest_pos: from_option(target.position)
            };
            Draw.edge(resolved)
        }, edges)
    }

    // TODO handle node selection via Revery
    let reducer = (action, world) =>
        switch(action) {
            // Clicking space creates a node and selects it
            // Clicking on a node creates an edge from the previously selected node
            | Click(pos) => {
                open World;
                let f = e => {
                    open Entity
                    switch (e.position) {
                        | Some(epos) => e.node != None && Vec.dist(epos, pos) < 100.
                        | None => false
                    }
                }
                let nearby = efilter(f, world);
                switch (nearby) {
                    | [(node_id, _)] => add_edge(node_id, world)
                    | _ => {
                        let w' = add_entity(mk_node(pos), world);
                        debug(w');
                        w'
                    }
                }
            }
            | Tick => step_physics(world)
            | Step => world//TODO World.step_lambda(world)
        };

    let component = React.component("Main");
    let createElement = (~children as _, ~initialWorld, ()) =>
        component(hooks => {
            let (refOption, setRefOption, hooks) =
                Hooks.state(None, hooks);
            let (world, dispatch, hooks) =
                Hooks.reducer(
                    ~initialState=initialWorld,
                    reducer,
                    hooks
                );
            let hooks =
                Hooks.effect(
                    OnMount,
                    () => {
                        Some(Revery.Tick.interval(_ => dispatch(Tick), Milliseconds(tick)))
                    },
                    hooks
                );

            // Draw the graph
            open World
            debug(world);
            let _ = emap(e => {print_endline("entity"); EntityUpdate.default}, world);
            let nodes = draw_nodes(world)
            let edges = draw_edges(world)
            let items = List.append(nodes, edges);

            // Computes the coordinates of the click relative to the parent element
            let handleClick = evt =>
                switch(refOption) {
                    | Some(ref) => {
                        open NodeEvents;
                        let (bbx, bby, _, _) = Revery.Math.BoundingBox2d.getBounds(ref#getBoundingBox());
                        let pos = Vec.{x: evt.mouseX -. bbx, y: evt.mouseY -. bby};
                        dispatch(Click(pos));
                    }
                    | None => {
                        print_endline("Error in Main: ref of the inner view is not set!")
                    }
            };

            let outerStyle =
                Style.[
                    backgroundColor(Revery.Colors.cornflowerBlue),
                    position(`Absolute),
                    left(100),
                    top(100),
                    bottom(100),
                    right(100),
                    border(Revery.Colors.black, 4),
                    borderRadius(4.0)
                ];

            let innerStyle =
                Style.[
                    position(`Absolute),
                    left(0),
                    top(0),
                    bottom(70),
                    right(0)
                ];

            let element =
                <View style=outerStyle>
                    <View
                        ref={r => setRefOption(Some(r))}
                        style=innerStyle
                        onMouseDown=handleClick>
                        ...items
                    </View>
                    <Button title="step" onClick={() => dispatch(Step)} />
                </View>;
            (hooks, element)
        });
}