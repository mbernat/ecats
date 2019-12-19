//open Revery;
open Revery.UI;
open Revery.UI.Components;
open Common
open Graphs
open Etymology
open Graph
open Data

let tick = 1000. /. 60.;

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
        update_entity(node_id, update, world)
    }

    // needs: nothing
    // modifies: selection
    // creates entity with: node, lambda_term, position, physical, forces
    let mk_node = pos => {
        open World
        open Vec
        let id = NodeId.allocate();
        let var_id = Lambda.Term.Id.Free(NodeId.string_of(id))
        let term = Lambda.Graph.Name.Var(var_id);
        let d = 5e-3;
        let g = 1e-3;
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

    let add_central_forces = world => {
        open World
        let edges = efilter(e => e.Entity.edge != None && e.Entity.forces != None, world);
/*
TODO convert edge forces into concrete point forces

how do we handle multiple edges between two nodes?
for now just add multiple forces; it's suboptimal but we don't care
*/
        world
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


/*
        let add_springs_for_edges = engine => {
            let edges = get_edges(state.data);
            module PairSet = Set.Make ({type t = (NodeId.t, NodeId.t); let compare = compare});
            let add_pair = (s, (source, id, dest)) => {
                let pair = if (source < dest)
                    (source, dest)
                else
                    (dest, source)
                PairSet.add(pair, s)
            }
            let pairs = List.fold_left(add_pair, PairSet.empty, edges);
            let add_spring = ((a, b), e) => Force.add_pairwise(a, b, spring_force(k, l), e);
            PairSet.fold(add_spring, pairs, engine);
        }



*/



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
                    | _ => add_entity(mk_node(pos), world)
                }
            }
            | Tick => step_physics(world)
            | Step => world//TODO World.step_lambda(world)
        };

    let component = React.component("Main");
    let createElement = (~children as _, ~world, ()) =>
        component(hooks => {
            let (refOption, setRefOption, hooks) =
                Hooks.state(None, hooks);
            let (state, dispatch, hooks) =
                Hooks.reducer(
                    ~initialState=world,
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
            let nodes = List.map(Draw.node(state.selectedNode), NodeMap.bindings(state.data.nodes));
            let edges = List.map(e => Draw.edge(ResolvedEdge.resolve(state, e)), get_edges(state.data));
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