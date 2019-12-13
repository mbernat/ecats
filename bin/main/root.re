//open Revery;
open Revery.UI;
open Revery.UI.Components;
open Common
open Graphs
open Etymology
open Graph
open Data

let tick = 1000. /. 60.;

module Main {
    type action =
        | Click(Vec.t)
        | Step
        | Tick;

    type state = World.t

    let add_edge = ((node_id, node), state) => {
        open World
        let data = switch(state.selectedNode) {
            | Some(prevSel) => {
                let id = EdgeId.allocate();
                //TODO this doesn't preserve structure of lambda terms
                add_edge(id, Lambda.Graph.Order.First, fst(prevSel), node_id, state.data)
            }
            | None => state.data
        };
        {
            ...state,
            data: data,
            selectedNode: Some((node_id, node))
        }
    }

    let add_node = (pos, state) => {
        open World
        open Vec
        let id = NodeId.allocate();
        let var_id = Lambda.Term.Id.Free(NodeId.string_of(id))
        let node = Node.{name: Lambda.Graph.Name.Var(var_id), pos: pos}
        let data = add_node(id, node, state.data)
        let point = World.mk_point(id, pos);
        let engine = Physics.Engine.add_point(point, state.engine);
        {
            ...state,
            data,
            engine,
            selectedNode: Some((id, node))
        }
    }

    let step_physics = state => {
        open World
        open Physics
        let k = 5e-5;
        let l = 1e2;
        let c = 5e1;
        let d = 5e-3;
        let g = 1e-3;
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
        let drag_force = p => Vec.scale(p.Point.vel, -. d);
        let engine = state.engine
            |> Force.add_gravity(Vec.{x: 0., y: g})
            |> Force.add_uniform(drag_force)
            |> Force.add_all_pairwise(coulomb_force(c))
            |> add_springs_for_edges
            |> Engine.step(tick);
        let points = Engine.to_list(engine);
        let update_node = (id, n) => {
            let point = List.find(p => p.Point.id == id, points);
            Node.{...n, pos: point.Point.pos}
        };
        let data = {...state.data, nodes: NodeMap.mapi(update_node, state.data.nodes)};
        {
            ...state,
            data: data,
            engine: engine
        }
    }

    // TODO handle node selection via Revery
    let reducer = (action, state) =>
        switch(action) {
            // Clicking space creates a node and selects it
            // Clicking on a node creates an edge from the previously selected node
            | Click(pos) => {
                open World;
                let f = (_, n) => Vec.dist(n.Node.pos, pos) < 100.
                let nearby_bindings = state.data.nodes
                    |> NodeMap.filter(f)
                    |> NodeMap.bindings;
                let oNode = List.nth_opt(nearby_bindings, 0);
                switch (oNode) {
                    | Some(node) => add_edge(node, state)
                    | None => add_node(pos, state)
                }
            }
            | Tick => step_physics(state)
            | Step => World.step_lambda(state)
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