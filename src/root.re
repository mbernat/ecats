//open Revery;
open Revery.UI;
open Revery.UI.Components;
open Etymology
module Draw = Draw.MkDraw(NodeId, EdgeId)

let tick = 1000. /. 60.;

module Main {
    type action =
        | Click(Vec.t)
        | Tick;

    type state = World.t(unit, unit)

    let handle_select = (node, world) => {
        open World
        let graph = switch(world.selectedNode) {
            | Some(prevSel) => {
                let id = EdgeId.allocate();
                let edge = Graphs.Edge.{id: id, source: prevSel.id, target: node.Graphs.Node.id, data: ()}
                ListGraph.add_edge(edge, world.graph);
            }
            | None => world.graph
        };
        {
            ...world,
            graph: graph,
            selectedNode: Some(node)
        }
    }

    let handle_ground_click = (pos, world) => {
        open World
        open Vec
        let id = NodeId.allocate();
        let node = Graphs.Node.{id:id, data: {pos: pos, data: ()}};
        let graph = ListGraph.add_node(node, world.graph);
        let point = World.mk_point(id, pos);
        let engine = Physics.Engine.add_point(point, world.engine);
        {
            engine: engine,
            graph: graph,
            selectedNode: Some(node)
        }
    }

    // TODO add coulomb and spring forces
    let handle_tick = (state) => {
        open World
        open Physics
        let engine = state.engine
            |> Force.add_gravity(Vec.{x: 0., y: 1e-4})
            |> Engine.step(tick);
        let points = Engine.to_list(engine);
        let update_node = n => {
            open Graphs.Node
            let data = n.data;
            let point = List.find(p => p.Point.id == n.id, points);
            Vec.{
                pos: point.Point.pos,
                data: data.data
            }
        };
        let graph = ListGraph.mapi_nodes(update_node, state.graph);
        {
            ...state,
            engine: engine,
            graph: graph
        }
    }
    
    let reducer = (action, state) =>
        switch(action) {
            // Clicking space creates a node and selects it
            // Clicking on a node creates an edge from the previously selected node
            | Click(pos) => {
                open Space;
                open World;
                let oNode = World.Space.getNodeAtPos(pos, state.graph);
                switch (oNode) {
                    | Some(node) => handle_select(node, state)
                    | None => handle_ground_click(pos, state)
                }
            }
            | Tick => handle_tick(state)
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
            open Space;
            let graph = ListGraph.extract(state.graph);
            let nodes = List.map(Draw.node(state.selectedNode), graph.nodes);
            let edges = List.map(e => Draw.edge(ListGraph.resolve_edge(e, state.graph)), graph.edges);
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
                </View>;
            (hooks, element)
        });
}