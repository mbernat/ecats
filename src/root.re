//open Revery;
open Revery.UI;
open Revery.UI.Components;

module Main {
    type action =
        | Click(Position.t)
        | UpdateTime;

    type state = {
        time: Unix.tm,
        world: World.t
    };
    
    let reducer = (action, state) =>
        switch(action) {
            | Click(pos) => {
                open Space;
                let oNode = Space.getNodeAtPos(pos, state.world.graph);
                let (graph, sel) = switch (oNode) {
                    | Some(node) => {
                        let graph = switch(state.world.selectedNode) {
                            | Some(prevSel) => {
                                let id = EdgeId.id_of_string("");
                                let edge = Graphs.Edge.{id: id, source: prevSel.id, target: node.id, data: ()}
                                ListGraph.add_edge(edge, state.world.graph);
                            }
                            | None => state.world.graph
                        };
                        (graph, Some(node))
                    }
                    | None => {
                        let id = NodeId.id_of_string("");
                        let node = Graphs.Node.{id:id, data: pos};
                        let graph = ListGraph.add_node(node, state.world.graph);
                        (graph, Some(node));
                    }
                };
                {
                    time: state.time,
                    world: {
                        graph: graph,
                        selectedNode: sel
                    }
                }
            }
            | UpdateTime => {
                time: Unix.time() |> Unix.localtime,
                world: state.world
            }
        };
        
    let component = React.component("Main");
    let createElement = (~children as _, ~world, ()) => 
        component(hooks => {
            let (refOption, setRefOption, hooks) =
                Hooks.state(None, hooks);
            let (state, dispatch, hooks) =
                Hooks.reducer(
                    ~initialState={
                        time: Unix.time() |> Unix.localtime,
                        world: world
                    },
                    reducer,
                    hooks
                );
            let hooks =
                Hooks.effect(
                    OnMount,
                    () => {
                        Some(Revery.Tick.interval(_ => dispatch(UpdateTime), Seconds(1.)))
                    },
                    hooks
                );
            
            open Space;
            let graph = ListGraph.extract(state.world.graph);
            let nodes = List.map(Draw.node(state.world.selectedNode), graph.nodes);
            let edges = List.map(e => Draw.edge(ListGraph.resolve_edge(e, state.world.graph)), graph.edges);
            let items = List.append(nodes, edges);

            let time = Timer.string_of_time(state.time);


            let handleClick = evt =>
                switch(refOption) {
                    | Some(ref) => {
                        open NodeEvents;
                        let (bbx, bby, _, _) = Revery.Math.BoundingBox2d.getBounds(ref#getBoundingBox());
                        let pos = Position.{x: evt.mouseX -. bbx, y: evt.mouseY -. bby};
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
                    <View style=Timer.style>
                        <Text text=time style=Styles.text />
                    </View>
                </View>;
            (hooks, element)
        });
}