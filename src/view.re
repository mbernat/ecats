open Revery;
open Revery.UI;
open Revery.UI.Components;

let outerStyle =
    Style.[
        backgroundColor(Colors.cornflowerBlue),
        position(`Absolute),
        left(100),
        top(100),
        bottom(100),
        right(100),
        border(Colors.black, 4),
        borderRadius(4.0)
    ];

let innerStyle =
    Style.[
        position(`Absolute),
        left(0),
        top(0),
        bottom(70),
        right(0)
    ]

let textStyle =
  Style.[
    fontSize(30),
    fontFamily("Roboto-Regular.ttf"),
    color(Colors.black),
  ];

let drawNode = (sel, (id, node)) => {
    open Position;
    let bgColor = switch(sel) {
        | Some((id', _)) => if(id == id') {Colors.red} else {Colors.azure}
        | None => Colors.azure
    };
    let pos = node.Graphs.Node.data;
    let style = Style.[
        backgroundColor(bgColor),
        position(`Absolute),
        left(int_of_float(pos.x)),
        top(int_of_float(pos.y)),
        width(40),
        height(30)
    ];
    <View style=style>
        <Text style=textStyle text=Graphs.IntId.string_of_id(id) />
    </View>
}

module ResolvedEdge {
    open Graphs;
    type t('a) = {
        source: (IntId.t, Node.t('a)),
        target: (IntId.t, Node.t('a))
    }
}

// TODO handle self edges
let drawEdge = ((id, edge)) => {
    open ResolvedEdge;
    open Graphs.Node;
    let (_, s) = edge.source;
    let (_, t) = edge.target;
    let v = Position.sub(t.data, s.data);
    let len = Position.abs(v);
    let thickness = 4.;
    let style = Style.[
        backgroundColor(Colors.black),
        position(`Absolute),
        left(int_of_float(s.data.x)),
        top(int_of_float(s.data.y)),
        transform([
            Transform.TranslateX(-. len /. 2.),
            Transform.TranslateY(-. thickness /. 2.),
            Transform.Rotate(Revery.Math.Angle.from_radians(atan2(v.y, v.x))),
            Transform.TranslateX(len /. 2.),
            Transform.TranslateY(thickness /. 2.)
        ]),
        width(int_of_float(Position.abs(v))),
        height(int_of_float(thickness))
    ];
    let label = String.concat("", ["      ", Graphs.IntId.string_of_id(id)]);
    <View style=style>
        <Text text=label style=textStyle />
    </View>
}

let string_of_time = tm => {
    open Unix;
    let h = string_of_int(tm.tm_hour);
    let m = string_of_int(tm.tm_min);
    let s = string_of_int(tm.tm_sec);
    String.concat(":", [h, m, s]);
}

let getNodeAtPos = (pos, graph) => {
    open Graphs
    let nodes = ListGraph.extract(graph).nodes;
    let nearby = ((_, n)) => Position.dist(pos, n.Node.data) < 100.0
    List.find_opt(nearby, nodes);
}

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
                open Graphs;
                let oNode = getNodeAtPos(pos, state.world.graph);
                let (graph, sel) = switch (oNode) {
                    | Some((clickedId, node)) => {
                        let graph = switch(state.world.selectedNode) {
                            | Some((oldSelId, _)) => {
                                print_endline(String.concat(" ", [
                                    "Adding edge between",
                                    IntId.string_of_id(oldSelId),
                                    IntId.string_of_id(clickedId)
                                ]))
                                let id = IntId.get();
                                let edge = Edge.{source: oldSelId, target: clickedId, data: ()}
                                ListGraph.addEdge(id, edge, state.world.graph);
                            }
                            | None => state.world.graph
                        };
                        (graph, Some((clickedId, node)))
                    }
                    | None => {
                        let id = IntId.get();
                        let graph = ListGraph.addNode(id, {data: pos}, state.world.graph);
                        (graph, Some((id, {data: pos})));
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
                        Some(Tick.interval(_ => dispatch(UpdateTime), Seconds(1.)))
                    },
                    hooks
                );
            open Graphs;
            let graph = ListGraph.extract(state.world.graph);
            let nodes = List.map(drawNode(state.world.selectedNode), graph.nodes);
            let resolveEdge = ((id, e)) => {
                open ListGraph;
                open Edge;
                open Util;
                let s = getNode(e.source, state.world.graph) |> fromOption;
                let t = getNode(e.target, state.world.graph) |> fromOption;
                (id, ResolvedEdge.{source: (e.source, s), target: (e.target, t)})
            }
            let edges = List.map(e => drawEdge(resolveEdge(e)), graph.edges);
            let items = List.append(nodes, edges);
            let time = string_of_time(state.time);
            let timeStyle = Style.[
                position(`Absolute),
                left(10),
                bottom(10),
                width(200)
            ];

            let handleClick = evt =>
                switch(refOption) {
                    | Some(ref) => {
                        open NodeEvents;
                        let (bbx, bby, _, _) = Math.BoundingBox2d.getBounds(ref#getBoundingBox());
                        let pos = Position.{x: evt.mouseX -. bbx, y: evt.mouseY -. bby};
                        dispatch(Click(pos));
                    }
                    | None => {
                        print_endline("Error in Main: ref of the inner view is not set!")
                    }
            };
            let element = 
                <View style=outerStyle>
                    <View
                        ref={r => setRefOption(Some(r))}
                        style=innerStyle
                        onMouseDown=handleClick>
                        ...items
                    </View>
                    <View style=timeStyle>
                        <Text text=time style=textStyle />
                    </View>
                </View>;
            (hooks, element)
        });
}