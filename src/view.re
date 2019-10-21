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

let renderPosition = pos => {
    open Position;
    let style = Style.[
        backgroundColor(Colors.black),
        position(`Absolute),
        left(int_of_float(pos.x)),
        top(int_of_float(pos.y)),
        width(20),
        height(30)
    ];
    <View style=style />
}

let string_of_time = tm => {
    open Unix;
    let h = string_of_int(tm.tm_hour);
    let m = string_of_int(tm.tm_min);
    let s = string_of_int(tm.tm_sec);
    String.concat(":", [h, m, s]);
}

module Main {
    open World;
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
                time: state.time,
                world: {
                    positions: List.cons(pos, state.world.positions),
                    graph: G.addNode({data: pos}, state.world.graph)
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
            let items = List.map(renderPosition, state.world.positions);
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
                        onMouseDown=handleClick>...items</View>
                    <View style=timeStyle>
                        <Text text=time style=textStyle />
                    </View>
                </View>;
            (hooks, element)
        });
}