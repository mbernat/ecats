
let initialWorld = World.{
    graph: Space.ListGraph.empty,
    selectedNode: None
}

let init (app: Revery.App.t) = {
    print_endline("init");
    let window = Revery.App.createWindow(app, "Ecats");
    let element = <Root.Main world=initialWorld />
    // NOTE renderFunc can be used to replace the root element
    let _renderFunc = Revery.UI.start(window, element);
}

// NOTE this gets called when Revery hasn't rendered anything for several frames
let idle () = {
    ()
}

Revery.App.start(~onIdle=idle, init)

