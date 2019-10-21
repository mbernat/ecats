
let initialWorld = World.{
    positions: [{x: 100.0, y: 100.0}]
}

let init (app: Revery.App.t) = {
    print_endline("init");
    let window = Revery.App.createWindow(app, "Ecats");
    let element = <View.Main world=initialWorld />
    // NOTE renderFunc can be used to replace the root element
    let _renderFunc = Revery.UI.start(window, element);
}

// NOTE this gets called when Revery hasn't rendered anything for several frames
let idle () = {
    ()
}

Revery.App.start(~onIdle=idle, init)

