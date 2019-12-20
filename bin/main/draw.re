open Revery
open Revery.UI
open Common
open Etymology
open Shared

module ResolvedEdge = {
    type t = {
        label: string,
        src_id: Shared.Id.t,
        dest_id: Shared.Id.t,
        src_pos: Vec.t,
        dest_pos: Vec.t
    }
}

let node = (pos, label, selected) => {
    open Vec;
    let bgColor = if(selected) Colors.red else Colors.azure;
    let style = Style.[
        backgroundColor(bgColor),
        position(`Absolute),
        left(int_of_float(pos.x)),
        top(int_of_float(pos.y)),
        height(70),
        padding(10),
        border(Revery.Colors.black, 4),
        borderRadius(4.)
    ];
    <View style=style>
        <Text style=Styles.text text=label />
    </View>
}

let self_loop = (pos, bgColor) => {
    open Vec;
    let style = Style.[
        backgroundColor(bgColor),
        position(`Absolute),
        left(int_of_float(pos.x)),
        top(int_of_float(pos.y))
    ];
    <View style=style>
        <Text text="o" style=Styles.text />
    </View>
}

let standard_edge = (label, pos, v) => {
    open Vec
    let len = abs(v);
    let thickness = 4.;
    let style = Style.[
        backgroundColor(Colors.black),
        position(`Absolute),
        left(int_of_float(pos.x)),
        top(int_of_float(pos.y)),
        transform([
            Transform.TranslateX(-. len /. 2.),
            Transform.TranslateY(-. thickness /. 2.),
            Transform.Rotate(Revery.Math.Angle.from_radians(atan2(v.y, v.x))),
            Transform.TranslateX(len /. 2.),
            Transform.TranslateY(thickness /. 2.)
        ]),
        width(int_of_float(len)),
        height(int_of_float(thickness))
    ];
    <View style=style>
        <Text text=label style=Styles.text />
    </View>
}

let edge = edge => {
    open Vec;
    open ResolvedEdge
    let src_pos = edge.src_pos;
    let dest_pos = edge.dest_pos;
    let v = sub(dest_pos, src_pos);

    // TODO draw multiple loops nicely
    if (edge.src_id == edge.dest_id) {
        self_loop(src_pos, Colors.yellow)
    // TODO draw short edges nicely
    } else if (abs(v) < 50.) {
        self_loop(src_pos, Colors.green)
    } else {
        standard_edge(edge.label, src_pos, v)
    }
}