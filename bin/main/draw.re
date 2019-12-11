open Revery
open Revery.UI
open Common
open Etymology
open World

let node = (oSel, (id, node)) => {
    open Vec;
    let bgColor = switch(oSel) {
        | Some((id', _)) => if(id' == id) {Colors.red} else {Colors.azure}
        | None => Colors.azure
    };
    let pos = node.Node.pos;
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
        <Text style=Styles.text text=node.Node.word />
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

let standard_edge = (edge, pos, v) => {
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
    let label =
        String.concat("", ["      ", Graphs.EdgeId.string_of(edge)]);
    <View style=style>
        <Text text=label style=Styles.text />
    </View>
}

let edge = edge => {
    open Vec;
    open ResolvedEdge
    let src = edge.src;
    let dest = edge.dest;
    let src_pos = src.pos;
    let dest_pos = dest.pos;
    let v = sub(dest_pos, src_pos);

    // TODO draw multiple loops nicely
    if (edge.src_id == edge.dest_id) {
        self_loop(src_pos, Colors.yellow)
    // TODO draw short edges nicely
    } else if (abs(v) < 50.) {
        self_loop(src_pos, Colors.green)
    } else {
        standard_edge(edge.id, src_pos, v)
    }
}