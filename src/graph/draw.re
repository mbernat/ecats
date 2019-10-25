open Revery;
open Revery.UI;

let node = (oSel, node) => {
    open Position;
    let bgColor = switch(oSel) {
        | Some(sel) => if(sel == node) {Colors.red} else {Colors.azure}
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
        <Text style=Styles.text text=Space.NodeId.string_of_id(node.Graphs.Node.id) />
    </View>
}

// TODO handle vertices that are too close together
let edge = (edge) => {
    open Graphs.Edge;
    let s = edge.source;
    let t = edge.target;
    let s_pos = s.Graphs.Node.data;
    let t_pos = t.Graphs.Node.data;
    let v = Position.sub(t_pos, s_pos);
    let len = Position.abs(v);
    let thickness = 4.;
    let style = Style.[
        backgroundColor(Colors.black),
        position(`Absolute),
        left(int_of_float(s_pos.x)),
        top(int_of_float(s_pos.y)),
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
    let label =
        if (s.id != t.id)
            String.concat("", ["      ", Space.EdgeId.string_of_id(edge.Graphs.Edge.id)])
        else
            "o";
    <View style=style>
        <Text text=label style=Styles.text />
    </View>
}