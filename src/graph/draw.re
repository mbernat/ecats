open Revery;
open Revery.UI;
open Graphs;

module MkDraw(NodeId: Id, EdgeId: Id) = {
    let node = (oSel, node) => {
        open Vec;
        open Space;
        let bgColor = switch(oSel) {
            | Some(sel) => if(sel == node) {Colors.red} else {Colors.azure}
            | None => Colors.azure
        };
        let pos = node.Graphs.Node.data.pos;
        let style = Style.[
            backgroundColor(bgColor),
            position(`Absolute),
            left(int_of_float(pos.x)),
            top(int_of_float(pos.y)),
            height(30)
        ];
        <View style=style>
            <Text style=Styles.text text=NodeId.string_of(node.Graphs.Node.id) />
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
        open Graphs.Edge;
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
            String.concat("", ["      ", EdgeId.string_of(edge.Graphs.Edge.id)]);
        <View style=style>
            <Text text=label style=Styles.text />
        </View>
    }

    let edge = (edge) => {
        open Graphs.Edge;
        open Vec;
        let s = edge.source;
        let t = edge.target;
        let s_pos = s.Graphs.Node.data.pos;
        let t_pos = t.Graphs.Node.data.pos;
        let v = sub(t_pos, s_pos);

        // TODO draw multiple loops nicely
        if (s.id == t.id) {
            self_loop(s_pos, Colors.yellow)
        // TODO draw short edges nicely
        } else if (abs(v) < 50.) {
            self_loop(s_pos, Colors.green)
        } else {
            standard_edge(edge, s_pos, v)
        }
    }
}