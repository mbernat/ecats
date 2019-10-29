/*

Layout a tree

The point here is just to have reasonable graphs I can play with.

In general, layouting will have various forms (tree, circle, force, etc.) and each of them will take their own parameters.

Moreover, layouting can be one-shot or dynamic.
One-shot: generate initial positions.
Dynamic: update the positions.

In fact, dynamic layouting should be considered as simply physics and doesn't properly belong here.
We can treat it as one-shot by preparing the initial positions (perhaps random), letting the graph evolve for some time, and returning the final state.

*/

open Graphs
module WithGraph(NodeId: Id, EdgeId: Id, G: WithIds(NodeId, EdgeId).Graph) {
    module type Layout {
        type params;

        let layout: params => G.t('a, 'b) => G.t(Space.with_pos('a), 'b)
    }

// TODO accept a bounding box in which to position the graph as an argument
    module Random: (Layout with type params := unit) {
        let layout = ((), g) => {
            let top_left = 200.;
            let extent = 1000.;
            let f = n => {
                let x = top_left +. Random.float(extent);
                let y = top_left +. Random.float(extent);
                Space.{data: n, pos: Position.{x: x, y: y}}
            }
            G.map_nodes(f, g)
        }
    }

    module FR {
        type params = {
            root: NodeId.t
        }

        module type Layout = Layout with type params := params
    }

    /*
    Layout the graph starting from the given node and ordering nodes by their distance from it
    The nodes not connected to the root will be layouted randomly

    Currently this only orders the vertical position, the horizontal position is still random
    
    A better algorithm: layout the subtree of every child; then reposition them relative to the parent
    This sounds very much like job for Revery, there's no point duplicating all the tree management.
    */
    module FromRoot: FR.Layout {
        module R = Random
        module W = WithIds(NodeId, EdgeId);

        let layout = (p, g) => {
            let distances = G.distances_from(p.FR.root, g);
            let spacing = 200.;
            let top = 200.;
            let f = n => {
                open Node
                let dist_node = G.find_node(n.id, distances) |> Util.fromOption;
                open Space
                let pos = switch(dist_node.data) {
                    | Some(d) => Position.{y: top +. float_of_int(d) *. spacing, x: n.data.pos.x}
                    | None => n.data.pos;
                };
                {data: n.data.data, pos: pos}
            };
            let g' = R.layout((), g);
            G.mapi_nodes(f, g')
        }
    }
}