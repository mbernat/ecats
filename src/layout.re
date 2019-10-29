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

module type Layout {
    type g('a, 'b);

    let layout: g('a, 'b) => g(Space.with_pos('a), 'b)
}

open Graphs;

// TODO accept a bounding box in which to position the graph as an argument
module Random(NodeId: Id, EdgeId: Id, G: WithIds(NodeId, EdgeId).Graph)
    : (Layout with type g('a, 'b) := G.t('a, 'b)) {
    type g('a, 'b) = G.t('a, 'b);

    let layout = g => {
        let f = n => {
            let x = 200. +. Random.float(1000.);
            let y = 200. +. Random.float(1000.);
            Space.{data: n, pos: Position.{x: x, y: y}}
        }
        G.map_nodes(f, g)
    }
}
