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

let foo = 1;

open Graphs

module type Bar {
    type a;
    type b;
};

module MkSimpleLayout (
    NodeId: Id, 
    EdgeId: Id,
    G: Graph
        with type node_id := NodeId.t
        and type edge_id := EdgeId.t
        and type n('a) := Node.t(NodeId.t, 'a)
        and type e('a) := Edge.t(EdgeId.t, NodeId.t, 'a)
        and type re('a, 'b) := Edge.t(EdgeId.t, Node.t(NodeId.t, 'a), 'b)
    ) 
    : Layout {
    type g('a, 'b) = G.t('a, 'b);

    let layout = g => {
        let f = n => Space.{data: n, pos: Position.zero};
        G.map_nodes(f, g)
    }
}