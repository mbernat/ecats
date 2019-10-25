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

type with_pos('a) = {
    data: 'a,
    pos: Position.t
}

module type Layout {
    type g('a, 'b);

    let layout: g('a, 'b) => g(with_pos('a), 'b)
}

let foo = 1;

/*

*/
module MkSimpleLayout (G: Graphs.Graph) : Layout {
    type g('a, 'b) = G.t('a, 'b);

    let layout = g => {
        let f = n => {data: n, pos: Position.zero};
        G.map_nodes(f, g)
    }
}