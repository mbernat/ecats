/* Graphs should be polymorphic in the kinds of nodes and edges they contain.
In particular:
there should be optional IDs
there should be optional positions
there should be optional opaque data

for now we'll consider two separate types:
base ones (just ids)
layout ones (together with position)

Graphs should also be polymorphic in the kind of their internal data structures so that
we can pick the right representation for various operations.

Should the IDs be generated or user-supplied?
Both options have merits:
* if the user already has natural IDs, they can just supply them.
* if they don't they could ask the engine to generate them.
*/

module type Id {
    type t;
    let get: unit => t;
};

module IntId: Id {
    type t = int;
    let next = ref(0);
    let get = () => {
        let id = next^;
        next := id + 1;
        id
    }
}

type list_graph('n_id, 'e_id, 'n, 'e) = {
    nodes: list(('n_id, 'n)),
    edges: list(('e_id, 'e))
};

module Node {
    type t('a) = {
        data: 'a
    }
}

module Edge {
    type t('node_id, 'a) = {
        source: 'node_id,
        target: 'node_id,
        data: 'a
    }
}

module type Sig {
    type node_id;
    type edge_id;
    type t('a, 'b);

    let empty: t('a, 'b);
    let addNode: Node.t('a) => t('a, 'b) => t('a, 'b);
    let addEdge: Edge.t(node_id, 'b) => t('a, 'b) => t('a, 'b);
    let extract: t('a, 'b) => list_graph(node_id, edge_id, Node.t('a), Edge.t(node_id, 'b))
};

module ListGraph = (NodeId: Id, EdgeId: Id) 
: (Sig with type node_id := NodeId.t and type edge_id := EdgeId.t) => {
    type t('a, 'b) = list_graph(NodeId.t, EdgeId.t, Node.t('a), Edge.t(NodeId.t, 'b));

    let empty = {nodes: [], edges: []};

    let addNode = n => g => {
        let id = NodeId.get();
        {
            nodes: List.cons((id, n), g.nodes),
            edges: g.edges
        }
    }

    let addEdge = e => g => {
        let id = EdgeId.get();
        {
            nodes: g.nodes,
            edges: List.cons((id, e), g.edges)
        }
    }

    let extract = g => g;
}