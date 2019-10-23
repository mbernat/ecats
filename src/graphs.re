/*

We should perhaps make IDs somewhat internal so that we can ensure they are unique.

*/

module type Id {
    type t;
    let get: unit => t;
    let id_of_string: string => t;
    let string_of_id: t => string;
};

module IntId: Id {
    type t = int;
    let next = ref(0);
    let get = () => {
        let id = next^;
        next := id + 1;
        id
    };
    let id_of_string = int_of_string;
    let string_of_id = string_of_int
}

module StringId: Id {
    type t = string;
    let next = ref("a");
    let get = () => {
        let id = next^;
        // TODO generate proper IDs
        next := id;
        id
    };
    let id_of_string = s => s;
    let string_of_id = id => id;
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

module type Graph {
    type node_id;
    type edge_id;
    type t('a, 'b);

    let empty: t('a, 'b);
    let addNode: node_id => Node.t('a) => t('a, 'b) => t('a, 'b);
    let getNode: node_id => t('a, 'b) => option(Node.t('a));
    let addEdge: edge_id => Edge.t(node_id, 'b) => t('a, 'b) => t('a, 'b);
    let extract: t('a, 'b) => list_graph(node_id, edge_id, Node.t('a), Edge.t(node_id, 'b))
}

module ListBased = (NodeId: Id, EdgeId: Id)
: (Graph with type node_id := NodeId.t and type edge_id := EdgeId.t) => {
    type t('a, 'b) = list_graph(NodeId.t, EdgeId.t, Node.t('a), Edge.t(NodeId.t, 'b));

    let empty = {nodes: [], edges: []};

    let addNode = (id, n, g) => {
        {
            nodes: List.cons((id, n), g.nodes),
            edges: g.edges
        }
    }

    let getNode = (id, g) => {
        List.find_opt(((id', _)) => id == id', g.nodes)
            // TODO replace with Option.map from some library
            |> Util.mapOption(((_, n)) => n);
    }

    let addEdge = (id, e, g) => {
        {
            nodes: g.nodes,
            edges: List.cons((id, e), g.edges)
        }
    }

    let extract = g => g;
}

module ListGraph = ListBased(IntId, IntId);