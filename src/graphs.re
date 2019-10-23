module type Id {
    type t;
    let get: unit => t;
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
    let string_of_id = string_of_int
}

type list_graph('a, 'b) = {
    nodes: list('a),
    edges: list('b)
};

module Node {
    type t('a) = {
        data: 'a
    }
}

module Edge {
    type t('a, 'b) = {
        source: 'a,
        target: 'a,
        data: 'b
    }
}

module type Graph {
    type t('a, 'b);

    let empty: t('a, 'b);
    let addNode: Node.t('a) => t('a, 'b) => t('a, 'b);
    let addEdge: Edge.t(Node.t('a), 'b) => t('a, 'b) => t('a, 'b);
    let extract: t('a, 'b) => list_graph(Node.t('a), Edge.t(Node.t('a), 'b))
}

module ListGraph : Graph = {
    type t('a, 'b) = list_graph(Node.t('a), Edge.t(Node.t('a), 'b));

    let empty = {nodes: [], edges: []};

    let addNode = (n, g) => {
        {
            nodes: List.cons(n, g.nodes),
            edges: g.edges
        }
    }

    let addEdge = (e, g) => {
        {
            nodes: g.nodes,
            edges: List.cons(e, g.edges)
        }
    }

    let extract = g => g;
}
