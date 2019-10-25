module type Id {
    type t;

    let string_of_id: t => string;
    // TODO figure out how to handle both generated and user-supplied IDs with a single interface
    // then remove this
    let id_of_string: string => t;
}

module MkIntId = (): Id => {
    type t = int;
    let next = ref(0);
    let id_of_string = _ => {
        let id = next^;
        next := id + 1;
        id
    };
    let string_of_id = string_of_int
}

module StringId: Id = {
    type t = string;
    let id_of_string = id => id;
    let string_of_id = id => id;
}

type list_graph('a, 'b) = {
    nodes: list('a),
    edges: list('b)
};

module Node {
    type t('id, 'a) = {
        id: 'id,
        data: 'a
    }
}

module Edge {
    type t('id, 'a, 'b) = {
        id: 'id,
        source: 'a,
        target: 'a,
        data: 'b
    }
}

module type Graph {
    type node_id;
    type edge_id;
    type n('a);
    type e('a);
    type re('a, 'b);
    type t('a, 'b);

    let empty: t('a, 'b);

    let add_node: n('a) => t('a, 'b) => t('a, 'b);
    let find_node: node_id => t('a, 'b) => option(n('a));
    let map_nodes: ('a => 'b) => t('a, 'c) => t('b, 'c);

    let add_edge: e('b) => t('a, 'b) => t('a, 'b);
    let find_edge: edge_id => t('a, 'b) => option(e('b));
    let resolve_edge: e('b) => t('a, 'b) => re('a, 'b);

    let extract: t('a, 'b) => list_graph(n('a), e('b))
}

module MkListGraph = (NodeId: Id, EdgeId: Id)
: (Graph
    with type node_id := NodeId.t
    and type edge_id := EdgeId.t
    and type n('a) := Node.t(NodeId.t, 'a)
    and type e('a) := Edge.t(EdgeId.t, NodeId.t, 'a)
    and type re('a, 'b) := Edge.t(EdgeId.t, Node.t(NodeId.t, 'a), 'b)
)
=> {
    type n('a) = Node.t(NodeId.t, 'a);
    type e('a) = Edge.t(EdgeId.t, NodeId.t, 'a);
    type t('a, 'b) = list_graph(n('a), e('b));

    let empty = {nodes: [], edges: []};

    let add_node = (n, g) => {
        ...g,
        nodes: List.cons(n, g.nodes),
    }

    let find_node = (id, g) => {
        List.find_opt(n => n.Node.id == id, g.nodes)
    }

    let map_nodes = (f, g) => {
        let f = n => Node.{
            id: n.id,
            data: f(n.data)
        };
        {
            ...g,
            nodes: List.map(f, g.nodes)
        }
    }

    let add_edge = (e, g) => {
        ...g,
        edges: List.cons(e, g.edges)
    }

    let find_edge = (id: EdgeId.t, g) => {
        List.find_opt(e => e.Edge.id == id, g.edges)
    }

    let resolve_edge = (e, g) => {
        open Edge;
        {
            id: e.id,
            source: find_node(e.source, g) |> Util.fromOption,
            target: find_node(e.target, g) |> Util.fromOption,
            data: e.data
        }
    }

    let extract = g => g;
}
