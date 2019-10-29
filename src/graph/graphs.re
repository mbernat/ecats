module type Id {
    type t;

    let allocate: unit => t;
    let string_of: t => string;
}

module type ListIds = {
    let ids: list(string)
}

module MkIntId(): Id = {
    type t = int;
    let id = ref(0);
    let allocate = () => { incr(id); id^ }
    let string_of = string_of_int
}

module MkIdFromList(L:ListIds, ()): Id = {
    module M {
        module IntId = MkIntId ();
        let prefix = "gen ";

        type t = string;
        let ids = ref(L.ids);
        let allocate = () => {
            if (List.length(ids^) > 0) {
                let head = List.hd(ids^);
                ids := List.tl(ids^);
                head
            } else {
                // TODO: To be fully correct this should also step over the ids in L.ids
                String.concat("", [prefix, IntId.string_of(IntId.allocate())])
            }
        }
        let string_of = s => s
    };

    type t = M.t;
    let allocate = M.allocate;
    let string_of = M.string_of;
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

module Tree {
    type t('n_id, 'a, 'b) = Leaf(int)
}

module WithIds(NodeId: Id, EdgeId: Id) = {
    type n('a) = Node.t(NodeId.t, 'a);
    type e('b) = Edge.t(EdgeId.t, NodeId.t, 'b);
    type re('a, 'b) = Edge.t(EdgeId.t, n('a), 'b);

    module Tree {
        type t('a, 'b) = Leaf(n('a)) | Branch(list(('b, t('a, 'b))))
    }

    module type Graph {
        type t('a, 'b);

        let empty: t('a, 'b);

        // TODO consider grouping these in a Node submodule
        // TODO throw on duplicate id?
        let add_node: n('a) => t('a, 'b) => t('a, 'b);
        // TODO throw on unknown id?
        let update_node: n('a) => t('a, 'b) => t('a, 'b);
        let find_node: NodeId.t => t('a, 'b) => option(n('a));
        let map_nodes: ('a => 'b) => t('a, 'c) => t('b, 'c);
        let mapi_nodes: (n('a) => 'b) => t('a, 'c) => t('b, 'c);

        let add_edge: e('b) => t('a, 'b) => t('a, 'b);
        let find_edge: EdgeId.t => t('a, 'b) => option(e('b));
        let resolve_edge: e('b) => t('a, 'b) => re('a, 'b);

        let extract: t('a, 'b) => list_graph(n('a), e('b));

        // TODO put this in an algorithms module (implemented using some expressive basic interface)
        let distances_from: NodeId.t => t('a, 'b) => t(option(int), 'b)
        // TODO add this; perhaps there should be two version for incoming and outgoing edges
        // let view_from: NodeId.t => t('a, 'b) => Tree.t('a, 'b)
    }
}

module MkListGraph(NodeId: Id, EdgeId: Id): WithIds(NodeId, EdgeId).Graph = {
    module G = WithIds(NodeId, EdgeId);

    type t('a, 'b) = list_graph(G.n('a), G.e('b));

    let empty = {nodes: [], edges: []};

    let add_node = (n, g) => {
        ...g,
        nodes: List.cons(n, g.nodes),
    }

    let update_node = (n, g) => {
        let update = n' => {
            open Node;
            if (n.id == n'.id) n else n'
        };
        {
            ...g,
            nodes: List.map(update, g.nodes)
        }
    }

    let find_node = (id, g) => {
        List.find_opt(n => n.Node.id == id, g.nodes)
    }

    let mapi_nodes = (f, g) => {
        let f = n => Node.{
            ...n,
            data: f(n)
        };
        {
            ...g,
            nodes: List.map(f, g.nodes)
        }
    }

    let map_nodes = (f, g) => {
        mapi_nodes(n => f(n.data), g)
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

/*
recursive algorithms are easiest to implement on recursive data structures,
one merely walks down the constructors

when one doesn't have such a structure there are two options:
1. one can use non-structural recursion
2. one can create a view of the problem that presents it as a recursive structure;
   this view is simply a tree with adjacent nodes as branches

Here we implement 1., later we can generalize it to create a view as in 2.
*/
/*
if a vertex is not visited, we run the algorithm across its children

I was sort of assuming the graph is undirected. What happens in directed graphs?
*/
    type adjacent('b) = {
        incoming: list(G.e('b)),
        outgoing: list(G.e('b))
    }

    let get_adjacent_edges(id, g) = {
        /*
            split edges into three groups: incoming, outgoing, non-adjacent
        */
        open Edge
        let e' = List.filter(e => e.source == id || e.target == id, g.edges);
        let (inc, out) = List.partition(e => e.target == id, e');
        {
            incoming: inc,
            outgoing: out
        }
    }

    let rec dist_from' = (root, depth, g) => {
        open Node;
        switch(root.data) {
            | None => {
                let g' = update_node({...root, data: Some(depth)}, g);
                let adjacent = get_adjacent_edges(root.id, g');
                let f = (g, e) => {
                    let n = find_node(e.Edge.target, g) |> Util.fromOption;
                    dist_from'(n, depth+1, g);
                };
                List.fold_left(f, g', adjacent.outgoing);
            }
            | Some(_) => g
        }
    }

    let distances_from = (id, g) => {
        let dist_graph = map_nodes(_ => None, g);
        let root = find_node(id, dist_graph);
        switch (root) {
            | None => dist_graph
            | Some(root) => dist_from'(root, 0, dist_graph)
        }
    }

    let extract = g => g;
}

