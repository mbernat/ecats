open Term
open Common
open Graphs
open Data

module Name = {
    type t = Var(Term.Id.t) | Lam(Term.Id.t) | App
}

module Node = {
    type t = {
        name: Name.t,
        pos: Vec.t
    }

    let to_string = n => switch(n.name) {
        | Var(x) => "Var " ++ Term.Id.to_string(x)
        | Lam(x) => "Lam " ++ Term.Id.to_string(x)
        | App => "@"
    }
}

module Order = {
    type t = First | Second
}

module Rooted = {
    type t = {
        graph: Graphs.Data.t(Node.t, Order.t),
        root: NodeId.t
    }
}

module Box = {
    type t = {
        top_left: Vec.t,
        bottom_right: Vec.t
    }

    let mid_point = b => Vec.(scale(add(b.top_left, b.bottom_right), 0.5))

    let split_vertically = (ratio, b) => {
        open Vec
        let diff = ratio *. (b.bottom_right.y -. b.top_left.y);
        (
            {
                ...b,
                bottom_right: {...b.bottom_right, y: b.top_left.y +. diff}
            },
            {
                ...b,
                top_left: {...b.top_left, y: b.top_left.y +. diff}
            }
        )
    }

    let split_horizontally = (ratio, b) => {
        open Vec
        let diff = ratio *. (b.bottom_right.x -. b.top_left.x);
        (
            {
                ...b,
                bottom_right: {...b.bottom_right, x: b.top_left.x +. diff}
            },
            {
                ...b,
                top_left: {...b.top_left, x: b.top_left.x +. diff}
            }
        )
    }
}

let reposition = (source_box, target_box, v) => {
    open Vec
    open Box
    let rel = sub(v, source_box.top_left)
    let source_diff = sub(source_box.bottom_right, source_box.top_left)
    let target_diff = sub(target_box.bottom_right, target_box.top_left)
    let new_rel = {
        x: rel.x *. target_diff.x /. source_diff.x,
        y: rel.y *. target_diff.y /. source_diff.y
    }
    add(target_box.top_left, new_rel)
}

let rec of_term = (box, t) => switch(t) {
    | Var((), x) => {
        let root = NodeId.allocate();
        let node = Node.{name: Name.Var(x), pos: Box.mid_point(box)}
        let graph = add_node(root, node, empty);
        Rooted.{graph, root}
    }
    | Lam((), x, e) => {
        let root = NodeId.allocate();
        open Rooted
        open Box
        let (top, bottom) = split_vertically(0.2, box)
        let body = of_term(bottom, e);

        let node = Node.{name: Name.Lam(x), pos: mid_point(top)}
        let graph = body.graph
            |> add_node(root, node)
            |> add_edge(EdgeId.allocate(), Order.First, root, body.root);
        {graph, root}
    }
    | App((), e1, e2) => {
        let root = NodeId.allocate();
        open Rooted
        open Box
        let (top, bottom) = split_vertically(0.2, box)
        let (left, right) = split_horizontally(0.5, bottom)
        let lam = of_term(left, e1);
        let arg = of_term(right, e2);
        let node = Node.{name: Name.App, pos: mid_point(top)}
        let graph = disjoint_union(lam.graph, arg.graph)
            |> add_node(root, node)
            |> add_edge(EdgeId.allocate(), Order.First, root, lam.root)
            |> add_edge(EdgeId.allocate(), Order.Second, root, arg.root);
        {graph, root}
    }
}

module Annotated = {
    type t = {
        id: NodeId.t,
        pos: Vec.t
    }
}

let rec of_annotated_term = t => switch(t) {
    | Var(a, x) => {
        let root = NodeId.allocate()
        let node = Node.{name: Name.Var(x), pos: a.Annotated.pos}
        let graph = add_node(root, node, empty);
        Rooted.{graph, root}
    }
    | Lam(a, x, e) => {
        let root = NodeId.allocate()
        open Rooted
        let body = of_annotated_term(e);
        let node = Node.{name: Name.Lam(x), pos: a.pos}
        let graph = body.graph
            |> add_node(root, node)
            |> add_edge(EdgeId.allocate(), Order.First, root, body.root);
        {graph, root}
    }
    | App(a, e1, e2) => {
        let root = NodeId.allocate()
        open Rooted
        open Box
        let lam = of_annotated_term(e1);
        let arg = of_annotated_term(e2);
        let node = Node.{name: Name.App, pos: a.pos}
        let graph = disjoint_union(lam.graph, arg.graph)
            |> add_node(root, node)
            |> add_edge(EdgeId.allocate(), Order.First, root, lam.root)
            |> add_edge(EdgeId.allocate(), Order.Second, root, arg.root);
        {graph, root}
    }
}

exception ViewBadChildren(Name.t, list(G.edge))

let view = rooted => {
    open Rooted
    let graph = rooted.graph
    let rec view' = id => {
        let node = NodeMap.find(id, graph.nodes)
        open Node
        let annot = Annotated.{id: id, pos: node.Node.pos}
        open Name
        switch(node.name) {
            | Var(x) => Term.Var(annot, x)
            | Lam(x) => {
                let children = G.succ_e(graph.graph, id)
                switch(children) {
                    | [(_, _, c)] => Term.Lam(annot, x, view'(c))
                    | cs => raise(ViewBadChildren(node.name, cs))
                }
            }
            | App => {
                let children = G.succ_e(graph.graph, id)
                switch(children) {
                    | [(_, l1, c1), (_, l2, c2)] as cs => {
                        let e1 = EdgeMap.find(l1, graph.edges)
                        let e2 = EdgeMap.find(l2, graph.edges)
                        let (c1, c2) = switch((e1, e2)) {
                            | ((Order.First, Order.Second)) => (c1, c2)
                            | ((Order.Second, Order.First)) => (c2, c1)
                            | _ => raise(ViewBadChildren(node.name, cs))
                        }
                        Term.App(annot, view'(c1), view'(c2))
                    }
                    | cs => raise(ViewBadChildren(node.name, cs))
                }
            }
        }
    }
    view'(rooted.root)
}