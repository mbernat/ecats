open Term
open Common
open Graphs

module O = Graph.Oper.P(G)

let rec of_term = t => switch(t) {
    | Var(x) => {
        let id = NodeId.allocate();
        let g = G.add_vertex(G.empty, id);
        (id, g)
    }
    | Lam(v, e) => {
        let id = NodeId.allocate();
        let (id1, g) = of_term(e);
        let g = G.add_edge_e(g, (id1, EdgeId.allocate(), id));
        (id, g)
    }
    | App(e1, e2) => {
        let id = NodeId.allocate();
        let (id1, g1) = of_term(e1);
        let (id2, g2) = of_term(e2);
        let g = O.union(g1, g2);
        let g = G.add_edge_e(g, (id1, EdgeId.allocate(), id));
        let g = G.add_edge_e(g, (id2, EdgeId.allocate(), id));
        (id, g)
    }
}