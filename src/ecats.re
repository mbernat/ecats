module type Graph {
    type t;

    type v;
    type e;

    let empty: t;
    let addVertex: v => t => t;
}

module Graph2 : Graph = {
    type t = list(int);

    type v = int;
    type e = (int, int);

    let empty = [];
    let addVertex = List.cons;
}
