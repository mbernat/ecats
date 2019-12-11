module type S = {
    type t;

    let allocate: unit => t;
    let string_of: t => string;

    let compare: t => t => int;
    let hash: t => int;
    let equal: t => t => bool;
    // Only using this because ocamlgraph requires it for labeled edges
    let default: t;
}

module MkInt(): S = {
    type t = int;

    let id = ref(0);
    let allocate = () => { incr(id); id^ }
    let string_of = string_of_int

    let compare = compare;
    let hash = Hashtbl.hash;
    let equal = (==);
    let default = 0
}
