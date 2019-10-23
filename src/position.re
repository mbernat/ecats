type t = {
    x: float,
    y: float
};

let sub = (a, b) => {
    x: a.x -. b.x,
    y: a.y -. b.y
}

let abs = v => {
    sqrt(v.x**2. +. v.y**2.)
}

let dist = (a, b) => {
    abs(sub(a, b))
}