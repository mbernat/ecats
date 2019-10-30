type t = {
    x: float,
    y: float
};

let zero = {x: 0., y: 0.}

let add = (a, b) => {
    x: a.x +. b.x,
    y: a.y +. b.y
}

let sub = (a, b) => {
    x: a.x -. b.x,
    y: a.y -. b.y
}

let scale = (v, s) => {
    x: v.x *. s,
    y: v.y *. s
}

let abs = v => {
    sqrt(v.x**2. +. v.y**2.)
}

let dist = (a, b) => {
    abs(sub(a, b))
}

let random = (bound) => {
    x: Random.float(bound),
    y: Random.float(bound)
}

type with_pos('a) = {
    pos: t,
    data: 'a
}