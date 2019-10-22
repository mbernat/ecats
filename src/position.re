type t = {
    x: float,
    y: float
};

let dist = (a, b) => {
    sqrt((a.x -. b.x)**2.0 +. (a.y -. b.y)**2.0)
}