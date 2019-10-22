let mapOption = (f, o) =>
    switch(o) {
        | Some(x) => Some(f(x))
        | None => None
    }