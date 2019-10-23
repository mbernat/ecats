let mapOption = (f, o) =>
    switch(o) {
        | Some(x) => Some(f(x))
        | None => None
    }

exception OptionIsNone

let fromOption = o =>
    switch(o) {
        | Some(x) => x
        | None => raise(OptionIsNone)
    }