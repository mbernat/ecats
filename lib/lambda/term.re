module Id {
    type t = Free(string) | Bound(int)
    let to_string = id => switch(id) {
        | Free(n) => n
        | Bound(x) => string_of_int(x)
    }
}

module Unique {
    let cur = ref(0)
    let get = () => {
        let now = cur^;
        cur := now + 1;
        now
    }
}

type t = Var(Id.t) | Lam(Id.t, t) | App(t, t)

let lam = f => {
    let x = Id.Bound(Unique.get())
    Lam(x, f((Var(x))))
}

let free = n => Var(Free(n))

let ex0 = Var(Free("x"))
let ex = App(lam(x => App(x, free("e"))), free("f"))
let ex2 = App(App(free("e"), free("f")), App(free("g"), free("h")))
let ex3 = App(lam(x => App(x, x)), lam(x => App(x, x)))

/*
Two ways to do substitutions:
a) copy the term
b) add reference edges to it; we won't do this at first, partially because don't yet support non-tree layouts,
   but it's actually a much better way to do it since graphs won't be changing as dramatically;
   also, adding references steps outside the simple term syntax.
*/

/*
Substitutes variable `x` for term `e` in `f`.
Problems:
a) need to keep track which bindings in `f` are for `x` itself and which are introduced by other lambdas
b) the free variables in `e` might become captured in `f`
Solutions:
a1) when walking the tree, ignore the subtrees with lambdas shadowing `x`
a2) prevent shadowing by always requiring fresh variables in lambdas
b1) need to collect all free variables in `e` and then replace all binders for them in `f` with fresh variables
    this can 
b2) distinguish free and bound variables so that we can never confuse them
*/
let rec sub = (f, x, e) => switch(f) {
    | Var(y) => if (x == y) e else Var(y)
    | Lam(y, g) => sub(g, x, e)
    | App(g, h) => App(sub(g, x, e), sub(h, x, e))
}

// Lazy semantics, we only ever evaluate the first argument of `App`
let rec step = t => switch(t) {
    | Var(x) => Var(x)
    | Lam(id, t) => Lam(id, t)
    | App(x, e) => switch(x) {
        | Var(y) => App(x, e)
        | App(y, f) => App(step(x), e)
        | Lam(y, f) => sub(f, y, e)
    }
}

let rec alpha_eq = (e, f) => switch((e, f)) {
    | (Var(x), Var(y)) => x == y
    | (Lam(id1, e), Lam(id2, f)) => {
        let v = Var(Bound(Unique.get()));
        alpha_eq(sub(e, id1, v), sub(f, id2, v))
    }
    | (App(e1, e2), App(f1, f2)) => alpha_eq(e1, f1) && alpha_eq(e2, f2)
    | (_, _) => false
}

let rec to_string = t => switch(t) {
    | Var(id) => Id.to_string(id)
    | Lam(id, e) => "(\\" ++ Id.to_string(id) ++ " -> " ++ to_string(e) ++ ")"
    | App(f, e) => "(" ++ to_string(f) ++ " @ " ++ to_string(e) ++ ")"
}

let rec debug_print = (n, t) => {
    print_endline(to_string(t));
    let next = step(t)
    if (n > 0 && !alpha_eq(t, next)) debug_print(n-1, next)
}