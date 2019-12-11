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

type t'('a) = Var('a, Id.t) | Lam('a, Id.t, t'('a)) | App('a, t'('a), t'('a))

type t = t'(unit)

let var = id => Var((), id)
let free = n => var(Free(n))

let lam = f => {
    let x = Id.Bound(Unique.get())
    Lam((), x, f(var(x)))
}

let app = (e1, e2) => App((), e1, e2)

let ex0 = free("x")
let ex = app(lam(x => app(x, free("e"))), free("f"))
let ex2 = app(app(free("e"), free("f")), app(free("g"), free("h")))
let ex3 = app(lam(x => app(x, x)), lam(x => app(x, x)))

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
let rec sub_simple = (f, x, e) => switch(f) {
    | Var(a, y) => if (x == y) e else Var(a, y)
    | Lam(a, y, g) => sub_simple(g, x, e)
    | App(a, g, h) => App(a, sub_simple(g, x, e), sub_simple(h, x, e))
}

// Lazy semantics, we only ever evaluate the first argument of `App`
let rec step_sub = (sub, t) => switch(t) {
    | Var(a, x) => Var(a, x)
    | Lam(a, id, t) => Lam(a, id, t)
    | App(a, x, e) => switch(x) {
        | Var(b, y) => App(a, x, e)
        | App(b, y, f) => App(a, step_sub(sub, x), e)
        // TODO use a and b in the substitution
        | Lam(b, y, f) => sub(a, b, f, y, e)
    }
}

let step = step_sub(((), ()) => sub_simple)

let rec alpha_eq = (e, f) => switch((e, f)) {
    | (Var(_, x), Var(_, y)) => x == y
    | (Lam(a, id1, e), Lam(_, id2, f)) => {
        // TODO this is a bit dirty, we reuse an annotation because we're ignoring them anyway
        let v = Var(a, Bound(Unique.get()));
        alpha_eq(sub_simple(e, id1, v), sub_simple(f, id2, v))
    }
    | (App(_, e1, e2), App(_, f1, f2)) => alpha_eq(e1, f1) && alpha_eq(e2, f2)
    | (_, _) => false
}

let rec to_string = t => switch(t) {
    | Var(_, id) => Id.to_string(id)
    | Lam(_, id, e) => "(\\" ++ Id.to_string(id) ++ " -> " ++ to_string(e) ++ ")"
    | App(_, f, e) => "(" ++ to_string(f) ++ " @ " ++ to_string(e) ++ ")"
}

let rec debug_print = (n, t) => {
    print_endline(to_string(t));
    let next = step(t)
    if (n > 0 && !alpha_eq(t, next)) debug_print(n-1, next)
}