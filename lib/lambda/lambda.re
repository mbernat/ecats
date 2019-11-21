type id = Free(string) | Bound(int)
module Unique {
    let cur = ref(0)
    let get = () => {
        let now = cur^;
        cur := now + 1;
        now
    }
}

type t = Var(id) | Lam(id, t) | App(t, t)

let lam = f => {
    let x = Bound(Unique.get())
    Lam(x, f((Var(x))))
}

let free = n => Var(Free(n))

let ex = App(lam(x => App(x, free("e"))), free("f"))

/*
Two ways to do substitutions:
a) literally copy the term
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
let sub = (f, x, e) => e

// Lazy semantics, we only ever evaluate the first argument of `App`
let rec step = t => switch(t) {
    | Var(x) => Var(x)
    | Lam(id, t) => Lam(id, t)
    | App(x, e) => switch(x) {
        | Var(y) => Var(y)
        | App(y, f) => App(step(x), e)
        | Lam(y, f) => sub(f, y, e)
    }
}