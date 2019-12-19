/*

This module provides 2D physical simulations.
Initially this means just point kinematics and dynamics:
- collection of points with properties (position, velocity, mass)
- forces:
    1. at point
    2. pairwise (Coulomb, spring)
    3. global (gravity)
Properties need to be set initially, but can be updated at any time.
Forces have to be provided at every time step.

Physics engine works as follows:
there can be several systems that progressively concretize forces
1. attach force descriptions to nodes and edges
2. convert the descriptions to actual forces acting at points
3. use the forces to step the simulation
*/

open Common

module Point = {
    type t = {
        mass: float,
        position: Vec.t,
        velocity: Vec.t,
    }
}

let step_point = (dt, p, force) => {
    open Point
    open Vec
    let acc = scale(force, (1. /. p.mass));
    // TODO symplectic integrator
    let vel' = add(p.velocity, scale(acc, dt));
    let pos' = add(p.position, scale(vel', dt));
    {
        ...p,
        position: pos',
        velocity: vel'
    }
}

// The first argument specifies the sign and magnitude of the force: r => \pm |F(r)|
// The force is oriented along the pair's axis; positive sign means an attractive force



module Forces = {
    module OneBody = {
        type t = Point.t => Vec.t

        let uniform : Vec.t => t = (f, _) => f
    }

    module Central = {
        type t = float => float

        let coulomb = (c, r) => c /. r**2.;

        // k is the spring constant, l the rest length
        let spring = (k, l, r) => -. k *. (r -. l)

        let apply = (f, a, b) => {
            open Vec

            let v = sub(a, b);
            let r = abs(v);
            let n = scale(v, 1. /. r);
            // TODO what's the best way to handle collisions?
            if (r < 1e-3)
                zero
            else
                scale(n, f(r))
        }
    }

    // This should really be splint into three components
    // Not doing that for now, since it would require a lot of boilerplate
    type t = {
        one_body: list(OneBody.t),
        central: list(Central.t),
        concrete: Vec.t
    }

    /*
    for uniform forces: just add them
    for point forces: nodes, apply them to the physical properties
    for radial forces: edges, get the source and target nodes, compute the force, and apply it to both
    */

}
