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

This module implements only raw physics, graph physics will be done by another module.
Geometry and collisions are not handle at the moment.
*/

open Common

module Point {
    type t('a) = {
        id: 'a,
        pos: Vec.t,
        vel: Vec.t,
        // Forces should probably not be here, they belong to the engine, not the point
        forces: list(Vec.t),
        // Mass == None means infinite mass, i.e. the particle won't move
        mass: option(float)
    }
}

module type Engine {
    type t('a);

    let init: list(Point.t('a)) => t('a);
    let add_point: Point.t('a) => t('a) => t('a);
    let add_force: 'a => Vec.t => t('a) => t('a);
    let step: float => t('a) => t('a);
    let to_list: t('a) => list(Point.t('a));
}

module Engine: Engine {
    type t('a) = list(Point.t('a));

    let init = ps => ps;

    let add_point = List.cons;

    let update = (id, f, e) => {
        List.map(p' => if (p'.Point.id == id) f(p') else p', e)
    }

    let add_force = (id, v, e) => {
        open Point;
        update(id, p => {...p, forces: List.cons(v, p.forces)}, e)
    }

    let step_point = (dt, p) => {
        open Point
        switch(p.mass) {
            | None => p
            | Some(mass) => {
                open Vec
                let force = List.fold_left(add, zero, p.forces);
                let acc = scale(force, (1. /. mass));
                // TODO symplectic integrator 
                let vel' = add(p.vel, scale(acc, dt));
                let pos' = add(p.pos, scale(vel', dt));
                {
                    ...p,
                    pos: pos',
                    vel: vel',
                    forces: []
                }
            }
        }
    }

    let step = (dt, e) => List.map(step_point(dt), e);

    let to_list = e => e;
}

// The first argument specifies the sign and magnitude of the force: r => \pm |F(r)|
// The force is oriented along the pair's axis; positive sign means an attractive force
type radial_force = float => float;

let coulomb_force = (c, r) => c /. r**2.;

// k is the spring constant, l the rest length
let spring_force = (k, l, r) => {
    -. k *. (r -. l)
}

let radial_force = (f, a, b) => {
    open Point
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

module WithEngine(E: Engine) {
    module type Forces {
        let add_uniform: (Point.t('a) => Vec.t) => E.t('a) => E.t('a)
        let add_gravity: Vec.t => E.t('a) => E.t('a);
        let add_all_pairwise: radial_force => E.t('a) => E.t('a);
        let add_pairwise: 'a => 'a => radial_force => E.t('a) => E.t('a);
    }
}

module Forces(E: Engine): WithEngine(E).Forces {
    let add_uniform = (f, e) => {
        let l = E.to_list(e);
        List.fold_left((e, p) => E.add_force(p.Point.id, f(p), e), e, l)
    }

    let add_gravity = (g, e) => add_uniform(_p => g, e)

    let all_pairwise_forces_for = (p, f, e) => {
        open Point
        let forces = List.map(p' => radial_force(f, p.pos, p'.pos), E.to_list(e))
        List.fold_left(Vec.add, Vec.zero, forces)
    }

    // TODO take charge signs into account
    let add_all_pairwise = (f, e) => {
        let l = E.to_list(e);
        List.fold_left((e, p) => E.add_force(p.Point.id, all_pairwise_forces_for(p, f, e), e), e, l)
    }

    // This is unsafe
    let get_point = (id, e) => {
        List.find(p => p.Point.id == id, E.to_list(e));
    }

    // TODO handle wrong IDs
    let add_pairwise = (p_id, q_id, f, e) => {
        let p = get_point(p_id, e);
        let q = get_point(q_id, e);
        e
        |> E.add_force(p_id, radial_force(f, p.pos, q.pos))
        |> E.add_force(q_id, radial_force(f, q.pos, p.pos))
    }
}

module Force = Forces(Engine)