//! Contification pass
//!
//! Transforms (mutually) tail-recursive functions inside a `Fix` into
//! local continuations (`Letk`) when it is safe and profitable.
//!
//! A set of functions is "contifiable" if:
//! * Every call between them is a tail call (no non–tail uses).
//! * They (collectively) share a single (k_ret, k_handler) continuation
//!   pair for all their external tail calls (captured via `common_return_cont`).
//! * Their bodies do not pass any of the functions themselves as
//!   (non-tail) arguments to primitive calls that would escape.
//!
//! High level pipeline per `Fix`:
//! 1. Recursively transform children (`rec`).
//! 2. Build an internal tail-call graph (edges are tail calls).
//! 3. Find SCCs (mutually recursive groups).
//! 4. For each SCC compute a unique common (return, handler) continuation
//!    pair using `common_return_cont`; discard if ambiguous / conflicting.
//! 5. For each qualifying SCC, call `Term::contify`:
//!    * Replace functions by continuations whose bodies
//!      have their individual (return, handler) continuations substituted
//!      with the shared pair.
//!    * Insert resulting `Letk` as deep as possible near its sole dynamic
//!      use via `push_in` / `push_down` (keeps scope tight).
//!    * Rewrite tail `App` sites targeting contified functions into
//!      `Continue`.
//!
//! This pass runs to a fixed point ([`fixedpoint`]) because contification of
//! one `fix` can expose new opportunities for contification.

use std::cell::Cell;

use rsgc::{
    Gc,
    alloc::{Array, ArrayRef},
    barrier::Unlock,
    cell::Lock,
    traits::IterGc,
};

use crate::{
    cps::{
        Map, Set, SingleValueSet,
        term::{Atom, Cont, ContRef, Expression, Func, FuncRef, Term, TermRef},
    },
    expander::core::LVarRef,
    runtime::Context,
    utils::fixedpoint,
};

/// Entry point: repeatedly applies `rec` until no further contifiable
/// opportunities remain (idempotent fixed point).
pub fn contify<'gc>(ctx: Context<'gc>, t: TermRef<'gc>) -> TermRef<'gc> {
    fixedpoint(t, None)(|&t| rec(ctx, t))
}

/// Recursive traversal that performs contification *inside* each `Fix`
/// after transforming its substructure.
fn rec<'gc>(ctx: Context<'gc>, t: TermRef<'gc>) -> TermRef<'gc> {
    match &*t {
        Term::Let(name, expr, body) => {
            let body = rec(ctx, *body);
            Gc::new(&ctx, Term::Let(*name, *expr, body))
        }

        Term::Letk(ks, body) => {
            let ks = ks
                .iter()
                .map(|k| k.with_body(ctx, rec(ctx, k.body())))
                .collect_gc(&ctx);
            let body = rec(ctx, *body);
            Gc::new(&ctx, Term::Letk(ks, body))
        }

        Term::Fix(funs, body) => {
            // (1) Transform functions & body first so analysis sees normalized children.
            let funs = funs
                .iter()
                .map(|f| f.with_body(ctx, rec(ctx, f.body)))
                .collect_gc(&ctx);
            let body = rec(ctx, *body);

            let fix1 = Gc::new(&ctx, Term::Fix(funs, body));
            // (2) Identify contifiable SCCs (each yields (names, common_rc)).
            let cf = fix1.contifiables();
            // (3) Fold: successively contify each qualifying SCC.

            cf.iter()
                .fold(fix1, |fix, (ns, rc)| fix.contify(ns, *rc, ctx))
        }

        _ => t,
    }
}

impl<'gc> Func<'gc> {
    /// Collects (transitively local) function bindings this function
    /// tail-calls (only direct tail calls counted; used to build the
    /// call graph restricted to sibling bindings inside the same `Fix`).
    pub fn tailcalls(&self) -> Set<LVarRef<'gc>> {
        self.body.tailcalls(self.return_cont)
    }
}

impl<'gc> Term<'gc> {
    /// Traverses term and records any `App` whose
    /// continuation equals the given return continuation,
    /// indicating a tail call site.
    pub fn tailcalls(&self, retc: LVarRef<'gc>) -> Set<LVarRef<'gc>> {
        match self {
            Self::Fix(_, body) | Self::Let(_, _, body) => body.tailcalls(retc),

            Self::Letk(ks, body) => {
                let body_tc = body.tailcalls(retc);
                ks.iter().fold(body_tc, |mut acc, k| {
                    acc.extend(k.body().tailcalls(retc));
                    acc
                })
            }

            Self::App(Atom::Local(f), c, ..) if *c == retc => [*f].into_iter().collect(),

            _ => Set::default(),
        }
    }

    /// Attempts to discover a single common (return_cont, handler_cont) pair
    /// for all *external* tail calls within the given set `ns` of functions.
    ///
    /// The lattice `SingleValueSet`:
    /// * Bottom  : no evidence yet / neutral (continue searching)
    /// * Singleton(x): exactly one consistent pair observed so far
    /// * Top     : conflict (multiple distinct pairs or interfering uses)
    ///
    /// `ignore` temporarily suppresses counting of a function's own
    /// (return, handler) pair during recursive descent in its body
    /// to avoid self-bias when analyzing mutually recursive groups.
    pub fn common_return_cont(
        &self,
        ns: &Set<LVarRef<'gc>>,
        ignore: Option<(LVarRef<'gc>, LVarRef<'gc>)>,
    ) -> SingleValueSet<(LVarRef<'gc>, LVarRef<'gc>)> {
        match self {
            Self::Let(_, expr, body) => match expr {
                Expression::PrimCall(_, args, _, _) => {
                    if args
                        .iter()
                        .filter_map(|arg| match arg {
                            Atom::Local(l) => Some(*l),
                            _ => None,
                        })
                        .any(|arg| ns.contains(&arg))
                    {
                        SingleValueSet::Top
                    } else {
                        body.common_return_cont(ns, ignore)
                    }
                }

                _ => body.common_return_cont(ns, ignore),
            },

            Self::Letk(ks, body) => ks
                .iter()
                .fold(body.common_return_cont(ns, ignore), |acc, k| {
                    acc.join(k.body().common_return_cont(ns, ignore))
                }),

            Self::Fix(funs, body) => {
                funs.iter()
                    .fold(body.common_return_cont(ns, ignore), |acc, fun| {
                        let new_ignore = ns
                            .contains(&fun.binding())
                            .then_some((fun.return_cont, fun.handler_cont));
                        acc.join(fun.body.common_return_cont(ns, new_ignore))
                    })
            }

            Self::Continue(_, args, _) => {
                if args
                    .iter()
                    .filter_map(|arg| match arg {
                        Atom::Local(l) => Some(*l),
                        _ => None,
                    })
                    .any(|arg| ns.contains(&arg))
                {
                    SingleValueSet::Top
                } else {
                    SingleValueSet::Bottom
                }
            }

            Self::App(Atom::Local(fun), retc, reth, args, _) => {
                if args
                    .iter()
                    .filter_map(|arg| match arg {
                        Atom::Local(l) => Some(*l),
                        _ => None,
                    })
                    .any(|arg| ns.contains(&arg))
                {
                    return SingleValueSet::Top;
                }

                if ns.contains(fun) && ignore != Some((*retc, *reth)) {
                    return SingleValueSet::Singleton((*retc, *reth));
                }

                SingleValueSet::Bottom
            }

            Self::App(..) => SingleValueSet::Bottom,

            _ => SingleValueSet::Bottom,
        }
    }

    /// Returns a list of (function-name-set, common_return/handler pair) for
    /// all SCCs in the tail-call graph that are eligible for contification.
    /// SCCs without a unique common continuation pair are discarded.
    pub fn contifiables(&self) -> Vec<(Set<LVarRef<'gc>>, (LVarRef<'gc>, LVarRef<'gc>))> {
        let Term::Fix(funs, _) = self else {
            return vec![];
        };

        let mut graph = petgraph::Graph::new();
        let mut node_map = Map::default();

        let names = funs
            .iter()
            .map(|f| {
                let node = graph.add_node(f.binding());
                node_map.insert(f.binding(), node);
                f.binding
            })
            .collect::<Set<_>>();

        // Build call graph restricted to tail calls among siblings.
        for fun in funs.iter() {
            let tailcalls = fun
                .tailcalls()
                .intersection(&names)
                .cloned()
                .collect::<Vec<_>>();
            let from = node_map[&fun.binding()];
            for to_fun in tailcalls {
                if let Some(&to) = node_map.get(&to_fun) {
                    graph.add_edge(from, to, ());
                }
            }
        }

        // Run Tarjan to obtain SCCs; each SCC is a candidate group.
        let sccs = petgraph::algo::tarjan_scc(&graph);

        sccs.into_iter()
            .map(|scc| {
                let ns = scc
                    .iter()
                    .map(|&node| graph[node])
                    .collect::<Set<LVarRef<'_>>>();
                let c = match self.common_return_cont(&ns, None) {
                    SingleValueSet::Singleton(val) => Some(val),
                    _ => None,
                };

                c.map(|x| (ns, x))
            })
            .flatten()
            .collect()
    }

    /// Contifies the group `ns` (a set of mutually recursive function
    /// bindings) assuming they share the continuation pair `rc`.
    ///
    /// Steps:
    /// 1. Split functions: (to_contify, untouched).
    /// 2. Convert each target function into a `Cont::Local`, substituting
    ///    its individual (return, handler) with the shared `rc` pair.
    /// 3. Rebuild the surrounding `Fix` with any untouched functions.
    /// 4. Insert the new continuations via a `Letk`, pushing them down
    ///    as deep as possible (to narrow scope) using `push_down`.
    /// 5. Rewrite tail `App`s to these functions into `Continue`.
    ///
    /// `rc`: (return_cont, handler_cont) chosen by `common_return_cont`.
    pub fn contify(
        &self,
        ns: &Set<LVarRef<'gc>>,
        rc: (LVarRef<'gc>, LVarRef<'gc>),
        ctx: Context<'gc>,
    ) -> TermRef<'gc> {
        let Term::Fix(funs, body) = self else {
            unreachable!("contify should only be called on a Fix term");
        };

        let (to_contify, untouched): (Vec<_>, Vec<_>) = funs
            .iter()
            .copied()
            .partition(|f| ns.contains(&f.binding()));
        let contified: ArrayRef<'_, ContRef> = to_contify
            .into_iter()
            .map(|f| {
                // Substitute each function's own (ret, handler) pair with the shared one
                let subst_map: Map<LVarRef<'gc>, LVarRef<'gc>> =
                    [(f.return_cont(), rc.0), (f.handler_cont, rc.1)]
                        .into_iter()
                        .collect();

                Gc::new(
                    &ctx,
                    Cont {
                        name: f.name,
                        binding: f.binding,
                        args: f.args,
                        variadic: f.variadic,
                        body: f.body.subst(ctx, &subst_map),
                        source: f.source,
                        free_vars: Lock::new(None),
                        reified: Cell::new(false),
                        handler: Lock::new(rc.1),
                    },
                )
            })
            .collect_gc(&ctx);
        let untouched: ArrayRef<'gc, FuncRef<'gc>> = untouched.into_iter().collect_gc(&ctx);

        let fix = if untouched.is_empty() {
            *body
        } else {
            Gc::new(&ctx, Term::Fix(untouched, *body))
        };

        // Insert Letk as deep as possible; then rewrite Apps to Continue.
        transform_apps(
            push_down(
                ctx,
                &|body| TermRef::new(&ctx, Term::Letk(contified, body)),
                fix,
                ns,
            ),
            ns,
            ctx,
        )
    }
}

/// Recursive helper that attempts to "push" the insertion site of the
/// newly created `Letk` (continuations) as deep as possible while still
/// dominating all uses. Returns (whether wrapped_here, new_term).
fn push_in<'gc>(
    ctx: Context<'gc>,
    wrap_with_cnts: &dyn Fn(TermRef<'gc>) -> TermRef<'gc>,
    t: TermRef<'gc>,
    ns: &Set<LVarRef<'gc>>,
) -> (bool, TermRef<'gc>) {
    match *t {
        Term::Let(name, expr, body) => {
            let (pushed, body1) = push_in(ctx, wrap_with_cnts, body, ns);
            (pushed, TermRef::new(&ctx, Term::Let(name, expr, body1)))
        }

        Term::Letk(ks, body) => {
            let (pushed_k, ks1): (Vec<bool>, Vec<_>) = ks
                .iter()
                .copied()
                .map(|k| {
                    let (pushed, cbody1) = push_in(ctx, wrap_with_cnts, k.body(), ns);

                    (pushed, k.with_body(ctx, cbody1))
                })
                .unzip();
            let ks1 = Array::from_slice(&ctx, ks1);
            let (pushed, body1) = push_in(ctx, wrap_with_cnts, body, ns);
            let pushed_count = pushed as usize + pushed_k.iter().filter(|&&x| x).count();
            // If more than one branch pushed the site, wrap above them
            // so the continuations are in scope for all.
            if pushed_count > 1 {
                (true, wrap_with_cnts(t))
            } else {
                (
                    pushed_count == 1,
                    TermRef::new(&ctx, Term::Letk(ks1, body1)),
                )
            }
        }

        Term::Fix(funs, body) => {
            let (pushed_f, funs1): (Vec<bool>, Vec<_>) = funs
                .iter()
                .copied()
                .map(|f| {
                    let (pushed, body1) = push_in(ctx, wrap_with_cnts, f.body, ns);
                    (pushed, f.with_body(ctx, body1))
                })
                .unzip();

            let (pushed, body1) = push_in(ctx, wrap_with_cnts, body, ns);
            let pushed_count = pushed as usize + pushed_f.iter().filter(|&&x| x).count();
            assert!(pushed_count <= 1, "More than one pushed function in Fix");

            if funs1.is_empty() {
                return (pushed_count == 1, body1);
            }

            (
                pushed_count == 1,
                TermRef::new(&ctx, Term::Fix(Array::from_slice(&ctx, funs1), body1)),
            )
        }

        // Anchor: application using the shared return continuation name.
        Term::App(Atom::Local(f), ..) if ns.contains(&f) => (true, wrap_with_cnts(t)),

        _ => (false, t),
    }
}

/// Wrapper that keeps only the transformed term (dropping the bool flag).
fn push_down<'gc>(
    ctx: Context<'gc>,
    wrap_with_cnts: &dyn Fn(TermRef<'gc>) -> TermRef<'gc>,
    t: TermRef<'gc>,
    ns: &Set<LVarRef<'gc>>,
) -> TermRef<'gc> {
    let res = push_in(ctx, wrap_with_cnts, t, ns);
    assert!(res.0, "push_in should always push down");
    res.1
}

/// Rewrites tail calls (`App`) to contified functions into direct
/// continuation invocations (`Continue`). Traverses structure.
fn transform_apps<'gc>(t: TermRef<'gc>, ns: &Set<LVarRef<'gc>>, ctx: Context<'gc>) -> TermRef<'gc> {
    match *t {
        Term::App(Atom::Local(f), _, _, args, src) if ns.contains(&f) => {
            // Tail call now becomes a continuation jump.
            Gc::new(&ctx, Term::Continue(f, args, src))
        }

        Term::Let(name, expr, body) => {
            let body = transform_apps(body, ns, ctx);
            Gc::new(&ctx, Term::Let(name, expr, body))
        }

        Term::Fix(funs, body) => {
            let funs = funs
                .iter()
                .map(|f| f.with_body(ctx, transform_apps(f.body, ns, ctx)))
                .collect_gc(&ctx);

            let body = transform_apps(body, ns, ctx);

            Gc::new(&ctx, Term::Fix(funs, body))
        }

        Term::Letk(ks, body) => {
            let ks = ks
                .iter()
                .map(|k| k.with_body(ctx, transform_apps(k.body(), ns, ctx)))
                .collect_gc(&ctx);

            let body = transform_apps(body, ns, ctx);

            Gc::new(&ctx, Term::Letk(ks, body))
        }

        _ => t,
    }
}

impl<'gc> Term<'gc> {
    pub fn subst(
        &self,
        ctx: Context<'gc>,
        subst: &Map<LVarRef<'gc>, LVarRef<'gc>>,
    ) -> TermRef<'gc> {
        match self {
            Self::Let(name, expr, body) => {
                let expr = expr.subst(ctx, subst);
                let body = body.subst(ctx, subst);
                TermRef::new(&ctx, Self::Let(*name, expr, body))
            }

            Self::Letk(ks, body) => {
                let ks = ks.iter().map(|k| k.subst(ctx, subst)).collect_gc(&ctx);
                let body = body.subst(ctx, subst);
                TermRef::new(&ctx, Self::Letk(ks, body))
            }

            Self::Fix(funs, body) => {
                let funs = funs.iter().map(|f| f.subst(ctx, subst)).collect_gc(&ctx);
                let body = body.subst(ctx, subst);
                TermRef::new(&ctx, Self::Fix(funs, body))
            }

            Self::App(func, k, h, args, src) => {
                let func = func.subst(ctx, subst);
                let k = subst.get(&*k).copied().unwrap_or(*k);
                let h = subst.get(&*h).copied().unwrap_or(*h);
                let args = args
                    .into_iter()
                    .map(|arg| arg.subst(ctx, subst))
                    .collect_gc(&ctx);
                TermRef::new(&ctx, Self::App(func, k, h, args, *src))
            }

            Self::Continue(k, args, src) => {
                let k = subst.get(&*k).copied().unwrap_or(*k);
                let args = args
                    .into_iter()
                    .map(|arg| arg.subst(ctx, subst))
                    .collect_gc(&ctx);
                TermRef::new(&ctx, Self::Continue(k, args, *src))
            }

            Self::If(test, kcons, kalt, hint) => {
                let test = test.subst(ctx, subst);
                let kcons = subst.get(&*kcons).copied().unwrap_or(*kcons);
                let kalt = subst.get(&*kalt).copied().unwrap_or(*kalt);
                TermRef::new(&ctx, Self::If(test, kcons, kalt, *hint))
            }

            Term::Throw(val, data) => TermRef::new(&ctx, Term::Throw(*val, *data)),
        }
    }
}

impl<'gc> Atom<'gc> {
    pub fn subst(self, _ctx: Context<'gc>, subst: &Map<LVarRef<'gc>, LVarRef<'gc>>) -> Atom<'gc> {
        match self {
            Atom::Local(l) => subst.get(&l).copied().map(Atom::Local).unwrap_or(self),
            _ => self,
        }
    }
}

impl<'gc> Func<'gc> {
    pub fn subst(
        &self,
        ctx: Context<'gc>,
        subst: &Map<LVarRef<'gc>, LVarRef<'gc>>,
    ) -> FuncRef<'gc> {
        let body = self.body.subst(ctx, subst);
        self.with_body(ctx, body)
    }
}

impl<'gc> Cont<'gc> {
    pub fn subst(
        &self,
        ctx: Context<'gc>,
        subst: &Map<LVarRef<'gc>, LVarRef<'gc>>,
    ) -> ContRef<'gc> {
        let handler = subst
            .get(&self.handler.get())
            .copied()
            .unwrap_or(self.handler.get());
        let body = self.body().subst(ctx, subst);
        let k = self.with_body(ctx, body);
        unsafe {
            k.handler.unlock_unchecked().set(handler);
        }
        k
    }
}

impl<'gc> Expression<'gc> {
    pub fn subst(self, ctx: Context<'gc>, subst: &Map<LVarRef<'gc>, LVarRef<'gc>>) -> Self {
        match self {
            Self::PrimCall(name, args, h, src) => {
                let args = args
                    .into_iter()
                    .map(|arg| arg.subst(ctx, subst))
                    .collect_gc(&ctx);
                let h = subst.get(&h).copied().unwrap_or(h);
                Self::PrimCall(name, args, h, src)
            }
            _ => self,
        }
    }
}
