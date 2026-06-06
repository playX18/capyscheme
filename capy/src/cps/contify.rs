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

use crate::rsgc::{
    Gc,
    alloc::{Array, ArrayRef},
    cell::Lock,
    traits::IterGc,
};

use crate::{
    cps::{
        Map, Set, SingleValueSet, Substitute,
        term::{Atom, Cont, ContRef, Expression, Func, FuncRef, Term, TermRef},
    },
    expander::core::LVarRef,
    runtime::Context,
    utils::pass_profile::ProfileScope,
};

/// Return continuation used for contification.
type ContPair<'gc> = LVarRef<'gc>;

/// Entry point: repeatedly applies `rec` until no further contifiable
/// opportunities remain (idempotent fixed point).
pub fn contify<'gc>(ctx: Context<'gc>, t: TermRef<'gc>) -> TermRef<'gc> {
    let mut total_profile = ProfileScope::new("cps.contify.total");
    let mut current = t;
    let mut rounds = 0;

    for round in 0..2 {
        rounds = round + 1;
        let mut round_profile = ProfileScope::new("cps.contify.round");
        if round_profile.is_enabled() {
            round_profile.field("round", rounds);
        }

        let (next, changed) = rec(ctx, current);
        if round_profile.is_enabled() {
            round_profile.field("changed", changed);
        }

        current = next;
        if !changed {
            break;
        }
    }

    if total_profile.is_enabled() {
        total_profile.field("rounds", rounds);
    }

    current
}

/// Recursive traversal that performs contification *inside* each `Fix`
/// after transforming its substructure.
fn rec<'gc>(ctx: Context<'gc>, t: TermRef<'gc>) -> (TermRef<'gc>, bool) {
    match &*t {
        Term::Let(name, expr, prev_body) => {
            let (body, changed) = rec(ctx, *prev_body);
            if !changed {
                return (t, false);
            }
            (Gc::new(*ctx, Term::Let(*name, *expr, body)), true)
        }

        Term::Letk(prev_ks, prev_body) => {
            let mut changed = false;
            let ks = prev_ks
                .iter()
                .map(|k| {
                    let (body, body_changed) = rec(ctx, k.body());
                    changed |= body_changed;
                    if body_changed {
                        k.with_body(ctx, body)
                    } else {
                        *k
                    }
                })
                .collect::<Vec<_>>();
            let ks = if ks.iter().zip(prev_ks.iter()).all(|(a, b)| a == b) {
                *prev_ks
            } else {
                changed = true;
                Array::from_slice(*ctx, ks)
            };

            let (body, body_changed) = rec(ctx, *prev_body);
            changed |= body_changed;
            if !changed {
                return (t, false);
            }
            (Gc::new(*ctx, Term::Letk(ks, body)), true)
        }

        Term::Fix(funs, body) => {
            // (1) Transform functions & body first so analysis sees normalized children.
            let mut changed = false;
            let funs = funs
                .iter()
                .map(|f| {
                    let (body, body_changed) = rec(ctx, f.body());
                    changed |= body_changed;
                    if body_changed {
                        f.with_body(ctx, body)
                    } else {
                        *f
                    }
                })
                .collect_gc(*ctx);
            let (body, body_changed) = rec(ctx, *body);
            changed |= body_changed;

            let fix1 = if changed {
                Gc::new(*ctx, Term::Fix(funs, body))
            } else {
                t
            };

            let func_count = funs.len();

            let (sccs, cf) = {
                let mut analysis_profile = ProfileScope::new("cps.contify.fix.analysis");
                analysis_profile.field("func_count", func_count);

                let sccs = {
                    let mut scc_profile = ProfileScope::new("cps.contify.fix.scc");
                    scc_profile.field("func_count", func_count);
                    let sccs = fix1.tailcall_sccs();
                    scc_profile.field("scc_count", sccs.len());
                    sccs
                };
                analysis_profile.field("scc_count", sccs.len());

                let cf = {
                    let mut eligibility_profile = ProfileScope::new("cps.contify.fix.eligibility");
                    eligibility_profile.field("func_count", func_count);
                    eligibility_profile.field("scc_count", sccs.len());
                    let cf = fix1.eligible_contifiables(&sccs);
                    eligibility_profile.field("eligible_scc_count", cf.len());
                    cf
                };
                analysis_profile.field("eligible_scc_count", cf.len());

                (sccs, cf)
            };
            if cf.is_empty() {
                return (fix1, changed);
            }

            // (3) Fold: successively contify each qualifying SCC.
            let transformed = {
                let mut transform_profile = ProfileScope::new("cps.contify.fix.transform");
                transform_profile.field("func_count", func_count);
                transform_profile.field("scc_count", sccs.len());
                transform_profile.field("eligible_scc_count", cf.len());
                cf.iter()
                    .fold(fix1, |fix, (ns, rc)| fix.contify(ns, *rc, ctx))
            };

            (transformed, true)
        }

        _ => (t, false),
    }
}

impl<'gc> Func<'gc> {
    /// Collects (transitively local) function bindings this function
    /// tail-calls (only direct tail calls counted; used to build the
    /// call graph restricted to sibling bindings inside the same `Fix`).
    pub fn tailcalls(&self) -> Set<LVarRef<'gc>> {
        self.body().tailcalls(self.return_cont)
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

            Self::App(Atom::Local(f), c, m, ..) if *c == retc => [*f].into_iter().collect(),

            _ => Set::default(),
        }
    }

    /// Attempts to discover a single common return continuation
    /// for all *external* tail calls within the given set `ns` of functions.
    ///
    /// The lattice `SingleValueSet`:
    /// * Bottom  : no evidence yet / neutral (continue searching)
    /// * Singleton(x): exactly one consistent pair observed so far
    /// * Top     : conflict (multiple distinct pairs or interfering uses)
    ///
    /// `ignore` temporarily suppresses counting of a function's own
    /// return continuation during recursive descent in its body
    /// to avoid self-bias when analyzing mutually recursive groups.
    pub fn common_return_cont(
        &self,
        ns: &Set<LVarRef<'gc>>,
        ignore: Option<ContPair<'gc>>,
    ) -> SingleValueSet<ContPair<'gc>> {
        match self {
            Self::Let(_, expr, body) => match expr {
                Expression::PrimCall(_, args, _) => {
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
            },

            Self::Letk(ks, body) => ks
                .iter()
                .fold(body.common_return_cont(ns, ignore), |acc, k| {
                    acc.join(k.body().common_return_cont(ns, ignore))
                }),

            Self::Fix(funs, body) => {
                funs.iter()
                    .fold(body.common_return_cont(ns, ignore), |acc, fun| {
                        let new_ignore = ns.contains(&fun.binding()).then_some(fun.return_cont);
                        acc.join(fun.body().common_return_cont(ns, new_ignore))
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

            Self::Raise { args, .. } => {
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

            Self::App(Atom::Local(fun), retc, args, _) => {
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

                if ns.contains(fun) && ignore != Some(*retc) {
                    return SingleValueSet::Singleton(*retc);
                }

                SingleValueSet::Bottom
            }

            Self::App(..) => SingleValueSet::Bottom,

            _ => SingleValueSet::Bottom,
        }
    }

    /// Returns a list of (function-name-set, common return continuation) for
    /// all SCCs in the tail-call graph that are eligible for contification.
    /// SCCs without a unique common continuation pair are discarded.
    fn tailcall_sccs(&self) -> Vec<Set<LVarRef<'gc>>> {
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
            let tailcalls = fun.tailcalls().intersection(names.clone());

            let from = node_map[&fun.binding()];
            for to_fun in tailcalls {
                if let Some(&to) = node_map.get(&to_fun) {
                    graph.add_edge(from, to, ());
                }
            }
        }

        // Run SCC algorithm; each SCC is a candidate group.
        petgraph::algo::kosaraju_scc(&graph)
            .into_iter()
            .map(|scc| {
                scc.iter()
                    .map(|&node| graph[node])
                    .collect::<Set<LVarRef<'_>>>()
            })
            .collect()
    }

    fn eligible_contifiables(
        &self,
        sccs: &[Set<LVarRef<'gc>>],
    ) -> Vec<(Set<LVarRef<'gc>>, ContPair<'gc>)> {
        sccs.iter()
            .cloned()
            .filter_map(|ns| match self.common_return_cont(&ns, None) {
                SingleValueSet::Singleton(val) => Some((ns, val)),
                _ => None,
            })
            .collect()
    }

    pub fn contifiables(&self) -> Vec<(Set<LVarRef<'gc>>, ContPair<'gc>)> {
        let sccs = self.tailcall_sccs();
        self.eligible_contifiables(&sccs)
    }

    /// Contifies the group `ns` (a set of mutually recursive function
    /// bindings) assuming they share the return continuation `rc`.
    ///
    /// Steps:
    /// 1. Split functions: (to_contify, untouched).
    /// 2. Convert each target function into a `Cont::Local`, substituting
    ///    its individual return continuation with the shared `rc`.
    /// 3. Rebuild the surrounding `Fix` with any untouched functions.
    /// 4. Insert the new continuations via a `Letk`, pushing them down
    ///    as deep as possible (to narrow scope) using `push_down`.
    /// 5. Rewrite tail `App`s to these functions into `Continue`.
    ///
    /// `ns`: set of function bindings to contify.
    /// `rc`: return_cont chosen by `common_return_cont`.
    pub fn contify(
        &self,
        ns: &Set<LVarRef<'gc>>,
        rc: ContPair<'gc>,
        ctx: Context<'gc>,
    ) -> TermRef<'gc> {
        let Term::Fix(funs, body) = self else {
            unreachable!("contify should only be called on a Fix term");
        };
        let mut profile = ProfileScope::new("cps.contify.fix.contify");
        if profile.is_enabled() {
            profile.field("fix_func_count", funs.len());
            profile.field("contified_func_count", ns.len());
        }

        let (to_contify, untouched): (Vec<_>, Vec<_>) = funs
            .iter()
            .copied()
            .partition(|f| ns.contains(&f.binding()));
        let contified: ArrayRef<'_, ContRef> = to_contify
            .into_iter()
            .map(|f| {
                let subst_map: Map<LVarRef<'gc>, LVarRef<'gc>> =
                    [(f.return_cont(), rc)].into_iter().collect();

                Gc::new(
                    *ctx,
                    Cont {
                        name: f.name,
                        noinline: false,
                        binding: f.binding,
                        args: f.args,
                        variadic: f.variadic,
                        body: Lock::new(f.body().subst(ctx, &subst_map)),
                        source: f.source,

                        free_vars: Lock::new(None),
                        reified: Cell::new(false),
                        cold: false,
                        meta: f.meta,
                    },
                )
            })
            .collect_gc(*ctx);
        let untouched: ArrayRef<'gc, FuncRef<'gc>> = untouched.into_iter().collect_gc(*ctx);

        let fix = if untouched.is_empty() {
            *body
        } else {
            Gc::new(*ctx, Term::Fix(untouched, *body))
        };

        // Insert Letk as deep as possible; then rewrite Apps to Continue.
        let pushed = {
            let mut push_down_profile = ProfileScope::new("cps.contify.fix.push_down");
            if push_down_profile.is_enabled() {
                push_down_profile.field("contified_func_count", ns.len());
            }
            push_down(
                ctx,
                &|body| TermRef::new(*ctx, Term::Letk(contified, body)),
                fix,
                ns,
            )
        };

        {
            let mut transform_profile = ProfileScope::new("cps.contify.fix.transform_apps");
            if transform_profile.is_enabled() {
                transform_profile.field("contified_func_count", ns.len());
            }
            transform_apps(pushed, ns, ctx)
        }
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
            (pushed, TermRef::new(*ctx, Term::Let(name, expr, body1)))
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
            let ks1 = Array::from_slice(*ctx, ks1);
            let (pushed, body1) = push_in(ctx, wrap_with_cnts, body, ns);
            let pushed_count = pushed as usize + pushed_k.iter().filter(|&&x| x).count();
            // If more than one branch pushed the site, wrap above them
            // so the continuations are in scope for all.
            if pushed_count > 1 {
                (true, wrap_with_cnts(t))
            } else {
                (
                    pushed_count == 1,
                    TermRef::new(*ctx, Term::Letk(ks1, body1)),
                )
            }
        }

        Term::Fix(funs, body) => {
            let (pushed_f, funs1): (Vec<bool>, Vec<_>) = funs
                .iter()
                .copied()
                .map(|f| {
                    let (pushed, body1) = push_in(ctx, wrap_with_cnts, f.body(), ns);
                    (pushed, f.with_body(ctx, body1))
                })
                .unzip();

            let (pushed, body1) = push_in(ctx, wrap_with_cnts, body, ns);
            let pushed_count = pushed as usize + pushed_f.iter().filter(|&&x| x).count();
            assert!(pushed_count <= 1, "More than one pushed function in Fix");

            (
                pushed_count == 1,
                TermRef::new(*ctx, Term::Fix(Array::from_slice(*ctx, funs1), body1)),
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
        Term::App(Atom::Local(f), _, args, src) if ns.contains(&f) => {
            // Tail call now becomes a continuation jump.
            Gc::new(*ctx, Term::Continue(f, args, src))
        }

        Term::Let(name, expr, body) => {
            let body = transform_apps(body, ns, ctx);
            Gc::new(*ctx, Term::Let(name, expr, body))
        }

        Term::Fix(funs, body) => {
            let funs = funs
                .iter()
                .map(|f| f.with_body(ctx, transform_apps(f.body(), ns, ctx)))
                .collect_gc(*ctx);

            let body = transform_apps(body, ns, ctx);

            Gc::new(*ctx, Term::Fix(funs, body))
        }

        Term::Letk(ks, body) => {
            let ks = ks
                .iter()
                .map(|k| k.with_body(ctx, transform_apps(k.body(), ns, ctx)))
                .collect_gc(*ctx);

            let body = transform_apps(body, ns, ctx);

            Gc::new(*ctx, Term::Letk(ks, body))
        }

        _ => t,
    }
}

impl<'gc> Term<'gc> {
    pub fn subst(
        self: TermRef<'gc>,
        ctx: Context<'gc>,
        subst: &Map<LVarRef<'gc>, LVarRef<'gc>>,
    ) -> TermRef<'gc> {
        match &*self {
            Self::Let(name, prev_expr, prev_body) => {
                let expr = prev_expr.subst(ctx, subst);
                let body = prev_body.subst(ctx, subst);
                if &expr == prev_expr && Gc::ptr_eq(body, *prev_body) {
                    return self;
                }
                TermRef::new(*ctx, Self::Let(*name, expr, body))
            }

            Self::Letk(prev_ks, prev_body) => {
                let ks = prev_ks
                    .iter()
                    .map(|k| k.subst(ctx, subst))
                    .collect::<Vec<_>>();
                let ks = if ks.iter().zip(prev_ks.iter()).all(|(a, b)| a == b) {
                    *prev_ks
                } else {
                    Array::from_slice(*ctx, ks)
                };
                let body = prev_body.subst(ctx, subst);
                if Gc::ptr_eq(body, *prev_body) && Gc::ptr_eq(ks, *prev_ks) {
                    return self;
                }
                TermRef::new(*ctx, Self::Letk(ks, body))
            }

            Self::Fix(prev_funs, prev_body) => {
                let funs = prev_funs
                    .iter()
                    .map(|f| f.subst(ctx, subst))
                    .collect::<Vec<_>>();
                let body = prev_body.subst(ctx, subst);
                if funs
                    .iter()
                    .zip(prev_funs.iter())
                    .all(|(a, b)| Gc::ptr_eq(*a, *b))
                    && Gc::ptr_eq(body, *prev_body)
                {
                    return self;
                }
                let funs = Array::from_slice(*ctx, funs);
                TermRef::new(*ctx, Self::Fix(funs, body))
            }

            Self::App(prev_func, prev_k, prev_args, src) => {
                let func = prev_func.subst(ctx, subst);
                let k = subst.subst(*prev_k);

                let args = prev_args
                    .iter()
                    .map(|arg| arg.subst(ctx, subst))
                    .collect::<Vec<_>>();
                let args = if args.iter().zip(prev_args.iter()).all(|(a, b)| a == b) {
                    *prev_args
                } else {
                    Array::from_slice(*ctx, args)
                };

                if &func == prev_func && *prev_k == k && Gc::ptr_eq(args, *prev_args) {
                    return self;
                }

                TermRef::new(*ctx, Self::App(func, k, args, *src))
            }

            Self::Continue(prev_k, prev_args, src) => {
                let k = subst.subst(*prev_k);
                let args = prev_args
                    .iter()
                    .map(|arg| arg.subst(ctx, subst))
                    .collect::<Vec<_>>();
                let args = if args.iter().zip(prev_args.iter()).all(|(a, b)| a == b) {
                    *prev_args
                } else {
                    Array::from_slice(*ctx, args)
                };
                if *prev_k == k && Gc::ptr_eq(args, *prev_args) {
                    return self;
                }
                TermRef::new(*ctx, Self::Continue(k, args, *src))
            }

            Self::Raise {
                kind,
                args: prev_args,
                source,
            } => {
                let args = prev_args
                    .iter()
                    .map(|arg| arg.subst(ctx, subst))
                    .collect::<Vec<_>>();
                let args = if args.iter().zip(prev_args.iter()).all(|(a, b)| a == b) {
                    *prev_args
                } else {
                    Array::from_slice(*ctx, args)
                };
                if Gc::ptr_eq(args, *prev_args) {
                    return self;
                }
                TermRef::new(
                    *ctx,
                    Self::Raise {
                        kind: *kind,
                        args,
                        source: *source,
                    },
                )
            }

            Self::If {
                test: prev_test,
                consequent: prev_consequent,
                consequent_args: prev_consequent_args,
                alternative: prev_alternative,
                alternative_args: prev_alternative_args,
                hints,
            } => {
                let test = prev_test.subst(ctx, subst);
                let consequent = subst.subst(*prev_consequent);
                let alternative = subst.subst(*prev_alternative);
                let hints = *hints;
                let consequent_args = prev_consequent_args.map(|prev_args| {
                    let args = prev_args
                        .iter()
                        .map(|arg| arg.subst(ctx, subst))
                        .collect::<Vec<_>>();
                    if args.iter().zip(prev_args.iter()).all(|(a, b)| a == b) {
                        prev_args
                    } else {
                        Array::from_slice(*ctx, args)
                    }
                });
                let alternative_args = prev_alternative_args.map(|prev_args| {
                    let args = prev_args
                        .iter()
                        .map(|arg| arg.subst(ctx, subst))
                        .collect::<Vec<_>>();
                    if args.iter().zip(prev_args.iter()).all(|(a, b)| a == b) {
                        prev_args
                    } else {
                        Array::from_slice(*ctx, args)
                    }
                });

                if test == *prev_test
                    && Gc::ptr_eq(consequent, *prev_consequent)
                    && Gc::ptr_eq(alternative, *prev_alternative)
                    && (consequent_args
                        .is_some_and(|args| Gc::ptr_eq(args, prev_consequent_args.unwrap())))
                    && (alternative_args
                        .is_some_and(|args| Gc::ptr_eq(args, prev_alternative_args.unwrap())))
                {
                    return self;
                }

                TermRef::new(
                    *ctx,
                    Self::If {
                        test,
                        consequent,
                        consequent_args,
                        alternative,
                        alternative_args,
                        hints,
                    },
                )
            }
        }
    }
}

impl<'gc> Atom<'gc> {
    pub fn subst(self, _ctx: Context<'gc>, subst: &Map<LVarRef<'gc>, LVarRef<'gc>>) -> Atom<'gc> {
        match self {
            Atom::Local(l) => Atom::Local(subst.subst(l)),
            _ => self,
        }
    }
}

impl<'gc> Func<'gc> {
    pub fn subst(
        self: Gc<'gc, Self>,
        ctx: Context<'gc>,
        subst: &Map<LVarRef<'gc>, LVarRef<'gc>>,
    ) -> FuncRef<'gc> {
        let body = self.body().subst(ctx, subst);
        self.with_body(ctx, body)
    }
}

impl<'gc> Cont<'gc> {
    pub fn subst(
        self: Gc<'gc, Self>,
        ctx: Context<'gc>,
        subst: &Map<LVarRef<'gc>, LVarRef<'gc>>,
    ) -> ContRef<'gc> {
        let body = self.body().subst(ctx, subst);
        self.with_body(ctx, body)
    }
}

impl<'gc> Expression<'gc> {
    pub fn subst(self, ctx: Context<'gc>, subst: &Map<LVarRef<'gc>, LVarRef<'gc>>) -> Self {
        match self {
            Self::PrimCall(name, prev_args, src) => {
                let args = prev_args
                    .iter()
                    .map(|arg| arg.subst(ctx, subst))
                    .collect::<Vec<_>>();
                let args = if args.iter().zip(prev_args.iter()).all(|(a, b)| a == b) {
                    prev_args
                } else {
                    Array::from_slice(*ctx, args)
                };
                Self::PrimCall(name, args, src)
            }
        }
    }
}
