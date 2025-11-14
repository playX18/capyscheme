use crate::rsgc::{Gc, alloc::Array};

use crate::{
    expander::core::{Fix, Proc, Term, TermKind, TermRef},
    runtime::Context,
};

pub fn pre_post_order<'gc, PRE, POST>(
    ctx: Context<'gc>,
    mut pre: PRE,
    mut post: POST,
    term: TermRef<'gc>,
) -> TermRef<'gc>
where
    PRE: FnMut(Context<'gc>, TermRef<'gc>) -> TermRef<'gc>,
    POST: FnMut(Context<'gc>, TermRef<'gc>) -> TermRef<'gc>,
{
    fn rec<'gc>(
        ctx: Context<'gc>,
        pre: &mut impl FnMut(Context<'gc>, TermRef<'gc>) -> TermRef<'gc>,
        post: &mut impl FnMut(Context<'gc>, TermRef<'gc>) -> TermRef<'gc>,
        term: TermRef<'gc>,
    ) -> TermRef<'gc> {
        let x = pre(ctx, term);

        match x.kind {
            TermKind::Const(_)
            | TermKind::LRef(_)
            | TermKind::ModuleRef(_, _, _)
            | TermKind::PrimRef(_)
            | TermKind::ToplevelRef(_, _) => post(ctx, x),
            TermKind::LSet(lvar, val) => {
                let nexp = rec(ctx, pre, post, val);
                if Gc::ptr_eq(nexp, val) {
                    post(ctx, x)
                } else {
                    let lset = Gc::new(
                        &ctx,
                        Term {
                            source: x.source.get().into(),
                            kind: TermKind::LSet(lvar, nexp),
                        },
                    );
                    post(ctx, lset)
                }
            }

            TermKind::ModuleSet(module, name, public, val) => {
                let nexp = rec(ctx, pre, post, val);
                if Gc::ptr_eq(nexp, val) {
                    post(ctx, x)
                } else {
                    let mset = Gc::new(
                        &ctx,
                        Term {
                            source: x.source.get().into(),
                            kind: TermKind::ModuleSet(module, name, public, nexp),
                        },
                    );
                    post(ctx, mset)
                }
            }

            TermKind::ToplevelSet(module, name, val) => {
                let nexp = rec(ctx, pre, post, val);
                if Gc::ptr_eq(nexp, val) {
                    post(ctx, x)
                } else {
                    let tset = Gc::new(
                        &ctx,
                        Term {
                            source: x.source.get().into(),
                            kind: TermKind::ToplevelSet(module, name, nexp),
                        },
                    );
                    post(ctx, tset)
                }
            }

            TermKind::Define(module, name, value) => {
                let nexp = rec(ctx, pre, post, value);
                if Gc::ptr_eq(nexp, value) {
                    post(ctx, x)
                } else {
                    let def = Gc::new(
                        &ctx,
                        Term {
                            source: x.source.get().into(),
                            kind: TermKind::Define(module, name, nexp),
                        },
                    );
                    post(ctx, def)
                }
            }

            TermKind::If(cond, cons, alt) => {
                let ncond = rec(ctx, pre, post, cond);
                let ncons = rec(ctx, pre, post, cons);
                let nalt = rec(ctx, pre, post, alt);
                if Gc::ptr_eq(ncond, cond) && Gc::ptr_eq(ncons, cons) && Gc::ptr_eq(nalt, alt) {
                    post(ctx, x)
                } else {
                    let ifterm = Gc::new(
                        &ctx,
                        Term {
                            source: x.source.get().into(),
                            kind: TermKind::If(ncond, ncons, nalt),
                        },
                    );
                    post(ctx, ifterm)
                }
            }

            TermKind::Call(proc, args) => {
                let nproc = rec(ctx, pre, post, proc);
                let mut nargs = Vec::with_capacity(args.len());
                let mut changed = !Gc::ptr_eq(nproc, proc);
                for arg in args.iter() {
                    let narg = rec(ctx, pre, post, *arg);
                    if !Gc::ptr_eq(narg, *arg) {
                        changed = true;
                    }
                    nargs.push(narg);
                }
                if !changed {
                    post(ctx, x)
                } else {
                    let call = Gc::new(
                        &ctx,
                        Term {
                            source: x.source.get().into(),
                            kind: TermKind::Call(nproc, Array::from_slice(&ctx, &nargs)),
                        },
                    );
                    post(ctx, call)
                }
            }

            TermKind::PrimCall(prim, args) => {
                let mut nargs = Vec::with_capacity(args.len());
                let mut changed = false;
                for arg in args.iter() {
                    let narg = rec(ctx, pre, post, *arg);
                    if !Gc::ptr_eq(narg, *arg) {
                        changed = true;
                    }
                    nargs.push(narg);
                }
                if !changed {
                    post(ctx, x)
                } else {
                    let call = Gc::new(
                        &ctx,
                        Term {
                            source: x.source.get().into(),
                            kind: TermKind::PrimCall(prim, Array::from_slice(&ctx, &nargs)),
                        },
                    );
                    post(ctx, call)
                }
            }

            TermKind::Seq(head, tail) => {
                /*let mut nseq = Vec::with_capacity(seq.len());
                let mut changed = false;
                for stmt in seq.iter() {
                    let nstmt = rec(ctx, pre, post, *stmt);
                    if !Gc::ptr_eq(nstmt, *stmt) {
                        changed = true;
                    }
                    nseq.push(nstmt);
                }
                if !changed {
                    post(ctx, x)
                } else {
                    let seqterm = Gc::new(
                        &ctx,
                        Term {
                            source: x.source.get().into(),
                            kind: TermKind::Seq(Array::from_slice(&ctx, &nseq)),
                        },
                    );
                    post(ctx, seqterm)
                }*/

                let nhead = rec(ctx, pre, post, head);
                let ntail = rec(ctx, pre, post, tail);

                if Gc::ptr_eq(nhead, head) && Gc::ptr_eq(ntail, tail) {
                    post(ctx, x)
                } else {
                    let seqterm = Gc::new(
                        &ctx,
                        Term {
                            source: x.source.get().into(),
                            kind: TermKind::Seq(nhead, ntail),
                        },
                    );
                    post(ctx, seqterm)
                }
            }

            TermKind::Proc(proc) => {
                let nbody = rec(ctx, pre, post, proc.body);
                if Gc::ptr_eq(nbody, proc.body) {
                    post(ctx, x)
                } else {
                    let proc = Gc::new(
                        &ctx,
                        Proc {
                            name: proc.name,
                            variadic: proc.variadic,
                            args: proc.args,
                            body: nbody,
                            source: proc.source,
                            meta: proc.meta,
                        },
                    );
                    let term = Gc::new(
                        &ctx,
                        Term {
                            source: x.source.get().into(),
                            kind: TermKind::Proc(proc),
                        },
                    );
                    post(ctx, term)
                }
            }

            TermKind::Fix(fix) => {
                let mut rhs = Vec::with_capacity(fix.rhs.len());
                let mut changed = false;
                for r in fix.rhs.iter() {
                    let nbody = rec(ctx, pre, post, r.body);
                    if !Gc::ptr_eq(nbody, r.body) {
                        changed = true;
                    }
                    let proc = Gc::new(
                        &ctx,
                        Proc {
                            name: r.name,
                            variadic: r.variadic,
                            args: r.args,
                            body: nbody,
                            source: r.source,
                            meta: r.meta,
                        },
                    );
                    rhs.push(proc);
                }

                let nbody = rec(ctx, pre, post, fix.body);
                if !changed && Gc::ptr_eq(nbody, fix.body) {
                    post(ctx, x)
                } else {
                    let fixt = Gc::new(
                        &ctx,
                        Term {
                            source: x.source.get().into(),
                            kind: TermKind::Fix(Fix {
                                lhs: fix.lhs,
                                rhs: Array::from_slice(&ctx, &rhs),
                                body: nbody,
                            }),
                        },
                    );
                    post(ctx, fixt)
                }
            }

            TermKind::Let(l_) => {
                let mut rhs = Vec::with_capacity(l_.rhs.len());
                let mut changed = false;
                for r in l_.rhs.iter() {
                    let nexp = rec(ctx, pre, post, *r);
                    if !Gc::ptr_eq(nexp, *r) {
                        changed = true;
                    }
                    rhs.push(nexp);
                }

                let nbody = rec(ctx, pre, post, l_.body);
                if !changed && Gc::ptr_eq(nbody, l_.body) {
                    post(ctx, x)
                } else {
                    let lett = Gc::new(
                        &ctx,
                        Term {
                            source: x.source.get().into(),
                            kind: TermKind::Let(crate::expander::core::Let {
                                style: l_.style,
                                lhs: l_.lhs,
                                rhs: Array::from_slice(&ctx, &rhs),
                                body: nbody,
                            }),
                        },
                    );
                    post(ctx, lett)
                }
            }

            TermKind::Values(vals) => {
                let mut nvals = Vec::with_capacity(vals.len());
                let mut changed = false;
                for v in vals.iter() {
                    let nval = rec(ctx, pre, post, *v);
                    if !Gc::ptr_eq(nval, *v) {
                        changed = true;
                    }
                    nvals.push(nval);
                }
                if !changed {
                    post(ctx, x)
                } else {
                    let valterm = Gc::new(
                        &ctx,
                        Term {
                            source: x.source.get().into(),
                            kind: TermKind::Values(Array::from_slice(&ctx, &nvals)),
                        },
                    );
                    post(ctx, valterm)
                }
            }

            TermKind::Receive(vars, variadic, producer, receiver) => {
                let nproducer = rec(ctx, pre, post, producer);
                let nreceiver = rec(ctx, pre, post, receiver);
                if Gc::ptr_eq(nproducer, producer) && Gc::ptr_eq(nreceiver, receiver) {
                    post(ctx, x)
                } else {
                    let recterm = Gc::new(
                        &ctx,
                        Term {
                            source: x.source.get().into(),
                            kind: TermKind::Receive(vars, variadic, nproducer, nreceiver),
                        },
                    );
                    post(ctx, recterm)
                }
            }

            TermKind::WithContinuationMark(key, mark, result) => {
                let nkey = rec(ctx, pre, post, key);
                let nmark = rec(ctx, pre, post, mark);
                let nresult = rec(ctx, pre, post, result);
                if Gc::ptr_eq(nkey, key) && Gc::ptr_eq(nmark, mark) && Gc::ptr_eq(nresult, result) {
                    post(ctx, x)
                } else {
                    let wcmterm = Gc::new(
                        &ctx,
                        Term {
                            source: x.source.get().into(),
                            kind: TermKind::WithContinuationMark(nkey, nmark, nresult),
                        },
                    );
                    post(ctx, wcmterm)
                }
            }
        }
    }

    rec(ctx, &mut pre, &mut post, term)
}

pub fn post_order<'gc, F>(ctx: Context<'gc>, f: F, term: TermRef<'gc>) -> TermRef<'gc>
where
    F: FnMut(Context<'gc>, TermRef<'gc>) -> TermRef<'gc>,
{
    pre_post_order(ctx, |_, t| t, f, term)
}

pub fn pre_order<'gc, F>(ctx: Context<'gc>, f: F, term: TermRef<'gc>) -> TermRef<'gc>
where
    F: Fn(Context<'gc>, TermRef<'gc>) -> TermRef<'gc>,
{
    pre_post_order(ctx, f, |_, t| t, term)
}

pub fn fold_tree<'gc, ACC>(
    tree: TermRef<'gc>,
    down: &impl Fn(TermRef<'gc>, ACC) -> ACC,
    up: &impl Fn(TermRef<'gc>, ACC) -> ACC,
    init: ACC,
) -> ACC {
    fn foldts<'gc, ACC>(
        tree: TermRef<'gc>,
        down: &impl Fn(TermRef<'gc>, ACC) -> ACC,
        up: &impl Fn(TermRef<'gc>, ACC) -> ACC,
        acc: ACC,
    ) -> ACC {
        let acc = down(tree, acc);
        let acc = match tree.kind {
            TermKind::Const(_)
            | TermKind::LRef(_)
            | TermKind::ModuleRef(_, _, _)
            | TermKind::PrimRef(_)
            | TermKind::ToplevelRef(_, _) => acc,
            TermKind::LSet(_, val) => foldts(val, down, up, acc),
            TermKind::ModuleSet(_, _, _, val) => foldts(val, down, up, acc),
            TermKind::ToplevelSet(_, _, val) => foldts(val, down, up, acc),
            TermKind::Define(_, _, val) => foldts(val, down, up, acc),
            TermKind::If(cond, cons, alt) => {
                let acc = foldts(cond, down, up, acc);
                let acc = foldts(cons, down, up, acc);
                foldts(alt, down, up, acc)
            }
            TermKind::Call(proc, args) => {
                let acc = foldts(proc, down, up, acc);
                args.iter().fold(acc, |a, arg| foldts(*arg, down, up, a))
            }
            TermKind::PrimCall(_, args) => {
                args.iter().fold(acc, |a, arg| foldts(*arg, down, up, a))
            }
            TermKind::Seq(head, tail) => {
                //seq.iter().fold(acc, |a, stmt| foldts(*stmt, down, up, a))
                let acc = foldts(head, down, up, acc);
                foldts(tail, down, up, acc)
            }
            TermKind::Proc(proc) => foldts(proc.body, down, up, acc),
            TermKind::Fix(fix) => {
                let acc = fix.rhs.iter().fold(acc, |a, r| foldts(r.body, down, up, a));
                foldts(fix.body, down, up, acc)
            }
            TermKind::Let(l_) => {
                let acc = l_.rhs.iter().fold(acc, |a, r| foldts(*r, down, up, a));
                foldts(l_.body, down, up, acc)
            }
            TermKind::Values(vals) => vals.iter().fold(acc, |a, v| foldts(*v, down, up, a)),
            TermKind::Receive(_, _, producer, receiver) => {
                let acc = foldts(producer, down, up, acc);
                foldts(receiver, down, up, acc)
            }

            TermKind::WithContinuationMark(key, mark, result) => {
                let acc = foldts(key, down, up, acc);
                let acc = foldts(mark, down, up, acc);
                foldts(result, down, up, acc)
            }
        };
        up(tree, acc)
    }

    foldts(tree, down, up, init)
}

impl<'gc> Term<'gc> {
    pub fn post_order<F>(self: TermRef<'gc>, ctx: Context<'gc>, f: F) -> TermRef<'gc>
    where
        F: FnMut(Context<'gc>, TermRef<'gc>) -> TermRef<'gc>,
    {
        post_order(ctx, f, self)
    }

    pub fn pre_order<F>(self: TermRef<'gc>, ctx: Context<'gc>, f: F) -> TermRef<'gc>
    where
        F: Fn(Context<'gc>, TermRef<'gc>) -> TermRef<'gc>,
    {
        pre_order(ctx, f, self)
    }

    pub fn fold<ACC>(
        self: TermRef<'gc>,

        down: impl Fn(TermRef<'gc>, ACC) -> ACC,
        up: impl Fn(TermRef<'gc>, ACC) -> ACC,
        init: ACC,
    ) -> ACC {
        fold_tree(self, &down, &up, init)
    }

    pub fn for_each<F>(self: TermRef<'gc>, f: F)
    where
        F: Fn(TermRef<'gc>),
    {
        self.fold(
            |t, ()| {
                f(t);
            },
            |_, ()| {},
            (),
        );
    }
}
