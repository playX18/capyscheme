use std::collections::HashMap;

use rsgc::{alloc::array::Array, cell::Lock};

use super::*;
use crate::{
    expander::{
        core::{
            Fix, LVarRef, Let, LetStyle, Proc, Term, TermKind, TermRef, call_term, define,
            fresh_lvar, let_term, lref, prim_call_term,
        },
        primitives::{sym_make_variable, sym_variable_ref, sym_variable_set},
    },
    runtime::Context,
};

fn box_ref<'gc>(ctx: Context<'gc>, var: LVarRef<'gc>) -> TermRef<'gc> {
    prim_call_term(
        ctx,
        sym_variable_ref(ctx).into(),
        &[lref(ctx, var)],
        Value::new(false),
    )
}

fn box_set<'gc>(ctx: Context<'gc>, var: LVarRef<'gc>, value: TermRef<'gc>) -> TermRef<'gc> {
    prim_call_term(
        ctx,
        sym_variable_set(ctx).into(),
        &[lref(ctx, var), value],
        Value::new(false),
    )
}

fn pbox<'gc>(ctx: Context<'gc>, var: LVarRef<'gc>) -> TermRef<'gc> {
    prim_call_term(
        ctx,
        sym_make_variable(ctx).into(),
        &[lref(ctx, var)],
        Value::new(false),
    )
}

fn substitute<'gc>(
    ctx: Context<'gc>,
    term: TermRef<'gc>,
    substs: &HashMap<LVarRef<'gc>, LVarRef<'gc>>,
) -> TermRef<'gc> {
    match &term.kind {
        TermKind::Values(values) => {
            let values = values
                .iter()
                .map(|v| substitute(ctx, *v, substs))
                .collect::<Vec<_>>();
            Gc::new(
                &ctx,
                Term {
                    source: Lock::new(term.source()),
                    kind: TermKind::Values(Array::from_slice(&ctx, &values)),
                },
            )
        }

        TermKind::LRef(lvar) => {
            if let Some(new_lvar) = substs.get(lvar) {
                box_ref(ctx, *new_lvar)
            } else {
                term
            }
        }

        TermKind::LSet(lvar, value) => {
            let value = substitute(ctx, *value, substs);
            let Some(new) = substs.get(lvar) else {
                return Gc::new(
                    &ctx,
                    Term {
                        source: Lock::new(term.source()),
                        kind: TermKind::LSet(*lvar, value),
                    },
                );
            };

            box_set(ctx, *new, value)
        }

        TermKind::Call(proc, args) => {
            let proc = substitute(ctx, *proc, substs);
            let args = args
                .iter()
                .map(|arg| substitute(ctx, *arg, substs))
                .collect::<Vec<_>>();

            call_term(ctx, proc, args, term.source())
        }

        TermKind::PrimCall(prim, args) => {
            let args = args
                .iter()
                .map(|arg| substitute(ctx, *arg, substs))
                .collect::<Vec<_>>();

            prim_call_term(ctx, *prim, args, term.source())
        }

        TermKind::Define(module, var, val) => {
            let val = substitute(ctx, *val, substs);
            define(ctx, *module, *var, val, term.source())
        }

        TermKind::Fix(fix) => {
            let procs = fix
                .rhs
                .iter()
                .map(|proc| {
                    let body = substitute(ctx, proc.body, substs);

                    Gc::new(
                        &ctx,
                        Proc {
                            name: proc.name,
                            body,
                            source: proc.source,
                            args: proc.args,
                            variadic: proc.variadic,
                            meta: proc.meta,
                        },
                    )
                })
                .collect::<Vec<_>>();
            let procs = Array::from_slice(&ctx, &procs);
            let body = substitute(ctx, fix.body, substs);

            Gc::new(
                &ctx,
                Term {
                    source: Lock::new(term.source()),
                    kind: TermKind::Fix(Fix {
                        body,
                        lhs: fix.lhs,
                        rhs: procs,
                    }),
                },
            )
        }

        TermKind::Let(l) => {
            let rhs = l
                .rhs
                .iter()
                .map(|value| {
                    let value = substitute(ctx, *value, substs);
                    value
                })
                .collect::<Vec<_>>();
            let rhs = Array::from_slice(&ctx, &rhs);

            let body = substitute(ctx, l.body, substs);

            Gc::new(
                &ctx,
                Term {
                    source: Lock::new(term.source()),
                    kind: TermKind::Let(Let {
                        style: l.style,
                        lhs: l.lhs,
                        rhs,
                        body,
                    }),
                },
            )
        }

        TermKind::Receive(formals, opt_formal, producer, consumer) => {
            let producer = substitute(ctx, *producer, substs);
            let consumer = substitute(ctx, *consumer, substs);

            Gc::new(
                &ctx,
                Term {
                    source: Lock::new(term.source()),
                    kind: TermKind::Receive(
                        formals.clone(),
                        opt_formal.clone(),
                        producer,
                        consumer,
                    ),
                },
            )
        }

        TermKind::Proc(proc) => {
            let body = substitute(ctx, proc.body, substs);

            Gc::new(
                &ctx,
                Term {
                    source: Lock::new(term.source()),
                    kind: TermKind::Proc(Gc::new(
                        &ctx,
                        Proc {
                            name: proc.name,
                            body,
                            args: proc.args,
                            variadic: proc.variadic,
                            source: proc.source,
                            meta: proc.meta,
                        },
                    )),
                },
            )
        }

        TermKind::Seq(seq) => {
            let terms = seq
                .iter()
                .map(|t| substitute(ctx, *t, substs))
                .collect::<Vec<_>>();
            let terms = Array::from_slice(&ctx, &terms);

            Gc::new(
                &ctx,
                Term {
                    source: Lock::new(term.source()),
                    kind: TermKind::Seq(terms),
                },
            )
        }

        TermKind::If(test, cons, alt) => {
            let test = substitute(ctx, *test, substs);
            let cons = substitute(ctx, *cons, substs);
            let alt = substitute(ctx, *alt, substs);

            Gc::new(
                &ctx,
                Term {
                    source: Lock::new(term.source()),
                    kind: TermKind::If(test, cons, alt),
                },
            )
        }

        TermKind::ToplevelSet(module, var, val) => {
            let val = substitute(ctx, *val, substs);

            Gc::new(
                &ctx,
                Term {
                    source: Lock::new(term.source()),
                    kind: TermKind::ToplevelSet(*module, *var, val),
                },
            )
        }

        TermKind::ModuleSet(module, var, public, val) => {
            let val = substitute(ctx, *val, substs);

            Gc::new(
                &ctx,
                Term {
                    source: Lock::new(term.source()),
                    kind: TermKind::ModuleSet(*module, *var, *public, val),
                },
            )
        }

        TermKind::Const(_)
        | TermKind::ToplevelRef(_, _)
        | TermKind::ModuleRef(_, _, _)
        | TermKind::PrimRef(_) => term,
    }
}

fn wrap_mutables<'gc>(ctx: Context<'gc>, term: TermRef<'gc>) -> TermRef<'gc> {
    match &term.kind {
        TermKind::Values(values) => {
            let values = values
                .iter()
                .map(|v| wrap_mutables(ctx, *v))
                .collect::<Vec<_>>();
            Gc::new(
                &ctx,
                Term {
                    source: Lock::new(term.source()),
                    kind: TermKind::Values(Array::from_slice(&ctx, &values)),
                },
            )
        }
        TermKind::LRef(_)
        | TermKind::ToplevelRef(_, _)
        | TermKind::ModuleRef(_, _, _)
        | TermKind::PrimRef(_)
        | TermKind::Const(_) => term,
        TermKind::Let(l) => {
            let rhs = l
                .rhs
                .iter()
                .map(|val| wrap_mutables(ctx, *val))
                .collect::<Vec<_>>();

            let mutables = l
                .lhs
                .iter()
                .zip(rhs.iter())
                .filter(|(var, _)| var.is_mutated())
                .collect::<Vec<_>>();

            if mutables.is_empty() {
                let body = wrap_mutables(ctx, l.body);

                return Gc::new(
                    &ctx,
                    Term {
                        source: Lock::new(term.source()),
                        kind: TermKind::Let(Let {
                            style: l.style,
                            lhs: l.lhs.clone(),
                            rhs: Array::from_slice(&ctx, &rhs),
                            body,
                        }),
                    },
                );
            }

            let mut mlhs = Vec::new();
            let mut mrhs = Vec::new();
            let mut subst = HashMap::new();
            for (var, _value) in mutables.iter() {
                let name = Symbol::from_str_uninterned(&ctx, &format!("&{}", var.name), None);
                let new_var = fresh_lvar(ctx, name.into());
                mlhs.push(new_var);
                mrhs.push(pbox(ctx, **var));
                subst.insert(**var, new_var);
            }

            let body = wrap_mutables(ctx, substitute(ctx, l.body, &subst));

            let wrap = Gc::new(
                &ctx,
                Term {
                    source: Lock::new(term.source()),
                    kind: TermKind::Let(Let {
                        style: LetStyle::Let,
                        lhs: Array::from_slice(&ctx, &mlhs),
                        rhs: Array::from_slice(&ctx, &mrhs),
                        body,
                    }),
                },
            );

            Gc::new(
                &ctx,
                Term {
                    source: Lock::new(term.source()),
                    kind: TermKind::Let(Let {
                        style: l.style,
                        lhs: l.lhs.clone(),
                        rhs: Array::from_slice(&ctx, &rhs),
                        body: wrap,
                    }),
                },
            )
        }

        TermKind::Receive(formals, opt_formals, producer, consumer) => {
            let producer = wrap_mutables(ctx, *producer);

            let mutables = formals
                .iter()
                .chain(opt_formals.iter())
                .filter(|arg| arg.is_mutated())
                .collect::<Vec<_>>();

            if mutables.is_empty() {
                let body = wrap_mutables(ctx, *consumer);

                return Gc::new(
                    &ctx,
                    Term {
                        source: Lock::new(term.source()),
                        kind: TermKind::Receive(
                            formals.clone(),
                            opt_formals.clone(),
                            producer,
                            body,
                        ),
                    },
                );
            }

            let mut mlhs = Vec::new();
            let mut mrhs = Vec::new();

            let mut subst = HashMap::new();

            for var in mutables.iter() {
                let name = Symbol::from_str_uninterned(&ctx, &format!("&{}", var.name), None);
                let new_var = fresh_lvar(ctx, name.into());
                mlhs.push(new_var);
                mrhs.push(pbox(ctx, **var));
                subst.insert(**var, new_var);
            }

            let body = wrap_mutables(ctx, substitute(ctx, *consumer, &subst));

            let wrap = Gc::new(
                &ctx,
                Term {
                    source: Lock::new(term.source()),
                    kind: TermKind::Let(Let {
                        style: LetStyle::Let,
                        lhs: Array::from_slice(&ctx, &mlhs),
                        rhs: Array::from_slice(&ctx, &mrhs),
                        body,
                    }),
                },
            );

            Gc::new(
                &ctx,
                Term {
                    source: Lock::new(term.source()),
                    kind: TermKind::Receive(formals.clone(), opt_formals.clone(), producer, wrap),
                },
            )
        }

        TermKind::Call(proc, args) => {
            let proc = wrap_mutables(ctx, *proc);
            let args = args
                .iter()
                .map(|arg| wrap_mutables(ctx, *arg))
                .collect::<Vec<_>>();

            call_term(ctx, proc, args, term.source())
        }

        TermKind::Define(module, var, val) => {
            let val = wrap_mutables(ctx, *val);
            define(ctx, *module, *var, val, term.source())
        }

        TermKind::Fix(fix) => {
            let procs = fix
                .rhs
                .iter()
                .map(|proc| {
                    let body = wrap_mutables_case(ctx, &proc.args, proc.variadic, proc.body);

                    Gc::new(
                        &ctx,
                        Proc {
                            name: proc.name,
                            body,
                            args: proc.args,
                            variadic: proc.variadic,
                            source: proc.source,
                            meta: proc.meta,
                        },
                    )
                })
                .collect::<Vec<_>>();
            let procs = Array::from_slice(&ctx, &procs);
            let body = wrap_mutables(ctx, fix.body);

            Gc::new(
                &ctx,
                Term {
                    source: Lock::new(term.source()),
                    kind: TermKind::Fix(Fix {
                        body,
                        lhs: fix.lhs,
                        rhs: procs,
                    }),
                },
            )
        }

        TermKind::Proc(proc) => {
            let body = wrap_mutables_case(ctx, &proc.args, proc.variadic, proc.body);
            Gc::new(
                &ctx,
                Term {
                    source: Lock::new(term.source()),
                    kind: TermKind::Proc(Gc::new(
                        &ctx,
                        Proc {
                            name: proc.name,
                            body: body,
                            args: proc.args,
                            variadic: proc.variadic,
                            source: proc.source,
                            meta: proc.meta,
                        },
                    )),
                },
            )
        }

        TermKind::ToplevelSet(module, var, val) => {
            let val = wrap_mutables(ctx, *val);

            Gc::new(
                &ctx,
                Term {
                    source: Lock::new(term.source()),
                    kind: TermKind::ToplevelSet(*module, *var, val),
                },
            )
        }

        TermKind::ModuleSet(module, var, public, val) => {
            let val = wrap_mutables(ctx, *val);

            Gc::new(
                &ctx,
                Term {
                    source: Lock::new(term.source()),
                    kind: TermKind::ModuleSet(*module, *var, *public, val),
                },
            )
        }

        TermKind::If(test, cons, alt) => {
            let test = wrap_mutables(ctx, *test);
            let cons = wrap_mutables(ctx, *cons);
            let alt = wrap_mutables(ctx, *alt);

            Gc::new(
                &ctx,
                Term {
                    source: Lock::new(term.source()),
                    kind: TermKind::If(test, cons, alt),
                },
            )
        }

        TermKind::LSet(var, value) => {
            let value = wrap_mutables(ctx, *value);
            Gc::new(
                &ctx,
                Term {
                    source: Lock::new(term.source()),
                    kind: TermKind::LSet(*var, value),
                },
            )
        }

        TermKind::PrimCall(prim, args) => {
            let args = args
                .iter()
                .map(|arg| wrap_mutables(ctx, *arg))
                .collect::<Vec<_>>();

            prim_call_term(ctx, *prim, args, term.source())
        }

        TermKind::Seq(seq) => {
            let terms = seq
                .iter()
                .map(|t| wrap_mutables(ctx, *t))
                .collect::<Vec<_>>();
            let terms = Array::from_slice(&ctx, &terms);

            Gc::new(
                &ctx,
                Term {
                    source: Lock::new(term.source()),
                    kind: TermKind::Seq(terms),
                },
            )
        }
    }
}

fn wrap_mutables_case<'gc>(
    ctx: Context<'gc>,
    args: &[LVarRef<'gc>],
    variadic: Option<LVarRef<'gc>>,
    body: TermRef<'gc>,
) -> TermRef<'gc> {
    let mutables = args
        .iter()
        .chain(variadic.iter())
        .filter(|arg| arg.is_mutated())
        .collect::<Vec<_>>();

    if mutables.is_empty() {
        return wrap_mutables(ctx, body);
    }

    let mut mlhs = Vec::new();
    let mut mrhs = Vec::new();
    let mut subst = HashMap::new();

    for var in mutables.iter() {
        let new_var = fresh_lvar(ctx, var.name);
        mlhs.push(new_var);
        mrhs.push(pbox(ctx, **var));
        subst.insert(**var, new_var);
    }

    let body = wrap_mutables(ctx, substitute(ctx, body, &subst));

    let let_ = let_term(
        ctx,
        LetStyle::Let,
        Array::from_slice(&ctx, mlhs),
        Array::from_slice(&ctx, mrhs),
        body,
        Value::new(false),
    );

    let_
}

pub fn eliminate_assignments<'gc>(ctx: Context<'gc>, term: TermRef<'gc>) -> TermRef<'gc> {
    wrap_mutables(ctx, term)
}
