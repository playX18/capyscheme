use std::collections::HashMap;

use crate::rsgc::{cell::Lock, traits::IterGc};

use super::*;
use crate::{
    expander::{
        core::{
            Fix, LVarRef, Let, LetStyle, Proc, Term, TermKind, TermRef, call_term, define,
            fresh_lvar, lref, prim_call_term,
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

/// Convert all lexical assignments into boxed variable operations.
///
/// Also converts `let*` into nested `let`s for simplicity.
pub fn eliminate_assignments<'gc>(ctx: Context<'gc>, term: TermRef<'gc>) -> TermRef<'gc> {
    // rescan reference counts of local-variables in term. Previous rewrites
    // could've changed them.
    term.count_refs();
    rec(ctx, term, &mut HashMap::new())
}

/// Recursively walk down the term, substituting lvars according to the substitutions map
/// or wrapping them in boxes as needed.
fn rec<'gc>(
    ctx: Context<'gc>,
    term: TermRef<'gc>,
    substitutions: &mut HashMap<LVarRef<'gc>, LVarRef<'gc>>,
) -> TermRef<'gc> {
    match &term.kind {
        TermKind::ToplevelRef(_, _)
        | TermKind::ModuleRef(_, _, _)
        | TermKind::PrimRef(_)
        | TermKind::Const(_) => term,
        TermKind::LRef(lvar) => {
            if let Some(new_var) = substitutions.get(lvar) {
                box_ref(ctx, *new_var)
            } else {
                assert!(
                    !lvar.is_mutated(),
                    "<lref> must have substitution when mutated"
                );
                term
            }
        }

        TermKind::LSet(lvar, val) => {
            let subst = substitutions
                .get(lvar)
                .copied()
                .expect("<lset> must have substitution");
            let val = rec(ctx, *val, substitutions);
            box_set(ctx, subst, val)
        }

        TermKind::Call(proc, args) => {
            let proc = rec(ctx, *proc, substitutions);
            let args = args
                .iter()
                .map(|arg| rec(ctx, *arg, substitutions))
                .collect::<Vec<_>>();
            call_term(ctx, proc, args, term.source())
        }

        TermKind::Define(module, name, val) => {
            let val = rec(ctx, *val, substitutions);
            define(ctx, *module, *name, val, term.source())
        }

        TermKind::If(test, cons, alt) => {
            let test = rec(ctx, *test, substitutions);
            let cons = rec(ctx, *cons, substitutions);
            let alt = rec(ctx, *alt, substitutions);
            Gc::new(
                &ctx,
                Term {
                    source: Lock::new(term.source()),
                    kind: TermKind::If(test, cons, alt),
                },
            )
        }

        TermKind::PrimCall(prim, args) => {
            let args = args
                .iter()
                .map(|arg| rec(ctx, *arg, substitutions))
                .collect::<Vec<_>>();
            prim_call_term(ctx, *prim, args, term.source())
        }

        TermKind::Seq(head, tail) => {
            let head = rec(ctx, *head, substitutions);
            let tail = rec(ctx, *tail, substitutions);
            Gc::new(
                &ctx,
                Term {
                    source: Lock::new(term.source()),
                    kind: TermKind::Seq(head, tail),
                },
            )
        }

        TermKind::Values(values) => {
            let values = values
                .iter()
                .map(|val| rec(ctx, *val, substitutions))
                .collect_gc(&ctx);
            Gc::new(
                &ctx,
                Term {
                    source: Lock::new(term.source()),
                    kind: TermKind::Values(values),
                },
            )
        }

        TermKind::ToplevelSet(module, name, val) => {
            let val = rec(ctx, *val, substitutions);
            Gc::new(
                &ctx,
                Term {
                    source: Lock::new(term.source()),
                    kind: TermKind::ToplevelSet(*module, *name, val),
                },
            )
        }

        TermKind::Proc(proc) => {
            let body = wrap_mutable(
                ctx,
                proc.args
                    .iter()
                    .copied()
                    .chain(proc.variadic.iter().copied()),
                proc.body,
                substitutions,
            );

            Gc::new(
                &ctx,
                Term {
                    source: term.source().into(),
                    kind: TermKind::Proc(proc.with_body(&ctx, body)),
                },
            )
        }

        TermKind::Let(let_) => {
            assert!(
                !matches!(let_.style, LetStyle::LetRec | LetStyle::LetRecStar),
                "letrec must be eliminated before assignment elimination"
            );
            if let LetStyle::LetStar = let_.style {
                // convert let* to nested lets for simplicity
                let mut body = let_.body;
                for (var, val) in let_.lhs.iter().zip(let_.rhs.iter()).rev() {
                    body = Term::let_(
                        &ctx,
                        LetStyle::Let,
                        std::iter::once(*var),
                        std::iter::once(*val),
                        body,
                        term.source(),
                    );
                }

                return rec(ctx, body, substitutions);
            }

            // RHS expressions do not depend on LHS vars, so we can process them first
            let rhs = let_
                .rhs
                .iter()
                .map(|val| rec(ctx, *val, substitutions))
                .collect_gc(&ctx);

            // `let` can be completed very simply by wrapping mutable LHS variables in boxes
            // and updating the body accordingly.
            let body = wrap_mutable(ctx, let_.lhs.iter().copied(), let_.body, substitutions);

            Gc::new(
                &ctx,
                Term {
                    source: Lock::new(term.source()),
                    kind: TermKind::Let(Let {
                        style: let_.style,
                        lhs: let_.lhs.clone(),
                        rhs,
                        body,
                    }),
                },
            )
        }

        TermKind::Fix(fix) => {
            let procs = fix
                .rhs
                .iter()
                .map(|proc| {
                    let body = wrap_mutable(
                        ctx,
                        proc.args
                            .iter()
                            .copied()
                            .chain(proc.variadic.iter().copied()),
                        proc.body,
                        substitutions,
                    );

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
                .collect_gc(&ctx);

            let body = rec(ctx, fix.body, substitutions);

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

        TermKind::Receive(vars, variadic, producer, consumer) => {
            let producer = rec(ctx, *producer, substitutions);

            let consumer = wrap_mutable(
                ctx,
                vars.iter().copied().chain(variadic.iter().copied()),
                *consumer,
                substitutions,
            );

            Gc::new(
                &ctx,
                Term {
                    source: Lock::new(term.source()),
                    kind: TermKind::Receive(vars.clone(), *variadic, producer, consumer),
                },
            )
        }

        TermKind::WithContinuationMark(key, value, result) => {
            let key = rec(ctx, *key, substitutions);
            let value = rec(ctx, *value, substitutions);
            let result = rec(ctx, *result, substitutions);
            Gc::new(
                &ctx,
                Term {
                    source: Lock::new(term.source()),
                    kind: TermKind::WithContinuationMark(key, value, result),
                },
            )
        }

        TermKind::ModuleSet(module, name, public, val) => {
            let val = rec(ctx, *val, substitutions);
            Gc::new(
                &ctx,
                Term {
                    source: Lock::new(term.source()),
                    kind: TermKind::ModuleSet(*module, *name, *public, val),
                },
            )
        }
    }
}

/// Wrap all mutable `lhs` variables in boxes and return corresponding new variable
/// substitutions.
fn wrap_mutable<'gc>(
    ctx: Context<'gc>,
    lhs: impl IntoIterator<Item = LVarRef<'gc>>,
    body: TermRef<'gc>,
    substitutions: &mut HashMap<LVarRef<'gc>, LVarRef<'gc>>,
) -> TermRef<'gc> {
    let mutated = lhs
        .into_iter()
        .filter(|var| var.is_mutated())
        .collect::<Vec<_>>();
    if mutated.is_empty() {
        return rec(ctx, body, substitutions);
    }

    for var in mutated.iter() {
        let new_var = fresh_lvar(ctx, ctx.intern(&format!("&mut_{}", var.name)));
        substitutions.insert(*var, new_var);
    }

    let body = rec(ctx, body, substitutions);

    let body = Term::let_(
        &ctx,
        LetStyle::Let,
        mutated.iter().map(|var| substitutions[var].clone()),
        mutated.iter().map(|var| pbox(ctx, *var)),
        body,
        body.source(),
    );
    body
}
impl<'gc> Term<'gc> {
    pub fn count_refs(&self) {
        match self.kind {
            TermKind::Const(_)
            | TermKind::PrimRef(_)
            | TermKind::ToplevelRef(_, _)
            | TermKind::ModuleRef(_, _, _) => (),
            TermKind::LRef(lvar) => {
                lvar.reference();
            }

            TermKind::LSet(lvar, val) => {
                lvar.mutate();
                val.count_refs();
            }

            TermKind::Call(proc, args) => {
                proc.count_refs();
                for arg in args.iter() {
                    arg.count_refs();
                }
            }

            TermKind::PrimCall(_prim, args) => {
                for arg in args.iter() {
                    arg.count_refs();
                }
            }

            TermKind::Define(_, _, val) => {
                val.count_refs();
            }

            TermKind::Fix(fix) => {
                for proc in fix.rhs.iter() {
                    proc.args.iter().for_each(|arg| {
                        arg.set_count.set(0);
                        arg.ref_count.set(0);
                    });
                    proc.body.count_refs();
                }
                fix.body.count_refs();
            }

            TermKind::If(test, cons, alt) => {
                test.count_refs();
                cons.count_refs();
                alt.count_refs();
            }

            TermKind::Let(l) => {
                for var in l.lhs.iter() {
                    var.set_count.set(0);
                    var.ref_count.set(0);
                }
                for val in l.rhs.iter() {
                    val.count_refs();
                }
                l.body.count_refs();
            }

            TermKind::ModuleSet(_, _, _, val) => {
                val.count_refs();
            }

            TermKind::Proc(proc) => {
                for arg in proc.args.iter() {
                    arg.set_count.set(0);
                    arg.ref_count.set(0);
                }
                proc.body.count_refs();
            }

            TermKind::Receive(vars, variadic, producer, consumer) => {
                for var in vars.iter() {
                    var.set_count.set(0);
                    var.ref_count.set(0);
                }
                if let Some(var) = variadic {
                    var.set_count.set(0);
                    var.ref_count.set(0);
                }
                producer.count_refs();
                consumer.count_refs();
            }

            TermKind::WithContinuationMark(key, mark, result) => {
                key.count_refs();
                mark.count_refs();
                result.count_refs();
            }

            TermKind::Seq(head, tail) => {
                head.count_refs();
                tail.count_refs();
            }

            TermKind::ToplevelSet(_, _, val) => {
                val.count_refs();
            }

            TermKind::Values(values) => {
                for val in values.iter() {
                    val.count_refs();
                }
            }
        }
    }
}
