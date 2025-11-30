use std::cell::RefCell;

use im::HashMap;

use crate::{
    Gc,
    expander::core::{
        LVarRef, Proc, ProcRef, Term, TermKind, TermRef, call_term, lref, prim_call_term,
    },
    runtime::Context,
    static_symbols,
};

/// Make lexically bound procedures well-known.
pub fn eta_expand<'gc>(ctx: Context<'gc>, term: TermRef<'gc>) -> TermRef<'gc> {
    let to_expand = analyze_procs(ctx, term);

    term.post_order(ctx, |ctx, expr| match expr.kind {
        TermKind::LRef(_) => do_eta_expand(ctx, expr, &to_expand).unwrap_or(expr),
        TermKind::Proc(proc) => do_eta_reduce(ctx, proc).unwrap_or(expr),
        _ => expr,
    })
}

#[derive(Clone, Copy)]
struct ProcInfo<'gc> {
    refcount: usize,
    op_refcount: usize,
    proc: Gc<'gc, Proc<'gc>>,
}

fn analyze_procs<'gc>(_ctx: Context<'gc>, t: TermRef<'gc>) -> HashMap<LVarRef<'gc>, ProcRef<'gc>> {
    let proc_infos = RefCell::new(HashMap::<LVarRef<'gc>, ProcInfo<'gc>>::new());

    t.for_each(|term| match term.kind {
        TermKind::LRef(var) => match proc_infos.borrow_mut().get_mut(&var) {
            Some(info) => {
                info.refcount += 1;
            }
            None => {}
        },

        TermKind::LSet(var, _) => {
            proc_infos.borrow_mut().remove(&var);
        }

        TermKind::Call(rator, _rands) => {
            if let TermKind::LRef(var) = rator.kind {
                if let Some(info) = proc_infos.borrow_mut().get_mut(&var) {
                    info.op_refcount += 1;
                }
            }
        }

        TermKind::Let(let_) => {
            for (lhs, rhs) in let_.lhs.iter().zip(let_.rhs.iter()) {
                if let TermKind::Proc(proc) = rhs.kind {
                    proc_infos.borrow_mut().insert(
                        *lhs,
                        ProcInfo {
                            refcount: 0,
                            op_refcount: 0,
                            proc,
                        },
                    );
                }
            }
        }

        TermKind::Fix(fix) => {
            for (lhs, rhs) in fix.lhs.iter().zip(fix.rhs.iter()) {
                proc_infos.borrow_mut().insert(
                    *lhs,
                    ProcInfo {
                        refcount: 0,
                        op_refcount: 0,
                        proc: *rhs,
                    },
                );
            }
        }

        _ => (),
    });

    let mut to_expand = HashMap::<LVarRef<'gc>, ProcRef<'gc>>::new();
    for (var, info) in proc_infos.into_inner().into_iter() {
        if info.op_refcount != 0 && info.refcount - info.op_refcount == 1 {
            to_expand.insert(var, info.proc);
        }
    }

    to_expand
}

static_symbols!(SYM_APPLY = "apply");

fn do_eta_expand<'gc>(
    ctx: Context<'gc>,
    lexical: TermRef<'gc>,
    to_expand: &HashMap<LVarRef<'gc>, ProcRef<'gc>>,
) -> Option<TermRef<'gc>> {
    let TermKind::LRef(var) = lexical.kind else {
        return None;
    };
    match to_expand.get(&var) {
        None => None,
        Some(proc) => {
            let args = proc
                .args
                .iter()
                .map(|arg| lref(ctx, *arg))
                .chain(proc.variadic.iter().map(|arg| lref(ctx, *arg)))
                .collect::<Vec<_>>();
            let body = if let Some(_) = proc.variadic {
                let mut args = args;
                args.insert(0, lexical);
                prim_call_term(ctx, sym_apply(ctx).into(), args, lexical.source())
            } else {
                call_term(ctx, lexical, args, lexical.source())
            };

            Some(Gc::new(
                *ctx,
                Term {
                    source: proc.source.into(),
                    kind: TermKind::Proc(Gc::new(
                        *ctx,
                        Proc {
                            args: proc.args,
                            name: proc.name,
                            source: proc.source,
                            variadic: proc.variadic,
                            body,
                            meta: proc.meta,
                        },
                    )),
                },
            ))
        }
    }
}

fn do_eta_reduce<'gc>(ctx: Context<'gc>, proc: ProcRef<'gc>) -> Option<TermRef<'gc>> {
    if let TermKind::Call(rator, rands) = proc.body.kind
        && let TermKind::LRef(var) = rator.kind
    {
        if rands.len() != proc.args.len() {
            return None;
        }
        rands
            .iter()
            .enumerate()
            .all(|(i, arg)| {
                if let TermKind::LRef(arg) = arg.kind {
                    Gc::ptr_eq(arg, proc.args[i])
                } else {
                    false
                }
            })
            .then(|| lref(ctx, var))
    } else if let TermKind::PrimCall(prim, rands) = proc.body.kind
        && prim == sym_apply(ctx).into()
        && rands.len() >= 2
        && let TermKind::LRef(var) = rands[0].kind
    {
        let Some(variadic) = proc.variadic else {
            return None;
        };

        if rands.len() != proc.args.len() + 2 {
            return None;
        }

        if !rands
            .iter()
            .skip(1)
            .take(proc.args.len())
            .zip(proc.args.iter())
            .all(|(term, formal)| matches!(term.kind, TermKind::LRef(name) if Gc::ptr_eq(name, *formal)))
        {
            return None;
        }

        match rands.last() {
            Some(last) => match last.kind {
                TermKind::LRef(arg) if Gc::ptr_eq(arg, variadic) => Some(lref(ctx, var)),
                _ => None,
            },
            None => None,
        }
    } else {
        None
    }
}
