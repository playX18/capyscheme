use crate::rsgc::{Gc, alloc::ArrayRef, barrier, traits::IterGc};

use crate::{
    cps::{
        free_vars::{FreeVars, get_fvf},
        term::{Cont, ContRef, Func, FuncRef},
    },
    runtime::Context,
};

pub struct ReifyInfo<'gc> {
    pub entrypoint: FuncRef<'gc>,
    pub functions: ArrayRef<'gc, FuncRef<'gc>>,
    pub continuations: ArrayRef<'gc, ContRef<'gc>>,
    pub free_vars: FreeVars<'gc>,
}

/// Reify the code in CPS function.
///
/// This pass collects free variables for each continuation and function, marks continuations that are "reified" (aka allocated on heap)
/// and also returns all continuations and functions in the program.
pub fn reify<'gc>(ctx: Context<'gc>, func: FuncRef<'gc>) -> ReifyInfo<'gc> {
    let mut fv = FreeVars::new();

    let _ = get_fvf(func, &mut fv);
    fv.fvars.insert(func, Default::default());

    for (&cont, vars) in fv.cvars.iter() {
        let wcont = Gc::write(*ctx, cont);
        barrier::field!(wcont, Cont, free_vars)
            .unlock()
            .set(Some(vars.iter().copied().collect_gc(*ctx)));
    }

    for (&func, vars) in fv.fvars.iter() {
        let wfunc = Gc::write(*ctx, func);
        barrier::field!(wfunc, Func, free_vars)
            .unlock()
            .set(Some(vars.iter().copied().collect_gc(*ctx)));
    }

    for cont in fv.conts.values() {
        cont.reified.set(false);
    }

    let mut stack = Vec::new();

    for var in fv.cvals.iter() {
        let Some(cont) = fv.conts.get(var) else {
            continue;
        };

        stack.push(*cont);
    }

    for (_, var) in fv.fvars.iter() {
        for var in var.iter() {
            let Some(cont) = fv.conts.get(var) else {
                continue;
            };

            stack.push(*cont);
        }
    }

    while let Some(cont) = stack.pop() {
        if cont.reified.get() {
            continue;
        }

        cont.reified.set(true);

        let Some(vars) = fv.cvars.get(&cont) else {
            continue;
        };

        for var in vars.iter() {
            if let Some(cont) = fv.conts.get(var) {
                stack.push(*cont);
            }
        }
    }

    ReifyInfo {
        entrypoint: func,
        functions: fv.funcs.values().copied().collect_gc(*ctx),
        continuations: fv.conts.values().copied().collect_gc(*ctx),
        free_vars: fv,
    }
}

#[cfg(test)]
mod tests {
    use super::reify;
    use crate::{
        cps::term::{Atom, Cont, Func, Term},
        expander::core::{LVarRef, fresh_lvar},
        rsgc::{Gc, alloc::Array, cell::Lock},
        runtime::{
            Context, Scheme,
            value::{Symbol, Value},
        },
    };
    use std::cell::Cell;

    fn with_ctx(f: impl for<'gc> FnOnce(Context<'gc>)) {
        let scm = Scheme::new_uninit();
        scm.enter(f);
    }

    fn lvar<'gc>(ctx: Context<'gc>, name: &str) -> LVarRef<'gc> {
        fresh_lvar(ctx, Symbol::from_str(ctx, name).into())
    }

    #[test]
    fn reify_recomputes_transitive_continuation_marks() {
        with_ctx(|ctx| {
            let f = lvar(ctx, "f");
            let ret = lvar(ctx, "ret");
            let callee = lvar(ctx, "callee");
            let outer = lvar(ctx, "outer");
            let inner = lvar(ctx, "inner");
            let atoms = Array::from_slice(*ctx, []);

            let inner_cont = Gc::new(
                *ctx,
                Cont {
                    name: Value::new(false),
                    binding: inner,
                    args: Array::from_slice(*ctx, []),
                    variadic: None,
                    body: Lock::new(Gc::new(*ctx, Term::Continue(ret, atoms, Value::new(false)))),
                    source: Value::new(false),
                    free_vars: Lock::new(None),
                    reified: Cell::new(false),
                    cold: false,
                    noinline: false,
                    meta: Value::new(false),
                },
            );

            let outer_cont = Gc::new(
                *ctx,
                Cont {
                    name: Value::new(false),
                    binding: outer,
                    args: Array::from_slice(*ctx, []),
                    variadic: None,
                    body: Lock::new(Gc::new(
                        *ctx,
                        Term::Continue(inner, atoms, Value::new(false)),
                    )),
                    source: Value::new(false),
                    free_vars: Lock::new(None),
                    reified: Cell::new(true),
                    cold: false,
                    noinline: false,
                    meta: Value::new(false),
                },
            );

            let body = Gc::new(
                *ctx,
                Term::Letk(
                    Array::from_slice(*ctx, [inner_cont]),
                    Gc::new(
                        *ctx,
                        Term::Letk(
                            Array::from_slice(*ctx, [outer_cont]),
                            Gc::new(
                                *ctx,
                                Term::App(
                                    Atom::Local(callee),
                                    outer,
                                    Array::from_slice(*ctx, []),
                                    Value::new(false),
                                ),
                            ),
                        ),
                    ),
                ),
            );

            let func = Gc::new(
                *ctx,
                Func {
                    name: Value::new(false),
                    source: Value::new(false),
                    binding: f,
                    return_cont: ret,
                    args: Array::from_slice(*ctx, []),
                    variadic: None,
                    body: Lock::new(body),
                    free_vars: Lock::new(None),
                    meta: Value::new(false),
                },
            );

            reify(ctx, func);

            assert!(outer_cont.reified.get());
            assert!(inner_cont.reified.get());
        });
    }
}
