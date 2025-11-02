use rsgc::{Gc, alloc::ArrayRef, barrier, traits::IterGc};

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
        let wcont = Gc::write(&ctx, cont);
        barrier::field!(wcont, Cont, free_vars)
            .unlock()
            .set(Some(vars.iter().copied().collect_gc(&ctx)));
    }

    for (&func, vars) in fv.fvars.iter() {
        let wfunc = Gc::write(&ctx, func);
        barrier::field!(wfunc, Func, free_vars)
            .unlock()
            .set(Some(vars.iter().copied().collect_gc(&ctx)));
    }

    let mut stack = Vec::new();

    for var in fv.cvals.iter() {
        let Some(cont) = fv.conts.get(var) else {
            println!("can't reify {}", var.name);
            continue;
        };

        stack.push(*cont);
    }

    for (_, var) in fv.fvars.iter() {
        for var in var.iter() {
            let Some(cont) = fv.conts.get(&var) else {
                continue;
            };

            stack.push(*cont);
        }
    }

    while let Some(cont) = stack.pop() {
        println!("reifying continuation {}@{}", cont.name, cont.binding.name);
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
        functions: fv.funcs.values().copied().collect_gc(&ctx),
        continuations: fv.conts.values().copied().collect_gc(&ctx),
        free_vars: fv,
    }
}
