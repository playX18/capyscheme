//! Reify continuations into first-class values.
//!

use std::collections::HashSet;

use crate::cps::{
    free_vars::{FreeVars, get_fvf},
    term::{ContRef, FuncRef},
};

pub struct ReifyInfo<'gc> {
    pub reified: HashSet<ContRef<'gc>>,
    pub free_vars: FreeVars<'gc>,
}

/// Reify continuations in the given CPS term.
///
/// This pass does not produce a new term, but rather marks reified continuations as such.
///
/// Continuation is considered reified when it is used as value, captured in a closure, or passed to a function that expects
/// a continuation as an argument. When one continuation is captured by another continuation, it is not reified
/// unless the capturing continuation is also reified.
pub fn reify<'gc>(func: FuncRef<'gc>) -> ReifyInfo<'gc> {
    let mut fv = FreeVars::new();
    let _ = get_fvf(func, &mut fv);

    let mut reified = HashSet::new();

    let mut stack = Vec::new();

    for var in fv.cvals.iter() {
        let Some(cont) = fv.conts.get(var) else {
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
        if reified.contains(&cont) {
            continue;
        }

        reified.insert(cont);

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
        reified,
        free_vars: fv,
    }
}
