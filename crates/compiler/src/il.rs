//! Immediate Language (IL) representation
//!
//! Simple tree-based language for the compiler's intermediate representation. Scheme code is lowered
//! to Tree IL, which is then converted to CPS IL.

use crate::{ast::P, cps};

pub mod assignment_elimination;
pub mod fix_letrec;
pub mod pretty;
pub mod primitives;
pub mod term;

/// Optimizations and lowering of the IL representation.
/// 
/// At the moment, this includes:
/// - Primitive resolution: finding and replacing calls to primitive functions with `primapp` and `primref` forms.
/// - Letrec fixing: transforming `letrec` bindings into a form that is more amenable to optimization.
/// - Assignment elimination: replacing assignments to local variables with boxed variables. 
pub fn optimize(form: P<term::IForm>) -> P<term::IForm> {
    let primitives = primitives::resolve_primitives(&form);
    let fixed = fix_letrec::fix_letrec(primitives);
    let boxed = assignment_elimination::assignment_elimination(fixed);

    boxed
}

pub fn to_cps(form: P<term::IForm>) -> P<cps::term::Func> {
    cps::convert::CPSBuilder::new()
        .convert_toplevel(&[form])
}
