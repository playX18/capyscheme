use crate::ast::P;

pub mod term;
pub mod convert;
pub mod pretty;
pub mod optimizer;
pub mod contification;
pub mod reify;
pub mod free;

pub fn optimize(form: P<term::Term>) -> P<term::Term> {
    let shrunken = optimizer::rewrite(form);
    let contified = contification::contify(shrunken);

    contified 
}

pub fn optimize_func(form: P<term::Func>) -> P<term::Func> {
    let shrunken = optimizer::rewrite(form.body.clone());
    let contified = contification::contify(shrunken);

    form.with_body(contified)
}