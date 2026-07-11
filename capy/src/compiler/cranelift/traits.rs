use cranelift_codegen::ir;

use crate::{
    compiler::{cps::graph::Atom, cranelift::SsaBuilder},
    runtime::value::Value,
};

pub trait IntoSsa<'gc, 'a, 'f> {
    fn into_ssa(self, builder: &mut SsaBuilder<'gc, 'a, 'f>) -> ir::Value;
}

impl<'gc, 'a, 'f> IntoSsa<'gc, 'a, 'f> for Atom<'gc> {
    fn into_ssa(self, builder: &mut SsaBuilder<'gc, 'a, 'f>) -> ir::Value {
        builder.atom(self)
    }
}

impl<'gc, 'a, 'f> IntoSsa<'gc, 'a, 'f> for ir::Value {
    fn into_ssa(self, _builder: &mut SsaBuilder<'gc, 'a, 'f>) -> ir::Value {
        self
    }
}

impl<'gc, 'a, 'f> IntoSsa<'gc, 'a, 'f> for Value<'gc> {
    fn into_ssa(self, builder: &mut SsaBuilder<'gc, 'a, 'f>) -> ir::Value {
        builder.atom(Atom::Constant(self))
    }
}
