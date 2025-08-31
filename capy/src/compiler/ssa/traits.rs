use cranelift_codegen::ir;

use crate::{compiler::ssa::SSABuilder, cps::term::Atom, runtime::value::Value};

pub trait IntoSSA<'gc, 'a, 'f> {
    fn into_ssa(self, builder: &mut SSABuilder<'gc, 'a, 'f>) -> ir::Value;
}

impl<'gc, 'a, 'f> IntoSSA<'gc, 'a, 'f> for Atom<'gc> {
    fn into_ssa(self, builder: &mut SSABuilder<'gc, 'a, 'f>) -> ir::Value {
        builder.atom(self)
    }
}

impl<'gc, 'a, 'f> IntoSSA<'gc, 'a, 'f> for ir::Value {
    fn into_ssa(self, _builder: &mut SSABuilder<'gc, 'a, 'f>) -> ir::Value {
        self
    }
}

impl<'gc, 'a, 'f> IntoSSA<'gc, 'a, 'f> for Value<'gc> {
    fn into_ssa(self, builder: &mut SSABuilder<'gc, 'a, 'f>) -> ir::Value {
        builder.atom(Atom::Constant(self))
    }
}
