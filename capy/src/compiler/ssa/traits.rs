use cranelift_codegen::ir;
use cranelift_module::Module;

use crate::{compiler::ssa::SSABuilder, cps::term::Atom, runtime::value::Value};

pub trait IntoSSA<'gc, 'a, 'f, M: Module> {
    fn into_ssa(self, builder: &mut SSABuilder<'gc, 'a, 'f, M>) -> ir::Value;
}

impl<'gc, 'a, 'f, M: Module> IntoSSA<'gc, 'a, 'f, M> for Atom<'gc> {
    fn into_ssa(self, builder: &mut SSABuilder<'gc, 'a, 'f, M>) -> ir::Value {
        builder.atom(self)
    }
}

impl<'gc, 'a, 'f, M: Module> IntoSSA<'gc, 'a, 'f, M> for ir::Value {
    fn into_ssa(self, _builder: &mut SSABuilder<'gc, 'a, 'f, M>) -> ir::Value {
        self
    }
}

impl<'gc, 'a, 'f, M: Module> IntoSSA<'gc, 'a, 'f, M> for Value<'gc> {
    fn into_ssa(self, builder: &mut SSABuilder<'gc, 'a, 'f, M>) -> ir::Value {
        builder.atom(Atom::Constant(self))
    }
}
