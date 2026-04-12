use asmkit::core::operand::{Label, Operand};

use crate::object::VTable;

pub trait CodeStub<C> {
    /// Label at the stub entrypoint.
    fn entry(&self) -> Label;
    /// Label where the stub continues, if any.
    fn continuation(&self) -> Option<Label>;

    fn emit_code(&mut self, comp: &mut C);
}

pub struct AllocateObjectStub {
    pub entry: Label,
    pub continuation: Label,
    pub scratch1: Operand,
    pub scratch2: Operand,
    pub vtable_reg: Operand,
    pub vtable: Option<&'static VTable>,
    pub result: Operand,
}

pub struct AllocateVarSizedObjectStub {
    pub entry: Label,
    pub continuation: Label,
    pub scratch1: Operand,
    pub scratch2: Operand,
    pub vtable_reg: Operand,
    pub vtable: Option<&'static VTable>,
    pub var_size_in_bytes: Operand,
    pub result: Operand,
}
