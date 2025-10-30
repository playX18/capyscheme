use im::*;

use crate::{expander::core::LVarRef, prelude::Value};

/// CPS graph.
pub struct CPS<'gc> {}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Var(u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Atom<'gc> {
    Constant(Value<'gc>),
    Variable(LVarRef<'gc>),
}
