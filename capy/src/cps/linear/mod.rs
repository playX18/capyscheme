mod builder;

#[cfg(test)]
mod tests;

pub use builder::linearize;
pub use crate::compiler::ssa::*;
pub(crate) use crate::compiler::ssa::finish_procedure;
pub(crate) use crate::compiler::ssa::infer_switches;
