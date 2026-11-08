//! The outcome of entering Scheme from Rust.

use std::ops::{FromResidual, Try};

use crate::heap::Trace;

use super::Value;

/// A Scheme VM execution result.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Trace)]
#[collect(no_drop)]
#[repr(C)]
pub enum ExecutionResult<'gc> {
    /// VM executed successfully and value was returned.
    Ok(Value<'gc>),
    /// VM executed, but error was thrown.
    Err(Value<'gc>),
}

impl<'gc> FromResidual<Value<'gc>> for ExecutionResult<'gc> {
    fn from_residual(residual: Value<'gc>) -> Self {
        Self::Err(residual)
    }
}

impl<'gc> std::ops::Residual<Value<'gc>> for Value<'gc> {
    type TryType = ExecutionResult<'gc>;
}

impl<'gc> Try for ExecutionResult<'gc> {
    type Output = Value<'gc>;
    type Residual = Value<'gc>;

    fn from_output(output: Self::Output) -> Self {
        Self::Ok(output)
    }

    fn branch(self) -> std::ops::ControlFlow<Self::Residual, Self::Output> {
        match self {
            ExecutionResult::Ok(v) => std::ops::ControlFlow::Continue(v),
            ExecutionResult::Err(e) => std::ops::ControlFlow::Break(e),
        }
    }
}

