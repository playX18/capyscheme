use std::borrow::Cow;

use super::value::{IntoValue, Value};

/// A Scheme error.
///
/// Represented as a enum in Rust, but gets converted to R6RS condition
/// when thrown.
pub enum Violation<'gc> {
    /// Expected `expected` but got `got`.
    WrongTypeArgument {
        who: Option<Cow<'gc, str>>,
        position: usize,
        expected: Cow<'gc, str>,
        got: Value<'gc>,
        args: Value<'gc>,
    },

    InvalidApplication {
        value: Value<'gc>,
        args: Value<'gc>,
    },

    AssertionViolation {
        who: Option<Cow<'gc, str>>,
        message: Cow<'gc, str>,
        irritants: Value<'gc>,
    },

    WrongNumberOfArguments {
        who: Option<Cow<'gc, str>>,
        required_min: usize,
        required_max: Option<usize>,
        args: Value<'gc>,
    },
}

impl<'gc> std::fmt::Debug for Violation<'gc> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Violation::WrongTypeArgument {
                who,
                position,
                expected,
                got,
                args,
            } => {
                write!(
                    f,
                    "WrongTypeArgument {{ who: {:?}, position: {}, expected: {:?}, }}",
                    who, position, expected,
                )
            }
            Violation::InvalidApplication { value, args } => {
                write!(f, "InvalidApplication {{ }}")
            }
            Violation::AssertionViolation {
                who,
                message,
                irritants,
            } => {
                write!(
                    f,
                    "AssertionViolation {{ who: {:?}, message: {:?}, }}",
                    who, message,
                )
            }
            Violation::WrongNumberOfArguments {
                who,
                required_min,
                required_max,
                args,
            } => {
                write!(
                    f,
                    "WrongNumberOfArguments {{ who: {:?}, required_min: {}, required_max: {:?}, }}",
                    who, required_min, required_max
                )
            }
        }
    }
}

impl Violation<'_> {
    pub fn with_position(self, position: usize) -> Self {
        match self {
            Violation::WrongTypeArgument {
                who,
                expected,
                got,
                args,
                ..
            } => Violation::WrongTypeArgument {
                who,
                position,
                expected,
                got,
                args,
            },
            _ => self,
        }
    }
}

pub type Result<'gc, T> = std::result::Result<T, Value<'gc>>;

impl<'gc> IntoValue<'gc> for Violation<'gc> {
    fn into_value(self, ctx: super::Context<'gc>) -> Value<'gc> {
        todo!()
    }
}
