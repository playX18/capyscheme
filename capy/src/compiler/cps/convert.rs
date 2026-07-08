//! Conversion result types for graph CPS.

use std::fmt;

use super::graph::{FunctionId, Graph, Subterm};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConvertError {
    DeadTermLink(Subterm),
    DeadExprLink(super::graph::Subexpr),
    DeadFunctionLink(super::graph::FunctionLink),
    FunctionInFixHasNoReturnContinuation(FunctionId),
    FunctionInLetkHasReturnContinuation(FunctionId),
}

impl fmt::Display for ConvertError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::DeadTermLink(link) => write!(f, "dead term link while lowering graph: {link}"),
            Self::DeadExprLink(link) => {
                write!(f, "dead expr link while lowering graph: {link}")
            }
            Self::DeadFunctionLink(link) => {
                write!(f, "dead function link while lowering graph: {link}")
            }
            Self::FunctionInFixHasNoReturnContinuation(function) => {
                write!(f, "function in fix has no return continuation: {function}")
            }
            Self::FunctionInLetkHasReturnContinuation(function) => {
                write!(
                    f,
                    "continuation in letk has a return continuation: {function}"
                )
            }
        }
    }
}

impl std::error::Error for ConvertError {}

pub type ConvertResult<T> = Result<T, ConvertError>;

pub struct GraphProgram<'gc> {
    pub graph: Graph<'gc>,
    pub root: Subterm,
}

pub struct GraphFunctionProgram<'gc> {
    pub graph: Graph<'gc>,
    pub entry: FunctionId,
}

impl<'gc> GraphFunctionProgram<'gc> {
    pub fn root(&self) -> Subterm {
        self.graph[self.entry].body
    }
}
