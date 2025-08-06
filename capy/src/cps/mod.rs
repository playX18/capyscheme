//! Continuation-Passing Style (CPS) IR and compiler to machine code.

pub mod builder;
pub mod closure;
//pub mod contify;
pub mod fold;
pub mod free_vars;
pub mod optimizer;
pub mod peval;
pub mod pretty;
pub mod reify;
pub mod sexp;
pub mod term;

pub use optimizer::rewrite_func;
pub use reify::{ReifyInfo, reify};
