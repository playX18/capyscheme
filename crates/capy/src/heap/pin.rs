//! Unified rooting vocabulary over [`crate::runtime::root`] and OopStorage.
//!
//! Prefer [`Scope`] / [`Pin`] for short-lived Values across `call_in_native`.
//! Prefer [`Frame`] for nest / SNI locals. Process-long handles stay on
//! [`crate::heap::Global`] / SNI `Global`.

pub use crate::root_scope;
pub use crate::runtime::root::{RootScope as Scope, Rooted as Pin, with_rooted_native};
pub use crate::runtime::sni::LocalFrame as Frame;

/// Create a [`Scope`] bound to `ctx`'s root stack.
#[macro_export]
macro_rules! scope {
    ($ctx:expr, $name:ident) => {
        let $name = $crate::heap::pin::Scope::new($ctx);
    };
}
