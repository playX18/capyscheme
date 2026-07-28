//! C SNI (Scheme Native Interface) — embedding API.
//!
//! Public values are opaque [`ScmRef`] handles rooted in OopStorage. Raw
//! nanoboxed [`Value`]s are not part of the C ABI.

mod call;
mod cps;
mod env;
mod exceptions;
mod lifecycle;
mod load;
mod native;
mod oop;
mod pod;
mod raise;
mod refs;
mod util;
mod values;
mod vtable;

pub use call::*;
pub use cps::*;
pub use env::*;
pub use exceptions::*;
pub use lifecycle::*;
pub use load::*;
pub use native::*;
pub use oop::*;
pub use pod::*;
pub use raise::*;
pub use refs::*;
pub use values::*;
pub use vtable::*;
