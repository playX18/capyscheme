//! Code metadata shared by the object model and the compilers.

pub(crate) mod exits;
pub(crate) mod metadata;

pub(crate) use exits::ExitProfile;
pub use metadata::GuardKind;
