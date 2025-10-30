#![allow(
    clippy::type_complexity,
    internal_features,
    clippy::new_without_default,
    clippy::from_over_into,
    clippy::not_unsafe_ptr_arg_deref
)]
#![feature(
    arbitrary_self_types,
    try_trait_v2,
    link_llvm_intrinsics,
    min_specialization,
    portable_simd
)]

pub mod api;
pub mod compiler;
pub mod cps;
//pub mod cps2;
pub mod expander;
pub mod frontend;
pub mod runtime;
pub mod utils;

pub use rsgc;

pub mod prelude {
    pub use crate::global;
    pub use crate::list;
    pub use crate::runtime::modules::Module;
    pub use crate::runtime::modules::ModuleRef;
    pub use crate::runtime::modules::current_module;
    pub use crate::runtime::prelude::*;
    pub use crate::runtime::vm::{NativeCallContext, NativeCallReturn};
    pub use crate::vector;
    pub use capy_derive::scheme;
    pub use rsgc::{Global, Rootable, Trace, barrier};
}

pub use capy_derive::scheme;
