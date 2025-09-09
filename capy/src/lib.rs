#![allow(internal_features)]
#![feature(
    arbitrary_self_types,
    try_trait_v2,
    link_llvm_intrinsics,
    min_specialization,
    portable_simd
)]
//pub mod compiler;
pub mod cps;
pub mod expander;
pub mod frontend;
//pub mod jit;
pub mod compiler;
pub mod runtime;
pub mod utils;

pub use rsgc;
use rsgc::Mutation;

pub fn take_yieldpoint(mc: &Mutation<'_>) -> bool {
    mc.take_yieldpoint() != 0
}
