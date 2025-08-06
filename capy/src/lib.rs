#![feature(arbitrary_self_types)]
//pub mod compiler;
pub mod cps;
pub mod expander;
pub mod frontend;
//pub mod jit;
pub mod runtime;
pub mod utils;

pub use rsgc;
use rsgc::Mutation;

pub fn take_yieldpoint(mc: &Mutation<'_>) -> bool {
    mc.take_yieldpoint() != 0
}
