//! Machine code generator.

#[allow(unused_macros)]
macro_rules! unsupported_target {
    () => {
        unreachable!("supported targets are x86-64 and AArch64")
    };
}

pub mod addr;
pub(crate) mod emission;
pub mod gc;
pub mod image;
pub mod jit;
pub(crate) mod lazy;
pub mod masm;
pub mod operation;
pub(crate) mod reclaim;
pub(crate) mod reloc;
pub mod slow;
pub mod stubs;
pub mod template;
pub mod template_image;
pub(crate) mod unit;
