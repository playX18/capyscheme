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
use mimalloc::MiMalloc;
#[global_allocator]
static ALLOC: MiMalloc = MiMalloc;

#[derive(Default)]
struct DebugAlloc;

unsafe impl std::alloc::GlobalAlloc for DebugAlloc {
    unsafe fn alloc(&self, layout: std::alloc::Layout) -> *mut u8 {
        let ptr = unsafe { MiMalloc.alloc(layout) };

        ptr
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: std::alloc::Layout) {
        unsafe { MiMalloc.dealloc(ptr, layout) }
    }

    unsafe fn realloc(&self, ptr: *mut u8, layout: std::alloc::Layout, new_size: usize) -> *mut u8 {
        let ptr = unsafe { MiMalloc.realloc(ptr, layout, new_size) };

        ptr
    }

    unsafe fn alloc_zeroed(&self, layout: std::alloc::Layout) -> *mut u8 {
        let ptr = unsafe { MiMalloc.alloc_zeroed(layout) };

        ptr
    }
}
