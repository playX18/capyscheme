//! Heap Images of the loaded runtime.
//!
//!
//! Works by compressing all loaded Scheme artifacts using `zstd`, and then
//! walking heap starting from the context to find all reachable objects.
//!
//! On deserialization, artifacts are decompressed and can be `dlopen`ed as
//! shared libraries. Heap state is restored by restoring directly from roots.
//!
//! ### NOTE
//!
//! When you add a new global variable through `global!`, `fluid!`, or similar macros
//! you need to make sure to register them for iamge serialization appropriately.

use std::sync::OnceLock;

use rsgc::mmtk::util::{
    Address,
    options::{Options, PlanSelector},
};

use crate::runtime::Context;

pub mod builder;
pub mod reader;

/// Collects all native procedures' addresses in the runtime.
///
/// Must be carefully modified once new native procedures are added to the runtime.
pub fn all_native_procedures<'gc>(ctx: Context<'gc>) -> Vec<Address> {
    use crate::runtime::{self, vm};
    let mut native_procedures = Vec::with_capacity(600);
    unsafe {
        runtime::fluids::fluid_ops::for_each_continuation(ctx, |addr| {
            native_procedures.push(Address::from_ptr(addr));
        });

        runtime::fluids::fluid_ops::for_each_fn(ctx, |addr| {
            native_procedures.push(Address::from_ptr(addr));
        });

        runtime::modules::module_ops::for_each_continuation(ctx, |addr| {
            native_procedures.push(Address::from_ptr(addr));
        });

        runtime::modules::module_ops::for_each_fn(ctx, |addr| {
            native_procedures.push(Address::from_ptr(addr));
        });

        vm::arith::arith_operations::for_each_continuation(ctx, |addr| {
            native_procedures.push(Address::from_ptr(addr));
        });

        vm::arith::arith_operations::for_each_fn(ctx, |addr| {
            native_procedures.push(Address::from_ptr(addr));
        });

        vm::base::base_ops::for_each_continuation(ctx, |addr| {
            native_procedures.push(Address::from_ptr(addr));
        });

        vm::base::base_ops::for_each_fn(ctx, |addr| {
            native_procedures.push(Address::from_ptr(addr));
        });

        vm::debug::debug_ops::for_each_continuation(ctx, |addr| {
            native_procedures.push(Address::from_ptr(addr));
        });

        vm::debug::debug_ops::for_each_fn(ctx, |addr| {
            native_procedures.push(Address::from_ptr(addr));
        });

        vm::dl::dl_ops::for_each_continuation(ctx, |addr| {
            native_procedures.push(Address::from_ptr(addr));
        });

        vm::dl::dl_ops::for_each_fn(ctx, |addr| {
            native_procedures.push(Address::from_ptr(addr));
        });

        vm::eval::eval_ops::for_each_continuation(ctx, |addr| {
            native_procedures.push(Address::from_ptr(addr));
        });

        vm::eval::eval_ops::for_each_fn(ctx, |addr| {
            native_procedures.push(Address::from_ptr(addr));
        });

        vm::ffi::ffi_ops::for_each_continuation(ctx, |addr| {
            native_procedures.push(Address::from_ptr(addr));
        });

        vm::ffi::ffi_ops::for_each_fn(ctx, |addr| {
            native_procedures.push(Address::from_ptr(addr));
        });

        vm::hash::hash_ops::for_each_continuation(ctx, |addr| {
            native_procedures.push(Address::from_ptr(addr));
        });

        vm::hash::hash_ops::for_each_fn(ctx, |addr| {
            native_procedures.push(Address::from_ptr(addr));
        });

        vm::io::io_ops::for_each_fn(ctx, |addr| {
            native_procedures.push(Address::from_ptr(addr));
        });

        vm::io::io_ops::for_each_continuation(ctx, |addr| {
            native_procedures.push(Address::from_ptr(addr));
        });

        vm::list::list_ops::for_each_fn(ctx, |addr| {
            native_procedures.push(Address::from_ptr(addr));
        });

        vm::list::list_ops::for_each_continuation(ctx, |addr| {
            native_procedures.push(Address::from_ptr(addr));
        });

        vm::load::load_ops::for_each_fn(ctx, |addr| {
            native_procedures.push(Address::from_ptr(addr));
        });

        vm::load::load_ops::for_each_continuation(ctx, |addr| {
            native_procedures.push(Address::from_ptr(addr));
        });

        vm::strings::string_ops::for_each_fn(ctx, |addr| {
            native_procedures.push(Address::from_ptr(addr));
        });

        vm::strings::string_ops::for_each_continuation(ctx, |addr| {
            native_procedures.push(Address::from_ptr(addr));
        });

        vm::syntax::syntax_ops::for_each_fn(ctx, |addr| {
            native_procedures.push(Address::from_ptr(addr));
        });

        vm::syntax::syntax_ops::for_each_continuation(ctx, |addr| {
            native_procedures.push(Address::from_ptr(addr));
        });

        vm::threading::threading_ops::for_each_fn(ctx, |addr| {
            native_procedures.push(Address::from_ptr(addr));
        });

        vm::vector::vector_ops::for_each_continuation(ctx, |addr| {
            native_procedures.push(Address::from_ptr(addr));
        });

        vm::vector::vector_ops::for_each_fn(ctx, |addr| {
            native_procedures.push(Address::from_ptr(addr));
        });

        vm::control::control_ops::for_each_fn(ctx, |addr| {
            native_procedures.push(Address::from_ptr(addr));
        });
        vm::control::control_ops::for_each_continuation(ctx, |addr| {
            native_procedures.push(Address::from_ptr(addr));
        });
        native_procedures.push(Address::from_ptr(
            vm::load::_raw_scm_cont_continue_loading_k as *const (),
        ));

        native_procedures.push(Address::from_ptr(
            vm::control::_raw_scm_cont_c_star as *const (),
        ));

        native_procedures.push(Address::from_ptr(
            vm::base::_raw_scm_cont_handler_cont as *const (),
        ));

        native_procedures.push(Address::from_ptr(vm::ffi::c_foreign_call as *const ()));
    }

    native_procedures
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum AllowedGC {
    /// Allow only to enable generational GC plans:
    /// - StickyImmix
    /// - GenImmix
    /// - GenCopy
    Generational,
    /// Allow only to enable concurrent GC plans:
    /// - ConcurrentImmix
    Concurrent,
    /// Allow only to enable regular GC plans:
    /// - MarkSweep
    /// - Immix
    /// - SemiSpace
    Regular,
}

impl AllowedGC {
    pub fn adjust_mmtk_options(&self, opts: &mut Options) {
        match self {
            AllowedGC::Generational => {
                if !matches!(
                    *opts.plan,
                    PlanSelector::StickyImmix | PlanSelector::GenImmix | PlanSelector::GenCopy
                ) {
                    println!(
                        ";; WARN: The loaded heap image only allows generational GC plans. Switching to GenImmix plan."
                    );
                    opts.plan.set(PlanSelector::GenImmix);
                }
            }

            AllowedGC::Concurrent => {
                if !matches!(*opts.plan, PlanSelector::ConcurrentImmix) {
                    println!(
                        ";; WARN: The loaded heap image only allows concurrent GC plans. Switching to ConcurrentImmix plan."
                    );
                    opts.plan.set(PlanSelector::ConcurrentImmix);
                }
            }

            AllowedGC::Regular => {
                if !matches!(
                    *opts.plan,
                    PlanSelector::MarkSweep | PlanSelector::Immix | PlanSelector::SemiSpace
                ) {
                    println!(
                        ";; WARN: The loaded heap image only allows regular GC plans. Switching to Immix plan."
                    );
                    opts.plan.set(PlanSelector::Immix);
                }
            }
        }
    }
}

/// A global static which indicates which GC strategies are allowed when
/// deserialized through the image. Image stores this information at the start.
///
/// If you build heap image with a specific GC flavor, you *only* can use similar
/// algorithms.
pub static ALLOWED_GC: OnceLock<AllowedGC> = OnceLock::new();
