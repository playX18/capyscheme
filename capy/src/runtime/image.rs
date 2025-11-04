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

use rsgc::mmtk::util::Address;

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
