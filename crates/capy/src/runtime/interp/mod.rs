//! Interpreters: shared frame access plus the two bytecode engines.
//!
//! - [`frame`]: [`CallFrame`](frame::CallFrame), the one typed frame
//!   vocabulary every engine uses for slot traffic.
//! - [`rust_interp`]: the Rust engine (`CAPY_ENGINE=rust`), one handler
//!   function per opcode.
//! - `template_interp` (machine targets only): the template
//!   interpreter's decoding context, the templates it decodes through stay
//!   in [`codegen`](crate::runtime::codegen), shared with the baseline JIT.
//! - [`stack`]: the Scheme stack both engines grow their frames on,
//!   with first-class continuations.
//!
//! - [`slow`]: the slow paths both engines execute, and the shared
//!   control core (enter/return/raise/native) behind them.
//! - [`markers`]: machine return-address markers for frames and stubs.
//! - [`ic`]: inline-cache fills for slots and generic calls.
//! - [`profile`]: profiles and caches stored in code block metadata.
//! - [`runtime_procs`]: procedures the runtime provides to out-of-line
//!   slow paths.
//!
//! A new engine adds its loop here and drives [`slow`] and [`stack`]
//! like the existing ones do.

pub mod frame;
pub mod ic;
pub mod markers;
pub mod profile;
pub mod runtime_procs;
pub mod rust_interp;
pub mod slow;
pub mod stack;
#[cfg(all(not(windows), any(target_arch = "x86_64", target_arch = "aarch64")))]
pub mod template_interp;
