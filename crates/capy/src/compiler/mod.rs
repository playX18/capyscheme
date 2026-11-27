//! The bootstrap compiler: what stage-0 uses before the Scheme compiler
//! (`pkgs/core/src/core/compiler`) exists. Stage-1 and stage-2, and the installed runtime,
//! compile through Scheme only; this module is built with `feature = "bootstrap"` alone.
//!
//! - [`tree`]: the Tree IL node types of the Rust expander.
//! - [`direct`]: the seed emitter, Tree IL straight to a FASL program image.
//! - `bootstrap`: `compile_file`, a boot file through the Rust reader and expander.

mod bootstrap;
pub(crate) mod direct;
pub mod tree;

pub use bootstrap::compile_file;
