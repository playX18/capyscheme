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

pub mod builder;
