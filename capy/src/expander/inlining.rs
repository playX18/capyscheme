//! Cross-module inlining of functions and constants.
//!
//! This pass is not as powerful as loop-unrolling and inlining in CPS, but
//! it helps CPS IR to be optimized better by exposing more opportunities for
//! constant folding and dead code elimination.
