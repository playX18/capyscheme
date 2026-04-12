pub mod clif;
pub mod options;
pub mod regalloc;
pub mod slotalloc;
pub mod stubs;
pub mod thunks;
pub mod x64;

/// A backend for JIT compilation.
///
/// Runtime can only use one backend at a time, but multiple backends can be compiled in and selected at runtime.
///
/// To change backend, pass `CAPY_JIT_BACKEND` environment variable to the process,
/// with value being one of `cranelift`, `lazyjit`, or `lbbv`.
///
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Backend {
    /// Simplest backend, which generates machine code from SSA IR using Cranelift.
    ///
    /// Pros:
    /// - Battle tested, Capy was using it for a long time in form of incremental AOT compilation.
    /// - Fast compilation time, suitable for development and debugging.
    /// Cons:
    /// - Not very good at optimizing code, so the generated code is not very fast.
    /// - Calls are super slow, they have to push arguments to `runstack` in order to do
    /// anything at all.
    Cranelift,
    /// A simple JIT backend that generates machine code directly from SSA IR, without any optimizations.
    ///
    /// Pros:
    /// - Very fast compilation time.
    /// - In theory, faster than Cranelift due to the fact that calls are now fast.
    /// Cons:
    /// - Not battle tested
    LazyJIT,
    /// Lazy Basic Block Versioning backend, generates code lazily per each *basic block* of SSA IR.
    ///
    /// This one is the best and most advanced JIT backend, it can specialize code efficeintly and remove
    /// a lot of redundant type checks.
    ///
    /// Pros:
    /// - Fast optimized code
    /// - Fast compilation time: code is compiled on basic-block basis rather than procedure basis
    /// - Calls are just as fast as in LazyJIT, same calling convention is used.
    ///
    /// Cons:
    /// - Not battle tested, it's a new backend and it may have some bugs.
    /// - More complex than the other two backends, so it may be way harder to debug.
    /// - More memory hungry than the other two backends, since it may generate multiple versions of the same basic block.
    LBBV,
}
