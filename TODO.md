# A list of TODOs that would be nice to accomplish

## High-priority

- R6RS compliance: pass 95%+ of racket/r6rs test-suite
- Implement native extension API
- Optimize CPS compiler: it spends too much time in the optimization pipeline
- Extend standard-library to include TUI manipulation library, Sum-Tree based ropes, 
and other datastructures.
- Direct-call optimization: introduce a way for functions to call themselves without using shadow-stack
to pass arguments but pass them in registers directly.
- Improve debugging experience
    - Add DWARF information regarding variables in function so that its possible to check variables easily
    with GDB and LLDB
    - Improve integration with LLDB: Write a custom wrapper to print Scheme values, inspect shadow-stack etc.
- Improve CLI experience:
    - Interactive REPL with auto-complete, highlighting, error reporting and more
    - Cargo-like tool to manage dependencies and run projects with single command rather
    than requiring setting up paths. Possibly integrate it with Akku (or build on top of Akku)
    and make it work with other Scheme systems
- Beautify internal code: get rid of all usages of `native_fn!` macros and replace it with `#[scheme]`, verify
safety of the code, handle panics properly and so on.

## Medium-priority

- R7RS-large conformance
- capy/interp: introduce bytecode interpreted version of runtime that does not depend on Cranelift. Useful for portability and might actually be a way to introduce *tracing* JIT into the system. 
- Delimited continatuions
- Native FFI (get rid of libffi): we can use Cranelift to directly call into C functions. Requires quite some work
on changing CPS compiler to make it work but may result in performance boosts when we can inline calls directly into Scheme code. Current approach requires going from Scheme code through trampoline, Rust function, libffi trampoline and only then call does occur!

## Low-effort/research needed
- Self-host compiler entirely: maybe we need to add Cranelift bindigns for Scheme? (Bonus points: use r6rs-pffi so other Scheme systems can use them).
- Support for continuation marks: requires quite a lot of changes into CPS compiler if we want them to be fast, maybe there's other way to implement them? 