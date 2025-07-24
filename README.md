# CapyScheme

R7RS compiler and runtime written in Rust.

# Goals

- Continuatio-Passing Style compiler based on [Compiling with Continuations, Continued](https://www.microsoft.com/en-us/research/wp-content/uploads/2007/10/compilingwithcontinuationscontinued.pdf) together with JIT
compiler utilizing Cranelift to emit native code in CPS style.
- Runtime with support for loading modules, native extensions, and fast GC.
- User-friendly: develop standard library and set of utilities to make using Scheme easier.
- Interactive: Provide a REPL with auto-complete, debugger, disassembler etc.
- Run on WASM (TBD): Be able to run on WASM. This probably requires changing compiler and runtime
significantly so this is not done yet. 