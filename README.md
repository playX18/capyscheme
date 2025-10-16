# CapyScheme

R6RS/R7RS compiler and runtime written in Rust.

# Goals

- Continuation-Passing Style compiler based on [Compiling with Continuations, Continued](https://www.microsoft.com/en-us/research/wp-content/uploads/2007/10/compilingwithcontinuationscontinued.pdf) together with JIT
  compiler utilizing Cranelift to emit native code in CPS style.
- Runtime with support for loading modules, native extensions, and fast GC.
- User-friendly: develop standard library and set of utilities to make using Scheme easier.
- Interactive: Provide a REPL with auto-complete, debugger, disassembler etc.

## Thanks

Big thanks to authors of [Larceny](https://github/larcenists/larceny), [Guile](https://www.gnu.org/software/guile/) and [Ypsilon](https://https://github.com/fujita-y/ypsilon). CapyScheme uses stdlib parts from all of them and takes inspiration from them.
