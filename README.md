# CapyScheme

R6RS/R7RS compiler and runtime written in Rust.

## Features

- Full R6RS and R7RS-small support.
- Package manager and build system provided via `capy-pkg`, no need to write annoying makefiles or build scripts.
- Static compilation: Capy can statically link your project either to a FASL file you can redistribute with runtime, or to a single *binary* for target which contains everything needed to run program. Just run `capy dist --format portable|rpm|deb --static-native`, or `capy install`.
- Complex compiler pipeline for Scheme: Scheme -> TreeIL -> Graph CPS -> CFG -> Bytecode.
  Graph CPS is implemented based on [Shrinking Lambda Expressions in Linear Time](https://dl.acm.org/doi/10.1017/S0956796897002839) and [Compiling with Continuations, Continue](https://www.microsoft.com/en-us/research/wp-content/uploads/2007/10/compilingwithcontinuationscontinued.pdf), which 
  shrinks down most of CPS opts to O(N) aka linear time, and allows us compiling incredibly huge files in mere seconds, instead of minutes, and we apply complicated inlining optimizations, are able to turn recursive functions into tail-recursive functions, infer switches from `(if ... (if ...))` blocks, and so much more.
  Bytecode lowering is also using linear-scan register allocation which allows us to speed up interpreter by emitting more efficient code.
- Adaptive Just-In-Time compilation pipeline:
- - Tempalte Interpeter as Tier 0: fully written in assembly and executes code as effectively as possible.
- - Template JIT as Tier 1: uses the same code as interpreter, but stitches opcodes together (Copy&Patch JIT) to remove dispatch overhead, on top of that adds lazy inline cache generation for calls,
    arithmetic sites, slot accesses.
- - Optimizing JIT as Tier 2: full complex JIT with many optimizations like unboxing, Partial Escape Analysis, bounds check eliminations, and so much more: code compiled by this JIT runs almost as fast as native code or even faster in some circumstances\*, beats ChezScheme\*
    in some benchmarks, and adapts dynamically to your code.
- Garbage Collection via [MMTk](https://github.com/mmtk/mmtk-core): you can choose from range of GC algorithms suitable for *your* usage goals. Capy defaults to ConcurrentImmix which brings pause times to a minimum and also defragments memory.

\* While Capy is indeed faster than Chez and even Rust on some code, it is not always the fastest. There are a lot of optimizations and strength reductions that are missing from JIT pipeline or compilation pipeline. It is being constantly improved and the goal is to be faster than Chez eventually.
# Capy 4.0.0

Capy 4.0.0 adds `capy-pkg` tool which is a Cargo-like package manager and build system for Capy and we also bring a bunch of improvements to JIT and compiler parts:
- Scheme code now can be linked statically to libcapy_static.a 
- Whole-Program Optimization is available 
- Lazy deopts were added to JIT which improve call-heavy benchmarks by ~20-30%, and also allow LICM to do better work.
- Partial Escape Analysis in JIT as a replacement to SROA.
- u64 handling was added to JIT which improve SHA1, SHA256 and srfi-27 performance significantly, nearing native speeds.
- A lot of fixes to existing libraries in std/ and core/ packages.
- (capy compress), (capy html), (capy json), and (capy sxml) were added to std.

# Capy 3.0.0

Capy 3.0.0 replaces the native code backend with a bytecode runtime.

- The compiler now produces bytecode instead of native code. The Cranelift backend, static basic block versioning (SBBV), and relocatable code blocks are gone.
- Code runs on a Rust interpreter or a template interpreter generated at startup, and hot procedures are compiled by a baseline JIT on worker threads.
- Startup is faster.
- CLOS is fast. The JIT and the template interpreter use inline caches for slot access and generic dispatch. Only the SBCL MOP is faster than Capy.
- Rewritten I/O system: ports are no longer vendored from Larceny. Capy has its own port system with better memory management and less overhead. `format`/`printf` calls are on average 10x faster and use 100-200B of heap memory instead of 12KiB. The new ports also fully support UTF-16 and UTF-8.

# Performance

Capy v3+ is on average 1.3x faster than Capy v2 across r7rs-benchmarks. Loop-heavy math benchmarks are still faster on the Cranelift AOT backend. New optimizing JIT tiers in coming versions should close that gap.

# Installation

## Homebrew (macOS arm64, Linux x86_64/arm64)

```sh
brew tap playX18/tap
brew install capyscheme            # stable release: capy, capyc
brew install capyscheme-nightly    # nightly build: capy-nightly, capyc-nightly
```

Prebuilt tarballs, `.deb` and `.rpm` packages are also attached to each [release](https://github.com/playX18/capyscheme/releases).

## From source

Supported targets are macOS and Linux on x86-64 and aarch64. Windows is not supported.

You need a Rust nightly toolchain and Bash.

```sh
./bootstrap.sh 
stage-2/capy install --toolchain stage-2 --prefix $HOME/.local   
(cd pkgs && ../stage-2/capy test --workspace)  
```

# Quick start

`capy` builds packages, fetches their dependencies, and runs scripts. A package is a directory
with a `package.scm` and a directory of libraries.

```sh
$ capy new hello && cd hello
$ capy run                        # builds, then runs src/main.scm
Hello, world!
$ capy test                       # runs tests/**/*.scm and tests/**/*.sps (SRFI 64)
$ capy install                    # installs the programs of the package into ~/.capy/bin
$ capy dist --format deb          # Produce deb package, you can also produce RPM and portable distributions.
$ capy build --release            # compiles into target/release
$ capy add https://github.com/x/json   # adds a dependency on its latest vX.Y.Z tag, writes package.lock
```

A single file runs without a package:

```sh
$ capy script.scm arg1 arg2
```

A script can ask for packages in a comment block at its top. The driver fetches and builds them
once, into `~/.cache/capy/scripts/`:

```scheme
#!/usr/bin/env capy
#|capy
(dependencies
  (json (git "https://github.com/x/json") (version "1.2")))
|#
(import (rnrs) (json))
```


# R6RS/R7RS support

All of R6RS and R7RS should be supported, work on supporting R7RS-large is ongoing. 

You can run R6RS test suite from [racket/r6rs](https://github.com/racket/r6rs):
```sh 
$ cd pkgs/std && capy test r6rs
```

## Thanks

Thanks to the authors of [Loko](https://gitlab.com/weinholt/loko/), [Larceny](https://github.com/larcenists/larceny), [Guile](https://www.gnu.org/software/guile/) and [Ypsilon](https://github.com/fujita-y/ypsilon). CapyScheme uses parts of the standard library from all of them and takes inspiration from them.
