# Implementation Overview

CapyScheme implements R6RS and R7RS. It has three layers:

- The **front end** is written in Scheme and lives in `lib/`. It holds the
  reader, the macro expander, the module system, and the first compiler
  passes.
- The **back end** is written in Rust and lives in `crates/`. It holds the
  compiler from TreeIL down to bytecode, the interpreters and JIT that run
  it, the runtime, and the garbage collector.
- The **C API** for embedding and native extensions is declared in
  `c/capy.h`. The `libcapy` shared library implements it (`libcapy.so`, or
  `libcapy.dylib` on macOS). It is built from `crates/capy/`.

## From source to machine code

The front end ends at TreeIL. Everything below it is Rust. Machine code is
never produced by the compiler. The JIT makes it at run time from bytecode
(see `docs/BYTECODE.md` and `docs/JIT.md`).

## Source layout

- `lib/boot/` holds the boot libraries: the reader (`reader.scm`), the
  psyntax expander (`psyntax.scm` and its pre-expanded copy
  `psyntax-exp.scm`), the TreeIL terms (`expand.scm`), the evaluator and
  file compiler driver (`eval.scm`, `compiler.scm`), and the CLI (`cli.scm`).
- `lib/capy/compiler/tree-il/` holds the self-hosted TreeIL passes:
  `terms.scm`, `fold.scm`, `primitives.scm`, `resolve-free-vars.scm`, and
  `letrectify.scm`.
- `crates/capy/src/expander/` holds Rust versions of the expander and the
  TreeIL passes. It is used only for bootstrapping and compiles only R4RS-like
  code.
- `crates/capy/src/compiler/` is the compiler back end:
  - `pipeline.rs` orchestrates lowering.
  - `tree/` has the TreeIL types.
  - `cps/` has graph CPS and its optimizer.
  - `cfg/` has the CFG IR, lowering from graph CPS, and cleanup passes.
  - `bytecode/` turns CFG into bytecode (`lower.rs`, `disasm.rs`).
  - `object.rs` emits FASL.
  - `dump.rs` is the dump machinery.
- `crates/capy/src/bytecode/` has the instruction set and the verifier.
- `crates/capy/src/runtime/` is the runtime:
  - `value/` has the object representations.
  - `class/` has classes and inline caches.
  - `vm/` has runtime functions called from interpreters and JIT code. This
    includes `vm/load/`, the compile, cache, and load path.
  - `fasl/` is the FASL reader and writer.
  - `thread.rs` and `vmthread.rs` hold mutator threads and the per-thread
    `State` and `Tc`.
  - `interp/` is the Rust interpreter, the Scheme stack, and shared slow
    paths.
  - `codegen/` is the template interpreter and the JIT, built with
    `onlineasm`.
  - `prims/` is the primitive table.
  - `profile/` is the sampling profiler.
  - `sabi.rs` is the return-address ABI and the offsets in `Tc`.
  - `modules.rs`, `root.rs`, `code_memory.rs`, `sni.rs`, and `gdb_jit.rs`
    cover modules, root scopes, executable memory, SNI support, and GDB
    registration.
- `crates/capy/src/heap/` is the garbage collector, built on MMTk.
- `Makefile` builds the Rust runtime and bootstraps the Scheme libraries.

Related documents:

| Topic | Document |
|-------|----------|
| Instructions, frames, calls, engines | `docs/BYTECODE.md` |
| Values, classes, inline caches | `docs/OBJECTS.md` |
| JIT | `docs/JIT.md` |
| Garbage collector | `docs/HEAP.md` |
| Native interface | `docs/SNI.md` |
| Build stages | `docs/BOOTSTRAP.md` |

# Compiled Files and Boot Files

A Scheme file compiles to a `.fasl` file. FASL is the own binary format of
Capy. It is not ELF, PE, or any platform object format. One compressed image
holds, for every code block, the bytecode, the constant pools, the liveness
masks, the metadata counts, and the source maps.

`compile-file` (`lib/boot/eval.scm`) produces it. `lib/boot/compiler.scm`
installs the active compiler. The FASL reader in
`crates/capy/src/runtime/fasl/` loads it.

## The compile cache

When no output path is given, the result goes to a cache directory:

```text
 $XDG_CACHE_HOME/capy/cache/<version>/<barrier-kind>/
```

`XDG_CACHE_HOME` falls back to the platform default, such as `~/.cache`.
File names inside are scoped by architecture
(`crates/capy/src/runtime/vm/load/paths.rs`). Scheme computes the name in
`compiled-file-name` (`lib/boot/eval.scm`) from the `%compile-fallback-path`
parameter.

Bytecode does not depend on the GC plan or on its write barrier. One set of
compiled libraries therefore serves every plan.

## Boot libraries

The libraries in `BOOT_SRCS` in the `Makefile` are ordinary Scheme files.
One is special. `lib/boot/psyntax-exp.scm` is the pre-expanded copy of the
psyntax expander. It lets the system load the expander before the expander
can expand itself. The file is checked in and rarely changes. After editing
`lib/boot/psyntax.scm`, regenerate it with `make COMPILE_PSYNTAX=1`. That
runs `lib/boot/compile-psyntax.scm` during stage 0.

# Build System

The `Makefile` drives everything. `make build` is the default target. It
builds the Rust runtime, then runs the three bootstrap stages.

| Variable | Meaning | Default |
|----------|---------|---------|
| `PROFILE` | cargo profile | `release` |
| `TARGET` | target triple (`cross` is used if it differs from the host) | host |
| `PREFIX` | install root | `~/.local/share` |
| `VERSION` | taken from `crates/capy/Cargo.toml` | |
| `COMPILE_PSYNTAX` | regenerate `psyntax-exp.scm` | `0` |
| `TEST_TIMEOUT` | seconds per test file | `300` |

The environment for running `capy` during the build is collected in
`CAPY_ENV`:

- `MMTK_PLAN` (default `GenImmix`)
- `XDG_CACHE_HOME` (pointed at `stage-0/cache`)
- `CAPY_LOAD_PATH=./lib`
- `CAPY_GC_MAX_HEAP`
- `RUST_MIN_STACK`
- dump flags: `CAPY_COMPILE_DUMP`, `CAPY_COMPILE_DUMP_DIR`,
  `CAPY_DUMP_DIR`, `CAPY_BARRIER_KIND`

`capy --help-flags` lists every flag the runtime reads.

The runtime is two cargo builds. The `capy` crate produces `libcapy.so`. The
`capy-cli` crate produces the thin `capy` and `capyc` executables that link
against it. `capy` is the script runner and REPL. `capyc` is the standalone
file compiler. Both translate `--flag` and `--no-flag` options (such as
`--jit`, `--engine template`, and `--gc-plan`) into environment variables
before the VM boots.

## Bootstrap stages

1. `cargo build` with the `bootstrap` feature produces stage-0: `capy`,
   `capyc` and `libcapy.so`. They warm the compile cache.
2. `stage-0/capyc` compiles every library into `stage-1/compiled/`.
3. `stage-1/capyc` compiles every library again into `stage-2/compiled/`,
   the installable result.


- `make stage-0` builds the runtime with the `bootstrap` cargo feature. That
  feature adds the Rust core expander and the Tree-Sitter reader. The
  binaries land in `stage-0/`. The fresh binary then compiles the boot
  libraries for a few imports, which warms the cache. With
  `COMPILE_PSYNTAX=1` it regenerates `psyntax-exp.scm` first.
- `make stage-1` recompiles every library with `stage-0/capyc`. The set is
  boot, core, rnrs, srfi, scheme (R7RS), capy, common, and the CLI. The
  result is compiled by a compiler built from the current sources.
- `make stage-2` recompiles everything with `stage-1/capyc` into
  `stage-2/compiled/`.

## Install

`make install` does an FHS-style install under `PREFIX`:

| Content | Location |
|---------|----------|
| binaries | `bin/` |
| `libcapy.so` | `lib/` |
| `capy.h` | `include/` |
| libraries | `share/capy/` |
| compiled files | `lib/capy/compiled/` |

`make install-portable` installs a self-contained tree under
`$(PREFIX)/capy/$(VERSION)`. `make dist-portable`, `dist-deb`, and
`dist-rpm` build packages.

## Changing the instruction set or FASL format

After such a change:

1. Bump `FASL_VERSION`. The FASL header also stores
   `bytecode::def::format_hash()`, which changes when the opcode list or
   the primitive order changes.
2. Remove the compile cache (`~/.cache/capy`).
3. Rebuild.

The loader rejects stale FASL images.

# Tests

Tests live under `tests/`:

- `tests/lib/` mirrors `lib/` one to one. Each file tests the matching
  library with SRFI-64 style forms.
- `tests/r6rs/` is the R6RS conformance suite, started by
  `tests/r6rs/run-via-eval.sps`.

`make test` runs every `tests/lib/**/*.scm` under `timeout` with:

```sh
capy -L lib --fresh-auto-compile -s <file>
```

A run fails if the exit code is nonzero or the output mentions unexpected
failures or successes. Then it runs the R6RS suite:

```sh
capy -L lib -L . --fresh-auto-compile --r6rs -s tests/r6rs/run-via-eval.sps
```

By default it uses `stage-0/capy` and builds stage 0 first if needed. Set
`CAPY=/path/to/capy` to test another binary, such as an installed one. To run
one library test, run the same command by hand:

```sh
capy -L lib --fresh-auto-compile -s tests/lib/core/lists.scm
```

# Adding Functionality

New Scheme libraries go under `lib/` in the fitting subtree (`core/`,
`rnrs/`, `scheme/`, `srfi/`, `capy/`, `common/`). To compile and install a
library during bootstrap, add it to the matching `*_SRCS` list in the
`Makefile`.

## Adding a primitive

A primitive exists at three levels, so it touches several places:

1. **Name tables.** The TreeIL resolution and expansion logic exists twice
   and the copies must agree. The Rust copy is in
   `crates/capy/src/expander/primitives.rs` (`interesting_prim_names!`,
   `resolve_primitives`, `expand_primitives`) and serves bootstrap. The
   Scheme copy is in `lib/capy/compiler/tree-il/primitives.scm`
   (`interesting-primitive-names`, `resolve-primitives`,
   `expand-primitives`) and serves the self-hosted compiler.
2. **Implementation.** Write a thunk under
   `crates/capy/src/runtime/vm/thunks/`. Usually expose it to Scheme with a
   `#[scheme]` binding.
3. **Bytecode.** If bytecode can name the primitive, add a variant to the
   `Primitive` enum (`runtime/prims/def.rs`) and a case in `call_prim` and
   the `PRIM_WRAPPERS` table (`runtime/prims/calls.rs`). Append new
   variants and bump `FASL_VERSION`, since the enum order is part of the
   format.

`compiler/bytecode/lower.rs` emits `call_primitive*` for a primitive by
default. To give it a dedicated opcode:

1. Add the instruction to `bytecode/def.rs`.
2. Lower it in `lower.rs`.
3. Add a handler to the Rust interpreter.
4. Add a template for the template interpreter and the JIT.

# Compilation Pipeline

## Front end

The reader (`lib/boot/reader.scm`) reads all forms with `read-syntax`. The
result is annotated S-expressions whose annotations carry source vectors.
Psyntax expands them to TreeIL (`compile-tree-il` in `lib/boot/eval.scm`).
Four Scheme passes then run in order:

1. `resolve-primitives`
2. `expand-primitives`
3. `resolve-free-vars`
4. `letrectify`

Finally the `%compile` primitive (`runtime/vm/load/scheme.rs`) converts the
Scheme TreeIL records into Rust TreeIL with `TermConverter`
(`runtime/vm/expand.rs`). It then runs the native pipeline.

## TreeIL

TreeIL is a small term language. The Rust definition is `TermKind` in
`crates/capy/src/compiler/tree/mod.rs`. The Scheme records in
`lib/boot/expand.scm` mirror it.

| Term | Meaning |
|------|---------|
| `lref`, `lset` | local variable read, write |
| `module-ref`, `module-set!`, `toplevel-ref`, `toplevel-set!` | module bindings |
| `define` | definition |
| `primref`, `primcall` | primitives |
| `if`, `seq`, `call`, `values`, `receive` | control |
| `with-continuation-mark` | continuation marks |
| `fix` | mutually recursive procedures (made only by the letrec pass) |
| `let` | binding, with a style: plain, `let*`, or `letrec` |
| `proc` | procedure literal |
| `const` | constant |

## Native TreeIL passes

`lower_expanded_to_cps` in `compiler/pipeline.rs` runs these in order:

1. `fix_letrec` (`expander/recursive_bindings.rs`) rewrites `letrec`. Simple
   variables are bound to lambdas that are never mutated. Complex variables
   may be assigned. What remains is truly recursive and becomes `fix`. See
   "Fixing Letrec: A Faithful Yet Efficient Implementation of Scheme's
   Recursive Binding Construct" (Waddell, Sarkar, Dybvig) and "Fixing Letrec
   (reloaded)" (Ghuloum, Dybvig).
2. `expand_well_known_procs` (`expander/well_known_procs.rs`) inlines
   lexically bound procedures that are used once as operators. It is off by
   default (`ENABLE_WELL_KNOWN_EXPANSION`) because the CPS optimizer does the
   same.
3. `assignment_elimination` (`expander/assignment_elimination.rs`) boxes every
   mutable variable. Reads become `box-ref` and writes become `box-set!`.
4. `compile_cps::cps_toplevel` (`expander/compile_cps.rs`) lowers TreeIL to
   graph CPS.

During bootstrap there is no self-hosted compiler yet. The Rust side then
also runs `resolve_primitives`, `expand_primitives`, `resolve_free_vars`, and
`letrectify` first. The core expander behind the `bootstrap` feature
(`expander/core.rs`) supports only the core forms: `define`, `lambda`,
`case-lambda`, `if`, `quote`, `set!`, `let`, `let*`, `letrec`, `letrec*`,
`begin`, `cond`, `case`, `and`, `or`, `do`, `when`, `unless`, `values`,
`receive`, `with-continuation-mark`, `define-struct`, and the `@` and `@@`
module references.

## Graph CPS

Graph CPS (`compiler/cps/graph.rs`) shares nodes by reference instead of
nesting them. That keeps transformations local.

| Term | Meaning |
|------|---------|
| `let-val` | bind the result of an expression |
| `fix` | mutually recursive functions |
| `letk` | bind continuations |
| `if` | branch |
| `continue` | jump to a continuation |
| `app` | call with an explicit return continuation |
| `raise` | raise |

Expressions are literals and primitive calls. A function records its
parameters, an optional variadic binder, an optional return continuation
(procedures have one, continuations do not), and its free variables. See
"Compiling with Continuations, Continued" by Matt Might.

The optimizer (`cps/optimize.rs`) rewrites the graph in place with a
worklist. A gas budget bounds it (`DEFAULT_GAS` is 42000). It does:

- dead binding removal
- eta reduction
- constant and identical-branch simplification
- known-primitive propagation
- singleton inlining
- limited recursive unrolling
- contification
- reification analysis

Contification turns a procedure that is only called in tail position with a
known continuation into a continuation. It then becomes a basic block and
needs no heap closure. There are two implementations, `cps/scc_contify.rs`
and `cps/dom_contify.rs`. `CAPY_GCPS_CONTIFY` picks `off`, `scc`, `dom` (the
default), or `dom-then-scc`.

After optimization, `cps/reify.rs` decides which functions become native
functions and which continuations become closures. `cfg/lower.rs` then
lowers the result to CFG.

## Closure sharing

Closures made at one site can share a heap `EnvRecord`
(`runtime/value/env.rs`) instead of each copying the common free variables.
`cps/share.rs` decides per site, using escape information from
`cps/flow.rs`. `CAPY_CLOSURE_SHARING=0` turns it off and `CAPY_SHARE_DUMP=1`
prints the decisions.

Take five closures at one site that capture the same two variables, `x` and
`y`:

```text
 without sharing                 with sharing
 closure 1 [x y]                 EnvRecord [x y]
 closure 2 [x y]                    ^  ^  ^  ^  ^
 closure 3 [x y]                 closure 1..5, one pointer each (slot 0)
 closure 4 [x y]                 plus their own private captures
 closure 5 [x y]
 10 capture slots                1 record, 5 pointers
```

## CFG

The CFG IR (`compiler/cfg/ir.rs`) is a control-flow graph of basic blocks
over mutable variables. An explicit `assign` copies a value into the mutable
home of a variable. The IR is deliberately not SSA.

A `Program` is an entry `CodeId` plus a list of procedures. Each procedure is
a function or a continuation. It has parameters, an optional rest binder, a
list of free variables, and blocks of instructions that end in a terminator.

| Instructions | |
|---|---|
| `assign`, `const` | values |
| `make-closure`, `closure-ref`, `closure-set` | closures |
| `cache-ref`, `cache-set` | global variable cells |
| `prim-call` | primitive calls |
| `rest-to-list`, `rest-ref`, `rest-length`, `rest-predicate` | rest arguments |

| Terminators | |
|---|---|
| `call`, `tail-call` | calls (`call` carries a return continuation, `retk`) |
| `raise` | exceptions |
| `jump`, `branch` | control |
| `branch-prim` | a test fused with its branch |
| `switch` | multiway jump |

A global variable reference goes through a cache cell. `cache-ref` checks a
cell owned by the call site and falls back to a module lookup on a miss.
Repeated accesses cost one load.

Before bytecode generation, `cfg::finish_procedure` (`compiler/cfg/mod.rs`)
runs these passes on each procedure:

1. rest-argument lowering (`cfg/rest.rs`)
2. dead effect-free instruction elimination (`cfg/effects.rs`)
3. switch inference (`cfg/switch.rs`)
4. cache-operation lowering (`cfg/cache.rs`)

# Backend

`compiler/bytecode/lower.rs` turns each CFG procedure into a bytecode code
block:

- Registers come from a linear scan, one interval per CFG variable.
- A continuation that was not reified becomes a jump inside the procedure.
  Reified continuations and functions become separate code blocks.
- A non-tail call gets a return point whose mask lists the registers live
  across the call.
- Hot primitives become dedicated opcodes. The rest become
  `call_primitive*` with the number of the primitive. Adjacent pairs of
  instructions fuse into superinstructions.
- A conditional branch fuses its test into the jump (`jump_if_less`,
  `jump_if_pair`, and so on). A `switch` becomes `switch_fixnum`,
  `switch_char`, or `switch_symbol`.

The CFG `Call` terminator carries a return continuation (`retk`). The
backend turns it into the `return_point` that follows the call.

# Linking and FASL

The compiler emits no platform object files. `compile_lowered_to_fasl_bytes`
in `compiler/object.rs` takes a lowered CFG program, lowers every procedure
to bytecode, and writes one FASL image. It can gzip the image.

The format is defined in `runtime/fasl/mod.rs`:

- Header: magic `"CAPYFSL "` (8 bytes), `FASL_VERSION`, bytecode format
  hash, compression flag.
- Body: tagged items (immediates, strings, symbols, bignums, vectors,
  program records with code blocks) and graph markers for shared or cyclic
  structure.

At load time the `Reader` (`fasl/reader.rs`) builds the objects and a
`CodeBlock` for each code spec. It runs the verifier (`bytecode/verify.rs`)
on every code block before anything executes. A version mismatch, a format
hash mismatch, or a failed verification rejects the file.
