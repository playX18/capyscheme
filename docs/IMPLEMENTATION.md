# Implementation Overview

CapyScheme implements R6RS and R7RS. It has three parts:

- **Scheme, `pkgs/`**: the reader, the expander (psyntax), the module system, and the whole
  compiler: Tree IL passes, graph CPS, CFG, register allocation, bytecode encoding, and the
  program linker (`pkgs/core/src/core/compiler/`).
- **Rust, `crates/`**: the interpreters and JIT that run bytecode, the runtime, and the garbage
  collector. There is also a seed compiler built into runtime with `bootstrap` feature enabled, and it 
  is used only once: to build self-hosted compiler.
- **C API, `c/capy.h`**: implemented by `libcapy` (built from `crates/capy/`), for embedding and
  native extensions.

# Compilation pipeline

```text
source -> reader -> psyntax -> Tree IL -> GCPS -> CFG -> bytecode -> FASL
```

1. **Front end.** `boot/reader.scm` reads annotated S-expressions. psyntax expands them to Tree IL
   (`compile-tree-il`, `boot/eval.scm`).
2. **Tree IL** (`core/compiler/tree-il/`): `resolve-primitives`, `expand-primitives`,
   `resolve-free-vars`, `letrectify`, then `fix-letrec`, `well-known-procs` and
   `assignment-elimination` (every mutable variable is boxed).
3. **Graph CPS** (`core/compiler/cps/`): `convert` builds it, `optimize` rewrites it in place with
   a gas-bounded worklist (inlining, contification, dead code, folding), and `reify` decides which
   functions and continuations become code blocks and closures. Nodes are shared by reference, so
   rewrites stay local.
4. **CFG** (`core/compiler/cfg/`): basic blocks over mutable variables, not SSA. Passes: rest
   arguments, dead-effect elimination, switch inference, global-cache lowering.
5. **Bytecode** (`core/compiler/bytecode/`): linear-scan registers, jumps for continuations that
   were not reified, return points with liveness masks, fused test-and-branch, hot primitives as
   dedicated opcodes (`call_primitive*` for the rest), then the encoder. See
   [BYTECODE.md](BYTECODE.md).
6. **Linking** (`core/compiler/object.scm`): one FASL image per file. `pipeline.scm` ties 3 to 6
   together.

# FASL and the compile cache

FASL is Capy's own binary format: one compressed image with the bytecode, constant pools, liveness
masks, and source maps of every code block. `compile-file` (`boot/eval.scm`) writes it, and the
reader in `crates/capy/src/runtime/fasl/` loads it. The header has the magic (its last byte is the
layout version), the SHA-256 trailer, and `bytecode::def::format_hash()`. The loader verifies every
code block (`bytecode/verify.rs`) and rejects a version mismatch, a hash mismatch, or a failed
verification.

Without an output path the result goes to `$XDG_CACHE_HOME/capy/cache/<version>/`. 

# Build system

`./bootstrap.sh` builds everything. [BOOTSTRAP.md](BOOTSTRAP.md) is the reference; in short:

| Stage | What it is |
|-------|------------|
| stage-0 | `cargo build` with `--features portable,bootstrap`. It runs the Scheme sources directly. |
| stage-1 | stage-0 runs the driver (`pkgs/capy-pkg`) and compiles every package: `capy build --toolchain stage-1 --packages pkgs`. |
| stage-2 | The same build with stage-1's driver and runtime. |
| stage-2/fhs | The runtime that is installed onto `CAPY_SYSROOT`. |

The script also checks package layering, checks that `psyntax-exp.scm` is the expansion of
`psyntax.scm`, and requires stage-1 and stage-2 to be byte-identical. 

Options: `--clean`, `--skip-stage-0`, `--no-fhs`, `--profile`, `--target`,
`--sysroot`, `-j`.

`psyntax-exp.scm` lets the system load the expander before the expander can expand itself. After
editing `psyntax.scm`, regenerate it:

```sh
stage-1/capy-rt -s pkgs/core/tools/compile-psyntax.scm \
    pkgs/core/src/boot/psyntax.scm pkgs/core/src/boot/psyntax-exp.scm
```

Install and package with the driver:

```sh
stage-2/capy install --toolchain stage-2 --prefix $HOME/.local
stage-2/capy dist --toolchain stage-2 --format portable    # or deb, rpm
```

## Changing the instruction set or FASL format

1. Change the last byte of `FASL_MAGIC` (`runtime/fasl/mod.rs`).
2. Regenerate `core/compiler/bytecode/isa.scm` with `tools/gen-isa.scm` if the opcodes changed.
3. Remove `~/.cache/capy` and run `./bootstrap.sh --clean`.

# Tests

Tests live in `pkgs/<pkg>/tests/` as SRFI 64 `*.scm` scripts and `*.sps`
R6RS programs.

```sh
(cd pkgs && ../stage-2/capy test --workspace)
(cd pkgs/std && ../../stage-2/capy test srfi)     # files whose path contains "srfi"
```

Each file runs in its own process, under a timeout. See [PACKAGES.md](PACKAGES.md) for the flags.

# Adding a library

Put it under `pkgs/<pkg>/src/`. The driver finds it from the `library` header, and it must match
`provides` in `package.scm`. .

# Adding a primitive

A primitive lives in three places in the codebase and they all must be carefully maintained:

1. **Implementation (Rust).** Write the function in `runtime/vm/thunks/` and expose it to Scheme
   with `#[scheme]`. If bytecode can call it by id, also:
   - append a variant to `Primitive` and to `ALL`, and add its name to `name` and `from_name`
     (`runtime/prims/def.rs`). Append only: the enum order is part of the format;
   - add a case to `call_prim` and an entry to `PRIM_WRAPPERS` and `PRIM_ARITY`
     (`runtime/prims/calls.rs`);
   - declare its effects in `primitive_effects` (`runtime/prims/effects.rs`), and
     `is_branch_fusable` if it is a predicate.
2. **Compiler tables (Scheme).** `(%primitive-table)` (`vm/load/boundary.rs`) reports the Rust
   data, but some tables are copies you update by hand:
   - `core/compiler/bytecode/prims.scm`: the `prim-names` vector in enum order, and
     `fused-jump-kinds`;
   - `core/compiler/cps/primitives.scm`: the fallback branch-fusable list.
3. **Name tables for inlining.** If the expander should resolve or inline it, add it to
   `interesting-primitive-names`, `resolve-primitives` and `expand-primitives` in
   `core/compiler/tree-il/primitives.scm`, and to the Rust copy used by stage-0
   (`expander/prim_names.rs` and `expander/primitives.rs`). The two must match.

Then change the FASL layout byte and rebuild with `./bootstrap.sh --clean`.

A new dedicated opcode (so engines can inline it) also needs:

1. The instruction in `bytecode/def.rs`, then `tools/gen-isa.scm` to regenerate `isa.scm`.
2. Selection in `emit-prim` (`core/compiler/bytecode/lower.scm`) and in `dedicated_opcodes`
   (`boundary.rs`), which must agree.
3. A handler in the Rust interpreter (`runtime/interp/rust_interp.rs`), a template in
   `runtime/codegen/template/emit/primitives.rs`, and JIT support (`runtime/codegen/jit/`).
