# Bootstrapping

The CapyScheme compiler is written in Scheme. `bootstrap.sh` builds a runtime that runs the
Scheme sources in `pkgs/` directly, then uses it to compile them.

## Requirements

- Rust nightly
- Bash
- `cross`, for non-host targets
- `dpkg-deb` (and `fakeroot`, if present) for `capy dist --format deb`, `rpmbuild` for `--format rpm`

## Commands

```sh
./bootstrap.sh                                                  # stage-0, stage-1, stage-2, stage-2/fhs
(cd pkgs && ../stage-2/capy test --workspace)                   # run pkgs/*/tests
stage-2/capy install --toolchain stage-2 --prefix $HOME/.local
stage-2/capy dist --toolchain stage-2 --format portable         # or deb, rpm
```

`./bootstrap.sh --help` lists the options:

- `--clean` removes the stage directories first.
- `--skip-stage-0` reuses the existing stage-0 runtime.
- `--no-fhs` skips the installed runtime.
- `--profile`, `--target`, `--sysroot`.
- `-j N` sets the number of parallel compile jobs (default: CPU count).


## Stages

stage-0 is the runtime cargo builds with the `bootstrap` feature. It has no compiled libraries and
runs package sources from `stage-0/<pkg>/src`, which link to `pkgs/<pkg>/src`. The Rust expander
expands the boot files (library loader and expander), because psyntax is not loaded yet. The seed
compiler in `crates/capy/src/compiler/direct/` compiles everything in stage-0. It takes Tree IL from
the Rust expander, or from psyntax via `%compile-seed` for library code, runs the Rust Tree IL
passes in `expander/{primitives,recursive_bindings,well_known_procs}.rs`, and emits bytecode. Its
only job is to compile the Scheme compiler once.

stage-1 runs the driver (`pkgs/capy-pkg`) from source on the stage-0 runtime with
`capy build --toolchain stage-1 --packages pkgs`. It compiles core, std, the extras and the driver
into `stage-1/<pkg>/fasl`, writes `stage-1/capy.libmap` and links `stage-1/capy`. The seed compiler
compiles `(core compiler)` first, and `(core compiler)` then compiles every other file
(`boot/compiler.scm`).

stage-2 repeats the build with stage-1's driver and runtime, so the Scheme compiler compiles itself.

Between stage-1 and stage-2, `bootstrap.sh` regenerates `pkgs/core/src/boot/psyntax-exp.scm` from
`psyntax.scm` and fails if the result differs from the committed file.

## Install layout

```
P/bin/capy-rt                the runtime
P/bin/capyc                  link to capy-rt (capy-rt --compile)
P/bin/capy                   the driver, static; finds capy-rt next to itself
P/lib/libcapy.{so,dylib}     executables have an rpath to ../lib
P/lib/capy/<ver>/capy.libmap
P/lib/capy/<ver>/<pkg>/{src,fasl,out,native,package.scm}   core, std, common, closer-mop, r7rs-drafts, capy-pkg
P/lib/capy/<ver>/std/native/cparse.*                        C header parser, loaded by c-include on a cache miss
P/include/capy.h
```

## Environment variables

| Variable | Meaning |
|---|---|
| `CAPY_LIBMAP` | Libmap to use instead of the toolchain's. Must exist. Same as `--libmap`. |
| `CAPY_RT` | Runtime the driver starts. Default: `capy-rt` next to the driver. |
| `CAPY_CPARSE_LIB` | C header parser library. Default: `lib/capy/` of the install, or next to the stage. |
| `XDG_CACHE_HOME` | Auto-compile cache and C binding cache. Each stage uses its own. |
| `CAPY_SYSROOT` | Build time: fallback prefix of the installed runtime. |
| `CAPY_LIB_DIR` | Build time: where cargo finds libcapy when linking `capy-rt`. |
| `JOBS` | Compile jobs in `bootstrap.sh`. |
