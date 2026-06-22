
# Bootstrapping CapyScheme (stages 0 → 2)

CapyScheme can **self-host** itself by building a minimal runtime and then *bootstrapping* the Scheme libraries and compiler through a few stages.

The recipes are provided by `Makefile` (ported from the original `Justfile`). This file explains what each stage does, which targets to run, and how to tweak the build.

## Top-level overview

CapyScheme is built in two layers:

1. A **Rust executable** (`capy`) plus a shell **compiler wrapper** (`capyc`).
2. A set of **Scheme libraries** that are compiled into FASL code images (`.fasl`) using `capyc`.

Bootstrapping means:

- build a first runnable compiler (`capyc`) using the Rust runtime,
- use it to compile libraries,
- then use the newly built compiler to rebuild those libraries again (stage-2).

At the end you get `stage-2/` as the “final” self-hosted result.

## Prerequisites

You’ll need:

- Rust toolchain (this repo includes `rust-toolchain.toml`).
- `rsync` for install and distribution targets.
- Optional: `cross` if you want to build for a non-host target.

## Quickstart

Build everything (runtime + stage-0/1/2):

```sh
make build
```

Afterwards, you’ll have:

- `stage-0/capy`, `stage-0/capyc`
- `stage-1/capy`, `stage-1/capyc`, plus compiled libraries in `stage-1/compiled/`
- `stage-2/capy`, `stage-2/capyc`, plus compiled libraries in `stage-2/compiled/`

## Makefile knobs (variables)

Most targets accept these variables:

- `PROFILE` (default: `release`)
	- Rust profile used for builds.
	- Typical values: `release` or `debug`.
- `TARGET` (default: `rustc --print host-tuple`)
	- Rust target triple.
	- If `TARGET != host`, the Makefile will try to use `cross`.
- `PREFIX` (default: `$HOME/.local/share`)
	- Installation prefix used by `install-portable` and `install`.
- `VERSION`
	- Auto-detected from `capy/Cargo.toml` (you can override it).
- `PORTABLE` (default: `1`)
	- Retained in the Makefile defaults/help output. The actual runtime layout is selected by `build-runtime-portable` or `build-runtime-fhs`.
- `COMPILED_SCM_EXT` (currently `fasl`)
	- Extension used by the per-library compilation rules. The loader treats AOT Scheme artifacts as FASL code images.
- `COMPILE_PSYNTAX` (default: `0`)
   - When set to `1`, stage-0 will additionally compile psyntax into psyntax-exp.
   Set this to 1 when you modify macro-expander code (psyntax.scm).
  

## Artifacts and directories

### Target directory

Rust builds under a target-specific directory:

- `target/<TARGET>/<PROFILE>/`

This directory is referenced as:

- `TARGET_PATH := target/<TARGET>/<PROFILE>`

It contains the compiled `capy` binary and any other Rust build products.

### Stage directories

Each stage has two kinds of artifacts:

- `stage-N/capy` and `stage-N/capyc`: the Rust executable and a shell wrapper that starts compiler mode.
- `stage-N/compiled/`: compiled Scheme libraries as `.fasl` files.

The compiler (`capyc`) compiles Scheme sources from `lib/` into `.fasl` files under `stage-N/compiled/`. Examples include:

- `stage-N/compiled/boot/base.fasl`
- `stage-N/compiled/core/lists.fasl`
- `stage-N/compiled/rnrs/base.fasl`

At runtime, `-C DIR` / `--compiled-load-path DIR` prepends `DIR` to `%load-compiled-path`. The loader searches compiled paths for `*.fasl` artifacts, checking an architecture-scoped subdirectory first (for example `DIR/x86_64/...`) and then the unscoped path when no explicit architecture is requested.

## The bootstrap stages

### Stage 0: build a runnable system

Target: `make stage-0`

What it does:

1. Builds the Rust crate `capy` with `--features portable,bootstrap`.
2. Copies the Rust binary to `stage-0/capy` and writes the `stage-0/capyc` compiler wrapper.
3. Warms up the auto-compile cache:
	 - runs `stage-0/capy -L lib --fresh-auto-compile -c 42`
	 - imports `(rnrs)`, `(scheme base)`, `(srfi 1)`, and `(srfi 13)`
	 - uses `XDG_CACHE_HOME=stage-0/cache`.

Optional psyntax compile (when `COMPILE_PSYNTAX=1`):

- runs `lib/boot/compile-psyntax.scm` to compile `psyntax.scm` into `psyntax-exp.scm`.

Environment used in stage-0 runs:

- `CAPY_LOAD_PATH=./lib`
- `XDG_CACHE_HOME=stage-0/cache`

For local experiments, set `CAPY_LOAD_COMPILED_PATH` or pass `-C DIR` when you want to prepend an existing compiled library directory.

### Stage 1: compile libraries using stage-0 compiler

Target: `make stage-1`

What it does:

1. Uses `stage-0/capyc` to compile every Scheme library under `lib/`:
	 - boot libraries (`lib/boot/*.scm`)
	 - core (`lib/core/*.scm` and `lib/core.scm`)
	 - rnrs (`lib/rnrs/*.scm` and `lib/rnrs.scm`)
	 - srfi (`lib/srfi/*.scm`)
	 - r7rs (`lib/scheme/*.scm`)
	 - CLI (`lib/boot/cli.scm` and `lib/boot.scm`)
	 - capy libraries (`lib/capy/*.sls` and some `.scm`)
	 - common libraries (`lib/common/*.scm`)
2. Copies the stage-0 executable and compiler wrapper:
	 - `stage-1/capy` and `stage-1/capyc`

### Stage 2: rebuild libraries using stage-1 compiler

Target: `make stage-2`

What it does:

1. Uses `stage-1/capyc` to compile libraries again into `stage-2/compiled/`.
2. Copies the stage-1 executable and compiler wrapper.

Stage-2 is the “final” self-hosted output used by the install/dist targets.

## Runtime-only builds

If you only want the Rust runtime (no Scheme bootstrap stages):

- `make build-runtime`
	- Builds `capy` crate with `--features portable --no-default-features`.

There are also convenience targets that build the “full” launchers:

- `make build-runtime-fhs`
	- Builds `bin/capy-full` with `CAPY_SYSROOT=$(PREFIX)` so runtime paths resolve through the install prefix.
- `make build-runtime-portable`
	- Builds `bin/capy-full` with portable runtime path discovery.

## Installing and distributing

### Portable install (self-contained in one directory)

Target: `make install-portable`

Installs under:

- `$(PREFIX)/capy/$(VERSION)/`

It copies:

- `lib/` (Scheme sources)
- `stage-2/compiled/` (compiled libraries)
- `bin/capy-full` as `capy`
- a generated `capyc` wrapper that starts `capy` with `--capy-compiler-entrypoint`

It also creates a versioned symlink:

- `capy-$(VERSION)` → `capy`

After installing, add the install directory to your `PATH`.

### Portable tarball (no install)

Target: `make dist-portable`

Creates a `.tar.gz` containing a portable layout like the one from `install-portable`.

You can customize output locations:

- `OUTDIR` (default: `dist`)
- `STAGEDIR` (default: `stage-dist`)
- `OUTNAME` (default: auto-generated)

### FHS-ish install (bin/lib/share)

Target: `make install`

Installs into an FHS-ish layout under `$(PREFIX)`:

- `$(PREFIX)/bin/capy` and `$(PREFIX)/bin/capyc`
- `$(PREFIX)/lib/capy/compiled/` (compiled libraries)
- `$(PREFIX)/share/capy/lib/` (Scheme sources)

This target may use `sudo` if you’re not root.

## Notes on how compilation works

The Makefile compiles each Scheme file into a FASL code image:

- output extension: `.fasl`
- selected in `Makefile` by `COMPILED_SCM_EXT := fasl`

Compilation is done by invoking the compiler launcher (`capyc`) with a module set (`-m`) and library search path (`-L`).

Examples from the rules:

- boot libraries are compiled with `-m "capy" -L lib`
- most others are compiled with `-m "capy user"`

Scheme library artifacts are no longer produced or loaded as shared objects; the loader reports an error for shared-object Scheme artifacts and expects FASL code images in AOT mode. See `docs/FASL.md` for the artifact format and load path details.

## Troubleshooting

### `capyc` starts the wrong executable

`capyc` is a shell wrapper that executes the sibling `capy` binary with `--capy-compiler-entrypoint`. If a custom install moves one without the other, regenerate the wrapper or keep both files in the same directory.

### `cross: command not found`

If you set `TARGET` to something other than your host triple, the Makefile will choose `cross`.

Install it via:

```sh
make install-cross
```

Or build for your native target (don’t override `TARGET`).

### Stage cache issues

Stage-0 uses:

- `XDG_CACHE_HOME=stage-0/cache`

Runtime auto-compilation writes fallback FASL files under a versioned cache path such as:

- `stage-0/cache/capy/cache/<version>/<plan-bucket>/<arch>/...`
- `$HOME/.cache/capy/cache/<version>/<plan-bucket>/<arch>/...` when `XDG_CACHE_HOME` is not set

The plan bucket is derived from the selected MMTk plan: `gen` for `StickyImmix`, `conc` for `ConcurrentImmix`, and `regular` for `Immix` or `MarkSweep`.

Each FASL write is protected by a sibling lock file, for example `library.fasl.lock`, and is written atomically. If a cache artifact is stale or corrupt, remove the cache directory and rebuild or rerun with `--fresh-auto-compile` so sources are recompiled:

```sh
rm -rf stage-0/cache
make stage-0
```

For direct debugging of FASL load failures, pass `--debug` to `capy`. This loads FASL code through debug stacktrace trampolines; `--nobacktrace` on `capyc` is kept only as a deprecated compatibility flag.
