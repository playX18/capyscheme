
# Bootstrapping CapyScheme (stages 0 → 2)

CapyScheme can **self-host** itself by building a minimal runtime and then *bootstrapping* the Scheme libraries and compiler through a few stages.

The recipes are provided by `Makefile` (ported from the original `Justfile`). This files explains what each stage does, which targets to run, and how to tweak the build.

## Top-level overview

CapyScheme is built in two layers:

1. A **Rust runtime** (`libcapy`) plus tiny **C boot binaries** (`capy` and `capyc`).
2. A set of **Scheme libraries** that are compiled into shared objects (e.g. `.so` on Linux) using `capyc`.

Bootstrapping means:

- build a first runnable compiler (`capyc`) using the Rust runtime,
- use it to compile libraries,
- then use the newly built compiler to rebuild those libraries again (stage-2).

At the end you get `stage-2/` as the “final” self-hosted result.

## Prerequisites

You’ll need:

- Rust toolchain (this repo includes `rust-toolchain.toml`).
- A C compiler (`clang` by default, but `CC=gcc` works on many setups).
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
- `CC` (default: `clang`)
	- C compiler used to build the launcher binaries.
- `PREFIX` (default: `$HOME/.local/share`)
	- Installation prefix used by `install-portable` and `install`.
- `VERSION`
	- Auto-detected from `capy/Cargo.toml` (you can override it).
- `PORTABLE` (default: `1`)
	- Controls some rpath behavior for the produced launchers.
- `COMPILE_PSYNTAX` (default: `0`)
   - When set to `1`, stage-0 will additionally compile psyntax into psyntax-exp.
   Set this to 1 when you modify macro-expander code (psyntax.scm).
  

## Artifacts and directories

### Target directory

Rust builds under a target-specific directory:

- `target/<TARGET>/<PROFILE>/`

This directory is referenced as:

- `TARGET_PATH := target/<TARGET>/<PROFILE>`

It contains `libcapy.*` and any other Rust build products.

### Stage directories

Each stage has two kinds of artifacts:

- `stage-N/capy` and `stage-N/capyc`: boot binaries wired to the Rust runtime.
- `stage-N/compiled/`: compiled Scheme libraries as shared objects.

The compiler (`capyc`) compiles Scheme sources from `lib/` into `.so`/`.dylib`/`.dll` under `stage-N/compiled/`.

## The bootstrap stages

### Stage 0: build a runnable system

Target: `make stage-0`

What it does:

1. Builds the Rust crate `capy` with `--features portable,bootstrap`.
2. Builds `stage-0/capy` and `stage-0/capyc` (C launchers linked to `libcapy`).
3. Warms up a cache and triggers auto-compilation once:
	 - runs `stage-0/capy -L lib --fresh-auto-compile -c 42`
	 - with `XDG_CACHE_HOME=stage-0/cache`.

Optional psyntax compile (when `COMPILE_PSYNTAX=1`):

- runs `lib/boot/compile-psyntax.scm` to compile `psyntax.scm` into `psyntax-exp.scm`.

Environment used in stage-0 runs:

- `CAPY_LOAD_PATH=./lib`
- `XDG_CACHE_HOME=stage-0/cache`
- `LD_LIBRARY_PATH=<TARGET_PATH>` (and `DYLD_FALLBACK_LIBRARY_PATH` on macOS)

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
2. Copies boot binaries from stage-0:
	 - `stage-1/capy` and `stage-1/capyc` are copies of stage-0 binaries.

### Stage 2: rebuild libraries using stage-1 compiler

Target: `make stage-2`

What it does:

1. Uses `stage-1/capyc` to compile libraries again into `stage-2/compiled/`.
2. Copies boot binaries from stage-1.

Stage-2 is the “final” self-hosted output used by the install/dist targets.

## Runtime-only builds

If you only want the Rust runtime (no Scheme bootstrap stages):

- `make build-runtime`
	- Builds `capy` crate with `--features portable --no-default-features`.

There are also convenience targets that build the “full” launchers:

- `make build-runtime-fhs`
	- Builds with `CAPY_SYSROOT=$(PREFIX)` and links launchers with an rpath pointing at `${PREFIX}/lib/`.
- `make build-runtime-portable`
	- Same Rust build flags as `build-runtime`, but links launchers with rpath `$ORIGIN` so `libcapy.*` can live next to them.

## Installing and distributing

### Portable install (self-contained in one directory)

Target: `make install-portable`

Installs under:

- `$(PREFIX)/capy/$(VERSION)/`

It copies:

- `lib/` (Scheme sources)
- `stage-2/compiled/` (compiled libraries)
- `libcapy.*` from the Rust target directory
- `stage-2/capy-full` and `stage-2/capyc-full` (launchers)

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
- `$(PREFIX)/lib/libcapy.*`
- `$(PREFIX)/lib/capy/compiled/` (compiled libraries)
- `$(PREFIX)/share/capy/lib/` (Scheme sources)

This target may use `sudo` if you’re not root.

## Notes on how compilation works

The Makefile compiles each Scheme file into a shared library:

- Linux: `.so`
- macOS: `.dylib`
- Windows (mingw): `.dll`

Compilation is done by invoking the compiler launcher (`capyc`) with a module set (`-m`) and library search path (`-L`).

Examples from the rules:

- boot libraries are compiled with `-m "capy" -L lib`
- most others are compiled with `-m "capy user"`

## Troubleshooting

### `error while loading shared libraries: libcapy.so: cannot open shared object file`

This means the launcher can’t find `libcapy`.

- For stage builds, the Makefile sets `LD_LIBRARY_PATH=$(TARGET_PATH)` when running `stage-0/capy`.
- For installed builds:
	- Portable installs should work because rpath is set to `$ORIGIN`.
	- FHS installs rely on rpath pointing at `$(PREFIX)/lib/`.

If you built custom binaries, verify their rpath and dynamic deps.

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

If something is corrupted, you can remove it and rebuild stage-0:

```sh
rm -rf stage-0/cache
make stage-0
```
