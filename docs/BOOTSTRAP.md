# Bootstrapping CapyScheme

CapyScheme bootstraps by building a Rust `capy` executable, creating a
`capyc` wrapper around the same executable, and then using that compiler entry
point to compile the Scheme libraries into `.fasl` files.

The current build is driven by `Makefile`.

## Quickstart

Build the runtime and bootstrap stages:

```sh
make build
```

This runs, in order:

1. `make stage-0`
2. `make build-runtime`
3. `make stage-1`
4. `make stage-2`

The result is:

- `bin/capy` and `bin/capyc`
- `stage-0/capy` and `stage-0/capyc`
- `stage-1/capy`, `stage-1/capyc`, and `stage-1/compiled/`
- `stage-2/capy`, `stage-2/capyc`, and `stage-2/compiled/`

`stage-2/compiled/` is the final bootstrapped library set used by the install
and packaging targets.

## Prerequisites

You need:

- Rust, using the toolchain selected by `rust-toolchain.toml`.
- `make` and Bash, which the Makefile selects as its recipe shell.
- `rsync` for install targets.
- Optional: `cross`, when building for a `TARGET` different from the host.
- Optional: `dpkg-deb`, `fakeroot`, or `rpmbuild` for package targets.

## Main Makefile Variables

- `PROFILE` defaults to `release`.
  It is passed to Cargo as `--profile $(PROFILE)`.
- `TARGET` defaults to `rustc --print host-tuple`.
  If it differs from the host target, the Makefile uses `cross`; otherwise it
  uses `cargo`.
- `PREFIX` defaults to `$(HOME)/.local/share`.
  Install targets write below this path unless overridden.
- `VERSION` is discovered with `cargo info capy`, falling back to
  `capy/Cargo.toml`.
- `PORTABLE` defaults to `1`.
  It is still exposed as a Makefile knob, but the current runtime targets encode
  portable behavior through Cargo features.
- `COMPILE_PSYNTAX` defaults to `0`.
  Set it to `1` when changes to `lib/boot/psyntax.scm` require regenerating
  `lib/boot/psyntax-exp.scm`.
- `MMTK_PLAN` defaults to `StickyImmix`.
- `CAPY_LOAD_PATH` defaults to `./lib`.
- `CAPY` defaults to `stage-0/capy` for `make test`.

Rust artifacts are built under:

```text
target/$(TARGET)/$(PROFILE)/
```

The Makefile refers to that path as `$(TARGET_PATH)`.

## Runtime Targets

### `make build-runtime-bootstrap`

Builds `capy` with:

```sh
cargo build --profile $(PROFILE) --target $(TARGET) -p capy --bin capy --features portable,bootstrap
```

Then it copies the executable to `bin/capy` and writes `bin/capyc` as a shell
wrapper:

```sh
exec "$(dirname "$0")/capy" --capy-compiler-entrypoint "$@"
```

### `make build-runtime`

Builds the normal portable runtime:

```sh
cargo build --profile $(PROFILE) --target $(TARGET) -p capy --bin capy --features portable --no-default-features
```

It also writes `bin/capy` and `bin/capyc`.

### `make build-runtime-portable`

Builds the same portable runtime shape as `build-runtime`, but installs it as:

- `bin/capy-full`
- `bin/capyc-full`

Portable install and tarball targets copy `bin/capy-full` as the installed
`capy` executable and generate a matching `capyc` wrapper.

### `make build-runtime-fhs`

Builds with:

```sh
CAPY_SYSROOT=$(PREFIX) cargo build --no-default-features --profile $(PROFILE) --target $(TARGET) -p capy --bin capy
```

It writes `bin/capy-full` and `bin/capyc-full`. The FHS install and package
targets use this form so the executable looks for its system root under
`$(PREFIX)`.

## Bootstrap Stages

### Stage 0: `make stage-0`

`stage-0` depends on `build-runtime-bootstrap`.

It creates:

- `stage-0/capy`, copied from `$(TARGET_PATH)/capy`
- `stage-0/capyc`, a wrapper that invokes `stage-0/capy` with
  `--capy-compiler-entrypoint`
- `stage-0/cache`, populated by warm-up runs

The warm-up commands run with:

- `RUST_MIN_STACK=134217728`
- `MMTK_PLAN=StickyImmix`
- `CAPY_GC_MAX_HEAP=8G`
- `XDG_CACHE_HOME=stage-0/cache`
- `CAPY_LOAD_PATH=./lib`

They evaluate:

- `42`
- `(import (rnrs))`
- `(import (scheme base))`
- `(import (srfi 1))`
- `(import (srfi 13))`

If `COMPILE_PSYNTAX=1`, stage 0 also runs
`lib/boot/compile-psyntax.scm` and then checks an import of both
`(scheme base)` and `(rnrs)`.

### Stage 1: `make stage-1`

`stage-1` compiles all Scheme libraries with:

```text
COMPILER=stage-0/capyc
OUT=stage-1/compiled
```

Then it copies:

- `stage-0/capy` to `stage-1/capy`
- `stage-0/capyc` to `stage-1/capyc`

### Stage 2: `make stage-2`

`stage-2` compiles the same Scheme library set with:

```text
COMPILER=stage-1/capyc
OUT=stage-2/compiled
```

Then it copies:

- `stage-1/capy` to `stage-2/capy`
- `stage-1/capyc` to `stage-2/capyc`

## Compiled Libraries

Compiled Scheme output uses the `fasl` extension:

```text
COMPILED_SCM_EXT := fasl
```

`make compile-all` expands to:

- `compile-boot`
- `compile-core`
- `compile-rnrs`
- `compile-srfi`
- `compile-r7rs`
- `compile-capy-args`
- `compile-cli`
- `compile-capy`
- `compile-common`

The stage targets pass `COMPILER` and `OUT` into these compile targets. Most
files are compiled with:

```sh
$(CAPY_ENV) $(COMPILER) --nobacktrace -o <output.fasl> -m "capy user" <source.scm>
```

Boot and CLI files use `-m "capy" -L lib`.

The current source groups include:

- boot libraries under `lib/boot/`
- core libraries under `lib/core/` plus `lib/core.scm`
- R6RS/RNRS libraries under `lib/rnrs/` plus `lib/rnrs.scm`
- SRFI libraries under `lib/srfi/`
- R7RS libraries under `lib/scheme/`
- Capy libraries under `lib/capy/`
- common libraries under `lib/common/`
- CLI entry libraries `lib/boot/cli.scm` and `lib/boot.scm`

## Tests

Run:

```sh
make test
```

By default this uses `CAPY=stage-0/capy`. If that executable does not exist,
the target builds `stage-0` first. It runs every `*.scm` file under
`tests/lib/` with:

```sh
$(CAPY_ENV) $(CAPY) -L lib --fresh-auto-compile -s <test>
```

Use another executable with:

```sh
make test CAPY=/path/to/capy
```

## Installing

### Portable Install

Run:

```sh
make install-portable
```

This depends on `build` and `build-runtime-portable`, then installs into:

```text
$(PREFIX)/capy/$(VERSION)/
```

It writes:

- `capy`, copied from `bin/capy-full`
- `capyc`, generated as a wrapper around installed `capy`
- `capy-$(VERSION)`, a symlink to `capy`
- `lib/`, copied from the source tree
- `compiled/`, copied from `stage-2/compiled`
- `extensions/`

Add `$(PREFIX)/capy/$(VERSION)` to `PATH` to use that installation.

### FHS Install

Run:

```sh
make install PREFIX=/usr/local
```

This depends on `build`, then runs `build-runtime-fhs` and installs:

- `$(PREFIX)/bin/capy`
- `$(PREFIX)/bin/capyc`
- `$(PREFIX)/lib/capy/compiled/`
- `$(PREFIX)/share/capy/lib/`

## Distribution Targets

### Portable Tarball

Run:

```sh
make dist-portable
```

This depends on `build` and `build-runtime-portable`. It stages the same layout
as `install-portable`, adds `LICENSE` and `CHANGELOG.md`, and writes:

```text
dist/capyscheme-$(VERSION)-$(TARGET).tar.gz
```

Environment variables accepted by the recipe:

- `OUTDIR`, defaulting to `dist`
- `STAGEDIR`, defaulting to `stage-dist`
- `OUTNAME`, defaulting to `capyscheme-$(VERSION)-$(TARGET).tar.gz`

### Musl Portable Tarball

The Forgejo release workflow also builds a Linux musl portable tarball for
systems where a self-contained runtime is preferable to a glibc-linked archive.
The artifact uses the same staged layout as `dist-portable`; only `TARGET`
changes the Rust target triple and archive name:

```sh
# Debian/Ubuntu packages used by the release workflow:
sudo apt install -y musl-tools linux-libc-dev

rustup target add x86_64-unknown-linux-musl
CFLAGS="-idirafter /usr/include -idirafter /usr/include/x86_64-linux-gnu" \
  make -j4 VERSION="$VERSION" \
  TARGET=x86_64-unknown-linux-musl \
  CARGO_BIN=cargo \
  dist-portable
```

The default archive name is:

```text
dist/capyscheme-$(VERSION)-x86_64-unknown-linux-musl.tar.gz
```

The `CFLAGS` include path workaround mirrors `.forgejo/workflows/release.yml`.
Keep it with the musl build unless the workflow and local toolchain no longer
need access to the host Linux headers.

### Debian Package

Run:

```sh
make dist-deb
```

This depends on `build`, builds an FHS runtime with `PREFIX=/usr`, stages files
under `stage-pkg/deb`, and writes a `.deb` into `dist/`.

Useful knobs:

- `DIST_DIR`
- `PKG_NAME`
- `PKG_ROOT`
- `DEB_MAINTAINER`
- `DEB_SECTION`
- `DEB_PRIORITY`

### RPM Package

Run:

```sh
make dist-rpm
```

This depends on `build`, builds an FHS runtime with `PREFIX=/usr`, stages files
under `stage-pkg/rpm`, writes a spec file, runs `rpmbuild`, and copies resulting
RPMs into `dist/`.

Useful knobs:

- `DIST_DIR`
- `PKG_NAME`
- `PKG_ROOT`
- `RPM_LICENSE`
- `RPM_SUMMARY`

## Troubleshooting

### `cross: command not found`

The Makefile uses `cross` when `TARGET` differs from the host target. Install it
with:

```sh
make install-cross
```

If you intended a native build, leave `TARGET` unset.

### Tests say `CAPY executable not found`

Build stage 0 first:

```sh
make stage-0
```

Or point the test target at an existing executable:

```sh
make test CAPY=/path/to/capy
```

### Compiled libraries are missing

Run the full bootstrap:

```sh
make build
```

The install and package targets expect `stage-2/compiled/` to exist and contain
the bootstrapped `.fasl` library set.
