# Bootstrapping

CapyScheme builds its Rust runtime first, then uses that runtime to compile
the Scheme libraries in `lib/` through three stages.

## Requirements

- Rust nightly (selected by `rust-toolchain.toml`)
- `make`, Bash, and `rsync`
- `cross` when building for a non-host target
- `dpkg-deb` or `rpmbuild` only for the matching package target

## Build and test

```sh
make build
make test
make PREFIX=$HOME/.local install
```

`make build` runs the stages below and leaves the final compiled libraries in
`stage-2/compiled/`.

`make test` runs each test file under `timeout` (`gtimeout` on macOS). The
per-file limit is `TEST_TIMEOUT` seconds, 300 by default:

```sh
make TEST_TIMEOUT=600 test
```

## Stages

- **stage-0** builds bootstrap binaries and warms the compile cache.
- **stage-1** compiles all libraries with the stage-0 compiler.
- **stage-2** recompiles them with the stage-1 compiler into one
  `stage-2/compiled/`, which is the installable result. Compiled files do not
  depend on the GC plan or write barrier, so there is a single output.

Each compiled directory has a `.fasl-version` file. When the `FASL_VERSION`
constant in `crates/capy/src/runtime/fasl/mod.rs` changes, `make` deletes the
stale `stage-1/compiled` or `stage-2/compiled` and rebuilds it. Without this,
the mtime-based rules would keep old files, and an installed runtime has no
compiler to regenerate them.

To rebuild after a compiler or bytecode change:

```sh
make stage-0
```

For a clean rebuild after instruction set changes:

```sh
rm -rf stage-0 stage-1 stage-2
make stage-0
```

The FASL loader rejects files from a different format version or instruction
set with an error. If old compiled files still cause trouble, also remove `~/.cache/capy`.

