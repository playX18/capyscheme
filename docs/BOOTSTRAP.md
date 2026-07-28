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

## Stages

- **stage-0** builds bootstrap binaries and warms the compile cache.
- **stage-1** compiles all libraries with the stage-0 compiler.
- **stage-2** recompiles them with the stage-1 compiler for all write-barrier
  variants, then produces the installable result.

To rebuild after a compiler or code-generation change:

```sh
make stage-0
```

For a clean rebuild after ABI changes:

```sh
rm -rf stage-0 stage-1 stage-2
make stage-0
```

If old compiled files still cause trouble, also remove `~/.cache/capy`.

