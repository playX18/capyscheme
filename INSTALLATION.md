# Installation

## From a portable tarball

Release tarballs contain a `capy/<version>/` directory with:

- `capy`
- `capyc`
- `lib/`
- `compiled/`
- `extensions/`

Extract the archive and add that directory to `PATH`:

```sh
tar -xf capyscheme-<version>-<target>.tar.gz
export PATH="$PWD/capy/<version>:$PATH"
```

Use the `x86_64-unknown-linux-musl` tarball on Linux systems where a more
self-contained portable runtime is preferable to a glibc-linked archive.

## From source

To install and run CapyScheme from source you need:

- `lld` on macOS to link Scheme files after they are built
- `make`
- Rust, using the toolchain selected by `rust-toolchain.toml`
- `rsync`

For a portable install, run:

```sh
make install-portable
```

This installs into `$(PREFIX)/capy/$(VERSION)/`. Add that directory to `PATH`.

For a system-wide FHS install, run:

```sh
make PREFIX=/usr/local install
```

This installs `capy` and `capyc` into `$(PREFIX)/bin`, source libraries under
`$(PREFIX)/share/capy/lib`, and compiled libraries under
`$(PREFIX)/lib/capy/compiled`.

Portable tarballs and `make install-portable` include an `extensions/`
directory. If you install native extensions there, add that directory to
`LD_LIBRARY_PATH` on Linux or `DYLD_FALLBACK_LIBRARY_PATH` on macOS. The FHS
`make install` target does not currently create an extensions directory.

See [Bootstrapping CapyScheme](docs/BOOTSTRAP.md) for full build, test, and
packaging details.
