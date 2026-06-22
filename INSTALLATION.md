# Installation

To install and run CapyScheme you need:

- a Rust toolchain (this repo includes `rust-toolchain.toml`)
- `make`
- `rsync` for install and distribution targets

After they are installed simply run:
```sh
$ make install-portable
```

Or if you want to install system-wide
```sh
$ make PREFIX=/usr/local install

```

After everything is installed you should add the installed directory to `PATH`.

See `docs/BOOTSTRAP.md` for the full build and bootstrap layout.
