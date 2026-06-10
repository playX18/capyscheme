# Installation

To install and run CapyScheme you need:

- Rust toolchain (the repository pins one in `rust-toolchain.toml`)
- `make`

After they are installed simply run:
```sh
$ make install-portable
```

Or if you want to install system-wide
```sh
$ make PREFIX=/usr/local install

```

After everything is installed you should add installed directory into PATH.
If you wish to use native extensions also add `extensions/` directory under prefix
into your LD_LIBRARY_PATH/DYLD_FALLBACK_LIBRARY_PATH.
