# Installation

To build CapyScheme you need the pinned Rust nightly toolchain (`rust-toolchain.toml`; rustup installs
it on first use) and Bash. macOS and Linux on x86-64 and aarch64 are supported; Windows is not. Build the toolchain, then install it under a prefix:

```sh
$ ./bootstrap.sh
$ stage-2/capy install --toolchain stage-2 --prefix $HOME/.local
```

To install system-wide, run:
```sh
$ sudo stage-2/capy install --toolchain stage-2 --prefix /usr/local
```

The prefix gets `bin/` (`capy`, `capy-rt`, `capyc`), `lib/` (libcapy and the libraries of the
toolchain under `lib/capy/<version>`) and `include/`. Add `<prefix>/bin` to your PATH. The
executables find their libraries from their own location, so no environment variable is needed and you can move the tree.

The same tree can be packaged instead of installed:

```sh
$ stage-2/capy dist --toolchain stage-2 --format portable   # dist/capyscheme-<version>-<target>.tar.gz
$ stage-2/capy dist --toolchain stage-2 --format deb        # needs dpkg-deb (and fakeroot, if present)
$ stage-2/capy dist --toolchain stage-2 --format rpm        # needs rpmbuild
```
