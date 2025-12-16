# Installation

To install & run CapyScheme you need additonal dependencies:
- Just: for running Justfile recipe
- lld: for linking resulting Scheme dynamic libraries. Right now it is hardcoded, but in the future
it should be possible to change it

After they are installed simply run:
```sh
$ just install-portable
```

Or if you want different installation prefix:
```sh
$ just install-prefix=/usr/local/share install-portable 

```

After everything is installed you should add installed directory into PATH.
If you wish to use native extensions also add `extensions/` directory under prefix
into your LD_LIBRARY_PATH/DYLD_FALLBACK_LIBRARY_PATH.

## FHS-style install (system layout)

For a traditional Unix filesystem layout (FHS), you can install:

```sh
just fhs-prefix=/usr/local install-fhs
```

This installs:

- `capy` to `/usr/local/bin/capy`
- `libcapy.*` to `/usr/local/lib/`
- Scheme stdlib to `/usr/local/share/capy/`

The FHS sysroot prefix is compiled into the `capy` binary during `just install-fhs`.

## Sharing compiled artifacts across machines

CapyScheme caches compiled Scheme modules as native dynamic libraries. These are only safely
reusable across systems when the ABI-relevant inputs match (OS/arch, CapyScheme version, GC plan,
and toolchain/loader compatibility).

Recommended approach:

1. Use a shared directory (e.g. NFS) for compiled artifacts.
2. Add it to the compiled search path via `CAPY_LOAD_COMPILED_PATH`.

Example:

```sh
export CAPY_LOAD_COMPILED_PATH=/srv/capy/compiled
capy
```

CapyScheme will still use a per-user cache under `$XDG_CACHE_HOME` (or `~/.cache`) as a fallback.

## GC limitations

Once the `capy` binary and heap image are built the GC algorithm is locked in. For example
if you built with `MMTK_PLAN=GenImmix` env variable set you can only change GC plans to GenCopy 
or StickyImmix when running this binary. 

If you want to run with concurrent Immix simply perform build with `MMTK_PLAN=ConcurrentImmix` env variable
set.