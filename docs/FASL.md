# FASL code images

FASL ("fast loading") is CapyScheme's binary format for compiled Scheme values and code. In the current AOT execution policy, Scheme libraries are compiled to `.fasl` files and loaded back into the runtime with `FaslReader`; they are not linked as platform shared objects.

## Where FASL fits

The compilation path is:

```text
Scheme source -> Tree IR -> CPS -> Linear CPS -> Cranelift code -> FASL bytes
```

The source entrypoints are:

- `capy/src/compiler/object.rs`
  - `compile_cps_to_fasl_bytes` reifies CPS, linearizes it, builds SSA/Cranelift code with `ModuleBuilder`, and returns unified FASL bytes.
- `capy/src/runtime/vm/load/compile.rs`
  - `compile_cps_to_destination` writes those bytes to a `.fasl` destination under an artifact lock.
- `capy/src/runtime/vm/libraries.rs`
  - `LibraryCollection::load` reads `.fasl` files through `FaslReader` and keeps loaded libraries reachable by the GC.

The format constants live in `capy/src/runtime/fasl/mod.rs`:

- magic bytes: `CAPYFSL\0`
- current version: `5`
- minimum supported version: `4`

## Producing artifacts

Use `capyc -o` to write a FASL image:

```sh
stage-0/capyc -o stage-1/compiled/boot/base.fasl -m "capy" -L lib lib/boot/base.scm
```

The bootstrap Makefile sets `COMPILED_SCM_EXT := fasl` and uses this pattern for every library group. Typical outputs are:

```text
stage-1/compiled/boot/base.fasl
stage-1/compiled/core/lists.fasl
stage-1/compiled/rnrs/base.fasl
```

The FASL change applies to compiled Scheme library artifacts. The `capy` executable itself is built by Cargo, while `capyc` is a wrapper that starts compiler mode.

## Loading artifacts

The default execution policy is AOT, and AOT maps to `LoadArtifactKind::FaslCode` with extension `.fasl`.

Load resolution works in this order:

1. If the requested filename already has an artifact extension, load that artifact directly.
2. Otherwise, resolve source through `%load-path` / `-L`.
3. Search `%load-compiled-path` / `-C` for a fresh matching `.fasl`.
4. If no compiled artifact is usable, compile to the fallback cache path and load that artifact when auto-compilation is forced.

`CAPY_LOAD_PATH` initializes `%load-path`; `CAPY_LOAD_COMPILED_PATH` initializes `%load-compiled-path`. Command-line `-L` and `-C` prepend paths for the current process.

Compiled path lookup checks both architecture-scoped and unscoped layouts when no explicit architecture is requested:

```text
<compiled-dir>/<arch>/lib/name.fasl
<compiled-dir>/lib/name.fasl
```

Fallback auto-compile output is always architecture-scoped under the cache root:

```text
<cache>/capy/cache/<version>/<plan-bucket>/<arch>/<absolute-or-relative-source>.fasl
```

The plan bucket is `gen` for `StickyImmix`, `conc` for `ConcurrentImmix`, and `regular` for `Immix` or `MarkSweep`.

## Troubleshooting

- Use `--fresh-auto-compile` when source changed and you want the loader to rebuild fallback artifacts instead of reusing an existing compiled file.
- Remove the cache directory if a fallback `.fasl` is corrupt or was produced by an incompatible local build.
- Use `--debug` when running `capy` to load FASL code through debug stacktrace trampolines.
- If compilation fails with a lock error, check the sibling `*.fasl.lock` file and whether another process is compiling the same source.
