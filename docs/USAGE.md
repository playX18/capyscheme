# Using CapyScheme

This page covers runtime behavior that matters when running scripts, importing
libraries, or debugging load-path problems.

## Execution mode

Capy starts in R7RS mode. Pass `--r6rs` or `--r7rs` to `capy` or `capyc` to
choose the execution mode explicitly:

```sh
capy --r6rs -L . -s tests/r6rs/run-via-eval.sps
capyc --r7rs -L lib -m "capy user" source.scm
```

The flags are mutually exclusive. Execution mode affects both expander rules and
which source file extensions are preferred when a library is loaded by name.

Reader directives such as `#!r6rs` and `#!r7rs` set the reader mode for that
file. They do not replace the CLI execution-mode flags when you need a different
library extension preference.

## Source extension search

When a file name already has an extension, Capy checks that exact name first. For
extensionless library names, the mode-specific extension list is used.

### R7RS mode

R7RS mode is the default. It prefers R7RS library files before shared Scheme
source extensions:

1. `<host-os>.sld`
2. `<host-family>.sld`
3. `<host-arch>.sld`
4. `capy.sld`
5. `sld`
6. `<host-os>.scm`
7. `<host-family>.scm`
8. `<host-arch>.scm`
9. `capy.scm`
10. `scm`
11. `sch`
12. `ss`

### R6RS mode

R6RS mode prefers R6RS library and program files before the same shared Scheme
source extensions:

1. `<host-os>.sls`
2. `<host-family>.sls`
3. `<host-arch>.sls`
4. `capy.sls`
5. `sls`
6. `sps`
7. `<host-os>.scm`
8. `<host-family>.scm`
9. `<host-arch>.scm`
10. `capy.scm`
11. `scm`
12. `sch`
13. `ss`

Use `--r6rs` when extensionless imports should find `.sls` or `.sps` files
before `.sld` files. Use `--r7rs` when the same library path should prefer
`.sld` files.

Custom extensions from `-x` / `--extensions` are added ahead of the built-in
mode list for that invocation:

```sh
capy --r7rs -x capy.scm,scm -L lib -s program.scm
```

## Load paths

Capy initializes source and compiled load paths from the install layout, then
applies environment variables and CLI flags.

- `CAPY_LOAD_PATH` prepends source library directories.
- `CAPY_LOAD_COMPILED_PATH` prepends compiled artifact directories.
- `-L DIR` / `--load-path DIR` adds a source directory before the existing load
  path.
- `-A DIR` / `--append-load-path DIR` adds a source directory after the existing
  load path.
- `-C DIR` / `--compiled-load-path DIR` adds a compiled artifact directory.

Example:

```sh
CAPY_LOAD_PATH=./lib capy --r6rs -L tests -C stage-2/compiled -s tests/r6rs/run-via-eval.sps
```

Portable installs look for source libraries under `lib/` and compiled libraries
under `compiled/` next to the `capy` executable. FHS installs look under
`share/capy/lib/` and `lib/capy/compiled/` below `CAPY_SYSROOT`.

## FASL artifacts and auto-compile

Compiled Scheme artifacts use the `.fasl` extension. A direct `.fasl` path is
loaded as an artifact. Otherwise Capy resolves source first, then looks for a
fresh matching `.fasl` in the compiled load path and in the fallback compile
cache.

The fallback cache lives under `capy/cache/<version>/` in `XDG_CACHE_HOME`,
`HOME`, `LOCALAPPDATA`, or `APPDATA`, and is split by GC plan. This prevents
compiled artifacts for one runtime configuration from being reused by another.

Use `--fresh-auto-compile` when tests or development runs should bypass existing
fallback cache artifacts and compile loaded source again:

```sh
capy -L lib --fresh-auto-compile -s tests/lib/some-test.scm
```

Concurrent auto-compiles coordinate through a sidecar lock file next to the
destination `.fasl`, then write the artifact atomically.
