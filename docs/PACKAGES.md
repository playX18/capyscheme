# Packages and the `capy` driver

A package is a directory with a `package.scm` and a directory of Scheme libraries. The `capy`
driver builds packages, fetches their dependencies, runs their programs and tests, and runs
scripts. 

- [A first package](#a-first-package)
- [The manifest](#the-manifest-packagescm)
- [Dependencies](#dependencies)
- [Version selection](#version-selection)
- [`package.lock`](#packagelock)
- [Views and renames](#views-and-renames)
- [Native extensions](#native-extensions)
- [The build](#the-build)
- [Commands](#commands)
- [Scripts and frontmatter](#scripts-and-frontmatter)

## A first package

```sh
$ capy new hello
$ cd hello
$ capy run
Hello, world!
$ capy test
test result: ok. 1 test passed; 0 files failed of 1; 0.2s
```

`capy new` writes `package.scm` (the manifest), `src/main.scm` (the program), `tests/example.scm`
(a SRFI 64 test) and `.gitignore` (`target/`). `capy new hello --lib` makes a library package
instead: `src/hello.scm` defines `(hello)` and there is no program. `capy init` does the same in the
current directory and leaves existing files alone.

## The manifest, `package.scm`

One datum, `(package FIELD ...)`. A field is `(NAME VALUE ...)`, may be given once, and an unknown
field is an error.

```scheme
(package
  (name hello)
  (version "0.1.0")
  (edition r6rs)
  (provides (hello *))
  (source "src")
  (dependencies
    (util (path "../util")))
  (programs
    (hello "src/main.scm")
    (hello-static "src/main.scm" (static #t)))
  (native
    (fast (c "native/fast.c" (cflags "-O2") (link "m"))))
  (no-std #f))
```

| Field | Value | Meaning |
| --- | --- | --- |
| `name` | symbol | Required. |
| `version` | string | Required, such as `"1.2.0"`. Git dependents ask for it with the tag `v1.2.0`. |
| `edition` | `r6rs` or `r7rs` | Required. Recorded and checked; it does not change compilation. |
| `provides` | patterns | Required, at least one. The libraries dependents can import. |
| `source` | string | Required. The directory of libraries, relative to the manifest. It must exist. |
| `dependencies` | list | See [Dependencies](#dependencies). Empty by default. |
| `programs` | list | The files `capy run` can run. Empty by default. |
| `native` | list | Shared libraries the package builds or ships. See [Native extensions](#native-extensions). |
| `no-std` | `#t` / `#f` | `#t` leaves `std` out of the package's view. `#f` by default. |
| `description`, `license`, `authors`, `homepage` | strings | Metadata for `capy dist`. The first author is the deb maintainer. |
| `dist` | `(deb (depends S ...)) (rpm (requires S ...))` | System packages a deb or rpm needs, e.g. `(deb (depends "libssl3"))`. |
| `boot-expander`, `source-only` | lists | Only for the packages that ship with the runtime: boot files the Rust expander compiles, and libraries the toolchain build skips. |

**Patterns.** A name in `provides` is a library name such as `(hello util)`, or a prefix ending in
`*`. `(hello *)` provides every library that starts with `hello` and has at least one more part
(`(hello util)`, `(hello util strings)`, not `(hello)`). Names are lists of symbols and non-negative
integers.

Every file below `source` whose top forms are `library` or `define-library` is a library of the
package. The driver reads only those headers, without expanding, to learn which file defines which
library and what it imports. A library the package defines but does not provide is an error that
names the file.

**Programs.** `(NAME "FILE")` names a script, relative to the manifest. `(NAME "FILE" (static #t))`
makes `capy build` link it into an executable with `capyc --static --exe`. `(static native)` also
links its native extensions in, see [Fully static native executables](#fully-static-native-executables).
`--static` links every program. `capy run` runs the only program, or the one named:
`capy run hello-static`.

## Dependencies

A dependency is `(NAME CLAUSE ...)`. Note that name of package and what libraries it provides is different.

```scheme
(dependencies
  (util (path "../util"))                                    ; a directory
  (json (git "https://github.com/x/json") (version "1.2"))   ; a tag
  (parser (git "https://github.com/x/parser") (rev "4f1c2e9")) ; a commit, tag or branch
  (json2 (git "https://github.com/x/json") (version "2.0")
         (rename (((json) (json2))))))
```

| Clause | Meaning |
| --- | --- |
| `(path "DIR")` | The package in DIR, relative to the manifest. |
| `(git "URL")` | A git repository. Needs exactly one of `version` and `rev`. |
| `(version "X.Y.Z")` | The lowest version needed. |
| `(rev "R")` | Commit by tag, branch, or full or abbreviated hash. Looked up once, then recorded in `package.lock`. |
| `(rename PAIRS)` | See [Views and renames](#views-and-renames). |

**Identity.** A fetched package is identified by its repository and major version, such as
`github.com/x/json@1`. That is its name in the build graph, its directory in `target/`, and its id
in the libmap. Two major versions are two packages, and a program can use both. 

## Version selection

A `version` is the lowest version a package works with, so the
build uses the highest version anything in the graph asks for.

1. Start at the root. Follow path and git dependencies.
2. For each repository and major version, take the highest `version` anyone asks for.
3. Fetch the tag `vX.Y.Z` of that version.
4. Read the manifest of every version anyone asked for so the whole graph is known
   before anything is chosen.

If the root asks for `json` 1.0 and `util` asks for 1.2, the build uses 1.2.0. A `v1.3.0` tag is not
used until something asks for it or `capy update` raises the version.

A `rev` pins a commit and replaces the versions of that package. It must satisfy them: if a package
asks for 1.2 and the revision's manifest says `1.0.0`, resolution fails. Two different revisions of
one package are an error. 

## `package.lock`

Resolution writes `package.lock` next to the root `package.scm`. It is recommended to commit it to your repositories.

Package lock is required to resolve dependencies online, perform updates, and verify freshness of each package to allow incremental recompilation. 

## Views and renames

In order to use a library in code, package can import its *view* which provides the following:

1. the libraries it provides,
2. the libraries each direct dependency provides,
3. the libraries of `core`, and of `std` unless `(no-std #t)`.

A dependency's own dependencies are not in the view. If two dependencies provide the same library 
error is raised.

`rename` can be used to rename libraries, and it takes `((FROM ...) (TO ...))` pairs: it moves libraries
which being with `FROM` under `TO`: `((json) (z json))` turns `(json)` into `(z json)`. 

## Native extensions

The `native` field lists shared libraries that the package's libraries load with
`load-native-extension`. Each entry is `(NAME KIND ...)`:

| Kind | Form | What `capy build` does |
| --- | --- | --- |
| `cargo` | `(name (cargo "dir"))` | Runs `cargo build` on the crate in `dir` (a `cdylib` on `capy-sni`, as a path or git dependency) against the toolchain's `libcapy`. |
| `c` | `(name (c "a.c" "b.c" (cflags "-O2") (link "m")))` | Compiles each file with `cc` (or `$CC`) against `capy.h`, then links one shared library. |
| `prebuilt` | `(name (prebuilt "path"))` | Uses the shared library at `path`. |

Outputs go to `target/<profile>/native/<package>/NAME.<dylib|so>`, each with a `NAME.native` file
recording its identity. An extension is rebuilt only when the identity changes.

The libmap lists each extension under `(#%native <package> NAME)`, and `load-native-extension`
accepts that key or a path. Use the macro `require-native-extension` of `(capy native)`:

```scheme
(library (fast ops)
  (export fast-add)
  (import (rnrs) (capy native))
  (require-native-extension fast)
  (define fast-add (native-extension-ref '(fast module) 'fast-add)))
```

The macro looks the extension up at compile time. A missing extension is a compile error, and the key
becomes a constant of the code. The extension registers its module itself (`SNI_OnLoad`), and
`native-extension-ref` takes a value from it. The library records the key and identity as a
compile-time dependency, so changing the extension recompiles that library (see
[LINKING.md](LINKING.md#compile_deps)).

### Fully static native executables

By default a `--static` executable has the Scheme part in one file, with the extensions and `libcapy`
beside it. To get a single statically linked binary declare a program as `(static native)`:

```scheme
(programs (hello "src/main.scm" (static native)))
```

or `capy build --static-native` for every program (implies `--static`), or
`capyc --static --exe --static-native -o OUT FILE` outside a package.

- `capy build` also makes a static form of each extension, `NAME.a` and a descriptor `NAME.link`, in
  `target/<profile>/native/<package>/`. The shared library is still built, because compile-time
  `c-include` and extension loading need it.
  - `cargo`: `cargo rustc --lib --crate-type staticlib`, so the crate may stay a `cdylib`. It must
    export its entry point with `#[onload]` of `capy-sni`, since a hand-written `SNI_OnLoad` has the
    same name in every extension. 
  - `c`: sources are recompiled with `-DSNI_OnLoad=capy_ext_<package>_<name>_OnLoad` and archived
    with `ar` (or `$AR`). `(link "m")` libraries go to the final link.
  - `prebuilt`: needs the archive, `(prebuilt "lib/libvendor.dylib" "lib/libvendor.a")`. A shared
    library alone is an error; it is never linked dynamically as a fallback. The archive must export
    `SNI_OnLoad`, so at most one prebuilt extension fits in an executable.
- `capyc` links with `cc` (or `$CC`) a generated C file with the `(key, SNI_OnLoad)` table, the
  archives of the extensions the program loads, and `libcapy_static.a`, found next to the launcher
  in `../lib` (an install).
- At run time `load-native-extension` looks the key up in that table.

The result runs from anywhere, has no `lib/` directory, and is larger because it contains the
runtime. Libraries loaded by FFI (`dlopen`, a `c-include` shim) load as usual, but a shared library
that calls the runtime's SNI functions cannot be loaded by such an executable.

## The build

`capy build` compiles the package and its dependencies into `target/debug/` (`target/release/` with
`--release`):

```text
target/debug/capy.libmap               libraries of the toolchain and the build, and the views
target/debug/hello/fasl/...            compiled libraries (hello is the package id)
target/debug/hello/programs/NAME.fasl  compiled programs
target/debug/bin/NAME                  static executables (libcapy is copied beside them)
target/debug/build/hello/out/          the out dir
```

1. Resolve git dependencies.
2. Read the headers of the source files and build the graph of libraries from their imports.
3. Decide what is stale, by the rule in [LINKING.md](LINKING.md#the-recompile-rule). 
4. Compile the stale files, one `capy-rt --compile` subprocess per file, `-j` at a time (4 by
   default).
5. Write `capy.libmap`. 

`capy build -v` says why each file is compiled. `capy check` compiles into `target/check`, for errors
only, and also warns about calls with the wrong number of arguments. 

**The out dir** holds files generated while compiling, as Cargo's `OUT_DIR` does: `c-include` writes
its library `(capy c gen <hash> ...)` there. See [LINKING.md](LINKING.md#the-out-dir) and
[C-INCLUDE.md](C-INCLUDE.md).

## Commands

```text
capy new NAME [--lib]             create a package in the directory NAME
capy init [--lib]                 create a package in the current directory
capy build                        compile the package and its dependencies
capy run [PROGRAM] [-- ARGUMENT ...]
capy test [FILTER ...]            run the test files of tests/
capy install [URL | --path DIR]   install the programs into ~/.capy/bin
capy uninstall NAME               remove what `capy install` installed
capy dist [--format F]            a portable .tar.gz, a .deb or an .rpm of the programs
capy check                        compile into target/check, reporting every error and warning
capy clean                        remove target/
capy repl                         a REPL that can import the package's libraries
capy add URL | --path DIR         add a dependency
capy remove NAME                  remove a dependency
capy update [NAME ...]            move git dependencies to the latest tag of the same major version
capy fetch                        resolve and fetch, write package.lock
capy tree                         show the dependency graph
capy FILE.scm [ARGUMENT ...]      run a script
capy help | capy --version
```

Flags of `build`, `run`, `test`, `check`, `repl`, `fetch` and `tree`:

| Flag | Meaning |
| --- | --- |
| `--release` | Build into `target/release`. |
| `--static` | Link every program into `target/<profile>/bin/NAME`. |
| `-j N`, `--jobs N` | Compile N files at once. 4 by default. |
| `--offline` | Use only `package.lock` and the cache. |
| `--locked` | Fail if `package.lock` would change. |
| `-v`, `--verbose` | Say why a file is compiled. |
| `--color auto\|always\|never` | Status line colors. `NO_COLOR` turns them off. |
| `--message-format human\|json` | Diagnostics format. `json` is one object per line on stdout. |

### `capy test`

Builds, then runs each test file in its own process, in the package directory, with the
package's libraries. A test file is a `tests/**/*.scm` script or a `tests/**/*.sps` R6RS program. Tests use SRFI 64.

A file fails when it exits nonzero, is killed, exceeds `--timeout`, or its SRFI 64 report counts an
unexpected failure or success. The exit status is 1 if any file failed.

| Flag | Meaning |
| --- | --- |
| `FILTER ...` | Run only files whose path below `tests/` contains one of them. |
| `--filter NAME` | Run only the SRFI 64 test of this name. |
| `--list` | List the test files. |
| `--no-fail-fast` | Keep going after a failure (by default no file starts after one failed). |
| `--test-threads N` | Run N files at once. 1 by default. |
| `--timeout SECONDS` | Stop a file that runs longer. 300 by default; 0 is never. |
| `-p NAME`, `--workspace` | Test the workspace member NAME, or every member that has `tests/`. By default: the member of the current directory, or every member at a virtual workspace root. |

### `capy install` and `capy dist`

`install` builds the programs for release as static executables and copies them to `PREFIX/bin`.
`PREFIX` is `--prefix`, `$CAPY_HOME`, or `~/.capy`. The package is the current directory, `--path
DIR`, or a git URL (latest `vX.Y.Z` tag, or `--version V`, or `--rev R`). `PREFIX/.capy-installs.scm`
records each package's files. Installing again replaces them, `--list` lists them, and `capy
uninstall NAME` removes them. A file that another package or nothing installed is not replaced
without `--force`. 

NOTE: `capy install --toolchain STAGE --prefix P` is an internal command to install *toolchain* of Capy, it should not be used
for regular Scheme projects.

`capy dist` packs tree into `target/dist` in a format you select: 
- `portable`: tar archive with project build for specific target
- `deb`/`rpm`: DEB and RPM packages. They use metadata from package.scm.

### Dependencies

```sh
capy add https://github.com/x/json                 # the latest vX.Y.Z tag
capy add https://github.com/x/json --version 1.2
capy add https://github.com/x/json --rev main --name json
capy add --path ../util
```

The name is the last part of the URL unless `--name` gives one. For `--path` it is the name in that
package's manifest. `package.scm` is edited in place, keeping comments and layout, and restored if
resolution fails.

`capy remove NAME` removes a dependency and updates the lock. `capy update` raises the `version` of
the direct dependencies to the highest tag of the same major version, and
looks revisions up again. `capy fetch` resolves and fetches without building. 
`capy tree` prints the graph, marking a package seen before with `(*)`:


## Scripts

`capy FILE.scm ARGUMENT ...` runs a file against `core` and `std`, with no package. A script can declare dependencies
in after `#!` block in `#|capy |#` blocks:

```scheme
#!/usr/bin/env capy
#|capy
(dependencies
  (greet (git "https://github.com/x/greet") (version "1.0")))
|#
(import (rnrs) (greet))
(display (hello (caddr (command-line))))
(newline)
```

```sh
$ capy hi.scm world
    Fetching github.com/x/greet v1.0.0
   Compiling greet v1.0.0 (github.com/x/greet@1)
Hello, world!
$ capy hi.scm again
Hello, again!
```

Scripts do not have `target/` dir and their artifacts are stored in `$XDG_CACHE_HOME/capy/scripts/`.
