# Linking: linklets, decls, the libmap

## Linklets and FASL files

A `.fasl` file is a directory of **linklets**. A linklet is the compiled code of one unit (a library
body or a boot file) with its imports and exports as named variables. When
something requires a library, the registry declares it from its decl and instantiates it after its
imports.

The directory can be read without running code (`fasl-read-directory`) which can be used to inspect
the library without instantiating it.

A `.wpo` file next to a `.fasl` holds the Tree IL of the library code, which is how we do Whole-Program Optimization.

## Decls

Each library in a FASL has a decl:

```text
#(#%library key version exports imports language replacements
  source_hash interface_hash deps compiler_id knowns compile_deps)
```

| Field | Meaning |
|-------|---------|
| `exports` | `(external internal kind origin)`. `origin` is the `(key . name)` a re-export resolves to, else `#f`. |
| `source_hash` | SHA-256 of the source text. |
| `interface_hash` | What an importer can see: exports, macros, knowns. |
| `deps` | `(key . interface_hash)` of the libraries code was compiled against. |
| `compiler_id` | The compiler that made the file. |
| `knowns` | Facts for cross-library inlining, keyed by internal name: `(constant datum)` or `(procedure arity-mask)`. |
| `compile_deps` | `(path . hash)` of the files expansion read besides the source. |

## The recompile rule

A compiled library is rebuilt when:

1. its `source_hash` differs from the hash of the source file,
2. its `compiler_id` is not the running compiler's,
3. the `interface_hash` of any library in `deps` changed, or
4. any file in `compile_deps` has another hash.

## compile_deps

A library can depend on more than its source text. `(include "file.scm")` reads a file, and
`(c-include "header.h")` parses a header. During expansion they call `note-compile-dependency!`, and
the file's hash goes into the decl. This allows us to trigger rebuild only when you modify said header or file.

A native extension loaded while a library is compiled (`require-native-extension`) is noted the same
way, as `(native-key . identity)` (`note-native-dependency!`). 

## The output directory

Code that generates files at compile time writes them to `(compile-output-directory)`. Under
`capy build` that is `target/<profile>/build/<pkg-id>/out/`. Outside a package it is under the user
cache directory (`--compile-output-directory` changes it).

`c-include` writes its generated libraries there as ordinary libraries named `(capy c gen <sha>)`.

## Libmap

`capy.libmap` sits next to the compiled files of a toolchain or a build:

```text
#(#%libmap 2 compiler-id libraries views native)
```

- `libraries`: entries `(key source fasl index interface-hash wpo)`, with paths relative to the
  libmap's directory. Keys of fetched packages are `(#%pkg <id> . <name>)`, so two packages can
  have a library of the same name.
- `views`: which libraries each package can see, from its manifest.
- `native`: entries `(key file identity)`. The key is `(#%native <package-id> <name>)`, and the file
  is relative to the libmap or absolute. The ids are `core` and `std` for the toolchain. Installed,
  `std`'s extension is `lib/capy/<ver>/std/native/cparse.<dll>`.

Runtime resolves imports through libmap files.

## Native extensions in a static executable

`capyc --static --exe` scans the constants of the linked body for keys the libmap lists as native
extensions. It copies their shared libraries to `lib/<package-id>/<name>.<dll>` beside the
executable, next to the `libcapy`. An executable has no libmap, so
`load-native-extension` falls back to that path, relative to `(executable-path)`. 

NOTE: use `require-native-extension` for extnesions loaded at runtime. 

### Fully static: `--static-native`

`capyc --static --exe --static-native` produces a fully statically linked executable by linking `libcapy_static.a`,
generates a C file with `capy_static_extension_table()` which returns extensions defined in binary in form of `(key text, INIT) 
per extension. The INIT function is `SNI_OnLoad` of all extensions registered and is called by entrypoint. 
