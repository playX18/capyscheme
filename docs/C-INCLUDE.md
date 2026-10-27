# c-include

`c-include` reads a C header and generates Scheme bindings for it, like Rust's bindgen.

```scheme
(import (rnrs) (capy c))

(c-include "math.h" #:link '("m"))
(sqrt 2.0)   ; => 1.4142135623730951
M_PI         ; => 3.141592653589793

(c-include "stdlib.h" #:prefix c:)
(c:atoi "42")                                  ; => 42
(let ([r (c:ldiv 17 5)])                       ; struct returned by value
  (list (c:ldiv_t-quot r) (c:ldiv_t-rem r)))   ; => (3 2)
```

## Options

| Option | Meaning |
| --- | --- |
| `#:link '("m")` | Libraries to search after the process itself. |
| `#:include-dirs '("dir")` | Extra `-I` directories. |
| `#:defines '(("N" . "V") "FLAG")` | `-D` definitions. |
| `#:std "c23"` | Language standard. |
| `#:quote-include #t` | Look up the header like `#include "x.h"`. |
| `#:only-from '("a.h" "b.h")` | Export declarations from these headers instead of only the requested one. |
| `#:include '("str*")` | Export only these names. Trailing `*` matches a prefix. |
| `#:exclude '("sin*")` | Never export these names. |
| `#:rename '(("old" . "new"))` | Rename generated names. |
| `#:strip-prefix '("SDL_")` | Remove prefixes from names. |
| `#:add-prefix "c:"` | Add a prefix to names. |
| `#:prefix c:` | Import the library under this prefix. |
| `#:blocking` | `#t`, `#f`, or a list of names that may block. Default: a built-in list (`read`, `write`, `poll`, `accept`, ...). |
| `#:frameworks '("CoreFoundation")` | macOS frameworks to link. Errors elsewhere. |
| `#:pkg-config "zlib"` | Take flags from `pkg-config --cflags --libs`. |
| `#:string-returns #t` | `char *` results become strings, NULL becomes `#f`. Default `#f`. |

## What is generated

**Functions.** Each is a procedure. Variadic arguments follow the fixed ones. A
`char *` argument accepts a string; a movable bytevector is copied, so pass a
nonmoving one for a buffer C writes into. `_Bool` maps to booleans.

A function-pointer parameter takes a Scheme procedure, valid only for that call.
If C keeps the pointer (a signal handler, say), build the callback with
`make-TYPENAME` (see Typedefs).

**Constants and enums.** Object-like macros with a fixed value and enumerators
become constants. Each enum `NAME` also gets:

| Name | Meaning |
| --- | --- |
| `NAME-names` | alist of `(enumerator . value)` |
| `NAME->symbol`, `symbol->NAME` | convert between value and enumerator symbol |
| `NAME-flags->list` | symbols in a value. Only if all values are zero or powers of two |

**Typedefs.** A typedef is a value standing for its foreign type. A
function-pointer typedef also gets `make-NAME` (procedure to callback. Release
with `free-callback-pointer`) and `NAME->procedure` (C function pointer to
procedure).

**Structs and unions.** A struct `NAME` (or its typedef name) gets:

| Name | Meaning |
| --- | --- |
| `make-NAME` | constructor, fields in declaration order, missing fields are zeroed |
| `sizeof-NAME`, `alignof-NAME` | size and alignment |
| `NAME-FIELD`, `NAME-FIELD-set!` | field access |

A struct value is a garbage-collected pointer to its bytes. Unions have the same
accessors, but `make-NAME` takes no values.

- Nested fields: `OUTER-INNER-FIELD`. Anonymous members are flattened.
- Array fields take an index: `(NAME-FIELD p i)`.
- `char NAME[N]` also gets `NAME-string` and `NAME-string-set!` (up to the first NUL).
- Bitfields read as integers, sign-extended if signed.
- `_Bool` fields read as 0 or 1. Convert with `bool->boolean` / `boolean->bool`.

**Globals.** `NAME` (read, scalars only), `NAME-set!` (scalar, non-`const`) and
`NAME-address` (thunk). Arrays and structs get only `NAME-address`.

**Function-like macros.** Expression macros become procedures with C semantics (best effort).

**Static inline functions.** C shim is compiled for such functions, and they are dynamically loaded.

## Output parameters

`with-c-out` allocates scratch memory for its body and frees it on exit, even on
a raise. `c-out-ref` reads from it.

```scheme
(with-c-out ((p int))
  (list (frexp 8.0 p) (c-out-ref p int)))   ; => (0.5 4)
```

## Diagnostics

`(c-include-report)` returns `(HEADER KIND NAME WHY)` entries for the most recent
generation in this process. A cache hit generates nothing, so the report is then
empty. Skipped declarations and parse errors are also printed to stderr.

| Kind | Meaning |
| --- | --- |
| `skipped` | declaration not generated |
| `macro-skipped` | macro not generated |
| `shim-skipped` | no shim possible (variadic, or a type C cannot spell) |
| `shim-failed` | shim did not compile |
| `parse-errors` | parser errors |

## Cache

Generation runs the C parser (and compiler, for shims), so results are cached.
`(c-cache-directory)` is the first of: `CAPY_FFI_CACHE`,
`$XDG_CACHE_HOME/capy/ffi`, `$HOME/.cache/capy/ffi`.

The key covers OS, architecture, header name, effective
options, and the variables `CC`, `CAPY_CC`, `SDKROOT`, `CPATH`,
`C_INCLUDE_PATH`, `CPLUS_INCLUDE_PATH`, `DEVELOPER_DIR`. The manifest also
records the mtime and size of every file the parser read, so editing a header
regenerates.

A hit stats those files and loads the generated library, with no parser or
compiler involved. The cache directory is recorded at expansion time, so a
compiled program uses the cache that existed then. `c-use-cache-directory!`
adds a directory to the load path by hand.

A failed shim build is stored and retried only when the compiler, key variables
or shim source change. To force a retry, delete the request's file under
`requests/` in the cache directory.

To ship a warm cache, run the program once with `CAPY_FFI_CACHE` set to the
directory to ship, on a machine matching the target's headers and compiler.

## The C parser

Capy ships with DIY C23 parser which does only the necessary steps for binding generation. We do not use libclang
due to its big size and requirement of adding another dependency to build process. Note that parser and preprocessor
might be buggy, but so far they handle macOS and glibc headers with mostly no problems.

## Finding libraries

The generated library opens the process itself, then each `#:link` name:

- No slash or dot (`"m"`): `libm.so.0`..`libm.so.9`, `libm.so`, `libm.dylib`, then `m`.
- Slash or dot: tried as given.
- An absolute path the loader refuses (a macOS framework binary) is opened with `dlopen`.

`#:frameworks` opens binaries under `/System/Library/Frameworks` and compiles
shims with `-framework NAME`.

## Example: libc sockets

Use one `c-include` per header:

```scheme
(c-include "sys/socket.h" #:include '("AF_INET" "sockaddr_storage"))
(c-include "netinet/in.h"
  #:only-from '("netinet/in.h" "netinet6/in6.h")
  #:include '("sockaddr_in" "sockaddr_in6" "in6_addr_t"))

(let ([s (make-sockaddr_in)])
  (sockaddr_in-sin_family-set! s AF_INET)
  (sockaddr_in-sin_addr-s_addr-set! s #x0100007f)   ; 127.0.0.1, network order
  (sockaddr_in-sin_family s))                        ; => 2
```

- A struct with a typedef is named by the typedef (`in6_addr_t`, not `in6_addr`).
- Byte order and string terminators are yours to handle, there is no `htons`.
- `sin_len` and `sun_len` exist only on BSDs such as macOS.
- Wrap `getaddrinfo` results with `(make-pointer address)` and walk `addrinfo-ai_next`.

See `lib/capy/net/sys.scm` for a larger example.

## Limitations

- `long double`, `_Complex`, `__int128` and `va_list` are unsupported; declarations using them are skipped and reported.
- Static non-inline functions are skipped silently (no symbol).
- Shim signatures drop `const`.
- `<<` in function-like macros is not wrapped to its type's width, so narrow types may differ from C.
