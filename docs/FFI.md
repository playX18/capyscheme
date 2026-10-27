# Foreign function interface

`(core foreign)` and `(core foreign-library)` call C functions from Scheme. You
describe a function's types and get back a procedure. To generate those
descriptions from a header instead, see [C-INCLUDE.md](C-INCLUDE.md).

```scheme
(import (core) (core foreign) (core foreign-library))

(define libc (load-foreign-library #f))   ; #f is the running process

(define labs* (foreign-library-function libc "labs"
                #:return long
                #:arguments (list long)))

(labs* -7)   ; => 7
```

The symbol is looked up once. Each call checks its arguments, converts them to
C values, calls, and converts the result back.

## Loading libraries

`(load-foreign-library filename [extensions search-path search-system-paths?
lazy? global?])` returns a library record.

- A name with a slash is a path. A bare name is searched in the current
  directory and the search path, then by the system loader. The default
  extension is the platform's shared-library suffix.
- `lazy?` defaults to `#t`, `global?` to `#f`.
- A library that cannot be found raises an error.

`(foreign-library-function lib name keyword ...)`:

| Keyword | Default | Meaning |
| --- | --- | --- |
| `#:return` | `void` | return type |
| `#:arguments` | `'()` | list of argument types |
| `#:variadic?` | `#f` | the function takes `...` after the listed arguments |
| `#:blocking?` | `#t` | see [Blocking calls](#blocking-calls) |

`(foreign-library-pointer lib name)` returns a symbol's address as a pointer. Use for global variables.

## Types

| Type | C type |
| --- | --- |
| `void` | `void` (return only) |
| `int8` `uint8` `int16` `uint16` `int32` `uint32` `int64` `uint64` | fixed width |
| `short` `unsigned-short` `int` `unsigned-int` `long` `unsigned-long` | sized from the platform |
| `size_t` `ssize_t` `ptrdiff_t` `intptr_t` `uintptr_t` `wchar_t` | sized from the platform |
| `float` `double` | floating point; `double` also accepts exact integers |
| `complex-float` `complex-double` | `_Complex` |
| `bool` | `_Bool`, as a `uint8`; convert with `bool->boolean` and `boolean->bool` |
| `'*` | any pointer |
| `(list T ...)` | struct by value, fields in order |
| `(vector T N)` | array of `N` elements, at most 1 GiB in total |

`sizeof` and `alignof` take these descriptions. Integer arguments are range
checked: a value that does not fit the type raises instead of wrapping.

A struct argument is a non-NULL pointer or a bytevector at least as long as the
struct. A struct returned by value comes back as a pointer to a copy of its
bytes. Read fields with `pointer-ref` at computed offsets, or let
[`c-include`](C-INCLUDE.md) generate accessors.

## Blocking calls

`#:blocking?` says whether the function may wait on something outside the
program.

- `#t` (default): the call runs in native mode. The collector does not wait for
  it and other threads keep running.
- `#f`: the call runs in place. It is cheaper and right for functions that only
  compute. Do not mark a function that waits as non-blocking.

## Pointers

`%null-pointer` is NULL. `make-pointer` builds a pointer from an address and
`pointer-address` returns it.

```scheme
(define mem (foreign-alloc 64))          ; zeroed C memory
(pointer-set! uint32 mem 0 #xDEADBEEF)   ; typed store at byte offset 0
(pointer-ref uint32 mem 0)               ; => #xDEADBEEF
(foreign-free mem)
```

Offsets are in bytes. `pointer-ref/pointer` and `pointer-set/pointer!` read and
write pointer values. NULL pointers and offsets that wrap the address space are
rejected. The collector never frees `foreign-alloc` memory.

For hot paths, use the typed forms `pointer-ref/<t>` and `pointer-set!/<t>`, where
`<t>` is one of `u8 s8 u16 s16 u32 s32 u64 s64 f32 f64`. They take the same
`(pointer offset [value])` arguments minus the type, and are compiler primitives: the JIT
inlines `u8`, `s8`, `u16`, `s16`, `s32`, `f32` and `f64` (the 64-bit and `u32` forms can need a bignum and make an out-of-line call). The bytevector accessors
`bytevector-*-ref` and the matching `-set!` procedures are inlined the same way. 

A `'*` argument accepts a pointer, `#f` (NULL), or a bytevector made with
`make-bytevector/nonmoving`. A movable bytevector or a string raises, because
the collector could move it while C holds its address.

| Procedure | Purpose |
| --- | --- |
| `string->pointer` | copy a string to nonmoving memory as NUL-terminated UTF-8 |
| `pointer->string` | read a NUL-terminated string |
| `bytevector->pointer` | pointer to a bytevector's memory, and a movable bytevector is copied first |
| `pointer->bytevector` | view memory as a bytevector |
| `dereference-pointer` | read a pointer stored at an address |
| `set-pointer-finalizer!` | run a procedure when the pointer is collected |
| `null-pointer?` | NULL test |

## Variadic functions

Pass `#:variadic? #t`:

```scheme
(define snprintf*
  (foreign-library-function libc "snprintf" #:return int
    #:arguments (list '* size_t '*) #:variadic? #t))
```

An untagged variadic argument is typed from its value: a fixnum is `int`, a
bignum `int64`, a flonum `double`, a pointer, bytevector or `#f` is `'*`. Use
`(typed TYPE VALUE)` for anything else:

```scheme
(snprintf* out 64 (string->pointer "%ld") (typed int64 8589934593))
```

## errno

`(errno)` returns `errno` as it was right after the last foreign call on this
thread. `(errno-set! n)` sets it before a call.

## Callbacks

`procedure->callback-pointer` makes a C function pointer from a procedure.

```scheme
(define qsort* (foreign-library-function libc "qsort"
                 #:return void #:arguments (list '* size_t size_t '*)))

(define cb
  (procedure->callback-pointer int (list '* '*)
    (lambda (a b)
      (- (pointer-ref int32 a 0) (pointer-ref int32 b 0)))))

(qsort* arr 5 4 cb)
(free-callback-pointer cb)
```

- A callback lives until `free-callback-pointer`. C must not use it afterwards.
- It may run on any thread's worker, including during a blocking call. A
  call from an OS thread the runtime does not know prints a message and returns 0.
- An exception cannot unwind through C. It prints a message and the callback
  returns 0, as it does for a result that does not fit the return type.

## Packed structs

`define-c-struct` describes a struct for reading and writing bytevectors:

```scheme
(define-c-struct point point-size list read-point write-point!
  (x int32)
  (y int32))

(define bv (make-bytevector point-size))
(write-point! bv 0 3 4)
(read-point bv)            ; => (3 4)
(point field-offset y)     ; => 4
```

`point-size` is the size in bytes, `read-point` calls the wrapper (`list` here)
with the field values, and `write-point!` takes a bytevector, an offset and the
values. Field types may also be `(array T N)`, `'*`, `int128`, or `(T ~ endian)`
for a fixed byte order. `struct-size`, `struct-alignment`, `read-c-struct`,
`write-c-struct`, `make-c-struct` and `parse-c-struct` are the procedural
versions.

## Limitations

- `long double` and `__int128` arguments are not supported.
- Variadic arguments of complex type raise.
