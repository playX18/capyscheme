# Scheme Native Interface

The Scheme Native Interface (SNI) is how C and Rust code embeds CapyScheme or
extends it with native procedures. It has three layers:

| Layer | Path | Role |
|-------|------|------|
| C ABI | `c/capy.h`, implemented in `crates/capy/src/api/` | opaque `ScmRef` handles and a function table |
| Raw Rust bindings | `crates/capy-sni-sys/` | `extern "C"` declarations mirroring `capy.h` |
| Safe Rust API | `crates/capy-sni/`, macros in `crates/capy-sni-derive/` | `Scm`, `Env`, `Ref`, `#[scheme]`, `#[onload]`, `#[cps]` |

A Rust extension uses `capy-sni` (safe API), which sits on `capy-sni-sys`
(raw bindings). A C extension uses the header directly. Both go through the
`c/capy.h` function table, implemented by `libcapy`
(`crates/capy/src/api/`).

## Handles and rooting

SNI never exposes raw `Value` words. Every Scheme object crosses the boundary
as an opaque `ScmRef`. The collector can move objects, so a `ScmRef` is a slot
in an `OopStorage` that the collector updates:

- Local refs are created by SNI calls, live in the `SNI Locals` storage, and
  are released when the surrounding local frame is popped
  (`PushLocalFrame` / `PopLocalFrame`).
- Global refs (`NewGlobalRef` / `DeleteGlobalRef`) live in `SNI Globals` until
  deleted. They are not dropped automatically.

See "OopStorage" in `docs/HEAP.md`.

## Errors

Accessors that hit a type error or an out-of-range index set a pending
exception and return `SCM_REF_NULL` (or 0). Read it with
`sni_exception_occurred` and clear it with `sni_exception_clear`. Passing a
NULL env or ref returns silently without setting an exception.

## Function table

A `SniEnv` begins with a pointer to a `SniNativeInterface`, a table of
function pointers (`PushLocalFrame`, `NewLocalRef`, ...) that is also
available as plain functions. The Rust definition of the table is in
`crates/capy/src/api/vtable.rs`, and `crates/capy-sni-sys/src/lib.rs` must
mirror it in field order and types. The C header still spells the type
`SNINativeInterface` and the load callback `SNI_OnLoadFn`, while the Rust
side uses `SniNativeInterface` and `SniOnLoadFn`; the header also has `SniNativeInterface` as an alias.
The table starts with `abi_version` and `struct_size`. Extensions receive the env in their load
function and can use either style.

## Lifecycle

A process has one runtime. A second `scm_new` (or `scm_run_static`) returns null and logs the
reason, also after `scm_free`: the collector, the reference storage and the scheduler are
process-wide and are not torn down. `scm_run_static` keeps its runtime for the life of the process.

```c
Scm *scm = scm_new();
scm_enter(scm, my_main, NULL);           // runs my_main(SniEnv*, void*)
scm_free(scm);
```

A foreign OS thread cannot attach to a runtime (there is no `scm_attach_current_thread`): the
collector and the scheduler know only the threads they started. Native code that wants another
thread starts one with `scm_fork` from inside an env, which makes a Scheme thread.

## Calling Scheme from native code

Look up bindings with `sni_public_ref` / `sni_private_ref` and call them with
the call functions in `api/call.rs`. A nested call from native code raises the
thread's nest level, which allows collection at yieldpoints inside the callee,
so the caller's locals must be in local refs. For blocking work, native code
can leave the mutator with `call_in_native`.

Native code called from Scheme runs with no Scheme frame of its own. The
trampoline for an SNI procedure builds a fresh `SniEnv`, wraps each argument
in a local ref, calls the C function, and converts the result back. Inside
the runtime, native procedures read their arguments through `Call`
(`api/call.rs`).

## Native extensions

A shared library loaded with `load-native-extension` exports an entry
point of type `SniOnLoadFn`:

```c
int SNI_OnLoad(SniEnv *env, void *reserved);   // return 0 on success
unsigned SNI_ABIVersion(void);                 // c/capy.h defines it; #[onload] exports it
```

The load function typically registers procedures with the module `define`
functions in `api/native.rs`. `crates/capy-cparse-sni` is an in-tree example.
It exposes the C header parser behind `c-include`, and is built as the `cparse` `(cargo ...)` native extension of `std` (see [Packages](PACKAGES.md#native-extensions)).
A native procedure has type
`SniNativeFn`: `ScmRef f(SniEnv *env, int argc, const ScmRef *argv)`.

## Rust API

```rust
use capy_sni::{Env, Ref, onload, scheme};

#[scheme(name = "add", module = "smoke")]
fn add(x: i32, y: i32) -> i32 { x + y }

#[onload(frame = 8)]
fn init(env: &mut Env<'_>) {
    scheme_register_add(env);
}
```

- `#[scheme]` turns a Rust function into a Scheme procedure, converting
  arguments with `TryFromScm` and results with `IntoScm`
  (`conversions.rs`). It generates a `scheme_register_<fn>` function. The
  attribute also accepts structs.
- A `#[scheme]` fn may take `&mut Env` anywhere in its parameter list.
  ```rust
  #[scheme(name = "pair-of", module = "smoke")]
  fn pair_of<'env>(env: &mut Env<'env>, a: Ref<'env>, b: Ref<'env>)
      -> Result<Ref<'env>, Ref<'env>> {
      Ok(env.cons(a, b))
  }
  ```

  An `Err(Ref)` is raised as the exception object. Like every non-`cps`
  `#[scheme]` fn, it must not call back into Scheme.
- `#[onload]` emits the C `SNI_OnLoad` symbol and runs your function with an
  `Env`.
- `#[cps]` and `#[scheme(cps)]` rewrite `cps::call`, `cps::raise`, and
  `cps::guard` inside a function into continuation legs, so a native
  procedure can call back into Scheme without nesting.
- `Scm::new()` creates a runtime and `Scm::enter` runs a closure with an
  `Env`. Neither the `Env` nor any `Ref` can escape the closure.
- `PodRecord` (`pod.rs`) gives a typed view of a plain-old-data struct
  stored in a Scheme bytevector. The view borrows the `Env`, because an
  allocation or nested call could move the bytevector.

## ABI version

`SNI_ABI_VERSION` (`c/capy.h`, `capy-sni-sys`, `api/vtable.rs`) is bumped whenever the function table
or a function in it changes incompatibly. A shared library that is loaded with
`load-native-extension` must export `SNI_ABIVersion()`; the loader refuses a library that does not,
and one whose version differs from the runtime's, before it calls `SNI_OnLoad`. The legacy Rust
entry point `capy_register_extension` is no longer accepted. 
