# Heap

Capy’s garbage collector lives in `crates/capy/src/heap/`. It is precise (no
conservative native-stack scan) and driven by MMTk via the `MemoryManager`
binding in `heap/mm.rs`.

If you are writing mutator code (runtime, VM, natives), you almost always want
`runtime::Context`. Do not pass `Mutation` around at call sites. There is no
public `Heap` capability type. `Mutator` is `pub(crate)` and only binds the
thread to MMTk (`heap/mutator.rs`).

## Context

`Context<'gc>` (`runtime/thread.rs`) wraps the per-thread `Mutation`. Use it for:

- allocation (`ctx.alloc`, `ctx.alloc_in`)
- rooting (`ctx.roots()`, `root_scope!(ctx, scope)` / `scope!(ctx, scope)`)
- barriers (via `Gc::write`, `Gc::unlock`, `field!`)
- leaving the mutator (`ctx.call_in_native`)

```rust
fn example(ctx: Context<'_>) {
    let pair = ctx.alloc(something);

    // Interior mutation goes through the write barrier.
    let w = Gc::write(ctx, locked);
    w.field.set(new_value);

    let scope = ctx.roots(); // or root_scope!(ctx, scope)
    let pinned = scope.root(value);
    ctx.call_in_native(|| blocking_io());
    let value = pinned.get(); // reload after InNative / safepoint
}
```

`Context` derefs to `Mutation`, so low-level `Mutation` APIs are reachable
through it. `Mutation` itself is `#[doc(hidden)]`; it still exists for MMTk
plumbing, derives, and `Rootable`. Prefer `Context` everywhere else.

## Allocation

`ctx.alloc(T)` puts `T` in the default space (movable nursery / immix,
depending on plan). `ctx.alloc_in(space, T)` selects:

| Space | Use |
|-------|-----|
| `Space::Default` | Normal Scheme objects (may move) |
| `Space::NonMoving` | Addresses exposed to C / FFI interiors |
| `Space::Los` | Large objects |

Objects of `size + header >= 8 KiB` are automatically promoted to `Los` in
`heap/mutator.rs`. The default-space fast path bumps a per-thread TLAB
(`heap/lab.rs`); slow paths flush and refill it around MMTk calls.

Allocation uses `at_safepoint: false`: it may **schedule** a collection but
must not block, collect, or run yieldpoints inline. Safe `#[scheme]` leaves
and compiled alloc sites rely on this — the mutator pauses only at compiled
yieldpoints, while `InNative`, or inside a nested Scheme call. When GC is
pending, schedule it; do not soft-defer forever.

### FFI and NonMoving

Anything whose raw address escapes into C must not move:

- `bytevector->pointer` copies a movable owned bytevector into NonMoving
  before returning a pointer; an ephemeron keeps the bytevector alive while
  the pointer is live (`runtime/vm/ffi.rs`)
- `string->utf8/nul` allocates NonMoving; plain `string->utf8` is movable
  (`runtime/vm/strings.rs`)
- `string->pointer` builds a NUL-terminated bytevector via
  `make-bytevector/nonmoving` (`lib/core/foreign.scm`)
- Mutexes, conditions, and the poller are allocated NonMoving because their
  interior addresses are held across blocking waits
  (`runtime/vm/threading.rs`, `runtime/vm/io.rs`)

`Mutation::is_in_nonmoving_space` / `ByteVector::is_nonmoving` test whether an
object resides in a non-movable space.

## Rooting

The GC does not scan the native C/Rust stack. If a `Value` / `Gc` must survive
a safepoint, nest, or `call_in_native`, put it in a root slot and reload with
`.get()` afterwards. Stack copies are stale after a moving collection.

| Intent | API | Storage |
|--------|-----|---------|
| Short-lived across `call_in_native` | `ctx.roots()` (`Scope` / `Pin`), `root_scope!` | Thread `State::root_stack` |
| Nest / SNI locals | `Frame` (`sni::LocalFrame`) | OopStorage `sni_locals` |
| Process-long / cross-callback | `heap::Global`, SNI `Global` | Global registry / OopStorage `sni_globals` |

`State::root_stack` is kept at the **end** of `State` so it does not shift the
Cranelift/FASL offsets of earlier fields. After any `State` layout ABI change,
clean `stage-*` and rebuild.

### Root stack (`Scope` / `Pin`)

`runtime/root.rs` defines `RootStack` (a growable, relocatable `Value` array
traced by `State`), `RootScope` (RAII, truncates on drop), and `Rooted` (a
single slot). `heap::pin` re-exports them as `Scope` / `Pin` plus the
`scope!` macro; `root_scope!` is the crate-level macro.

```rust
let scope = ctx.roots();
let r = scope.root(v);
ctx.call_in_native(|| { /* no Scheme, no GC */ });
let v = r.get();
```

`with_rooted_native(ctx, [values], f)` roots an array, runs `call_in_native`,
and returns the result together with the reloaded values. `rooted_native!`
does the same as a macro.

### OopStorage

`heap/oop_storage/` is a port of HotSpot’s `OopStorage`: off-heap blocks of
precise `Value` slots. Clients allocate a slot, store a value, and keep an
`OopHandle` (`*mut Value`). GC walks allocated slots as relocatable
`ObjectSlot` roots from `scan_vm_specific_roots`. Unused or released slots are
empty (`Value::empty()`).

`OopStorageSet` holds the process storages: `VM Global`, `SNI Locals`, and
`SNI Globals`. SNI local refs live in `sni_locals` and are released when a
`LocalFrame` drops; SNI globals live in `sni_globals` until deleted
(`runtime/sni.rs`). `HandleScope` releases a batch of handles on drop.

### `heap::Global`

`Global<R>` (`heap/global.rs`) is a reference-counted, process-long root in a
global registry scanned as a VM root. Dropping the last `Global` unregisters
it. Access the value with `fetch(ctx)`. (SNI’s `Global` is different: no
`Drop`; you must call `delete`.)

## Safepoints and `gc_save`

Compiled Scheme code parks live ABI values in `State::gc_save`
(rator + argc + 4 argument slots) before a yieldpoint, then calls the
`yieldpoint_block` thunk (`runtime/vm/thunks/gc.rs`), and reloads the slots
afterward. GC traces `gc_save` as part of `State` and relocates those slots in
place — they are not pinned and do not use `pin_root`. Overflow arguments are
already rooted by the runstack.

Native code outside compiled safepoints should poll `take_yieldpoint()` and
call `Mutator::yieldpoint` / `Thread::yieldpoint` when a GC request is
pending. Thread states (`heap/sync/thread.rs`): `Mutating`, `InNative`,
`MutatingToBlock`, `BlockedInNative`, `New`, `Terminated`.

When `nest_level > 0`, Scheme may GC at yieldpoints. Nesting is how native
code re-enters Scheme (`runtime::sni`, or unsafe `call_scheme`). Safe
`#[scheme]` leaves must not raise nest or collect; use `#[scheme(unsafe)]` +
`sni::call_function` (or CPS `return_call`) when you need to nest.

## InNative

`ctx.call_in_native(|| …)` transitions the thread to `InNative`: blocking FFI,
no Scheme, no GC safepoints (alias: `outside_gc_world`). Prepare anything that
needs `Context` before entering; the closure must not capture unrooted GC
handles. Root values you need after return and reload with `.get()`.

## Write barriers

Interior mutation of GC objects goes through `Gc::write` / `Write` /
`Gc::unlock` (`heap/barrier.rs`) so the barrier for the live plan runs.
`field!` projects a field out of `&Write<T>`; `unlock!` combines projection
with `Unlock`. Compiled code calls the barrier thunks in
`runtime/vm/thunks/gc.rs` (`pre_write_barrier_at_slot`,
`post_write_barrier_at_slot`, `post_write_barrier_slow`).

Artifact kinds (stdlib FASL is built and cached per kind; the bucket is
computed in `runtime/vm/load/paths.rs`):

| Kind | Plans |
|------|-------|
| `nobarrier` | MarkSweep, SemiSpace, Immix |
| `objbarrier` | StickyImmix, GenImmix, GenCopy |
| `satbbarrier` | ConcurrentImmix |

The barrier kind codegen emits is chosen by `compile_barrier` in
`heap/plans.rs`: explicit override, else `CAPY_BARRIER_KIND`, else the live
plan. `capyc --barrier KIND` sets the override (via the Scheme primitive
`compile-barrier-kind!`; `compile-barrier-kind` reads it back). See
`lib/boot/cli.scm` and `runtime/vm/base.rs`.

## Plans

Allowed MMTk plans (`ALLOWED_GC_PLAN_NAMES` in `heap/plans.rs`):
StickyImmix, ConcurrentImmix, MarkSweep, Immix, SemiSpace, GenImmix, GenCopy.
The plan is selected with `--gc-plan` (default StickyImmix). An unsupported
plan falls back to StickyImmix at init (`heap/mod.rs`, `heap/logging.rs`).

## Heuristics and tuning

The GC trigger policy is Capy’s own (`heap/heuristics.rs` +
`heap/heuristics/{adaptive,aggressive,compact}.rs`), installed through the
MMTk `create_gc_trigger` hook. When `MMTK_GC_TRIGGER` is unset the trigger is
`Delegated` and Capy decides. Key env vars:

| Variable | Meaning | Default |
|----------|---------|---------|
| `CAPY_GC_HEURISTIC` | `adaptive`, `static`, `compact`, `aggressive`, `passive` | `adaptive` |
| `CAPY_GC_MAX_HEAP` | Max heap size (`2G`, `512M`, …) | 2 GiB |
| `CAPY_GC_MIN_FREE_PERCENT` | Min free heap threshold | 10 |
| `CAPY_GC_INIT_FREE_PERCENT` | Initial free heap threshold | 70 |
| `CAPY_GC_GUARANTEED_INTERVAL_MS` | Force a GC at least this often (0 = off) | 5 min |
| `MMTK_GC_TRIGGER` | MMTk trigger override (disables Capy heuristics) | unset |
| `CAPY_BARRIER_KIND` | Compile-time barrier kind override | unset |
| `RUST_LOG` | Log filter; `mmtk` modules are off unless named | unset |

`--log-trace` (before `--`) enables GC trace logging and per-collection
summary lines on stderr. CLI flags `--gc-plan`, `--gc-trigger`,
`--gc-max-heap`, `--gc-heuristic`, `--gc-min-free-percent`,
`--gc-init-free-percent` map onto these (`lib/boot/cli.scm`).

## Marking constraints

Pluggable roots and outgrowths live in `heap/constraints/` (JSC-style marking
constraints adapted to MMTk). Core constraints (OopStorage strong, global
registry, weak refs, finalizers) register at GC init via
`add_core_constraints`. Add custom ones with
`GarbageCollector::add_marking_constraint` / `heap::add_marking_constraint`;
adding panics during an active collection.

| Volatility | Meaning |
|------------|---------|
| `GreyedByExecution` | Classic roots: GC start and after mutator resumes |
| `GreyedByMarking` | Outgrowths: may grey more as objects are marked |
| `SeldomGreyed` | Late tie-breaker |

`GreyedByExecution` work runs during Prepare / mutator root scan.
`GreyedByMarking` convergence runs in MMTk’s `VMRefClosure` stage (from
`Scanning::process_weak_refs`); parallel constraints are sharded to GC workers
with `add_work_packets` (`constraints/solver.rs`). The set iterates until no
constraint greys new work.

## Weak refs, ephemerons, finalizers

`Weak<'gc, T>` (`heap/weak.rs`) is cleared by GC when the referent dies;
`upgrade` applies the weak-reference load barrier. `Ephemeron<K, V>` clears
its value when the key dies. Both register for weak processing during tracing
(`Visitor::register_for_weak_processing`); objects with weaks are revisited in
`process_weak_refs` via `WeakProcessor`. Weak callbacks
(`GarbageCollector::add_weak_callback`) run each collection.

Finalizers (`heap/finalizer.rs`): objects are registered with a
`FinalizerQueue`; after marking, unreachable candidates are resurrected and
queued, and queues are scheduled from MMTk’s `schedule_finalization` hook
(`collection.rs`). `make_finalizer_queue!` defines a queue type.

## Tracing

Types that hold GC pointers implement `Trace` (`heap/traits.rs`) —
`#[derive(Trace)]` from `capy_derive`, or hand-written. `trace` must visit
every GC-reachable field; mutation during tracing is UB. Scanning walks object
edges via `Visitor` (`heap/collection.rs`) over `ObjectSlot`s. Roots come from
mutator thread `State` (`gc_save`, `call_data`, runstack, `root_stack`,
shadow stack, thread object, …), OopStorage, and the global registry
(`heap/scanning.rs`).

Object layout (`heap/object.rs`): each object has a `HeapObjectHeader` 8 bytes
before the object reference (`OBJECT_REF_OFFSET`), carrying a class ID —
built-in IDs under `builtin_class_ids`, dynamically registered IDs above —
plus hash bits. `AllocationHooks` per class provide trace / weak / size
functions. 