# Heap

Capy’s garbage collector lives in `crates/capy/src/heap/`. It is precise (no
conservative native-stack scan) and driven by MMTk via the `MemoryManager`
binding in `heap/mm.rs`.

If you are writing mutator code (runtime, VM, natives), you almost always want
`runtime::MutationContext`. Do not pass `Mutation` around at call sites. There is no
public `Heap` capability type. 

## MutationContext

`MutationContext<'gc>` (`runtime/thread.rs`) wraps the per-thread `Mutation`. Use it for:

- allocation (`ctx.alloc`, `ctx.alloc_in`)
- rooting (`ctx.roots()`, `root_scope!(ctx, scope)` / `scope!(ctx, scope)`)
- barriers (via `Gc::write`, `Gc::unlock`, `field!`)
- leaving the mutator (`ctx.call_in_native`)

```rust
fn example(ctx: MutationContext<'_>) {
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

`MutationContext` derefs to `Mutation`, so low-level `Mutation` APIs are reachable
through it. `Mutation` itself is `#[doc(hidden)]`. It still exists for MMTk
plumbing, derives, and `Rootable`. Prefer `MutationContext` everywhere else.

## Allocation

`ctx.alloc(T)` puts `T` in the default space (movable nursery / immix,
depending on plan). `ctx.alloc_in(space, T)` selects:

| Space | Use |
|-------|-----|
| `Space::Default` | Normal Scheme objects (may move) |
| `Space::NonMoving` | Addresses exposed to C / FFI interiors |
| `Space::Los` | Large objects |

The default-space fast path bumps a per-thread TLAB
(`heap/lab.rs`). Slow paths flush and refill it around MMTk calls.

Allocation uses `at_safepoint: false`: it may **schedule** a collection but
must not block, collect, or run yieldpoints inline. Safe `#[scheme]` leaves
and interpreter and JIT alloc sites rely on this. The mutator pauses only at
yieldpoints, while `InNative`, or inside a nested Scheme call. When a GC is
pending, schedule it. Do not defer it forever.

### FFI and NonMoving

Anything whose raw address escapes into C must not move:

- `bytevector->pointer` copies a movable owned bytevector into NonMoving
  before it returns a pointer. An ephemeron keeps the bytevector alive while
  the pointer is live (`runtime/vm/ffi.rs`)
- `string->utf8/nul` allocates NonMoving. Plain `string->utf8` is movable
  (`runtime/vm/strings.rs`)
- `string->pointer` builds a NUL-terminated bytevector via
  `make-bytevector/nonmoving`
- Mutexes, conditions, and the poller are allocated in `Los` because their
  interior addresses are held across blocking waits

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

`State::root_stack` is a Rust-side structure: generated code does not address
`State` fields. Machine-visible per-thread fields live in `Tc` (offsets in
`runtime/sabi.rs`, checked with `offset_of!`), so changing `State` does not
change any ABI that the interpreter or JIT depends on.

### Root stack (`Scope` / `Pin`)

`runtime/root.rs` defines three types. `RootStack` is a growable `Value`
array traced by `State`, and the collector may update its slots when objects
move. `RootScope` is RAII and truncates the stack on drop. `Rooted` is a
single slot. `heap::pin` re-exports them as `Scope` / `Pin` plus the
`scope!` macro. `root_scope!` is the crate-level macro.

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
`OopHandle` (`*mut Value`). GC walks allocated slots as
`ObjectSlot` roots from `scan_vm_specific_roots`. Unused or released slots are
empty (`Value::empty()`).

`OopStorageSet` holds the process storages: `VM Global`, `SNI Locals`, and
`SNI Globals`. SNI local refs live in `sni_locals` and are released when a
`LocalFrame` drops. SNI globals live in `sni_globals` until deleted
(`runtime/sni.rs`). `HandleScope` releases a batch of handles on drop.

### `heap::Global`

`Global<R>` is a reference-counted, process-long root in a
global registry scanned as a VM root. Dropping the last `Global` unregisters
it. Access the value with `fetch(ctx)`. (SNI’s `Global` is different: no
`Drop`, so you must call `delete`.)

## Safepoints and stack scanning

Scheme frames live on the Scheme stack in `Tc`, not on the native stack. At
a yieldpoint the engine's state is already in the frame: live values are
stack slots. The collector traces `Tc` and walks the stack frame by frame.
Each return address has an `RpHeader` (`runtime/sabi.rs`) giving the frame
size and a mask of live slots. For bytecode frames the mask comes from
the `return_point` instruction, and `bytecode/verify.rs` checks that masks stay
inside the pool and the frame. Slots outside the mask, including the reserved
`r[base-1]`, are not read. Live slots are relocated in place when objects move.

Yieldpoints are taken at `enter` and `loop_hint`, in JIT code too. A thread
enters one when its yieldpoint flag is set or `esp` has been set to null. The
slow path then calls the `yieldpoint_block` thunk (`runtime/vm/thunks/gc.rs`).

Native code outside Scheme safepoints should poll `take_yieldpoint()` and
call `Mutator::yieldpoint` / `Thread::yieldpoint` when a GC request is
pending. 

When `nest_level > 0`, Scheme may GC at yieldpoints. Nesting is how native
code re-enters Scheme (`runtime::sni`, or unsafe `call_scheme`). Safe
`#[scheme]` leaves must not raise nest or collect. Use `#[scheme(unsafe)]` and
`sni::call_function` (or CPS `return_call`) when you need to nest.

## InNative

`ctx.call_in_native(|| …)` transitions the thread to `InNative`: blocking FFI,
no Scheme, and no GC safepoints. Prepare anything that
needs `MutationContext` before entering. The closure must not capture unrooted GC
handles. Root values you need after return and reload with `.get()`.

## Write barriers

Interior mutation of GC objects goes through `Gc::write` / `Write` /
`Gc::unlock` so the barrier for the live plan runs.
`field!` projects a field out of `&Write<T>`. `unlock!` combines projection
with `Unlock`. 

Barrier kinds by plan:

| Kind | Plans |
|------|-------|
| `nobarrier` | MarkSweep, SemiSpace, Immix |
| `objbarrier` | StickyImmix, GenImmix, GenCopy |
| `satbbarrier` | ConcurrentImmix |

## Plans

Allowed MMTk plans:
StickyImmix, ConcurrentImmix, MarkSweep, Immix, SemiSpace, GenImmix, GenCopy.
The plan is selected with `--gc-plan` (default ConcurrentImmix). An unsupported
plan also falls back to ConcurrentImmix.

## Heuristics and tuning

Capy decides when a collection starts, how large the heap limit is, and whether
allocating threads are paced. MMTk still performs the collection. The code is in
`crates/capy/src/heap/heuristics/`. `mod.rs` holds the presets and the trigger
dispatch.

Three core options:

- Trigger: when a collection starts, by default `predictive` heuristic
- Heap sizing: how heap resizes after a GC cycle, by default `free-target`
- Pacing of mutators: whether mutators are paced to allow concurrent marking to finish. By default is off.

### Defaults

- Plan: `ConcurrentImmix`, unless `MMTK_PLAN` is set to any other plan.
- Max heap: `--gc-max-heap` if set. Otherwise MMTk's default, which is half of
  physical memory. 
- Initial heap: available memory / 32, clamped to [32 MiB, max / 4].
  `--gc-init-heap=0` forces heap to be at max size.
- The first five cycles (`--gc-learning-steps`) trigger at 50% free space.
- A collection is forced at least every 5 minutes (`--gc-guaranteed-interval-ms`).

### Presets

| Preset | Trigger | Sizing | Pacing | Use when |
|--------|---------|--------|--------|----------|
| `predictive` (default) | predictive | free-target | off | General use |
| `generational` | generational | free-target | off | Only with a generational plan (`MMTK_PLAN=GenImmix`, `GenCopy`, `StickyImmix`). Otherwise falls back to `predictive` |
| `runway` | runway | free-target | off | Steady allocators, ZGC-style |
| `goal` | goal | free-target | off | Concurrent plans, with a heap-size goal |
| `proportional` | proportional | free-target | off | Simple; triggers on live data |
| `floor` | floor | free-target | off | Least GC work, at the cost of late collections and larger pauses |
| `eager` | eager | free-target | off | GC stress tests |
| `deferred` | deferred | free-target | off | Only the plan decides when to collect |
| `banded` | deferred | banded | off | Memory-bounded runs with a steady live set |
| `overhead` | deferred | overhead | off | Keeping GC CPU near 15% |
| `balanced` | deferred | balanced | off | Footprint-aware general use |
| `concurrent` | goal | balanced | tax | Lower latency under allocation pressure |

### Triggers

- **predictive**: starts when the predicted allocation during the expected GC
  time exceeds usable free space. It uses GC-time history, the mean and spread of
  the allocation rate, and short spikes. Ported from Shenandoah. Its parameters
  are adjusted after each cycle from the free space that cycle left.
- **generational**: predictive for young cycles. An old cycle starts when old
  usage grows past a threshold.
- **runway**: starts when free space would run out before a worst-case GC
  finishes (ZGC's director rule). Allocation is sampled every 100 ms. The first
  three cycles use fixed points, at 10%, 20% and 30% of the heap allocated.
- **goal**: starts marking so that it finishes as the heap reaches
  `live × 2` (growth 100%). Ported from Go's pacer.
- **proportional**: starts after allocating `live / 3`, bounded to [8 MiB, half
  the heap]. It also has a 30 s guaranteed interval.
- **floor / eager / deferred**: min-free and interval only / every evaluation /
  the plan alone.

### Sizing

Sizing sets the heap limit:

- **free-target**: after each collection, the target is `used / (1 − 75%)`. The
  heap grows at once, and shrinks only after three cycles under half its size.
  With the default, the heap settles at about four times live data.
- **banded**: after full-heap pauses, keeps used memory between 1.2× and 1.65×
  live.
- **overhead**: after full-heap pauses, adjusts the size toward 15% GC CPU.
- **balanced**: after every collection, sizes headroom from the allocation rate
  and the collection speed (MemBalancer).

### Pacing

`tax` works only under a concurrent plan. It arms when concurrent marking starts
and disarms when marking ends. While armed, each allocation slow path is charged
against an allowance. A thread over its allowance sleeps with backoff for at most
10 ms, then proceeds. Stalls are bounded, and the cost is some throughput under
heavy allocation.

### Flags

| Flag / env | Default | Effect |
|------------|---------|--------|
| `--gc-heuristic` / `CAPY_GC_HEURISTIC` | `predictive` | Preset |
| `--gc-trigger-policy` | from preset | Overrides the trigger |
| `--gc-sizing` | from preset | Overrides the sizing |
| `--gc-pacing` | from preset | `off` or `tax` |
| `--gc-plan` / `MMTK_PLAN` | `ConcurrentImmix` | MMTk plan |
| `--gc-max-heap` / `CAPY_GC_MAX_HEAP` | MMTk default | Hard max heap |
| `--gc-init-heap` / `CAPY_GC_INIT_HEAP` | auto | Starting limit. `0` means fixed at max |
| `--gc-heap-growth-free-percent` | 75 | Free-target headroom after a GC |
| `--gc-min-free-percent` | 10 | Min-free trigger |
| `--gc-init-free-percent` | 50 | Learning-phase trigger threshold |
| `--gc-guaranteed-interval-ms` | 300000 | Forced interval. `0` disables |
| `--gc-learning-steps` | 5 | Cycles before adaptive triggers are trusted |


### Logging

- `--log-trace` before `--`, or `(gc-logging-enable! #t)`, turns GC logging on.
- `RUST_LOG=capy::gc=info` shows trigger and resize decisions.
- Useful messages: `GC trigger accepted (...)`, `GC cycle finished (...)`,
  `heap resized A -> B`, `GC #N: pause=...`.
- `(gc-statistics)` returns collections, used bytes, bytes reclaimed, GC CPU ms
  and GC real ms.


## Marking constraints

Pluggable roots and outgrowths live in `heap/constraints/`. 
Core constraints (OopStorage strong, global
registry, weak refs, finalizers) register at GC init via
`add_core_constraints`. Add custom ones with
`GarbageCollector::add_marking_constraint` / `heap::add_marking_constraint`.
Adding one panics during an active collection.

| Volatility | Meaning |
|------------|---------|
| `GreyedByExecution` | Classic roots: GC start and after mutator resumes |
| `GreyedByMarking` | Outgrowths: may grey more as objects are marked |
| `SeldomGreyed` | Late tie-breaker |

`GreyedByExecution` work runs during Prepare / mutator root scan.
`GreyedByMarking` convergence runs in MMTk’s `VMRefClosure` stage (from
`Scanning::process_weak_refs`). Parallel constraints are sharded to GC workers
with `add_work_packets`. The set iterates until no constraint greys new work.

## Weak refs, ephemerons, finalizers

`Weak<'gc, T>` is cleared by GC when the referent dies.
`upgrade` applies the weak-reference load barrier. `Ephemeron<K, V>` clears
its value when the key dies. Both register for weak processing during tracing
(`Visitor::register_for_weak_processing`). Objects with weak refs are revisited in
`process_weak_refs` via `WeakProcessor`. Weak callbacks
(`GarbageCollector::add_weak_callback`) run each collection.

Finalizers: objects are registered with a
`FinalizerQueue`. After marking, unreachable candidates are resurrected and
queued, and queues are scheduled from MMTk’s `schedule_finalization` hook. 
`make_finalizer_queue!` defines a queue type.

## Tracing

Types that hold GC pointers implement `Trace`, either
with `#[derive(Trace)]` from `capy_derive` or by hand. `trace` must visit
every GC-reachable field. Mutation during tracing is UB. Scanning walks object
edges via `Visitor` over `ObjectSlot`s. Roots come from
mutator thread `State` and `Tc` (the Scheme stack, `root_stack`,
continuation marks, thread object, …), OopStorage, and the global registry.

Object layout: each object has a `HeapObjectHeader` 8 bytes
before the object reference (`OBJECT_REF_OFFSET`), carrying a class ID -
built-in IDs under `builtin_class_ids`, dynamically registered IDs above -
plus hash bits. `AllocationHooks` per class provide trace / weak / size
functions. 