# Objects, Classes and Inline Caches

This file covers how Scheme values look in memory, how classes and their
instances work, and how inline caches keep slot access and generic dispatch
fast.

## Values

A Scheme value is one 64-bit word, `Value<'gc>`
(`crates/capy/src/runtime/value.rs`). It uses NaN boxing. If any of the
high tag bits are set, the word is an immediate. Any other nonzero word is a
pointer to a heap object.

Immediates:

- **Fixnums:** 32-bit signed integers, tagged with `NUMBER_TAG`
  (`0xfffe000000000000`).
- **Flonums:** doubles with `1 << 49` added to their bits. This keeps them
  apart from fixnums and pointers. NaN payloads are canonicalized when a
  flonum is made.
- **Characters:** the Unicode scalar shifted left by 16 bits, tagged `0x82`.
- **Specials:** `#t`, `#f`, `()`, undefined, void, unspecified, EOF, and the
  broken weak pointer. They differ in the low bits under `OTHER_TAG`.
- **Internal sentinels:** empty and deleted, used by hash tables.

```text
 pointer    0000 xxxx xxxx xxxx   (no tag bits set)
 flonum     bits + (1 << 49)
 fixnum     fffe 0000 iiii iiii   (NUMBER_TAG, 32-bit payload)
 other      tagged special values and characters
```

## Heap objects

Every heap object has a GC header word 8 bytes before its payload
(`OBJECT_REF_OFFSET` in `crates/capy/src/heap/object.rs`). A `Value` points
at the payload, so the header is at a negative offset.

The object layout, from low to high address:

- hashcode (optional)
- header: 24-bit class id and hash bits
- payload: the address a `Value` points at

The class id selects a `ClassDescriptor` in the class table
(`runtime/class/`). Built-in ids (pair, vector, string, closure, module,
port, thread, and others) are constants in `builtin_class_ids`.

Object representations live in `runtime/value/`:

| Kind | File |
|------|------|
| pairs, lists | `list.rs` |
| strings | `string.rs` |
| vectors | `vector.rs` |
| symbols | `symbols.rs` |
| numbers (bignum, rational, complex) | `number/` |
| procedures | `proc/` |
| ports | `port.rs` |
| hash tables (HAMT) and weak tables | `hamt.rs` and friends |

### Closures and code blocks

A `Closure` (`value/proc/closure.rs`) holds a machine entry pointer, its
`CodeBlock`, a metadata slot, and an inline array of free variables. A
reified continuation is a closure of another kind.

A closure holds:

- the entry pointer
- the code block
- a metadata slot
- the free variables

A `CodeBlock` (`value/proc/code.rs`) holds:

- the `CodeBuffer` with the bytecode
- the constant pool
- the aux pool with liveness masks
- metadata: profiles and caches
- arity, frame size and source map
- the JIT entry, when compiled

JIT images come from executable memory (`runtime/code_memory.rs`).
`docs/JIT.md` explains who owns them.

## Classes and instances

An instance of a Scheme class (`SchemeInstance`) points at its
`ClassDescriptor` and at a separate slots array.

The slots array is a `SlotStorage`: a layout stamp followed by the slot
values.

### Redefinition

Redefining a class installs a new descriptor under the same class id. An
instance migrates lazily, the next time something touches it. The migration
builds a new slots array and publishes it. It stores the slots first and the
class word second, both with Release.

`SlotStorage` (`class/storage.rs`) carries a layout stamp. Each layout of a
class has its own stamp. A cache entry remembers the stamp it was filled for.
A hit compares the class id of the instance, then the stamp of the storage it
loads from. A cached index is therefore always in bounds, and a migration can
never be mistaken for the old layout.

Redefinition while other threads write to the instances is memory safe. One
race can lose a write. A writer may store into the old array just after the
migrating thread copied from it. Synchronize externally if such writes must
survive a redefinition.

## Slot inline caches

`get_slot` and `put_slot` own a `SlotSite` in their metadata. The site caches
what a slot access resolved to.

Both words point at immutable data. A fill publishes a whole entry with one
CAS. The poly array is replaced copy-on-write. A hit loads the pointer and
trusts the entry.

A site on a class with a custom slot protocol does not cache. It takes the
out-of-line `slot-ref` call, which dispatches through the table of the
generic.

### Invalidation

Each class owns a watchpoint set for its slot protocol.

A fill allocates the entry, registers the site in the class set, publishes
the entry, then rechecks the set. When the set fires, every registered site
is cleared.

The recheck after publishing closes the race with a set that fires in
between. These events fire the sets of the classes they affect, and only
those:

- A class redefinition fires the set of the old descriptor.
- Adding or replacing a method of `slot-ref-using-class` (or `-set!`,
  `-bound?`) fires the sets of the classes the method can apply to. They are
  found from the specializers of the method.

### Grace period

Changes that alter behaviour (`add-method!`, `set-generic-fallback!`,
`define-generic`, class redefinition) end with a soft handshake with every
mutator thread (`heap/sync/handshake.rs`). When it returns, no thread still
runs code that read an old cache entry. The next call on every thread sees
the new method.

## Generic dispatch

Generic functions cache their applicable methods in an open-addressed table
of immutable buckets. The table is replaced copy-on-write and published by
CAS.

A call `(f a b c)` builds a key from the class ids of the first *width*
arguments and the argument count (24 bits per class id, 16 bits for the
count, packed into one word). The table is probed with that key. A hit gives
the applicable methods. A miss computes them and inserts them. The table
holds up to 4096 keys.

The key holds the classes of the first *width* arguments, where width is one
more than the highest specialized position. It also holds the argument
count, capped at the highest count any method distinguishes. Variadic calls
therefore share an entry. Past 4096 keys the table stops growing and dispatch
recomputes methods for new keys.
