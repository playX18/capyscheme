# capy-derive CPS `block!` Design

## Summary

Add a `#[scheme(cps)]` mode to `capy-derive` for native Scheme procedures. The mode lets runtime functions be written in direct style with explicit suspension points:

```rust
let result = block!(pin poller; {
    // blocking Rust code
});

// continuation code
```

The macro lowers each `block!` suspension point to native continuation-passing style. Code before a `block!` runs immediately. Code after it is moved into generated native continuation code. The blocking body runs through Capy's existing VM blocking-operation path, so the mutator can leave the runtime while the operation waits and GC can still see the saved roots.

The first target is native Scheme procedures only, not a general Rust async replacement. The design directly addresses the current manual pattern in `capy/src/runtime/vm/io.rs` around `poller_wait`, where hand-written continuation and blocking-operation glue is too awkward to scale.

## Goals

- Support `#[scheme(cps)]` native Scheme procedures in `capy-derive`.
- Support multiple `block!` suspension points in one function.
- Use `block!(pin a, b; { ... })` for explicit pinned GC captures used by blocking code.
- Preserve normal Rust-looking code after the block: `block!` evaluates to the Rust value produced by the blocking body.
- Make all locals that survive suspension explicit and GC-traceable in generated code.
- Forbid ordinary Rust borrows across suspension points.
- Keep runtime changes focused on ergonomic helpers for generated code, not a broad scheduler rewrite.

## Non-Goals

- Do not implement a general Rust `async`/`await` runtime.
- Do not use real Rust `.await` syntax.
- Do not infer hidden pinning of GC values in blocking bodies.
- Do not expose generated block-result payloads as Scheme objects.
- Do not make `block!` impose `Result` or exception semantics.

## Surface Syntax

The macro is enabled with `cps` in the existing `#[scheme]` attribute:

```rust
#[scheme(cps, name = "poller-wait")]
pub fn poller_wait(poller: Gc<'gc, Poller>, timeout: Option<u64>) -> Value<'gc> {
    let result = block!(pin poller; {
        // blocking operation
    });

    // continuation
    nctx.return_(result)
}
```

`block!` syntax:

```rust
block!(pin a, b, c; {
    blocking_expr
})
```

The `pin` list is required. Names in the pin list are values that may be used by blocking code while the mutator is blocked. The generated blocking operation traces and pins those roots according to the runtime's existing plan-dependent pinning rules.

If a blocking section needs no pinned values, it still uses an explicit empty header:

```rust
block!(pin; {
    std::thread::sleep(duration)
})
```

The blocking body runs outside the active mutator context. It must not use `ctx`, `nctx`, or allocate into the GC heap. Any heap allocation or Scheme error construction happens after resume in the generated continuation.

The block body returns a plain Rust value. If the body returns `Result<T, E>`, the continuation receives that `Result<T, E>` and user code handles it explicitly.

## Lowering Model

For each `block!`, `capy-derive` generates:

- A blocking operation struct, for example `PollerWait__Block0`.
- A GC-only result payload type, for example `PollerWait__Block0Result`.
- A generated native continuation, for example `poller_wait__k0`.

The generated blocking operation implements `BlockingOperationWithReturn`. Its `prepare` method captures the data needed by the blocking body, runs the body outside the mutator, and returns an after-blocking callback. The after-blocking callback allocates the result payload on the GC heap and pushes an opaque pointer value as the argument used to resume the generated continuation.

Rough generated shape:

```rust
#[derive(Trace)]
#[collect(no_drop)]
struct PollerWait__Block0Result<'gc> {
    value: PollerWaitBlockReturnType,
    _gc: PhantomData<&'gc ()>,
}

struct PollerWait__Block0<'gc> {
    poller: Gc<'gc, Poller>,
    timeout: Option<u64>,
}

impl<'gc> BlockingOperationWithReturn<'gc> for PollerWait__Block0<'gc> {
    fn prepare(&self, ctx: Context<'gc>) -> Box<dyn FnOnce() -> AfterBlockingOperationCallback> {
        // prepare raw inputs and pinned roots
        Box::new(move || {
            let value = /* blocking body */;

            Box::new(move |ctx, args| {
                let result = Gc::new(*ctx, PollerWait__Block0Result { value, _gc: PhantomData });
                args.push(Value::from_raw(Gc::as_ptr(result) as u64));
            })
        })
    }
}
```

The generated continuation receives the opaque `Value<'gc>` and converts it back to a typed GC pointer with a generated or runtime helper:

```rust
let block_result: Gc<'gc, PollerWait__Block0Result<'gc>> =
    unsafe { Gc::from_ptr(block_result.bits() as *const _) };
```

This payload is allocated on the GC heap and has a GC header/vtable, but it is not a Scheme object implementing `Tagged`. It must not be unpacked through `Value::downcast::<T>()`, pattern matched by Scheme code, or exposed as a meaningful Scheme value.

## Capture Rules

For each suspension point, the macro computes three sets:

- Pinned inputs: exactly the names listed in `pin ...`. These may be GC values used by blocking code.
- Prepared inputs: non-pinned owned locals referenced by the blocking body. These are copied or moved into the generated blocking operation when allowed by Rust ownership.
- Continuation captures: locals needed by code after the block. These are stored in generated continuation free variables or a generated GC-only environment object.

Ordinary Rust borrows may not cross a `block!`. A borrow may exist before the block or entirely inside the blocking body, but not survive into code after resume. Locals that do survive suspension must be owned values or GC values whose containers implement `Trace`.

If a continuation capture or result payload field does not implement `Trace`, the expanded code should fail with a clear diagnostic where possible. For types that are intentionally non-GC data, existing `Trace` implementations for primitives and other non-GC types can be used.

## Control Flow

The design target is full-ish Rust control flow within `#[scheme(cps)]` functions:

- `let` and assignments
- nested blocks
- `if`
- `match`
- loops
- early `return`
- `break` and `continue`
- multiple sequential or nested `block!` suspension points

The macro may stage implementation support, but unsupported control-flow shapes must produce explicit compile errors rather than surprising runtime behavior.

The transform treats code after each `block!` as the continuation of that suspension point. If control flow can reach different subsequent points, the macro generates the required continuation fragments and captures only the locals live in each fragment.

## Error Handling

`block!` has no built-in error semantics. It returns whatever the blocking expression evaluates to.

Examples:

```rust
let rc = block!(pin sock, bv; {
    unsafe { libc::send(sock.fd, bv.as_ptr() as _, bv.len(), flags) }
});

if rc < 0 {
    return nctx.raise_io_error(...);
}

nctx.return_(rc)
```

```rust
let result = block!(pin poller; {
    wait_for_events(poller, timeout)
});

match result {
    Ok(events) => nctx.return_(events_to_list(ctx, events)),
    Err(err) => nctx.raise_io_error(...),
}
```

Runtime errors remain user code after resume, where `ctx` and `nctx` are available.

## Runtime Helpers

Add small helpers in the VM/runtime layer so generated code does not depend directly on low-level saved-call details.

Expected helpers:

- A helper around `NativeCallContext::perform_returning_to` for "run this operation and resume this continuation".
- A helper for packing and unpacking opaque GC-only continuation payload pointers through `Value`.
- Optional helper traits for generated blocking operations that pin named GC roots consistently with existing `SendOperation` and `FFICallOperation` behavior.

The runtime refactor should stay focused. Do not redesign scheduling or the blocking-operation loop unless implementation exposes a concrete duplication problem.

## Diagnostics

The macro should emit clear compile errors for:

- `block!` outside `#[scheme(cps)]`.
- `#[scheme(cps)]` on unsupported items.
- Missing `pin ...` header.
- GC-looking values referenced inside a blocking body but not listed in `pin`.
- `ctx`, `nctx`, or other active-mutator-only APIs referenced inside a blocking body.
- Ordinary Rust borrows live across a suspension point.
- Non-`Trace` continuation captures or result payload fields, when diagnosable by the macro.
- Attempts to bind or expose the internal opaque block-result argument in user-visible code.
- Unsupported control-flow shapes in the current implementation stage.

Diagnostics should point at the source `block!` or capture where possible, not only at generated code.

## Testing Plan

Macro-focused tests:

- One `block!` in a native Scheme function.
- Two sequential `block!` points.
- `block!` inside `if` and `match`.
- `block!` in a loop, including `break` and `continue` paths if implemented in the first stage.
- Missing `pin` list rejection.
- Borrow-across-suspension rejection.
- Non-`Trace` capture rejection where practical.

Runtime-focused tests:

- A simple generated blocking operation that returns a primitive value.
- A generated blocking operation returning a GC-traced payload.
- A function with two suspensions that resumes in order.
- A converted `poller_wait` integration test using `(capy polling)`.

Verification commands:

```sh
cargo check -p capy-derive
cargo check -p capy
```

Add the narrowest available runtime test command for polling once the integration test exists.

## Rollout

1. Add the `#[scheme(cps)]` parser path and `block!` marker recognition in `capy-derive`.
2. Add runtime helpers for opaque GC payload packing/unpacking and blocking-operation resume.
3. Implement one-block lowering.
4. Extend lowering to multiple blocks and structured control flow.
5. Convert `poller_wait` as the integration proof.
6. Broaden diagnostics and tests around borrow/capture rejection.

The feature should remain internal to Capy's native runtime code until the transform has enough coverage and diagnostics to be comfortable for broader use.
