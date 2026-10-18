# JIT

The JIT lives in `crates/capy/src/runtime/codegen/`. It turns a bytecode
procedure into machine code, one image per procedure, using the same
templates the template interpreter uses. The code is generated at run time
with `onlineasm`. 

This file covers the JIT pipeline.


## Pipeline

The JIT compiles one procedure at a time, straight from its bytecode:

1. `build_image` (`jit/compile.rs`) decodes each instruction of the
   `CodeBuffer`.
2. `CompiledContext` (`jit/context.rs`) turns operands into immediates and
   sends control flow through `Targets`.
3. `emit_template` (`template/emit/`) emits one template per opcode into an
   `Emission` (`emission.rs`). The `Emission` collects the deferred slow
   blocks, relocations and pending lazy cells.
4. `emit_deferred`, `finish` and `Emission::link` load the code through
   `CodeMemory` and resolve the relocations. The result is a `BuiltUnit`.
5. `publish` (`jit/compile.rs`) sets the owner slot, registers the unit and
   builds the `ResumeTable`. The finished `CodeUnit` (`unit.rs`) is held
   inline by a heap `CodeImage`.

Compiles run one of two ways. With `CAPY_JIT_WORKERS=0`, `compile_with`
builds and publishes on the requesting mutator. Otherwise the request goes
to a worker (`jit/worker.rs`, `submit`). The worker runs `TierCompiler::build`.
A mutator later calls `try_install`,
which installs the finished unit through `publish`. Only whole-procedure
compiles go through the worker. Lazy hits always materialize on the hitting
mutator.

Machine code is allocated through `CodeMemory` (`runtime/code_memory.rs`),
which wraps asmkit's `JitAllocator`. Patching live code goes through the same
type.

## Template contexts

A template is generic over `TemplateContext`, which is a union of
four traits (`template/mod.rs`):

- `Operands`: where values come from (registers, constants, metadata).
- `Flow`: `next`, `jump`, `branch_if`, calls and returns.
- `Slow`: where slow blocks go (`control`, `slow_entry`, `slow_site`).
- `Speculation`: codegen-time decisions. `arith_site` returns an `ArithSpec`,
  `profiling` and `link_checks` say whether to emit profile and link code,
  and `known`/`refine` are the LBBV type hooks (no-ops for now).

The template interpreter (`interp/template_interp.rs`) implements the four
traits directly. The JIT uses `CompiledContext<'e, T: Targets, S: SpecSource>`
(`jit/context.rs`), which implements them through two parameters:

| Context | `Targets` | `SpecSource` | Used for |
|---|---|---|---|
| `JitTemplateContext` | `ProcTargets` (label per pc, deferred slows) | `ProfileSpec` (live profile) | the baseline procedure |
| `StubContext` | `OriginExits` (absolute addresses via the origin resume table, inline slows) | `FixedSpec` (the shape being generated) | arithmetic stubs |

`Targets::target` maps a bytecode pc to a `Target`: `Label` (inside the
image being built), `Absolute` (a known machine address), or `Edge` (a
label of a lazy edge). `Targets::placement` returns `Deferred` (slow blocks
at the end, so fast paths fall through) or `Inline`. 

## Lazy edges

A lazy edge is a label that generated code jumps to when a fast path fails
or a block has not been compiled yet. Behind it sits a cell, which holds the
behaviour of the edge (a `Lazy`) and the address the edge jumps through.

### The `Lazy` trait

`lazy/mod.rs` defines:

```rust
pub(crate) trait Lazy: Sized + Send + Sync + 'static {
    const NAME: &'static str;
    fn pc(&self) -> usize;                 // bytecode offset, for logs
    fn ip(&self) -> Ip;                    // bytecode address the hit stages in PC
    fn log_context(&self) -> String { String::new() }
    fn hit<'gc>(&self, cx: &mut HitCx<'gc>) -> *const u8 {
        cx.materialize_and_enter(self)     // default
    }
    fn materialize<'gc>(&self, cx: &mut MaterializeCx<'gc>) -> Materialize;
}
```

`materialize` runs under the cell's lock, with the cell in state `Slow` or
`Specialized`. It returns one of:

- `Materialize::Code { again }`: code was installed with
  `MaterializeCx::install`. `again: true` moves the cell to `Specialized`,
  where a miss in the new code comes back to the same cell. `again: false`
  moves it to `Full`, where a miss is a plain call into Rust.
- `Materialize::Sticky(reason)`: never generate for this cell. The cell
  moves to `Sticky`.
- `Materialize::Later`: a budget or a transient condition. The cell stays as
  it is, and the next hit tries again.

`MaterializeCx::fail(err)` logs the error and makes the cell sticky.

The default `hit` calls `materialize_and_enter`, which continues at the new
code. It panics if `materialize` did not leave the cell `Specialized` or
`Full`. A lazy whose `materialize` can return `Sticky` or `Later` must
override `hit` and continue somewhere else. `ArithSite` does this: it runs
the generic operation, materializes, and resumes past the instruction.

### Cell states

`CellState` (`lazy/mod.rs`):

- `Slow`: the hit block, nothing generated yet.
- `Specialized`: generated code that still misses back to the cell.
- `Full`: terminal code. A miss is a plain call into Rust.
- `Sticky`: the hit block, forever.

Hits are never generated on a worker. A frame is waiting on them.

A cell moves from `Slow` to `Specialized` to `Full` through `materialize`.
It can move to `Sticky` from any of them. Misses from `Specialized` code
return to the cell. A miss from `Full` code is a call to Rust.

### Declaring an edge

`Lazies` declares an edge. It has one method for an existing label and one
that makes the label:

```rust
pub(crate) trait Lazies {
    fn edge_at<T: Lazy>(&mut self, masm: &mut Masm, label: Label, kind: EdgeKind, value: T);
    fn edge<T: Lazy>(&mut self, masm: &mut Masm, kind: EdgeKind, value: T) -> Label;
}
```

It is implemented twice:

- `Emission` (before load): the cell is pending and its address is a
  `Reloc::Cell`, filled in by `Emission::link`.
- `MaterializeCx` (after load): the cell is pushed into the unit's
  `CellArena` now, and its address is baked into the code.

Because of the second impl, code materialized from an edge can declare edges
of its own. A `Targets` implementation can declare an edge too.

### Hits

The hit block (`emit_hit_block`, `lazy/edge.rs`) stores the bytecode address
in `PC`, passes the cell in the second C argument, and calls
`capy_lazy_hit`. That entry rebuilds the mutator from `th`, reads the
cell's vtable, and calls `Lazy::hit` through `HitCx`. `HitCx::materialize`
checks the cell's state, takes the cell's `try_lock`, checks
`LIVE_CODE_BYTES` against `max_code()`, and then calls `materialize`. A
`Code` result stores the new entry in `cell.target`, sets the state, and
calls `resolve_direct`.

## Edge kinds

`EdgeKind` (`lazy/edge.rs`) has two variants.

### Indirect

`move SITE_TMP, cell; jmp [SITE_TMP]`. The jump reads `cell.target`, which
starts at the edge's hit block and is then set to generated code. Repatching
writes data, not code, so any number of threads can be inside the edge while
it changes. Use it for an edge that can be retargeted more than once:
arithmetic sites, inline caches, side exits, and any `Specialized` code.

### Direct

At the edge label there is one patch-aligned `jmp rel32` (x86-64) or `b imm26`
(AArch64) to a local thunk. The thunk is an indirect edge.

When the cell gets its code, `resolve_direct` does two things:

1. It writes `cell.target`. Any thread already in the thunk, or one that
   read the old jump, reaches the new code through the thunk.
2. It asks `direct_patch_policy` whether to rewrite the jump. The policy
   patches only `Full` code, only when the jump field is 4-byte aligned, and
   only when the target is in range: +-2 GB for `jmp rel32`, +-128 MB for
   `b imm26`. The rewrite is one atomic 4-byte store through
   `CodeMemory::patch_live_u32`, which flushes the icache.

An out-of-range target is expected, since allocator blocks can land anywhere.
The jump then stays on the thunk, which is always correct and only slower. A
failed patch logs a warning and keeps the thunk. It never fails the resolve.

A `Direct` edge pays off only if its cell reaches `Full`. A cell that stays
`Specialized` keeps its thunk forever.

## Adding a lazy kind

A lazy kind is one `impl Lazy`. This example is only an illustration:

```rust
pub(crate) struct Example {
    pc: usize,
    ip: Ip,
}

impl Lazy for Example {
    const NAME: &'static str = "example";

    fn pc(&self) -> usize {
        self.pc
    }

    fn ip(&self) -> Ip {
        self.ip
    }

    fn materialize<'gc>(&self, cx: &mut MaterializeCx<'gc>) -> Materialize {
        let mut masm = Masm::new();
        let start = masm.label();
        masm.bind(start);
        // Emit the code for this state. Declare further edges with cx.edge(..).
        onlineasm!(*masm.assembler(); move t0, 1; ret;);
        match cx.install(masm, "example", start) {
            Ok(_) => Materialize::Code { again: false },
            Err(err) => cx.fail(err),
        }
    }
}
```

To declare the edge, call `Lazies::edge` on the `Emission` while emitting
the enclosing code, or on `MaterializeCx` while materializing another lazy:

```rust
let slow = em.edge(masm, EdgeKind::Indirect, Example { pc, ip });
```


## Adding a template context

Two routes, depending on what differs.

If the new context only changes how a pc becomes a target and where slow
blocks go, implement `Targets` and `SpecSource`, then use
`CompiledContext<'e, T, S>`. `ProcTargets` and `OriginExits` are the two
examples. `SpecSource::arith_site` gives the `ArithSpec` for a site.

If the context needs different operand or control handling, implement the
four traits directly, as `InterpreterTemplateContext` does. A template bound by
`C: TemplateContext` then works with the new context without changes.

`Speculation` has default methods for `arith_site`, `profiling`,
`link_checks`, `known` and `refine`. 



## Environment variables

- `CAPY_JIT=0` turns the JIT off. Any other value leaves it on.
- `CAPY_JIT_THRESHOLD=n`: entries or back-edges before a procedure compiles.
  Default 10.
- `CAPY_JIT_WORKERS=n`: tier-up worker threads. Default 1. `0` compiles on the
  requesting thread.
- `CAPY_JIT_TIERS=list`: tiers that may compile, for example
  `baseline,lbbv`. Default `baseline`.
- `CAPY_JIT_LOG=1`: the decision log. The log target is `capy::jit`, and `capy::jit::resume` has the resume trace.
- `CAPY_JIT_LOG_VERBOSE=1`: adds bulky per-procedure detail (bytecode dump,
  full address maps, control-flow trace) to the decision log.
- `CAPY_JIT_LOG_RESUME=1`: log every resume into compiled code.
- `CAPY_JIT_STATS=1`: compile, resume and slow-path counters, printed at exit.
- `CAPY_JIT_DUMP_DIR=path`: write each compiled procedure's listing and ELF.
- `CAPY_JIT_MAX_CODE=bytes|max`: stop tier-up past this much live code.
- `CAPY_JIT_MAX_CODE_PER_BLOCK=bytes|max`: procedures whose image is larger
  stay interpreted.
- `CAPY_JIT_LIMIT=n` compiles only the first `n` procedures
- `CAPY_JIT_FILTER=name` compiles only procedures whose metadata mentions `name`.

