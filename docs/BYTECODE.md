# Bytecode

Compiled Scheme code is bytecode. It is run by two engines: Rust or template interpreter. Template interpreter
in addition can also trigger Baseline JIT compilation of hot code blocks.

## Instruction format

An instruction is an opcode byte followed by typed operands (`operand.rs`):

| Type | Meaning |
|------|---------|
| `Register(u32)` | frame slot `fp[n]` |
| `Constant(u32)` | index into the constant pool of the code block |
| `Immediate(i32)` | signed immediate |
| `Count(u32)` | unsigned count, size, index, or small enum |
| `PrimitiveId(u32)` | `Primitive::id()` |
| `PoolReference(u32)` | word index into the aux pool of the code block |

Operands are one byte each by default. If one does not fit, the instruction
gets a prefix and all its operands widen:

```text
 narrow   [op][a][b][c]
 wide16   [WIDE16_PREFIX 3B][op][a:2][b:2][c:2]
 wide32   [WIDE32_PREFIX 5B][op][a:4][b:4][c:4]
```

### Metadata

Some instructions own a metadata slot. It is a mutable record in a flat area
of the code block. Slots hold value profiles, arithmetic profiles, inline
caches, and call link info. An instruction names its slot with a
`MetadataId`.

### Compatibility

While our bytecode is not stable, we try to keep it somewhat stable. All opcodes should get `last_opcode + 1` IDs, and not 
override existing ones. The FASL files also additionally keep `format_hash()` of opcode set. This allows us to quickly distinguish
invalid files built with incompatible Capy versions.

### Instruction groups

- **Entry:** `enter`, `enter_rest`. They check the argument count and the
  room in the stack (`esp`), and they bump the hot counter.
- **Return points:** `return_point`, `return_point_forward`,
  `return_point_values`, `return_point_values_rest`. A return point is where
  a callee returns to. It carries the frame size, a liveness mask in the aux
  pool (what the GC scans), and how multiple values are handled
  (`MV_ERROR` or `MV_IGNORE`).
- **Data movement:** `move`, `move_two`, `load_constant`, `load_immediate`,
  `load_raw`, `cell_ref`, `cell_set`.
- **Closures and environments:** `closure`, `closure_env`, `free_ref`,
  `closure_ref`, `closure_set`, `make_env`, `env_ref`, `env_set`.
- **Control:** `jump`, conditional jumps fused with their test
  (`jump_if_null`, `jump_if_pair`, `jump_if_fixnum`, `jump_if_eq`,
  `jump_if_less`, and others, each with a `not` form), `switch_fixnum`,
  `switch_char`, `switch_symbol`, `loop_hint`, `call`, `tail_call`,
  `call_self`, `tail_call_self`, `return`, `return_values`, `raise`.
- **Rest arguments:** `rest_ref`, `rest_len`, `rest_null`, `rest_pair`,
  `rest_list`, `values_rest`.
- **Primitives:** `call_primitive0` to `call_primitive3` and
  `call_primitive_n` call a primitive by id. Hot primitives get dedicated
  opcodes so engines can inline them: `car`, `cdr`, `cons`, `set_car`,
  `set_cdr`, `eq`, `not`, `add`, `sub`, `mul`, `add_immediate`, the numeric
  comparisons, `div`, `quotient`, `remainder`, `modulo`, `negate`, `abs`,
  `sqrt`, `vector_ref`, `vector_set`, `vector_length`, `variable_ref`,
  `variable_set`, `get_slot`, `put_slot`.
- **Superinstructions:** `move_two`, `add_immediate_move`, `vector_ref_mul`.
  The compiler fuses adjacent pairs. Every engine can run a fused instruction
  as the sequence it stands for. A `call` is never fused with its
  `return_point`, because the return point is a resume target addressed by
  bytecode position.


## Frames and calls

Procedures run on a Scheme stack, not on the C stack. A frame is a run of
slots:

```text
 fp[0]     return address
 fp[1]     closure being executed
 fp[2..]   arguments, then locals
```

There is no saved frame pointer. A register in bytecode is a slot of the
current frame, so `r0` is the return address and `r1` is the closure.

The compiler allocates registers by linear scan, one interval per CFG
variable (`compiler/bytecode/lower.rs`).

### Non-tail call

A `call` builds the callee frame right above the live registers of the
caller. It writes the callee and its arguments straight into the new frame.

`base` is above the highest register that is live after the call. The slot
`r[base-1]` is reserved and holds no live value. It is in no liveness mask.

Every `call` is followed by a `return_point` or `return_point_forward` whose
`frame` equals `base`. A single returned value goes straight to the `dst`
register of the return point.

### Tail call

A `tail_call` reuses the current frame. The callee goes in `r1` and the
arguments go in `r2..`. The stack does not grow. The return address stays
the same and the old locals are dropped.

### Variadic procedures

A variadic procedure starts with `enter_rest`. It moves the extra arguments
into a rest area of the frame. The `rest_*` instructions read that area.

### Thread state

The per-thread `Tc` (`runtime/thread.rs`) holds what machine code needs:
the stack bounds, `fp` and `sp`, the allocation cursor and limit, the
stack-overflow limit `esp`, and the yieldpoint flag. Generated code reaches
it through the `TC` register at offsets computed in `runtime/sabi.rs`.

The slower per-thread `State` holds the rest: the dynamic state, continuation
marks and winders, the nest level, and the root stack that keeps values alive
across native calls.

`enter` checks the argument count and that the frame fits below `esp`.
Another thread asks for a yieldpoint by setting `esp` to null. The next
`enter` then takes the slow path. `loop_hint` polls the yield flag on loop
back edges.

### Exceptions and native calls

`raise` ends a CFG block with a `RaiseKind`. It runs the installed exception
handler, found through the continuation marks.

A native procedure (`#[scheme]` function or SNI callback) gets the frame
pointer and reads its arguments through `Call` (`api/call.rs`). See
`docs/SNI.md`.

## Return-address ABI

`fp[0]` of every frame is a machine address. An `RpHeader` sits just before
it (`runtime/sabi.rs`):

- owner link: the code object that owns the address
- mv address: where to return for other than one value
- mask: live slots of the frame
- frame size: size of the frame returned to

`fp[0]` points at the return address that follows the header.

The compact header packs size, mask, and mode into one word. The stack
walker and the collector read these headers to find live slots and to keep
code alive. No side tables are needed.

Interpreted frames share one return address, the `interpreter_return` glue.
Its size and mask come from the bytecode `return_point` whose position is
saved in `fp[-1]`. That is the reserved slot from the call diagram. JIT
frames use their own return addresses with real headers.

## Continuations

Stacks and continuations follow the segmented stack model of Chez Scheme
(Hieb, Dybvig, Bruggeman, 1990). Capturing a continuation splits the stack
at the current frame:

Frames A, B and C sit on the stack, with C at `fp`. After the capture, A and
B belong to the captured continuation and C continues on a new segment.

A one-shot continuation resumes in place. A multi-shot continuation is
copied back when it is reinstated. Continuation marks are kept as an
attachment list and restored when a frame underflows. The code is in
`runtime/interp/stack.rs` and `runtime/vm/control.rs`.

## The verifier

`bytecode/verify.rs` runs when the FASL reader loads a code block:
```rust
verify::verify(&CodeShape { code, pool, nconsts, frame_size, metadata_counts })
```

The verifier checks that:

- The code block decodes as instructions with no gaps. The first one is an
  entry instruction.
- The last instruction does not fall through. It is `jump`, `return`,
  `return_values`, `tail_call`, `tail_call_self`, or `raise`.
- Every register is below `frame_size`, and `frame_size` is below `1 << 16`,
  the size of a stack segment. Frame sizes in entries and return points do
  not exceed the one of the block.
- Every constant index, primitive id, runtime procedure id, and metadata id
  is in range. Metadata counts are bounded by the code length, so a corrupt
  file cannot ask for a huge metadata area.
- Every jump and switch target starts an instruction that is not an entry
  and not a return point. Return points expect state that only a call or a
  return sets up.
- A return point directly follows a call. A multiple-value entry follows
  `return_point_forward`, which has the same frame size as that entry. The
  `mv` operand is `MV_ERROR` or `MV_IGNORE`.
- Liveness masks of return points lie inside the aux pool and do not name a
  register at or above `base - 1`.

## Engines

`CAPY_ENGINE` (or `--engine`) picks the engine for new activations, once per
thread. The values are `rust` and `template`. The default is `template` on
x86-64 and AArch64 outside Windows, and `rust` elsewhere.

### Rust interpreter

`runtime/interp/rust_interp.rs` has one handler per opcode and width,
reached through a dispatch table. It is the portable fallback. The template
interpreter also bails out to it for work it does not handle. It uses the
shared stack (`interp/stack.rs`) and the slow paths in `interp/slow.rs`.

### Template interpreter

`runtime/codegen/template_image.rs` generates one machine code image per
process. The image holds a handler per opcode and width, an opcode map, glue,
and stubs. The handlers come from the same per-instruction templates as the
JIT (`codegen/template/`). Here the template context decodes operands from
the bytecode at run time. 

The image also holds a bail handler per opcode and width. It saves state,
records the program counter, and finishes the activation in the Rust
interpreter.

Closures created under this engine carry the interpreter entry as their
machine entry. A call between interpreted procedures is a plain machine call
that uses the return-address ABI.

### Baseline JIT

`runtime/codegen/jit/` reuses the same templates with another context:

| | Template interpreter | Baseline JIT |
|---|---|---|
| Registers | frame slots | frame slots |
| Operands | decoded at run time | immediates |
| Constants, metadata | loaded by index | absolute addresses |
| Control flow | dispatch table | jumps between labels |


### Tier-up

1. `enter` and `loop_hint` count entries and back edges. At the threshold
   the slow path calls `promotion::request_tier_up`.
2. The request installs finished code if there is any. Otherwise it submits
   a compile job when the block is hot. A bit per block allows one
   submission. The mutator never blocks.
3. Workers (`CAPY_JIT_WORKERS`, default 1) compile from the code buffer. A
   global root keeps the buffer alive until the job ends. Workers never
   touch the movable code block.
4. A mutator installs the result at its next `enter` or loop slow path. It
   jumps in through a resume table that maps `pc` to a machine address. If
   a job panics, that one procedure stays interpreted.

