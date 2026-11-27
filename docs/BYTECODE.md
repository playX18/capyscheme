# Bytecode

Compiled Scheme code is bytecode. Two engines run it: the Rust interpreter and the template interpreter. The template interpreter
also triggers baseline JIT compilation of hot code blocks.

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

The bytecode is not stable, but changes should be small. A new opcode gets the ID `last_opcode + 1` and never reuses an existing ID. FASL files also store the `format_hash()` of the opcode set, so the loader can reject files built by an incompatible Capy version.

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
  `apply`, `tail_apply`, `call_self`, `tail_call_self`, `return`, `return_values`, `raise`.
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


## Encoding (for the Scheme assembler)

### One instruction

```text
 instruction = header operand* [metadata-id]
 narrow      header = [opcode:1]                       operand width 1
 wide16      header = [0x00][opcode:2]                 operand width 2
 wide32      header = [0x01][opcode:4]                 operand width 4
```

- `0x00` (`WIDE16_PREFIX`) and `0x01` (`WIDE32_PREFIX`) are not opcodes. Opcode numbers start at
  2 (`first-opcode`) and follow the order of `bytecode_list!` in `def.rs` (`preserve_order`).
  A narrow header holds the opcode as its single byte, so only opcodes 2..=255 can be
  narrow. Wide headers take any number.
- The operands follow the header in the order the opcode declares them, each in the width of
  the header. There is one width per instruction. Every operand, including the metadata id,
  is encoded in it.
- An opcode with a `metadata` block has one more operand after the declared ones, the
  *metadata id*, an unsigned operand. It is the index of the instruction's record among the
  records of the same opcode in the code block: the n-th instruction of that opcode (in
  code order, counting from 0) has id n. Instructions of other opcodes do not count.
- An unsigned operand (`Register`, `Constant`, `Count`, `PrimitiveId`, `PoolReference`, the
  metadata id) is its value truncated to the width. Narrow holds 0..=255, wide16 0..=65535, wide32
  0..=4294967295. A signed operand (`Immediate`, and the `i32` jump offsets) is its two's
  complement truncated to the width: narrow holds -128..=127, wide16 -32768..=32767, wide32 the
  whole `i32` range. `isa-operand-kinds` gives each operand kind its type, `u32` or `i32`.
- Instruction length is `header-length + width * operand-count`, where header length is 1, 3 or
  5 for narrow, wide16, wide32, and `operand-count` includes the metadata id.

### Choosing the width

The width of an instruction is the smallest of narrow, wide16, wide32 for which the opcode fits
the header (narrow needs `2 <= opcode <= 255`) and every operand, including the metadata id,
fits. Without jumps that is all: the width follows from the operand values.

Instructions with a jump offset (`relative-start` operands: `jump`, `jump-if-*`, `get-slot`,
`put-slot`) need the final layout, which in turn depends on their widths. `link` solves it by
fixpoint loop:

1. Every instruction gets its *seed* width: as above, where a label operand counts as the value 0
   (it fits any width).
2. Repeat:
   1. Lay out the stream with the current widths. Each instruction has the offset of the byte
      after the previous one. A label is bound to the offset of the next instruction (a label
      emits no bytes).
   2. Walk the instructions in order. For each instruction with a label operand, resolve the label
      with the offsets of this pass and find the smallest width that fits the opcode and all
      operands, now with the resolved offsets. If that width is *larger* than the current
      width, set the instruction's width to it and mark the pass as changed. Widths never
      shrink. Offsets within the pass are not updated while walking.
   3. Stop when the pass changed nothing.
3. Emit with the final widths and the final label offsets.

The offset stored for a jump is `label-offset - instruction-offset` (`relative_start`): signed, in
bytes, measured from the *first byte of the instruction*, which for a wide instruction is its
prefix byte, not from the end of the instruction. 

### Pool words and `metadata-counts`

The aux pool (`pool` in a program image) is a vector of `u32` words. It is filled by the lowering,
not the encoder, and some words are jump displacements that are only known after the layout:

- `switch-fixnum`, `switch-char`: `table` is the word index of the table in the pool. Sparse:
  `[default disp][count][key:i32 disp]*count`, keys sorted ascending. Dense (when the keys cluster,
  see `dense_span_for_keys`): `[default disp][span | 0x80000000][min:i32][disp]*span`, indexed by
  `key - min`.
- `switch-symbol`: `[default disp][count][mask_lo][mask_hi]([key_lo][key_hi][disp])*count`.
- `enter-case`: `[count]([min][max][disp][frame][rest_base][rest_cnt])*count`, with
  `CASE_NO_REST` (`0xFFFFFFFF`) for a clause without a rest argument.
- Every `disp` in a table is `target-label-offset - offset` of the switch (or enter-case)
  instruction, as a `u32` holding an `i32`. The table word is written after the layout.
- A liveness mask word index (`mask` operands of `return-point`, `loop-hint`) refers to a
  mask stored in the pool, masks deduplicate by content.


## Frames and calls

Procedures run on a Scheme stack with the following layout:

```text
 fp[0]     return address
 fp[1]     closure being executed
 fp[2..]   arguments, then locals
```

There is no saved frame pointer. A register in bytecode is a slot of the
current frame, so `r0` is the return address and `r1` is the closure.

The compiler allocates registers by Linear Scan Register Allocator (`core/compiler/bytecode/lower.scm`, `regalloc.scm`).

### Non-tail call

A `call` builds the callee frame right above the live registers of the
caller. It writes the callee and its arguments into the new frame.

`base` is above the highest register that is live after the call. The slot
`r[base-1]` is reserved and holds no live value. It is in no liveness mask.

Every `call` is followed by a `return_point` or `return_point_forward` whose
`frame` equals `base`. A single returned value goes to the `dst`
register of the return point.

### Tail call

A `tail_call` reuses the current frame. The callee goes in `r1` and the
arguments go in `r2..`. The stack does not grow. The return address stays
the same and the old locals are dropped.

### Variadic procedures

A variadic procedure starts with `enter_rest`. It moves the extra arguments
into a rest area of the frame. The `rest_*` instructions read that area.

### Thread state

The per-thread `ThreadContext` holds execution state read by interpreter and JIT.
That is the stack bounds, `fp` and `sp`, the allocation cursor and limit, the
stack-overflow limit `esp`, and the yieldpoint flag. Generated code pins `TC` register for fast-access
to that context,

The slower per-thread `State` holds the rest. That is the dynamic state, continuation
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
it:

- owner link: the code object that owns the address
- mv address: where to return for other than one value
- mask: live slots of the frame
- frame size: size of the frame returned to

`fp[0]` points at the return address that follows the header.

There is also compact form which packs size, mask, and mode into one machine word. 

Stack-walker uses these headers to find live values for GC or to produce backtraces.

Interpreter frames use single return address (`interpreter_return` glue), which computes size and mask
from `return_point` in bytecode whose position is in `fp[-1]` (which is a reserved slot!).


## Continuations

Stacks and continuations follow the segmented stack model of Chez Scheme
(Hieb, Dybvig, Bruggeman, 1990). Capturing a continuation splits the stack
at the current frame.

Say frames A, B and C sit on the stack, with C at `fp`. After the capture, A and
B belong to the captured continuation and C continues on a new segment.

A one-shot continuation resumes in place. A multi-shot continuation is
copied back when it is reinstated.

## Engines

`CAPY_ENGINE` (or `--engine`) picks the engine for new activations, once per
thread. The values are `rust` and `template`. The default is `template` on
x86-64 and AArch64 outside Windows, and `rust` elsewhere.

### Rust interpreter

Rust interpreter uses `become` and `rust-preserve-none` calling convention to achieve indirect dispatched interpreter. It is slower
than template interpreter, but still remains competitive to other *interpreted* Scheme implementations, and is enough to execute Capy
on unsupported platforms.

### Template interpreter

Template interpreter is generated at startup of a Scheme VM. Each instruction handler is carefully
converted to efficient machine code, and some of inline-caches can even generate more code at runtime 
to e.g perform calls efficiently. 

There is also a "bail handler" per each opcode which allows any JIT code to leave into Rust interpreter,
which is useful for debugging or adding new opcodes into Rust interpreter first, and then extending it to JIT 
and template interpreter. 

Calls between template interpreter frames are regular `jmp [closure.code]` as they share the same calling convention.

### Baseline JIT

Baseline JIT uses the same code as tempalte interpreter, except with different compilation context:

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
3. Workers (`CAPY_JIT_WORKERS`, default 1) compile from the code buffer. 
4. A mutator installs the result at its next `enter` or loop slow path. It
   jumps in through a resume table that maps `pc` to a machine address.

