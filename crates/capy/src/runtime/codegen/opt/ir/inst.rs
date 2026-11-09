//! Instructions, terminators and the effect model.
//!
//! Operands live in [`InstData::args`](InstData), not in [`Op`]. An `Op`
//! carries only immediates, so a pass can rewrite operands without matching
//! every variant.

use cranelift_entity::{EntityList, entity_impl};

use super::frame_state::FrameState;
use super::func::{Block, Value};
use super::types::{ClassRef, CodeRef};

/// An instruction.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Inst(u32);
entity_impl!(Inst, "inst");

/// The object a [`Op::Remat`] marker rebuilds at a deopt.
///
/// A marker stands in for an allocation that escape analysis removed from the
/// code. It has no runtime definition: the value only exists in frame states,
/// and the deopt runtime reconstructs the object from the marker's operands
/// (the object's fields).
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum RematKind {
    /// `[car, cdr] -> pair`
    Pair,
    /// `[free...] -> closure` running `code`. `free` counts the operands.
    Closure { code: CodeRef, free: u32 },
    /// `[slot...] -> env record`. `slots` counts the operands.
    Env { slots: u32 },
    /// `[element...] -> vector`. `len` counts the operands.
    Vector { len: u32 },
    /// `[data...] -> tuple`. `len` counts the operands. Records use this
    /// layout (`[length][rtd, field...]`).
    Tuple { len: u32 },
    /// `[slot...] -> class instance`. `class` is the instance's class id and
    /// `slots` counts the operands. The deopt runtime looks the class
    /// descriptor up in the class table and rebuilds its slot storage.
    Instance { class: u32, slots: u32 },
    /// `[value] -> variable`. A fresh `VARIABLE` cell holding `value`, with no
    /// watchpoint set. Used by the cell-elimination pass: a locally boxed
    /// mutable variable that no code can observe needs to exist again only
    /// when baseline resumes a deoptimized frame.
    Variable,
}

impl RematKind {
    /// The number of field operands the marker takes.
    pub const fn field_count(self) -> usize {
        match self {
            RematKind::Pair => 2,
            RematKind::Closure { free, .. } => free as usize,
            RematKind::Env { slots } => slots as usize,
            RematKind::Vector { len } => len as usize,
            RematKind::Tuple { len } => len as usize,
            RematKind::Instance { slots, .. } => slots as usize,
            RematKind::Variable => 1,
        }
    }
}

/// A primitive, named by its index in the primitive table.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PrimRef(u32);
entity_impl!(PrimRef, "prim");

/// A returning runtime helper, named by its index in the operation table.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HelperRef(u32);
entity_impl!(HelperRef, "helper");

/// The returning helpers Tier 2 calls through [`Op::CallOp`]. Each returns a
/// value and a status (glue.rs `OptReturn`). A nonzero status deopts before
/// the instruction, so baseline runs it from its own pc. The emitter's helper
/// table (emit/mod.rs `HELPERS`) is in this order.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Helper {
    /// `[id] -> Any`: the runtime procedure at index `id`.
    RuntimeProc = 0,
    /// `[value, table] -> Any`: a fixnum, the case index of a symbol switch or -1.
    SwitchSymbol = 1,
    /// `[obj, metadata] -> Any`: the cached slot of `obj`.
    GetSlot = 2,
    /// `[obj, src, metadata] -> none`: stores `src` in the cached slot of `obj`.
    PutSlot = 3,
    /// `[ptr, count] -> Any`: a vector of the `count` words at `ptr`. Only the
    /// entry glue calls it, for the extra arguments of a variadic procedure.
    CollectRest = 4,
    /// `[vector, skip] -> Any`: the list of a rest vector's elements from `skip`.
    RestList = 5,
    /// `[var, value] -> none`: stores `value` in the variable `var` and fires
    /// its watchpoint set. Never fails.
    VariableSet = 6,
    /// `[kind, lhs, rhs] -> Any`: the generic numeric operation `kind` (a
    /// fixnum, see `build/arith.rs` `NumOp`) on two Scheme values. A non-number
    /// operand is the status, so baseline raises the error.
    Arith = 7,
    /// `[list] -> Any`: the length of a proper list as a fixnum. An improper
    /// or circular list is the status, so baseline raises the error.
    Length = 8,
    /// `[a, b] -> Any`: `(append a b)` for a proper list `a`.
    Append = 9,
    /// `[num_lo, num_hi, den_lo, den_hi] -> Any`
    ReduceRational128 = 10,
    /// `[generic, obj] -> Any`: a plain instance slot resolved by the generic's
    /// slot cache, guarded by the argument's class, the entry's writer bit and
    /// the instance storage's layout stamp. The cache is read at run time and
    /// the descriptor's epoch is read before and after, so a cleared cache, a
    /// re-fill or an odd/racing epoch is a deopt. A miss or an odd epoch is
    /// the status.
    GenericSlotGet = 11,
    /// `[generic, obj, src] -> Any`: the writer counterpart; the result is
    /// `src`. The store runs through the instance's checked slot setter, whose
    /// barrier names the storage. Never allocates, so no root is needed.
    GenericSlotPut = 12,
    /// `[generic, arg0, arg1, argc] -> Any`: the cached `NO_NEXT` method body
    /// for arguments of the given classes. `arg1` is ignored when `argc` is
    /// one, and `argc` is a raw integer. The lookup reads the method cache at
    /// run time under the epoch seqlock (read, match, re-read); a non-cell
    /// argument, no matching entry, or a changed epoch is the status.
    GenericMethod = 13,
    /// `[string, index, char] -> none`: `string-set!` after the inline
    /// string-class and index checks. A mutable buffer with a character that
    /// fits its encoding is stored directly; an immutable or shared string
    /// and a wide character in a narrow buffer deopt, so baseline raises or
    /// detaches exactly as before.
    StringSet = 14,
    /// `[ptr, count] -> Any`: the list of the `count` values at `ptr`, the
    /// extras of a `return_point_values_rest` entry. It allocates without
    /// collecting, so the unrooted callee frame it reads stays valid.
    ValuesRest = 15,
}

impl Helper {
    /// The helper's index in the emitter's table.
    pub fn helper_ref(self) -> HelperRef {
        HelperRef::from_u32(self as u32)
    }
}

/// An immediate or a constant-table slot. A constant never holds a movable
/// heap pointer, so code can embed it directly.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Constant {
    Int32(i32),
    /// A raw 64-bit integer immediate. Like a raw int32 it is not a Scheme
    /// value and can be embedded in code.
    Int64(i64),
    /// IEEE bits of a double.
    Double(u64),
    /// A tagged immediate such as a boolean, null or character, as raw bits.
    Bits(u64),
    /// A slot in the unit's constant table, for objects that may move.
    Table(u32),
    /// Constant `index` of the bytecode of `code`, which lives in its
    /// non-moving code buffer. Code embeds its address.
    Pool {
        code: CodeRef,
        index: u32,
    },
}

/// The unboxed representations.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Repr {
    Int32,
    Int64,
    /// An unboxed two-limb integer. Only `Box(Repr::Int128)` is defined for
    /// it: a fixnum when the value fits `i32`, otherwise a one- or two-limb
    /// sign-magnitude `BIGINT`.
    Int128,
    Double,
}

/// What a call's return point does when the callee returns other than one
/// value. These are the bytecode's `MV_ERROR` and `MV_IGNORE`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ReturnValues {
    /// Raise, as a single-value return point does (`MV_ERROR`).
    Error,
    /// Take the first value and continue (`MV_IGNORE`).
    Ignore,
    /// A return point that forwards to a multiple-value entry
    /// (`return_point_forward`). One value continues; any other count leaves
    /// for a deopt exit at the entry, which baseline resumes.
    Forward,
    /// A return point that forwards to a `return_point_values` entry taking
    /// exactly this many values (never one). A callee that returns that many
    /// continues in Tier 2 code with the values left in the callee frame
    /// (`Op::ReturnedValue`). Any other count, including one, leaves for a
    /// deopt exit at the entry, as `Forward` does.
    Take(u32),
    /// A return point that forwards to a `return_point_values_rest` entry
    /// taking this many values or more. The callee's values stay in the
    /// callee frame: the fixed ones are read with `Op::ReturnedValue`, the
    /// extras with `Op::ReturnedCount` and `Op::ReturnedAddr`. A count below
    /// this leaves for a deopt exit at the entry, as `Forward` does.
    TakeRest(u32),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Cmp {
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

/// The memory regions of the effect model.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum MemoryRegion {
    Heap = 0,
    Variables = 1,
    Modules = 2,
    Continuations = 3,
    Runtime = 4,
    Unknown = 5,
}

/// The operation an instruction performs, without its operands.
///
/// The operand list of each variant is given in its comment as `[args]`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Op {
    /// `[] -> ty`
    Const(Constant),

    /// `[v] -> Int32`. Deopts unless `v` is a fixnum.
    CheckInt32,
    /// `[v] -> Int64`. Deopts unless `v` is a fixnum, and sign-extends its
    /// payload. Runtime fixnums stay 32-bit, so the guard is the same one as
    /// `CheckInt32`; only the widened result differs.
    CheckInt64,
    /// `[v] -> Int64`. A fixnum gives its sign-extended payload. Otherwise
    /// deopts unless `v` is a `BIGINT` whose magnitude fits an i64. Reads the
    /// sign-magnitude payload exactly as `BigInt::try_as_i64` does: `count`
    /// at payload offset 0, `negative` at offset 4 and `words[0]` at offset
    /// 8. A count above one, or a magnitude above the representable range for
    /// the sign, deopts before the instruction so baseline runs it. A zero
    /// count is zero; `i64::MIN` is accepted (negative magnitude `2^63`).
    /// Accepting a fixnum lets one guard type an `Int64` loop parameter that
    /// may hold either representation (an accumulator that outgrows `i32`).
    CheckBigIntFits64,
    /// `[v] -> Double`. Deopts unless `v` is a flonum.
    CheckDouble,
    /// `[v] -> Int32`. Deopts unless `v` is a character. The result is the
    /// code point.
    CheckChar,
    /// `[v] -> Heap(class)`. Deopts unless `v` is an instance of the class.
    CheckClass(ClassRef),
    /// `[closure] -> Closure(Some(code))`. Deopts unless the closure runs `code`.
    CheckCode(CodeRef),
    /// `[] -> none`. Deopts once the unit has been invalidated.
    CheckNotInvalid,

    /// `[v] -> Any`
    ///
    /// `Box(Repr::Int64)` tags a fixnum when the value fits `i32` and
    /// otherwise allocates a one-limb sign-magnitude `BIGINT` (class 22,
    /// 8-byte header plus a 16-byte `{count, negative, pad, word}` payload) in
    /// a deferred allocation block. It therefore **allocates**; see
    /// [`Op::effects`]. The object holds no GC-visible fields, so the
    /// allocator does not trace it, and the payload is written before any
    /// safepoint. Values always fit i64 here; results past i64 are prevented
    /// by the checked arithmetic that feeds this box.
    Box(Repr),
    /// `[v] -> Int32 | Int64 | Double`. Assumes `v` was checked.
    Unbox(Repr),
    /// `[v: Int64] -> Int32`. The low 32 bits of a raw i64. Representation
    /// selection emits it when a raw i64 loop parameter feeds the int32 lane
    /// (an index or a fixnum conversion), instead of boxing it and untagging
    /// it again.
    Int64ToInt32,

    /// `[a, b] -> Int32`. Deopts on overflow.
    AddI32Ovf,
    /// `[a, b] -> Int32`. Deopts on overflow.
    SubI32Ovf,
    /// `[a, b] -> Int32`. Deopts on overflow.
    MulI32Ovf,
    /// `[a, b] -> Int32`, truncating quotient. Deopts when `b` is zero or the
    /// quotient overflows (`MIN / -1`), so baseline raises or wraps.
    QuotI32,
    /// `[a, b] -> Int32`, remainder with the sign of `a`. Deopts when `b` is
    /// zero.
    RemI32,
    /// `[a, b] -> Int32`, modulo with the sign of `b`. Deopts when `b` is zero.
    ModI32,

    /// `[a, b] -> Int64`. Total: a two's-complement wrap cannot happen for
    /// two fixnum operands, and the wide lane treats it as the machine add.
    AddI64,
    /// `[a, b] -> Int64`. Total.
    SubI64,
    /// `[a, b] -> Int64`. Total.
    MulI64,
    /// `[a, b] -> Int64`. Deopts on overflow.
    AddI64Ovf,
    /// `[a, b] -> Int64`. Deopts on overflow.
    SubI64Ovf,
    /// `[a, b] -> Int64`. Deopts on overflow.
    MulI64Ovf,
    /// `[a, b] -> Int64`, truncating quotient. Deopts when `b` is zero or the
    /// quotient overflows (`MIN / -1`).
    QuotI64,
    /// `[a, b] -> Int64`, remainder with the sign of `a`. Deopts when `b` is
    /// zero.
    RemI64,
    /// `[a, b] -> Int64`, modulo with the sign of `b`. Deopts when `b` is
    /// zero.
    ModI64,
    /// `[a, b] -> Bool`
    CmpI64(Cmp),
    /// `[a, b] -> Double`
    AddF64,
    /// `[a, b] -> Double`
    SubF64,
    /// `[a, b] -> Double`
    MulF64,
    /// `[a, b] -> Double`
    DivF64,
    /// `[v: Double] -> Double`. The largest integral value not greater than
    /// `v`, as the baseline's `floor` flonum fast path computes it.
    FloorF64,
    /// `[v: Double] -> Double`. The smallest integral value not less than `v`.
    CeilingF64,
    /// `[v: Double] -> Double`. The integral value nearest `v` toward zero.
    TruncateF64,
    /// `[v: Double] -> Double`. The hardware square root, as the baseline's
    /// `sqrt` flonum fast path computes it (libm's `sqrt` for a flonum).
    SqrtF64,
    /// `[a, b] -> Bool`
    CmpI32(Cmp),
    /// `[a, b] -> Bool`
    CmpF64(Cmp),
    /// `[a, b] -> Bool`. Compares the raw bits of two values, as `eq?`.
    EqBits,
    /// `[v] -> Bool`. Whether `v` is a fixnum.
    TestInt32,
    /// `[v] -> Bool`. Whether `v` is a fixnum, for the wide lane. Runtime
    /// fixnums are still 32-bit values, so it tests exactly what `TestInt32`
    /// does; it exists so the wide path names its own test.
    TestInt64,
    /// `[v] -> Bool`. Whether `v` is a flonum.
    TestDouble,
    /// `[v: Int32] -> Double`. The exact double of an int32.
    Int32ToDouble,
    /// `[v: Int64] -> Double`. The double of an int64, rounded to nearest.
    Int64ToDouble,
    /// `[v] -> Bool`. Whether `v` is a pair.
    TestPair,
    /// `[v] -> Bool`. Whether `v` is a character with a Unicode scalar
    /// value, exactly the test the baseline's `char?` fast path makes. Total.
    TestChar,
    /// `[v] -> Bool`. Whether `v` is `#t` or `#f`. Total.
    TestBoolean,
    /// `[v] -> Bool`. Whether `v` is a number: a fixnum, flonum, or a heap
    /// object of class `BIGINT`, `RATIONAL` or `COMPLEX`. Total, as the
    /// baseline's `number?` fast path.
    TestNumber,
    /// `[v] -> Bool`. Whether `v` is a heap object of the class. Total.
    TestClass(ClassRef),
    /// `[v] -> Bool`. Whether `v` is a heap object (a cell). Total.
    TestHeap,
    /// `[value] -> Bool`. Whether the value is a closure that runs `code`.
    TestCode(CodeRef),
    /// `[b] -> Any`. Boxes the boolean `b` as `#t` or `#f`. A Bool is a raw
    /// 0/1 flag, so a value that goes into a register or a frame state must
    /// pass through this first. Pure.
    BoolToValue,

    /// `[rational] -> Int64`. Loads component `index` (0 is the numerator, 1
    /// the denominator) of a checked `RATIONAL` as a sign-extended `i64`.
    /// The `Number` layout is the stable one from `value/number/mod.rs`:
    /// component `index` occupies payload bytes `16 * index .. 16 * index +
    /// 16`, with the `u64` tag at offset 0 and the payload at offset 8. The
    /// guard requires the tag to be `TAG_FIXNUM` (0); a `BIGINT`, `FLONUM`,
    /// `RATIONAL` or `COMPLEX` component deopts to baseline (Stage A handles
    /// fixnum components only).
    LoadRationalComponent { index: u32 },
    /// `[rational] -> Int64`
    LoadRationalComponentWide { index: u32 },

    /// `[a: Int64, b: Int64] -> Int128`. The signed 64x64 widening product:
    /// the low limb is the truncating product and the high limb its signed
    /// high half. Total (every i64 pair has an exact i128 product).
    Mul64x64To128,
    /// `[a: Int128, b: Int128] -> Int128`. Two's-complement addition; deopts
    /// when the exact sum leaves the i128 range (the 129-bit carry case the
    /// rational plan must send back to baseline).
    Add128Ovf,
    /// `[a: Int128, b: Int128] -> Int128`. Deopts on i128 overflow.
    Sub128Ovf,
    /// `[a: Int128, b: Int128] -> Bool`. Signed 128-bit comparison.
    Cmp128(Cmp),
    /// `[a: Int128] -> Int128`. Shift left by the immediate amount, which is
    /// at most 127.
    Shl128(u32),
    /// `[a: Int128] -> Int128`. Zero-filling shift right by the immediate
    /// amount, which is at most 127.
    Shr128(u32),
    /// `[a: Int128] -> Int128`. Sign-filling shift right by the immediate
    /// amount, which is at most 127.
    Ashr128(u32),
    /// `[a: Int64] -> Int128`. Sign-extends an i64 into the two-limb lane.
    ExtendI64To128,
    /// `[a: Int128] -> Int64`. The low limb.
    Int128Low,
    /// `[a: Int128] -> Int64`. The high limb.
    Int128High,
    /// `[obj] -> ty`. Reads word `field` of `obj`.
    Load { field: u32, region: MemoryRegion },
    /// `[obj, v] -> none`. Writes word `field` of `obj`.
    Store { field: u32, region: MemoryRegion },
    /// `[closure] -> Any`. Reads free variable `index`.
    LoadFree { index: u32 },
    /// `[cell] -> Any`
    LoadCell,
    /// `[cell, v] -> none`
    StoreCell,
    /// `[closure, v] -> none`. Writes free variable `index`.
    StoreFree { index: u32 },
    /// `[obj, v] -> none`. Records that `obj` now refers to `v`.
    WriteBarrier,

    /// `[vector] -> Int32`. The length of a checked vector. Deopts unless
    /// the length fits an int32.
    VectorLength,
    /// `[string, index] -> Int32`. Checks that `index` is a fixnum below the
    /// length of a checked string, and yields it as an int32. Deopts
    /// otherwise. The length word is payload word `STRING_LENGTH / 8`.
    CheckStringIndex,
    /// `[string, index: Int32] -> Any`. Reads character `index` of a checked
    /// string (bounds checked by [`Op::CheckStringIndex`]) and tags it. A
    /// wide string deopts to baseline.
    StringRef,
    /// `[vector, index] -> Int32`. Checks that `index` is a fixnum below the
    /// length of a checked vector, and yields it as an int32. Deopts otherwise.
    CheckIndex,
    /// `[vector, index: Int64] -> Int32`. The raw-index form of
    /// [`Op::CheckIndex`]: one unsigned 64-bit `index < length` compare (a
    /// negative i64 is a huge word, so it fails too), then the low 32 bits.
    /// The vector is already class-checked. A counted loop can therefore keep
    /// its counter unboxed while indexing.
    CheckIndexRaw,
    /// `[vector, index: Int32] -> Any`. Reads element `index`. The index was
    /// checked by [`Op::CheckIndex`].
    LoadElement,
    /// `[vector, index: Int32, v] -> none`. Writes element `index`. The index
    /// was checked by [`Op::CheckIndex`].
    StoreElement,
    /// `[bytevector, index: Int32] -> Int32`. Reads the unsigned byte
    /// `index`. The object is a class-checked bytevector and the index was
    /// checked by [`Op::CheckIndex`].
    LoadBytevectorU8,
    /// `[bytevector, index: Int32, v: Int32] -> none`. Writes the unsigned
    /// byte `v` at `index`. The object is a class-checked bytevector, the
    /// index was checked by [`Op::CheckIndex`], and `v` is a fixnum in
    /// `0..=255`.
    StoreBytevectorU8,

    /// `[car, cdr] -> Pair`
    AllocPair,
    /// `[free...] -> Closure(Some(code))`
    AllocClosure(CodeRef),
    /// `[slots...] -> Any`
    AllocEnv,
    /// `[elements...] -> Vector`
    AllocVector,
    /// `[size, fill] -> Vector`. Allocates a vector whose length is the
    /// tagged fixnum `size` and fills every element with `fill`. A
    /// non-fixnum, negative or too-large size deopts to baseline, which runs
    /// `make-vector` and reports what it reports.
    AllocVectorFill,
    /// `[element...] -> Tuple`. Allocates a fixed-length internal tuple
    /// (class `TUPLE`): payload word 0 is the number of elements, then one
    /// word per element. Records use the same layout, with the record type
    /// descriptor as the first element. The length is the operand count.
    AllocTuple,
    /// `[class, header, stamp_len, slot...] -> Any`. Allocates a Scheme class
    /// instance of `class`: a `SchemeInstance` payload `[descriptor, storage]`
    /// and a `SlotStorage` with the layout stamp and one value per slot.
    /// `header` and `stamp_len` are raw `Int64` operands (the slot storage's
    /// type-class header word, and `(slots << 32) | stamp`); the descriptor is
    /// a tagged value the builder interned in the constant table. `slots`
    /// counts the trailing slot values, so the operand count is `slots + 3`.
    /// The builder emits it only for `make-instance` with no initargs on a
    /// class with the stock allocation hook and no init thunks.
    AllocInstance { class: u32, slots: u32 },
    /// `[num, den] -> Rational`. Allocates a `RATIONAL` (class 23) whose
    /// payload is two inline `Number`s: the numerator at payload offset 0 and
    /// the denominator at offset 16. Each operand is a tagged Scheme value
    /// that is a fixnum or a single-limb `BIGINT` (as `Box(Repr::Int64)`
    /// produces); the tag is written as `TAG_FIXNUM` (0) with an `i32`
    /// payload, or `TAG_BIGINT` (2) with the `Gc` pointer. Any other operand
    /// deopts before the instruction. The object is fresh, so no field needs a
    /// write barrier, and no safepoint runs between the allocation and the
    /// stores.
    AllocRational,

    /// `[fields...] -> Any`. A rematerialization marker: it stands in for a
    /// removed allocation whose object is only needed to rebuild a frame at a
    /// deopt. It has no runtime effect and emits no code; an exit whose frame
    /// state names it gets a [`RematKind`] descriptor in its deopt metadata,
    /// and the deopt runtime reconstructs the object from the operands.
    ///
    /// The contract a pass must uphold when replacing an allocation with a
    /// marker:
    ///
    /// - the marker's operands are the object's fields, so the verifier,
    ///   liveness and DCE keep them alive through every exit that names the
    ///   marker (they are ordinary operands of a producer);
    /// - the marker value may appear only in frame states and as a field of
    ///   another marker. It may not be returned, passed to a call, stored
    ///   into a live object, put on a control-flow edge, or read by another
    ///   instruction, since it has no location in the optimized frame;
    /// - field values are the values at every exit that names the marker, so
    ///   a pass that mutates the object must not remove its stores.
    Remat(RematKind),

    /// `[obj] -> Any`. Reads the instance slot that the `get_slot` site at
    /// `meta` has cached, when `obj` is in the cached class and layout. The
    /// address is of the site's `OpGetSlotMetadata` in the non-moving buffer.
    /// Deopts unless the site's inline entry matches and the slot is bound
    /// (see `build/object.rs`).
    SlotGet { meta: u64 },
    /// `[obj, v] -> none`. Stores `v` in the slot that the `put_slot` site at
    /// `meta` has cached. It deopts before any store under the same
    /// conditions, and its write barrier is part of the instruction, since
    /// the barrier must name the slot storage rather than `obj`.
    SlotPut { meta: u64 },

    /// `[callee, args...] -> Any`. A call to a known code block.
    CallKnown(CodeRef, ReturnValues),
    /// `[callee, args...] -> Any`
    CallGeneric(ReturnValues),
    /// `[call] -> Any`. The `index`th value a `ReturnValues::Take` call left
    /// in the callee frame. It reads the frame the next call reuses, so it
    /// must run right after `call` and stays ordered before any later call.
    ReturnedValue(u32),
    /// `[call] -> Int32`. The number of values a `Take`/`TakeRest` call
    /// returned, less `skip`. It reads the count register, so it must run
    /// right after `call`.
    ReturnedCount(u32),
    /// `[call] -> Int64`. The address of the `index`th value the call left in
    /// the callee frame, for a helper that reads the extras.
    ReturnedAddr(u32),
    /// `[args...] -> Any`. A returning runtime helper.
    CallOp(HelperRef),
    /// `[args...] -> Any`
    CallPrim { prim: PrimRef, effects: Effects },
    /// `[] -> none`. A yieldpoint, where GC may run.
    Safepoint,
}

impl Op {
    /// The operand count, or `None` when it varies.
    pub fn fixed_arity(self) -> Option<usize> {
        match self {
            Op::Const(_) | Op::CheckNotInvalid | Op::Safepoint => Some(0),
            Op::CheckInt32
            | Op::CheckInt64
            | Op::CheckBigIntFits64
            | Op::CheckDouble
            | Op::CheckChar
            | Op::VectorLength
            | Op::CheckClass(_)
            | Op::CheckCode(_)
            | Op::Box(_)
            | Op::Unbox(_)
            | Op::Int64ToInt32
            | Op::LoadRationalComponent { .. }
            | Op::LoadRationalComponentWide { .. }
            | Op::Shl128(_)
            | Op::Shr128(_)
            | Op::Ashr128(_)
            | Op::ExtendI64To128
            | Op::Int128Low
            | Op::Int128High
            | Op::Load { .. }
            | Op::TestInt32
            | Op::TestInt64
            | Op::TestDouble
            | Op::Int32ToDouble
            | Op::Int64ToDouble
            | Op::FloorF64
            | Op::CeilingF64
            | Op::TruncateF64
            | Op::SqrtF64
            | Op::TestPair
            | Op::TestChar
            | Op::TestBoolean
            | Op::TestNumber
            | Op::TestClass(_)
            | Op::TestHeap
            | Op::TestCode(_)
            | Op::BoolToValue
            | Op::LoadFree { .. }
            | Op::LoadCell
            | Op::ReturnedValue(_)
            | Op::ReturnedCount(_)
            | Op::ReturnedAddr(_)
            | Op::SlotGet { .. } => Some(1),
            Op::CheckIndex
            | Op::CheckIndexRaw
            | Op::CheckStringIndex
            | Op::StringRef
            | Op::LoadElement
            | Op::LoadBytevectorU8 => Some(2),
            Op::StoreElement | Op::StoreBytevectorU8 => Some(3),
            Op::AddI32Ovf
            | Op::SubI32Ovf
            | Op::MulI32Ovf
            | Op::QuotI32
            | Op::RemI32
            | Op::ModI32
            | Op::AddI64
            | Op::SubI64
            | Op::MulI64
            | Op::AddI64Ovf
            | Op::SubI64Ovf
            | Op::MulI64Ovf
            | Op::QuotI64
            | Op::RemI64
            | Op::ModI64
            | Op::CmpI64(_)
            | Op::Mul64x64To128
            | Op::Add128Ovf
            | Op::Sub128Ovf
            | Op::Cmp128(_)
            | Op::AddF64
            | Op::SubF64
            | Op::MulF64
            | Op::DivF64
            | Op::CmpI32(_)
            | Op::CmpF64(_)
            | Op::EqBits
            | Op::Store { .. }
            | Op::StoreCell
            | Op::StoreFree { .. }
            | Op::WriteBarrier
            | Op::SlotPut { .. }
            | Op::AllocPair => Some(2),
            Op::AllocRational => Some(2),
            Op::AllocClosure(_)
            | Op::AllocEnv
            | Op::AllocVector
            | Op::AllocVectorFill
            | Op::AllocTuple
            | Op::AllocInstance { .. }
            | Op::Remat(_)
            | Op::CallKnown(..)
            | Op::CallGeneric(_)
            | Op::CallOp(_)
            | Op::CallPrim { .. } => None,
        }
    }

    /// Whether the instruction produces a value.
    pub fn has_result(self) -> bool {
        !matches!(
            self,
            Op::CheckNotInvalid
                | Op::Store { .. }
                | Op::StoreCell
                | Op::StoreFree { .. }
                | Op::WriteBarrier
                | Op::StoreElement
                | Op::StoreBytevectorU8
                | Op::SlotPut { .. }
                | Op::Safepoint
        )
    }

    /// Whether a failed check makes the instruction deopt. Such an
    /// instruction needs a frame state.
    pub fn can_deopt(self) -> bool {
        matches!(
            self,
            Op::CheckInt32
                | Op::CheckInt64
                | Op::CheckBigIntFits64
                | Op::CheckDouble
                | Op::CheckChar
                | Op::CheckClass(_)
                | Op::CheckCode(_)
                | Op::CheckNotInvalid
                | Op::VectorLength
                | Op::CheckIndex
                | Op::CheckIndexRaw
                | Op::CheckStringIndex
                | Op::LoadRationalComponent { .. }
                | Op::LoadRationalComponentWide { .. }
                | Op::AllocRational
                | Op::Add128Ovf
                | Op::Sub128Ovf
                | Op::AddI32Ovf
                | Op::SubI32Ovf
                | Op::MulI32Ovf
                | Op::QuotI32
                | Op::RemI32
                | Op::ModI32
                | Op::AddI64Ovf
                | Op::SubI64Ovf
                | Op::MulI64Ovf
                | Op::QuotI64
                | Op::RemI64
                | Op::ModI64
                | Op::SlotGet { .. }
                | Op::SlotPut { .. }
        )
    }

    /// Whether the instruction is a call or a safepoint. Such an instruction
    /// needs a frame state for its livemask.
    pub fn is_call(self) -> bool {
        matches!(
            self,
            Op::CallKnown(..)
                | Op::CallGeneric(_)
                | Op::CallOp(_)
                | Op::CallPrim { .. }
                | Op::Safepoint
        )
    }

    pub fn effects(self) -> Effects {
        // A failed check deopts, so it is observable and cannot be dropped.
        let guard = Effects::pure().checked();
        match self {
            Op::Const(_)
            | Op::Box(Repr::Int32)
            | Op::Box(Repr::Double)
            | Op::Unbox(Repr::Int32)
            | Op::Unbox(Repr::Int64)
            | Op::Unbox(Repr::Int128)
            | Op::Int64ToInt32
            | Op::AddI64
            | Op::SubI64
            | Op::MulI64
            | Op::CmpI64(_)
            | Op::Mul64x64To128
            | Op::Cmp128(_)
            | Op::Shl128(_)
            | Op::Shr128(_)
            | Op::Ashr128(_)
            | Op::ExtendI64To128
            | Op::Int128Low
            | Op::Int128High
            | Op::AddF64
            | Op::SubF64
            | Op::MulF64
            | Op::DivF64
            | Op::CmpI32(_)
            | Op::CmpF64(_)
            | Op::EqBits
            | Op::TestInt32
            | Op::TestInt64
            | Op::TestDouble
            | Op::Int32ToDouble
            | Op::Int64ToDouble
            | Op::FloorF64
            | Op::CeilingF64
            | Op::TruncateF64
            | Op::SqrtF64
            | Op::TestPair
            | Op::TestChar
            | Op::TestBoolean
            | Op::TestNumber
            | Op::TestClass(_)
            | Op::TestHeap
            | Op::BoolToValue
            // A marker allocates nothing while optimized code runs; its object
            // is rebuilt only at a deopt, so it has no runtime effect.
            | Op::Remat(_) => Effects::pure(),
            Op::TestCode(_) => Effects::read(MemoryRegion::Heap, SemanticEffects::NONE),
            Op::AllocPair
            | Op::AllocClosure(_)
            | Op::AllocEnv
            | Op::AllocVector
            | Op::AllocVectorFill
            | Op::AllocTuple
            | Op::AllocInstance { .. }
            | Op::AllocRational
            | Op::Box(Repr::Int64)
            | Op::Box(Repr::Int128) => Effects::allocate(),
            Op::Unbox(Repr::Double) | Op::StringRef => {
                Effects::read(MemoryRegion::Heap, SemanticEffects::NONE)
            }
            // Every other check deopts on failure, so it is observable and
            // cannot be dropped. `CheckBigIntFits64` additionally reads the
            // object's class and limbs.
            Op::CheckInt32
            | Op::CheckInt64
            | Op::CheckDouble
            | Op::CheckChar
            | Op::CheckClass(_)
            | Op::CheckCode(_)
            | Op::VectorLength
            | Op::CheckIndex
            | Op::CheckIndexRaw
            | Op::CheckStringIndex
            | Op::AddI32Ovf
            | Op::SubI32Ovf
            | Op::MulI32Ovf
            | Op::QuotI32
            | Op::RemI32
            | Op::ModI32
            | Op::AddI64Ovf
            | Op::SubI64Ovf
            | Op::MulI64Ovf
            | Op::QuotI64
            | Op::RemI64
            | Op::ModI64
            | Op::Add128Ovf
            | Op::Sub128Ovf => guard,
            Op::CheckBigIntFits64 => guard.union(Effects::read(
                MemoryRegion::Heap,
                SemanticEffects::NONE,
            )),
            // Reads the rational's class-checked tag and payload words; a
            // non-fixnum component deopts, so it is observable.
            Op::LoadRationalComponent { .. } | Op::LoadRationalComponentWide { .. } => {
                Effects::read(MemoryRegion::Heap, SemanticEffects::NONE).checked()
            }
            Op::LoadElement => Effects::read(MemoryRegion::Heap, SemanticEffects::VECTOR),
            Op::StoreElement => Effects::write(MemoryRegion::Heap, SemanticEffects::VECTOR),
            // Bytevector storage is separate from every other effect kind, so
            // these load and store no aliasing semantic state.
            Op::LoadBytevectorU8 => Effects::read(MemoryRegion::Heap, SemanticEffects::NONE),
            Op::StoreBytevectorU8 => Effects::write(MemoryRegion::Heap, SemanticEffects::NONE),
            Op::SlotGet { .. } => Effects::read(MemoryRegion::Heap, SemanticEffects::NONE).checked(),
            Op::SlotPut { .. } => Effects::write(MemoryRegion::Heap, SemanticEffects::NONE).checked(),
            Op::CheckNotInvalid => {
                Effects::read(MemoryRegion::Runtime, SemanticEffects::NONE).checked()
            }
            Op::Load { region, .. } => Effects::read(region, SemanticEffects::NONE),
            Op::Store { region, .. } => Effects::write(region, SemanticEffects::NONE),
            Op::LoadFree { .. } => Effects::read(MemoryRegion::Heap, SemanticEffects::NONE),
            Op::LoadCell => Effects::read(MemoryRegion::Variables, SemanticEffects::VARIABLE),
            // The callee frame is stack memory no other region names, and
            // every call writes everything, so a later call stays after it.
            Op::ReturnedValue(_) | Op::ReturnedCount(_) | Op::ReturnedAddr(_) => {
                Effects::read(MemoryRegion::Unknown, SemanticEffects::NONE)
            }
            Op::StoreCell => Effects::write(MemoryRegion::Variables, SemanticEffects::VARIABLE),
            Op::StoreFree { .. } => Effects::write(MemoryRegion::Heap, SemanticEffects::NONE),
            Op::WriteBarrier => Effects::write(MemoryRegion::Heap, SemanticEffects::NONE),
            Op::CallKnown(..) | Op::CallGeneric(_) | Op::CallOp(_) => {
                Effects::for_call()
            }
            // A safepoint polls for the collector but runs no Scheme code:
            // the collector preserves every Scheme value, and finalization
            // runs on the service thread, never at the poll. It therefore
            // changes no memory a load could observe and keeps its call
            // shape for liveness and frame-state purposes.
            Op::Safepoint => Effects::pure(),
            Op::CallPrim { effects, .. } => effects,
        }
    }
}

/// An instruction: its operation, operands, result and frame state.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct InstData {
    pub op: Op,
    pub args: EntityList<Value>,
    pub result: Option<Value>,
    /// The state a deopt or call rebuilds. Set on every instruction that
    /// deopts or calls.
    pub frame: Option<FrameState>,
}

/// A control-flow edge: a target block and the values for its parameters.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Edge {
    pub block: Block,
    pub args: EntityList<Value>,
}

/// The key layout of a [`Terminator::Switch`].
///
/// The builder knows whether the bytecode table was dense (indexed by
/// `key - min`) or sparse (sorted `(key, target)` pairs), and carries that
/// choice so the emitter can pick a jump table instead of a compare chain.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum SwitchDensity {
    /// The cases cover `min..min + cases.len()` exactly, in key order: entry
    /// `i` is the case for key `min + i`. Holes in the bytecode table are
    /// already filled with the default target.
    Dense { min: i32 },
    /// The cases are sorted by key and need a search.
    Sparse,
}

/// The last instruction of a block.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Terminator {
    Jump(Edge),
    Branch {
        cond: Value,
        then: Edge,
        otherwise: Edge,
    },
    Switch {
        value: Value,
        cases: Vec<(i32, Edge)>,
        default: Edge,
        density: SwitchDensity,
    },
    Return(Value),
    /// Returns a fixed number of values other than one (`return-values`):
    /// the values go to `fp[2..]` and the count to `AC`, and the caller's
    /// `%mv-jump` target is entered. Every return address has one, so it
    /// never leaves for a deopt exit.
    ReturnMany {
        values: Vec<Value>,
    },
    /// A call that replaces the current frame. `frame` is the caller's.
    ///
    /// `check` is a profile-named code block the callee is expected to run.
    /// It is not a deopting guard: the emitter compares the closure's block
    /// with this one and takes the generic tail path on a mismatch. A
    /// tail-call frame is never rebuilt, so it cannot host a deopt point
    /// (register 1 already holds the callee while slot 1 is the caller's
    /// closure).
    TailCall {
        callee: Value,
        args: EntityList<Value>,
        frame: FrameState,
        check: Option<CodeRef>,
    },
    /// Leaves compiled code, rebuilding the frames in `frame`.
    Deopt {
        frame: FrameState,
    },
    Unreachable,
}

impl Terminator {
    /// The outgoing edges, in order.
    pub fn edges(&self) -> Vec<Edge> {
        match self {
            Terminator::Jump(edge) => vec![*edge],
            Terminator::Branch {
                then, otherwise, ..
            } => vec![*then, *otherwise],
            Terminator::Switch { cases, default, .. } => cases
                .iter()
                .map(|&(_, edge)| edge)
                .chain(std::iter::once(*default))
                .collect(),
            Terminator::Return(_)
            | Terminator::ReturnMany { .. }
            | Terminator::TailCall { .. }
            | Terminator::Deopt { .. }
            | Terminator::Unreachable => vec![],
        }
    }
}

/// The effect of an operation, a port of the bytecode CFG model in
/// `compiler/cfg/effects.rs` to the JIT's own types.
///
/// Memory ranges describe aliasing. The semantic bits describe dependencies
/// and causes. An operation with a cause must run, even if its result is
/// unused.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Default)]
#[repr(transparent)]
pub struct MemoryRanges(u16);

impl MemoryRanges {
    pub const NONE: Self = Self(0);
    pub const TOP: Self = Self((1 << (MemoryRegion::Unknown as u8 + 1)) - 1);

    pub const fn region(region: MemoryRegion) -> Self {
        Self(1 << region as u8)
    }

    pub const fn is_empty(self) -> bool {
        self.0 == 0
    }

    pub const fn contains(self, other: Self) -> bool {
        self.0 & other.0 == other.0
    }

    pub const fn overlaps(self, other: Self) -> bool {
        self.0 & other.0 != 0
    }

    pub const fn union(self, other: Self) -> Self {
        Self(self.0 | other.0)
    }

    pub const fn without(self, other: Self) -> Self {
        Self(self.0 & !other.0)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Default)]
#[repr(transparent)]
pub struct EffectFlags(u16);

impl EffectFlags {
    pub const NONE: Self = Self(0);
    pub const TERMINAL: Self = Self(1 << 0);
    pub const EXITS_SIDEWAYS: Self = Self(1 << 1);
    pub const CONTROL_DEPENDENT: Self = Self(1 << 2);
    pub const READS_MUTABLE: Self = Self(1 << 3);
    pub const WRITES_LOCAL_STATE: Self = Self(1 << 4);
    pub const READS_LOCAL_STATE: Self = Self(1 << 5);
    pub const READS_PINNED: Self = Self(1 << 6);
    pub const WRITES_PINNED: Self = Self(1 << 7);
    pub const FENCE: Self = Self(1 << 8);
    pub const MAY_CALL: Self = Self(1 << 9);
    pub const MAY_RAISE: Self = Self(1 << 10);
    pub const ALLOCATES: Self = Self(1 << 11);

    pub const fn contains(self, flags: Self) -> bool {
        self.0 & flags.0 == flags.0
    }

    pub const fn union(self, other: Self) -> Self {
        Self(self.0 | other.0)
    }
}

/// Semantic dependencies and causes.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Default)]
#[repr(transparent)]
pub struct SemanticEffects(u32);

impl SemanticEffects {
    pub const NONE: Self = Self(0);
    pub const MUTABLE_LEXICAL: Self = Self(1 << 0);
    pub const TOPLEVEL: Self = Self(1 << 1);
    pub const FLUID: Self = Self(1 << 2);
    pub const DEFINITE_BAILOUT: Self = Self(1 << 3);
    pub const POSSIBLE_BAILOUT: Self = Self(1 << 4);
    pub const ZERO_VALUES: Self = Self(1 << 5);
    pub const ALLOCATION: Self = Self(1 << 6);
    pub const TYPE_CHECK: Self = Self(1 << 7);
    pub const CAR: Self = Self(1 << 8);
    pub const CDR: Self = Self(1 << 9);
    pub const VECTOR: Self = Self(1 << 10);
    pub const VARIABLE: Self = Self(1 << 11);
    pub const STRING: Self = Self(1 << 12);
    pub const BYTEVECTOR: Self = Self(1 << 13);
    pub const UNKNOWN: Self = Self(1 << 14);
    pub const ALL: Self = Self((1 << 15) - 1);

    pub const fn is_empty(self) -> bool {
        self.0 == 0
    }

    pub const fn contains(self, other: Self) -> bool {
        self.0 & other.0 == other.0
    }

    pub const fn union(self, other: Self) -> Self {
        Self(self.0 | other.0)
    }

    pub const fn without(self, other: Self) -> Self {
        Self(self.0 & !other.0)
    }

    pub const fn overlaps(self, other: Self) -> bool {
        self.0 & other.0 != 0
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Default)]
pub struct Effects {
    pub flags: EffectFlags,
    /// Semantic effects this operation reads.
    pub depends_on: SemanticEffects,
    /// Semantic effects this operation has.
    pub causes: SemanticEffects,
    pub reads: MemoryRanges,
    pub writes: MemoryRanges,
}

impl Effects {
    pub const fn pure() -> Self {
        Self {
            flags: EffectFlags::NONE,
            depends_on: SemanticEffects::NONE,
            causes: SemanticEffects::NONE,
            reads: MemoryRanges::NONE,
            writes: MemoryRanges::NONE,
        }
    }

    /// A call, which may run anything.
    pub const fn for_call() -> Self {
        Self {
            flags: EffectFlags::EXITS_SIDEWAYS
                .union(EffectFlags::CONTROL_DEPENDENT)
                .union(EffectFlags::READS_MUTABLE)
                .union(EffectFlags::READS_PINNED)
                .union(EffectFlags::WRITES_PINNED)
                .union(EffectFlags::MAY_CALL)
                .union(EffectFlags::MAY_RAISE)
                .union(EffectFlags::ALLOCATES),
            depends_on: SemanticEffects::ALL,
            causes: SemanticEffects::ALL.without(SemanticEffects::DEFINITE_BAILOUT),
            reads: MemoryRanges::TOP,
            writes: MemoryRanges::TOP,
        }
    }

    /// A primitive call. A primitive may read and write heap and runtime
    /// state, allocate, and raise (deopting to baseline), but it does not run
    /// Scheme code and it does not write a variable cell. Its write set
    /// therefore covers everything except [`MemoryRegion::Variables`], which
    /// is what lets a loop hoist a `load.cell` across it. The builder routes
    /// `variable-set!` through [`Effects::for_call`] instead.
    pub const fn for_primitive() -> Self {
        Self {
            flags: EffectFlags::CONTROL_DEPENDENT
                .union(EffectFlags::READS_MUTABLE)
                .union(EffectFlags::MAY_CALL)
                .union(EffectFlags::MAY_RAISE)
                .union(EffectFlags::ALLOCATES),
            depends_on: SemanticEffects::ALL,
            causes: SemanticEffects::ALL.without(SemanticEffects::DEFINITE_BAILOUT),
            reads: MemoryRanges::TOP,
            writes: MemoryRanges::TOP.without(MemoryRanges::region(MemoryRegion::Variables)),
        }
    }

    pub const fn read(region: MemoryRegion, semantic: SemanticEffects) -> Self {
        Self {
            flags: EffectFlags::CONTROL_DEPENDENT.union(EffectFlags::READS_MUTABLE),
            depends_on: semantic,
            causes: SemanticEffects::NONE,
            reads: MemoryRanges::region(region),
            writes: MemoryRanges::NONE,
        }
    }

    pub const fn write(region: MemoryRegion, semantic: SemanticEffects) -> Self {
        Self {
            flags: EffectFlags::NONE,
            depends_on: SemanticEffects::NONE,
            causes: semantic,
            reads: MemoryRanges::NONE,
            writes: MemoryRanges::region(region),
        }
    }

    /// Adds the type-check cause and the raise flag, for an operation that
    /// may fail a check.
    pub const fn checked(mut self) -> Self {
        self.causes = self.causes.union(SemanticEffects::TYPE_CHECK);
        self.flags = self.flags.union(EffectFlags::MAY_RAISE);
        self
    }

    pub const fn allocate() -> Self {
        Self {
            flags: EffectFlags::ALLOCATES,
            depends_on: SemanticEffects::NONE,
            causes: SemanticEffects::ALLOCATION,
            reads: MemoryRanges::NONE,
            writes: MemoryRanges::region(MemoryRegion::Heap),
        }
    }

    pub const fn union(self, other: Self) -> Self {
        Self {
            flags: self.flags.union(other.flags),
            depends_on: self.depends_on.union(other.depends_on),
            causes: self.causes.union(other.causes),
            reads: self.reads.union(other.reads),
            writes: self.writes.union(other.writes),
        }
    }

    /// True when the operation has no observable cause and can be dropped
    /// when its result is unused. Dependencies alone do not force execution.
    pub const fn effect_free(self) -> bool {
        self.causes.is_empty()
            && !self.flags.contains(
                EffectFlags::TERMINAL
                    .union(EffectFlags::EXITS_SIDEWAYS)
                    .union(EffectFlags::MAY_CALL)
                    .union(EffectFlags::MAY_RAISE)
                    .union(EffectFlags::WRITES_LOCAL_STATE)
                    .union(EffectFlags::WRITES_PINNED)
                    .union(EffectFlags::FENCE),
            )
            && self.writes.is_empty()
    }

    /// True when the operation only allocates a fresh object. Such an
    /// operation can be dropped when its result is unused, since no program
    /// can observe the object.
    pub const fn allocation_only(self) -> bool {
        self.causes.contains(SemanticEffects::ALLOCATION)
            && self.causes.without(SemanticEffects::ALLOCATION).is_empty()
            && !self.flags.contains(
                EffectFlags::TERMINAL
                    .union(EffectFlags::EXITS_SIDEWAYS)
                    .union(EffectFlags::MAY_CALL)
                    .union(EffectFlags::MAY_RAISE)
                    .union(EffectFlags::WRITES_LOCAL_STATE)
                    .union(EffectFlags::WRITES_PINNED)
                    .union(EffectFlags::FENCE),
            )
            && self.reads.is_empty()
            && (self.writes.is_empty()
                || (self
                    .writes
                    .contains(MemoryRanges::region(MemoryRegion::Heap))
                    && self
                        .writes
                        .without(MemoryRanges::region(MemoryRegion::Heap))
                        .is_empty()))
    }

    /// True when reordering `self` and `other` would change behavior.
    pub const fn interferes(self, other: Self) -> bool {
        (self.flags.contains(EffectFlags::TERMINAL)
            && (other.flags.contains(EffectFlags::TERMINAL)
                || other.flags.contains(EffectFlags::CONTROL_DEPENDENT)
                || !other.writes.is_empty()))
            || (other.flags.contains(EffectFlags::TERMINAL)
                && (self.flags.contains(EffectFlags::CONTROL_DEPENDENT) || !self.writes.is_empty()))
            || (self.flags.contains(EffectFlags::EXITS_SIDEWAYS)
                && (!other.writes.is_empty()
                    || other.flags.contains(EffectFlags::CONTROL_DEPENDENT)))
            || (other.flags.contains(EffectFlags::EXITS_SIDEWAYS)
                && (!self.writes.is_empty() || self.flags.contains(EffectFlags::CONTROL_DEPENDENT)))
            || (self.flags.contains(EffectFlags::WRITES_LOCAL_STATE)
                && (other.flags.contains(EffectFlags::WRITES_LOCAL_STATE)
                    || other.flags.contains(EffectFlags::READS_LOCAL_STATE)))
            || (other.flags.contains(EffectFlags::WRITES_LOCAL_STATE)
                && (self.flags.contains(EffectFlags::WRITES_LOCAL_STATE)
                    || self.flags.contains(EffectFlags::READS_LOCAL_STATE)))
            || (self.flags.contains(EffectFlags::WRITES_PINNED)
                && (other.flags.contains(EffectFlags::WRITES_PINNED)
                    || other.flags.contains(EffectFlags::READS_PINNED)))
            || (other.flags.contains(EffectFlags::WRITES_PINNED)
                && (self.flags.contains(EffectFlags::WRITES_PINNED)
                    || self.flags.contains(EffectFlags::READS_PINNED)))
            || self.writes.overlaps(other.writes.union(other.reads))
            || other.writes.overlaps(self.writes.union(self.reads))
            || self.causes.overlaps(other.depends_on)
            || other.causes.overlaps(self.depends_on)
            || (self.flags.contains(EffectFlags::FENCE) && other.flags.contains(EffectFlags::FENCE))
    }
}
