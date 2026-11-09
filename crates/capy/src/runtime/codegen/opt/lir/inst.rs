use super::*;
use std::fmt::{self, Display, Formatter};

/// What a [`LirInst::LoadReturned`] reads from the call just made.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ReturnedKind {
    /// The value itself (a tagged word).
    Value,
    /// The address of the value, as a raw 64-bit integer.
    Addr,
    /// The call's value count (`AC`) less `index`, as a raw int32.
    Count,
}

/// Low-level IR instruction
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum LirInst {
    /// `dst <- src`, within one class.
    Move {
        dst: VReg,
        src: VReg,
    },
    /// `dst <- constant`.
    Const {
        dst: VReg,
        value: Constant,
    },
    /// `dst <- ` the `index`th value in the frame of the call just made,
    /// which is `fp[2 + index]` of the callee frame. Only valid right after
    /// a `ReturnValues::Take` call, before anything else uses that frame.
    LoadReturned {
        dst: VReg,
        index: u32,
        kind: ReturnedKind,
    },
    /// Tags an int32 as a fixnum.
    TagInt {
        dst: VReg,
        src: VReg,
    },
    /// `dst <- src ? #t : #f`, where `src` is a 0/1 flag.
    BoolToValue {
        dst: VReg,
        src: VReg,
    },
    /// Untags a fixnum to an int32.
    UntagInt {
        dst: VReg,
        src: VReg,
    },
    /// Reads the code point of a character into an int32. The operand must
    /// have passed [`Test::Char`].
    UntagChar {
        dst: VReg,
        src: VReg,
    },
    /// Branches to `exit` unless `test` holds for `src`.
    Guard {
        test: Test,
        src: Option<VReg>,
        exit: DeoptExit,
    },
    /// Branches to `exit` unless `lhs cmp rhs` holds. Both operands are
    /// int32 or word values, and the comparison is signed.
    GuardCmp {
        cmp: Cmp,
        lhs: VReg,
        rhs: VReg,
        exit: DeoptExit,
    },
    /// Branches to `exit` unless `index < length` holds unsigned. The index
    /// is an already-untagged fixnum, so a negative value is a huge unsigned
    /// word and fails
    GuardIndex {
        index: VReg,
        length: VReg,
        exit: DeoptExit,
    },
    /// `dst <- test(src)`, as 0 or 1 in a GPR.
    TestTag {
        dst: VReg,
        src: VReg,
        test: Test,
    },
    /// Int32 arithmetic that branches to `exit` on overflow.
    IntArith {
        op: IntOp,
        dst: VReg,
        lhs: VReg,
        rhs: VReg,
        exit: DeoptExit,
    },
    /// Int64 arithmetic that branches to `exit` on overflow (`Add`, `Sub`,
    /// `Mul`) or a zero/overflowing divisor (`Quot`, `Rem`, `Mod`).
    Int64Arith {
        op: IntOp,
        dst: VReg,
        lhs: VReg,
        rhs: VReg,
        exit: DeoptExit,
    },
    /// Total int64 add, subtract or multiply.
    Int64Wrapping {
        op: IntOp,
        dst: VReg,
        lhs: VReg,
        rhs: VReg,
    },
    /// Tags an int64 as a fixnum when it fits `i32`, else allocates a one-limb
    /// `BIGINT` with `count = 1`, `negative = src < 0` and the magnitude as
    /// its limb.
    BoxInt64 {
        dst: VReg,
        src: VReg,
    },
    /// Guards that `obj` is a fixnum (whose payload it sign-extends) or a
    /// `BIGINT` of at most one limb whose signed value fits i64, and produces
    /// that value. Deopts before the instruction otherwise.
    UnboxBigInt64 {
        dst: VReg,
        obj: VReg,
        exit: DeoptExit,
    },
    /// Untags a fixnum and sign-extends its payload to int64. The operand must
    /// have passed [`Test::Fixnum`].
    UntagInt64 {
        dst: VReg,
        src: VReg,
    },
    /// The low 32 bits of a raw int64.
    TruncateI64ToI32 {
        dst: VReg,
        src: VReg,
    },
    /// `dst <- lhs cmp rhs`, as 0 or 1 in a GPR. Compares full words.
    IntCmp {
        cmp: Cmp,
        dst: VReg,
        lhs: VReg,
        rhs: VReg,
    },
    FloatArith {
        op: FloatOp,
        dst: VReg,
        lhs: VReg,
        rhs: VReg,
    },
    /// `dst <- (double) src`, for a raw int32 `src`. Exact.
    IntToDouble {
        dst: VReg,
        src: VReg,
    },
    /// `dst <- (double) src`, for a raw int64 `src`. Rounded to nearest.
    Int64ToDouble {
        dst: VReg,
        src: VReg,
    },
    /// `dst <- floor(src)`, both unboxed doubles.
    FloorF64 {
        dst: VReg,
        src: VReg,
    },
    /// `dst <- ceiling(src)`, both unboxed doubles.
    CeilingF64 {
        dst: VReg,
        src: VReg,
    },
    /// `dst <- truncate(src)`, both unboxed doubles.
    TruncateF64 {
        dst: VReg,
        src: VReg,
    },
    /// `dst <- sqrt(src)`, both unboxed doubles.
    SqrtF64 {
        dst: VReg,
        src: VReg,
    },
    /// `dst <- lhs cmp rhs`, as 0 or 1 in a GPR.
    FloatCmp {
        cmp: Cmp,
        dst: VReg,
        lhs: VReg,
        rhs: VReg,
    },
    /// Reads the payload of a flonum into an FPR.
    LoadFlonum {
        dst: VReg,
        obj: VReg,
    },
    /// Writes the payload of a freshly allocated flonum.
    StoreFlonum {
        obj: VReg,
        src: VReg,
    },
    /// Boxes an unboxed double into a Scheme word
    BoxDouble {
        dst: VReg,
        src: VReg,
    },
    /// Allocates an object from the LAB.
    Alloc {
        dst: VReg,
        layout: AllocLayout,
    },
    /// Allocates a vector of the tagged fixnum `size` and fills it with
    /// `fill`. The guards and the allocation slow path branch to `exit`.
    AllocVectorFill {
        dst: VReg,
        size: VReg,
        fill: VReg,
        exit: DeoptExit,
    },
    /// `dst <- *(base + offset)`, with `offset` in bytes.
    Load {
        dst: VReg,
        base: VReg,
        offset: i32,
    },
    /// `*(base + offset) <- src`, with `offset` in bytes.
    Store {
        base: VReg,
        offset: i32,
        src: VReg,
    },
    LoadField {
        dst: VReg,
        obj: VReg,
        field: u32,
    },
    StoreField {
        obj: VReg,
        field: u32,
        src: VReg,
    },
    /// Writes one inline `Number` component (16 bytes) of a fresh rational at
    /// `offset` bytes from the object base: 0 for the numerator, 16 for the
    /// denominator.
    StoreNumberTagged {
        obj: VReg,
        offset: u32,
        src: VReg,
        kind: NumberStoreKind,
        exit: DeoptExit,
    },
    /// Reads one inline `Number` component of a checked rational at `offset`
    /// bytes (0 numerator, 16 denominator) as a raw `i64`: the tag word must
    /// be 0 (`TAG_FIXNUM`) and the low 32 bits of the payload are
    /// sign-extended. Any other tag deopts before the instruction.
    LoadNumberFixnum {
        dst: VReg,
        obj: VReg,
        offset: u32,
        exit: DeoptExit,
    },

    LoadNumberWide {
        dst: VReg,
        obj: VReg,
        offset: u32,
        exit: DeoptExit,
    },

    /// `dst = lhs * rhs`, sign-extended to two limbs: `dst_lo` (an
    /// `RawInt128` pair head) and `dst_hi`. Total.
    MulWide64 {
        dst_lo: VReg,
        dst_hi: VReg,
        lhs: VReg,
        rhs: VReg,
    },
    /// Two's-complement `dst = lhs + rhs` over two limbs. Branches to `exit`
    /// when the exact sum leaves the i128 range.
    WideAdd {
        dst_lo: VReg,
        dst_hi: VReg,
        lhs_lo: VReg,
        lhs_hi: VReg,
        rhs_lo: VReg,
        rhs_hi: VReg,
        exit: DeoptExit,
    },
    /// Two's-complement `dst = lhs - rhs` over two limbs. Branches to `exit`
    /// on i128 overflow.
    WideSub {
        dst_lo: VReg,
        dst_hi: VReg,
        lhs_lo: VReg,
        lhs_hi: VReg,
        rhs_lo: VReg,
        rhs_hi: VReg,
        exit: DeoptExit,
    },
    /// `dst = lhs cmp rhs`, as 0 or 1 in a GPR. Signed 128-bit comparison:
    /// the high limbs compare signed and equal high limbs compare the low
    /// limbs unsigned.
    WideCmp {
        cmp: Cmp,
        dst: VReg,
        lhs_lo: VReg,
        lhs_hi: VReg,
        rhs_lo: VReg,
        rhs_hi: VReg,
    },
    /// `dst = src << amount` (or a right shift) over two limbs, with a
    /// constant amount of at most 127.
    WideShift {
        op: WideShiftOp,
        dst_lo: VReg,
        dst_hi: VReg,
        src_lo: VReg,
        src_hi: VReg,
        amount: u32,
    },
    /// `dst_lo = src`, `dst_hi = sign(src)`: sign-extends an i64 to two
    /// limbs.
    WideExtend {
        dst_lo: VReg,
        dst_hi: VReg,
        src: VReg,
    },
    /// `dst = src_lo` (`high == false`) or `dst = src_hi`, splitting a
    /// two-limb value into its halves.
    WideSplit {
        dst: VReg,
        src_lo: VReg,
        src_hi: VReg,
        high: bool,
    },
    /// Tags a two-limb value as a fixnum when it fits `i32`, otherwise
    /// allocates a one-limb `BIGINT` when it fits `i64`, otherwise a two-limb
    /// sign-magnitude `BIGINT` (`count = 2`, normalized high limb).
    /// [`LirInst::BoxInt64`].
    BoxInt128 {
        dst: VReg,
        src_lo: VReg,
        src_hi: VReg,
    },
    /// Reads free variable `index` of a closure.
    LoadFree {
        dst: VReg,
        closure: VReg,
        index: u32,
    },
    /// Writes free variable `index` of a closure.
    StoreFree {
        closure: VReg,
        index: u32,
        src: VReg,
    },
    LoadCell {
        dst: VReg,
        cell: VReg,
    },
    StoreCell {
        cell: VReg,
        src: VReg,
    },
    /// Records that `obj` now refers to `value`.
    WriteBarrier {
        obj: VReg,
        value: VReg,
    },
    /// `dst <- vector[index]`, where `index` is an untagged int32 that was
    /// checked against the length.
    LoadElement {
        dst: VReg,
        vector: VReg,
        index: VReg,
    },
    /// `vector[index] <- src`, where `index` is an untagged int32 that was
    /// checked against the length. Needs a [`LirInst::WriteBarrier`] before it.
    StoreElement {
        vector: VReg,
        index: VReg,
        src: VReg,
    },
    /// `dst <- byte(contents + index)`, zero-extended. `base` is a
    /// bytevector's contents pointer and `index` is an untagged int32 checked
    /// against the length.
    LoadByte {
        dst: VReg,
        base: VReg,
        index: VReg,
    },
    /// Reads character `index` of a checked narrow string and tags it. A wide
    /// string (or a non-string) branches to `exit`; the bounds were checked
    /// by `CheckStringIndex`.
    StringRef {
        dst: VReg,
        string: VReg,
        index: VReg,
        exit: DeoptExit,
    },
    /// `byte(contents + index) <- src`, the low 8 bits of `src`.
    StoreByte {
        base: VReg,
        index: VReg,
        src: VReg,
    },
    /// `dst <- obj.slot`, through the inline entry of the `get_slot` site at
    /// `meta`. Branches to `exit` unless the entry matches `obj` (class and
    /// layout stamp) and the slot is bound. See `emit/slots.rs`.
    SlotGet {
        dst: VReg,
        obj: VReg,
        meta: u64,
        exit: DeoptExit,
    },
    /// `obj.slot <- src`, through the `put_slot` site at `meta`, with the
    /// write barrier on the slot storage. Branches to `exit` before any store
    /// unless the entry matches. Its barrier slow path clobbers like
    /// [`LirInst::WriteBarrier`]'s.
    SlotPut {
        obj: VReg,
        src: VReg,
        meta: u64,
        exit: DeoptExit,
    },
    /// A call. `args` follow the rules on [`CallTarget`]. `rv` is the
    /// return value, which is in the [`PhysRole::Rv`] register.
    Call {
        target: CallTarget,
        args: Vec<VReg>,
        rv: Option<VReg>,
        safepoint: SafePoint,
        /// `Prim` only: the deopt exit taken when the primitive reports an
        /// error. It runs before the instruction, so baseline re-runs the
        /// call and raises from there. It has the safepoint's
        /// frame and values.
        exit: Option<DeoptExit>,
        /// `Known` and `Generic` only: how the return point takes a callee
        /// that returns other than one value.
        values: ReturnValues,
    },
    /// A yieldpoint, where GC may run.
    Poll {
        safepoint: SafePoint,
    },
}

/// The last instruction of a block.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum LirTerm {
    Jump(LEdge),
    Branch {
        /// A GPR holding 0 or 1.
        cond: VReg,
        then: LEdge,
        otherwise: LEdge,
    },
    Switch {
        value: VReg,
        cases: Vec<(i32, LEdge)>,
        default: LEdge,
        /// The key layout the builder saw: a dense table indexes the cases
        /// directly, a sparse one searches them.
        density: crate::runtime::codegen::opt::ir::inst::SwitchDensity,
    },
    Return {
        value: VReg,
    },
    /// Returns `values.len()` values other than one through the caller's
    /// multiple-values target. Every return address has one, so it never
    /// leaves the function.
    ReturnMany {
        values: Vec<VReg>,
    },
    /// Replaces the current frame.
    TailCall {
        target: CallTarget,
        args: Vec<VReg>,
        checked: bool,
    },
    Deopt {
        exit: DeoptExit,
    },
    Unreachable,
}

pub(super) fn use_any(vreg: VReg) -> Operand {
    Operand {
        vreg,
        role: Role::Use,
        constraint: Constraint::Any,
    }
}

pub(super) fn def_with(vreg: VReg, constraint: Constraint) -> Operand {
    Operand {
        vreg,
        role: Role::Def,
        constraint,
    }
}

pub(super) fn def_any(vreg: VReg) -> Operand {
    def_with(vreg, Constraint::Any)
}

impl LirInst {
    /// Every operand, uses before defs, with its constraint. Frame values of
    /// a deopt exit or a safepoint are included as uses, so `func` supplies
    /// the tables they live in.
    pub fn operands(&self, func: &LirFunc) -> Vec<Operand> {
        let mut ops = Vec::new();
        match self {
            LirInst::Move { dst, src } => {
                ops.push(use_any(*src));
                ops.push(def_any(*dst));
            }
            LirInst::Const { dst, .. } | LirInst::LoadReturned { dst, .. } => {
                ops.push(def_any(*dst))
            }
            LirInst::TagInt { dst, src }
            | LirInst::UntagInt { dst, src }
            | LirInst::UntagChar { dst, src }
            | LirInst::TestTag { dst, src, .. }
            | LirInst::IntToDouble { dst, src }
            | LirInst::Int64ToDouble { dst, src }
            | LirInst::FloorF64 { dst, src }
            | LirInst::CeilingF64 { dst, src }
            | LirInst::TruncateF64 { dst, src }
            | LirInst::SqrtF64 { dst, src }
            | LirInst::BoolToValue { dst, src } => {
                ops.push(use_any(*src));
                ops.push(def_any(*dst));
            }
            LirInst::Guard { src, exit, .. } => {
                if let Some(src) = src {
                    ops.push(use_any(*src));
                }
                ops.extend(func.frame_ops(&func.exits[*exit].values));
            }
            LirInst::GuardCmp { lhs, rhs, exit, .. } => {
                ops.push(use_any(*lhs));
                ops.push(use_any(*rhs));
                ops.extend(func.frame_ops(&func.exits[*exit].values));
            }
            LirInst::GuardIndex {
                index,
                length,
                exit,
            } => {
                ops.push(use_any(*index));
                ops.push(use_any(*length));
                ops.extend(func.frame_ops(&func.exits[*exit].values));
            }
            LirInst::IntArith {
                dst,
                lhs,
                rhs,
                exit,
                ..
            } => {
                ops.push(use_any(*lhs));
                ops.push(use_any(*rhs));
                ops.push(def_any(*dst));

                ops.extend(func.frame_ops(&func.exits[*exit].values));
            }
            LirInst::Int64Arith {
                dst,
                lhs,
                rhs,
                exit,
                ..
            } => {
                ops.push(use_any(*lhs));
                ops.push(use_any(*rhs));
                // The emitter computes through its own GPR temporary, so the
                // destination is a fresh register rather than a two-address
                // tie.
                ops.push(def_any(*dst));
                ops.extend(func.frame_ops(&func.exits[*exit].values));
            }
            LirInst::Int64Wrapping { dst, lhs, rhs, .. } => {
                ops.push(use_any(*lhs));
                ops.push(use_any(*rhs));
                ops.push(def_any(*dst));
            }
            LirInst::BoxInt64 { dst, src } => {
                ops.push(use_any(*src));
                ops.push(def_any(*dst));
            }
            LirInst::UnboxBigInt64 { dst, obj, exit } => {
                ops.push(use_any(*obj));
                ops.extend(func.frame_ops(&func.exits[*exit].values));
                ops.push(def_any(*dst));
            }
            LirInst::UntagInt64 { dst, src } => {
                ops.push(use_any(*src));
                ops.push(def_any(*dst));
            }
            LirInst::TruncateI64ToI32 { dst, src } => {
                ops.push(use_any(*src));
                ops.push(def_any(*dst));
            }
            LirInst::FloatArith { dst, lhs, rhs, .. } => {
                ops.push(use_any(*lhs));
                ops.push(use_any(*rhs));
                ops.push(def_with(*dst, Constraint::Tied(0)));
            }
            LirInst::IntCmp { dst, lhs, rhs, .. } | LirInst::FloatCmp { dst, lhs, rhs, .. } => {
                ops.push(use_any(*lhs));
                ops.push(use_any(*rhs));
                ops.push(def_any(*dst));
            }
            LirInst::LoadFlonum { dst, obj } | LirInst::LoadField { dst, obj, .. } => {
                ops.push(use_any(*obj));
                ops.push(def_any(*dst));
            }
            LirInst::LoadNumberFixnum { dst, obj, exit, .. }
            | LirInst::LoadNumberWide { dst, obj, exit, .. } => {
                ops.push(use_any(*obj));
                ops.extend(func.frame_ops(&func.exits[*exit].values));
                ops.push(def_any(*dst));
            }
            LirInst::MulWide64 {
                dst_lo,
                dst_hi,
                lhs,
                rhs,
            } => {
                ops.push(use_any(*lhs));
                ops.push(use_any(*rhs));
                ops.push(def_any(*dst_lo));
                ops.push(def_any(*dst_hi));
            }
            LirInst::WideAdd {
                dst_lo,
                dst_hi,
                lhs_lo,
                lhs_hi,
                rhs_lo,
                rhs_hi,
                exit,
            }
            | LirInst::WideSub {
                dst_lo,
                dst_hi,
                lhs_lo,
                lhs_hi,
                rhs_lo,
                rhs_hi,
                exit,
            } => {
                ops.push(use_any(*lhs_lo));
                ops.push(use_any(*lhs_hi));
                ops.push(use_any(*rhs_lo));
                ops.push(use_any(*rhs_hi));
                ops.extend(func.frame_ops(&func.exits[*exit].values));
                ops.push(def_any(*dst_lo));
                ops.push(def_any(*dst_hi));
            }
            LirInst::WideCmp {
                dst,
                lhs_lo,
                lhs_hi,
                rhs_lo,
                rhs_hi,
                ..
            } => {
                ops.push(use_any(*lhs_lo));
                ops.push(use_any(*lhs_hi));
                ops.push(use_any(*rhs_lo));
                ops.push(use_any(*rhs_hi));
                ops.push(def_any(*dst));
            }
            LirInst::WideShift {
                dst_lo,
                dst_hi,
                src_lo,
                src_hi,
                ..
            } => {
                ops.push(use_any(*src_lo));
                ops.push(use_any(*src_hi));
                ops.push(def_any(*dst_lo));
                ops.push(def_any(*dst_hi));
            }
            LirInst::WideExtend {
                dst_lo,
                dst_hi,
                src,
            } => {
                ops.push(use_any(*src));
                ops.push(def_any(*dst_lo));
                ops.push(def_any(*dst_hi));
            }
            LirInst::WideSplit {
                dst,
                src_lo,
                src_hi,
                ..
            } => {
                ops.push(use_any(*src_lo));
                ops.push(use_any(*src_hi));
                ops.push(def_any(*dst));
            }
            LirInst::BoxInt128 {
                dst,
                src_lo,
                src_hi,
            } => {
                ops.push(use_any(*src_lo));
                ops.push(use_any(*src_hi));
                ops.push(def_any(*dst));
            }
            LirInst::StoreFlonum { obj, src } | LirInst::StoreField { obj, src, .. } => {
                ops.push(use_any(*obj));
                ops.push(use_any(*src));
            }
            LirInst::StoreNumberTagged { obj, src, exit, .. } => {
                ops.push(use_any(*obj));
                ops.push(use_any(*src));
                ops.extend(func.frame_ops(&func.exits[*exit].values));
            }
            LirInst::BoxDouble { dst, src } => {
                ops.push(use_any(*src));
                ops.push(def_any(*dst));
            }
            LirInst::Alloc { dst, .. } => ops.push(def_any(*dst)),
            LirInst::AllocVectorFill {
                dst,
                size,
                fill,
                exit,
            } => {
                ops.push(use_any(*size));
                ops.push(use_any(*fill));
                ops.push(def_any(*dst));
                ops.extend(func.frame_ops(&func.exits[*exit].values));
            }
            LirInst::Load { dst, base, .. } => {
                ops.push(use_any(*base));
                ops.push(def_any(*dst));
            }
            LirInst::Store { base, src, .. } => {
                ops.push(use_any(*base));
                ops.push(use_any(*src));
            }
            LirInst::LoadFree { dst, closure, .. } => {
                ops.push(use_any(*closure));
                ops.push(def_any(*dst));
            }
            LirInst::StoreFree { closure, src, .. } => {
                ops.push(use_any(*closure));
                ops.push(use_any(*src));
            }
            LirInst::LoadCell { dst, cell } => {
                ops.push(use_any(*cell));
                ops.push(def_any(*dst));
            }
            LirInst::StoreCell { cell, src } => {
                ops.push(use_any(*cell));
                ops.push(use_any(*src));
            }
            LirInst::WriteBarrier { obj, value } => {
                ops.push(use_any(*obj));
                ops.push(use_any(*value));
            }
            LirInst::LoadElement { dst, vector, index } => {
                ops.push(use_any(*vector));
                ops.push(use_any(*index));
                ops.push(def_any(*dst));
            }
            LirInst::StoreElement { vector, index, src } => {
                ops.push(use_any(*vector));
                ops.push(use_any(*index));
                ops.push(use_any(*src));
            }
            LirInst::LoadByte { dst, base, index } => {
                ops.push(use_any(*base));
                ops.push(use_any(*index));
                ops.push(def_any(*dst));
            }
            LirInst::StringRef {
                dst,
                string,
                index,
                exit,
            } => {
                ops.push(use_any(*string));
                ops.push(use_any(*index));
                ops.push(def_any(*dst));
                ops.extend(func.frame_ops(&func.exits[*exit].values));
            }
            LirInst::StoreByte { base, index, src } => {
                ops.push(use_any(*base));
                ops.push(use_any(*index));
                ops.push(use_any(*src));
            }
            LirInst::SlotGet { dst, obj, exit, .. } => {
                ops.push(use_any(*obj));
                ops.extend(func.frame_ops(&func.exits[*exit].values));
                ops.push(def_any(*dst));
            }
            LirInst::SlotPut { obj, src, exit, .. } => {
                ops.push(use_any(*obj));
                ops.push(use_any(*src));
                ops.extend(func.frame_ops(&func.exits[*exit].values));
            }
            LirInst::Call {
                target,
                args,
                rv,
                safepoint,
                ..
            } => {
                for (i, &arg) in args.iter().enumerate() {
                    let constraint = match target {
                        CallTarget::Helper(_) => Constraint::Fixed(PhysRole::OpArg(i as u8)),
                        CallTarget::Prim(_) => Constraint::Fixed(PhysRole::OpArg(i as u8 + 1)),
                        _ => Constraint::Any,
                    };
                    ops.push(Operand {
                        vreg: arg,
                        role: Role::Use,
                        constraint,
                    });
                }
                ops.extend(func.frame_ops(&func.safepoints[*safepoint].values));
                if let Some(rv) = rv {
                    ops.push(def_with(*rv, Constraint::Fixed(PhysRole::Rv)));
                }
            }
            LirInst::Poll { safepoint } => {
                ops.extend(func.frame_ops(&func.safepoints[*safepoint].values));
            }
        }
        ops
    }

    /// What the instruction destroys.
    pub fn clobbers(&self) -> Clobbers {
        match self {
            // A compiled Scheme callee is entered directly and does not save
            // the callee-saved registers (its prologue does not), so a frame
            // call still clobbers the whole pool. A Rust helper follows the
            // target C ABI, which preserves them.
            LirInst::Call { target, .. } => match target {
                CallTarget::Helper(_) | CallTarget::Prim(_) | CallTarget::PrimN(_) => {
                    Clobbers::RustCall
                }
                CallTarget::Known(_) | CallTarget::Generic => Clobbers::All,
            },
            // The yieldpoint's slow path saves the live registers to its save
            // slots and restores them after the yield, so the fast path destroys nothing.
            LirInst::Poll { .. } => Clobbers::None,
            // The allocation slow path never collects and saves every live
            // caller-saved pool register, so the fast path destroys nothing.
            LirInst::Alloc { .. } | LirInst::AllocVectorFill { .. } => Clobbers::None,
            LirInst::BoxInt64 { .. } | LirInst::BoxInt128 { .. } => Clobbers::None,
            // The wide instructions compute in emitter scratch registers and
            // save/restore the fixed GPR pair they use, so no pool value is
            // destroyed.
            LirInst::MulWide64 { .. }
            | LirInst::WideAdd { .. }
            | LirInst::WideSub { .. }
            | LirInst::WideCmp { .. }
            | LirInst::WideShift { .. }
            | LirInst::WideExtend { .. }
            | LirInst::WideSplit { .. } => Clobbers::None,
            LirInst::WriteBarrier { .. } | LirInst::SlotPut { .. } => Clobbers::Slow,
            _ => Clobbers::None,
        }
    }

    /// The deopt exit this instruction branches to on failure, if any.
    pub fn deopt_exit(&self) -> Option<DeoptExit> {
        match self {
            LirInst::Guard { exit, .. }
            | LirInst::GuardCmp { exit, .. }
            | LirInst::GuardIndex { exit, .. }
            | LirInst::AllocVectorFill { exit, .. }
            | LirInst::StringRef { exit, .. }
            | LirInst::IntArith { exit, .. }
            | LirInst::Int64Arith { exit, .. }
            | LirInst::UnboxBigInt64 { exit, .. }
            | LirInst::StoreNumberTagged { exit, .. }
            | LirInst::LoadNumberFixnum { exit, .. }
            | LirInst::LoadNumberWide { exit, .. }
            | LirInst::WideAdd { exit, .. }
            | LirInst::WideSub { exit, .. }
            | LirInst::SlotGet { exit, .. }
            | LirInst::SlotPut { exit, .. } => Some(*exit),
            LirInst::Call { exit, .. } => *exit,
            _ => None,
        }
    }

    /// The safepoint of a call or a yieldpoint, if this is one.
    pub fn safepoint(&self) -> Option<SafePoint> {
        match self {
            LirInst::Call { safepoint, .. } | LirInst::Poll { safepoint } => Some(*safepoint),
            _ => None,
        }
    }
}

impl LirTerm {
    /// The outgoing edges, in order.
    pub fn edges(&self) -> Vec<&LEdge> {
        match self {
            LirTerm::Jump(edge) => vec![edge],
            LirTerm::Branch {
                then, otherwise, ..
            } => vec![then, otherwise],
            LirTerm::Switch { cases, default, .. } => cases
                .iter()
                .map(|(_, edge)| edge)
                .chain(std::iter::once(default))
                .collect(),
            LirTerm::Return { .. }
            | LirTerm::ReturnMany { .. }
            | LirTerm::TailCall { .. }
            | LirTerm::Deopt { .. }
            | LirTerm::Unreachable => vec![],
        }
    }

    /// Every operand the terminator reads, including edge arguments and the
    /// frame values of a deopt exit. See [`LirInst::operands`].
    pub fn operands(&self, func: &LirFunc) -> Vec<Operand> {
        let mut ops = Vec::new();
        match self {
            LirTerm::Jump(_) => {}
            LirTerm::Branch { cond, .. } => ops.push(use_any(*cond)),
            LirTerm::Switch { value, .. } => ops.push(use_any(*value)),
            LirTerm::Return { value } => ops.push(Operand {
                vreg: *value,
                role: Role::Use,
                constraint: Constraint::Fixed(PhysRole::Rv),
            }),
            LirTerm::ReturnMany { values } => {
                ops.extend(values.iter().map(|&value| use_any(value)));
            }
            LirTerm::TailCall { target, args, .. } => {
                for (i, &arg) in args.iter().enumerate() {
                    let constraint = match target {
                        CallTarget::Helper(_) => Constraint::Fixed(PhysRole::OpArg(i as u8)),
                        _ => Constraint::Any,
                    };
                    ops.push(Operand {
                        vreg: arg,
                        role: Role::Use,
                        constraint,
                    });
                }
            }
            LirTerm::Deopt { exit } => {
                ops.extend(func.frame_ops(&func.exits[*exit].values));
            }
            LirTerm::Unreachable => {}
        }
        for edge in self.edges() {
            ops.extend(edge.args.iter().map(|&v| use_any(v)));
        }
        ops
    }
}
