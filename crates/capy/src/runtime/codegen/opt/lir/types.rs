use super::*;
use std::fmt::{self, Display, Formatter};

use cranelift_entity::{PrimaryMap, entity_impl};

use crate::runtime::codegen::opt::ir::frame_state::FrameState;
use crate::runtime::codegen::opt::ir::func::{Block, Value};
use crate::runtime::codegen::opt::ir::inst::{
    Cmp, Constant, HelperRef, PrimRef, RematKind, ReturnValues, SwitchDensity,
};
use crate::runtime::codegen::opt::ir::types::{ClassRef, CodeRef, Type};

/// A virtual register.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VReg(u32);
entity_impl!(VReg, "vr");

/// A LIR block.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LBlock(u32);
entity_impl!(LBlock, "lbb");

/// A deopt exit. Its index is the exit id that the runtime reports.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DeoptExit(u32);
entity_impl!(DeoptExit, "exit");

/// A safepoint: a call or a yieldpoint, and the frame it resumes.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SafePoint(u32);
entity_impl!(SafePoint, "safepoint");

/// A rematerialization descriptor. The value of its IR marker only exists in
/// frame states; the deopt runtime rebuilds the object from `fields`.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RematId(u32);
entity_impl!(RematId, "remat");

pub use crate::runtime::code::GuardKind;

/// The register file a virtual register lives in.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum RegClass {
    Gpr,
    Fpr,
}

impl Display for RegClass {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            RegClass::Gpr => "gpr",
            RegClass::Fpr => "fpr",
        })
    }
}

/// What a virtual register holds. The representation decides its register
/// class, whether a stack map reports it as a GC root, and how a deopt
/// rebuilds it. Every vreg is created with one, and it never changes, except
/// that a [`LirInst::Move`] copies its source's.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum VRepr {
    /// A Scheme word, which is a GC root when spilled. Heap pointers and
    /// immediates both use this, since the collector tells them apart.
    Tagged,
    /// An unboxed int32, held as a full word in a GPR. Not a GC root.
    RawInt,
    /// An unboxed int64, held as a full word in a GPR. Not a GC root. A deopt
    /// reboxes it: a value that fits `i32` becomes a fixnum, and a wider one
    /// allocates a bignum during frame reconstruction.
    RawInt64,
    /// The low limb of an unboxed int128. A 128-bit value is lowered to a
    /// pair of vregs: the low one has this representation and the high one is
    /// an `RawInt64`. The two limbs are independent register-allocator
    /// values (each spends an 8-byte slot when spilled, 16 bytes together),
    /// and the wide instructions take them as explicit `lo`/`hi` operands
    /// instead of relying on a register-pair constraint. Not a GC root, and
    /// never a block parameter or a frame value.
    RawInt128,
    /// A raw machine word in a GPR that is not an int32 or a Scheme value:
    /// a 0/1 flag from a comparison or a type test, or a word read from
    /// memory before it is range-checked. Not a GC root.
    RawBits,
    /// An unboxed double in an FPR. Not a GC root.
    Double,
}

impl VRepr {
    /// The representation of an IR value of type `ty`. Booleans are 0/1
    /// flags. Doubles are unboxed. Int32 is unboxed. Everything else is a
    /// Scheme word.
    pub fn of_type(ty: Type) -> Self {
        match ty {
            Type::Int32 => VRepr::RawInt,
            Type::Int64 => VRepr::RawInt64,
            Type::Int128 => VRepr::RawInt128,
            Type::Double => VRepr::Double,
            Type::Bool => VRepr::RawBits,
            _ => VRepr::Tagged,
        }
    }

    /// The representation of the value a constant holds.
    pub fn of_constant(constant: Constant) -> Self {
        match constant {
            Constant::Int32(_) => VRepr::RawInt,
            Constant::Int64(_) => VRepr::RawInt64,
            Constant::Double(_) => VRepr::Double,
            Constant::Bits(_) | Constant::Table(_) | Constant::Pool { .. } => VRepr::Tagged,
        }
    }

    /// The register class a value of this representation lives in.
    pub fn class(self) -> RegClass {
        match self {
            VRepr::Double => RegClass::Fpr,
            VRepr::Tagged | VRepr::RawInt | VRepr::RawInt64 | VRepr::RawInt128 | VRepr::RawBits => {
                RegClass::Gpr
            }
        }
    }

    /// Whether the value is a word in a GPR that is not a Scheme value.
    pub fn is_raw_word(self) -> bool {
        matches!(
            self,
            VRepr::RawInt | VRepr::RawInt64 | VRepr::RawInt128 | VRepr::RawBits
        )
    }
}

impl Display for VRepr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            VRepr::Tagged => "tagged",
            VRepr::RawInt => "i32",
            VRepr::RawInt64 => "i64",
            VRepr::RawInt128 => "i128",
            VRepr::RawBits => "bits",
            VRepr::Double => "f64",
        })
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct VRegData {
    pub repr: VRepr,
    /// The IR value this register holds, if one maps to it.
    pub value: Option<Value>,
    /// A tagged immediate that frame states rebuild from the constant. Such a
    /// register has no definition, no location and no live range.
    pub imm: Option<Constant>,
    /// A rematerialization marker: the register has no runtime value and no
    /// location. An exit whose frame state names it gets a `Materialize`
    /// location and a descriptor, and its fields are the register's
    /// `LirRematDesc`.
    pub remat: Option<RematId>,
}

/// A rematerialization descriptor in the LIR: what to rebuild, and the vregs
/// holding its fields. A field may itself be a remat marker (a nested sunk
/// object).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LirRematDesc {
    pub kind: RematKind,
    pub fields: Vec<VReg>,
}

/// An integer operation on int32 values. The result overflows to a deopt.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum IntOp {
    Add,
    Sub,
    Mul,
    /// Truncating quotient. Deopts on a zero divisor and on `MIN / -1`.
    Quot,
    /// Remainder with the sign of the dividend. Deopts on a zero divisor.
    Rem,
    /// Modulo with the sign of the divisor. Deopts on a zero divisor.
    Mod,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum FloatOp {
    Add,
    Sub,
    Mul,
    Div,
}

/// A shift of a two-limb integer by an immediate count.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum WideShiftOp {
    /// Shift left, filling with zero.
    Shl,
    /// Shift right, filling with zero.
    Shr,
    /// Shift right, filling with the sign.
    Ashr,
}

/// A type test. A guard branches to its exit when the test fails.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Test {
    Fixnum,
    Flonum,
    Pair,
    /// A character whose code point is a Unicode scalar value (not a
    /// surrogate, at most U+10FFFF).
    Char,
    /// `#t` or `#f`: the two boolean words and nothing else.
    Boolean,
    /// A number: a fixnum, flonum, or a heap object of class `BIGINT`,
    /// `RATIONAL` or `COMPLEX`.
    Number,
    /// A heap object of the class.
    Class(ClassRef),
    /// Any heap object (a cell).
    Heap,
    /// A closure running the code block.
    Code(CodeRef),
    /// The code block of an operand already known to be a closure.
    CodeOfClosure(CodeRef),
    /// The unit has not been invalidated (reads its invalid word).
    NotInvalid,
}

/// The object an allocation creates.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum AllocLayout {
    Pair,
    Flonum,
    Closure {
        code: CodeRef,
        free: u32,
    },
    Env {
        slots: u32,
    },
    Vector {
        len: u32,
    },
    /// A fixed-length internal tuple: payload word 0 is the length, then one
    /// word per element. Records use the same layout.
    Tuple {
        len: u32,
    },
    /// A class instance: a `SchemeInstance` object whose payload is
    /// `[class descriptor, slot storage]`.
    Instance {
        class: u32,
        slots: u32,
    },
    /// The slot storage of a class instance: header `header` (the registered
    /// `SlotStorage` type class), then a word packing the layout `stamp` (low
    /// 32 bits) and `len` (high 32 bits), then `len` values.
    SlotStorage {
        header: u64,
        stamp: u32,
        len: u32,
    },
    /// An exact rational: class 23 with a 32-byte payload of two inline
    /// `Number`s (numerator at 0, denominator at 16). The lowering writes
    /// both components with [`LirInst::StoreNumberTagged`].
    Rational,
}

/// How much a rational-component store knows about the value it writes.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum NumberStoreKind {
    /// Any tagged value: test for a fixnum, then for the `BIGINT` class.
    Any,
    /// A tagged fixnum (an immediate constant or `box.int32`), so the store
    /// writes tag 0 and the sign-extended payload with no test.
    Fixnum,
    /// A fixnum or a one-limb `BIGINT` (`box.int64`), so the store tests the
    /// tag but the wide path needs no class test.
    Number,
}

/// What a call transfers control to.
///
/// For `Known`, `Generic` and `Prim`, the arguments are written to the
/// callee's outgoing frame, starting at slot 1 (`fp[1]`, the closure). The
/// emitter does this with a parallel move, so a tail call may read slots
/// that it overwrites. `Helper` passes its arguments in [`PhysRole::OpArg`]
/// registers instead, at most four.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum CallTarget {
    Known(CodeRef),
    /// The callee is the first argument.
    Generic,
    Helper(HelperRef),
    Prim(PrimRef),
    PrimN(PrimRef),
}

/// An ABI role that a constraint names.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum PhysRole {
    /// The return value register.
    Rv,
    /// Argument `n` of a runtime helper.
    OpArg(u8),
}

/// A constraint on an operand's register.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Constraint {
    Any,
    /// The operand must be in the register of this role.
    Fixed(PhysRole),
    /// A definition that shares the register of operand `n` of the same
    /// instruction. The allocator must honour it on two-address targets and
    /// may use it as a hint elsewhere.
    Tied(usize),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Role {
    Use,
    Def,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Operand {
    pub vreg: VReg,
    pub role: Role,
    pub constraint: Constraint,
}

/// Which registers an instruction destroys.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Clobbers {
    None,
    /// An out-of-line helper that does not collect. It may destroy scratch
    /// registers, so the allocator treats it as clobbering the caller-saved
    /// set unless the slow path saves what it needs.
    Slow,
    /// A call or a safepoint. Every allocatable register is clobbered, and
    /// every live value is spilled before and reloaded after.
    All,
    /// A call into an out-of-line Rust helper (`Helper`, `Prim` or `PrimN`).
    /// The Rust ABI preserves the callee-saved registers, so those survive;
    /// every caller-saved register is clobbered. A value that is a GC root
    /// across a collecting call still needs its slot for the safepoint.
    RustCall,
}

/// A frame value at a deopt exit or a safepoint, and where it lives.
///
/// `ty` tells the runtime how to rebuild the value. An unboxed `Int32` or
/// `Double` is boxed during reconstruction.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct FrameValue {
    pub ir: Value,
    pub vreg: VReg,
    pub ty: Type,
}

/// A deopt exit: the frame state to rebuild, and where each value lives.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DeoptExitData {
    pub frame: FrameState,
    /// In the order of [`Func::frame_values`](crate::runtime::codegen::opt::ir::func::Func::frame_values).
    pub values: Vec<FrameValue>,
    /// The check that failed. Diagnostics only
    pub kind: GuardKind,
}

/// A safepoint's frame and the values that must survive it.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SafePointData {
    pub frame: FrameState,
    pub values: Vec<FrameValue>,
    /// Filled in by the allocator, which owns the stack map. `None` until then.
    pub livemask: Option<LiveMask>,
    /// Filled in by the allocator: the general-purpose registers that hold a
    /// tagged GC root at this safepoint. `None` until then.
    pub regmask: Option<RegMask>,
}

/// The tagged spill slots that are live at a safepoint, one bit per slot.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LiveMask {
    pub words: Vec<u64>,
}

/// The general-purpose registers that hold a tagged GC root at a safepoint,
/// one bit per allocator register id (see `regalloc::PhysReg`). Bit `i` names
/// the register whose id is `i`, in the low bit of word 0.
#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct RegMask {
    pub words: Vec<u64>,
}

impl RegMask {
    /// Adds register id `reg`.
    pub fn set(&mut self, reg: u16) {
        let (word, bit) = (reg as usize / 64, reg as usize % 64);
        if self.words.len() <= word {
            self.words.resize(word + 1, 0);
        }
        self.words[word] |= 1u64 << bit;
    }

    /// Whether register id `reg` is named.
    pub fn get(&self, reg: u16) -> bool {
        self.words
            .get(reg as usize / 64)
            .is_some_and(|w| (w >> (reg as usize % 64)) & 1 == 1)
    }
}

/// An edge to a block, with the values for its parameters.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LEdge {
    pub block: LBlock,
    pub args: Vec<VReg>,
}
