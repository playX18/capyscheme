use crate::bytecode::def::{self, OpcodeId};
use crate::heap::Gc;
use crate::runtime::codegen::jit::context::DecodedInstruction;
use crate::runtime::codegen::template::ArithShape;
use crate::runtime::codegen::template::metadata_kind;
use crate::runtime::interp::profile::{
    ARITH_RHS_SHIFT, ARITH_WIDE_RATIONAL, NUMBER_BIGNUM, NUMBER_COMPLEX, NUMBER_FIXNUM,
    NUMBER_FLONUM, NUMBER_KIND_MASK, NUMBER_NON_NUMBER, NUMBER_RATNUM,
};
use crate::runtime::interp::profile::{CALL_IC_CASES, CallLinkInfo, LINK_MEGA, LINK_POLY};
use crate::runtime::objectmodel::{CodeBlock, CodeBuffer, Value as SchemeValue};
use std::sync::atomic::{AtomicU32, Ordering::Relaxed};

/// Which profile word a metadata field holds.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ProfileKind {
    Arith,
    Value,
    Array,
}

impl ProfileKind {
    /// The kind of a metadata field declared with the profile type `ty`.
    fn of_type(ty: &str) -> Option<Self> {
        match ty {
            "ArithProfile" => Some(Self::Arith),
            "ValueProfile" => Some(Self::Value),
            "ArrayProfile" => Some(Self::Array),
            _ => None,
        }
    }
}

macro_rules! profile_fields_table {
    ($(($Op:ident, $Meta:ident, $kind:expr, { $($field:ident : $ty:ty),* }),)*) => {
        #[allow(unused_assignments)]
        fn profile_fields(op: OpcodeId) -> Vec<(usize, ProfileKind)> {
            match op {
                $(OpcodeId::$Op => {
                    let mut out = Vec::new();
                    let mut field = 0;
                    $(
                        if let Some(kind) = ProfileKind::of_type(stringify!($ty)) {
                            out.push((field, kind));
                        }
                        field += 1;
                    )*
                    out
                })*
                _ => Vec::new(),
            }
        }
    };
}

def::for_each_metadata!(profile_fields_table);

/// The instruction at `pc` of `buffer`
pub fn decode(buffer: &CodeBuffer<'_>, pc: usize) -> DecodedInstruction {
    let entry = buffer.code().as_ptr();
    let md = buffer.metadata();
    // SAFETY: `pc` starts a decoded instruction within the code.
    let (width, op, _) = unsafe { def::decode_header(entry.add(pc)) };
    let len = unsafe { def::Instruction::size_at_ptr(entry.add(pc)) };
    DecodedInstruction {
        op,
        width,
        ip: unsafe { entry.add(pc) },
        len,
        entry,
        constant_pointer: entry as usize,
        md: md as usize,
    }
}

/// Every profile word of the instruction at `pc`
pub fn profile_words(buffer: &CodeBuffer<'_>, pc: usize) -> Vec<(ProfileKind, u32)> {
    if buffer.metadata().is_null() {
        return Vec::new();
    }
    let insn = decode(buffer, pc);
    profile_fields(insn.op)
        .into_iter()
        .map(|(field, kind)| {
            let addr = insn.metadata_addr(metadata_kind(insn.op), field);
            // SAFETY: `addr` is this site's profile word in the live metadata
            // area, which never moves, and profile words are atomic.
            let word = unsafe { &*(addr as *const AtomicU32) }.load(Relaxed);
            (kind, word)
        })
        .collect()
}

/// One callee of a call site
#[derive(Clone, Copy)]
pub struct CallTarget<'gc> {
    pub callee: Gc<'gc, CodeBlock<'gc>>,
    pub hits: u32,
}

/// What a call site's link held when it was read.
#[derive(Clone)]
pub enum CallShape<'gc> {
    /// [`LINK_MONO`](crate::runtime::interp::profile::LINK_MONO): the one
    /// callee, `None` when the site never called a procedure.
    Mono(Option<CallTarget<'gc>>),
    /// [`LINK_POLY`]: the callees the call IC stub enters directly, ordered
    /// by how often they were entered (hottest first).
    Poly(Vec<CallTarget<'gc>>),
    /// [`LINK_MEGA`]: more callees than the stub holds.
    Mega,
}

/// The call shape of the call-like instruction at `pc`, or `None` when `pc`
/// is not a call.
///
/// The inliner reads it to pick the callee(s) to inline
pub fn call_shape<'gc>(buffer: &CodeBuffer<'gc>, pc: usize) -> Option<CallShape<'gc>> {
    if buffer.metadata().is_null() {
        return None;
    }
    let insn = decode(buffer, pc);
    if !matches!(
        insn.op,
        OpcodeId::OpCall | OpcodeId::OpTailCall | OpcodeId::OpCallSelf | OpcodeId::OpTailCallSelf
    ) {
        return None;
    }
    // A call-like op has its link as metadata field 0.
    let addr = insn.metadata_addr(metadata_kind(insn.op), 0);
    // SAFETY: `addr` is this site's `CallLinkInfo` in the live metadata area,
    // which never moves.
    let link = unsafe { &*(addr as *const CallLinkInfo) };
    let site_hits = link.hits.load(Relaxed);
    Some(match link.polymorphic.load(Relaxed) {
        LINK_POLY => {
            let mut targets: Vec<CallTarget<'gc>> = link.cases[..CALL_IC_CASES]
                .iter()
                .enumerate()
                .filter_map(|(index, case)| {
                    let callee = case.get()?;
                    let hits = if index == 0 && link.case_hits[0].load(Relaxed) == 0 {
                        site_hits
                    } else {
                        link.case_hits[index].load(Relaxed)
                    };
                    Some(CallTarget { callee, hits })
                })
                .collect();

            if let Some(callee) = link.callee.get()
                && !targets.iter().any(|target| {
                    SchemeValue::from(target.callee).bits() == SchemeValue::from(callee).bits()
                })
            {
                targets.push(CallTarget {
                    callee,
                    hits: site_hits,
                });
            }
            targets.sort_by(|a, b| b.hits.cmp(&a.hits));
            CallShape::Poly(targets)
        }
        LINK_MEGA => CallShape::Mega,
        _ => CallShape::Mono(link.callee.get().map(|callee| CallTarget {
            callee,
            hits: site_hits,
        })),
    })
}

/// How Tier 2 lowers one arithmetic or comparison site, from its profile.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ArithPlan {
    /// Both operands fixnums alone: the int32 path. Any other operand kind,
    /// or an int32 overflow, deopts before the instruction.
    Fixnum,
    /// Both operands flonums alone: the double path. Any other operand kind
    /// deopts before the instruction.
    Flonum,
    /// Each operand seen only as a fixnum or a flonum, with both kinds
    /// observed across the site: an inline path for each pair. A fixnum pair
    /// whose result leaves the int32 range takes the generic helper.
    Dual,
    /// The site saw exact integers only (fixnums and/or bignums) and at least
    /// one bignum, but no flonum, ratnum or complex. Each operand is widened
    /// to i64 through a fixnum test or `check.bigint.fits64`
    Wide,
    /// The site saw exact rationals (with or without fixnum/bignum integers)
    /// and no flonum, complex or non-number. Both operands are lifted to
    /// `n/d` i64 components: a rational's fields load directly when they are
    /// fixnums, an integer becomes `n/1`, and a bignum component or integer
    /// operand deopts.
    Rational,
    /// The same site, but it observed a rational operand with a `BIGINT`
    /// component.
    RationalWide,
    /// Anything else: a side never observed, a non-number, ratnum or complex
    /// operand, no profile at all, or a blocked pc. The generic helper runs
    /// the operation, and it deopts only on a non-number.
    Generic,
}

/// The plan of a site whose arithmetic profile word is `word`, or `None` when
/// the procedure keeps no profile.
pub fn plan_for(word: Option<u32>) -> ArithPlan {
    let Some(word) = word else {
        return ArithPlan::Generic;
    };
    match ArithShape::from_profile(word) {
        ArithShape::FixnumOnly => ArithPlan::Fixnum,
        ArithShape::FlonumOnly => ArithPlan::Flonum,
        _ => {
            let lhs = word & NUMBER_KIND_MASK;
            let rhs = (word >> ARITH_RHS_SHIFT) & NUMBER_KIND_MASK;
            // An exact-rational site: at least one side was a rational, and
            // the other is an exact integer (fixnum or bignum). A flonum,
            // complex or non-number side keeps the generic plan so the double
            // and generic paths are still available.
            if lhs != 0 && rhs != 0 {
                let kinds = lhs | rhs;
                let inexact = NUMBER_FLONUM | NUMBER_COMPLEX | NUMBER_NON_NUMBER;
                if kinds & NUMBER_RATNUM != 0 && kinds & inexact == 0 {
                    return if word & ARITH_WIDE_RATIONAL != 0 {
                        ArithPlan::RationalWide
                    } else {
                        ArithPlan::Rational
                    };
                }
            }
            let ints = NUMBER_FIXNUM | NUMBER_BIGNUM;
            let numeric = NUMBER_FIXNUM | NUMBER_FLONUM;
            if lhs != 0 && rhs != 0 {
                // A site that saw exact integers on both sides including a
                // bignum gets the wide path; the fixnum-only site (both
                // operands fixnum) already returned above, and a flonum side
                // keeps its own plan.
                let kinds = lhs | rhs;
                if kinds & NUMBER_BIGNUM != 0 && kinds & !ints == 0 {
                    return ArithPlan::Wide;
                }
                // Each side is a fixnum or a flonum, and the site is not
                // fixnum/fixnum or flonum/flonum (those returned above): the
                // mixed plan. This must sit outside the exact-integer guard,
                // or a fixnum/flonum pair can never select it.
                if lhs & !numeric == 0 && rhs & !numeric == 0 {
                    return ArithPlan::Dual;
                }
            }
            ArithPlan::Generic
        }
    }
}
