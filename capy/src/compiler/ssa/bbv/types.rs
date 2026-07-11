//! SBBV type lattice: motley types with fixnum/length intervals and symbolic
//! vector-length bounds (ECOOP'24 Section 3.3).

use crate::compiler::ssa::ValueId;
use std::collections::HashMap;
use std::fmt;

pub(super) const FIXNUM_MIN: i64 = i32::MIN as i64;
pub(super) const FIXNUM_MAX: i64 = i32::MAX as i64;

/// Bit index for each runtime value kind in an [`Type::kinds`] bitset.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub(super) enum TypeKind {
    Fixnum = 0,
    Flonum = 1,
    Bignum = 2,
    Pair = 3,
    Vector = 4,
    String = 5,
    Bytevector = 6,
    Symbol = 7,
    Char = 8,
    BoolTrue = 9,
    BoolFalse = 10,
    Null = 11,
    Void = 12,
    Procedure = 13,
    Other = 14,
}

impl TypeKind {
    pub(super) const COUNT: u32 = 15;

    pub(super) const fn bit(self) -> u32 {
        1 << (self as u32)
    }

    pub(super) const fn name(self) -> &'static str {
        match self {
            Self::Fixnum => "fixnum",
            Self::Flonum => "flonum",
            Self::Bignum => "bignum",
            Self::Pair => "pair",
            Self::Vector => "vector",
            Self::String => "string",
            Self::Bytevector => "bytevector",
            Self::Symbol => "symbol",
            Self::Char => "char",
            Self::BoolTrue => "bool-true",
            Self::BoolFalse => "bool-false",
            Self::Null => "null",
            Self::Void => "void",
            Self::Procedure => "procedure",
            Self::Other => "other",
        }
    }
}

pub(super) const KIND_FIXNUM: u32 = TypeKind::Fixnum.bit();
pub(super) const KIND_FLONUM: u32 = TypeKind::Flonum.bit();
pub(super) const KIND_BIGNUM: u32 = TypeKind::Bignum.bit();
pub(super) const KIND_PAIR: u32 = TypeKind::Pair.bit();
pub(super) const KIND_VECTOR: u32 = TypeKind::Vector.bit();
pub(super) const KIND_STRING: u32 = TypeKind::String.bit();
pub(super) const KIND_BYTEVECTOR: u32 = TypeKind::Bytevector.bit();
pub(super) const KIND_SYMBOL: u32 = TypeKind::Symbol.bit();
pub(super) const KIND_CHAR: u32 = TypeKind::Char.bit();
pub(super) const KIND_BOOL_TRUE: u32 = TypeKind::BoolTrue.bit();
pub(super) const KIND_BOOL_FALSE: u32 = TypeKind::BoolFalse.bit();
pub(super) const KIND_NULL: u32 = TypeKind::Null.bit();
pub(super) const KIND_VOID: u32 = TypeKind::Void.bit();
pub(super) const KIND_PROCEDURE: u32 = TypeKind::Procedure.bit();
pub(super) const KIND_OTHER: u32 = TypeKind::Other.bit();

pub(super) const ALL_KINDS: u32 = (1 << TypeKind::COUNT) - 1;

/// Interval endpoint: concrete integer, symbolic `[[v]] - offset`, or sentinels.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(super) enum Bound {
    /// Smallest representable fixnum (Gambit `>=`).
    Min,
    /// Largest representable fixnum (Gambit `<=`).
    Max,
    Int(i64),
    /// `[[value]] - offset` where offset >= 0.
    VecLenMinus(ValueId, i64),
    /// Non-fixnum upper bound produced by interval arithmetic.
    Overflow,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(super) struct Interval {
    pub(super) lo: Bound,
    pub(super) hi: Bound,
}

impl Interval {
    pub(super) const BOT: Self = Self {
        lo: Bound::Int(0),
        hi: Bound::Int(-1),
    };

    pub(super) const TOP_FIXNUM: Self = Self {
        lo: Bound::Min,
        hi: Bound::Max,
    };

    pub(super) const TOP_LENGTH: Self = Self {
        lo: Bound::Int(0),
        hi: Bound::Max,
    };

    pub(super) fn singleton(value: i64) -> Self {
        Self {
            lo: Bound::Int(value),
            hi: Bound::Int(value),
        }
    }

    pub(super) fn is_empty(self) -> bool {
        match (self.lo, self.hi) {
            (Bound::Overflow, _) | (_, Bound::Overflow) => true,
            (Bound::Int(lo), Bound::Int(hi)) => lo > hi,
            (Bound::VecLenMinus(v1, o1), Bound::VecLenMinus(v2, o2)) if v1 == v2 => o1 < o2,
            _ => false,
        }
    }

    #[allow(clippy::should_implement_trait)]
    pub(super) fn add(self, other: Self) -> Self {
        Self {
            lo: add_lo(self.lo, other.lo),
            hi: add_hi(self.hi, other.hi),
        }
    }

    #[allow(clippy::should_implement_trait)]
    pub(super) fn sub(self, other: Self) -> Self {
        Self {
            lo: sub_lo(self.lo, other.hi),
            hi: sub_hi(self.hi, other.lo),
        }
    }

    #[allow(clippy::should_implement_trait)]
    pub(super) fn mul(self, other: Self) -> Self {
        mul_interval(self, other)
    }

    pub(super) fn intersect(self, other: Self) -> Self {
        Self {
            lo: max_lo(self.lo, other.lo),
            hi: min_hi(self.hi, other.hi),
        }
    }

    pub(super) fn union(self, other: Self, widen: bool) -> Self {
        Self {
            lo: union_lo(self.lo, other.lo, widen),
            hi: union_hi(self.hi, other.hi, widen),
        }
    }

    /// Narrow for `x < y`: returns `(x_true, y_true)` and `(x_false, y_false)`.
    pub(super) fn narrow_lt(self, other: Self) -> ((Self, Self), (Self, Self)) {
        let true_x = Self {
            lo: self.lo,
            hi: min_hi(self.hi, dec_hi(other.hi)),
        };
        let true_y = Self {
            lo: max_lo(inc_lo(self.lo), other.lo),
            hi: other.hi,
        };
        let false_x = Self {
            lo: max_lo(self.lo, other.lo),
            hi: self.hi,
        };
        let false_y = Self {
            lo: other.lo,
            hi: min_hi(other.hi, self.hi),
        };
        ((true_x, true_y), (false_x, false_y))
    }

    pub(super) fn narrow_le(self, other: Self) -> ((Self, Self), (Self, Self)) {
        let true_x = Self {
            lo: self.lo,
            hi: min_hi(self.hi, other.hi),
        };
        let true_y = Self {
            lo: max_lo(self.lo, other.lo),
            hi: other.hi,
        };
        let false_x = Self {
            lo: max_lo(inc_lo(other.lo), self.lo),
            hi: self.hi,
        };
        let false_y = Self {
            lo: other.lo,
            hi: min_hi(other.hi, dec_hi(self.hi)),
        };
        ((true_x, true_y), (false_x, false_y))
    }

    pub(super) fn narrow_gt(self, other: Self) -> ((Self, Self), (Self, Self)) {
        let ((true_other, true_self), (false_other, false_self)) = other.narrow_lt(self);
        ((true_self, true_other), (false_self, false_other))
    }

    pub(super) fn narrow_ge(self, other: Self) -> ((Self, Self), (Self, Self)) {
        let ((true_other, true_self), (false_other, false_self)) = other.narrow_le(self);
        ((true_self, true_other), (false_self, false_other))
    }

    pub(super) fn narrow_eq(self, other: Self) -> ((Self, Self), (Self, Self)) {
        let inter = self.intersect(other);
        ((inter, inter), (self, other))
    }
}

/// Comparison used by [`TypeContext::narrow_for_predicate`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum CmpOp {
    Lt,
    Le,
    Gt,
    Ge,
    Eq,
}

/// Motley type: kind bitset plus optional fixnum/length intervals.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct Type {
    pub(super) kinds: u32,
    pub(super) fixnum_range: Option<Interval>,
    pub(super) length_range: Option<Interval>,
    /// When set, this value is a known fixnum constant.
    pub(super) singleton: Option<i64>,
}

impl Type {
    pub(super) const BOT: Self = Self {
        kinds: 0,
        fixnum_range: None,
        length_range: None,
        singleton: None,
    };

    pub(super) const TOP: Self = Self {
        kinds: ALL_KINDS,
        fixnum_range: Some(Interval::TOP_FIXNUM),
        length_range: Some(Interval::TOP_LENGTH),
        singleton: None,
    };

    pub(super) fn kind(kind: TypeKind) -> Self {
        let mut ty = Self {
            kinds: kind.bit(),
            fixnum_range: None,
            length_range: None,
            singleton: None,
        };
        if kind == TypeKind::Fixnum {
            ty.fixnum_range = Some(Interval::TOP_FIXNUM);
        }
        if matches!(
            kind,
            TypeKind::Vector | TypeKind::String | TypeKind::Bytevector
        ) {
            ty.length_range = Some(Interval::TOP_LENGTH);
        }
        ty
    }

    pub(super) fn fixnum(lo: Bound, hi: Bound) -> Self {
        Self {
            kinds: KIND_FIXNUM,
            fixnum_range: Some(Interval { lo, hi }),
            length_range: None,
            singleton: None,
        }
    }

    pub(super) fn fixnum_int(lo: i64, hi: i64) -> Self {
        Self::fixnum(Bound::Int(lo), Bound::Int(hi))
    }

    pub(super) fn constant(value: i64) -> Self {
        Self {
            kinds: KIND_FIXNUM,
            fixnum_range: Some(Interval::singleton(value)),
            length_range: None,
            singleton: Some(value),
        }
    }

    pub(super) fn has_kind(self, kind: TypeKind) -> bool {
        self.kinds & kind.bit() != 0
    }
}

pub(super) fn is_empty(ty: &Type) -> bool {
    ty.kinds == 0
        || ty
            .fixnum_range
            .is_some_and(|interval| interval.is_empty() && ty.kinds == KIND_FIXNUM)
}

pub(super) fn exclude_kind(ty: Type, kind: TypeKind) -> Type {
    let mut ty = ty;
    ty.kinds &= !kind.bit();
    if kind == TypeKind::Fixnum {
        ty.fixnum_range = None;
        ty.singleton = None;
    }
    if matches!(
        kind,
        TypeKind::Vector | TypeKind::String | TypeKind::Bytevector
    ) {
        ty.length_range = None;
    }
    ty
}

pub(super) fn union_types(a: Type, b: Type, widen: bool) -> Type {
    if is_empty(&a) {
        return b;
    }
    if is_empty(&b) {
        return a;
    }
    if a.singleton.is_some() && a == b {
        return a;
    }

    let kinds = a.kinds | b.kinds;
    let fixnum_range = match (a.fixnum_range, b.fixnum_range) {
        (Some(x), Some(y)) => Some(x.union(y, widen)),
        (Some(x), None) => Some(x),
        (None, Some(y)) => Some(y),
        (None, None) => None,
    };
    let length_range = match (a.length_range, b.length_range) {
        (Some(x), Some(y)) => Some(x.union(y, widen)),
        (Some(x), None) => Some(x),
        (None, Some(y)) => Some(y),
        (None, None) => None,
    };

    let singleton = match (a.singleton, b.singleton) {
        (Some(x), Some(y)) if x == y => Some(x),
        _ => None,
    };

    Type {
        kinds,
        fixnum_range,
        length_range,
        singleton,
    }
}

pub(super) fn intersect_types(a: Type, b: Type) -> Type {
    let kinds = a.kinds & b.kinds;
    if kinds == 0 {
        return Type::BOT;
    }

    let fixnum_range = match (a.fixnum_range, b.fixnum_range) {
        (Some(x), Some(y)) => {
            let interval = x.intersect(y);
            if interval.is_empty() {
                return Type::BOT;
            }
            Some(interval)
        }
        (Some(x), None) if kinds & KIND_FIXNUM != 0 => Some(x),
        (None, Some(y)) if kinds & KIND_FIXNUM != 0 => Some(y),
        _ => None,
    };

    let length_range = match (a.length_range, b.length_range) {
        (Some(x), Some(y)) => {
            let interval = x.intersect(y);
            if interval.is_empty() {
                return Type::BOT;
            }
            Some(interval)
        }
        (Some(x), None) if kinds & (KIND_VECTOR | KIND_STRING) != 0 => Some(x),
        (None, Some(y)) if kinds & (KIND_VECTOR | KIND_STRING) != 0 => Some(y),
        _ => None,
    };

    let singleton = match (a.singleton, b.singleton) {
        (Some(x), Some(y)) if x == y => Some(x),
        (Some(x), None) => Some(x),
        (None, Some(y)) => Some(y),
        _ => None,
    };

    Type {
        kinds,
        fixnum_range,
        length_range,
        singleton,
    }
}

/// Per-value type environment for SBBV specialization.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub(super) struct TypeContext {
    pub(super) types: HashMap<ValueId, Type>,
}

impl TypeContext {
    pub(super) fn new() -> Self {
        Self::default()
    }

    pub(super) fn get(&self, id: ValueId) -> Type {
        self.types.get(&id).cloned().unwrap_or(Type::TOP)
    }

    pub(super) fn set(&mut self, id: ValueId, ty: Type) {
        if is_empty(&ty) {
            self.types.remove(&id);
        } else {
            self.types.insert(id, ty);
        }
    }

    pub(super) fn union(&self, other: &Self, widen: bool) -> Self {
        let mut out = self.clone();
        for (id, ty) in &other.types {
            let merged = union_types(out.get(*id), ty.clone(), widen);
            out.set(*id, merged);
        }
        out
    }

    pub(super) fn intersect(&self, other: &Self) -> Self {
        let ids: std::collections::HashSet<_> =
            self.types.keys().chain(other.types.keys()).collect();
        let mut out = Self::new();
        for id in ids {
            let ty = intersect_types(self.get(*id), other.get(*id));
            if !is_empty(&ty) {
                out.set(*id, ty);
            }
        }
        out
    }

    /// Narrow types for `lhs op rhs`; returns `(true_ctx, false_ctx)`.
    pub(super) fn narrow_for_predicate(
        &self,
        op: CmpOp,
        lhs: ValueId,
        rhs: ValueId,
    ) -> (Self, Self) {
        let lhs_ty = self.get(lhs);
        let rhs_ty = self.get(rhs);

        let (lhs_interval, rhs_interval) = match (lhs_ty.fixnum_range, rhs_ty.fixnum_range) {
            (Some(x), Some(y)) => (x, y),
            _ => return (self.clone(), self.clone()),
        };

        let ((true_lhs, true_rhs), (false_lhs, false_rhs)) = match op {
            CmpOp::Lt => lhs_interval.narrow_lt(rhs_interval),
            CmpOp::Le => lhs_interval.narrow_le(rhs_interval),
            CmpOp::Gt => lhs_interval.narrow_gt(rhs_interval),
            CmpOp::Ge => lhs_interval.narrow_ge(rhs_interval),
            CmpOp::Eq => lhs_interval.narrow_eq(rhs_interval),
        };

        let mut true_ctx = self.clone();
        let mut false_ctx = self.clone();

        let narrow_value = |ctx: &mut Self, id: ValueId, base: &Type, interval: Interval| {
            let mut ty = base.clone();
            ty.fixnum_range = Some(interval);
            if let Some(value) = ty.singleton
                && !interval_contains_int(interval, value)
            {
                ty.singleton = None;
            }
            ctx.set(id, ty);
        };

        if !true_lhs.is_empty() && !true_rhs.is_empty() {
            narrow_value(&mut true_ctx, lhs, &lhs_ty, true_lhs);
            narrow_value(&mut true_ctx, rhs, &rhs_ty, true_rhs);
        } else {
            true_ctx = Self::new();
        }

        if !false_lhs.is_empty() && !false_rhs.is_empty() {
            narrow_value(&mut false_ctx, lhs, &lhs_ty, false_lhs);
            narrow_value(&mut false_ctx, rhs, &rhs_ty, false_rhs);
        } else {
            false_ctx = Self::new();
        }

        (true_ctx, false_ctx)
    }
}

impl fmt::Display for Bound {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Min => write!(f, ">="),
            Self::Max => write!(f, "<="),
            Self::Int(n) => write!(f, "{n}"),
            Self::VecLenMinus(id, offset) => {
                if *offset == 0 {
                    write!(f, "[[{id:?}]]")
                } else if *offset > 0 {
                    write!(f, "[[{id:?}]]-{offset}")
                } else {
                    write!(f, "[[{id:?}]]+{}", -offset)
                }
            }
            Self::Overflow => write!(f, "overflow"),
        }
    }
}

impl fmt::Display for Interval {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_empty() {
            return write!(f, "empty");
        }
        write!(f, "{}..{}", self.lo, self.hi)
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if is_empty(self) {
            return write!(f, "bot");
        }
        if self.kinds == ALL_KINDS
            && self.fixnum_range == Some(Interval::TOP_FIXNUM)
            && self.length_range == Some(Interval::TOP_LENGTH)
            && self.singleton.is_none()
        {
            return write!(f, "top");
        }

        let mut parts = Vec::new();
        if let Some(value) = self.singleton {
            parts.push(format!("#{value}"));
        }
        for kind in typed_kinds(self.kinds) {
            parts.push(kind.name().to_string());
        }
        write!(f, "{}", parts.join("|"))?;
        if let Some(interval) = self.fixnum_range {
            write!(f, " fx[{interval}]")?;
        }
        if let Some(interval) = self.length_range {
            write!(f, " len[{interval}]")?;
        }
        Ok(())
    }
}

impl fmt::Display for TypeContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.types.is_empty() {
            return write!(f, "{{}}");
        }
        let mut ids: Vec<_> = self.types.keys().copied().collect();
        ids.sort_by_key(|id| id.0);
        write!(f, "{{")?;
        for (idx, id) in ids.iter().enumerate() {
            if idx > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{id:?}={}", self.types[id])?;
        }
        write!(f, "}}")
    }
}

// --- bound/interval helpers (paper Section 3.3) ---

fn val_lo(b: Bound) -> i64 {
    match b {
        Bound::Min => FIXNUM_MIN,
        Bound::Max => FIXNUM_MAX,
        Bound::Int(n) => n,
        Bound::VecLenMinus(_, offset) => offset.saturating_neg(),
        Bound::Overflow => FIXNUM_MAX,
    }
}

fn val_hi(b: Bound) -> i64 {
    match b {
        Bound::Min => FIXNUM_MIN,
        Bound::Max => FIXNUM_MAX,
        Bound::Int(n) => n,
        Bound::VecLenMinus(_, offset) => FIXNUM_MAX.saturating_sub(offset),
        Bound::Overflow => FIXNUM_MIN,
    }
}

fn add_lo(x: Bound, y: Bound) -> Bound {
    if let (Some(a), Some(b)) = (exact_bound_value(x), exact_bound_value(y)) {
        return int_or_overflow(a + b);
    }
    match (x, y) {
        (Bound::VecLenMinus(_v, i), Bound::Int(j)) | (Bound::Int(j), Bound::VecLenMinus(_v, i)) => {
            int_or_overflow(j.saturating_sub(i))
        }
        (Bound::VecLenMinus(_, i), Bound::VecLenMinus(_, j)) => {
            int_or_overflow(i.saturating_neg().saturating_sub(j))
        }
        _ => Bound::Overflow,
    }
}

fn add_hi(x: Bound, y: Bound) -> Bound {
    if let (Some(a), Some(b)) = (exact_bound_value(x), exact_bound_value(y)) {
        return int_or_overflow(a + b);
    }
    match (x, y) {
        (Bound::VecLenMinus(v, i), Bound::Int(j)) => i
            .checked_sub(j)
            .map(|offset| vec_len_minus_or_overflow(v, offset))
            .unwrap_or(Bound::Overflow),
        (Bound::Int(j), Bound::VecLenMinus(v, i)) => {
            add_hi(Bound::VecLenMinus(v, i), Bound::Int(j))
        }
        (Bound::VecLenMinus(_, _), Bound::VecLenMinus(_, _)) => Bound::Overflow,
        _ => Bound::Overflow,
    }
}

fn sub_lo(x: Bound, y: Bound) -> Bound {
    if let (Some(a), Some(b)) = (exact_bound_value(x), exact_bound_value(y)) {
        return int_or_overflow(a - b);
    }
    match (x, y) {
        (Bound::VecLenMinus(_, i), Bound::Int(j)) => {
            int_or_overflow(i.saturating_add(j).saturating_neg())
        }
        _ => Bound::Overflow,
    }
}

fn sub_hi(x: Bound, y: Bound) -> Bound {
    if let (Some(a), Some(b)) = (exact_bound_value(x), exact_bound_value(y)) {
        return int_or_overflow(a - b);
    }
    match (x, y) {
        (Bound::VecLenMinus(v, i), Bound::Int(j)) => i
            .checked_add(j)
            .map(|offset| vec_len_minus_or_overflow(v, offset))
            .unwrap_or(Bound::Overflow),
        (Bound::VecLenMinus(_, _i), Bound::VecLenMinus(_, j)) if j < 0 => Bound::Overflow,
        _ => Bound::Overflow,
    }
}

fn mul_interval(a: Interval, b: Interval) -> Interval {
    let endpoints = [
        mul_bounds(a.lo, b.lo),
        mul_bounds(a.lo, b.hi),
        mul_bounds(a.hi, b.lo),
        mul_bounds(a.hi, b.hi),
    ];
    if endpoints.contains(&Bound::Overflow) {
        return Interval {
            lo: Bound::Overflow,
            hi: Bound::Overflow,
        };
    }
    let mut lo = endpoints[0];
    let mut hi = endpoints[0];
    for &point in &endpoints[1..] {
        lo = min_lo(lo, point);
        hi = max_hi(hi, point);
    }
    Interval { lo, hi }
}

fn mul_bounds(x: Bound, y: Bound) -> Bound {
    match (x, y) {
        (Bound::Int(0), _) | (_, Bound::Int(0)) => Bound::Int(0),
        (Bound::Int(1), other) | (other, Bound::Int(1)) => other,
        (Bound::Int(-1), Bound::Int(b)) => int_or_overflow(-b),
        (Bound::Int(a), Bound::Int(b)) => int_or_overflow(a.saturating_mul(b)),
        _ => Bound::Overflow,
    }
}

fn inc_lo(b: Bound) -> Bound {
    match b {
        Bound::Min | Bound::Max => b,
        Bound::Int(n) => Bound::Int(n.saturating_add(1)),
        // Preserve the symbolic bound when its offset can absorb the one.
        Bound::VecLenMinus(v, offset) if offset > 0 => Bound::VecLenMinus(v, offset - 1),
        // [[v]] + 1 may overflow, but 1 remains a sound concrete lower bound.
        Bound::VecLenMinus(_, 0) => Bound::Int(1),
        Bound::VecLenMinus(_, _) => Bound::Overflow,
        Bound::Overflow => Bound::Overflow,
    }
}

fn dec_hi(b: Bound) -> Bound {
    match b {
        Bound::Min | Bound::Max => b,
        Bound::Int(n) => Bound::Int(n.saturating_sub(1)),
        // ([[v]] - offset) - 1 = [[v]] - (offset + 1)
        Bound::VecLenMinus(v, offset) => Bound::VecLenMinus(v, offset.saturating_add(1)),
        Bound::Overflow => Bound::Overflow,
    }
}

fn max_lo(x: Bound, y: Bound) -> Bound {
    match (x, y) {
        (Bound::VecLenMinus(x_vector, x_offset), Bound::VecLenMinus(y_vector, y_offset))
            if x_vector == y_vector =>
        {
            Bound::VecLenMinus(x_vector, x_offset.min(y_offset))
        }
        _ if exact_bound_value(x).is_some() && exact_bound_value(y).is_some() => {
            if val_lo(x) >= val_lo(y) { x } else { y }
        }
        (Bound::VecLenMinus(_, _), _) => y,
        (_, Bound::VecLenMinus(_, _)) => x,
        _ => x,
    }
}

fn min_hi(x: Bound, y: Bound) -> Bound {
    match (x, y) {
        (Bound::VecLenMinus(x_vector, x_offset), Bound::VecLenMinus(y_vector, y_offset))
            if x_vector == y_vector =>
        {
            Bound::VecLenMinus(x_vector, x_offset.max(y_offset))
        }
        _ if exact_bound_value(x).is_some() && exact_bound_value(y).is_some() => {
            if val_hi(x) <= val_hi(y) { x } else { y }
        }
        (Bound::VecLenMinus(_, _), _) => y,
        (_, Bound::VecLenMinus(_, _)) => x,
        _ => x,
    }
}

fn union_lo(x: Bound, y: Bound, widen: bool) -> Bound {
    if x == y {
        return x;
    }
    let chosen = min_lo(x, y);
    if widen { widen_lo(chosen) } else { chosen }
}

fn union_hi(x: Bound, y: Bound, widen: bool) -> Bound {
    if x == y {
        return x;
    }
    let chosen = max_hi(x, y);
    if widen { widen_hi(chosen) } else { chosen }
}

fn widen_lo(b: Bound) -> Bound {
    match b {
        Bound::Int(n) if n > 0 => Bound::Int(0),
        Bound::Int(n) if n < 0 => Bound::Min,
        Bound::VecLenMinus(_, _) => Bound::Min,
        other => other,
    }
}

fn widen_hi(b: Bound) -> Bound {
    match b {
        Bound::Int(n) if n > 0 => Bound::Max,
        Bound::Int(n) if n < 0 => Bound::Int(0),
        Bound::VecLenMinus(_, _) => Bound::Max,
        other => other,
    }
}

fn min_lo(x: Bound, y: Bound) -> Bound {
    match (x, y) {
        (Bound::VecLenMinus(x_vector, x_offset), Bound::VecLenMinus(y_vector, y_offset))
            if x_vector == y_vector =>
        {
            Bound::VecLenMinus(x_vector, x_offset.max(y_offset))
        }
        _ if exact_bound_value(x).is_some() && exact_bound_value(y).is_some() => {
            if val_lo(x) <= val_lo(y) { x } else { y }
        }
        _ => Bound::Min,
    }
}

fn max_hi(x: Bound, y: Bound) -> Bound {
    match (x, y) {
        (Bound::VecLenMinus(x_vector, x_offset), Bound::VecLenMinus(y_vector, y_offset))
            if x_vector == y_vector =>
        {
            Bound::VecLenMinus(x_vector, x_offset.min(y_offset))
        }
        _ if exact_bound_value(x).is_some() && exact_bound_value(y).is_some() => {
            if val_hi(x) >= val_hi(y) { x } else { y }
        }
        _ => Bound::Max,
    }
}

fn exact_bound_value(bound: Bound) -> Option<i64> {
    match bound {
        Bound::Min => Some(FIXNUM_MIN),
        Bound::Max => Some(FIXNUM_MAX),
        Bound::Int(value) => Some(value),
        Bound::VecLenMinus(_, _) | Bound::Overflow => None,
    }
}

fn int_or_overflow(n: i64) -> Bound {
    if (FIXNUM_MIN..=FIXNUM_MAX).contains(&n) {
        Bound::Int(n)
    } else {
        Bound::Overflow
    }
}

fn vec_len_minus_or_overflow(v: ValueId, offset: i64) -> Bound {
    if offset >= 0 {
        Bound::VecLenMinus(v, offset)
    } else {
        Bound::Overflow
    }
}

fn interval_contains_int(interval: Interval, value: i64) -> bool {
    val_lo(interval.lo) <= value && value <= val_hi(interval.hi)
}

fn typed_kinds(bits: u32) -> impl Iterator<Item = TypeKind> {
    [
        TypeKind::Fixnum,
        TypeKind::Flonum,
        TypeKind::Bignum,
        TypeKind::Pair,
        TypeKind::Vector,
        TypeKind::String,
        TypeKind::Bytevector,
        TypeKind::Symbol,
        TypeKind::Char,
        TypeKind::BoolTrue,
        TypeKind::BoolFalse,
        TypeKind::Null,
        TypeKind::Void,
        TypeKind::Procedure,
        TypeKind::Other,
    ]
    .into_iter()
    .filter(move |kind| bits & kind.bit() != 0)
}

#[cfg(test)]
mod tests {
    use super::*;

    impl Interval {
        fn fixnum_int(lo: i64, hi: i64) -> Self {
            Self {
                lo: Bound::Int(lo),
                hi: Bound::Int(hi),
            }
        }
    }

    #[test]
    fn interval_add_integers() {
        let a = Interval::fixnum_int(1, 3);
        let b = Interval::fixnum_int(10, 20);
        let sum = a.add(b);
        assert_eq!(sum.lo, Bound::Int(11));
        assert_eq!(sum.hi, Bound::Int(23));
    }

    #[test]
    fn interval_add_top_preserves_possible_overflow() {
        let sum = Interval::TOP_FIXNUM.add(Interval::TOP_FIXNUM);

        assert_eq!(sum.lo, Bound::Overflow);
        assert_eq!(sum.hi, Bound::Overflow);
    }

    #[test]
    fn interval_subtract_top_preserves_possible_overflow() {
        let difference = Interval::TOP_FIXNUM.sub(Interval::TOP_FIXNUM);

        assert_eq!(difference.lo, Bound::Overflow);
        assert_eq!(difference.hi, Bound::Overflow);
    }

    #[test]
    fn interval_add_symbolic_lower() {
        let v = ValueId(7);
        let a = Interval {
            lo: Bound::VecLenMinus(v, 2),
            hi: Bound::VecLenMinus(v, 0),
        };
        let b = Interval::singleton(5);
        let sum = a.add(b);
        assert_eq!(sum.lo, Bound::Int(3));
    }

    #[test]
    fn interval_add_symbolic_upper() {
        let v = ValueId(7);
        // [[v]]-2 + 1 = [[v]]-1; the offset absorbs the constant.
        let a = Interval {
            lo: Bound::VecLenMinus(v, 2),
            hi: Bound::VecLenMinus(v, 2),
        };
        let b = Interval::singleton(1);
        let sum = a.add(b);
        assert_eq!(sum.hi, Bound::VecLenMinus(v, 1));
    }

    #[test]
    fn interval_add_symbolic_upper_overflows() {
        let v = ValueId(7);
        // [[v]]-0 + 1 = [[v]]+1 may exceed the fixnum range, so the upper
        // bound degrades to overflow.
        let a = Interval {
            lo: Bound::VecLenMinus(v, 0),
            hi: Bound::VecLenMinus(v, 0),
        };
        let b = Interval::singleton(1);
        let sum = a.add(b);
        assert_eq!(sum.hi, Bound::Overflow);
    }

    #[test]
    fn symbolic_interval_with_decreasing_offsets_is_not_empty() {
        let vector = ValueId(7);
        let interval = Interval {
            lo: Bound::VecLenMinus(vector, 2),
            hi: Bound::VecLenMinus(vector, 0),
        };

        assert!(!interval.is_empty());
    }

    #[test]
    fn symbolic_interval_with_increasing_offsets_is_empty() {
        let vector = ValueId(7);
        let interval = Interval {
            lo: Bound::VecLenMinus(vector, 0),
            hi: Bound::VecLenMinus(vector, 2),
        };

        assert!(interval.is_empty());
    }

    #[test]
    fn interval_subtracts_from_symbolic_bounds() {
        let vector = ValueId(7);
        let difference = Interval {
            lo: Bound::VecLenMinus(vector, 0),
            hi: Bound::VecLenMinus(vector, 0),
        }
        .sub(Interval::singleton(1));

        assert_eq!(difference.lo, Bound::Int(-1));
        assert_eq!(difference.hi, Bound::VecLenMinus(vector, 1));
    }

    #[test]
    fn interval_symbolic_subtraction_preserves_possible_overflow() {
        let vector = ValueId(7);
        let difference = Interval {
            lo: Bound::VecLenMinus(vector, 0),
            hi: Bound::VecLenMinus(vector, 0),
        }
        .sub(Interval::singleton(-1));

        assert_eq!(difference.hi, Bound::Overflow);
    }

    #[test]
    fn interval_mul_preserves_symbolic_range() {
        let vector = ValueId(7);
        let product = Interval {
            lo: Bound::VecLenMinus(vector, 2),
            hi: Bound::VecLenMinus(vector, 0),
        }
        .mul(Interval::singleton(1));

        assert_eq!(product.lo, Bound::VecLenMinus(vector, 2));
        assert_eq!(product.hi, Bound::VecLenMinus(vector, 0));
    }

    #[test]
    fn interval_union_uses_safe_mixed_lower_bound() {
        let vector = ValueId(7);
        let symbolic = Interval {
            lo: Bound::VecLenMinus(vector, 2),
            hi: Bound::VecLenMinus(vector, 0),
        };
        let union = symbolic.union(Interval::singleton(-1), false);

        assert_eq!(union.lo, Bound::Min);
        assert_eq!(union.hi, Bound::Max);
    }

    #[test]
    fn interval_union_preserves_same_vector_relation() {
        let vector = ValueId(7);
        let left = Interval {
            lo: Bound::VecLenMinus(vector, 2),
            hi: Bound::VecLenMinus(vector, 1),
        };
        let right = Interval {
            lo: Bound::VecLenMinus(vector, 1),
            hi: Bound::VecLenMinus(vector, 0),
        };
        let union = left.union(right, false);

        assert_eq!(union.lo, Bound::VecLenMinus(vector, 2));
        assert_eq!(union.hi, Bound::VecLenMinus(vector, 0));
    }

    #[test]
    fn interval_union_generalizes_different_vector_bounds() {
        let left = Interval {
            lo: Bound::VecLenMinus(ValueId(7), 1),
            hi: Bound::VecLenMinus(ValueId(7), 0),
        };
        let right = Interval {
            lo: Bound::VecLenMinus(ValueId(8), 1),
            hi: Bound::VecLenMinus(ValueId(8), 0),
        };
        let union = left.union(right, false);

        assert_eq!(union.lo, Bound::Min);
        assert_eq!(union.hi, Bound::Max);
    }

    #[test]
    fn interval_intersection_preserves_same_vector_relation() {
        let vector = ValueId(7);
        let left = Interval {
            lo: Bound::VecLenMinus(vector, 3),
            hi: Bound::VecLenMinus(vector, 0),
        };
        let right = Interval {
            lo: Bound::VecLenMinus(vector, 2),
            hi: Bound::VecLenMinus(vector, 1),
        };
        let intersection = left.intersect(right);

        assert_eq!(intersection.lo, Bound::VecLenMinus(vector, 2));
        assert_eq!(intersection.hi, Bound::VecLenMinus(vector, 1));
    }

    #[test]
    fn interval_widens_changing_symbolic_bounds() {
        let vector = ValueId(7);
        let left = Interval {
            lo: Bound::VecLenMinus(vector, 1),
            hi: Bound::VecLenMinus(vector, 1),
        };
        let right = Interval {
            lo: Bound::VecLenMinus(vector, 2),
            hi: Bound::VecLenMinus(vector, 0),
        };
        let union = left.union(right, true);

        assert_eq!(union.lo, Bound::Min);
        assert_eq!(union.hi, Bound::Max);
    }

    #[test]
    fn interval_mul_preserves_possible_overflow() {
        let product = Interval {
            lo: Bound::Int(0),
            hi: Bound::Max,
        }
        .mul(Interval::singleton(10));

        assert_eq!(product.lo, Bound::Overflow);
        assert_eq!(product.hi, Bound::Overflow);
    }

    #[test]
    fn interval_mul_preserves_fixnum_boundary() {
        let product = Interval::singleton(FIXNUM_MAX).mul(Interval::singleton(1));

        assert_eq!(product.lo, Bound::Int(FIXNUM_MAX));
        assert_eq!(product.hi, Bound::Int(FIXNUM_MAX));
    }

    #[test]
    fn interval_mul_preserves_negated_fixnum_overflow() {
        let negative_one = Interval::singleton(-1);
        let minimum = Interval::singleton(FIXNUM_MIN);

        let left_product = negative_one.mul(minimum);
        let right_product = minimum.mul(negative_one);

        assert_eq!(left_product.lo, Bound::Overflow);
        assert_eq!(left_product.hi, Bound::Overflow);
        assert_eq!(right_product, left_product);
    }

    #[test]
    fn interval_narrow_lt_true_branch() {
        let x = Interval::fixnum_int(0, 10);
        let y = Interval::fixnum_int(0, 10);
        let ((nx, ny), _) = x.narrow_lt(y);
        assert_eq!(nx.hi, Bound::Int(9));
        assert_eq!(ny.lo, Bound::Int(1));
    }

    #[test]
    fn interval_narrow_lt_false_branch() {
        let x = Interval::fixnum_int(0, 10);
        let y = Interval::fixnum_int(0, 10);
        let (_, (nx, ny)) = x.narrow_lt(y);
        assert_eq!(nx.lo, Bound::Int(0));
        assert_eq!(ny.hi, Bound::Int(10));
    }

    #[test]
    fn interval_narrow_gt_preserves_operand_order() {
        let x = Interval::fixnum_int(0, 10);
        let y = Interval::fixnum_int(5, 20);
        let ((true_x, true_y), (false_x, false_y)) = x.narrow_gt(y);

        assert_eq!(true_x, Interval::fixnum_int(6, 10));
        assert_eq!(true_y, Interval::fixnum_int(5, 9));
        assert_eq!(false_x, Interval::fixnum_int(0, 10));
        assert_eq!(false_y, Interval::fixnum_int(5, 20));
    }

    #[test]
    fn interval_narrow_ge_preserves_operand_order() {
        let x = Interval::fixnum_int(10, 20);
        let y = Interval::fixnum_int(0, 15);
        let ((true_x, true_y), (false_x, false_y)) = x.narrow_ge(y);

        assert_eq!(true_x, Interval::fixnum_int(10, 20));
        assert_eq!(true_y, Interval::fixnum_int(0, 15));
        assert_eq!(false_x, Interval::fixnum_int(10, 14));
        assert_eq!(false_y, Interval::fixnum_int(11, 15));
    }

    #[test]
    fn context_narrow_for_predicate() {
        let mut ctx = TypeContext::new();
        let x = ValueId(1);
        let y = ValueId(2);
        ctx.set(x, Type::fixnum_int(0, 10));
        ctx.set(y, Type::fixnum_int(0, 10));

        let (true_ctx, false_ctx) = ctx.narrow_for_predicate(CmpOp::Lt, x, y);
        assert_eq!(
            true_ctx.get(x).fixnum_range,
            Some(Interval::fixnum_int(0, 9))
        );
        assert_eq!(
            true_ctx.get(y).fixnum_range,
            Some(Interval::fixnum_int(1, 10))
        );
        assert_eq!(
            false_ctx.get(x).fixnum_range,
            Some(Interval::fixnum_int(0, 10))
        );
        assert_eq!(
            false_ctx.get(y).fixnum_range,
            Some(Interval::fixnum_int(0, 10))
        );
    }

    #[test]
    fn context_narrow_gt_assigns_original_operands() {
        let mut ctx = TypeContext::new();
        let x = ValueId(1);
        let y = ValueId(2);
        ctx.set(x, Type::fixnum_int(0, 10));
        ctx.set(y, Type::fixnum_int(5, 20));

        let (true_ctx, false_ctx) = ctx.narrow_for_predicate(CmpOp::Gt, x, y);

        assert_eq!(
            true_ctx.get(x).fixnum_range,
            Some(Interval::fixnum_int(6, 10))
        );
        assert_eq!(
            true_ctx.get(y).fixnum_range,
            Some(Interval::fixnum_int(5, 9))
        );
        assert_eq!(
            false_ctx.get(x).fixnum_range,
            Some(Interval::fixnum_int(0, 10))
        );
        assert_eq!(
            false_ctx.get(y).fixnum_range,
            Some(Interval::fixnum_int(5, 20))
        );
    }

    #[test]
    fn union_types_widen_drops_bounds() {
        let a = Type::fixnum_int(1, 3);
        let b = Type::fixnum_int(8, 12);
        let merged = union_types(a, b, true);
        assert_eq!(
            merged.fixnum_range,
            Some(Interval {
                lo: Bound::Int(0),
                hi: Bound::Max,
            })
        );
    }
}
