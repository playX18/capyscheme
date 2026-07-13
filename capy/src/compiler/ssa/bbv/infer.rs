//! Abstract type inference for primitive calls (ECOOP'24 Section 3.4).
//!
//! [`specialize_prim`] threads the SBBV type lattice through a primitive call:
//! it computes the result type, optionally rewrites the primitive to a cheaper
//! unchecked/fixnum variant when the argument types prove the guard, and folds
//! the call to a constant when the result is statically known.

use super::types::{
    Bound, CmpOp, FIXNUM_MAX, FIXNUM_MIN, Interval, KIND_BIGNUM, KIND_BOOL_FALSE, KIND_BOOL_TRUE,
    KIND_BYTEVECTOR, KIND_CHAR, KIND_FIXNUM, KIND_FLONUM, KIND_NULL, KIND_OTHER, KIND_PAIR,
    KIND_PROCEDURE, KIND_STRING, KIND_SYMBOL, KIND_VECTOR, Type, TypeContext, TypeKind,
    intersect_types,
};
use crate::compiler::cranelift::primitive::Primitive;
use crate::compiler::ssa::ValueId;
use crate::runtime::value::{ByteVector, Pair, Str, Symbol, Value, Vector};

/// Outcome of specializing a primitive call under a typing context.
pub(super) struct PrimSpec<'gc> {
    /// Inferred type of the destination value.
    pub(super) result: Type,
    /// Primitive to emit (possibly a cheaper specialized variant).
    pub(super) prim: Primitive,
    /// When set, the call is statically known and can be folded to a constant.
    pub(super) fold: Option<Value<'gc>>,
}

/// Motley type of a boolean-producing operation with unknown truth value.
pub(super) fn boolean_type() -> Type {
    Type {
        kinds: KIND_BOOL_TRUE | KIND_BOOL_FALSE,
        fixnum_range: None,
        length_range: None,
        singleton: None,
    }
}

fn bool_const(value: bool) -> Type {
    Type {
        kinds: if value {
            KIND_BOOL_TRUE
        } else {
            KIND_BOOL_FALSE
        },
        fixnum_range: None,
        length_range: None,
        singleton: None,
    }
}

/// Infers the SBBV type of a literal constant.
pub(super) fn type_of_constant(value: Value<'_>) -> Type {
    if let Some(int) = value.int32() {
        Type::constant(int as i64)
    } else if value.is_flonum() {
        Type::kind(TypeKind::Flonum)
    } else if value.is_char() {
        Type::kind(TypeKind::Char)
    } else if value.is_null() {
        Type::kind(TypeKind::Null)
    } else if value.is_bool() {
        bool_const(value.as_bool())
    } else if value.is::<Symbol>() {
        Type::kind(TypeKind::Symbol)
    } else if value.is::<Str>() {
        Type::kind(TypeKind::String)
    } else if value.is::<Vector>() {
        Type::kind(TypeKind::Vector)
    } else if value.is::<ByteVector>() {
        Type::kind(TypeKind::Bytevector)
    } else if value.is::<Pair>() {
        Type::kind(TypeKind::Pair)
    } else {
        Type::TOP
    }
}

/// Determines the definite truth value of `ty`, if statically known.
///
/// In Scheme only `#f` is false, so any type excluding `bool-false` is truthy.
pub(super) fn truthiness(ty: &Type) -> Option<bool> {
    if ty.kinds == 0 {
        return None;
    }
    if ty.kinds == KIND_BOOL_FALSE {
        Some(false)
    } else if ty.kinds & KIND_BOOL_FALSE == 0 {
        Some(true)
    } else {
        None
    }
}

/// Maps a comparison primitive to the lattice comparison it narrows with.
pub(super) fn cmp_op(prim: Primitive) -> Option<CmpOp> {
    match prim {
        Primitive::NumericLt | Primitive::FxLt => Some(CmpOp::Lt),
        Primitive::NumericLte | Primitive::FxLe => Some(CmpOp::Le),
        Primitive::NumericGt | Primitive::FxGt => Some(CmpOp::Gt),
        Primitive::NumericGte | Primitive::FxGe => Some(CmpOp::Ge),
        Primitive::NumericEqual | Primitive::FxEq | Primitive::FxEqU | Primitive::IsEqv => {
            Some(CmpOp::Eq)
        }
        Primitive::FlLt => Some(CmpOp::Lt),
        Primitive::FlLe => Some(CmpOp::Le),
        Primitive::FlGt => Some(CmpOp::Gt),
        Primitive::FlGe => Some(CmpOp::Ge),
        Primitive::FlEq => Some(CmpOp::Eq),
        _ => None,
    }
}

/// Whether `prim` is a unary type test that [`narrow_type_test`] understands.
pub(super) fn is_type_test(prim: Primitive) -> bool {
    predicate_mask(prim).is_some()
}

/// Kinds bitset that satisfies a type predicate, for statically folding it.
fn predicate_mask(prim: Primitive) -> Option<u32> {
    Some(match prim {
        Primitive::IsNull => KIND_NULL,
        Primitive::IsPair => KIND_PAIR,
        Primitive::IsVector => KIND_VECTOR,
        Primitive::IsBytevector => KIND_BYTEVECTOR,
        Primitive::IsString => KIND_STRING,
        Primitive::IsSymbol => KIND_SYMBOL,
        Primitive::IsChar => KIND_CHAR,
        Primitive::IsBoolean => KIND_BOOL_TRUE | KIND_BOOL_FALSE,
        Primitive::IsProcedure => KIND_PROCEDURE,
        Primitive::IsFlonum => KIND_FLONUM,
        Primitive::IsFixnum => KIND_FIXNUM,
        Primitive::IsExactInteger => KIND_FIXNUM | KIND_BIGNUM,
        Primitive::IsNumber | Primitive::IsComplex | Primitive::IsReal => {
            KIND_FIXNUM | KIND_FLONUM | KIND_BIGNUM
        }
        _ => return None,
    })
}

fn predicate_ambiguous_mask(prim: Primitive) -> u32 {
    match prim {
        Primitive::IsNumber | Primitive::IsComplex | Primitive::IsReal => KIND_OTHER,
        _ => 0,
    }
}

fn is_definitely_fixnum(ty: &Type) -> bool {
    ty.kinds == KIND_FIXNUM
}

fn is_definitely_flonum(ty: &Type) -> bool {
    ty.kinds == KIND_FLONUM
}

fn interval_singleton(interval: Interval) -> Option<i64> {
    match (interval.lo, interval.hi) {
        (super::types::Bound::Int(lo), super::types::Bound::Int(hi)) if lo == hi => Some(lo),
        _ => None,
    }
}

fn interval_overflows(interval: Interval) -> bool {
    matches!(interval.lo, super::types::Bound::Overflow)
        || matches!(interval.hi, super::types::Bound::Overflow)
}

fn fixnum_from_interval(interval: Interval) -> Type {
    let singleton = interval_singleton(interval);
    Type {
        kinds: KIND_FIXNUM,
        fixnum_range: Some(interval),
        length_range: None,
        singleton,
    }
}

fn fits_fixnum(value: i64) -> bool {
    (FIXNUM_MIN..=FIXNUM_MAX).contains(&value)
}

#[derive(Clone, Copy)]
enum ArithOp {
    Add,
    Sub,
    Mul,
}

fn arith_result(op: ArithOp, a: &Type, b: &Type) -> Type {
    if is_definitely_fixnum(a) && is_definitely_fixnum(b) {
        if let (Some(ia), Some(ib)) = (a.fixnum_range, b.fixnum_range) {
            let interval = match op {
                ArithOp::Add => ia.add(ib),
                ArithOp::Sub => ia.sub(ib),
                ArithOp::Mul => ia.mul(ib),
            };
            if !interval.is_empty() && !interval_overflows(interval) {
                return fixnum_from_interval(interval);
            }
        }
        // Fixnum operands whose result may overflow into a bignum.
        return Type {
            kinds: KIND_FIXNUM | KIND_BIGNUM,
            fixnum_range: Some(Interval::TOP_FIXNUM),
            length_range: None,
            singleton: None,
        };
    }
    let exact_integer_kinds = KIND_FIXNUM | KIND_BIGNUM;
    if a.kinds != 0
        && b.kinds != 0
        && a.kinds & !exact_integer_kinds == 0
        && b.kinds & !exact_integer_kinds == 0
    {
        return Type {
            kinds: exact_integer_kinds,
            fixnum_range: Some(Interval::TOP_FIXNUM),
            length_range: None,
            singleton: None,
        };
    }
    let numeric_kinds = KIND_FIXNUM | KIND_FLONUM | KIND_BIGNUM;
    if ((is_definitely_flonum(a) && b.kinds & !numeric_kinds == 0)
        || (is_definitely_flonum(b) && a.kinds & !numeric_kinds == 0))
        && a.kinds != 0
        && b.kinds != 0
    {
        return Type::kind(TypeKind::Flonum);
    }
    Type::TOP
}

fn fold_arith(op: ArithOp, a: &Type, b: &Type) -> Option<Value<'static>> {
    let (x, y) = (a.singleton?, b.singleton?);
    let result = match op {
        ArithOp::Add => x.checked_add(y)?,
        ArithOp::Sub => x.checked_sub(y)?,
        ArithOp::Mul => x.checked_mul(y)?,
    };
    fits_fixnum(result).then(|| Value::from_i32(result as i32))
}

/// `a < b` for every possible pair of values, when provable.
fn bound_definitely_lt(a: Bound, b: Bound) -> bool {
    match (a, b) {
        (Bound::Int(x), Bound::Int(y)) => x < y,
        (Bound::VecLenMinus(v, i), Bound::VecLenMinus(w, j)) if v == w => i > j,
        // [[v]] - i ranges over [-i, FIXNUM_MAX - i].
        (Bound::VecLenMinus(_, i), Bound::Int(y)) => FIXNUM_MAX.saturating_sub(i) < y,
        (Bound::Int(x), Bound::VecLenMinus(_, i)) => x < -i,
        (Bound::Min, Bound::Int(y)) => FIXNUM_MIN < y,
        (Bound::Int(x), Bound::Max) => x < FIXNUM_MAX,
        (Bound::Min, Bound::Max) => true,
        _ => false,
    }
}

/// `a <= b` for every possible pair of values, when provable.
fn bound_definitely_le(a: Bound, b: Bound) -> bool {
    match (a, b) {
        (Bound::Int(x), Bound::Int(y)) => x <= y,
        (Bound::VecLenMinus(v, i), Bound::VecLenMinus(w, j)) if v == w => i >= j,
        (Bound::VecLenMinus(_, i), Bound::Int(y)) => FIXNUM_MAX.saturating_sub(i) <= y,
        (Bound::Int(x), Bound::VecLenMinus(_, i)) => x <= -i,
        (Bound::Min, _) => true,
        (_, Bound::Max) => true,
        _ => false,
    }
}

/// Statically decides a fixnum comparison from interval bounds, if possible.
fn compare_intervals(op: CmpOp, x: Interval, y: Interval) -> Option<bool> {
    if x.is_empty() || y.is_empty() {
        return None;
    }
    match op {
        CmpOp::Lt => {
            if bound_definitely_lt(x.hi, y.lo) {
                Some(true)
            } else if bound_definitely_le(y.hi, x.lo) {
                Some(false)
            } else {
                None
            }
        }
        CmpOp::Le => {
            if bound_definitely_le(x.hi, y.lo) {
                Some(true)
            } else if bound_definitely_lt(y.hi, x.lo) {
                Some(false)
            } else {
                None
            }
        }
        CmpOp::Gt => compare_intervals(CmpOp::Lt, y, x),
        CmpOp::Ge => compare_intervals(CmpOp::Le, y, x),
        CmpOp::Eq => {
            if let (Some(a), Some(b)) = (interval_singleton(x), interval_singleton(y)) {
                return Some(a == b);
            }
            if bound_definitely_lt(x.hi, y.lo) || bound_definitely_lt(y.hi, x.lo) {
                Some(false)
            } else {
                None
            }
        }
    }
}

fn fold_compare(op: CmpOp, a: &Type, b: &Type) -> Option<Value<'static>> {
    if let (Some(x), Some(y)) = (a.singleton, b.singleton) {
        let outcome = match op {
            CmpOp::Lt => x < y,
            CmpOp::Le => x <= y,
            CmpOp::Gt => x > y,
            CmpOp::Ge => x >= y,
            CmpOp::Eq => x == y,
        };
        return Some(Value::from_bool(outcome));
    }
    // Interval-based folding is only sound when both operands are fixnums.
    if is_definitely_fixnum(a) && is_definitely_fixnum(b) {
        let outcome = compare_intervals(op, a.fixnum_range?, b.fixnum_range?)?;
        return Some(Value::from_bool(outcome));
    }
    None
}

/// Specializes `prim` given the abstract types of its arguments.
pub(super) fn specialize_prim<'gc>(prim: Primitive, args: &[Type]) -> PrimSpec<'gc> {
    let unchanged = |result: Type| PrimSpec {
        result,
        prim,
        fold: None,
    };

    // Type predicates: fold to a boolean constant whenever the kinds prove it.
    if let Some(mask) = predicate_mask(prim) {
        if let Some(arg) = args.first()
            && arg.kinds != 0
        {
            let ambiguous = predicate_ambiguous_mask(prim);
            if arg.kinds & ambiguous == 0 && arg.kinds & mask == arg.kinds {
                return PrimSpec {
                    result: bool_const(true),
                    prim,
                    fold: Some(Value::from_bool(true)),
                };
            }
            if arg.kinds & (mask | ambiguous) == 0 {
                return PrimSpec {
                    result: bool_const(false),
                    prim,
                    fold: Some(Value::from_bool(false)),
                };
            }
        }
        return unchanged(boolean_type());
    }

    match prim {
        Primitive::Not => {
            if let Some(arg) = args.first() {
                match truthiness(arg) {
                    Some(true) => {
                        return PrimSpec {
                            result: bool_const(false),
                            prim,
                            fold: Some(Value::from_bool(false)),
                        };
                    }
                    Some(false) => {
                        return PrimSpec {
                            result: bool_const(true),
                            prim,
                            fold: Some(Value::from_bool(true)),
                        };
                    }
                    None => {}
                }
            }
            unchanged(boolean_type())
        }

        Primitive::IsZero => {
            if let Some(arg) = args.first()
                && let Some(value) = arg.singleton
            {
                return PrimSpec {
                    result: bool_const(value == 0),
                    prim,
                    fold: Some(Value::from_bool(value == 0)),
                };
            }
            unchanged(boolean_type())
        }

        Primitive::Plus | Primitive::Minus | Primitive::Times => {
            if args.len() == 2 {
                let op = match prim {
                    Primitive::Plus => ArithOp::Add,
                    Primitive::Minus => ArithOp::Sub,
                    _ => ArithOp::Mul,
                };
                let result = arith_result(op, &args[0], &args[1]);
                let fold = fold_arith(op, &args[0], &args[1]);
                return PrimSpec { result, prim, fold };
            }
            unchanged(Type::TOP)
        }

        // Overflow-checked fixnum ops: when interval arithmetic proves the
        // result stays in fixnum range, drop the overflow check entirely.
        Primitive::FxAddOvf | Primitive::FxSubOvf | Primitive::FxMulOvf => {
            if args.len() == 2 {
                let op = match prim {
                    Primitive::FxAddOvf => ArithOp::Add,
                    Primitive::FxSubOvf => ArithOp::Sub,
                    _ => ArithOp::Mul,
                };
                if is_definitely_fixnum(&args[0])
                    && is_definitely_fixnum(&args[1])
                    && let (Some(ia), Some(ib)) = (args[0].fixnum_range, args[1].fixnum_range)
                {
                    let interval = match op {
                        ArithOp::Add => ia.add(ib),
                        ArithOp::Sub => ia.sub(ib),
                        ArithOp::Mul => ia.mul(ib),
                    };
                    if !interval.is_empty() && !interval_overflows(interval) {
                        let result = fixnum_from_interval(interval);
                        let fold = fold_arith(op, &args[0], &args[1]);
                        return PrimSpec {
                            result,
                            prim: match prim {
                                Primitive::FxAddOvf => Primitive::FxAdd,
                                Primitive::FxSubOvf => Primitive::FxSub,
                                _ => Primitive::FxMul,
                            },
                            fold,
                        };
                    }
                }
                // Overflow possible: fixnum result or #f.
                return unchanged(Type {
                    kinds: KIND_FIXNUM | KIND_BOOL_FALSE,
                    fixnum_range: Some(Interval::TOP_FIXNUM),
                    length_range: None,
                    singleton: None,
                });
            }
            unchanged(Type::TOP)
        }

        Primitive::FxAdd | Primitive::FxSub | Primitive::FxMul => {
            if args.len() == 2 {
                let op = match prim {
                    Primitive::FxAdd => ArithOp::Add,
                    Primitive::FxSub => ArithOp::Sub,
                    _ => ArithOp::Mul,
                };
                if let (Some(ia), Some(ib)) = (args[0].fixnum_range, args[1].fixnum_range) {
                    let interval = match op {
                        ArithOp::Add => ia.add(ib),
                        ArithOp::Sub => ia.sub(ib),
                        ArithOp::Mul => ia.mul(ib),
                    };
                    if !interval.is_empty() && !interval_overflows(interval) {
                        let fold = fold_arith(op, &args[0], &args[1]);
                        return PrimSpec {
                            result: fixnum_from_interval(interval),
                            prim,
                            fold,
                        };
                    }
                }
            }
            unchanged(Type::kind(TypeKind::Fixnum))
        }

        Primitive::FxLt
        | Primitive::FxLe
        | Primitive::FxGt
        | Primitive::FxGe
        | Primitive::FxEqU => {
            if args.len() == 2 {
                let op = cmp_op(prim).expect("fixnum comparison has a cmp op");
                if let Some(value) = fold_compare(op, &args[0], &args[1]) {
                    return PrimSpec {
                        result: bool_const(value.as_bool()),
                        prim,
                        fold: Some(value),
                    };
                }
            }
            unchanged(boolean_type())
        }

        Primitive::FlAdd | Primitive::FlSub | Primitive::FlMul | Primitive::FlDiv => {
            unchanged(Type::kind(TypeKind::Flonum))
        }

        Primitive::FlLt | Primitive::FlLe | Primitive::FlGt | Primitive::FlGe | Primitive::FlEq => {
            if args.len() == 2 {
                let op = cmp_op(prim).expect("flonum comparison has a cmp op");
                if let Some(value) = fold_compare(op, &args[0], &args[1]) {
                    return PrimSpec {
                        result: bool_const(value.as_bool()),
                        prim,
                        fold: Some(value),
                    };
                }
            }
            unchanged(boolean_type())
        }

        Primitive::Sqrt | Primitive::Atan => {
            if args.first().is_some_and(|arg| arg.kinds == KIND_FLONUM) {
                let specialized = match prim {
                    Primitive::Sqrt => Primitive::FlSqrt,
                    _ => Primitive::FlAtan,
                };
                return PrimSpec {
                    result: Type::kind(TypeKind::Flonum),
                    prim: specialized,
                    fold: None,
                };
            }
            unchanged(Type::TOP)
        }

        Primitive::Quotient | Primitive::Remainder => unchanged(Type::TOP),

        Primitive::StringRef => PrimSpec {
            result: Type::kind(TypeKind::Char),
            prim,
            fold: None,
        },

        Primitive::BytevectorU8Ref => PrimSpec {
            result: Type::kind(TypeKind::Fixnum),
            prim,
            fold: None,
        },

        Primitive::NumericLt
        | Primitive::NumericLte
        | Primitive::NumericGt
        | Primitive::NumericGte
        | Primitive::NumericEqual => {
            if args.len() == 2 {
                let op = cmp_op(prim).expect("numeric comparison has a cmp op");
                let fold = fold_compare(op, &args[0], &args[1]);
                let specialized =
                    if is_definitely_fixnum(&args[0]) && is_definitely_fixnum(&args[1]) {
                        match prim {
                            Primitive::NumericLt => Primitive::FxLt,
                            Primitive::NumericLte => Primitive::FxLe,
                            Primitive::NumericGt => Primitive::FxGt,
                            Primitive::NumericGte => Primitive::FxGe,
                            _ => Primitive::FxEqU,
                        }
                    } else if is_definitely_flonum(&args[0]) && is_definitely_flonum(&args[1]) {
                        match prim {
                            Primitive::NumericLt => Primitive::FlLt,
                            Primitive::NumericLte => Primitive::FlLe,
                            Primitive::NumericGt => Primitive::FlGt,
                            Primitive::NumericGte => Primitive::FlGe,
                            _ => Primitive::FlEq,
                        }
                    } else {
                        prim
                    };
                return PrimSpec {
                    result: boolean_type(),
                    prim: specialized,
                    fold,
                };
            }
            unchanged(boolean_type())
        }

        // `case` expands to `eqv?`; RestLength is a fixnum, so rewrite to fx=?.
        Primitive::IsEqv => {
            if args.len() == 2 && is_definitely_fixnum(&args[0]) && is_definitely_fixnum(&args[1]) {
                let fold = fold_compare(CmpOp::Eq, &args[0], &args[1]);
                return PrimSpec {
                    result: boolean_type(),
                    prim: Primitive::FxEqU,
                    fold,
                };
            }
            unchanged(boolean_type())
        }

        Primitive::Car | Primitive::Cdr | Primitive::SetCar | Primitive::SetCdr => {
            let definitely_pair = args.first().is_some_and(|arg| arg.kinds == KIND_PAIR);
            let specialized = match (prim, definitely_pair) {
                (Primitive::Car, true) => Primitive::CarUnchecked,
                (Primitive::Cdr, true) => Primitive::CdrUnchecked,
                (Primitive::SetCar, true) => Primitive::SetCarUnchecked,
                (Primitive::SetCdr, true) => Primitive::SetCdrUnchecked,
                _ => prim,
            };
            let result = if matches!(prim, Primitive::SetCar | Primitive::SetCdr) {
                Type::kind(TypeKind::Void)
            } else {
                Type::TOP
            };
            PrimSpec {
                result,
                prim: specialized,
                fold: None,
            }
        }

        Primitive::VectorRef => PrimSpec {
            result: Type::TOP,
            prim,
            fold: None,
        },

        Primitive::VectorSet => PrimSpec {
            result: Type::kind(TypeKind::Void),
            prim,
            fold: None,
        },

        Primitive::Length
        | Primitive::StringLength
        | Primitive::BytevectorLength
        | Primitive::VectorLengthUnchecked
        | Primitive::StringLengthUnchecked
        | Primitive::BytevectorLengthUnchecked => {
            unchanged(Type::fixnum(Bound::Int(0), Bound::Max))
        }

        _ => unchanged(Type::TOP),
    }
}

/// Narrows the typing context for a unary type-test primitive.
pub(super) fn narrow_type_test(
    prim: Primitive,
    arg: ValueId,
    ctx: &TypeContext,
) -> Option<(TypeContext, TypeContext)> {
    let mask = predicate_mask(prim)?;
    let ambiguous = predicate_ambiguous_mask(prim);
    let base = ctx.get(arg);
    if base.kinds == 0 {
        return None;
    }
    let mut true_ctx = ctx.clone();
    let mut false_ctx = ctx.clone();
    let true_ty = intersect_types(
        base.clone(),
        Type {
            kinds: base.kinds & (mask | ambiguous),
            fixnum_range: base.fixnum_range,
            length_range: base.length_range,
            singleton: base.singleton,
        },
    );
    let false_ty = Type {
        kinds: base.kinds & !mask,
        fixnum_range: if base.kinds & KIND_FIXNUM != 0 && mask & KIND_FIXNUM == 0 {
            base.fixnum_range
        } else {
            None
        },
        length_range: base.length_range,
        singleton: None,
    };
    true_ctx.set(arg, true_ty);
    false_ctx.set(arg, false_ty);
    Some((true_ctx, false_ctx))
}

/// Narrows operand intervals for a binary comparison primitive.
pub(super) fn narrow_binary_test(
    prim: Primitive,
    lhs: ValueId,
    rhs: ValueId,
    ctx: &TypeContext,
) -> Option<(TypeContext, TypeContext)> {
    let op = cmp_op(prim)?;
    Some(ctx.narrow_for_predicate(op, lhs, rhs))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn checked_multiply_remains_checked_when_an_endpoint_can_overflow() {
        let broad_nonnegative = Type::fixnum(Bound::Int(0), Bound::Max);
        let ten = Type::constant(10);

        let spec = specialize_prim(Primitive::FxMulOvf, &[broad_nonnegative, ten]);

        assert_eq!(spec.prim, Primitive::FxMulOvf);
        assert_eq!(spec.result.kinds, KIND_FIXNUM | KIND_BOOL_FALSE);
    }

    #[test]
    fn checked_multiply_remains_checked_when_negating_minimum_fixnum() {
        let negative_one = Type::constant(-1);
        let minimum = Type::constant(FIXNUM_MIN);

        for args in [
            [negative_one.clone(), minimum.clone()],
            [minimum.clone(), negative_one.clone()],
        ] {
            let spec = specialize_prim(Primitive::FxMulOvf, &args);
            assert_eq!(spec.prim, Primitive::FxMulOvf);
            assert_eq!(spec.result.kinds, KIND_FIXNUM | KIND_BOOL_FALSE);
        }
    }

    #[test]
    fn checked_add_remains_checked_for_unbounded_fixnums() {
        let fixnum = Type::fixnum(Bound::Min, Bound::Max);

        let spec = specialize_prim(Primitive::FxAddOvf, &[fixnum.clone(), fixnum]);

        assert_eq!(spec.prim, Primitive::FxAddOvf);
        assert_eq!(spec.result.kinds, KIND_FIXNUM | KIND_BOOL_FALSE);
    }

    #[test]
    fn checked_subtract_remains_checked_for_unbounded_fixnums() {
        let fixnum = Type::fixnum(Bound::Min, Bound::Max);

        let spec = specialize_prim(Primitive::FxSubOvf, &[fixnum.clone(), fixnum]);

        assert_eq!(spec.prim, Primitive::FxSubOvf);
        assert_eq!(spec.result.kinds, KIND_FIXNUM | KIND_BOOL_FALSE);
    }

    #[test]
    fn checked_fixnum_operation_is_not_removed_for_a_mixed_type() {
        let mixed = Type {
            kinds: KIND_FIXNUM | KIND_BIGNUM,
            fixnum_range: Some(Interval {
                lo: Bound::Int(0),
                hi: Bound::Int(10),
            }),
            length_range: None,
            singleton: None,
        };
        let one = Type::constant(1);

        let spec = specialize_prim(Primitive::FxAddOvf, &[mixed, one]);

        assert_eq!(spec.prim, Primitive::FxAddOvf);
    }

    #[test]
    fn mixed_numeric_union_is_not_inferred_as_flonum() {
        let mixed = Type {
            kinds: KIND_FIXNUM | KIND_FLONUM,
            fixnum_range: Some(Interval::TOP_FIXNUM),
            length_range: None,
            singleton: None,
        };
        let fixnum = Type::kind(TypeKind::Fixnum);

        assert_eq!(arith_result(ArithOp::Add, &mixed, &fixnum), Type::TOP);
        assert_eq!(arith_result(ArithOp::Add, &fixnum, &mixed), Type::TOP);
    }

    #[test]
    fn definite_flonum_dominates_numeric_union() {
        let flonum = Type::kind(TypeKind::Flonum);
        let mixed = Type {
            kinds: KIND_FIXNUM | KIND_FLONUM | KIND_BIGNUM,
            fixnum_range: Some(Interval::TOP_FIXNUM),
            length_range: None,
            singleton: None,
        };

        assert_eq!(
            arith_result(ArithOp::Add, &flonum, &mixed),
            Type::kind(TypeKind::Flonum)
        );
        assert_eq!(
            arith_result(ArithOp::Add, &mixed, &flonum),
            Type::kind(TypeKind::Flonum)
        );
    }

    #[test]
    fn exact_integer_arithmetic_preserves_exactness_after_fixnum_overflow() {
        let exact_integer = Type {
            kinds: KIND_FIXNUM | KIND_BIGNUM,
            fixnum_range: Some(Interval::TOP_FIXNUM),
            length_range: None,
            singleton: None,
        };
        let fixnum = Type::kind(TypeKind::Fixnum);

        for op in [ArithOp::Add, ArithOp::Sub, ArithOp::Mul] {
            let result = arith_result(op, &exact_integer, &fixnum);
            assert_eq!(result.kinds, KIND_FIXNUM | KIND_BIGNUM);
            assert_eq!(result.fixnum_range, Some(Interval::TOP_FIXNUM));
        }
    }

    #[test]
    fn checked_indexed_access_does_not_reuse_an_unrelated_symbolic_bound() {
        let index = Type::fixnum(Bound::Int(0), Bound::VecLenMinus(ValueId(99), 1));

        for (prim, container) in [
            (Primitive::VectorRef, Type::kind(TypeKind::Vector)),
            (Primitive::VectorSet, Type::kind(TypeKind::Vector)),
            (Primitive::StringRef, Type::kind(TypeKind::String)),
            (Primitive::BytevectorU8Ref, Type::kind(TypeKind::Bytevector)),
        ] {
            let spec = specialize_prim(prim, &[container, index.clone(), Type::TOP]);
            assert_eq!(spec.prim, prim);
        }
    }

    #[test]
    fn checked_division_is_not_respecialized_inside_its_slow_path() {
        let minimum = Type::constant(FIXNUM_MIN);
        let negative_one = Type::constant(-1);

        for prim in [Primitive::Quotient, Primitive::Remainder] {
            let spec = specialize_prim(prim, &[minimum.clone(), negative_one.clone()]);
            assert_eq!(spec.prim, prim);
        }
    }

    #[test]
    fn ambiguous_heap_numbers_remain_on_both_type_test_branches() {
        let value = ValueId(1);
        let mut ctx = TypeContext::new();
        ctx.set(value, Type::kind(TypeKind::Other));

        for prim in [Primitive::IsNumber, Primitive::IsComplex, Primitive::IsReal] {
            let spec = specialize_prim(prim, &[Type::kind(TypeKind::Other)]);
            assert_eq!(spec.result, boolean_type());
            assert!(spec.fold.is_none());

            let (true_ctx, false_ctx) = narrow_type_test(prim, value, &ctx).unwrap();
            assert_eq!(true_ctx.get(value).kinds, KIND_OTHER);
            assert_eq!(false_ctx.get(value).kinds, KIND_OTHER);
        }
    }

    #[test]
    fn eqv_on_fixnums_specializes_to_fx_eq() {
        let a = Type::constant(1);
        let b = Type::constant(1);
        let spec = specialize_prim(Primitive::IsEqv, &[a.clone(), b.clone()]);
        assert_eq!(spec.prim, Primitive::FxEqU);
        assert_eq!(spec.fold, Some(Value::from_bool(true)));

        let c = Type::constant(2);
        let spec = specialize_prim(Primitive::IsEqv, &[a, c]);
        assert_eq!(spec.prim, Primitive::FxEqU);
        assert_eq!(spec.fold, Some(Value::from_bool(false)));

        // Non-fixnum eqv? stays generic.
        let spec = specialize_prim(
            Primitive::IsEqv,
            &[Type::kind(TypeKind::Symbol), Type::kind(TypeKind::Symbol)],
        );
        assert_eq!(spec.prim, Primitive::IsEqv);
        assert!(spec.fold.is_none());
    }
}
