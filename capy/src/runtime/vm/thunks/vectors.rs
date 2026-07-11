use super::{ThunkResult, make_assertion_violation};
use crate::rsgc::Gc;
use crate::runtime::value::{ByteVector, IntoValue, Number};
use crate::runtime::{
    Context,
    value::{Str, Symbol, Tuple, Value, Vector},
};

pub fn make_vector<'gc>(ctx: Context<'gc>, size: Value<'gc>, fill: Value<'gc>) -> ThunkResult<'gc> {
    let Some(size) = size.number() else {
        return ThunkResult {
            code: 1,
            value: make_assertion_violation(
                ctx,
                Symbol::from_str(ctx, "make-vector").into(),
                Str::new(*ctx, "not a number", true).into(),
                &[size],
            ),
        };
    };

    if !size.is_exact_integer() {
        return ThunkResult {
            code: 1,
            value: make_assertion_violation(
                ctx,
                Symbol::from_str(ctx, "make-vector").into(),
                Str::new(*ctx, "not an exact integer", true).into(),
                &[size.into_value(ctx)],
            ),
        };
    }

    let Some(size) = size.exact_integer_to_usize() else {
        return ThunkResult {
            code: 1,
            value: make_assertion_violation(
                ctx,
                Symbol::from_str(ctx, "make-vector").into(),
                Str::new(*ctx, "not in usize range", true).into(),
                &[size.into_value(ctx)],
            ),
        };
    };

    if size >= i32::MAX as usize {
        return ThunkResult {
            code: 1,
            value: make_assertion_violation(
                ctx,
                Symbol::from_str(ctx, "make-vector").into(),
                Str::new(*ctx, "size too large", true).into(),
                &[Number::from_usize(ctx, size).into_value(ctx)],
            ),
        };
    }

    ThunkResult {
        code: 0,
        value: Vector::new::<false>(*ctx, size, fill).into(),
    }
}

pub fn make_tuple<'gc>(ctx: Context<'gc>, size: Value<'gc>, fill: Value<'gc>) -> ThunkResult<'gc> {
    let Some(size) = size.number() else {
        return ThunkResult {
            code: 1,
            value: make_assertion_violation(
                ctx,
                Symbol::from_str(ctx, "make-tuple").into(),
                Str::new(*ctx, "not a number", true).into(),
                &[size],
            ),
        };
    };

    if !size.is_exact_integer() {
        return ThunkResult {
            code: 1,
            value: make_assertion_violation(
                ctx,
                Symbol::from_str(ctx, "make-tuple").into(),
                Str::new(*ctx, "not an exact integer", true).into(),
                &[size.into_value(ctx)],
            ),
        };
    }

    let Some(size) = size.exact_integer_to_usize() else {
        return ThunkResult {
            code: 1,
            value: make_assertion_violation(
                ctx,
                Symbol::from_str(ctx, "make-tuple").into(),
                Str::new(*ctx, "not in usize range", true).into(),
                &[size.into_value(ctx)],
            ),
        };
    };

    if size >= i32::MAX as usize {
        return ThunkResult {
            code: 1,
            value: make_assertion_violation(
                ctx,
                Symbol::from_str(ctx, "make-tuple").into(),
                Str::new(*ctx, "size too large", true).into(),
                &[Number::from_usize(ctx, size).into_value(ctx)],
            ),
        };
    }

    ThunkResult {
        code: 0,
        value: Tuple::new(*ctx, size, fill).into(),
    }
}

pub fn vector_ref<'gc>(ctx: Context<'gc>, vec: Value<'gc>, index: Value<'gc>) -> ThunkResult<'gc> {
    let Some(v) = vec.try_as::<Vector>() else {
        return ThunkResult {
            code: 1,
            value: make_assertion_violation(
                ctx,
                Symbol::from_str(ctx, "vector-ref").into(),
                Str::new(*ctx, "not a vector", true).into(),
                &[vec],
            ),
        };
    };

    let Some(index) = index.number() else {
        return ThunkResult {
            code: 1,
            value: make_assertion_violation(
                ctx,
                Symbol::from_str(ctx, "vector-ref").into(),
                Str::new(*ctx, "not a number", true).into(),
                &[index],
            ),
        };
    };

    if !index.is_exact_integer() {
        return ThunkResult {
            code: 1,
            value: make_assertion_violation(
                ctx,
                Symbol::from_str(ctx, "vector-ref").into(),
                Str::new(*ctx, "not an exact integer", true).into(),
                &[index.into_value(ctx)],
            ),
        };
    }

    let Some(index) = index
        .exact_integer_to_usize()
        .filter(|_| !index.is_negative())
    else {
        return ThunkResult {
            code: 1,
            value: make_assertion_violation(
                ctx,
                Symbol::from_str(ctx, "vector-ref").into(),
                Str::new(*ctx, "not in usize range", true).into(),
                &[index.into_value(ctx)],
            ),
        };
    };

    if index >= v.len() {
        return ThunkResult {
            code: 1,
            value: make_assertion_violation(
                ctx,
                Symbol::from_str(ctx, "vector-ref").into(),
                Str::new(*ctx, "index out of range", true).into(),
                &[
                    Number::from_usize(ctx, index).into_value(ctx),
                    Number::from_usize(ctx, v.len()).into_value(ctx),
                ],
            ),
        };
    }

    ThunkResult {
        code: 0,
        value: v[index].get(),
    }
}

pub fn vector_set<'gc>(
    ctx: Context<'gc>,
    vec: Value<'gc>,
    index: Value<'gc>,
    new_value: Value<'gc>,
) -> ThunkResult<'gc> {
    let Some(v) = vec.try_as::<Vector>() else {
        return ThunkResult {
            code: 1,
            value: make_assertion_violation(
                ctx,
                Symbol::from_str(ctx, "vector-set!").into(),
                Str::new(*ctx, "not a vector", true).into(),
                &[vec],
            ),
        };
    };

    let Some(index) = index.number() else {
        return ThunkResult {
            code: 1,
            value: make_assertion_violation(
                ctx,
                Symbol::from_str(ctx, "vector-set!").into(),
                Str::new(*ctx, "not a number", true).into(),
                &[index],
            ),
        };
    };

    if !index.is_exact_integer() {
        return ThunkResult {
            code: 1,
            value: make_assertion_violation(
                ctx,
                Symbol::from_str(ctx, "vector-set!").into(),
                Str::new(*ctx, "not an exact integer", true).into(),
                &[index.into_value(ctx)],
            ),
        };
    }

    let Some(index) = index
        .exact_integer_to_usize()
        .filter(|_| !index.is_negative())
    else {
        return ThunkResult {
            code: 1,
            value: make_assertion_violation(
                ctx,
                Symbol::from_str(ctx, "vector-set!").into(),
                Str::new(*ctx, "not in usize range", true).into(),
                &[index.into_value(ctx)],
            ),
        };
    };

    if index >= v.len() {
        return ThunkResult {
            code: 1,
            value: make_assertion_violation(
                ctx,
                Symbol::from_str(ctx, "vector-set!").into(),
                Str::new(*ctx, "index out of range", true).into(),
                &[
                    Number::from_usize(ctx, index).into_value(ctx),
                    Number::from_usize(ctx, v.len()).into_value(ctx),
                ],
            ),
        };
    }

    let wv = Gc::write(*ctx, v);
    wv[index].unlock().set(new_value);

    ThunkResult {
        code: 0,
        value: Value::undefined(),
    }
}

pub fn tuple_size<'gc>(ctx: Context<'gc>, tup: Value<'gc>) -> ThunkResult<'gc> {
    let Some(t) = tup.try_as::<Tuple>() else {
        return ThunkResult {
            code: 1,
            value: make_assertion_violation(
                ctx,
                Symbol::from_str(ctx, "tuple-size").into(),
                Str::new(*ctx, "not a tuple", true).into(),
                &[tup],
            ),
        };
    };

    ThunkResult {
        code: 0,
        value: Number::from_usize(ctx, t.len()).into_value(ctx),
    }
}

pub fn bytevector_length<'gc>(ctx: Context<'gc>, bv: Value<'gc>) -> ThunkResult<'gc> {
    let Some(bv) = bv.try_as::<ByteVector>() else {
        return ThunkResult {
            code: 1,
            value: make_assertion_violation(
                ctx,
                Symbol::from_str(ctx, "bytevector-length").into(),
                Str::new(*ctx, "not a bytevector", true).into(),
                &[bv],
            ),
        };
    };
    ThunkResult {
        code: 0,
        value: Number::from_usize(ctx, bv.len()).into_value(ctx),
    }
}

pub fn bytevector_u8_ref<'gc>(
    ctx: Context<'gc>,
    bv: Value<'gc>,
    index: Value<'gc>,
) -> ThunkResult<'gc> {
    let Some(bv) = bv.try_as::<ByteVector>() else {
        return ThunkResult {
            code: 1,
            value: make_assertion_violation(
                ctx,
                Symbol::from_str(ctx, "bytevector-u8-ref").into(),
                Str::new(*ctx, "not a bytevector", true).into(),
                &[bv],
            ),
        };
    };

    let Some(index) = index.int32() else {
        return ThunkResult {
            code: 1,
            value: make_assertion_violation(
                ctx,
                Symbol::from_str(ctx, "bytevector-u8-ref").into(),
                Str::new(*ctx, "not a fixnum", true).into(),
                &[index],
            ),
        };
    };

    let index = index as usize;
    if index >= bv.len() {
        return ThunkResult {
            code: 1,
            value: make_assertion_violation(
                ctx,
                Symbol::from_str(ctx, "bytevector-u8-ref").into(),
                Str::new(*ctx, "index out of bounds", true).into(),
                &[bv.into(), index.into_value(ctx)],
            ),
        };
    }

    ThunkResult {
        code: 0,
        value: Value::from_i32(bv[index] as i32),
    }
}
