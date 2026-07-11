use super::{ThunkResult, make_assertion_violation};
use crate::prelude::ClosureRef;
use crate::rsgc::Gc;
use crate::runtime::vm::syntax::props_to_sourcev;
use crate::runtime::{
    Context,
    value::{Str, Symbol, Value},
};
use crate::runtime::{
    value::{IntoValue, Number},
    vm::{control::ContinuationMarks, syntax::Syntax},
};

pub fn string2symbol<'gc>(ctx: Context<'gc>, s: Value<'gc>) -> ThunkResult<'gc> {
    let Some(s) = s.try_as::<Str>() else {
        return ThunkResult {
            code: 1,
            value: make_assertion_violation(
                ctx,
                Symbol::from_str(ctx, "string->symbol").into(),
                Str::new(*ctx, "not a string", true).into(),
                &[s],
            ),
        };
    };

    ThunkResult {
        code: 0,
        value: Symbol::from_string(ctx, s).into(),
    }
}

pub fn symbol2string<'gc>(ctx: Context<'gc>, s: Value<'gc>) -> ThunkResult<'gc> {
    let Some(s) = s.try_as::<Symbol>() else {
        return ThunkResult {
            code: 1,
            value: make_assertion_violation(
                ctx,
                Symbol::from_str(ctx, "symbol->string").into(),
                Str::new(*ctx, "not a symbol", true).into(),
                &[s],
            ),
        };
    };

    ThunkResult {
        code: 0,
        value: s.to_str(*ctx).into(),
    }
}

pub fn string_ref_value<'gc>(
    ctx: Context<'gc>,
    s: Value<'gc>,
    index: Value<'gc>,
) -> ThunkResult<'gc> {
    let Some(s) = s.try_as::<Str>() else {
        return ThunkResult {
            code: 1,
            value: make_assertion_violation(
                ctx,
                Symbol::from_str(ctx, "string-ref").into(),
                Str::new(*ctx, "not a string", true).into(),
                &[s],
            ),
        };
    };

    let Some(index) = index.int32() else {
        return ThunkResult {
            code: 1,
            value: make_assertion_violation(
                ctx,
                Symbol::from_str(ctx, "string-ref").into(),
                Str::new(*ctx, "not a fixnum", true).into(),
                &[index],
            ),
        };
    };

    let index = index as usize;
    if index >= s.len() {
        return ThunkResult {
            code: 1,
            value: make_assertion_violation(
                ctx,
                Symbol::from_str(ctx, "string-ref").into(),
                Str::new(*ctx, "index out of bounds", true).into(),
                &[s.into(), index.into_value(ctx)],
            ),
        };
    }

    let ch = s.get(index).expect("bounds checked above");
    ThunkResult {
        code: 0,
        value: Value::from_char(ch),
    }
}

/// Unchecked string-ref for SBBV specialized paths (caller proves bounds).
pub fn string_ref_unchecked_value<'gc>(
    ctx: Context<'gc>,
    s: Value<'gc>,
    index: Value<'gc>,
) -> ThunkResult<'gc> {
    let s = s
        .try_as::<Str>()
        .expect("string-ref/unchecked: not a string");
    let index = index.int32().expect("string-ref/unchecked: not a fixnum") as usize;
    let ch = s.get(index).expect("string-ref/unchecked: out of bounds");
    let _ = ctx;
    ThunkResult {
        code: 0,
        value: Value::from_char(ch),
    }
}

pub fn logxor<'gc>(ctx: Context<'gc>, a: Value<'gc>, b: Value<'gc>) -> ThunkResult<'gc> {
    let Some(a) = a.number() else {
        return ThunkResult {
            code: 1,
            value: make_assertion_violation(
                ctx,
                Symbol::from_str(ctx, "logxor").into(),
                Str::new(*ctx, "not a number", true).into(),
                &[a],
            ),
        };
    };

    let Some(b) = b.number() else {
        return ThunkResult {
            code: 1,
            value: make_assertion_violation(
                ctx,
                Symbol::from_str(ctx, "logxor").into(),
                Str::new(*ctx, "not a number", true).into(),
                &[b],
            ),
        };
    };

    if !a.is_exact_integer() {
        return ThunkResult {
            code: 1,
            value: make_assertion_violation(
                ctx,
                Symbol::from_str(ctx, "logxor").into(),
                Str::new(*ctx, "not an exact integer", true).into(),
                &[a.into_value(ctx)],
            ),
        };
    }

    if !b.is_exact_integer() {
        return ThunkResult {
            code: 1,
            value: make_assertion_violation(
                ctx,
                Symbol::from_str(ctx, "logxor").into(),
                Str::new(*ctx, "not an exact integer", true).into(),
                &[b.into_value(ctx)],
            ),
        };
    }

    ThunkResult {
        code: 0,
        value: a.logxor(ctx, b).into_value(ctx),
    }
}

pub fn char_to_integer<'gc>(ctx: Context<'gc>, c: Value<'gc>) -> ThunkResult<'gc> {
    if !c.is_char() {
        // SAFETY: Return address slot is valid — set up by the native calling convention
        let ret = unsafe { crate::runtime::vm::thunks::helpers::llvm_return_address() };
        backtrace::resolve(ret as *mut _, |symbol| {
            log::trace!("CHAR->INTEGER error {c}");
            log::trace!("{symbol:?}");
        });
        crate::runtime::vm::debug::print_stacktraces_impl(ctx);
        return ThunkResult {
            code: 1,
            value: make_assertion_violation(
                ctx,
                Symbol::from_str(ctx, "char->integer").into(),
                Str::new(*ctx, "not a char", true).into(),
                &[c],
            ),
        };
    }
    let c = c.char();

    ThunkResult {
        code: 0,
        value: Number::from_u32(ctx, c as u32).into_value(ctx),
    }
}

pub fn integer_to_char<'gc>(ctx: Context<'gc>, n: Value<'gc>) -> ThunkResult<'gc> {
    let Some(n) = n.number() else {
        return ThunkResult {
            code: 1,
            value: make_assertion_violation(
                ctx,
                Symbol::from_str(ctx, "integer->char").into(),
                Str::new(*ctx, "not a number", true).into(),
                &[n],
            ),
        };
    };

    if !n.is_exact_integer() {
        return ThunkResult {
            code: 1,
            value: make_assertion_violation(
                ctx,
                Symbol::from_str(ctx, "integer->char").into(),
                Str::new(*ctx, "not an exact integer", true).into(),
                &[n.into_value(ctx)],
            ),
        };
    }

    let Some(u) = n.exact_integer_to_u32() else {
        return ThunkResult {
            code: 1,
            value: make_assertion_violation(
                ctx,
                Symbol::from_str(ctx, "integer->char").into(),
                Str::new(*ctx, "not in char range", true).into(),
                &[n.into_value(ctx)],
            ),
        };
    };

    let Some(c) = std::char::from_u32(u) else {
        return ThunkResult {
            code: 1,
            value: make_assertion_violation(
                ctx,
                Symbol::from_str(ctx, "integer->char").into(),
                Str::new(*ctx, "not in char range", true).into(),
                &[n.into_value(ctx)],
            ),
        };
    };

    ThunkResult {
        code: 0,
        value: Value::from_char(c),
    }
}

pub fn push_cframe<'gc>(
    ctx: Context<'gc>,
    key: Value<'gc>,
    mark: Value<'gc>,
    retk: ClosureRef<'gc>,
) -> Value<'gc> {
    crate::runtime::vm::control::push_cframe(ctx, key, mark, retk)
}

pub fn current_continuation_marks<'gc>(ctx: Context<'gc>) -> Value<'gc> {
    let marks = ctx.state().current_marks();

    let obj = Gc::new_with_header_word(
        *ctx,
        ContinuationMarks { cmarks: marks },
        crate::runtime::vm::control::continuation_marks_header_word(),
    );

    obj.into()
}

pub fn set_attachments<'gc>(ctx: Context<'gc>, marks: Value<'gc>) -> ThunkResult<'gc> {
    if !marks.is::<ContinuationMarks>() {
        return ThunkResult {
            code: 1,
            value: make_assertion_violation(
                ctx,
                Symbol::from_str(ctx, "set-attachments").into(),
                Str::new(*ctx, "not continuation-marks", true).into(),
                &[marks],
            ),
        };
    }
    // SAFETY: Preconditions verified by the surrounding code
    unsafe {
        ctx.state()
            .set_current_marks(marks.downcast::<ContinuationMarks>().cmarks);
    }

    ThunkResult {
        code: 0,
        value: Value::undefined(),
    }
}

pub fn make_syntax<'gc>(
    ctx: Context<'gc>,
    exp: Value<'gc>,
    wrap: Value<'gc>,
    module: Value<'gc>,
    source: Value<'gc>,
    properties: Value<'gc>,
) -> Value<'gc> {
    let source = if source.is_pair() {
        props_to_sourcev(ctx, source)
    } else {
        source
    };
    Syntax::new(ctx, exp, wrap, module, source, properties).into()
}
