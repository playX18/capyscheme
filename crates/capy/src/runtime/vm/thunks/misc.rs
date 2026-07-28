use super::make_assertion_violation;
use crate::heap::Gc;
use crate::prelude::ClosureRef;
use crate::runtime::vm::syntax::props_to_sourcev;
use crate::runtime::vm::thunk_raise;
use crate::runtime::{
    Context,
    value::{Str, Symbol, Value},
};
use crate::runtime::{
    value::{IntoValue, Number},
    vm::{control::ContinuationMarks, syntax::Syntax},
};

pub fn string2symbol<'gc>(ctx: Context<'gc>, s: Value<'gc>) -> Value<'gc> {
    let Some(s) = s.try_as::<Str>() else {
        thunk_raise(
            ctx,
            make_assertion_violation(
                ctx,
                Symbol::from_str(ctx, "string->symbol").into(),
                Str::new(ctx, "not a string", true).into(),
                &[s],
            ),
        );
    };

    Symbol::from_string(ctx, s).into()
}

pub fn symbol2string<'gc>(ctx: Context<'gc>, s: Value<'gc>) -> Value<'gc> {
    let Some(s) = s.try_as::<Symbol>() else {
        thunk_raise(
            ctx,
            make_assertion_violation(
                ctx,
                Symbol::from_str(ctx, "symbol->string").into(),
                Str::new(ctx, "not a symbol", true).into(),
                &[s],
            ),
        );
    };

    s.to_str(ctx).into()
}

pub fn string_ref_value<'gc>(ctx: Context<'gc>, s: Value<'gc>, index: Value<'gc>) -> Value<'gc> {
    let Some(s) = s.try_as::<Str>() else {
        thunk_raise(
            ctx,
            make_assertion_violation(
                ctx,
                Symbol::from_str(ctx, "string-ref").into(),
                Str::new(ctx, "not a string", true).into(),
                &[s],
            ),
        );
    };

    let Some(index) = index.int32() else {
        thunk_raise(
            ctx,
            make_assertion_violation(
                ctx,
                Symbol::from_str(ctx, "string-ref").into(),
                Str::new(ctx, "not a fixnum", true).into(),
                &[index],
            ),
        );
    };

    let index = index as usize;
    if index >= s.len() {
        thunk_raise(
            ctx,
            make_assertion_violation(
                ctx,
                Symbol::from_str(ctx, "string-ref").into(),
                Str::new(ctx, "index out of bounds", true).into(),
                &[s.into(), index.into_value(ctx)],
            ),
        );
    }

    let ch = s.get(index).expect("bounds checked above");
    Value::from_char(ch)
}

/// Unchecked string-ref for SBBV specialized paths (caller proves bounds).
pub fn string_ref_unchecked_value<'gc>(
    ctx: Context<'gc>,
    s: Value<'gc>,
    index: Value<'gc>,
) -> Value<'gc> {
    let s = s
        .try_as::<Str>()
        .expect("string-ref/unchecked: not a string");
    let index = index.int32().expect("string-ref/unchecked: not a fixnum") as usize;
    let ch = s.get(index).expect("string-ref/unchecked: out of bounds");
    let _ = ctx;
    Value::from_char(ch)
}

pub fn logxor<'gc>(ctx: Context<'gc>, a: Value<'gc>, b: Value<'gc>) -> Value<'gc> {
    let Some(a) = a.number() else {
        thunk_raise(
            ctx,
            make_assertion_violation(
                ctx,
                Symbol::from_str(ctx, "logxor").into(),
                Str::new(ctx, "not a number", true).into(),
                &[a],
            ),
        );
    };

    let Some(b) = b.number() else {
        thunk_raise(
            ctx,
            make_assertion_violation(
                ctx,
                Symbol::from_str(ctx, "logxor").into(),
                Str::new(ctx, "not a number", true).into(),
                &[b],
            ),
        );
    };

    if !a.is_exact_integer() {
        thunk_raise(
            ctx,
            make_assertion_violation(
                ctx,
                Symbol::from_str(ctx, "logxor").into(),
                Str::new(ctx, "not an exact integer", true).into(),
                &[a.into_value(ctx)],
            ),
        );
    }

    if !b.is_exact_integer() {
        thunk_raise(
            ctx,
            make_assertion_violation(
                ctx,
                Symbol::from_str(ctx, "logxor").into(),
                Str::new(ctx, "not an exact integer", true).into(),
                &[b.into_value(ctx)],
            ),
        );
    }

    a.logxor(ctx, b).into_value(ctx)
}

pub fn char_to_integer<'gc>(ctx: Context<'gc>, c: Value<'gc>) -> Value<'gc> {
    if !c.is_char() {
        // SAFETY: Return address slot is valid — set up by the native calling convention
        let ret = unsafe { crate::runtime::vm::thunks::helpers::llvm_return_address() };
        backtrace::resolve(ret as *mut _, |symbol| {
            log::trace!("CHAR->INTEGER error {c}");
            log::trace!("{symbol:?}");
        });
        crate::runtime::vm::debug::print_stacktraces_impl(ctx);
        thunk_raise(
            ctx,
            make_assertion_violation(
                ctx,
                Symbol::from_str(ctx, "char->integer").into(),
                Str::new(ctx, "not a char", true).into(),
                &[c],
            ),
        );
    }
    let c = c.char();

    Number::from_u32(ctx, c as u32).into_value(ctx)
}

pub fn integer_to_char<'gc>(ctx: Context<'gc>, n: Value<'gc>) -> Value<'gc> {
    let Some(n) = n.number() else {
        thunk_raise(
            ctx,
            make_assertion_violation(
                ctx,
                Symbol::from_str(ctx, "integer->char").into(),
                Str::new(ctx, "not a number", true).into(),
                &[n],
            ),
        );
    };

    if !n.is_exact_integer() {
        thunk_raise(
            ctx,
            make_assertion_violation(
                ctx,
                Symbol::from_str(ctx, "integer->char").into(),
                Str::new(ctx, "not an exact integer", true).into(),
                &[n.into_value(ctx)],
            ),
        );
    }

    let Some(u) = n.exact_integer_to_u32() else {
        thunk_raise(
            ctx,
            make_assertion_violation(
                ctx,
                Symbol::from_str(ctx, "integer->char").into(),
                Str::new(ctx, "not in char range", true).into(),
                &[n.into_value(ctx)],
            ),
        );
    };

    let Some(c) = std::char::from_u32(u) else {
        thunk_raise(
            ctx,
            make_assertion_violation(
                ctx,
                Symbol::from_str(ctx, "integer->char").into(),
                Str::new(ctx, "not in char range", true).into(),
                &[n.into_value(ctx)],
            ),
        );
    };

    Value::from_char(c)
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
        ctx,
        ContinuationMarks { cmarks: marks },
        crate::runtime::vm::control::continuation_marks_header_word(),
    );

    obj.into()
}

pub fn set_attachments<'gc>(ctx: Context<'gc>, marks: Value<'gc>) -> Value<'gc> {
    if !marks.is::<ContinuationMarks>() {
        thunk_raise(
            ctx,
            make_assertion_violation(
                ctx,
                Symbol::from_str(ctx, "set-attachments").into(),
                Str::new(ctx, "not continuation-marks", true).into(),
                &[marks],
            ),
        );
    }
    // SAFETY: Preconditions verified by the surrounding code
    unsafe {
        ctx.state()
            .set_current_marks(marks.downcast::<ContinuationMarks>().cmarks);
    }

    Value::undefined()
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
