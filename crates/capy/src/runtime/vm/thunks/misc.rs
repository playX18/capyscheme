use super::make_assertion_violation;
use crate::heap::Gc;
use crate::runtime::interp::markers;
use crate::runtime::interp::stack;
use crate::runtime::vm::control;
use crate::runtime::vm::syntax::props_to_sourcev;
use crate::runtime::vm::thunk_raise;
use crate::runtime::{
    MutationContext,
    value::{Str, Symbol, Value},
};
use crate::runtime::{
    value::{IntoValue, Number},
    vm::{control::ContinuationMarks, syntax::Syntax},
};

pub fn string2symbol<'gc>(ctx: MutationContext<'gc>, s: Value<'gc>) -> Value<'gc> {
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

pub fn symbol2string<'gc>(ctx: MutationContext<'gc>, s: Value<'gc>) -> Value<'gc> {
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

pub fn string_ref_value<'gc>(
    ctx: MutationContext<'gc>,
    s: Value<'gc>,
    index: Value<'gc>,
) -> Value<'gc> {
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

pub fn logxor<'gc>(ctx: MutationContext<'gc>, a: Value<'gc>, b: Value<'gc>) -> Value<'gc> {
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

pub fn char_to_integer<'gc>(ctx: MutationContext<'gc>, c: Value<'gc>) -> Value<'gc> {
    if !c.is_char() {
        // SAFETY: Return address slot is valid, set up by the native calling convention
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

pub fn integer_to_char<'gc>(ctx: MutationContext<'gc>, n: Value<'gc>) -> Value<'gc> {
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

pub fn current_continuation_marks<'gc>(ctx: MutationContext<'gc>) -> Value<'gc> {
    let marks = ctx.state().current_marks();

    let obj = Gc::new_with_header_word(
        ctx,
        ContinuationMarks { cmarks: marks },
        crate::runtime::vm::control::continuation_marks_header_word(),
    );

    obj.into()
}

pub fn set_attachments<'gc>(ctx: MutationContext<'gc>, marks: Value<'gc>) -> Value<'gc> {
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

/// `$attachment-push!`: push TABLE, a mark alist from `$update-mark`, onto
/// the current continuation marks.
pub fn attachment_push<'gc>(ctx: MutationContext<'gc>, table: Value<'gc>) -> Value<'gc> {
    let marks = Value::cons(ctx, table, ctx.state().current_marks());
    // SAFETY: the compiler passes a mark alist from `$update-mark`, so the
    // extended list stays a well-formed marks list.
    unsafe {
        ctx.state().set_current_marks(marks);
    }
    Value::undefined()
}

/// `$attachment-pop!`: drop the innermost mark frame. The compiler emits this only on exits from an attachment scope,
/// so the list is non-empty.
pub fn attachment_pop<'gc>(ctx: MutationContext<'gc>) -> Value<'gc> {
    let current = ctx.state().current_marks();
    debug_assert!(current.is_pair(), "pop with no attachment");
    // SAFETY: the scope entry pushed, so popping restores its well-formed list.
    unsafe {
        ctx.state().set_current_marks(current.cdr());
    }
    Value::undefined()
}

/// `$attachment-set!`: replace the innermost mark frame with TABLE. The compiler emits this only where an attachment
/// is known to be in place, so the list is non-empty.
pub fn attachment_set<'gc>(ctx: MutationContext<'gc>, table: Value<'gc>) -> Value<'gc> {
    let current = ctx.state().current_marks();
    debug_assert!(current.is_pair(), "set with no attachment");
    let marks = Value::cons(ctx, table, current.cdr());
    // SAFETY: replacing the head of a well-formed marks list keeps it one.
    unsafe {
        ctx.state().set_current_marks(marks);
    }
    Value::undefined()
}

/// `$attachment-consume!`: pop the innermost mark frame and return it. The
/// compiler emits this only where an attachment is known to be in place.
pub fn attachment_consume<'gc>(ctx: MutationContext<'gc>) -> Value<'gc> {
    let current = ctx.state().current_marks();
    debug_assert!(current.is_pair(), "consume with no attachment");
    // SAFETY: the popped head stays alive as the return value.
    unsafe {
        ctx.state().set_current_marks(current.cdr());
    }
    current.car()
}

/// `$attachment-consume-reify!`: the tail-position consume. When the running
/// frame is reified and holds its own marks, pop and return them. When it is
/// reified without marks, return DEFAULT. Otherwise reify it one-shot and
/// return DEFAULT.
pub fn attachment_consume_reify<'gc>(ctx: MutationContext<'gc>, default: Value<'gc>) -> Value<'gc> {
    // Every engine parks the running frame before a primop (`prim_step`),
    // so this is the live top frame with its return address written.
    let fp = ctx.tc().stack.fp.get();
    // SAFETY: `fp` is the running frame, its caller stored the return
    // address in slot 0.
    let ref_ret = unsafe { stack::return_address(fp) };
    if ref_ret == markers::underflow() {
        // Maybe reified.
        let link = ctx.tc().stack.stack_link.get();
        assert!(
            link.is::<stack::Continuation>(),
            "attachment consume through the bottom of a stack with no continuation below"
        );
        let recorded = link.downcast::<stack::Continuation>().attachments.get();
        let current = ctx.state().current_marks();
        if recorded == current {
            // Reified, no own attachment.
            return default;
        }
        if recorded == Value::new(false) {
            // Split by overflow: fall through and reify.
        } else if current.is_pair() {
            // Reified with own attachment: consume it.
            // SAFETY: the popped head stays alive as the return value.
            unsafe {
                ctx.state().set_current_marks(current.cdr());
            }
            return current.car();
        } else {
            // An empty current list only happens transiently under wind
            // thunks, then there is nothing of ours to consume.
            return default;
        }
    }
    // Not reified: reify one-shot, then the default stands.
    // SAFETY: same frame as above.
    let _ = unsafe { stack::reify_cc(ctx, fp, true) };
    default
}

/// `$update-mark`: the mark table MARKS with KEY set to VAL, as a fresh list.
pub fn update_mark<'gc>(
    ctx: MutationContext<'gc>,
    marks: Value<'gc>,
    key: Value<'gc>,
    val: Value<'gc>,
) -> Value<'gc> {
    let pair = Value::cons(ctx, key, val);
    control::replace_or_add_mark(ctx, marks, key, pair)
}

pub fn make_syntax<'gc>(
    ctx: MutationContext<'gc>,
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
