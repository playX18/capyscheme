use crate::rsgc::cell::Lock;
use crate::rsgc::{Gc, Trace};

use crate::prelude::{ClosureRef, Value};

/// Continuation Frame. Used to implement continuation marks.
#[derive(Trace)]
pub struct CFrame<'gc> {
    pub parent: Option<CFrameRef<'gc>>,
    pub retk: ClosureRef<'gc>,
    pub mark_set: Lock<Value<'gc>>,
}

#[repr(C)]
#[derive(Trace)]
pub struct ContinuationMarks<'gc> {
    pub header: ScmHeader,
    pub cmarks: Value<'gc>,
}

unsafe impl<'gc> Tagged for ContinuationMarks<'gc> {
    const TC8: TypeCode8 = TypeCode8::CMARKS;

    const TYPE_NAME: &'static str = "continuation-marks";
}

pub type CFrameRef<'gc> = Gc<'gc, CFrame<'gc>>;

use crate::prelude::*;

/// A `C*` continuation that restores continuation marks from the captured frame.
#[scheme(continuation)]
pub(crate) fn c_star(results: &'gc [Value<'gc>]) -> Value<'gc> {
    let cm = ctx.state().current_marks();
    let marks = cm.car();
    unsafe {
        ctx.state().set_current_marks(cm.cdr());
        let results = results.to_vec();
        return nctx.continue_to(marks.cdr(), &results);
    };
}

fn replace_or_add_mark<'gc>(
    ctx: Context<'gc>,
    first_marks: Value<'gc>,
    key: Value<'gc>,
    pair: Value<'gc>,
) -> Value<'gc> {
    if first_marks.is_null() {
        crate::list!(ctx, pair)
    } else if key == first_marks.caar() {
        Value::cons(ctx, pair, first_marks.cdr())
    } else {
        let rest = replace_or_add_mark(ctx, first_marks.cdr(), key, pair);
        Value::cons(ctx, first_marks.car(), rest)
    }
}

/// Allocate a new boundary continuation frame.
///
/// Returns C* continuation that restores the old marks.
pub(crate) fn push_cframe<'gc>(
    ctx: Context<'gc>,
    key: Value<'gc>,
    value: Value<'gc>,
    retk: ClosureRef<'gc>,
) -> Value<'gc> {
    let old_marks = ctx.state().current_marks();
    let cont_closure = make_static_closure_c_star(ctx);

    let pair = Value::cons(ctx, key, value);

    // retk == C*: we're in tail position of another wcm
    if Gc::ptr_eq(retk, cont_closure) {
        let first_marks = old_marks.car();
        let new_marks = replace_or_add_mark(ctx, first_marks.car(), key, pair);

        first_marks.set_car(ctx, new_marks);
        return cont_closure.into();
    } else {
        let new_marks = list!(ctx, pair);

        let cframe = Value::cons(ctx, Value::cons(ctx, new_marks, retk.into()), old_marks);
        unsafe {
            ctx.state().set_current_marks(cframe);
        }

        return cont_closure.into();
    }
}

#[scheme(path = capy)]
pub mod control_ops {

    #[scheme(name = "call-with-continuation-mark")]
    pub fn call_with_continuation_mark(
        key: Value<'gc>,
        value: Value<'gc>,
        thunk: Gc<'gc, Closure<'gc>>,
    ) -> Value<'gc> {
        let retk = nctx.retk;
        let cont = push_cframe(ctx, key, value, retk.downcast());
        nctx.retk = cont;
        nctx.return_call(thunk.into(), &[])
    }

    #[scheme(name = "continuation-marks?")]
    pub fn is_continuation_marks(val: Value<'gc>) -> bool {
        nctx.return_(val.is::<ContinuationMarks<'gc>>())
    }

    #[scheme(name = "continuation-mark-set->list")]
    pub fn continuation_mark_set_to_list(
        set: Gc<'gc, ContinuationMarks<'gc>>,
        key: Value<'gc>,
    ) -> Value<'gc> {
        let mut set = set.cmarks;

        let mut result = Vec::new();
        /*while let Some(cmarks) = set {
            let mark_set = cmarks.mark_set.get();

            if let Some(val) = mark_set.assq(key) {
                result.push(val.cdr());
            }
            set = cmarks.parent;
        }*/

        while !set.is_null() {
            let mark_set = set.caar();
            if let Some(val) = mark_set.assq(key) {
                result.push(val.cdr());
            }

            set = set.cdr();
        }

        nctx.return_(
            result
                .into_iter()
                .rfold(Value::null(), |acc, x| Value::cons(ctx, x, acc)),
        )
    }

    #[scheme(name = "continuation-mark-set-first")]
    pub fn contination_mark_set_first(
        set: Gc<'gc, ContinuationMarks<'gc>>,
        key: Value<'gc>,
        default_value: Option<Value<'gc>>,
    ) -> Value<'gc> {
        let default_value = default_value.unwrap_or(Value::new(false));
        let mut set = set.cmarks;

        while !set.is_null() {
            let mark_set = set.caar();
            if let Some(val) = mark_set.assq(key) {
                return nctx.return_(val.cdr());
            }

            set = set.cdr();
        }

        nctx.return_(default_value)
    }

    #[scheme(name = "continuation-mark-set->list*")]
    pub fn continuation_mark_set_list_many(
        set: Gc<'gc, ContinuationMarks<'gc>>,
        keys: Value<'gc>,
        default_value: Option<Value<'gc>>,
    ) -> Value<'gc> {
        let default_value = default_value.unwrap_or(Value::new(false));

        fn rec<'gc>(
            mark_set: Value<'gc>,
            keys: Value<'gc>,
            default_value: Value<'gc>,
            ctx: Context<'gc>,
        ) -> Value<'gc> {
            /*match mark_set {
                None => Value::null(),
                Some(cmarks) if !keys.list_any(|key| cmarks.mark_set.get().assq(key).is_some()) => {
                    rec(cmarks.parent, keys, default_value, ctx)
                }

                Some(cmarks) => Value::cons(
                    ctx,
                    keys.map(ctx, |key| match cmarks.mark_set.get().assq(key) {
                        Some(val) => val.cdr(),
                        None => default_value,
                    })
                    .list_to_vector(ctx)
                    .into(),
                    rec(cmarks.parent, keys, default_value, ctx),
                ),
            }*/
            if mark_set.is_null() {
                Value::null()
            } else if !keys.list_any(|key| mark_set.caar().assq(key).is_some()) {
                rec(mark_set.cdr(), keys, default_value, ctx)
            } else {
                Value::cons(
                    ctx,
                    keys.map(ctx, |key| match mark_set.caar().assq(key) {
                        Some(val) => val.cdr(),
                        None => default_value,
                    })
                    .list_to_vector(ctx)
                    .into(),
                    rec(mark_set.cdr(), keys, default_value, ctx),
                )
            }
        }

        nctx.return_(rec(set.cmarks, keys, default_value, ctx))
    }

    /// Return a raw list of continuation frames.
    ///
    /// Note that changes to the continuation marks will be reflected in the frames.
    #[scheme(name = "$continuation-marks-markss")]
    pub fn continuation_marks_markss(marks: Gc<'gc, ContinuationMarks<'gc>>) -> Value<'gc> {
        nctx.return_(marks.cmarks)
    }
}

pub fn init_control<'gc>(ctx: Context<'gc>) {
    control_ops::register(ctx);
}
