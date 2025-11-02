use rsgc::cell::Lock;
use rsgc::{Gc, Trace};

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
    pub cmarks: Option<CFrameRef<'gc>>,
}

unsafe impl<'gc> Tagged for ContinuationMarks<'gc> {
    const TC8: TypeCode8 = TypeCode8::CMARKS;

    const TYPE_NAME: &'static str = "continuation-marks";
}

pub type CFrameRef<'gc> = Gc<'gc, CFrame<'gc>>;

#[derive(Trace, Copy, Clone)]
pub enum CFrameKind<'gc> {
    /// Boundary frame: Created at `with-continuation-marks` call
    /// and is used to restore old continuation marks on exit.
    Boundary(ClosureRef<'gc>, Option<CFrameRef<'gc>>),

    /// Normal frame: key-value pair for continuation marks.
    Set(Value<'gc>),
}

use crate::prelude::*;

/// A `C*` continuation that restores continuation marks from the captured frame.
#[scheme(continuation)]
pub(crate) fn c_star(results: &'gc [Value<'gc>]) -> Value<'gc> {
    let cm = ctx.state().current_marks();
    let marks = cm.expect("C* continuation must have captured continuation marks");
    unsafe {
        ctx.state().set_current_marks(marks.parent);
        let results = results.to_vec();
        return nctx.continue_to(marks.retk.into(), &results);
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
    ctx: &Context<'gc>,
    key: Value<'gc>,
    value: Value<'gc>,
    retk: ClosureRef<'gc>,
) -> Value<'gc> {
    let old_marks = ctx.state().current_marks();
    let cont_closure = make_static_closure_c_star(*ctx);

    let pair = Value::cons(*ctx, key, value);

    // retk == C*: we're in tail position of another wcm
    if Gc::ptr_eq(retk, cont_closure) {
        let first_marks = old_marks.unwrap();
        let new_marks = replace_or_add_mark(*ctx, first_marks.mark_set.get(), key, pair);
        let wmarks = Gc::write(&ctx, first_marks);
        barrier::field!(wmarks, CFrame, mark_set)
            .unlock()
            .set(new_marks);

        return cont_closure.into();
    } else {
        let new_marks = list!(*ctx, pair);
        let cframe = Gc::new(
            ctx,
            CFrame {
                parent: old_marks,
                retk,
                mark_set: Lock::new(new_marks),
            },
        );
        unsafe {
            ctx.state().set_current_marks(Some(cframe));
        }

        return cont_closure.into();
    }
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
    while let Some(cmarks) = set {
        let mark_set = cmarks.mark_set.get();

        if let Some(val) = mark_set.assq(key) {
            result.push(val.cdr());
        }
        set = cmarks.parent;
    }

    nctx.return_(
        result
            .into_iter()
            .rfold(Value::null(), |acc, x| Value::cons(ctx, x, acc)),
    )
}

pub fn init_control<'gc>(ctx: Context<'gc>) {
    register_is_continuation_marks(ctx);
    register_continuation_mark_set_to_list(ctx);
}
