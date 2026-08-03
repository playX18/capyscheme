//! Control utilities
//!
//!
//! Continuation marks implementation based on [A CPS-like Transformation of Continuation Marks](https://jeapostrophe.github.io/home/static/students/2012-ms-kgermane.pdf).

use crate::heap::{
    Gc, Trace,
    object::{ClassId, builtin_class_ids, class_header_word},
};
use crate::prelude::*;
use crate::prelude::{ClosureRef, Value};
use crate::runtime::sni::{self, LocalFrame};
use crate::runtime::vm::ExecutionResult;
use mmtk::util::Address;

#[repr(C)]
#[derive(Trace)]
pub struct ContinuationMarks<'gc> {
    pub cmarks: Value<'gc>,
}

pub(crate) fn continuation_marks_header_word() -> u64 {
    class_header_word(
        ClassId::new(builtin_class_ids::CONTINUATION_MARKS).expect("builtin class id is nonzero"),
    )
}

fn is_c_star_continuation<'gc>(retk: ClosureRef<'gc>) -> bool {
    if retk.nfree != 1 {
        return false;
    }

    let proc = retk[0].get();
    if let Some(proc) = proc.try_as::<NativeProc>() {
        proc.proc == Address::from_ptr(_raw_scm_cont_c_star as *const ())
    } else {
        false
    }
}

/// Free-var layout for `.reified-cc` / `.null-reified-cc`:
/// `[0]=NativeProc`, `[1]=retk`, `[2]=winders`, `[3]=raw marks list`.
const REIFIED_CC_NFREE: usize = 4;

/// True if `value` is a reified `call/cc` continuation (code identity).
pub fn is_reified_continuation_value(value: Value<'_>) -> bool {
    value
        .try_as::<Closure>()
        .is_some_and(|clos| is_reified_continuation_closure(&*clos))
}

pub fn is_reified_continuation_closure(clos: &Closure<'_>) -> bool {
    if clos.nfree != REIFIED_CC_NFREE {
        return false;
    }
    native_proc_addr_ref(clos).is_some_and(is_reified_cc_code)
}

fn native_proc_addr_ref(clos: &Closure<'_>) -> Option<Address> {
    clos[0].get().try_as::<NativeProc>().map(|p| p.proc)
}

fn is_reified_cc_code(addr: Address) -> bool {
    addr == Address::from_ptr(_raw_scm_proc_reified_cc as *const ())
        || addr == Address::from_ptr(_raw_scm_proc_null_reified_cc as *const ())
}

pub fn box_continuation_marks<'gc>(ctx: Context<'gc>, cmarks: Value<'gc>) -> Value<'gc> {
    Gc::new_with_header_word(
        ctx,
        ContinuationMarks { cmarks },
        continuation_marks_header_word(),
    )
    .into()
}

fn list_tail<'gc>(mut ls: Value<'gc>, n: usize) -> Value<'gc> {
    for _ in 0..n {
        ls = ls.cdr();
    }
    ls
}

fn common_winder_tail<'gc>(x: Value<'gc>, y: Value<'gc>) -> Value<'gc> {
    let nx = x.list_length();
    let ny = y.list_length();
    let mut x = if nx > ny { list_tail(x, nx - ny) } else { x };
    let mut y = if ny > nx { list_tail(y, ny - nx) } else { y };
    while x != y {
        x = x.cdr();
        y = y.cdr();
    }
    x
}

/// Run dynamic-wind outs/ins from the current winders list to `new_winders`.
///
/// Fast path: `eq?` identity. Caller must use `#[scheme(unsafe)]` and root
/// live values across this nest.
fn do_wind<'gc>(ctx: Context<'gc>, new_winders: Value<'gc>) -> Result<(), Value<'gc>> {
    let old = ctx.winders();
    if old == new_winders {
        return Ok(());
    }

    let mut frame = LocalFrame::push();
    let new_slot = frame.new_local_ref(new_winders).as_ptr();
    let old_slot = frame.new_local_ref(old).as_ptr();
    // SAFETY: slots live until `frame` drops.
    let tail = unsafe {
        common_winder_tail(
            std::mem::transmute::<Value<'static>, Value<'gc>>(*new_slot),
            std::mem::transmute::<Value<'static>, Value<'gc>>(*old_slot),
        )
    };
    let tail_slot = frame.new_local_ref(tail).as_ptr();

    // SAFETY: slots live until `frame` drops.
    let mut rec = unsafe { std::mem::transmute::<Value<'static>, Value<'gc>>(*old_slot) };
    let tail = unsafe { std::mem::transmute::<Value<'static>, Value<'gc>>(*tail_slot) };
    while rec != tail {
        let out = rec.cdar();
        let next = rec.cdr();
        ctx.set_winders(next);
        let out_slot = frame.new_local_ref(out).as_ptr();
        // SAFETY: slot lives until `frame` drops.
        let out = unsafe { std::mem::transmute::<Value<'static>, Value<'gc>>(*out_slot) };
        match sni::call_function(ctx, out, []) {
            ExecutionResult::Ok(_) => {}
            ExecutionResult::Err(err) => return Err(err),
        }
        rec = next;
    }

    // Wind in: recurse to the end, then call `in` on the way back.
    fn wind_in<'gc>(
        ctx: Context<'gc>,
        rec_slot: *mut Value<'static>,
        tail_slot: *mut Value<'static>,
        frame: &mut LocalFrame<'_>,
    ) -> Result<(), Value<'gc>> {
        // SAFETY: slots live until `frame` drops.
        let rec = unsafe { std::mem::transmute::<Value<'static>, Value<'gc>>(*rec_slot) };
        let tail = unsafe { std::mem::transmute::<Value<'static>, Value<'gc>>(*tail_slot) };
        if rec == tail {
            return Ok(());
        }
        let next = rec.cdr();
        let next_slot = frame.new_local_ref(next).as_ptr();
        wind_in(ctx, next_slot, tail_slot, frame)?;
        // SAFETY: reload after nest — slots still live.
        let rec = unsafe { std::mem::transmute::<Value<'static>, Value<'gc>>(*rec_slot) };
        let in_thunk = rec.caar();
        let in_slot = frame.new_local_ref(in_thunk).as_ptr();
        let in_thunk = unsafe { std::mem::transmute::<Value<'static>, Value<'gc>>(*in_slot) };
        match sni::call_function(ctx, in_thunk, []) {
            ExecutionResult::Ok(_) => {}
            ExecutionResult::Err(err) => return Err(err),
        }
        let rec = unsafe { std::mem::transmute::<Value<'static>, Value<'gc>>(*rec_slot) };
        ctx.set_winders(rec);
        Ok(())
    }

    wind_in(ctx, new_slot, tail_slot, &mut frame)
}

// SAFETY: `gc` for `ContinuationMarks` upholds all trait invariants
unsafe impl<'gc> ClassTagged for ContinuationMarks<'gc> {
    const CLASS_IDS: &'static [u32] = &[builtin_class_ids::CONTINUATION_MARKS];

    const TYPE_NAME: &'static str = "continuation-marks";
}

/// A `C*` continuation that restores continuation marks from the captured frame.
#[scheme(continuation)]
pub(crate) fn c_star(results: &'gc [Value<'gc>]) -> Value<'gc> {
    let cm = ctx.state().current_marks();
    let marks = cm.car();
    // SAFETY: `retk` is a valid continuation frame on the stack
    unsafe {
        ctx.state().set_current_marks(cm.cdr());
        let results = results.to_vec();
        return nctx.continue_to(marks.cdr(), &results);
    };
}

/// User-facing reified continuation from `%call/cc`.
///
/// Restores marks and winders, then continues to the captured `retk`.
/// Free vars: `[retk, winders, raw-marks]` (plus NativeProc at `[0]`).
#[scheme(name = " .reified-cc ", unsafe)]
pub(crate) fn reified_cc(ans: &'gc [Value<'gc>]) -> &'gc [Value<'gc>] {
    let clos = nctx.rator().downcast::<Closure>();
    debug_assert_eq!(clos.nfree, REIFIED_CC_NFREE);
    let retk = clos[1].get();
    let winders = clos[2].get();
    let marks = clos[3].get();

    // SAFETY: marks is the raw list snapshotted at capture.
    unsafe {
        ctx.state().set_current_marks(marks);
    }

    if winders == ctx.winders() {
        // SAFETY: retk is the captured return continuation.
        return unsafe { nctx.continue_to(retk, ans) };
    }

    let mut frame = LocalFrame::push();
    let retk_slot = frame.new_local_ref(retk).as_ptr();
    let winders_slot = frame.new_local_ref(winders).as_ptr();
    let mut ans_slots: Vec<*mut Value<'static>> = Vec::with_capacity(ans.len());
    for v in ans {
        ans_slots.push(frame.new_local_ref(*v).as_ptr());
    }

    // SAFETY: slots live until `frame` drops.
    let winders = unsafe { std::mem::transmute::<Value<'static>, Value<'gc>>(*winders_slot) };
    if let Err(err) = do_wind(ctx, winders) {
        return nctx.return_error(err);
    }

    // SAFETY: slots live until `frame` drops; Values reloaded after nest.
    let retk = unsafe { std::mem::transmute::<Value<'static>, Value<'gc>>(*retk_slot) };
    let args: Vec<Value<'gc>> = unsafe {
        ans_slots
            .iter()
            .map(|slot| std::mem::transmute::<Value<'static>, Value<'gc>>(**slot))
            .collect()
    };
    // SAFETY: retk is the captured return continuation.
    unsafe { nctx.continue_to(retk, &args) }
}

/// Null continuation: errors if invoked; still a `continuation?`.
#[scheme(name = " .null-reified-cc ")]
pub(crate) fn null_reified_cc(_ans: &'gc [Value<'gc>]) -> () {
    crate::runtime::vm::thunk_raise(
        nctx.ctx,
        crate::runtime::vm::exceptions::make_assertion_violation(
            nctx.ctx,
            Some("null-continuation"),
            "attempted to invoke the null continuation",
            &[],
        ),
    );
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

    let pair = Value::cons(ctx, key, value);

    // retk == C*: we're in tail position of another wcm
    if is_c_star_continuation(retk) {
        let first_marks = old_marks.car();
        let new_marks = replace_or_add_mark(ctx, first_marks.car(), key, pair);

        first_marks.set_car(ctx, new_marks);
        retk.into()
    } else {
        let cont_closure = make_static_closure_c_star(ctx);
        let new_marks = list!(ctx, pair);

        let cframe = Value::cons(ctx, Value::cons(ctx, new_marks, retk.into()), old_marks);
        // SAFETY: Preconditions verified by the surrounding code
        unsafe {
            ctx.state().set_current_marks(cframe);
        }

        cont_closure.into()
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

    /// Capture the current continuation and call `proc` with the reified k.
    ///
    /// Free vars on k: `[retk, winders, raw-marks]`.
    #[scheme(name = "%call/cc")]
    pub fn call_cc(proc: Value<'gc>) -> Result<Value<'gc>, Value<'gc>> {
        let retk = nctx.retk;
        let winders = ctx.winders();
        let marks = ctx.state().current_marks();
        let k = make_closure_reified_cc(ctx, [retk, winders, marks]);
        nctx.return_call(proc, &[k.into()])
    }

    #[scheme(name = "%null-continuation")]
    pub fn null_continuation() -> Value<'gc> {
        let k =
            make_closure_null_reified_cc(ctx, [Value::new(false), Value::null(), Value::null()]);
        nctx.return_(k.into())
    }

    #[scheme(name = "%continuation?")]
    pub fn is_continuation_pred(val: Value<'gc>) -> bool {
        nctx.return_(is_reified_continuation_value(val))
    }

    /// Box the raw marks list stored on a reified continuation.
    #[scheme(name = "%continuation-next-marks")]
    pub fn continuation_next_marks_prim(k: Value<'gc>) -> Value<'gc> {
        if !is_reified_continuation_value(k) {
            return nctx.wrong_argument_violation(
                "%continuation-next-marks",
                "expected a continuation",
                Some(k),
                Some(1),
                1,
                &[k],
            );
        }
        let marks = k.downcast::<Closure>()[3].get();
        nctx.return_(box_continuation_marks(ctx, marks))
    }

    /// Install raw marks from a reified continuation into the current state.
    #[scheme(name = "%set-continuation-marks!")]
    pub fn set_continuation_marks(k: Value<'gc>) -> Value<'gc> {
        if !is_reified_continuation_value(k) {
            return nctx.wrong_argument_violation(
                "%set-continuation-marks!",
                "expected a continuation",
                Some(k),
                Some(1),
                1,
                &[k],
            );
        }
        let marks = k.downcast::<Closure>()[3].get();
        // SAFETY: marks is a raw marks list from a reified continuation.
        unsafe {
            ctx.state().set_current_marks(marks);
        }
        nctx.return_(Value::undefined())
    }
}

pub fn init_control<'gc>(ctx: Context<'gc>) {
    control_ops::register(ctx);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::Scheme;

    #[test]
    fn continuation_marks_allocate_with_class_only_headers() {
        Scheme::new_uninit().enter(|ctx| {
            let marks = ctx.current_continuation_marks();
            assert_eq!(
                marks.as_gc_object().header().class_id(),
                ClassId::new(builtin_class_ids::CONTINUATION_MARKS).unwrap()
            );
        });
    }
}
