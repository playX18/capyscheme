//! Control utilities
//!
//! Continuation marks follow the continuation attachments design of Chez
//! Scheme. See [`crate::runtime::interp::stack`] for how they relate to the
//! stack. The current marks are a list of mark frames, innermost first. Each
//! mark frame is a bare association list of key to value.

use crate::heap::{
    Gc, Trace,
    object::{ClassId, builtin_class_ids, class_header_word},
};
use crate::prelude::*;
use crate::prelude::{ClosureRef, Value};
use crate::runtime::interp::stack;
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

/// Free-var layout for `.reified-cc` / `.reified-1cc` / `.null-reified-cc`:
/// `[0]=NativeProc`, `[1]=captured stack (a `Continuation`)`, `[2]=winders`,
/// `[3]=raw marks list`.
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
        || addr == Address::from_ptr(_raw_scm_proc_reified_1cc as *const ())
        || addr == Address::from_ptr(_raw_scm_proc_null_reified_cc as *const ())
}

pub fn box_continuation_marks<'gc>(ctx: MutationContext<'gc>, cmarks: Value<'gc>) -> Value<'gc> {
    ctx.allocate_with(
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
/// A winder is `(in out marks)`, where `marks` is the raw
/// continuation-marks list captured when `dynamic-wind` was entered. Each
/// thunk runs with its own winder's marks installed. The caller restores the
/// target marks afterwards.
///
/// Fast path: `eq?` identity. Caller must use `#[scheme(unsafe)]` and root
/// live values across this nest.
fn do_wind<'gc>(ctx: MutationContext<'gc>, new_winders: Value<'gc>) -> Result<(), Value<'gc>> {
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
    loop {
        // SAFETY: slots live until `frame` drops, both are reloaded every
        // iteration because the thunk below may collect.
        let tail = unsafe { std::mem::transmute::<Value<'static>, Value<'gc>>(*tail_slot) };
        if rec == tail {
            break;
        }
        let winder = rec.car();
        let out = winder.cadr();
        let winder_marks = winder.caddr();
        let next = rec.cdr();
        let next_slot = frame.new_local_ref(next).as_ptr();
        ctx.set_winders(next);
        // SAFETY: `winder_marks` is the raw marks list stored by
        // `dynamic-wind`, the winder cell stays alive via the rooted
        // old/new lists, and the marks move into traced state.
        unsafe {
            ctx.state().set_current_marks(winder_marks);
        }
        let out_slot = frame.new_local_ref(out).as_ptr();
        // SAFETY: slot lives until `frame` drops.
        let out = unsafe { std::mem::transmute::<Value<'static>, Value<'gc>>(*out_slot) };
        match sni::call_function(ctx, out, []) {
            ExecutionResult::Ok(_) => {}
            ExecutionResult::Err(err) => return Err(err),
        }
        // SAFETY: slot lives until `frame` drops.
        rec = unsafe { std::mem::transmute::<Value<'static>, Value<'gc>>(*next_slot) };
    }

    // Wind in: recurse to the end, then call `in` on the way back.
    fn wind_in<'gc>(
        ctx: MutationContext<'gc>,
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
        // SAFETY: reload after nest, slots still live.
        let rec = unsafe { std::mem::transmute::<Value<'static>, Value<'gc>>(*rec_slot) };
        let winder = rec.car();
        let in_thunk = winder.car();
        let winder_marks = winder.caddr();
        let in_slot = frame.new_local_ref(in_thunk).as_ptr();
        let in_thunk = unsafe { std::mem::transmute::<Value<'static>, Value<'gc>>(*in_slot) };
        // SAFETY: `winder_marks` is the raw marks list stored by
        // `dynamic-wind`, the winder cell stays alive via the rooted
        // old/new lists, and the marks move into traced state.
        unsafe {
            ctx.state().set_current_marks(winder_marks);
        }
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

/// Shared invocation path for reified continuations.
///
/// Winds to the captured winders first, each in/out thunk runs with its
/// own winder's marks, then installs the captured marks and continues to
/// the captured stack with `ans`.
fn invoke_reified<'gc>(
    call: &Call<'_, 'gc>,
    retk: Value<'gc>,
    winders: Value<'gc>,
    marks: Value<'gc>,
    ans: &[Value<'gc>],
) -> Value<'gc> {
    let ctx = call.ctx;
    if winders == ctx.winders() {
        // SAFETY: marks is the raw list snapshotted at capture.
        unsafe {
            ctx.state().set_current_marks(marks);
        }
        // SAFETY: retk is the captured return continuation.
        return unsafe { call.continue_to(retk, ans) };
    }

    let mut frame = LocalFrame::push();
    let retk_slot = frame.new_local_ref(retk).as_ptr();
    let winders_slot = frame.new_local_ref(winders).as_ptr();
    let marks_slot = frame.new_local_ref(marks).as_ptr();
    let mut ans_slots: Vec<*mut Value<'static>> = Vec::with_capacity(ans.len());
    for v in ans {
        ans_slots.push(frame.new_local_ref(*v).as_ptr());
    }

    // SAFETY: slots live until `frame` drops.
    let winders = unsafe { std::mem::transmute::<Value<'static>, Value<'gc>>(*winders_slot) };
    if let Err(err) = do_wind(ctx, winders) {
        return call.err(err);
    }

    // SAFETY: slots live until `frame` drops. Values reloaded after nest.
    let retk = unsafe { std::mem::transmute::<Value<'static>, Value<'gc>>(*retk_slot) };
    let marks = unsafe { std::mem::transmute::<Value<'static>, Value<'gc>>(*marks_slot) };
    // The wind above ran thunks under winder marks, install the target's.
    // SAFETY: marks is the raw list snapshotted at capture.
    unsafe {
        ctx.state().set_current_marks(marks);
    }
    let args: Vec<Value<'gc>> = unsafe {
        ans_slots
            .iter()
            .map(|slot| std::mem::transmute::<Value<'static>, Value<'gc>>(**slot))
            .collect()
    };
    // SAFETY: retk is the captured return continuation.
    unsafe { call.continue_to(retk, &args) }
}

/// User-facing reified continuation from `%call/cc`.
///
/// Winds to the captured winders, restores the captured marks, then
/// reinstates the captured stack.
/// Free vars: `[stack, winders, raw-marks]` (plus NativeProc at `[0]`).
#[scheme(name = " .reified-cc ", unsafe)]
pub(crate) fn reified_cc(ans: &'gc [Value<'gc>]) -> Value<'gc> {
    let clos = call.rator.downcast::<Closure>();
    debug_assert_eq!(clos.nfree, REIFIED_CC_NFREE);
    let retk = clos[1].get();
    let winders = clos[2].get();
    let marks = clos[3].get();
    invoke_reified(call, retk, winders, marks, ans)
}

/// User-facing one-shot continuation from `%call/1cc`.
///
/// Free vars: `[continuation, winders, raw-marks]` (plus NativeProc at `[0]`).
/// Resuming the continuation in place marks its `Continuation` shot. Invoking a
/// shot one raises.
#[scheme(name = " .reified-1cc ", unsafe)]
pub(crate) fn reified_1cc(ans: &'gc [Value<'gc>]) -> Value<'gc> {
    let clos = call.rator.downcast::<Closure>();
    debug_assert_eq!(clos.nfree, REIFIED_CC_NFREE);
    let retk = clos[1].get();
    if stack::is_shot(retk) {
        crate::runtime::vm::thunk_raise(
            ctx,
            crate::runtime::vm::exceptions::make_assertion_violation(
                ctx,
                Some("call/1cc"),
                "attempt to invoke shot one-shot continuation",
                &[],
            ),
        );
    }
    let winders = clos[2].get();
    let marks = clos[3].get();
    invoke_reified(call, retk, winders, marks, ans)
}

/// Null continuation: errors if invoked, still a `continuation?`.
#[scheme(name = " .null-reified-cc ")]
pub(crate) fn null_reified_cc(_ans: &'gc [Value<'gc>]) -> () {
    crate::runtime::vm::thunk_raise(
        ctx,
        crate::runtime::vm::exceptions::make_assertion_violation(
            ctx,
            Some("null-continuation"),
            "attempted to invoke the null continuation",
            &[],
        ),
    );
}

/// Builds a `.reified-cc`-family closure through the registry's site cache.
///
/// Every capture for one of these primitives shares the function address
/// and the metadata global, only the three free-var values
/// (`[stack, winders, raw-marks]`) differ, so a cached capture allocates
/// just the closure cell.
fn make_reified_closure<'gc>(
    ctx: MutationContext<'gc>,
    proc: NativeFn<'gc>,
    meta: Value<'gc>,
    captured: [Value<'gc>; 3],
) -> ClosureRef<'gc> {
    ctx.make_cached_native_closure(proc, captured, meta)
}

pub(crate) fn replace_or_add_mark<'gc>(
    ctx: MutationContext<'gc>,
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

/// Set the mark `key` to `value` on the continuation of the native
/// procedure running in the top frame, which is about to tail-call the body
/// of a `with-continuation-mark`.
///
/// The frame is first split off so that it is at the base of the segment,
/// it is already there when it was called in tail position of another
/// marked body, and then has marks of its own exactly when the current marks
/// differ from those of the continuation object below. Those are replaced or
/// extended, otherwise a new mark frame is pushed, which the return through
/// `underflow` pops.
pub(crate) fn set_mark<'gc>(ctx: MutationContext<'gc>, key: Value<'gc>, value: Value<'gc>) {
    // SAFETY: the running native procedure owns the top frame.
    let below = unsafe { stack::reify_cc(ctx, ctx.tc().stack.fp.get(), true) };
    let below_marks = below
        .try_as::<stack::Continuation>()
        .map_or(Value::null(), |continuation| continuation.attachments.get());
    let current = ctx.state().current_marks();
    let pair = Value::cons(ctx, key, value);
    let marks = if current != below_marks {
        let alist = replace_or_add_mark(ctx, current.car(), key, pair);
        Value::cons(ctx, alist, current.cdr())
    } else {
        let alist = crate::list!(ctx, pair);
        Value::cons(ctx, alist, current)
    };
    // SAFETY: the frame at the base now owns the new mark frame.
    unsafe {
        ctx.state().set_current_marks(marks);
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
        set_mark(ctx, key, value);
        call.tail(thunk.into(), &[])
    }

    #[scheme(name = "continuation-marks?")]
    pub fn is_continuation_marks(val: Value<'gc>) -> bool {
        val.is::<ContinuationMarks<'gc>>()
    }

    #[scheme(name = "continuation-mark-set->list")]
    pub fn continuation_mark_set_to_list(
        set: Gc<'gc, ContinuationMarks<'gc>>,
        key: Value<'gc>,
    ) -> Value<'gc> {
        let mut set = set.cmarks;

        let mut result = Vec::new();

        while !set.is_null() {
            let mark_set = set.car();
            if let Some(val) = mark_set.assq(key) {
                result.push(val.cdr());
            }

            set = set.cdr();
        }

        result
            .into_iter()
            .rfold(Value::null(), |acc, x| Value::cons(ctx, x, acc))
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
            let mark_set = set.car();
            if let Some(val) = mark_set.assq(key) {
                return val.cdr();
            }

            set = set.cdr();
        }

        default_value
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
            ctx: MutationContext<'gc>,
        ) -> Value<'gc> {
            if mark_set.is_null() {
                Value::null()
            } else if !keys.list_any(|key| mark_set.car().assq(key).is_some()) {
                rec(mark_set.cdr(), keys, default_value, ctx)
            } else {
                // `map` builds its list back to front, restore key order.
                let vector = keys
                    .map(ctx, |key| match mark_set.car().assq(key) {
                        Some(val) => val.cdr(),
                        None => default_value,
                    })
                    .list_reverse(ctx)
                    .list_to_vector(ctx);
                Value::cons(
                    ctx,
                    vector.into(),
                    rec(mark_set.cdr(), keys, default_value, ctx),
                )
            }
        }

        rec(set.cmarks, keys, default_value, ctx)
    }

    /// Return a raw list of continuation frames.
    ///
    /// Note that changes to the continuation marks will be reflected in the frames.
    #[scheme(name = "$continuation-marks-markss")]
    pub fn continuation_marks_markss(marks: Gc<'gc, ContinuationMarks<'gc>>) -> Value<'gc> {
        marks.cmarks
    }

    /// Capture the current continuation and call `proc` with the reified k.
    ///
    /// The frames below this call become a continuation object without
    /// being copied. Free vars on k: `[stack, winders, raw-marks]`.
    #[scheme(name = "%call/cc")]
    pub fn call_cc(proc: Value<'gc>) -> Result<Value<'gc>, Value<'gc>> {
        // SAFETY: this native procedure owns the top frame.
        let captured = unsafe { stack::reify_cc(ctx, ctx.tc().stack.fp.get(), false) };
        let winders = ctx.winders();
        let marks = ctx.state().current_marks();
        let k = make_reified_closure(
            ctx,
            _raw_scm_proc_reified_cc,
            _scheme_meta_reified_cc(ctx),
            [captured, winders, marks],
        );
        call.tail(proc, &[k.into()]);
        Err(Value::undefined())
    }

    /// Capture a one-shot continuation and call `proc` with it.
    ///
    /// Unlike `%call/cc`, the chain is left one-shot (no promotion). The
    /// continuation owns its storage and is resumed in place, which marks it
    /// shot: returning past the capture, or invoking it, uses it up and a
    /// later invocation raises.
    #[scheme(name = "%call/1cc")]
    pub fn call_1cc(proc: Value<'gc>) -> Result<Value<'gc>, Value<'gc>> {
        // SAFETY: this native procedure owns the top frame.
        let stack_state = &ctx.tc().stack;
        let captured = unsafe { stack::call1cc(ctx, stack_state.fp.get(), stack_state.sp.get()) };
        let winders = ctx.winders();
        let marks = ctx.state().current_marks();
        let k = make_reified_closure(
            ctx,
            _raw_scm_proc_reified_1cc,
            _scheme_meta_reified_1cc(ctx),
            [captured, winders, marks],
        );
        call.tail(proc, &[k.into()]);
        Err(Value::undefined())
    }

    #[scheme(name = "%null-continuation")]
    pub fn null_continuation() -> Value<'gc> {
        let k = make_reified_closure(
            ctx,
            _raw_scm_proc_null_reified_cc,
            _scheme_meta_null_reified_cc(ctx),
            [Value::new(false), Value::null(), Value::null()],
        );
        k.into()
    }

    #[scheme(name = "%continuation?")]
    pub fn is_continuation_pred(val: Value<'gc>) -> bool {
        is_reified_continuation_value(val)
    }

    /// Box the raw marks list stored on a reified continuation.
    #[scheme(name = "%continuation-next-marks")]
    pub fn continuation_next_marks_prim(k: Value<'gc>) -> Value<'gc> {
        if !is_reified_continuation_value(k) {
            return call.wrong_argument_violation(
                "%continuation-next-marks",
                "expected a continuation",
                Some(k),
                Some(1),
                1,
                &[k],
            );
        }
        let marks = k.downcast::<Closure>()[3].get();
        box_continuation_marks(ctx, marks)
    }

    /// Install raw marks from a reified continuation into the current state.
    #[scheme(name = "%set-continuation-marks!")]
    pub fn set_continuation_marks(k: Value<'gc>) -> Value<'gc> {
        if !is_reified_continuation_value(k) {
            return call.wrong_argument_violation(
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
        Value::undefined()
    }

    /// Raw winders list stored on a reified continuation.
    #[scheme(name = "$continuation-winders")]
    pub fn continuation_winders_prim(k: Value<'gc>) -> Value<'gc> {
        if !is_reified_continuation_value(k) {
            return call.wrong_argument_violation(
                "$continuation-winders",
                "expected a continuation",
                Some(k),
                Some(1),
                1,
                &[k],
            );
        }
        let winders = k.downcast::<Closure>()[2].get();
        winders
    }

    /// Wind from the current winders to `new_winders`, running out/in thunks.
    ///
    /// Each thunk runs with its own winder's marks, the caller restores the
    /// target marks afterwards.
    #[scheme(name = "$do-wind", unsafe)]
    pub fn do_wind_prim(new_winders: Value<'gc>) -> Value<'gc> {
        if let Err(err) = do_wind(ctx, new_winders) {
            return call.err(err);
        }
        Value::undefined()
    }
}

pub fn init_control<'gc>(ctx: MutationContext<'gc>) {
    control_ops::register(ctx);
}
