//! Returning glue for Tier 2 code

use crate::bytecode::def::{OpGetSlotMetadata, OpPutSlotMetadata};
use crate::runtime::MutationContext;
use crate::runtime::interp::{runtime_procs, slow};
use crate::runtime::objectmodel::{Str, Value, Vector};
use crate::runtime::prims::calls::{self as prims, PrimError};
use crate::runtime::prims::def::Primitive;
use crate::runtime::vm::thunks::gc::yieldpoint_block;

/// What a checked helper returns: the result in the first word and a status
/// in the second. Status 0 means the result is valid. Any other status means
/// the instruction must run in baseline.
#[repr(C)]
pub(crate) struct OptReturn {
    pub value: u64,
    pub status: u64,
}

impl OptReturn {
    const DEOPT: OptReturn = OptReturn {
        value: 0,
        status: 1,
    };

    fn ok(value: u64) -> OptReturn {
        OptReturn { value, status: 0 }
    }
}

/// The procedure registered at runtime-procedure index `id`.
///
/// # Safety
///
/// `th` is the current mutator's context, called from Tier 2 code.
pub(crate) unsafe extern "C" fn capy_opt_runtime_proc(th: *const (), id: u64) -> OptReturn {
    // SAFETY: `th` is the mutator context the emitter passes.
    let ctx: MutationContext<'_> = unsafe { MutationContext::from_ptr(th) };
    OptReturn::ok(runtime_procs::get(ctx, id as u32).bits())
}

/// The symbol switch of `value` on the table at `table`, as a fixnum: the
/// entry index of the matching case, or -1 for the default. A value that is
/// not a symbol deopts, and baseline falls through there.
///
/// # Safety
///
/// `table` is the address of an `OpSwitchSymbol` table in a live code buffer.
pub(crate) unsafe extern "C" fn capy_opt_switch_symbol(
    _th: *const (),
    value: u64,
    table: u64,
) -> OptReturn {
    // SAFETY: the builder passes the address of a table in the unit's code buffer.
    match unsafe { slow::switch_symbol_probe(Value::from_raw(value), table as *const u32) } {
        None => OptReturn::DEOPT,
        Some(None) => OptReturn::ok(Value::from_i32(-1).bits()),
        Some(Some(entry)) => OptReturn::ok(Value::from_i32(entry as i32).bits()),
    }
}

/// The hit path of `get_slot`: the value in the slot the site's cache names
/// for the object's class. A miss deopts, and baseline runs the lookup.
///
/// # Safety
///
/// `meta` is the address of the site's `OpGetSlotMetadata` in a live buffer.
pub(crate) unsafe extern "C" fn capy_opt_get_slot(
    _th: *const (),
    obj: u64,
    meta: u64,
) -> OptReturn {
    // SAFETY: `meta` is the site's metadata, which the builder names.
    let meta = unsafe { &*(meta as *const OpGetSlotMetadata) };
    match unsafe { slow::get_slot_fast(Value::from_raw(obj), meta) } {
        Some(value) => {
            meta.profile.observe(value);
            OptReturn::ok(value.bits())
        }
        None => OptReturn::DEOPT,
    }
}

/// The hit path of `put_slot`: stores `src` when the site's cache names the
/// object's class. Nothing is stored on a miss, which deopts before any write.
///
/// # Safety
///
/// `th` is the current mutator's context, and `meta` is the site's
/// `OpPutSlotMetadata` in a live buffer.
pub(crate) unsafe extern "C" fn capy_opt_put_slot(
    th: *const (),
    obj: u64,
    src: u64,
    meta: u64,
) -> OptReturn {
    // SAFETY: as in `capy_opt_get_slot`, with `th` the mutator context.
    unsafe {
        let ctx: MutationContext<'_> = MutationContext::from_ptr(th);
        let meta = &*(meta as *const OpPutSlotMetadata);
        if slow::put_slot_fast(ctx, Value::from_raw(obj), Value::from_raw(src), meta) {
            OptReturn::ok(0)
        } else {
            OptReturn::DEOPT
        }
    }
}

/// A generic slot read through the descriptor's slot cache. The cache is read
/// at run time (a cleared or re-filled cache is observed), and the epoch is
/// read before and after the lookup: an odd epoch (a change in progress) or a
/// changed epoch is a miss.
///
/// # Safety
///
/// `generic` is a live `GenericDescriptor` and `obj` a Scheme value, passed
/// from Tier 2 code. `th` is the current mutator's context.
pub(crate) unsafe extern "C" fn capy_opt_generic_slot_get(
    th: *const (),
    generic: u64,
    obj: u64,
) -> OptReturn {
    // SAFETY: the builder names a descriptor in the unit's constant table.
    unsafe {
        let ctx: MutationContext<'_> = MutationContext::from_ptr(th);
        let Some(generic) = crate::runtime::objectmodel::class::generic_descriptor_from_value(
            ctx,
            Value::from_raw(generic),
        ) else {
            return OptReturn::DEOPT;
        };
        let epoch = generic.epoch();
        if epoch & 1 != 0 {
            return OptReturn::DEOPT;
        }
        let result = crate::runtime::objectmodel::class::GenericDescriptor::slot_access_fast(
            ctx,
            generic,
            &[Value::from_raw(obj)],
        );
        if generic.epoch() != epoch {
            return OptReturn::DEOPT;
        }
        match result {
            Some(value) => OptReturn::ok(value.bits()),
            None => OptReturn::DEOPT,
        }
    }
}

/// The write counterpart of [`capy_opt_generic_slot_get`]. The result is the
/// stored value, as the generic slot writer returns it.
///
/// # Safety
///
/// As in `capy_opt_generic_slot_get`, with `src` a Scheme value.
pub(crate) unsafe extern "C" fn capy_opt_generic_slot_put(
    th: *const (),
    generic: u64,
    obj: u64,
    src: u64,
) -> OptReturn {
    // SAFETY: as in `capy_opt_generic_slot_get`.
    unsafe {
        let ctx: MutationContext<'_> = MutationContext::from_ptr(th);
        let Some(generic) = crate::runtime::objectmodel::class::generic_descriptor_from_value(
            ctx,
            Value::from_raw(generic),
        ) else {
            return OptReturn::DEOPT;
        };
        let epoch = generic.epoch();
        if epoch & 1 != 0 {
            return OptReturn::DEOPT;
        }
        let result = crate::runtime::objectmodel::class::GenericDescriptor::slot_access_fast(
            ctx,
            generic,
            &[Value::from_raw(obj), Value::from_raw(src)],
        );
        if generic.epoch() != epoch {
            return OptReturn::DEOPT;
        }
        match result {
            Some(value) => OptReturn::ok(value.bits()),
            None => OptReturn::DEOPT,
        }
    }
}

/// The cached `NO_NEXT` method body for the argument classes, under the
/// epoch seqlock. `arg1` is ignored when `argc` is one.
///
/// # Safety
///
/// `generic` is a live `GenericDescriptor`, the arguments are Scheme values
/// and `argc` is a raw integer, passed from Tier 2 code.
pub(crate) unsafe extern "C" fn capy_opt_generic_method(
    th: *const (),
    generic: u64,
    arg0: u64,
    arg1: u64,
    argc: u64,
) -> OptReturn {
    // SAFETY: as in `capy_opt_generic_slot_get`.
    unsafe {
        let ctx: MutationContext<'_> = MutationContext::from_ptr(th);
        let Some(generic) = crate::runtime::objectmodel::class::generic_descriptor_from_value(
            ctx,
            Value::from_raw(generic),
        ) else {
            return OptReturn::DEOPT;
        };
        let argc = argc as usize;
        if !(1..=2).contains(&argc) {
            return OptReturn::DEOPT;
        }
        let mut args = [Value::from_raw(arg0), Value::from_raw(arg1)];
        let epoch = generic.epoch();
        if epoch & 1 != 0 {
            return OptReturn::DEOPT;
        }
        let body = crate::runtime::objectmodel::class::GenericDescriptor::cached_method_body(
            generic,
            &args[..argc],
        );
        if generic.epoch() != epoch {
            return OptReturn::DEOPT;
        }
        match body {
            Some(body) => OptReturn::ok(body.bits()),
            None => OptReturn::DEOPT,
        }
    }
}

/// Runs a pending safepoint on the mutator and returns.
///
/// # Safety
///
/// Called only from a Tier 2 yieldpoint
pub(crate) unsafe extern "C" fn capy_opt_yield(th: *const ()) {
    // SAFETY: `th` is the mutator context the emitter passes.
    let ctx: MutationContext<'_> = unsafe { MutationContext::from_ptr(th) };
    yieldpoint_block(ctx);
}

/// Collects the `count` words at `from` into a fresh vector: the extra
/// arguments of a variadic procedure, still in the incoming frame. Called from
/// the entry glue before anything is written to the frame.
///
/// # Safety
///
/// `th` is the current mutator's context, and `from` points at `count` words
/// of Scheme values.
pub(crate) unsafe extern "C" fn capy_opt_collect_rest(
    th: *const (),
    from: u64,
    count: u64,
) -> OptReturn {
    if count == 0 {
        return OptReturn {
            value: Value::null().bits(),
            status: 0,
        };
    }
    // SAFETY: `th` is the mutator context the emitter passes.
    unsafe {
        let ctx: MutationContext<'_> = MutationContext::from_ptr(th);
        let values = std::slice::from_raw_parts(from as *const Value<'_>, count as usize);
        let vector = Vector::from_slice(ctx, values);
        // The entry glue reads the count back from the status word.
        OptReturn {
            value: Value::from(vector).bits(),
            status: count,
        }
    }
}

/// Runs primitive `prim` over the `count` consecutive words at `from`, as the
/// baseline's `capy_prim_n` does for a variadic call, but returns instead of
/// raising. A primitive error is the status (its kind code, never 0) and the
/// instruction runs again in baseline, which raises it.
///
/// # Safety
///
/// `th` is the current mutator's context, and `from` points at `count` words
/// of Scheme values.
pub(crate) unsafe extern "C" fn capy_opt_prim_n(
    th: *const (),
    prim: u64,
    from: u64,
    count: u64,
) -> OptReturn {
    // SAFETY: `th` is the mutator context the emitter passes.
    unsafe {
        let ctx: MutationContext<'_> = MutationContext::from_ptr(th);
        let Some(&prim) = Primitive::ALL.get(prim as usize) else {
            return OptReturn::DEOPT;
        };
        let args = std::slice::from_raw_parts(from as *const Value<'_>, count as usize);
        match prims::call_prim(ctx, prim, args) {
            Ok(value) => OptReturn::ok(value.bits()),
            Err(PrimError { kind, irritant }) => {
                ctx.state().exit_value.set(irritant);
                OptReturn {
                    value: 0,
                    status: kind.code() as u64,
                }
            }
        }
    }
}

/// # Safety
///
/// `th` is the current mutator's context and `vector` a vector made by
/// [`capy_opt_collect_rest`] (or the empty-rest marker).
pub(crate) unsafe extern "C" fn capy_opt_rest_list(
    th: *const (),
    vector: u64,
    skip: u64,
) -> OptReturn {
    // SAFETY: `th` is the mutator context the emitter passes.
    unsafe {
        let vector = Value::from_raw(vector);

        if vector.is_null() {
            return OptReturn::ok(Value::null().bits());
        }
        let ctx: MutationContext<'_> = MutationContext::from_ptr(th);
        if !vector.is::<Vector>() {
            return OptReturn::DEOPT;
        }
        let vector = vector.downcast::<Vector>();
        let elements = vector.as_slice();
        let from = (Value::from_raw(skip).as_int32().max(0) as usize).min(elements.len());

        if from == elements.len() {
            return OptReturn::ok(Value::null().bits());
        }
        let mut list = Value::null();
        for &element in elements[from..].iter().rev() {
            list = Value::cons(ctx, element, list);
        }
        OptReturn::ok(list.bits())
    }
}

/// Stores `value` in the variable `var` and fires its watchpoint set when it
/// has one
///
/// # Safety
///
/// `th` is the current mutator's context, and `var` a variable.
pub(crate) unsafe extern "C" fn capy_opt_variable_set(
    th: *const (),
    var: u64,
    value: u64,
) -> OptReturn {
    // SAFETY: `th` is the mutator context the emitter passes.
    unsafe {
        let ctx: MutationContext<'_> = MutationContext::from_ptr(th);
        let var = Value::from_raw(var).downcast::<crate::runtime::variable::Variable>();
        var.set(ctx, Value::from_raw(value));
    }
    OptReturn::ok(0)
}

/// The length of the proper list `list`, or `None` for an improper or circular
/// one.
fn proper_length(list: Value<'_>) -> Option<usize> {
    use crate::heap::object::builtin_class_ids::PAIR;
    use crate::runtime::prims::calls::has_heap_class;
    const PLAIN_WALK: usize = 1 << 12;
    let mut cursor = list;
    let mut count = 0usize;
    while count < PLAIN_WALK {
        if cursor.is_null() {
            return Some(count);
        }
        if !has_heap_class(cursor, PAIR) {
            return None;
        }
        cursor = cursor.cdr();
        count += 1;
    }
    checked_length(list)
}

/// [`proper_length`] with the cycle test from the start.
fn checked_length(list: Value<'_>) -> Option<usize> {
    let (mut slow, mut fast) = (list, list);
    let mut len = 0usize;
    loop {
        if fast.is_null() {
            return Some(len);
        }
        if !fast.is_pair() {
            return None;
        }
        fast = fast.cdr();
        len += 1;
        if fast.is_null() {
            return Some(len);
        }
        if !fast.is_pair() {
            return None;
        }
        fast = fast.cdr();
        len += 1;
        slow = slow.cdr();
        if fast == slow {
            return None;
        }
    }
}

/// `(length v)` for a proper list, as a fixnum, in one pass and without the
/// parked frame of the direct primitive wrapper. An improper or circular list
/// deopts, and baseline raises the error from the call.
///
/// # Safety
///
/// `th` is the current mutator's context, called from Tier 2 code.
pub(crate) unsafe extern "C" fn capy_opt_length(_th: *const (), list: u64) -> OptReturn {
    match proper_length(Value::from_raw(list)) {
        Some(len) if len <= i32::MAX as usize => OptReturn::ok(Value::from_i32(len as i32).bits()),
        _ => OptReturn::DEOPT,
    }
}

/// `(append a b)` for a proper list `a`: a fresh copy of `a` whose tail is
/// `b`. The copy is built from the back, so no store into a fresh pair (and no
/// barrier) is needed. An improper or circular `a` deopts, and baseline
/// raises the error.
///
/// # Safety
///
/// `th` is the current mutator's context, called from Tier 2 code. It
/// allocates without collecting
pub(crate) unsafe extern "C" fn capy_opt_append(th: *const (), a: u64, b: u64) -> OptReturn {
    // SAFETY: `th` is the mutator context the emitter passes.
    let ctx: MutationContext<'_> = unsafe { MutationContext::from_ptr(th) };
    let (list, tail) = (Value::from_raw(a), Value::from_raw(b));
    if list.is_null() {
        return OptReturn::ok(b);
    }
    let Some(len) = proper_length(list) else {
        return OptReturn::DEOPT;
    };

    const INLINE: usize = 32;
    let mut small = [Value::null(); INLINE];
    let mut big: Vec<Value<'_>>;
    let items: &mut [Value<'_>] = if len <= INLINE {
        &mut small[..len]
    } else {
        big = vec![Value::null(); len];
        &mut big[..]
    };
    let mut cursor = list;
    for slot in items.iter_mut() {
        *slot = cursor.car();
        cursor = cursor.cdr();
    }
    let mut result = tail;
    for &item in items.iter().rev() {
        result = Value::cons(ctx, item, result);
    }
    OptReturn::ok(result.bits())
}

/// # Safety
///
/// `th` is the current mutator's context, called from Tier 2 code.
pub(crate) unsafe extern "C" fn capy_opt_reduce_rational128(
    th: *const (),
    num_lo: u64,
    num_hi: u64,
    den_lo: u64,
    den_hi: u64,
) -> OptReturn {
    use crate::runtime::objectmodel::{BigInt, IntoValue, Number};
    // SAFETY: `th` is the mutator context the emitter passes.
    let ctx: MutationContext<'_> = unsafe { MutationContext::from_ptr(th) };
    let numerator = join_i128(num_lo, num_hi);
    let denominator = join_i128(den_lo, den_hi);
    if denominator == 0 {
        return OptReturn::DEOPT;
    }
    let value = Number::reduce(
        ctx,
        Number::BigInt(BigInt::from_i128(ctx, numerator)),
        Number::BigInt(BigInt::from_i128(ctx, denominator)),
    )
    .into_value(ctx);
    OptReturn::ok(value.bits())
}

/// Rebuilds the `i128` that a two-limb `(low, high)` pair encodes.
fn join_i128(lo: u64, hi: u64) -> i128 {
    (((hi as u128) << 64) | lo as u128) as i128
}

/// # Safety
///
/// `th` is the current mutator's context, called from Tier 2 code.
pub(crate) unsafe extern "C" fn capy_opt_arith(
    th: *const (),
    kind: u64,
    lhs: u64,
    rhs: u64,
) -> OptReturn {
    use crate::runtime::objectmodel::{IntoValue, Number};
    use crate::runtime::vm::thunks::numbers;
    use std::cmp::Ordering::{Greater, Less};

    // SAFETY: `th` is the mutator context the emitter passes.
    let ctx: MutationContext<'_> = unsafe { MutationContext::from_ptr(th) };
    let (x, y) = (Value::from_raw(lhs), Value::from_raw(rhs));
    let (Some(a), Some(b)) = (x.number(), y.number()) else {
        return OptReturn::DEOPT;
    };
    let order = || Number::compare(ctx, a, b);
    let value = match Value::from_raw(kind).as_int32() {
        0 => numbers::number_plus(ctx, x, y),
        1 => numbers::number_minus(ctx, x, y),
        2 => numbers::number_times(ctx, x, y),
        3 => Value::new(order() == Some(Less)),
        // `<=` and `>=` are the negations of `>` and `<`, as in `numbers::number_le`
        // and `number_ge`, so an unordered pair that has no NaN (a complex
        // number) is true. A NaN compares false, as the baseline's fast paths do.
        4 => Value::new(!(a.is_nan() || b.is_nan()) && order() != Some(Greater)),
        5 => Value::from(Number::equal(ctx, a, b)),
        6 => Value::new(order() == Some(Greater)),
        7 => Value::new(!(a.is_nan() || b.is_nan()) && order() != Some(Less)),
        8 => a.negate(ctx).into_value(ctx),
        _ => return OptReturn::DEOPT,
    };
    OptReturn::ok(value.bits())
}

/// # Safety
///
/// `th` is the mutator context the emitter passes.
pub(crate) unsafe extern "C" fn capy_opt_string_set(
    _th: *const (),
    s: u64,
    index: u64,
    ch: u64,
) -> OptReturn {
    let s = Value::from_raw(s);
    let Some(s) = s.try_as::<Str>() else {
        return OptReturn::DEOPT;
    };
    let index = (index as u32) as usize;
    if index >= s.len() || !s.is_mutable() {
        return OptReturn::DEOPT;
    }
    // A shared buffer must be detached first, which allocates.
    if !s.stringbuf.get().is_mutable() {
        return OptReturn::DEOPT;
    }
    let ch = Value::from_raw(ch);
    if !ch.is_char() {
        return OptReturn::DEOPT;
    }
    let c = ch.char();
    if let Some(chars) = s.chars_mut() {
        if c as u32 > 0xff {
            return OptReturn::DEOPT;
        }
        chars[index] = c as u8;
    } else if let Some(chars) = s.wide_chars_mut() {
        chars[index] = c;
    } else {
        return OptReturn::DEOPT;
    }
    OptReturn::ok(Value::undefined().bits())
}
