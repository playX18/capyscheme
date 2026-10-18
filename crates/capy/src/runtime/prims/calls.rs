//! Out-of-line implementations of the compiler primitives.

use super::def::Primitive;
use crate::heap::Gc;
use crate::heap::barrier;
use crate::heap::object::builtin_class_ids;
use crate::runtime::MutationContext;
use crate::runtime::interp::slow;
use crate::runtime::modules::Variable;
use crate::runtime::value::{ByteVector, Pair, Str, Tuple, Value, Vector};
use crate::runtime::vm::exceptions::RaiseKind;
use crate::runtime::vm::thunks;

/// A condition to raise in the frame that executed the primitive.
pub struct PrimError<'gc> {
    pub kind: RaiseKind,
    pub irritant: Value<'gc>,
}

pub type PrimResult<'gc> = Result<Value<'gc>, PrimError<'gc>>;

/// Per-prim execution counts for `CAPY_PRIM_STATS`, indexed by prim id.
///
/// Debug only: off by default.
static PRIM_COUNTS: [std::sync::atomic::AtomicU64; 256] =
    [const { std::sync::atomic::AtomicU64::new(0) }; 256];

unsafe extern "C" {
    fn atexit(callback: extern "C" fn());
}

extern "C" fn print_prim_stats() {
    use std::sync::atomic::Ordering::Relaxed;
    let mut ranked: Vec<(Primitive, u64)> = Vec::new();
    for (id, prim) in Primitive::ALL.iter().enumerate() {
        let count = PRIM_COUNTS[id].load(Relaxed);
        if count > 0 {
            ranked.push((*prim, count));
        }
    }
    ranked.sort_by(|a, b| b.1.cmp(&a.1));
    let total: u64 = ranked.iter().map(|(_, count)| count).sum();
    log::info!(
        target: crate::heap::logging::STATS_LOG_TARGET,
        "prim stats: {total} total non-inlined prim calls"
    );
    for (prim, count) in ranked.iter().take(40) {
        log::info!(
            target: crate::heap::logging::STATS_LOG_TARGET,
            "  {}: {count}",
            prim.name()
        );
    }
}

fn prim_stats_enabled() -> bool {
    static ENABLED: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ENABLED.get_or_init(|| {
        let on = crate::utils::flags::prim_stats();
        if on {
            // SAFETY: the callback only reads atomics and logs.
            unsafe { atexit(print_prim_stats) };
        }
        on
    })
}

#[inline(always)]
fn count_call(prim: Primitive) {
    if prim_stats_counting() {
        use std::sync::atomic::Ordering::Relaxed;
        if let Some(count) = PRIM_COUNTS.get(prim.id() as usize) {
            count.fetch_add(1, Relaxed);
        }
    }
}

/// Calls of `prim` observed by `call_prim` in this process. Differential
/// tests read it to assert an inline fired (no wrapper call) for its fast
/// types. Counting is always on in tests, the counts are process-global,
/// so each routing assertion retries a clean window (see the tests).
#[cfg(test)]
pub fn prim_call_count(prim: Primitive) -> u64 {
    use std::sync::atomic::Ordering::Relaxed;
    PRIM_COUNTS
        .get(prim.id() as usize)
        .map(|count| count.load(Relaxed))
        .unwrap_or(0)
}

/// Whether `call_prim` counts calls: always in tests (see
/// [`prim_call_count`]), otherwise `CAPY_PRIM_STATS=1` (which also reports
/// at exit). One predictable branch per prim call when off.
fn prim_stats_counting() -> bool {
    if cfg!(test) {
        return true;
    }
    prim_stats_enabled()
}

#[inline(always)]
fn boolean<'gc>(b: bool) -> Value<'gc> {
    Value::new(b)
}

#[inline(always)]
fn truthy(v: Value<'_>) -> bool {
    v.raw_i64() != Value::VALUE_FALSE
}

#[inline(always)]
fn fx<'gc>(v: i32) -> Value<'gc> {
    Value::from_i32(v)
}

/// Low 32 bits of a value known (or assumed) to be a fixnum.
#[inline(always)]
fn fxv(v: Value<'_>) -> i32 {
    v.bits() as i32
}

#[inline(always)]
fn fl<'gc>(v: f64) -> Value<'gc> {
    Value::new(v)
}

#[inline(always)]
fn flv(v: Value<'_>) -> f64 {
    v.as_flonum()
}

#[inline(always)]
fn undefined<'gc>() -> Value<'gc> {
    Value::undefined()
}

#[inline(always)]
fn assertion<'gc>(irritant: Value<'gc>) -> PrimError<'gc> {
    PrimError {
        kind: RaiseKind::AssertionViolation,
        irritant,
    }
}

/// Whether `v` is a heap object of class `class_id`.
#[inline(always)]
pub fn has_heap_class(v: Value<'_>, class_id: u32) -> bool {
    v.is_cell() && !v.is_empty() && v.as_cell_raw().class_id().bits() == class_id
}

/// `%class-id?`: class test that also knows the immediate classes.
pub fn has_class(v: Value<'_>, class_id: u32) -> bool {
    match class_id {
        builtin_class_ids::BOOL => v.is_bool(),
        builtin_class_ids::CHAR => v.is_char(),
        builtin_class_ids::NULL => v.is_null(),
        builtin_class_ids::EOF => v.is_eof(),
        builtin_class_ids::VOID => v.is_void(),
        builtin_class_ids::UNSPECIFIED => v.is_unspecified(),
        builtin_class_ids::UNDEFINED => v.raw_i64() == Value::VALUE_UNDEFINED,
        builtin_class_ids::FIXNUM => v.is_int32(),
        builtin_class_ids::FLONUM => v.is_flonum(),
        _ => has_heap_class(v, class_id),
    }
}

#[inline(always)]
pub unsafe fn pair_car<'gc>(pair: Value<'gc>) -> Value<'gc> {
    unsafe { (pair.bits() as usize as *const Value<'gc>).read() }
}

#[inline(always)]
pub unsafe fn pair_cdr<'gc>(pair: Value<'gc>) -> Value<'gc> {
    unsafe { (pair.bits() as usize as *const Value<'gc>).add(1).read() }
}

const _: () = {
    assert!(std::mem::offset_of!(Pair<'static>, car) == 0);
    assert!(std::mem::offset_of!(Pair<'static>, cdr) == size_of::<Value>());
    assert!(std::mem::offset_of!(Variable<'static>, value) == 0);
    assert!(std::mem::offset_of!(Vector<'static>, length) == 0);
    assert!(std::mem::offset_of!(Tuple<'static>, length) == 0);
};

/// Returns the length of `vector`.
///
/// # Safety
///
/// `vector` is a vector.
#[inline(always)]
pub unsafe fn vector_len(vector: Value<'_>) -> usize {
    unsafe { (vector.bits() as usize as *const usize).read() }
}

#[inline(always)]
pub unsafe fn vector_slot<'gc>(vector: Value<'gc>, index: usize) -> *mut Value<'gc> {
    unsafe {
        (vector.bits() as usize as *mut u8)
            .add(std::mem::offset_of!(Vector<'static>, data))
            .cast::<Value<'gc>>()
            .add(index)
    }
}

#[inline(always)]
unsafe fn tuple_slot<'gc>(tuple: Value<'gc>, index: usize) -> *mut Value<'gc> {
    unsafe {
        (tuple.bits() as usize as *mut u8)
            .add(std::mem::offset_of!(Tuple<'static>, data))
            .cast::<Value<'gc>>()
            .add(index)
    }
}

/// `vector-ref` with the checks the fast path needs, `None` sends the caller
/// to the thunk, which raises the proper condition.
#[inline(always)]
pub fn vector_ref_fast<'gc>(vector: Value<'gc>, index: Value<'gc>) -> Option<Value<'gc>> {
    if has_heap_class(vector, builtin_class_ids::VECTOR) && index.is_int32() {
        let index = fxv(index);
        // SAFETY: `vector` is a vector, the index is bounds-checked.
        unsafe {
            if index >= 0 && (index as usize) < vector_len(vector) {
                return Some(vector_slot(vector, index as usize).read());
            }
        }
    }
    None
}

#[inline(always)]
pub fn vector_set_fast<'gc>(
    ctx: MutationContext<'gc>,
    vector: Value<'gc>,
    index: Value<'gc>,
    value: Value<'gc>,
) -> bool {
    if has_heap_class(vector, builtin_class_ids::VECTOR) && index.is_int32() {
        let index = fxv(index);
        // SAFETY: `vector` is a vector, so the length read is in bounds.
        unsafe {
            if index >= 0 && (index as usize) < vector_len(vector) {
                Gc::write(ctx, vector.downcast::<Vector>())[index as usize]
                    .unlock()
                    .set(value);
                return true;
            }
        }
    }
    false
}

fn fl_min(a: f64, b: f64) -> f64 {
    if a.is_nan() || b.is_nan() {
        f64::NAN
    } else if a < b {
        a
    } else if b < a {
        b
    } else if a.is_sign_negative() {
        a
    } else {
        b
    }
}

fn fl_max(a: f64, b: f64) -> f64 {
    if a.is_nan() || b.is_nan() {
        f64::NAN
    } else if a > b {
        a
    } else if b > a {
        b
    } else if a.is_sign_positive() {
        a
    } else {
        b
    }
}

macro_rules! check_fx {
    ($a:expr) => {
        if !$a.is_int32() {
            return Err(assertion($a));
        }
    };
    ($a:expr, $b:expr) => {
        if !$a.is_int32() || !$b.is_int32() {
            return Err(assertion($a));
        }
    };
}

macro_rules! check_fl {
    ($a:expr) => {
        if !$a.is_flonum() {
            return Err(assertion($a));
        }
    };
    ($a:expr, $b:expr) => {
        if !$a.is_flonum() || !$b.is_flonum() {
            return Err(assertion($a));
        }
    };
}

fn ovf<'gc>(result: Option<i32>) -> Value<'gc> {
    match result {
        Some(value) => fx(value),
        None => Value::new(false),
    }
}

fn compare_chain<'gc>(
    ctx: MutationContext<'gc>,
    a: &[Value<'gc>],
    cmp: fn(MutationContext<'gc>, Value<'gc>, Value<'gc>) -> Value<'gc>,
) -> Value<'gc> {
    for pair in a.windows(2) {
        if !truthy(cmp(ctx, pair[0], pair[1])) {
            return Value::new(false);
        }
    }
    Value::new(true)
}

/// Run primitive `prim` on args `a`.
///
/// # Safety
///
/// `a` must hold as many arguments as the compiler emitted for `prim`.
pub unsafe fn call_prim<'gc>(
    ctx: MutationContext<'gc>,
    prim: Primitive,
    a: &[Value<'gc>],
) -> PrimResult<'gc> {
    count_call(prim);
    // SAFETY: see the function's docs.
    unsafe { PRIM_ARMS[prim.id() as usize](ctx, a) }
}

#[inline(always)]
pub(crate) unsafe fn prim_class_idp<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
    b: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(Value::new(has_class(a, b.as_int32() as u32)))
}

#[inline(always)]
pub(crate) unsafe fn prim_refptr<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
    b: Value<'gc>,
) -> PrimResult<'gc> {
    unsafe {
        let offset = b.bits() as u32 as usize;
        Ok(((a.bits() as usize + offset) as *const Value<'gc>).read())
    }
}

#[inline(always)]
pub(crate) unsafe fn prim_usize_to_value<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(Value::from_raw(
        (a.bits() as u32 as u64) | Value::NUMBER_TAG as u64,
    ))
}

#[inline(always)]
pub(crate) unsafe fn prim_cache_ref<'gc>(_ctx: MutationContext<'gc>) -> PrimResult<'gc> {
    unreachable!("cache operations are lowered to constant-pool instructions")
}

#[inline(always)]
pub(crate) unsafe fn prim_cache_set<'gc>(_ctx: MutationContext<'gc>) -> PrimResult<'gc> {
    unreachable!("cache operations are lowered to constant-pool instructions")
}

#[inline(always)]
pub(crate) unsafe fn prim_slot_ref<'gc>(_ctx: MutationContext<'gc>) -> PrimResult<'gc> {
    unreachable!("slot operations are lowered to slot instructions with slow paths")
}

#[inline(always)]
pub(crate) unsafe fn prim_slot_set<'gc>(_ctx: MutationContext<'gc>) -> PrimResult<'gc> {
    unreachable!("slot operations are lowered to slot instructions with slow paths")
}

#[inline(always)]
pub(crate) unsafe fn prim_is_immediate<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(Value::new(a.raw_i64() & Value::NOT_CELL_MASK != 0))
}

#[inline(always)]
pub(crate) unsafe fn prim_is_heap_object<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(Value::new(
        a.raw_i64() & Value::NOT_CELL_MASK == 0 && a.bits() != 0,
    ))
}

#[inline(always)]
pub(crate) unsafe fn prim_variable_bound<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    unsafe { Ok(Value::new(pair_car(a).raw_i64() != Value::VALUE_UNDEFINED)) }
}

#[inline(always)]
pub(crate) unsafe fn prim_variable_ref<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    unsafe { Ok(pair_car(a)) }
}

#[inline(always)]
pub(crate) unsafe fn prim_variable_set<'gc>(
    ctx: MutationContext<'gc>,
    a: Value<'gc>,
    b: Value<'gc>,
) -> PrimResult<'gc> {
    barrier::field!(Gc::write(ctx, a.downcast::<Variable>()), Variable, value)
        .unlock()
        .set(b);
    Ok(undefined())
}

#[inline(always)]
pub(crate) unsafe fn prim_make_box<'gc>(
    ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(thunks::make_variable(ctx, a))
}

#[inline(always)]
pub(crate) unsafe fn prim_ensure_local_var<'gc>(
    ctx: MutationContext<'gc>,
    a: Value<'gc>,
    b: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(thunks::module_ensure_local_variable(ctx, a, b))
}

#[inline(always)]
pub(crate) unsafe fn prim_lookup<'gc>(
    ctx: MutationContext<'gc>,
    a: Value<'gc>,
    b: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(thunks::lookup(ctx, a, b))
}

#[inline(always)]
pub(crate) unsafe fn prim_lookup_bound<'gc>(
    ctx: MutationContext<'gc>,
    a: Value<'gc>,
    b: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(thunks::lookup_bound(ctx, a, b))
}

#[inline(always)]
pub(crate) unsafe fn prim_lookup_bound_public<'gc>(
    ctx: MutationContext<'gc>,
    a: Value<'gc>,
    b: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(thunks::lookup_bound_public(ctx, a, b))
}

#[inline(always)]
pub(crate) unsafe fn prim_lookup_bound_private<'gc>(
    ctx: MutationContext<'gc>,
    a: Value<'gc>,
    b: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(thunks::lookup_bound_private(ctx, a, b))
}

#[inline(always)]
pub(crate) unsafe fn prim_current_module<'gc>(
    ctx: MutationContext<'gc>,
    a: &[Value<'gc>],
) -> PrimResult<'gc> {
    Ok(match a.first() {
        Some(module) => thunks::set_current_module(ctx, *module),
        None => thunks::current_module(ctx),
    })
}

#[inline(always)]
pub(crate) unsafe fn prim_define<'gc>(
    ctx: MutationContext<'gc>,
    a: Value<'gc>,
    b: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(thunks::define(ctx, a, b))
}

#[inline(always)]
pub(crate) unsafe fn prim_is_variable<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(Value::new(has_heap_class(a, builtin_class_ids::VARIABLE)))
}

#[inline(always)]
pub(crate) unsafe fn prim_set_car<'gc>(
    ctx: MutationContext<'gc>,
    a: Value<'gc>,
    b: Value<'gc>,
) -> PrimResult<'gc> {
    barrier::field!(Gc::write(ctx, a.downcast::<Pair>()), Pair, car)
        .unlock()
        .set(b);
    Ok(undefined())
}

#[inline(always)]
pub(crate) unsafe fn prim_set_cdr<'gc>(
    ctx: MutationContext<'gc>,
    a: Value<'gc>,
    b: Value<'gc>,
) -> PrimResult<'gc> {
    barrier::field!(Gc::write(ctx, a.downcast::<Pair>()), Pair, cdr)
        .unlock()
        .set(b);
    Ok(undefined())
}

#[inline(always)]
pub(crate) unsafe fn prim_cons<'gc>(
    ctx: MutationContext<'gc>,
    a: Value<'gc>,
    b: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(Value::cons(ctx, a, b))
}

#[inline(always)]
pub(crate) unsafe fn prim_reverse<'gc>(
    ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(thunks::reverse(ctx, a))
}

#[inline(always)]
pub(crate) unsafe fn prim_is_eof_object<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(Value::new(a.is_eof()))
}

#[inline(always)]
pub(crate) unsafe fn prim_is_null<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(Value::new(a.is_null()))
}

#[inline(always)]
pub(crate) unsafe fn prim_unspec<'gc>(_ctx: MutationContext<'gc>) -> PrimResult<'gc> {
    Ok(undefined())
}

#[inline(always)]
pub(crate) unsafe fn prim_is_unspecified<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(Value::new(a.raw_i64() == Value::VALUE_UNDEFINED))
}

#[inline(always)]
pub(crate) unsafe fn prim_is_pair<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(Value::new(has_heap_class(a, builtin_class_ids::PAIR)))
}

#[inline(always)]
pub(crate) unsafe fn prim_is_list<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(Value::new(thunks::listp(a)))
}

#[inline(always)]
pub(crate) unsafe fn prim_append<'gc>(
    ctx: MutationContext<'gc>,
    a: Value<'gc>,
    b: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(thunks::append(ctx, a, b))
}

#[inline(always)]
pub(crate) unsafe fn prim_list<'gc>(
    ctx: MutationContext<'gc>,
    a: &[Value<'gc>],
) -> PrimResult<'gc> {
    let mut list = Value::null();
    for value in a.iter().rev() {
        list = Value::cons(ctx, *value, list);
    }
    Ok(list)
}

#[inline(always)]
pub(crate) unsafe fn prim_memq<'gc>(
    ctx: MutationContext<'gc>,
    a: Value<'gc>,
    b: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(thunks::memq(ctx, a, b))
}

#[inline(always)]
pub(crate) unsafe fn prim_memv<'gc>(
    ctx: MutationContext<'gc>,
    a: Value<'gc>,
    b: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(thunks::memv(ctx, a, b))
}

#[inline(always)]
pub(crate) unsafe fn prim_car<'gc>(_ctx: MutationContext<'gc>, a: Value<'gc>) -> PrimResult<'gc> {
    unsafe {
        if !has_heap_class(a, builtin_class_ids::PAIR) {
            return Err(PrimError {
                kind: RaiseKind::CarNotPair,
                irritant: a,
            });
        }
        Ok(pair_car(a))
    }
}

#[inline(always)]
pub(crate) unsafe fn prim_cdr<'gc>(_ctx: MutationContext<'gc>, a: Value<'gc>) -> PrimResult<'gc> {
    unsafe {
        if !has_heap_class(a, builtin_class_ids::PAIR) {
            return Err(PrimError {
                kind: RaiseKind::CdrNotPair,
                irritant: a,
            });
        }
        Ok(pair_cdr(a))
    }
}

#[inline(always)]
pub(crate) unsafe fn prim_length<'gc>(ctx: MutationContext<'gc>, a: Value<'gc>) -> PrimResult<'gc> {
    Ok(thunks::length(ctx, a))
}

#[inline(always)]
pub(crate) unsafe fn prim_is_vector<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(Value::new(has_heap_class(a, builtin_class_ids::VECTOR)))
}

#[inline(always)]
pub(crate) unsafe fn prim_is_bytevector<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(Value::new(a.is_bytevector()))
}

#[inline(always)]
pub(crate) unsafe fn prim_vector<'gc>(
    ctx: MutationContext<'gc>,
    a: &[Value<'gc>],
) -> PrimResult<'gc> {
    Ok(Vector::from_slice(ctx, a).into())
}

#[inline(always)]
pub(crate) unsafe fn prim_tuple<'gc>(
    ctx: MutationContext<'gc>,
    a: &[Value<'gc>],
) -> PrimResult<'gc> {
    unsafe {
        let tuple = thunks::make_tuple(ctx, fx(a.len() as i32), undefined());
        for (index, value) in a.iter().enumerate() {
            tuple_slot(tuple, index).write(*value);
        }
        Ok(tuple)
    }
}

#[inline(always)]
pub(crate) unsafe fn prim_make_tuple<'gc>(
    ctx: MutationContext<'gc>,
    a: &[Value<'gc>],
) -> PrimResult<'gc> {
    Ok(thunks::make_tuple(
        ctx,
        a[0],
        a.get(1).copied().unwrap_or_else(undefined),
    ))
}

#[inline(always)]
pub(crate) unsafe fn prim_make_vector<'gc>(
    ctx: MutationContext<'gc>,
    a: &[Value<'gc>],
) -> PrimResult<'gc> {
    Ok(thunks::make_vector(
        ctx,
        a[0],
        a.get(1).copied().unwrap_or_else(undefined),
    ))
}

#[inline(always)]
pub(crate) unsafe fn prim_vector_ref<'gc>(
    ctx: MutationContext<'gc>,
    a: Value<'gc>,
    b: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(match vector_ref_fast(a, b) {
        Some(value) => value,
        None => thunks::vector_ref(ctx, a, b),
    })
}

#[inline(always)]
pub(crate) unsafe fn prim_vector_set<'gc>(
    ctx: MutationContext<'gc>,
    a: Value<'gc>,
    b: Value<'gc>,
    c: Value<'gc>,
) -> PrimResult<'gc> {
    if !vector_set_fast(ctx, a, b, c) {
        thunks::vector_set(ctx, a, b, c);
    }
    Ok(undefined())
}

#[inline(always)]
pub(crate) unsafe fn prim_bytevector_length<'gc>(
    ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(thunks::bytevector_length(ctx, a))
}

#[inline(always)]
pub(crate) unsafe fn prim_bytevector_u8_ref<'gc>(
    ctx: MutationContext<'gc>,
    a: Value<'gc>,
    b: Value<'gc>,
) -> PrimResult<'gc> {
    unsafe {
        if a.is_bytevector() && b.is_int32() {
            let bytes = a.downcast::<ByteVector>();
            let index = fxv(b);
            if index >= 0 && (index as usize) < bytes.len() {
                return Ok(fx(
                    bytes.contents().to_ptr::<u8>().add(index as usize).read() as i32,
                ));
            }
        }
        Ok(thunks::bytevector_u8_ref(ctx, a, b))
    }
}

#[inline(always)]
pub(crate) unsafe fn prim_tuple_size<'gc>(
    ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(thunks::tuple_size(ctx, a))
}

#[inline(always)]
pub(crate) unsafe fn prim_tuple_ref<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
    b: Value<'gc>,
) -> PrimResult<'gc> {
    unsafe { Ok(tuple_slot(a, fxv(b) as u32 as usize).read()) }
}

#[inline(always)]
pub(crate) unsafe fn prim_tuple_set<'gc>(
    ctx: MutationContext<'gc>,
    a: Value<'gc>,
    b: Value<'gc>,
    c: Value<'gc>,
) -> PrimResult<'gc> {
    Gc::write(ctx, a.downcast::<Tuple>())[fxv(b) as u32 as usize]
        .unlock()
        .set(c);
    Ok(undefined())
}

#[inline(always)]
pub(crate) unsafe fn prim_is_tuple<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(Value::new(has_heap_class(a, builtin_class_ids::TUPLE)))
}

#[inline(always)]
pub(crate) unsafe fn prim_fx_eq<'gc>(
    ctx: MutationContext<'gc>,
    a: &[Value<'gc>],
) -> PrimResult<'gc> {
    for pair in a.windows(2) {
        if !truthy(thunks::fxeq(ctx, pair[0], pair[1])) {
            return Ok(Value::new(false));
        }
    }
    Ok(Value::new(true))
}

#[inline(always)]
pub(crate) unsafe fn prim_ash<'gc>(
    ctx: MutationContext<'gc>,
    a: Value<'gc>,
    b: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(thunks::ash(ctx, a, b))
}

#[inline(always)]
pub(crate) unsafe fn prim_logand<'gc>(
    ctx: MutationContext<'gc>,
    a: Value<'gc>,
    b: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(thunks::logand(ctx, a, b))
}

#[inline(always)]
pub(crate) unsafe fn prim_logior<'gc>(
    ctx: MutationContext<'gc>,
    a: Value<'gc>,
    b: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(thunks::logior(ctx, a, b))
}

#[inline(always)]
pub(crate) unsafe fn prim_lognot<'gc>(ctx: MutationContext<'gc>, a: Value<'gc>) -> PrimResult<'gc> {
    Ok(thunks::lognot(ctx, a))
}

#[inline(always)]
pub(crate) unsafe fn prim_expt<'gc>(
    ctx: MutationContext<'gc>,
    a: Value<'gc>,
    b: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(thunks::expt(ctx, a, b))
}

#[inline(always)]
pub(crate) unsafe fn prim_abs<'gc>(ctx: MutationContext<'gc>, a: Value<'gc>) -> PrimResult<'gc> {
    Ok(thunks::abs(ctx, a))
}

#[inline(always)]
pub(crate) unsafe fn prim_sqrt<'gc>(ctx: MutationContext<'gc>, a: Value<'gc>) -> PrimResult<'gc> {
    Ok(if a.is_flonum() {
        fl(flv(a).sqrt())
    } else {
        thunks::sqrt(ctx, a)
    })
}

#[inline(always)]
pub(crate) unsafe fn prim_cos<'gc>(ctx: MutationContext<'gc>, a: Value<'gc>) -> PrimResult<'gc> {
    Ok(thunks::cos(ctx, a))
}

#[inline(always)]
pub(crate) unsafe fn prim_sin<'gc>(ctx: MutationContext<'gc>, a: Value<'gc>) -> PrimResult<'gc> {
    Ok(thunks::sin(ctx, a))
}

#[inline(always)]
pub(crate) unsafe fn prim_tan<'gc>(ctx: MutationContext<'gc>, a: Value<'gc>) -> PrimResult<'gc> {
    Ok(thunks::tan(ctx, a))
}

#[inline(always)]
pub(crate) unsafe fn prim_atan<'gc>(
    ctx: MutationContext<'gc>,
    a: &[Value<'gc>],
) -> PrimResult<'gc> {
    Ok(if a.len() == 2 {
        thunks::atan2(ctx, a[0], a[1])
    } else {
        thunks::atan(ctx, a[0])
    })
}

#[inline(always)]
pub(crate) unsafe fn prim_asin<'gc>(ctx: MutationContext<'gc>, a: Value<'gc>) -> PrimResult<'gc> {
    Ok(thunks::asin(ctx, a))
}

#[inline(always)]
pub(crate) unsafe fn prim_acos<'gc>(ctx: MutationContext<'gc>, a: Value<'gc>) -> PrimResult<'gc> {
    Ok(thunks::acos(ctx, a))
}

#[inline(always)]
pub(crate) unsafe fn prim_ceiling<'gc>(
    ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(thunks::ceiling(ctx, a))
}

#[inline(always)]
pub(crate) unsafe fn prim_floor<'gc>(ctx: MutationContext<'gc>, a: Value<'gc>) -> PrimResult<'gc> {
    Ok(thunks::floor(ctx, a))
}

#[inline(always)]
pub(crate) unsafe fn prim_truncate<'gc>(
    ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(thunks::truncate(ctx, a))
}

#[inline(always)]
pub(crate) unsafe fn prim_plus<'gc>(
    ctx: MutationContext<'gc>,
    a: &[Value<'gc>],
) -> PrimResult<'gc> {
    Ok(match a {
        [] => undefined(),
        [only] => *only,
        [first, rest @ ..] => {
            let mut acc = *first;
            for value in rest {
                acc = add(ctx, acc, *value);
            }
            acc
        }
    })
}

#[inline(always)]
pub(crate) unsafe fn prim_minus<'gc>(
    ctx: MutationContext<'gc>,
    a: &[Value<'gc>],
) -> PrimResult<'gc> {
    Ok(match a {
        [] => undefined(),
        [only] => thunks::negate(ctx, *only),
        [first, rest @ ..] => {
            let mut acc = *first;
            for value in rest {
                acc = sub(ctx, acc, *value);
            }
            acc
        }
    })
}

#[inline(always)]
pub(crate) unsafe fn prim_times<'gc>(
    ctx: MutationContext<'gc>,
    a: &[Value<'gc>],
) -> PrimResult<'gc> {
    Ok(match a {
        [] => undefined(),
        [only] => *only,
        [first, rest @ ..] => {
            let mut acc = *first;
            for value in rest {
                acc = mul(ctx, acc, *value);
            }
            acc
        }
    })
}

#[inline(always)]
pub(crate) unsafe fn prim_div<'gc>(
    ctx: MutationContext<'gc>,
    a: Value<'gc>,
    b: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(thunks::number_div(ctx, a, b))
}

#[inline(always)]
pub(crate) unsafe fn prim_numeric_equal<'gc>(
    ctx: MutationContext<'gc>,
    a: &[Value<'gc>],
) -> PrimResult<'gc> {
    Ok(compare_chain(ctx, a, thunks::number_eq))
}

#[inline(always)]
pub(crate) unsafe fn prim_numeric_lt<'gc>(
    ctx: MutationContext<'gc>,
    a: &[Value<'gc>],
) -> PrimResult<'gc> {
    Ok(compare_chain(ctx, a, thunks::number_lt))
}

#[inline(always)]
pub(crate) unsafe fn prim_numeric_gt<'gc>(
    ctx: MutationContext<'gc>,
    a: &[Value<'gc>],
) -> PrimResult<'gc> {
    Ok(compare_chain(ctx, a, thunks::number_gt))
}

#[inline(always)]
pub(crate) unsafe fn prim_numeric_gte<'gc>(
    ctx: MutationContext<'gc>,
    a: &[Value<'gc>],
) -> PrimResult<'gc> {
    Ok(compare_chain(ctx, a, thunks::number_ge))
}

#[inline(always)]
pub(crate) unsafe fn prim_numeric_lte<'gc>(
    ctx: MutationContext<'gc>,
    a: &[Value<'gc>],
) -> PrimResult<'gc> {
    Ok(if a.len() == 2 {
        thunks::number_le(ctx, a[0], a[1])
    } else {
        compare_chain(ctx, a, thunks::number_le)
    })
}

#[inline(always)]
pub(crate) unsafe fn prim_exact_to_inexact<'gc>(
    ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(thunks::exact2inexact(ctx, a))
}

#[inline(always)]
pub(crate) unsafe fn prim_inexact_to_exact<'gc>(
    ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(thunks::inexact_to_exact(ctx, a))
}

#[inline(always)]
pub(crate) unsafe fn prim_is_even<'gc>(
    ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(if a.is_int32() {
        Value::new(fxv(a) & 1 == 0)
    } else {
        Value::new(thunks::evenp(ctx, a) == Value::new(true))
    })
}

#[inline(always)]
pub(crate) unsafe fn prim_is_odd<'gc>(ctx: MutationContext<'gc>, a: Value<'gc>) -> PrimResult<'gc> {
    Ok(if a.is_int32() {
        Value::new(fxv(a) & 1 == 1)
    } else {
        Value::new(thunks::oddp(ctx, a) == Value::new(true))
    })
}

#[inline(always)]
pub(crate) unsafe fn prim_is_zero<'gc>(
    ctx: MutationContext<'gc>,
    a: &[Value<'gc>],
) -> PrimResult<'gc> {
    Ok(match a.first() {
        None => undefined(),
        Some(value) if value.is_int32() => Value::new(fxv(*value) == 0),
        Some(value) => Value::new(thunks::is_zero(ctx, *value) == Value::new(true)),
    })
}

#[inline(always)]
pub(crate) unsafe fn prim_quotient<'gc>(
    ctx: MutationContext<'gc>,
    a: Value<'gc>,
    b: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(thunks::quotient(ctx, a, b))
}

#[inline(always)]
pub(crate) unsafe fn prim_remainder<'gc>(
    ctx: MutationContext<'gc>,
    a: Value<'gc>,
    b: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(thunks::remainder(ctx, a, b))
}

#[inline(always)]
pub(crate) unsafe fn prim_modulo<'gc>(
    ctx: MutationContext<'gc>,
    a: Value<'gc>,
    b: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(thunks::modulo(ctx, a, b))
}

#[inline(always)]
pub(crate) unsafe fn prim_is_procedure<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(Value::new(has_heap_class(a, builtin_class_ids::CLOSURE)))
}

#[inline(always)]
pub(crate) unsafe fn prim_is_string<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(Value::new(has_heap_class(a, builtin_class_ids::STRING)))
}

#[inline(always)]
pub(crate) unsafe fn prim_is_boolean<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(Value::new(a.is_bool()))
}

#[inline(always)]
pub(crate) unsafe fn prim_is_symbol<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(Value::new(has_heap_class(a, builtin_class_ids::SYMBOL)))
}

#[inline(always)]
pub(crate) unsafe fn prim_is_eq<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
    b: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(Value::new(a.bits() == b.bits()))
}

#[inline(always)]
pub(crate) unsafe fn prim_is_eqv<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
    b: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(Value::new(thunks::eqv(a, b)))
}

#[inline(always)]
pub(crate) unsafe fn prim_is_equal<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
    b: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(Value::new(thunks::equal(a, b)))
}

#[inline(always)]
pub(crate) unsafe fn prim_is_exact_integer<'gc>(
    ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(thunks::exact_integerp(ctx, a))
}

#[inline(always)]
pub(crate) unsafe fn prim_is_integer<'gc>(
    ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(Value::new(thunks::integerp(ctx, a)))
}

#[inline(always)]
pub(crate) unsafe fn prim_is_char<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(Value::new(a.is_char()))
}

#[inline(always)]
pub(crate) unsafe fn prim_is_number<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(Value::new(a.is_number()))
}

#[inline(always)]
pub(crate) unsafe fn prim_is_complex<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(Value::new(thunks::complexp(a)))
}

#[inline(always)]
pub(crate) unsafe fn prim_is_nan<'gc>(ctx: MutationContext<'gc>, a: Value<'gc>) -> PrimResult<'gc> {
    Ok(thunks::nanp(ctx, a))
}

#[inline(always)]
pub(crate) unsafe fn prim_is_real<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(Value::new(thunks::realp(a)))
}

#[inline(always)]
pub(crate) unsafe fn prim_is_rational<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(Value::new(thunks::is_rational(a)))
}

#[inline(always)]
pub(crate) unsafe fn prim_is_inexact<'gc>(
    ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(thunks::inexactp(ctx, a))
}

#[inline(always)]
pub(crate) unsafe fn prim_is_exact<'gc>(
    ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(thunks::exactp(ctx, a))
}

#[inline(always)]
pub(crate) unsafe fn prim_not<'gc>(_ctx: MutationContext<'gc>, a: Value<'gc>) -> PrimResult<'gc> {
    Ok(Value::new(!truthy(a)))
}

#[inline(always)]
pub(crate) unsafe fn prim_string_length<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    unsafe { Ok(fx(a.downcast_unchecked::<Str>().length as i32)) }
}

#[inline(always)]
pub(crate) unsafe fn prim_string_ref<'gc>(
    ctx: MutationContext<'gc>,
    a: Value<'gc>,
    b: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(thunks::string_ref(ctx, a, b))
}

#[inline(always)]
pub(crate) unsafe fn prim_integer_to_char<'gc>(
    ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(thunks::integer_to_char(ctx, a))
}

#[inline(always)]
pub(crate) unsafe fn prim_char_to_integer<'gc>(
    ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(thunks::char_to_integer(ctx, a))
}

#[inline(always)]
pub(crate) unsafe fn prim_breakpoint<'gc>(_ctx: MutationContext<'gc>) -> PrimResult<'gc> {
    Ok(Value::new(true))
}

#[inline(always)]
pub(crate) unsafe fn prim_symbol_to_string<'gc>(
    ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(thunks::symbol2string(ctx, a))
}

#[inline(always)]
pub(crate) unsafe fn prim_string_to_symbol<'gc>(
    ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(thunks::string2symbol(ctx, a))
}

#[inline(always)]
pub(crate) unsafe fn prim_push_cframe<'gc>(_ctx: MutationContext<'gc>) -> PrimResult<'gc> {
    unreachable!("continuations as first-class objects are gone")
}

#[inline(always)]
pub(crate) unsafe fn prim_default_retk<'gc>(_ctx: MutationContext<'gc>) -> PrimResult<'gc> {
    unreachable!("continuations as first-class objects are gone")
}

#[inline(always)]
pub(crate) unsafe fn prim_current_continuation_marks<'gc>(
    ctx: MutationContext<'gc>,
) -> PrimResult<'gc> {
    Ok(thunks::current_continuation_marks(ctx))
}

#[inline(always)]
pub(crate) unsafe fn prim_set_attachments<'gc>(
    ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(thunks::set_attachments(ctx, a))
}

#[inline(always)]
pub(crate) unsafe fn prim_attachment_push<'gc>(
    ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(thunks::attachment_push(ctx, a))
}

#[inline(always)]
pub(crate) unsafe fn prim_attachment_pop<'gc>(ctx: MutationContext<'gc>) -> PrimResult<'gc> {
    Ok(thunks::attachment_pop(ctx))
}

#[inline(always)]
pub(crate) unsafe fn prim_attachment_set<'gc>(
    ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(thunks::attachment_set(ctx, a))
}

#[inline(always)]
pub(crate) unsafe fn prim_attachment_consume<'gc>(ctx: MutationContext<'gc>) -> PrimResult<'gc> {
    Ok(thunks::attachment_consume(ctx))
}

#[inline(always)]
pub(crate) unsafe fn prim_attachment_consume_reify<'gc>(
    ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(thunks::attachment_consume_reify(ctx, a))
}

#[inline(always)]
pub(crate) unsafe fn prim_update_mark<'gc>(
    ctx: MutationContext<'gc>,
    a: Value<'gc>,
    b: Value<'gc>,
    c: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(thunks::update_mark(ctx, a, b, c))
}

#[inline(always)]
pub(crate) unsafe fn prim_winders<'gc>(
    ctx: MutationContext<'gc>,
    a: &[Value<'gc>],
) -> PrimResult<'gc> {
    Ok(match a.first() {
        None => ctx.winders(),
        Some(winders) => {
            ctx.set_winders(*winders);
            *winders
        }
    })
}

#[inline(always)]
pub(crate) unsafe fn prim_make_syntax<'gc>(
    ctx: MutationContext<'gc>,
    a: &[Value<'gc>],
) -> PrimResult<'gc> {
    Ok(thunks::make_syntax(ctx, a[0], a[1], a[2], a[3], a[4]))
}

#[inline(always)]
pub(crate) unsafe fn prim_is_fixnum<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(Value::new(a.is_int32()))
}

#[inline(always)]
pub(crate) unsafe fn prim_is_flonum<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    Ok(Value::new(a.is_flonum()))
}

#[inline(always)]
pub(crate) unsafe fn prim_fx_add<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
    b: Value<'gc>,
) -> PrimResult<'gc> {
    check_fx!(a, b);
    Ok(fx(fxv(a).wrapping_add(fxv(b))))
}

#[inline(always)]
pub(crate) unsafe fn prim_fx_sub<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
    b: Value<'gc>,
) -> PrimResult<'gc> {
    check_fx!(a, b);
    Ok(fx(fxv(a).wrapping_sub(fxv(b))))
}

#[inline(always)]
pub(crate) unsafe fn prim_fx_mul<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
    b: Value<'gc>,
) -> PrimResult<'gc> {
    check_fx!(a, b);
    Ok(fx(fxv(a).wrapping_mul(fxv(b))))
}

#[inline(always)]
pub(crate) unsafe fn prim_fx_add_ovf<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
    b: Value<'gc>,
) -> PrimResult<'gc> {
    check_fx!(a, b);
    Ok(ovf(fxv(a).checked_add(fxv(b))))
}

#[inline(always)]
pub(crate) unsafe fn prim_fx_sub_ovf<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
    b: Value<'gc>,
) -> PrimResult<'gc> {
    check_fx!(a, b);
    Ok(ovf(fxv(a).checked_sub(fxv(b))))
}

#[inline(always)]
pub(crate) unsafe fn prim_fx_mul_ovf<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
    b: Value<'gc>,
) -> PrimResult<'gc> {
    check_fx!(a, b);
    Ok(ovf(fxv(a).checked_mul(fxv(b))))
}

#[inline(always)]
pub(crate) unsafe fn prim_fx_lt<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
    b: Value<'gc>,
) -> PrimResult<'gc> {
    check_fx!(a, b);
    Ok(Value::new(fxv(a) < fxv(b)))
}

#[inline(always)]
pub(crate) unsafe fn prim_fx_le<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
    b: Value<'gc>,
) -> PrimResult<'gc> {
    check_fx!(a, b);
    Ok(Value::new(fxv(a) <= fxv(b)))
}

#[inline(always)]
pub(crate) unsafe fn prim_fx_gt<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
    b: Value<'gc>,
) -> PrimResult<'gc> {
    check_fx!(a, b);
    Ok(Value::new(fxv(a) > fxv(b)))
}

#[inline(always)]
pub(crate) unsafe fn prim_fx_ge<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
    b: Value<'gc>,
) -> PrimResult<'gc> {
    check_fx!(a, b);
    Ok(Value::new(fxv(a) >= fxv(b)))
}

#[inline(always)]
pub(crate) unsafe fn prim_fx_and<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
    b: Value<'gc>,
) -> PrimResult<'gc> {
    check_fx!(a, b);
    Ok(fx(fxv(a) & fxv(b)))
}

#[inline(always)]
pub(crate) unsafe fn prim_fx_ior<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
    b: Value<'gc>,
) -> PrimResult<'gc> {
    check_fx!(a, b);
    Ok(fx(fxv(a) | fxv(b)))
}

#[inline(always)]
pub(crate) unsafe fn prim_fx_xor<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
    b: Value<'gc>,
) -> PrimResult<'gc> {
    check_fx!(a, b);
    Ok(fx(fxv(a) ^ fxv(b)))
}

#[inline(always)]
pub(crate) unsafe fn prim_fx_not<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    check_fx!(a);
    Ok(fx(!fxv(a)))
}

#[inline(always)]
pub(crate) unsafe fn prim_fx_ashl<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
    b: Value<'gc>,
) -> PrimResult<'gc> {
    check_fx!(a, b);
    Ok(fx(fxv(a).wrapping_shl(fxv(b) as u32)))
}

#[inline(always)]
pub(crate) unsafe fn prim_fx_ashr<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
    b: Value<'gc>,
) -> PrimResult<'gc> {
    check_fx!(a, b);
    Ok(fx(fxv(a).wrapping_shr(fxv(b) as u32)))
}

#[inline(always)]
pub(crate) unsafe fn prim_fx_zero<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    check_fx!(a);
    Ok(Value::new(fxv(a) == 0))
}

#[inline(always)]
pub(crate) unsafe fn prim_fx_positive<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    check_fx!(a);
    Ok(Value::new(fxv(a) > 0))
}

#[inline(always)]
pub(crate) unsafe fn prim_fx_negative<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    check_fx!(a);
    Ok(Value::new(fxv(a) < 0))
}

#[inline(always)]
pub(crate) unsafe fn prim_fx_odd<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    check_fx!(a);
    Ok(Value::new(fxv(a) & 1 == 1))
}

#[inline(always)]
pub(crate) unsafe fn prim_fx_even<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    check_fx!(a);
    Ok(Value::new(fxv(a) & 1 == 0))
}

#[inline(always)]
pub(crate) unsafe fn prim_fx_min<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
    b: Value<'gc>,
) -> PrimResult<'gc> {
    check_fx!(a, b);
    Ok(fx(fxv(a).min(fxv(b))))
}

#[inline(always)]
pub(crate) unsafe fn prim_fx_max<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
    b: Value<'gc>,
) -> PrimResult<'gc> {
    check_fx!(a, b);
    Ok(fx(fxv(a).max(fxv(b))))
}

#[inline(always)]
pub(crate) unsafe fn prim_fl_add<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
    b: Value<'gc>,
) -> PrimResult<'gc> {
    check_fl!(a, b);
    Ok(fl(flv(a) + flv(b)))
}

#[inline(always)]
pub(crate) unsafe fn prim_fl_sub<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
    b: Value<'gc>,
) -> PrimResult<'gc> {
    check_fl!(a, b);
    Ok(fl(flv(a) - flv(b)))
}

#[inline(always)]
pub(crate) unsafe fn prim_fl_mul<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
    b: Value<'gc>,
) -> PrimResult<'gc> {
    check_fl!(a, b);
    Ok(fl(flv(a) * flv(b)))
}

#[inline(always)]
pub(crate) unsafe fn prim_fl_div<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
    b: Value<'gc>,
) -> PrimResult<'gc> {
    check_fl!(a, b);
    Ok(fl(flv(a) / flv(b)))
}

#[inline(always)]
pub(crate) unsafe fn prim_fl_lt<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
    b: Value<'gc>,
) -> PrimResult<'gc> {
    check_fl!(a, b);
    Ok(Value::new(flv(a) < flv(b)))
}

#[inline(always)]
pub(crate) unsafe fn prim_fl_le<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
    b: Value<'gc>,
) -> PrimResult<'gc> {
    check_fl!(a, b);
    Ok(Value::new(flv(a) <= flv(b)))
}

#[inline(always)]
pub(crate) unsafe fn prim_fl_gt<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
    b: Value<'gc>,
) -> PrimResult<'gc> {
    check_fl!(a, b);
    Ok(Value::new(flv(a) > flv(b)))
}

#[inline(always)]
pub(crate) unsafe fn prim_fl_ge<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
    b: Value<'gc>,
) -> PrimResult<'gc> {
    check_fl!(a, b);
    Ok(Value::new(flv(a) >= flv(b)))
}

#[inline(always)]
pub(crate) unsafe fn prim_fl_eq<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
    b: Value<'gc>,
) -> PrimResult<'gc> {
    check_fl!(a, b);
    Ok(Value::new(flv(a) == flv(b)))
}

#[inline(always)]
pub(crate) unsafe fn prim_fl_zero<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    check_fl!(a);
    Ok(Value::new(flv(a) == 0.0))
}

#[inline(always)]
pub(crate) unsafe fn prim_fl_positive<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    check_fl!(a);
    Ok(Value::new(flv(a) > 0.0))
}

#[inline(always)]
pub(crate) unsafe fn prim_fl_negative<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    check_fl!(a);
    Ok(Value::new(flv(a) < 0.0))
}

#[inline(always)]
pub(crate) unsafe fn prim_fl_nan<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    check_fl!(a);
    Ok(Value::new(flv(a).is_nan()))
}

#[inline(always)]
pub(crate) unsafe fn prim_fl_infinite<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    check_fl!(a);
    Ok(Value::new(flv(a).is_infinite()))
}

#[inline(always)]
pub(crate) unsafe fn prim_fl_finite<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    check_fl!(a);
    Ok(Value::new(flv(a).is_finite()))
}

#[inline(always)]
pub(crate) unsafe fn prim_fl_min<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
    b: Value<'gc>,
) -> PrimResult<'gc> {
    check_fl!(a, b);
    Ok(fl(fl_min(flv(a), flv(b))))
}

#[inline(always)]
pub(crate) unsafe fn prim_fl_max<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
    b: Value<'gc>,
) -> PrimResult<'gc> {
    check_fl!(a, b);
    Ok(fl(fl_max(flv(a), flv(b))))
}

#[inline(always)]
pub(crate) unsafe fn prim_fl_abs<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    check_fl!(a);
    Ok(fl(flv(a).abs()))
}

#[inline(always)]
pub(crate) unsafe fn prim_fl_floor<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    check_fl!(a);
    Ok(fl(flv(a).floor()))
}

#[inline(always)]
pub(crate) unsafe fn prim_fl_ceiling<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    check_fl!(a);
    Ok(fl(flv(a).ceil()))
}

#[inline(always)]
pub(crate) unsafe fn prim_fl_truncate<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    check_fl!(a);
    Ok(fl(flv(a).trunc()))
}

#[inline(always)]
pub(crate) unsafe fn prim_fl_round<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    check_fl!(a);
    Ok(fl(flv(a).round_ties_even()))
}

#[inline(always)]
pub(crate) unsafe fn prim_fl_sqrt<'gc>(
    _ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    check_fl!(a);
    Ok(fl(flv(a).sqrt()))
}

#[inline(always)]
pub(crate) unsafe fn prim_fl_sin<'gc>(ctx: MutationContext<'gc>, a: Value<'gc>) -> PrimResult<'gc> {
    check_fl!(a);
    Ok(thunks::sin(ctx, a))
}

#[inline(always)]
pub(crate) unsafe fn prim_fl_cos<'gc>(ctx: MutationContext<'gc>, a: Value<'gc>) -> PrimResult<'gc> {
    check_fl!(a);
    Ok(thunks::cos(ctx, a))
}

#[inline(always)]
pub(crate) unsafe fn prim_fl_tan<'gc>(ctx: MutationContext<'gc>, a: Value<'gc>) -> PrimResult<'gc> {
    check_fl!(a);
    Ok(thunks::tan(ctx, a))
}

#[inline(always)]
pub(crate) unsafe fn prim_fl_exp<'gc>(ctx: MutationContext<'gc>, a: Value<'gc>) -> PrimResult<'gc> {
    check_fl!(a);
    Ok(thunks::exp(ctx, a))
}

#[inline(always)]
pub(crate) unsafe fn prim_fl_log<'gc>(ctx: MutationContext<'gc>, a: Value<'gc>) -> PrimResult<'gc> {
    check_fl!(a);
    Ok(thunks::log(ctx, a))
}

#[inline(always)]
pub(crate) unsafe fn prim_fl_asin<'gc>(
    ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    check_fl!(a);
    Ok(thunks::asin(ctx, a))
}

#[inline(always)]
pub(crate) unsafe fn prim_fl_acos<'gc>(
    ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    check_fl!(a);
    Ok(thunks::acos(ctx, a))
}

#[inline(always)]
pub(crate) unsafe fn prim_fl_atan<'gc>(
    ctx: MutationContext<'gc>,
    a: Value<'gc>,
) -> PrimResult<'gc> {
    check_fl!(a);
    Ok(thunks::atan(ctx, a))
}

#[inline(always)]
pub fn add<'gc>(ctx: MutationContext<'gc>, x: Value<'gc>, y: Value<'gc>) -> Value<'gc> {
    if x.is_int32()
        && y.is_int32()
        && let Some(sum) = fxv(x).checked_add(fxv(y))
    {
        return fx(sum);
    }
    thunks::number_plus(ctx, x, y)
}

#[inline(always)]
pub fn sub<'gc>(ctx: MutationContext<'gc>, x: Value<'gc>, y: Value<'gc>) -> Value<'gc> {
    if x.is_int32()
        && y.is_int32()
        && let Some(difference) = fxv(x).checked_sub(fxv(y))
    {
        return fx(difference);
    }
    thunks::number_minus(ctx, x, y)
}

#[inline(always)]
pub fn mul<'gc>(ctx: MutationContext<'gc>, x: Value<'gc>, y: Value<'gc>) -> Value<'gc> {
    if x.is_int32()
        && y.is_int32()
        && let Some(product) = fxv(x).checked_mul(fxv(y))
    {
        return fx(product);
    }
    thunks::number_times(ctx, x, y)
}

type MutatorPointer = *const ();

pub(crate) type PrimArm =
    for<'gc> unsafe fn(MutationContext<'gc>, &[Value<'gc>]) -> PrimResult<'gc>;

pub(crate) type PrimWrapper =
    unsafe extern "C" fn(MutatorPointer, u64, u64, u64, u64) -> PrimDirectReturn;

pub(crate) fn resolve_prim_direct(id: u32) -> *const u8 {
    PRIM_WRAPPERS[id as usize] as *const u8
}

#[repr(C)]
pub(crate) struct PrimDirectReturn {
    pub value: u64,
    pub error: u64,
}

macro_rules! prim_entry_fixed0 {
    ($variant:ident, $snake:ident, $index:literal) => {
        paste::paste! {
            const _: () = {
                assert!(Primitive::$variant as u16 == $index);
            };

            #[inline(always)]
            unsafe fn [<prim_ $snake _adapter>]<'gc>(
                ctx: MutationContext<'gc>,
                a: &[Value<'gc>],
            ) -> PrimResult<'gc> {
                let _ = a;
                // SAFETY: see the arm's contract.
                unsafe { [<prim_ $snake>](ctx) }
            }

            pub(crate) unsafe extern "C" fn [<capy_prim_direct_ $snake>](
                th: MutatorPointer,
                argc: u64,
                _: u64,
                _: u64,
                _: u64,
            ) -> PrimDirectReturn {
                unsafe {
                    // A mismatch aborts like the interpreter's slice panic.
                    assert!(argc == 0);
                    let mutator = MutationContext::from_ptr(th);
                    let fp = mutator.tc().stack.fp.get();
                    slow::park_frame(mutator, fp);
                    count_call(Primitive::$variant);
                    match [<prim_ $snake>](mutator) {
                        Ok(value) => PrimDirectReturn {
                            value: value.bits(),
                            error: 0,
                        },
                        Err(PrimError { kind, irritant }) => {
                            mutator.state().exit_value.set(irritant);
                            PrimDirectReturn {
                                value: 0,
                                error: kind.code() as u64,
                            }
                        }
                    }
                }
            }
        }
    };
}

macro_rules! prim_entry_fixed1 {
    ($variant:ident, $snake:ident, $index:literal) => {
        paste::paste! {
            const _: () = {
                assert!(Primitive::$variant as u16 == $index);
            };

            #[inline(always)]
            unsafe fn [<prim_ $snake _adapter>]<'gc>(
                ctx: MutationContext<'gc>,
                a: &[Value<'gc>],
            ) -> PrimResult<'gc> {
                unsafe { [<prim_ $snake>](ctx, a[0]) }
            }

            pub(crate) unsafe extern "C" fn [<capy_prim_direct_ $snake>](
                th: MutatorPointer,
                argc: u64,
                a: u64,
                _: u64,
                _: u64,
            ) -> PrimDirectReturn {
                unsafe {
                    debug_assert!(argc == 1);
                    let mutator = MutationContext::from_ptr(th);
                    let fp = mutator.tc().stack.fp.get();
                    slow::park_frame(mutator, fp);
                    count_call(Primitive::$variant);
                    match [<prim_ $snake>](mutator, Value::from_raw(a)) {
                        Ok(value) => PrimDirectReturn {
                            value: value.bits(),
                            error: 0,
                        },
                        Err(PrimError { kind, irritant }) => {
                            mutator.state().exit_value.set(irritant);
                            PrimDirectReturn {
                                value: 0,
                                error: kind.code() as u64,
                            }
                        }
                    }
                }
            }
        }
    };
}

macro_rules! prim_entry_fixed2 {
    ($variant:ident, $snake:ident, $index:literal) => {
        paste::paste! {
            const _: () = {
                assert!(Primitive::$variant as u16 == $index);
            };

            #[inline(always)]
            unsafe fn [<prim_ $snake _adapter>]<'gc>(
                ctx: MutationContext<'gc>,
                a: &[Value<'gc>],
            ) -> PrimResult<'gc> {
                unsafe { [<prim_ $snake>](ctx, a[0], a[1]) }
            }

            pub(crate) unsafe extern "C" fn [<capy_prim_direct_ $snake>](
                th: MutatorPointer,
                argc: u64,
                a: u64,
                b: u64,
                _: u64,
            ) -> PrimDirectReturn {
                unsafe {
                    debug_assert!(argc == 2);
                    let mutator = MutationContext::from_ptr(th);
                    let fp = mutator.tc().stack.fp.get();
                    slow::park_frame(mutator, fp);
                    count_call(Primitive::$variant);
                    match [<prim_ $snake>](mutator, Value::from_raw(a), Value::from_raw(b)) {
                        Ok(value) => PrimDirectReturn {
                            value: value.bits(),
                            error: 0,
                        },
                        Err(PrimError { kind, irritant }) => {
                            mutator.state().exit_value.set(irritant);
                            PrimDirectReturn {
                                value: 0,
                                error: kind.code() as u64,
                            }
                        }
                    }
                }
            }
        }
    };
}

macro_rules! prim_entry_fixed3 {
    ($variant:ident, $snake:ident, $index:literal) => {
        paste::paste! {
            const _: () = {
                assert!(Primitive::$variant as u16 == $index);
            };

            #[inline(always)]
            unsafe fn [<prim_ $snake _adapter>]<'gc>(
                ctx: MutationContext<'gc>,
                a: &[Value<'gc>],
            ) -> PrimResult<'gc> {
                unsafe { [<prim_ $snake>](ctx, a[0], a[1], a[2]) }
            }

            pub(crate) unsafe extern "C" fn [<capy_prim_direct_ $snake>](
                th: MutatorPointer,
                argc: u64,
                a: u64,
                b: u64,
                c: u64,
            ) -> PrimDirectReturn {
                unsafe {
                    debug_assert!(argc == 3);
                    let mutator = MutationContext::from_ptr(th);
                    let fp = mutator.tc().stack.fp.get();
                    slow::park_frame(mutator, fp);
                    count_call(Primitive::$variant);
                    match [<prim_ $snake>](
                        mutator,
                        Value::from_raw(a),
                        Value::from_raw(b),
                        Value::from_raw(c),
                    ) {
                        Ok(value) => PrimDirectReturn {
                            value: value.bits(),
                            error: 0,
                        },
                        Err(PrimError { kind, irritant }) => {
                            mutator.state().exit_value.set(irritant);
                            PrimDirectReturn {
                                value: 0,
                                error: kind.code() as u64,
                            }
                        }
                    }
                }
            }
        }
    };
}

macro_rules! prim_entry_dynamic {
    ($variant:ident, $snake:ident, $index:literal) => {
        paste::paste! {
            const _: () = {
                assert!(Primitive::$variant as u16 == $index);
            };

            pub(crate) unsafe extern "C" fn [<capy_prim_direct_ $snake>](
                th: MutatorPointer,
                argc: u64,
                a: u64,
                b: u64,
                c: u64,
            ) -> PrimDirectReturn {
                unsafe {
                    debug_assert!(argc <= 3);
                    let mutator = MutationContext::from_ptr(th);
                    let fp = mutator.tc().stack.fp.get();
                    slow::park_frame(mutator, fp);
                    count_call(Primitive::$variant);
                    let args = [Value::from_raw(a), Value::from_raw(b), Value::from_raw(c)];
                    match [<prim_ $snake>](mutator, &args[..argc as usize]) {
                        Ok(value) => PrimDirectReturn {
                            value: value.bits(),
                            error: 0,
                        },
                        Err(PrimError { kind, irritant }) => {
                            mutator.state().exit_value.set(irritant);
                            PrimDirectReturn {
                                value: 0,
                                error: kind.code() as u64,
                            }
                        }
                    }
                }
            }
        }
    };
}

macro_rules! prim_entry_arm {
    ($variant:ident => $snake:ident($index:literal): ..) => {
        paste::paste! { [<prim_ $snake>] }
    };
    ($variant:ident => $snake:ident($index:literal): $arity:literal) => {
        paste::paste! { [<prim_ $snake _adapter>] }
    };
}

macro_rules! prim_entry_wrapper {
    ($variant:ident => $snake:ident($index:literal): $($rest:tt)*) => {
        paste::paste! { [<capy_prim_direct_ $snake>] }
    };
}

macro_rules! prim_entry {
    ($variant:ident => $snake:ident($index:literal): ..) => {
        prim_entry_dynamic!($variant, $snake, $index);
    };
    ($variant:ident => $snake:ident($index:literal): 0) => {
        prim_entry_fixed0!($variant, $snake, $index);
    };
    ($variant:ident => $snake:ident($index:literal): 1) => {
        prim_entry_fixed1!($variant, $snake, $index);
    };
    ($variant:ident => $snake:ident($index:literal): 2) => {
        prim_entry_fixed2!($variant, $snake, $index);
    };
    ($variant:ident => $snake:ident($index:literal): 3) => {
        prim_entry_fixed3!($variant, $snake, $index);
    };
}

macro_rules! prim_tables {
    ( $( ( $($entry:tt)* ) );* ) => {
        $( prim_entry!($($entry)*); )*
        /// Every primitive by id.
        pub(crate) const PRIM_ARMS: &[PrimArm] =
            &[ $( prim_entry_arm!($($entry)*) ),* ];
        /// Every prim by id, `extern "C"`: generated code indexes (template interpreter)
        /// or resolves (JIT) this.
        pub(crate) const PRIM_WRAPPERS: &[PrimWrapper] =
            &[ $( prim_entry_wrapper!($($entry)*) ),* ];
        const _: () = {
            assert!(PRIM_ARMS.len() == Primitive::ALL.len());
            assert!(PRIM_WRAPPERS.len() == Primitive::ALL.len());
        };
    };
}

prim_tables! {
    (ClassIdp => class_idp(0): 2);
    (Refptr => refptr(1): 2);
    (UsizeToValue => usize_to_value(2): 1);
    (CacheRef => cache_ref(3): 0);
    (CacheSet => cache_set(4): 0);
    (IsImmediate => is_immediate(5): 1);
    (IsHeapObject => is_heap_object(6): 1);
    (VariableBound => variable_bound(7): 1);
    (VariableRef => variable_ref(8): 1);
    (VariableSet => variable_set(9): 2);
    (MakeBox => make_box(10): 1);
    (EnsureLocalVar => ensure_local_var(11): 2);
    (Lookup => lookup(12): 2);
    (LookupBound => lookup_bound(13): 2);
    (LookupBoundPublic => lookup_bound_public(14): 2);
    (LookupBoundPrivate => lookup_bound_private(15): 2);
    (CurrentModule => current_module(16): ..);
    (Define => define(17): 2);
    (IsVariable => is_variable(18): 1);
    (SetCar => set_car(19): 2);
    (SetCdr => set_cdr(20): 2);
    (Cons => cons(21): 2);
    (Reverse => reverse(22): 1);
    (IsEofObject => is_eof_object(23): 1);
    (IsNull => is_null(24): 1);
    (Unspec => unspec(25): 0);
    (IsUnspecified => is_unspecified(26): 1);
    (IsPair => is_pair(27): 1);
    (IsList => is_list(28): 1);
    (Append => append(29): 2);
    (List => list(30): ..);
    (Memq => memq(31): 2);
    (Memv => memv(32): 2);
    (Car => car(33): 1);
    (Cdr => cdr(34): 1);
    (Length => length(35): 1);
    (IsVector => is_vector(36): 1);
    (IsBytevector => is_bytevector(37): 1);
    (Vector => vector(38): ..);
    (Tuple => tuple(39): ..);
    (VectorRef => vector_ref(40): 2);
    (VectorSet => vector_set(41): 3);
    (BytevectorLength => bytevector_length(42): 1);
    (BytevectorU8Ref => bytevector_u8_ref(43): 2);
    (TupleSize => tuple_size(44): 1);
    (TupleRef => tuple_ref(45): 2);
    (TupleSet => tuple_set(46): 3);
    (IsTuple => is_tuple(47): 1);
    (FxEq => fx_eq(48): ..);
    (Ash => ash(49): 2);
    (Logand => logand(50): 2);
    (Logior => logior(51): 2);
    (Lognot => lognot(52): 1);
    (Expt => expt(53): 2);
    (Abs => abs(54): 1);
    (Sqrt => sqrt(55): 1);
    (Cos => cos(56): 1);
    (Sin => sin(57): 1);
    (Tan => tan(58): 1);
    (Atan => atan(59): ..);
    (Asin => asin(60): 1);
    (Acos => acos(61): 1);
    (Ceiling => ceiling(62): 1);
    (Floor => floor(63): 1);
    (Truncate => truncate(64): 1);
    (Plus => plus(65): ..);
    (Minus => minus(66): ..);
    (Times => times(67): ..);
    (Div => div(68): 2);
    (NumericEqual => numeric_equal(69): ..);
    (NumericLt => numeric_lt(70): ..);
    (NumericGt => numeric_gt(71): ..);
    (NumericGte => numeric_gte(72): ..);
    (NumericLte => numeric_lte(73): ..);
    (ExactToInexact => exact_to_inexact(74): 1);
    (InexactToExact => inexact_to_exact(75): 1);
    (IsEven => is_even(76): 1);
    (IsOdd => is_odd(77): 1);
    (IsZero => is_zero(78): ..);
    (Quotient => quotient(79): 2);
    (Remainder => remainder(80): 2);
    (Modulo => modulo(81): 2);
    (IsProcedure => is_procedure(82): 1);
    (IsString => is_string(83): 1);
    (IsBoolean => is_boolean(84): 1);
    (IsSymbol => is_symbol(85): 1);
    (IsEq => is_eq(86): 2);
    (IsEqv => is_eqv(87): 2);
    (IsEqual => is_equal(88): 2);
    (IsExactInteger => is_exact_integer(89): 1);
    (IsInteger => is_integer(90): 1);
    (IsChar => is_char(91): 1);
    (IsNumber => is_number(92): 1);
    (IsComplex => is_complex(93): 1);
    (IsNan => is_nan(94): 1);
    (IsReal => is_real(95): 1);
    (IsRational => is_rational(96): 1);
    (IsInexact => is_inexact(97): 1);
    (IsExact => is_exact(98): 1);
    (Not => not(99): 1);
    (MakeTuple => make_tuple(100): ..);
    (MakeVector => make_vector(101): ..);
    (StringLength => string_length(102): 1);
    (StringRef => string_ref(103): 2);
    (IntegerToChar => integer_to_char(104): 1);
    (CharToInteger => char_to_integer(105): 1);
    (Breakpoint => breakpoint(106): 0);
    (SymbolToString => symbol_to_string(107): 1);
    (StringToSymbol => string_to_symbol(108): 1);
    (PushCframe => push_cframe(109): 0);
    (CurrentContinuationMarks => current_continuation_marks(110): 0);
    (SetAttachments => set_attachments(111): 1);
    (Winders => winders(112): ..);
    (MakeSyntax => make_syntax(113): ..);
    (DefaultRetk => default_retk(114): 0);
    (IsFixnum => is_fixnum(115): 1);
    (IsFlonum => is_flonum(116): 1);
    (FxAdd => fx_add(117): 2);
    (FxSub => fx_sub(118): 2);
    (FxMul => fx_mul(119): 2);
    (FxAddOvf => fx_add_ovf(120): 2);
    (FxSubOvf => fx_sub_ovf(121): 2);
    (FxMulOvf => fx_mul_ovf(122): 2);
    (FxLt => fx_lt(123): 2);
    (FxLe => fx_le(124): 2);
    (FxGt => fx_gt(125): 2);
    (FxGe => fx_ge(126): 2);
    (FxAnd => fx_and(127): 2);
    (FxIor => fx_ior(128): 2);
    (FxXor => fx_xor(129): 2);
    (FxNot => fx_not(130): 1);
    (FxAshl => fx_ashl(131): 2);
    (FxAshr => fx_ashr(132): 2);
    (FxZero => fx_zero(133): 1);
    (FxPositive => fx_positive(134): 1);
    (FxNegative => fx_negative(135): 1);
    (FxOdd => fx_odd(136): 1);
    (FxEven => fx_even(137): 1);
    (FxMin => fx_min(138): 2);
    (FxMax => fx_max(139): 2);
    (FlAdd => fl_add(140): 2);
    (FlSub => fl_sub(141): 2);
    (FlMul => fl_mul(142): 2);
    (FlDiv => fl_div(143): 2);
    (FlLt => fl_lt(144): 2);
    (FlLe => fl_le(145): 2);
    (FlGt => fl_gt(146): 2);
    (FlGe => fl_ge(147): 2);
    (FlEq => fl_eq(148): 2);
    (FlZero => fl_zero(149): 1);
    (FlPositive => fl_positive(150): 1);
    (FlNegative => fl_negative(151): 1);
    (FlNan => fl_nan(152): 1);
    (FlInfinite => fl_infinite(153): 1);
    (FlFinite => fl_finite(154): 1);
    (FlMin => fl_min(155): 2);
    (FlMax => fl_max(156): 2);
    (FlAbs => fl_abs(157): 1);
    (FlFloor => fl_floor(158): 1);
    (FlCeiling => fl_ceiling(159): 1);
    (FlTruncate => fl_truncate(160): 1);
    (FlRound => fl_round(161): 1);
    (FlSin => fl_sin(162): 1);
    (FlCos => fl_cos(163): 1);
    (FlTan => fl_tan(164): 1);
    (FlExp => fl_exp(165): 1);
    (FlLog => fl_log(166): 1);
    (FlAsin => fl_asin(167): 1);
    (FlAcos => fl_acos(168): 1);
    (FlSqrt => fl_sqrt(169): 1);
    (FlAtan => fl_atan(170): 1);
    (SlotRef => slot_ref(171): 0);
    (SlotSet => slot_set(172): 0);
    (AttachmentPush => attachment_push(173): 1);
    (AttachmentPop => attachment_pop(174): 0);
    (AttachmentSet => attachment_set(175): 1);
    (AttachmentConsume => attachment_consume(176): 0);
    (AttachmentConsumeReify => attachment_consume_reify(177): 1);
    (UpdateMark => update_mark(178): 3)
}
