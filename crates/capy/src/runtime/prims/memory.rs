//! Typed memory primitives

use super::calls::PrimResult;
use crate::runtime::MutationContext;
use crate::runtime::value::{ByteVector, IntoValue, Str, Symbol, Value};
use crate::runtime::vm::ffi::{Pointer, exact_integer_to_i128, real_argument};
use crate::runtime::vm::thunk_raise;
use crate::runtime::vm::thunks::make_assertion_violation;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Elem {
    U8,
    S8,
    U16,
    S16,
    U32,
    S32,
    U64,
    S64,
    F32,
    F64,
}

impl Elem {
    pub(crate) const fn size(self) -> usize {
        match self {
            Elem::U8 | Elem::S8 => 1,
            Elem::U16 | Elem::S16 => 2,
            Elem::U32 | Elem::S32 | Elem::F32 => 4,
            Elem::U64 | Elem::S64 | Elem::F64 => 8,
        }
    }

    /// Required alignment of a bytevector index. Only floats insist on one.
    const fn index_alignment(self) -> usize {
        match self {
            Elem::F32 | Elem::F64 => self.size(),
            _ => 1,
        }
    }

    /// Inclusive range of the integers the element holds, or `None` for floats.
    const fn range(self) -> Option<(i128, i128)> {
        match self {
            Elem::U8 => Some((0, u8::MAX as i128)),
            Elem::S8 => Some((i8::MIN as i128, i8::MAX as i128)),
            Elem::U16 => Some((0, u16::MAX as i128)),
            Elem::S16 => Some((i16::MIN as i128, i16::MAX as i128)),
            Elem::U32 => Some((0, u32::MAX as i128)),
            Elem::S32 => Some((i32::MIN as i128, i32::MAX as i128)),
            Elem::U64 => Some((0, u64::MAX as i128)),
            Elem::S64 => Some((i64::MIN as i128, i64::MAX as i128)),
            Elem::F32 | Elem::F64 => None,
        }
    }
}

fn raise<'gc>(ctx: MutationContext<'gc>, who: &str, message: &str, irritants: &[Value<'gc>]) -> ! {
    thunk_raise(
        ctx,
        make_assertion_violation(
            ctx,
            Symbol::from_str(ctx, who).into(),
            Str::new(ctx, message, true).into(),
            irritants,
        ),
    )
}

/// Address of the element at `index` of `bv`, after checking type, bounds and alignment.
fn bytevector_address<'gc>(
    ctx: MutationContext<'gc>,
    who: &str,
    elem: Elem,
    bv: Value<'gc>,
    index: Value<'gc>,
) -> *mut u8 {
    let Some(bytes) = bv.try_as::<ByteVector>() else {
        raise(ctx, who, "not a bytevector", &[bv]);
    };
    let Some(i) = index.int32() else {
        raise(ctx, who, "not a fixnum", &[index]);
    };
    if i < 0 || (i as usize).saturating_add(elem.size()) > bytes.len() {
        raise(ctx, who, "index out of bounds", &[bv, index]);
    }
    let i = i as usize;
    if i % elem.index_alignment() != 0 {
        raise(ctx, who, "unaligned access", &[bv, index]);
    }
    // SAFETY: `i + size <= len`, so the element lies inside the bytevector's storage.
    unsafe { bytes.contents().to_mut_ptr::<u8>().add(i) }
}

/// Address of `pointer + offset`, after checking for NULL and wraparound. The caller vouches that the
/// address is valid memory, as with `pointer-ref`.
fn pointer_address<'gc>(
    ctx: MutationContext<'gc>,
    who: &str,
    pointer: Value<'gc>,
    offset: Value<'gc>,
) -> *mut u8 {
    if !pointer.is::<Pointer>() {
        raise(ctx, who, "not a pointer", &[pointer]);
    }
    let base = pointer.downcast::<Pointer>().value() as usize;
    if base == 0 {
        raise(ctx, who, "null pointer", &[pointer]);
    }
    let offset_value = offset;
    let Some(offset) = offset.int32().filter(|n| *n >= 0) else {
        raise(ctx, who, "not a non-negative fixnum", &[offset_value]);
    };
    match base.checked_add(offset as usize) {
        Some(at) => at as *mut u8,
        None => raise(
            ctx,
            who,
            "offset overflows the address space",
            &[pointer, offset_value],
        ),
    }
}

/// Reads one element at `at`.
///
/// # Safety
/// `at` must address `elem.size()` readable bytes.
unsafe fn load<'gc>(ctx: MutationContext<'gc>, elem: Elem, at: *const u8) -> Value<'gc> {
    // SAFETY: the caller guarantees the bytes are readable
    unsafe {
        match elem {
            Elem::U8 => Value::new(at.read() as i32),
            Elem::S8 => Value::new((at as *const i8).read() as i32),
            Elem::U16 => Value::new((at as *const u16).read_unaligned() as i32),
            Elem::S16 => Value::new((at as *const i16).read_unaligned() as i32),
            Elem::U32 => (at as *const u32).read_unaligned().into_value(ctx),
            Elem::S32 => Value::new((at as *const i32).read_unaligned()),
            Elem::U64 => (at as *const u64).read_unaligned().into_value(ctx),
            Elem::S64 => (at as *const i64).read_unaligned().into_value(ctx),
            Elem::F32 => Value::new((at as *const f32).read_unaligned() as f64),
            Elem::F64 => Value::new((at as *const f64).read_unaligned()),
        }
    }
}

/// Writes `value` as one element at `at`, raising when it does not fit.
///
/// # Safety
/// `at` must address `elem.size()` writable bytes.
unsafe fn store<'gc>(
    ctx: MutationContext<'gc>,
    who: &str,
    elem: Elem,
    at: *mut u8,
    value: Value<'gc>,
) {
    if let Some((min, max)) = elem.range() {
        let Some(n) = value
            .number()
            .and_then(exact_integer_to_i128)
            .filter(|n| (min..=max).contains(n))
        else {
            raise(ctx, who, "integer out of range for element type", &[value]);
        };
        // SAFETY: the caller guarantees the bytes are writable and `n` fits the element.
        unsafe {
            match elem.size() {
                1 => at.write(n as u8),
                2 => (at as *mut u16).write_unaligned(n as u16),
                4 => (at as *mut u32).write_unaligned(n as u32),
                _ => (at as *mut u64).write_unaligned(n as u64),
            }
        }
    } else {
        let Some(x) = real_argument(ctx, value) else {
            raise(ctx, who, "expected a real number", &[value]);
        };
        // SAFETY: as above.
        unsafe {
            if elem == Elem::F32 {
                (at as *mut f32).write_unaligned(x as f32);
            } else {
                (at as *mut f64).write_unaligned(x);
            }
        }
    }
}

pub(crate) fn bytevector_ref<'gc>(
    ctx: MutationContext<'gc>,
    who: &str,
    elem: Elem,
    bv: Value<'gc>,
    index: Value<'gc>,
) -> Value<'gc> {
    let at = bytevector_address(ctx, who, elem, bv, index);
    // SAFETY: `bytevector_address` checked the bounds.
    unsafe { load(ctx, elem, at) }
}

pub(crate) fn bytevector_set<'gc>(
    ctx: MutationContext<'gc>,
    who: &str,
    elem: Elem,
    bv: Value<'gc>,
    index: Value<'gc>,
    value: Value<'gc>,
) -> Value<'gc> {
    let at = bytevector_address(ctx, who, elem, bv, index);
    // SAFETY: `bytevector_address` checked the bounds.
    unsafe { store(ctx, who, elem, at, value) };
    Value::undefined()
}

pub(crate) fn pointer_ref<'gc>(
    ctx: MutationContext<'gc>,
    who: &str,
    elem: Elem,
    pointer: Value<'gc>,
    offset: Value<'gc>,
) -> Value<'gc> {
    let at = pointer_address(ctx, who, pointer, offset);
    // SAFETY: the caller of the Scheme procedure vouches for the address, as with `pointer-ref`.
    unsafe { load(ctx, elem, at) }
}

pub(crate) fn pointer_set<'gc>(
    ctx: MutationContext<'gc>,
    who: &str,
    elem: Elem,
    pointer: Value<'gc>,
    offset: Value<'gc>,
    value: Value<'gc>,
) -> Value<'gc> {
    let at = pointer_address(ctx, who, pointer, offset);
    // SAFETY: see `pointer_ref`.
    unsafe { store(ctx, who, elem, at, value) };
    Value::undefined()
}

macro_rules! bv_ref_prim {
    ($name:ident, $who:literal, $elem:expr) => {
        #[inline(always)]
        pub(crate) unsafe fn $name<'gc>(
            ctx: MutationContext<'gc>,
            bv: Value<'gc>,
            index: Value<'gc>,
        ) -> PrimResult<'gc> {
            Ok(bytevector_ref(ctx, $who, $elem, bv, index))
        }
    };
}

macro_rules! bv_set_prim {
    ($name:ident, $who:literal, $elem:expr) => {
        #[inline(always)]
        pub(crate) unsafe fn $name<'gc>(
            ctx: MutationContext<'gc>,
            bv: Value<'gc>,
            index: Value<'gc>,
            value: Value<'gc>,
        ) -> PrimResult<'gc> {
            Ok(bytevector_set(ctx, $who, $elem, bv, index, value))
        }
    };
}

macro_rules! ptr_ref_prim {
    ($name:ident, $who:literal, $elem:expr) => {
        #[inline(always)]
        pub(crate) unsafe fn $name<'gc>(
            ctx: MutationContext<'gc>,
            pointer: Value<'gc>,
            offset: Value<'gc>,
        ) -> PrimResult<'gc> {
            Ok(pointer_ref(ctx, $who, $elem, pointer, offset))
        }
    };
}

macro_rules! ptr_set_prim {
    ($name:ident, $who:literal, $elem:expr) => {
        #[inline(always)]
        pub(crate) unsafe fn $name<'gc>(
            ctx: MutationContext<'gc>,
            pointer: Value<'gc>,
            offset: Value<'gc>,
            value: Value<'gc>,
        ) -> PrimResult<'gc> {
            Ok(pointer_set(ctx, $who, $elem, pointer, offset, value))
        }
    };
}

bv_set_prim!(prim_bv_u8_set, "bytevector-u8-set!", Elem::U8);
bv_ref_prim!(prim_bv_s8_ref, "bytevector-s8-ref", Elem::S8);
bv_set_prim!(prim_bv_s8_set, "bytevector-s8-set!", Elem::S8);
bv_ref_prim!(prim_bv_u16_ref, "bytevector-u16-native-ref", Elem::U16);
bv_set_prim!(prim_bv_u16_set, "bytevector-u16-native-set!", Elem::U16);
bv_ref_prim!(prim_bv_s16_ref, "bytevector-s16-native-ref", Elem::S16);
bv_set_prim!(prim_bv_s16_set, "bytevector-s16-native-set!", Elem::S16);
bv_ref_prim!(prim_bv_u32_ref, "bytevector-u32-native-ref", Elem::U32);
bv_set_prim!(prim_bv_u32_set, "bytevector-u32-native-set!", Elem::U32);
bv_ref_prim!(prim_bv_s32_ref, "bytevector-s32-native-ref", Elem::S32);
bv_set_prim!(prim_bv_s32_set, "bytevector-s32-native-set!", Elem::S32);
bv_ref_prim!(prim_bv_u64_ref, "bytevector-u64-native-ref", Elem::U64);
bv_set_prim!(prim_bv_u64_set, "bytevector-u64-native-set!", Elem::U64);
bv_ref_prim!(prim_bv_s64_ref, "bytevector-s64-native-ref", Elem::S64);
bv_set_prim!(prim_bv_s64_set, "bytevector-s64-native-set!", Elem::S64);
bv_ref_prim!(
    prim_bv_f32_ref,
    "bytevector-ieee-single-native-ref",
    Elem::F32
);
bv_set_prim!(
    prim_bv_f32_set,
    "bytevector-ieee-single-native-set!",
    Elem::F32
);
bv_ref_prim!(
    prim_bv_f64_ref,
    "bytevector-ieee-double-native-ref",
    Elem::F64
);
bv_set_prim!(
    prim_bv_f64_set,
    "bytevector-ieee-double-native-set!",
    Elem::F64
);
ptr_ref_prim!(prim_ptr_u8_ref, "pointer-ref/u8", Elem::U8);
ptr_set_prim!(prim_ptr_u8_set, "pointer-set!/u8", Elem::U8);
ptr_ref_prim!(prim_ptr_s8_ref, "pointer-ref/s8", Elem::S8);
ptr_set_prim!(prim_ptr_s8_set, "pointer-set!/s8", Elem::S8);
ptr_ref_prim!(prim_ptr_u16_ref, "pointer-ref/u16", Elem::U16);
ptr_set_prim!(prim_ptr_u16_set, "pointer-set!/u16", Elem::U16);
ptr_ref_prim!(prim_ptr_s16_ref, "pointer-ref/s16", Elem::S16);
ptr_set_prim!(prim_ptr_s16_set, "pointer-set!/s16", Elem::S16);
ptr_ref_prim!(prim_ptr_u32_ref, "pointer-ref/u32", Elem::U32);
ptr_set_prim!(prim_ptr_u32_set, "pointer-set!/u32", Elem::U32);
ptr_ref_prim!(prim_ptr_s32_ref, "pointer-ref/s32", Elem::S32);
ptr_set_prim!(prim_ptr_s32_set, "pointer-set!/s32", Elem::S32);
ptr_ref_prim!(prim_ptr_u64_ref, "pointer-ref/u64", Elem::U64);
ptr_set_prim!(prim_ptr_u64_set, "pointer-set!/u64", Elem::U64);
ptr_ref_prim!(prim_ptr_s64_ref, "pointer-ref/s64", Elem::S64);
ptr_set_prim!(prim_ptr_s64_set, "pointer-set!/s64", Elem::S64);
ptr_ref_prim!(prim_ptr_f32_ref, "pointer-ref/f32", Elem::F32);
ptr_set_prim!(prim_ptr_f32_set, "pointer-set!/f32", Elem::F32);
ptr_ref_prim!(prim_ptr_f64_ref, "pointer-ref/f64", Elem::F64);
ptr_set_prim!(prim_ptr_f64_set, "pointer-set!/f64", Elem::F64);
