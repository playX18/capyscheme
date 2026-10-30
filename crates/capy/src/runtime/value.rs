//! Runtime representation and helpers for Scheme values.
//!
//! The core [`Value`] type stores immediate values and GC object references in a
//! NaN-boxed word. Submodules provide concrete heap object implementations and
//! operations for lists, strings, numbers, vectors, hash tables, and weak values.

use crate::heap::{
    Gc, ObjectSlot, Trace,
    barrier::Write,
    mmtk::{util::Address, vm::SlotVisitor},
    object::{ClassId, GcObject, HeapObjectHeader, builtin_class_ids},
};
use crate::runtime::MutationContext;
use crate::runtime::class::{ClassDescriptor, class_table, hash_primitive_value};
use std::{fmt, hash::Hash, marker::PhantomData};

/// A Scheme value.
///
/// Uses NaN boxing layout based on JavaScriptCore NaN boxing, we formally prove
/// the correctness of this encoding in tests/z3_value_encoding_proof.py.
///
/// Floating-point NaN payloads are canonicalized by [`Value::from_f64`] so NaN
/// values round-trip through the value encoding as [`pure_nan::PURE_NAN_BITS`].
///
/// Heap objects store their tags in the GC header that lives at `OBJECT_REF_OFFSET`
/// bytes before the payload. Some objects still use their first payload word for
/// per-instance metadata such as variable lengths.
#[derive(Clone, Copy)]
#[repr(C)]
pub struct Value<'gc> {
    pub(crate) desc: EncodedValueDescriptor,
    pub(crate) pd: PhantomData<&'gc ()>,
}

/// Raw storage for a NaN-boxed [`Value`].
///
/// The active field depends on the value kind and is interpreted only through
/// `Value` constructors and accessors.
#[derive(Clone, Copy)]
#[repr(C)]
pub union EncodedValueDescriptor {
    pub as_i64: i64,
    pub as_u64: u64,
    pub as_f64: f64,
    pub ptr: *mut (),
}

impl EncodedValueDescriptor {
    // SAFETY: Caller must ensure preconditions are met (see fn docs)
    pub(crate) unsafe fn ptr(self) -> GcObject {
        // SAFETY: `*mut ()` and `GcObject` have the same layout (both pointer-sized).
        // Caller must ensure the value is a cell (i.e. `is_cell()` is true) so that `self.ptr`
        // contains a valid GC-managed pointer, not a NaN-boxed integer/float/tag.
        unsafe { std::mem::transmute(self.ptr) }
    }
}

impl<'gc> Eq for Value<'gc> {}
impl<'gc> PartialEq for Value<'gc> {
    fn eq(&self, other: &Self) -> bool {
        self.raw_i64() == other.raw_i64()
    }
}

impl<'gc> Hash for Value<'gc> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        if self.is_cell() {
            if let Some(hash) = hash_primitive_value(*self) {
                state.write_u64(hash);
                return;
            }

            // SAFETY: The value descriptor contains a valid GC object pointer
            unsafe {
                // SAFETY: `is_cell()` guard above ensures `desc.ptr` is a valid GC object pointer.
                let obj = self.desc.ptr();
                state.write_u64(obj.hashcode());
            }
        } else {
            state.write_i64(self.raw_i64());
        }
    }
}

impl<'gc> Value<'gc> {
    /// Constructs a value from its signed raw word.
    pub const fn from_raw_i64(raw: i64) -> Self {
        Self {
            desc: EncodedValueDescriptor { as_i64: raw },
            pd: PhantomData,
        }
    }

    /// Constructs a value from its raw bits.
    pub const fn from_raw(bits: u64) -> Self {
        Self {
            desc: EncodedValueDescriptor { as_u64: bits },
            pd: PhantomData,
        }
    }

    /// Returns the signed raw word backing this value.
    pub const fn raw_i64(self) -> i64 {
        // SAFETY: NaN-boxed value bits are valid to read as raw i64/u64
        unsafe { self.desc.as_i64 }
    }

    /// Returns the raw bits backing this value.
    pub const fn bits(self) -> u64 {
        // SAFETY: NaN-boxed value bits are valid to read as raw i64/u64
        unsafe { self.desc.as_u64 }
    }
}

// SAFETY: Value's Trace impl correctly identifies GC-managed pointers via `is_cell()`.
// Non-cell values (ints, floats, tags) are ignored, cell values point into the GC heap
// and `desc.ptr` is at a stable address suitable for `ObjectSlot` construction.
unsafe impl<'gc> Trace for Value<'gc> {
    // SAFETY: All GC-reachable fields are traced via `visitor`
    unsafe fn trace(&mut self, visitor: &mut crate::heap::collection::Visitor) {
        // SAFETY: Preconditions verified by the surrounding code
        unsafe {
            if self.is_cell() && !self.is_empty() {
                // SAFETY: `is_cell() && !is_empty()` ensures `desc.ptr` holds a live GC pointer.
                // `&mut self.desc.ptr` is a stable reference to the slot the GC may update.
                visitor.visit_slot(ObjectSlot::from_address(Address::from_mut_ptr(
                    &mut self.desc.ptr,
                )));
            }
        }
    }

    // SAFETY: Weak refs are processed through the given weak_processor
    unsafe fn process_weak_refs(&mut self, _weak_processor: &mut crate::heap::WeakProcessor) {}
}

/// Constants used by the NaN-boxed floating-point encoding.
pub mod pure_nan {
    pub const PURE_NAN_BITS: u64 = 0x7ff8000000000000;
    pub const PURE_NAN: f64 = f64::from_bits(PURE_NAN_BITS);

    pub const IMPURE_NAN_BITS: u64 = 0xffff000000000000;
    pub const IMPURE_NAN: f64 = f64::from_bits(IMPURE_NAN_BITS);

    pub const VALUE_DOUBLE_ENCODE_OFFSET_BIT: u32 = 49;
    pub const VALUE_DOUBLE_ENCODE_OFFSET: u64 = 1 << VALUE_DOUBLE_ENCODE_OFFSET_BIT;
    pub const VALUE_NUMBER_TAG: u64 = 0xfffe000000000000;
}

#[cfg(target_pointer_width = "64")]
impl<'gc> Value<'gc> {
    pub const DOUBLE_ENCODE_OFFSET_BIT: u32 = pure_nan::VALUE_DOUBLE_ENCODE_OFFSET_BIT;
    pub const DOUBLE_ENCODE_OFFSET: u64 = pure_nan::VALUE_DOUBLE_ENCODE_OFFSET;

    pub const NUMBER_TAG: i64 = 0xfffe000000000000u64 as i64;
    pub const LOWEST_OF_HIGH_BITS: i64 = 1 << 49;

    pub const OTHER_TAG: i64 = 0x2;
    pub const BOOL_TAG: i64 = 0x4;
    pub const UNDEFINED_TAG: i64 = 0x8;

    pub const CHAR_TAG: i64 = 0x82;
    pub const CHAR_MASK: i64 = Self::NUMBER_TAG | Self::CHAR_TAG;

    pub const VALUE_FALSE: i64 = Self::OTHER_TAG | Self::BOOL_TAG;
    pub const VALUE_TRUE: i64 = Self::OTHER_TAG | Self::BOOL_TAG | 0x1;
    pub const VALUE_UNDEFINED: i64 = Self::OTHER_TAG | Self::UNDEFINED_TAG;
    pub const VALUE_NULL: i64 = Self::OTHER_TAG;

    pub const MISC_TAG: i64 = Self::OTHER_TAG | Self::BOOL_TAG | Self::UNDEFINED_TAG;

    pub const NOT_CELL_MASK: i64 = Self::NUMBER_TAG | Self::OTHER_TAG;

    pub const VALUE_EMPTY: i64 = 0x0;
    pub const VALUE_DELETED: i64 = 0x4;

    pub const VALUE_VOID: i64 = Self::MISC_TAG | 0x10;
    pub const VALUE_UNSPECIFIED: i64 = Self::MISC_TAG | 0x20;
    pub const VALUE_EOF: i64 = Self::MISC_TAG | 0x30;
    pub const VALUE_BWP: i64 = Self::MISC_TAG | 0x40;
    /// The continuation of the native call in progress, see [`Value::current_k`].
    pub const VALUE_CURRENT_K: i64 = Self::MISC_TAG | 0x50;

    /// Constructs a character value.
    pub fn from_char(c: char) -> Self {
        let raw = (c as u32 as i64) << 16 | Self::CHAR_TAG;
        Self::from_raw_i64(raw)
    }

    /// Constructs a 32-bit fixnum value.
    pub fn from_i32(i: i32) -> Self {
        let raw = (i as i64) | Self::NUMBER_TAG;
        Self::from_raw_i64(raw)
    }

    /// Constructs a flonum value, canonicalizing all NaN payloads.
    pub fn from_f64(f: f64) -> Self {
        let bits = if f.is_nan() {
            pure_nan::PURE_NAN_BITS
        } else {
            f.to_bits()
        };
        Self::from_raw_i64((bits as i64).wrapping_add(Self::DOUBLE_ENCODE_OFFSET as i64))
    }

    /// Constructs a boolean value.
    pub fn from_bool(b: bool) -> Self {
        if b {
            Self::from_raw_i64(Self::VALUE_TRUE)
        } else {
            Self::from_raw_i64(Self::VALUE_FALSE)
        }
    }

    /// Constructs the internal undefined sentinel.
    pub fn undefined() -> Self {
        Self::from_raw_i64(Self::VALUE_UNDEFINED)
    }

    /// Constructs Scheme's void value.
    pub fn void() -> Self {
        Self::from_raw_i64(Self::VALUE_VOID)
    }

    /// Constructs the unspecified result value.
    pub fn unspecified() -> Self {
        Self::from_raw_i64(Self::VALUE_UNSPECIFIED)
    }

    /// Constructs the end-of-file value.
    pub fn eof() -> Self {
        Self::from_raw_i64(Self::VALUE_EOF)
    }

    /// Constructs the broken weak pointer value.
    pub fn bwp() -> Self {
        Self::from_raw_i64(Self::VALUE_BWP)
    }

    /// Constructs the internal empty slot sentinel.
    pub const fn empty() -> Self {
        Self::from_raw_i64(Self::VALUE_EMPTY)
    }

    /// Constructs the internal deleted slot sentinel.
    pub fn deleted() -> Self {
        Self::from_raw_i64(Self::VALUE_DELETED)
    }

    /// The token a native procedure receives as its return continuation.
    ///
    /// It stands for "whatever the frame of this call returns to" and is only
    /// meaningful while that frame is the top of the stack: passing it back
    /// as the continuation of a call makes that call a tail call, and
    /// continuing to it returns values from the frame.
    pub fn current_k() -> Self {
        Self::from_raw_i64(Self::VALUE_CURRENT_K)
    }

    pub fn is_current_k(self) -> bool {
        self.raw_i64() == Self::VALUE_CURRENT_K
    }

    pub fn is_void(self) -> bool {
        self.raw_i64() == Self::VALUE_VOID
    }

    pub fn is_unspecified(self) -> bool {
        self.raw_i64() == Self::VALUE_UNSPECIFIED
    }

    pub fn is_eof(self) -> bool {
        self.raw_i64() == Self::VALUE_EOF
    }

    pub fn is_bwp(self) -> bool {
        self.raw_i64() == Self::VALUE_BWP
    }

    pub fn is_deleted(self) -> bool {
        self.raw_i64() == Self::VALUE_DELETED
    }

    pub fn is_empty(self) -> bool {
        self.raw_i64() == Self::VALUE_EMPTY
    }

    pub fn is_cell(self) -> bool {
        self.raw_i64() & Self::NOT_CELL_MASK == 0 && self.raw_i64() != Self::VALUE_EMPTY
    }

    pub fn is_int32(self) -> bool {
        self.raw_i64() & Self::NUMBER_TAG == Self::NUMBER_TAG
    }

    pub fn is_inline_number(self) -> bool {
        self.raw_i64() & Self::NUMBER_TAG != 0
    }

    pub fn is_flonum(self) -> bool {
        self.is_inline_number() && !self.is_int32()
    }

    pub fn is_char(self) -> bool {
        self.raw_i64() & Self::CHAR_MASK == Self::CHAR_TAG
            && char::from_u32((self.raw_i64() >> 16) as u32).is_some()
    }

    pub fn char(self) -> char {
        debug_assert!(self.is_char());
        char::from_u32((self.raw_i64() >> 16) as u32)
            .expect("char value must contain a valid Unicode scalar")
    }

    pub fn is_null(self) -> bool {
        self.raw_i64() == Self::VALUE_NULL
    }

    /// Constructs the empty list.
    pub fn null() -> Self {
        Self::from_raw_i64(Self::VALUE_NULL)
    }

    pub fn is_undefined_or_null(self) -> bool {
        self.raw_i64() & !Self::UNDEFINED_TAG == Self::VALUE_NULL
    }

    pub fn as_int32(self) -> i32 {
        debug_assert!(self.is_int32(), "not an int32: {}", self);
        self.raw_i64() as i32
    }

    pub fn int32(self) -> Option<i32> {
        if self.is_int32() {
            Some(self.as_int32())
        } else {
            None
        }
    }

    pub fn as_flonum(self) -> f64 {
        debug_assert!(self.is_flonum());
        let bits = (self.raw_i64() as u64).wrapping_sub(Self::DOUBLE_ENCODE_OFFSET);
        f64::from_bits(bits)
    }

    pub fn flonum(self) -> Option<f64> {
        if self.is_flonum() {
            Some(self.as_flonum())
        } else {
            None
        }
    }

    pub fn as_bool(self) -> bool {
        self.raw_i64() != Value::VALUE_FALSE
    }

    pub fn is_bool(self) -> bool {
        self.raw_i64() & !1 == Value::VALUE_FALSE
    }

    pub fn as_cell_raw(self) -> GcObject {
        // SAFETY: Caller must ensure the value is a cell. The NaN-boxing encoding guarantees
        // the low bits of a cell value form a valid pointer into the GC heap.
        unsafe { self.desc.ptr() }
    }
}

impl<'gc> Value<'gc> {
    /// Converts any supported Rust value into a Scheme value.
    pub fn new(x: impl Into<Self>) -> Self {
        x.into()
    }

    pub fn header(&self) -> &'gc HeapObjectHeader {
        debug_assert!(self.is_cell(), "Value must be a cell to have a header");
        let obj = self.as_cell_raw();
        obj.header()
    }

    #[inline]
    pub fn class_id(&self) -> Option<ClassId> {
        if self.is_empty() || self.is_deleted() || self.is_bwp() {
            return None;
        }

        if self.is_cell() {
            return Some(self.as_cell_raw().class_id());
        }

        self.immediate_class_id()
    }

    pub fn is_class_id(&self, id: ClassId) -> bool {
        self.class_id() == Some(id)
    }

    pub fn class(&self, ctx: MutationContext<'gc>) -> Option<Gc<'gc, ClassDescriptor<'gc>>> {
        let id = self.class_id()?;
        class_table(ctx).lookup(id)
    }

    pub fn is_a(&self, ctx: MutationContext<'gc>, id: ClassId) -> bool {
        let class_id = match self.class_id() {
            Some(class_id) => class_id,
            None => return false,
        };

        class_table(ctx).is_subclass(class_id, id)
    }

    fn immediate_class_id(&self) -> Option<ClassId> {
        let id = if self.is_bool() {
            builtin_class_ids::BOOL
        } else if self.is_char() {
            builtin_class_ids::CHAR
        } else if self.is_null() {
            builtin_class_ids::NULL
        } else if self.is_eof() {
            builtin_class_ids::EOF
        } else if self.is_void() {
            builtin_class_ids::VOID
        } else if self.is_unspecified() {
            builtin_class_ids::UNSPECIFIED
        } else if self.raw_i64() == Self::VALUE_UNDEFINED {
            builtin_class_ids::UNDEFINED
        } else if self.is_int32() {
            builtin_class_ids::FIXNUM
        } else if self.is_flonum() {
            builtin_class_ids::FLONUM
        } else {
            return None;
        };

        ClassId::new(id)
    }

    pub fn is_immediate(&self) -> bool {
        !self.is_cell()
    }

    /// Wraps a GC-managed object pointer as a value.
    pub fn from_gc<T: ClassTagged>(gc: Gc<'gc, T>) -> Self {
        Self {
            desc: EncodedValueDescriptor {
                ptr: gc.as_gc_object().to_address().to_mut_ptr(),
            },
            pd: PhantomData,
        }
    }

    pub fn is<T: ClassTagged>(&self) -> bool {
        if !self.is_cell() {
            return false;
        }

        let class_id = self.as_cell_raw().class_id().bits();
        T::CLASS_IDS.contains(&class_id)
    }

    pub fn downcast<T: ClassTagged>(self) -> Gc<'gc, T> {
        debug_assert!(
            self.is::<T>(),
            "Value is not of type {}: {}",
            std::any::type_name::<T>(),
            self
        );
        // SAFETY: The pointer references a valid GC-managed object of the expected type
        unsafe { Gc::from_gc_object(self.as_cell_raw()) }
    }

    /// Downcast this value to a GC pointer without checking its type tag.
    ///
    /// # Safety
    ///
    /// The value must be a cell whose runtime type is `T`.
    // SAFETY: Caller must ensure preconditions are met (see fn docs)
    pub unsafe fn downcast_unchecked<T: ClassTagged>(self) -> Gc<'gc, T> {
        // SAFETY: The pointer references a valid GC-managed object of the expected type
        unsafe { Gc::from_gc_object(self.as_cell_raw()) }
    }

    pub fn try_as<T: ClassTagged>(self) -> Option<Gc<'gc, T>> {
        if self.is::<T>() {
            Some(self.downcast())
        } else {
            None
        }
    }

    pub fn is_bytevector(self) -> bool {
        self.is_cell()
            && !self.is_empty()
            && matches!(
                self.as_cell_raw().class_id().bits(),
                builtin_class_ids::MUTABLE_BYTEVECTOR
                    | builtin_class_ids::IMMUTABLE_BYTEVECTOR
                    | builtin_class_ids::MAPPED_BYTEVECTOR
            )
    }
}

impl<'gc> std::ops::Not for Value<'gc> {
    type Output = bool;

    fn not(self) -> Self::Output {
        self.raw_i64() == Self::VALUE_FALSE
    }
}

pub mod boxed;
pub mod conversions;
pub mod env;
pub mod environment;
pub mod eq;
pub mod global;
pub mod hamt;
pub mod hash;
pub mod header;
pub mod list;
pub mod number;
pub mod port;
pub mod print;
pub mod proc;
pub mod string;
pub mod symbols;
pub mod vector;
pub mod weak_set;
pub mod weak_table;
pub mod weak_value;

pub use boxed::*;
pub use conversions::*;
pub use env::*;
pub use global::*;
pub use hamt::{HamtKind, PersistentMap, PersistentSet};
pub use hash::*;
pub use header::*;
pub use list::*;
pub use number::*;
pub use print::*;
pub use proc::*;
pub use string::*;
pub use symbols::*;
pub use vector::*;
pub use weak_set::*;
pub use weak_table::*;
pub use weak_value::*;

use crate::runtime::vm::syntax::Syntax;

impl<'gc> fmt::Pointer for Value<'gc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Value({:x})", self.raw_i64())
    }
}

impl<'gc, T: ClassTagged> From<Gc<'gc, T>> for Value<'gc> {
    fn from(gc: Gc<'gc, T>) -> Self {
        Value::from_gc(gc)
    }
}

// SAFETY: All-zero bit pattern corresponds to VALUE_EMPTY, which is a valid (non-cell) Value.
unsafe impl<'gc> bytemuck::Zeroable for Value<'gc> {}

impl<'gc> std::fmt::Display for Value<'gc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut formatter = print::ValueFmt::new(f);

        formatter.print(*self, false, usize::MAX)
    }
}

impl<'gc> fmt::Debug for Value<'gc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

#[derive(Trace, Copy, Clone)]
#[collect(no_drop)]
/// Equality-key wrapper for values that participates in GC tracing.
pub struct ValueEqual<'gc>(pub Value<'gc>);

impl<'gc> From<ValueEqual<'gc>> for Value<'gc> {
    fn from(value: ValueEqual<'gc>) -> Self {
        value.0
    }
}

impl<'gc> From<Value<'gc>> for ValueEqual<'gc> {
    fn from(value: Value<'gc>) -> Self {
        Self(value)
    }
}

impl<'gc> PartialEq<Value<'gc>> for ValueEqual<'gc> {
    fn eq(&self, other: &Value<'gc>) -> bool {
        self.0.equal(*other, &mut Default::default())
    }
}

impl<'gc> PartialEq for ValueEqual<'gc> {
    fn eq(&self, other: &Self) -> bool {
        self.0.equal(other.0, &mut Default::default())
    }
}

impl<'gc> Eq for ValueEqual<'gc> {}

impl<'gc> std::hash::Hash for ValueEqual<'gc> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        EqualHash(self.0).hash(state);
    }
}
