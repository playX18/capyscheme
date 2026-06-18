//! Runtime representation and helpers for Scheme values.
//!
//! The core [`Value`] type stores immediate values and GC object references in a
//! NaN-boxed word. Submodules provide concrete heap object implementations and
//! operations for lists, strings, numbers, vectors, hash tables, and weak values.

use crate::rsgc::{
    Gc, ObjectSlot, Trace,
    barrier::Write,
    mmtk::{util::Address, vm::SlotVisitor},
    object::{ClassId, GCObject, HeapObjectHeader, builtin_class_ids},
};
use crate::runtime::Context;
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
    pub(crate) unsafe fn ptr(self) -> GCObject {
        // SAFETY: `*mut ()` and `GCObject` have the same layout (both pointer-sized).
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
// Non-cell values (ints, floats, tags) are ignored; cell values point into the GC heap
// and `desc.ptr` is at a stable address suitable for `ObjectSlot` construction.
unsafe impl<'gc> Trace for Value<'gc> {
    // SAFETY: All GC-reachable fields are traced via `visitor`
    unsafe fn trace(&mut self, visitor: &mut crate::rsgc::collection::Visitor) {
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
    unsafe fn process_weak_refs(&mut self, _weak_processor: &mut crate::rsgc::WeakProcessor) {}
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

    pub fn as_cell_raw(self) -> GCObject {
        // SAFETY: Caller must ensure the value is a cell. The NaN-boxing encoding guarantees
        // the low bits of a cell value form a valid pointer into the GC heap.
        // debug_assert!(self.is_cell());
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

    pub fn class(&self, ctx: Context<'gc>) -> Option<Gc<'gc, ClassDescriptor<'gc>>> {
        let id = self.class_id()?;
        class_table(ctx).lookup(id)
    }

    pub fn is_a(&self, ctx: Context<'gc>, id: ClassId) -> bool {
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
                ptr: gc.as_gcobj().to_address().to_mut_ptr(),
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
        unsafe { Gc::from_gcobj(self.as_cell_raw()) }
    }

    /// Downcast this value to a GC pointer without checking its type tag.
    ///
    /// # Safety
    ///
    /// The value must be a cell whose runtime type is `T`.
    // SAFETY: Caller must ensure preconditions are met (see fn docs)
    pub unsafe fn downcast_unchecked<T: ClassTagged>(self) -> Gc<'gc, T> {
        // SAFETY: The pointer references a valid GC-managed object of the expected type
        unsafe { Gc::from_gcobj(self.as_cell_raw()) }
    }

    pub fn try_as<T: ClassTagged>(self) -> Option<Gc<'gc, T>> {
        if self.is::<T>() {
            Some(self.downcast())
        } else {
            None
        }
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
pub mod environment;
pub mod eq;
pub mod global;
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
pub use global::*;
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

use crate::runtime::{modules::Module, vm::syntax::Syntax};

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

#[cfg(test)]
mod tests {
    use super::{
        Boxed, ByteVector, Closure, CodeArity, CodeBlock, Complex, HashTable, HashTableType,
        Keyword, NativeProc, NativeReturn, Number, Rational, ReturnCode, Str, Symbol, Tuple, Value,
        Vector, WeakMapping, WeakSet, WeakTable, pure_nan,
    };
    use crate::frontend::reader::Annotation;
    use crate::rsgc::Gc;
    use crate::rsgc::mmtk::util::Address;
    use crate::rsgc::object::{ClassId, builtin_class_ids, class_header_word};
    use crate::runtime::Scheme;
    use crate::runtime::vm::control::ContinuationMarks;
    use crate::runtime::vm::ffi::{Pointer, pointer_header_word};
    use crate::runtime::vm::syntax::{Syntax, SyntaxTransformer};

    fn annotation_header_word() -> u64 {
        class_header_word(ClassId::new(builtin_class_ids::ANNOTATION).unwrap())
    }

    #[test]
    fn from_f64_canonicalizes_nan_payloads() {
        let payloads = [
            pure_nan::PURE_NAN_BITS,
            0x7ff8000000000001,
            0x7ff0000000000001,
            0xfff8000000000001,
        ];

        for bits in payloads {
            let value = Value::from_f64(f64::from_bits(bits));

            assert!(value.is_flonum());
            assert_eq!(value.as_flonum().to_bits(), pure_nan::PURE_NAN_BITS);
        }
    }

    #[test]
    fn is_char_rejects_invalid_scalar_payloads() {
        let surrogate = Value::from_raw_i64((0xd800_i64 << 16) | Value::CHAR_TAG);
        let above_scalar_range = Value::from_raw_i64((0x110000_i64 << 16) | Value::CHAR_TAG);

        assert!(!surrogate.is_char());
        assert!(!above_scalar_range.is_char());
    }

    #[test]
    fn immediate_values_have_builtin_class_ids() {
        let cases = [
            (Value::new(false), builtin_class_ids::BOOL),
            (Value::new(true), builtin_class_ids::BOOL),
            (Value::from_char('x'), builtin_class_ids::CHAR),
            (Value::null(), builtin_class_ids::NULL),
            (Value::eof(), builtin_class_ids::EOF),
            (Value::void(), builtin_class_ids::VOID),
            (Value::unspecified(), builtin_class_ids::UNSPECIFIED),
            (Value::undefined(), builtin_class_ids::UNDEFINED),
            (Value::from_i32(42), builtin_class_ids::FIXNUM),
            (Value::from_f64(1.25), builtin_class_ids::FLONUM),
        ];

        for (value, raw_id) in cases {
            let id = ClassId::new(raw_id).unwrap();
            assert_eq!(value.class_id(), Some(id));
            assert!(value.is_class_id(id));
        }
    }

    #[test]
    fn internal_immediate_sentinels_do_not_get_public_class_ids() {
        assert_eq!(Value::empty().class_id(), None);
        assert_eq!(Value::deleted().class_id(), None);
        assert_eq!(Value::bwp().class_id(), None);
    }

    extern "C-unwind" fn class_predicate_test_proc<'gc>(
        _ctx: crate::runtime::Context<'gc>,
        _rator: Value<'gc>,
        _rands: *const Value<'gc>,
        _num_rands: usize,
        _retk: Value<'gc>,
    ) -> NativeReturn<'gc> {
        NativeReturn {
            code: ReturnCode::ReturnOk,
            value: Value::undefined(),
        }
    }

    #[test]
    fn heap_values_use_fixed_builtin_class_ids() {
        Scheme::new_uninit().enter(|ctx| {
            let cases = [
                (
                    Value::cons(ctx, Value::new(1), Value::null()),
                    builtin_class_ids::PAIR,
                ),
                (
                    Value::from(Symbol::from_str(ctx, "class-id-symbol")),
                    builtin_class_ids::SYMBOL,
                ),
                (
                    Value::from(Symbol::from_str_uninterned(
                        *ctx,
                        "class-id-uninterned-symbol",
                        None,
                    )),
                    builtin_class_ids::SYMBOL,
                ),
                (
                    Value::from(Keyword::from_symbol(
                        *ctx,
                        Symbol::from_str_uninterned(*ctx, "class-id-keyword", None),
                    )),
                    builtin_class_ids::KEYWORD,
                ),
                (
                    Value::from(Str::from_str(*ctx, "class id string")),
                    builtin_class_ids::STRING,
                ),
                (
                    Value::from(Vector::new::<false>(*ctx, 2, Value::unspecified())),
                    builtin_class_ids::VECTOR,
                ),
                (
                    Value::from(ByteVector::new::<false>(*ctx, 4, true)),
                    builtin_class_ids::MUTABLE_BYTEVECTOR,
                ),
                (
                    Value::from(HashTable::new(*ctx, HashTableType::Eq, 4, 0.75)),
                    builtin_class_ids::HASHTABLE,
                ),
                (
                    Value::from(Boxed::new(ctx, Value::new(7))),
                    builtin_class_ids::BOX,
                ),
            ];

            for (value, raw_id) in cases {
                assert_eq!(value.class_id(), ClassId::new(raw_id));
            }
        });
    }

    #[test]
    fn collapsed_variant_values_use_layout_class_ids() {
        Scheme::new_uninit().enter(|ctx| {
            let pair = Value::cons(ctx, Value::new(1), Value::null());
            let code_block = CodeBlock::new_aot(
                ctx,
                Address::from_ptr(class_predicate_test_proc as *const ()),
                CodeArity::new(0),
                false,
                Value::null(),
            );
            let closure_proc = Value::from(Closure::new(ctx, code_block, &[], false));
            let closure_continuation = Value::from(Closure::new(ctx, code_block, &[], true));
            let mutable_vector = Value::from(Vector::new::<false>(*ctx, 1, Value::undefined()));
            let immutable_vector_gc = Vector::new::<true>(*ctx, 1, Value::undefined());
            let immutable_vector = Value::from(immutable_vector_gc);
            let mutable_bytevector = Value::from(ByteVector::new::<false>(*ctx, 4, true));
            let immutable_bytevector = Value::from(ByteVector::new::<true>(*ctx, 4, true));
            let bytevector_source = ByteVector::new::<false>(*ctx, 4, true);
            let mapped_bytevector = Value::from(ByteVector::new_mapping(
                *ctx,
                bytevector_source.contents(),
                4,
            ));
            let tuple = Value::from(Tuple::new(*ctx, 1, Value::undefined()));
            let string = Value::from(Str::new(*ctx, "class-only string", false));
            let immutable_string = Value::from(Str::new(*ctx, "class-only immutable string", true));
            let symbol = Value::from(Symbol::from_str(ctx, "class-only-symbol"));
            let uninterned_symbol = Value::from(Symbol::from_str_uninterned(
                *ctx,
                "class-only-uninterned-symbol",
                None,
            ));
            let keyword = Value::from(Keyword::from_symbol(
                *ctx,
                Symbol::from_str_uninterned(*ctx, "class-only-keyword", None),
            ));
            let mutable_hash = Value::from(HashTable::new(*ctx, HashTableType::Eq, 4, 0.75));
            let immutable_hash =
                Value::from(HashTable::new_immutable(*ctx, HashTableType::Eq, 4, 0.75));
            let weak_set = Value::from(WeakSet::new(*ctx, 31));
            let weak_table = Value::from(WeakTable::new(*ctx, 8, 0.75));
            let weak_mapping_key = Value::cons(ctx, Value::new(1), Value::null());
            let weak_mapping = Value::from(WeakMapping::new(ctx, weak_mapping_key, Value::new(2)));
            let boxed = Value::from(Boxed::new(ctx, Value::new(7)));
            let rational = Value::from(Rational::new(ctx, Number::Fixnum(1), Number::Fixnum(2)));
            let complex = Value::from(Complex::new(ctx, Number::Fixnum(1), Number::Fixnum(2)));
            let native_proc = Value::from(NativeProc::new(ctx, Address::ZERO, false));
            let native_continuation = Value::from(NativeProc::new(ctx, Address::ZERO, true));

            let cases = [
                (pair, builtin_class_ids::PAIR),
                (closure_proc, builtin_class_ids::CLOSURE),
                (closure_continuation, builtin_class_ids::CLOSURE),
                (mutable_vector, builtin_class_ids::VECTOR),
                (immutable_vector, builtin_class_ids::VECTOR),
                (mutable_bytevector, builtin_class_ids::MUTABLE_BYTEVECTOR),
                (
                    immutable_bytevector,
                    builtin_class_ids::IMMUTABLE_BYTEVECTOR,
                ),
                (mapped_bytevector, builtin_class_ids::MAPPED_BYTEVECTOR),
                (tuple, builtin_class_ids::TUPLE),
                (string, builtin_class_ids::STRING),
                (immutable_string, builtin_class_ids::STRING),
                (symbol, builtin_class_ids::SYMBOL),
                (uninterned_symbol, builtin_class_ids::SYMBOL),
                (keyword, builtin_class_ids::KEYWORD),
                (mutable_hash, builtin_class_ids::HASHTABLE),
                (immutable_hash, builtin_class_ids::IMMUTABLE_HASHTABLE),
                (weak_set, builtin_class_ids::WEAK_SET),
                (weak_table, builtin_class_ids::WEAK_TABLE),
                (weak_mapping, builtin_class_ids::WEAK_MAPPING),
                (boxed, builtin_class_ids::BOX),
                (rational, builtin_class_ids::RATIONAL),
                (complex, builtin_class_ids::COMPLEX),
                (native_proc, builtin_class_ids::NATIVE_PROCEDURE),
                (native_continuation, builtin_class_ids::NATIVE_CONTINUATION),
            ];

            for (value, raw_class_id) in cases {
                assert_eq!(value.class_id(), ClassId::new(raw_class_id));
            }
            assert!(!closure_proc.downcast::<Closure>().is_continuation());
            assert!(closure_continuation.downcast::<Closure>().is_continuation());
            assert!(immutable_vector_gc.is_immutable());
            assert!(immutable_string.downcast::<Str>().is_immutable());
            assert!(!symbol.downcast::<Symbol>().is_uninterned());
            assert!(uninterned_symbol.downcast::<Symbol>().is_uninterned());
        });
    }

    #[test]
    fn class_predicates_accept_collapsed_layout_classes() {
        Scheme::new_uninit().enter(|ctx| {
            let mutable_vector = Value::from(Vector::new::<false>(*ctx, 1, Value::undefined()));
            let immutable_vector_gc = Vector::new::<true>(*ctx, 1, Value::undefined());
            let immutable_vector = Value::from(immutable_vector_gc);
            assert!(mutable_vector.is::<Vector>());
            assert!(immutable_vector.is::<Vector>());
            assert!(mutable_vector.is_class_id(ClassId::new(builtin_class_ids::VECTOR).unwrap()));
            assert!(immutable_vector.is_class_id(ClassId::new(builtin_class_ids::VECTOR).unwrap()));
            assert!(immutable_vector_gc.is_immutable());

            let mutable_bytevector = Value::from(ByteVector::new::<false>(*ctx, 1, true));
            let immutable_bytevector = Value::from(ByteVector::new::<true>(*ctx, 1, true));
            assert!(mutable_bytevector.is::<ByteVector>());
            assert!(immutable_bytevector.is::<ByteVector>());
            assert!(
                mutable_bytevector
                    .is_class_id(ClassId::new(builtin_class_ids::MUTABLE_BYTEVECTOR).unwrap())
            );
            assert!(
                immutable_bytevector
                    .is_class_id(ClassId::new(builtin_class_ids::IMMUTABLE_BYTEVECTOR).unwrap())
            );

            let mutable_hash = Value::from(HashTable::new(*ctx, HashTableType::Eq, 4, 0.75));
            let immutable_hash =
                Value::from(HashTable::new_immutable(*ctx, HashTableType::Eq, 4, 0.75));
            assert!(mutable_hash.is::<HashTable>());
            assert!(immutable_hash.is::<HashTable>());
            assert!(mutable_hash.is_class_id(ClassId::new(builtin_class_ids::HASHTABLE).unwrap()));
            assert!(
                immutable_hash
                    .is_class_id(ClassId::new(builtin_class_ids::IMMUTABLE_HASHTABLE).unwrap())
            );

            let native_proc = Value::from(NativeProc::new(ctx, Address::ZERO, false));
            let native_continuation = Value::from(NativeProc::new(ctx, Address::ZERO, true));
            assert!(native_proc.is::<NativeProc>());
            assert!(native_continuation.is::<NativeProc>());
            assert!(
                native_proc.is_class_id(ClassId::new(builtin_class_ids::NATIVE_PROCEDURE).unwrap())
            );
            assert!(
                native_continuation
                    .is_class_id(ClassId::new(builtin_class_ids::NATIVE_CONTINUATION).unwrap())
            );

            let code_block = CodeBlock::new_aot(
                ctx,
                Address::from_ptr(class_predicate_test_proc as *const ()),
                CodeArity::new(0),
                false,
                Value::null(),
            );
            let closure_proc = Value::from(Closure::new(ctx, code_block, &[], false));
            let closure_continuation = Value::from(Closure::new(ctx, code_block, &[], true));
            assert!(closure_proc.is::<Closure>());
            assert!(closure_continuation.is::<Closure>());
            assert!(closure_proc.is_class_id(ClassId::new(builtin_class_ids::CLOSURE).unwrap()));
            assert!(
                closure_continuation.is_class_id(ClassId::new(builtin_class_ids::CLOSURE).unwrap())
            );
            assert!(!closure_proc.downcast::<Closure>().is_continuation());
            assert!(closure_continuation.downcast::<Closure>().is_continuation());

            let annotation = Value::from(Gc::new_with_header_word(
                *ctx,
                Annotation {
                    expression: Value::new(1),
                    stripped: Value::new(1),
                    source: Value::null(),
                    start_point: (0, 0),
                    end_point: (0, 0),
                },
                annotation_header_word(),
            ));
            assert!(annotation.is::<Annotation>());
            assert!(annotation.is_class_id(ClassId::new(builtin_class_ids::ANNOTATION).unwrap()));

            let marks = Value::from(ctx.current_continuation_marks());
            assert!(marks.is::<ContinuationMarks>());
            assert!(
                marks.is_class_id(ClassId::new(builtin_class_ids::CONTINUATION_MARKS).unwrap())
            );

            let pointer = Value::from(Gc::new_with_header_word(
                *ctx,
                Pointer::new(std::ptr::null_mut()),
                pointer_header_word(),
            ));
            assert!(pointer.is::<Pointer>());
            assert!(pointer.is_class_id(ClassId::new(builtin_class_ids::POINTER).unwrap()));

            let syntax = Value::from(Syntax::new(
                ctx,
                Value::new(1),
                Value::null(),
                Value::null(),
                Value::null(),
                Value::null(),
            ));
            assert!(syntax.is::<Syntax>());
            assert!(syntax.is_class_id(ClassId::new(builtin_class_ids::SYNTAX).unwrap()));

            let transformer = Value::from(SyntaxTransformer::new(
                ctx,
                Value::null(),
                Value::null(),
                Value::null(),
            ));
            assert!(transformer.is::<SyntaxTransformer>());
            assert!(
                transformer
                    .is_class_id(ClassId::new(builtin_class_ids::SYNTAX_TRANSFORMER).unwrap())
            );
        });
    }

    #[test]
    fn values_support_class_lookup_and_is_a() {
        Scheme::new_uninit().enter(|ctx| {
            let number = ClassId::new(builtin_class_ids::NUMBER).unwrap();
            let symbol_class = ClassId::new(builtin_class_ids::SYMBOL).unwrap();
            let object = ClassId::new(builtin_class_ids::OBJECT).unwrap();
            let top = ClassId::new(builtin_class_ids::TOP).unwrap();
            let pair = Value::cons(ctx, Value::new(1), Value::null());
            let fixnum = Value::new(42);
            let symbol = Value::from(Symbol::from_str_uninterned(*ctx, "class-lookup", None));

            assert_eq!(fixnum.class(ctx).unwrap().name(), "fixnum");
            assert!(fixnum.is_a(ctx, number));
            assert!(fixnum.is_a(ctx, object));
            assert!(fixnum.is_a(ctx, top));
            assert!(!pair.is_a(ctx, number));
            assert!(pair.is_a(ctx, object));
            assert!(symbol.is_a(ctx, symbol_class));
            assert!(symbol.is_a(ctx, object));
            assert!(Value::empty().class(ctx).is_none());
            assert!(!Value::empty().is_a(ctx, object));
        });
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
