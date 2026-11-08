/// | 3 | `Rational` | `Gc` pointer (non-null, 8 bytes) |
/// | 4 | `Complex` | `Gc` pointer (non-null, 8 bytes) |
///
/// A JIT load sequence is: load the `u64` tag at `TAG_OFFSET`, compare it,
/// then load the payload at `PAYLOAD_OFFSET` with the width and meaning from
/// the table. The four bytes after a `Fixnum` payload are padding and must
/// never be read. A `Gc` pointer is a VM object reference; it must stay in a
/// root or in a GC-traced object across any safepoint, exactly as any other
/// heap pointer.

    use super::{BigInt, Complex, Number, Rational};
    use crate::heap::Gc;
    use core::mem::{align_of, offset_of, size_of};
    use core::ptr;

    /// Tag of `Number::Fixnum`.
    pub const TAG_FIXNUM: u64 = 0;
    /// Tag of `Number::Flonum`.
    pub const TAG_FLONUM: u64 = 1;
    /// Tag of `Number::BigInt`.
    pub const TAG_BIGINT: u64 = 2;
    /// Tag of `Number::Rational`.
    pub const TAG_RATIONAL: u64 = 3;
    /// Tag of `Number::Complex`.
    pub const TAG_COMPLEX: u64 = 4;

    /// Offset of the tag word. `repr(u64)` puts the discriminant first.
    pub const TAG_OFFSET: usize = 0;
    /// Size of the tag word.
    pub const TAG_BYTES: usize = size_of::<u64>();
    /// Offset of the variant payload, i.e. the end of the tag word.
    pub const PAYLOAD_OFFSET: usize = TAG_BYTES;
    /// Size of the largest payload (a `f64` or a `Gc` pointer).
    pub const PAYLOAD_BYTES: usize = size_of::<u64>();
    /// Size of one `Number`.
    pub const SIZE: usize = size_of::<Number<'static>>();
    /// Alignment of one `Number`.
    pub const ALIGN: usize = align_of::<Number<'static>>();

    /// Size of a `Fixnum` payload in bytes. The payload word is 8 bytes; only
    /// these 4 contain the value.
    pub const FIXNUM_BYTES: usize = size_of::<i32>();
    /// Size of a `Flonum` payload: the raw bits of an `f64`.
    pub const FLONUM_BYTES: usize = size_of::<f64>();
    /// Size of a `Gc` payload. `Gc` is a non-null pointer.
    pub const GC_POINTER_BYTES: usize = size_of::<Gc<'static, BigInt<'static>>>();

    /// Reads the tag word of `value`.
    ///
    /// # Safety
    ///
    /// `value` must point to a live, initialized `Number`.
    #[inline(always)]
    pub const unsafe fn tag(value: &Number<'_>) -> u64 {
        // SAFETY: the caller guarantees a live `Number`, whose first word is
        // the `u64` discriminant.
        unsafe { ptr::read(value as *const Number<'_> as *const u64) }
    }

    /// Reads a `Fixnum` payload and sign-extends it (the stored word is `i32`).
    ///
    /// # Safety
    ///
    /// `value` must be a live `Number` whose tag is [`TAG_FIXNUM`].
    #[inline(always)]
    pub const unsafe fn fixnum_payload(value: &Number<'_>) -> i32 {
        // SAFETY: the caller guarantees the tag; the payload is 8-byte
        // aligned so a 4-byte `i32` read is aligned.
        unsafe { payload_read::<i32>(value) }
    }

    /// Reads the raw bits of a `Flonum` payload.
    ///
    /// # Safety
    ///
    /// `value` must be a live `Number` whose tag is [`TAG_FLONUM`].
    #[inline(always)]
    pub const unsafe fn flonum_bits(value: &Number<'_>) -> u64 {
        // SAFETY: the caller guarantees the tag.
        unsafe { payload_read::<u64>(value) }
    }

    /// Reads the non-null `Gc` pointer bits of a heap-number payload.
    ///
    /// # Safety
    ///
    /// `value` must be a live `Number` whose tag is [`TAG_BIGINT`],
    /// [`TAG_RATIONAL`] or [`TAG_COMPLEX`].
    #[inline(always)]
    pub const unsafe fn gc_pointer(value: &Number<'_>) -> usize {
        // SAFETY: the caller guarantees the tag.
        unsafe { payload_read::<usize>(value) }
    }

    /// # Safety
    ///
    /// The caller has checked the tag, so the payload bytes of `T` are
    /// initialized and aligned.
    #[inline(always)]
    const unsafe fn payload_read<T>(value: &Number<'_>) -> T {
        // SAFETY: forwarded from the caller.
        unsafe {
            ptr::read((value as *const Number<'_> as *const u8).add(PAYLOAD_OFFSET) as *const T)
        }
    }

    /// A dangling `Gc` used only to name the pointer-carrying variants in the
    /// compile-time checks below. It is never traced, dereferenced or read.
    const fn unreachable_gc<'gc, T>() -> Gc<'gc, T> {
        // SAFETY: layout-only. `Gc` is `repr(transparent)` over `NonNull<T>`;
        // the value is only matched by its tag word in a const assertion.
        unsafe { core::mem::transmute(core::ptr::NonNull::<T>::dangling()) }
    }

    const _: () = {
        // The tag and payload are adjacent and the enum is 16 bytes.
        assert!(TAG_OFFSET == 0);
        assert!(TAG_BYTES == 8);
        assert!(PAYLOAD_OFFSET == TAG_BYTES);
        assert!(PAYLOAD_OFFSET == 8);
        assert!(PAYLOAD_BYTES == 8);
        assert!(SIZE == PAYLOAD_OFFSET + PAYLOAD_BYTES);
        assert!(SIZE == 16);
        assert!(ALIGN == 8);

        // Payload widths used by the JIT.
        assert!(FIXNUM_BYTES == 4);
        assert!(FLONUM_BYTES == 8);
        assert!(GC_POINTER_BYTES == 8);

        // Variant tags are the declaration order.
        let fixnum = Number::Fixnum(-7);
        assert!(unsafe { tag(&fixnum) } == TAG_FIXNUM);
        assert!(unsafe { fixnum_payload(&fixnum) } == -7);

        let flonum = Number::Flonum(-2.5);
        assert!(unsafe { tag(&flonum) } == TAG_FLONUM);
        assert!(unsafe { flonum_bits(&flonum) } == (-2.5f64).to_bits());

        let bigint = Number::BigInt(unreachable_gc());
        assert!(unsafe { tag(&bigint) } == TAG_BIGINT);

        let rational = Number::Rational(unreachable_gc());
        assert!(unsafe { tag(&rational) } == TAG_RATIONAL);

        let complex = Number::Complex(unreachable_gc());
        assert!(unsafe { tag(&complex) } == TAG_COMPLEX);

        // `Rational` and `Complex` embed two `Number`s by value in a
        // `repr(C)` struct, so they are 32 bytes and their second component
        // starts at `SIZE`. Allocations use `size_of::<T>()` directly (there
        // is no `compute_size` hook for either type), so this is also the
        // allocated payload size the GC and the future `AllocRational` op
        // must use.
        assert!(size_of::<Rational<'static>>() == 2 * SIZE);
        assert!(align_of::<Rational<'static>>() == ALIGN);
        assert!(offset_of!(Rational<'static>, numerator) == 0);
        assert!(offset_of!(Rational<'static>, denominator) == SIZE);
        assert!(size_of::<Rational<'static>>() == 32);

        assert!(size_of::<Complex<'static>>() == 2 * SIZE);
        assert!(align_of::<Complex<'static>>() == ALIGN);
        assert!(offset_of!(Complex<'static>, real) == 0);
        assert!(offset_of!(Complex<'static>, imag) == SIZE);
        assert!(size_of::<Complex<'static>>() == 32);
    };

