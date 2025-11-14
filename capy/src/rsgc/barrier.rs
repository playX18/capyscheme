use std::mem;
use std::ops::{Deref, DerefMut, Index};

#[cfg(doc)]
use crate::rsgc::Gc;

/// An (interiorly-)mutable reference inside a GC'd object graph.
///
/// This type can only exist behind a reference; it is typically obtained by calling
/// [`Gc::write`] on a [`Gc`] pointer or by using the [`field!`] projection macro
/// on a pre-existing `&Write<T>`.
#[non_exhaustive]
#[repr(transparent)]
pub struct Write<T: ?Sized> {
    // Public so that the `field!` macro can pattern-match on it; the `non_exhaustive` attribute
    // prevents 3rd-party code from instanciating the struct directly.
    #[doc(hidden)]
    pub __inner: T,
}

impl<T: ?Sized> Deref for Write<T> {
    type Target = T;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.__inner
    }
}

impl<T: ?Sized> DerefMut for Write<T> {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.__inner
    }
}

impl<T: ?Sized> Write<T> {
    /// Asserts that the given reference can be safely written to.
    ///
    /// # Safety
    /// In order to maintain the invariants of the garbage collector, no new [`Gc`] pointers
    /// may be adopted by the referenced value as a result of the interior mutability enabled
    /// by this wrapper, unless [`Gc::write`] is invoked manually on the parent [`Gc`]
    /// pointer during the current arena callback.
    #[inline(always)]
    pub unsafe fn assume(v: &T) -> &Self {
        // SAFETY: `Self` is `repr(transparent)`.
        unsafe { mem::transmute(v) }
    }

    /// Gets a writable reference to non-GC'd data.
    ///
    /// This is safe, as `'static` types can never hold [`Gc`] pointers.
    #[inline]
    pub fn from_static(v: &T) -> &Self
    where
        T: 'static,
    {
        // SAFETY: `Self` is `repr(transparent)`.
        unsafe { mem::transmute(v) }
    }

    /// Gets a writable reference from a `&mut T`.
    ///
    /// This is safe, as exclusive access already implies writability.
    #[inline]
    pub fn from_mut(v: &mut T) -> &mut Self {
        // SAFETY: `Self` is `repr(transparent)`.
        unsafe { mem::transmute(v) }
    }

    /// Implementation detail of `write_field!`; same safety requirements as `assume`.
    #[inline(always)]
    #[doc(hidden)]
    pub unsafe fn __from_ref_and_ptr(v: &T, _: *const T) -> &Self {
        // SAFETY: `Self` is `repr(transparent)`.
        unsafe { mem::transmute(v) }
    }

    /// Unlocks the referenced value, providing full interior mutability.
    #[inline]
    pub fn unlock(&self) -> &T::Unlocked
    where
        T: Unlock,
    {
        // SAFETY: a `&Write<T>` implies that a write barrier was triggered on the parent `Gc`.
        unsafe { self.__inner.unlock_unchecked() }
    }
}

impl<T> Write<Option<T>> {
    /// Converts from `&Write<Option<T>>` to `Option<&Write<T>>`.
    #[inline(always)]
    pub fn as_write(&self) -> Option<&Write<T>> {
        // SAFETY: this is simple destructuring
        unsafe {
            match &self.__inner {
                None => None,
                Some(v) => Some(Write::assume(v)),
            }
        }
    }
}

impl<T, E> Write<Result<T, E>> {
    /// Converts from `&Write<Result<T, E>>` to `Result<&Write<T>, &Write<E>>`.
    #[inline(always)]
    pub fn as_write(&self) -> Result<&Write<T>, &Write<E>> {
        // SAFETY: this is simple destructuring
        unsafe {
            match &self.__inner {
                Ok(v) => Ok(Write::assume(v)),
                Err(e) => Err(Write::assume(e)),
            }
        }
    }
}

/// Types that support additional operations (typically, mutation) when behind a write barrier.
pub trait Unlock {
    /// This will typically be a cell-like type providing some sort of interior mutability.
    type Unlocked: ?Sized;

    /// Provides unsafe access to the unlocked type, *without* triggering a write barrier.
    ///
    /// # Safety
    ///
    /// In order to maintain the invariants of the garbage collector, no new `Gc` pointers
    /// may be adopted by as a result of the interior mutability afforded by the unlocked value,
    /// unless the write barrier for the containing `Gc` pointer is invoked manually before
    /// collection is triggered.
    unsafe fn unlock_unchecked(&self) -> &Self::Unlocked;
}

/// Macro for named field projection behind [`Write`] references.
///
/// # Usage
///
/// ```
/// # use rsgc::barrier::{field, Write};
/// struct Container<T> {
///     field: T,
/// }
///
/// fn project<T>(v: &Write<Container<T>>) -> &Write<T> {
///     field!(v, Container, field)
/// }
/// ```
///
/// # Limitations
///
/// This macro only support structs with named fields; tuples and enums aren't supported.
#[doc(inline)]
pub use crate::__field as field;

// Actual macro item, hidden so that it doesn't show at the crate root.
#[macro_export]
#[doc(hidden)]
macro_rules! __field {
    ($value:expr, $type:path, $field:ident) => {
        // SAFETY:
        // For this to be sound, we need to prevent deref coercions from happening, as they may
        // access nested `Gc` pointers, which would violate the write barrier invariant. This is
        // guaranteed as follows:
        // - the destructuring pattern, unlike a simple field access, cannot call `Deref`;
        // - similarly, the `__from_ref_and_ptr` method takes both a reference (for the lifetime)
        //   and a pointer, causing a compilation failure if the first argument was coerced.
        match $value {
            &$crate::rsgc::barrier::Write {
                __inner: $type { ref $field, .. },
                ..
            } => unsafe {
                $crate::rsgc::barrier::Write::__from_ref_and_ptr($field, $field as *const _)
            },
        }
    };

    ($value:expr, $type:path, $field:ident[$index: expr]) => {
        // SAFETY:
        // For this to be sound, we need to prevent deref coercions from happening, as they may
        // access nested `Gc` pointers, which would violate the write barrier invariant. This is
        // guaranteed as follows:
        // - the destructuring pattern, unlike a simple field access, cannot call `Deref`;
        // - similarly, the `__from_ref_and_ptr` method takes both a reference (for the lifetime)
        //   and a pointer, causing a compilation failure if the first argument was coerced.
        match $value {
            &$crate::barrier::Write {
                __inner: $type { ref $field, .. },
                ..
            } => unsafe {
                $crate::barrier::Write::__from_ref_and_ptr(
                    &$field[$index],
                    &$field[$index] as *const _,
                )
            },
        }
    };

    (unsafe_array $value: expr, $type:path, $field:ident[$index: expr; $len: expr]) => {
        // SAFETY:
        // For this to be sound, we need to prevent deref coercions from happening, as they may
        // access nested `Gc` pointers, which would violate the write barrier invariant. This is
        // guaranteed as follows:
        // - the destructuring pattern, unlike a simple field access, cannot call `Deref`;
        // - similarly, the `__from_ref_and_ptr` method takes both a reference (for the lifetime)
        //   and a pointer, causing a compilation failure if the first argument was coerced.
        match $value {
            &$crate::barrier::Write {
                __inner: $type { ref $field, .. },
                ..
            } => unsafe {
                let len = $len;
                let index = $index;
                let arr = std::slice::from_raw_parts($field as *const _, len);
                $crate::barrier::Write::__from_ref_and_ptr(&arr[index], &arr[index] as *const _)
            },
        }
    };
}

/// Shorthand for [`field!`]`(...).`[`unlock()`](Write::unlock).
#[doc(inline)]
pub use crate::__unlock as unlock;

// Actual macro item, hidden so that it doesn't show at the crate root.
#[macro_export]
#[doc(hidden)]
macro_rules! __unlock {
    ($value:expr, $type:path, $field:ident) => {
        $crate::barrier::field!($value, $type, $field).unlock()
    };
}

/// A marker trait for types that can be indexed when behind a write barrier.
///
/// # Safety
/// You must ensure that `T::Output` is safe to convert to `Write<T::Output>`.
pub unsafe trait IndexWrite<I>: Index<I> {}

impl<I, T: IndexWrite<I>> std::ops::Index<I> for Write<T> {
    type Output = Write<T::Output>;

    #[inline(always)]
    fn index(&self, index: I) -> &Self::Output {
        // SAFETY: `Self` is `repr(transparent)`.
        unsafe { Write::assume(&self.__inner[index]) }
    }
}

/// A marker trait for types that can be cheaply converted to other references when behind a write
/// barrier.
///
/// # Safety
///
/// You must ensure that `T` is safe to convert to `Write<T>`.
pub unsafe trait AsRefWrite<T: ?Sized>: AsRef<T> {}

impl<T, U: AsRefWrite<T>> AsRef<Write<T>> for Write<U> {
    #[inline(always)]
    fn as_ref(&self) -> &Write<T> {
        // SAFETY: `Self` is `repr(transparent)`.
        unsafe { Write::assume(self.__inner.as_ref()) }
    }
}
