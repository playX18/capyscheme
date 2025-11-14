use atomic::Atomic;
use bytemuck::NoUninit;

use super::barrier::*;
use crate::rsgc::Gc;
use crate::rsgc::Mutation;
use crate::rsgc::collection::Visitor;
use crate::rsgc::traits::Trace;
use std::cell::*;
use std::fmt;
use std::sync::atomic::Ordering;

macro_rules! make_lock_wrapper {
    (
        $(#[$meta:meta])*
        locked = $locked_type:ident as $gc_locked_type:ident;
        unlocked = $unlocked_type:ident unsafe $unsafe_unlock_method:ident;
        impl Sized { $($sized_items:tt)* }
        impl ?Sized { $($unsized_items:tt)* }
    ) => {
        /// A wrapper around a [`
        #[doc = stringify!($unlocked_type)]
        /// `] that implements [`Collect`].
        ///
        /// Only provides safe read access to the wrapped [`
        #[doc = stringify!($unlocked_type)]
        /// `], full write access requires unsafety.
        ///
        /// If the `
        #[doc = stringify!($locked_type)]
        /// ` is directly held in a [`Gc`] pointer, safe mutable access is provided,
        /// since methods on [`Gc`] can ensure that the write barrier is called.
        $(#[$meta])*
        #[repr(transparent)]
        pub struct $locked_type<T: ?Sized> {
            cell: $unlocked_type<T>,
        }

        #[doc = concat!("An alias for `Gc<'gc, ", stringify!($locked_type), "<T>>`.")]
        pub type $gc_locked_type<'gc, T> = Gc<'gc, $locked_type<T>>;

        impl<T> $locked_type<T> {
            #[inline]
            pub const fn new(t: T) -> $locked_type<T> {
                Self { cell: $unlocked_type::new(t) }
            }

            #[inline]
            pub fn into_inner(self) -> T {
                self.cell.into_inner()
            }

            $($sized_items)*
        }

        impl<T: ?Sized> $locked_type<T> {
            #[inline]
            pub fn as_ptr(&self) -> *mut T {
                self.cell.as_ptr()
            }

            $($unsized_items)*

            #[doc = concat!("Access the wrapped [`", stringify!($unlocked_type), "`].")]
            ///
            /// # Safety
            /// In order to maintain the invariants of the garbage collector, no new [`Gc`]
            /// pointers may be adopted by this type as a result of the interior mutability
            /// afforded by directly accessing the inner [`
            #[doc = stringify!($unlocked_type)]
            /// `], unless the write barrier for the containing [`Gc`] pointer is invoked manually
            /// before collection is triggered.
            #[inline]
            pub unsafe fn $unsafe_unlock_method(&self) -> &$unlocked_type<T> {
                &self.cell
            }

            #[inline]
            pub fn get_mut(&mut self) -> &mut T {
                self.cell.get_mut()
            }
        }

        impl<T: ?Sized> Unlock for $locked_type<T> {
            type Unlocked = $unlocked_type<T>;

            #[inline]
            unsafe fn unlock_unchecked(&self) -> &Self::Unlocked {
                &self.cell
            }
        }

        impl<T> From<T> for $locked_type<T> {
            #[inline]
            fn from(t: T) -> Self {
                Self::new(t)
            }
        }

        impl<T> From<$unlocked_type<T>> for $locked_type<T> {
            #[inline]
            fn from(cell: $unlocked_type<T>) -> Self {
                Self { cell }
            }
        }
    };
}

make_lock_wrapper!(
    #[derive(Default)]
    locked = Lock as GcLock;
    unlocked = Cell unsafe as_cell;
    impl Sized {
        #[inline]
        pub fn get(&self) -> T where T: Copy {
            self.cell.get()
        }

        #[inline]
        pub fn take(&self) -> T where T: Default {
            // Despite mutating the contained value, this doesn't need a write barrier, as
            // the return value of `Default::default` can never contain (non-leaked) `Gc` pointers.
            //
            // The reason for this is somewhat subtle, and boils down to lifetime parametricity.
            // Because Rust doesn't allow naming concrete lifetimes, and because `Default` doesn't
            // have any lifetime parameters, any potential `'gc` lifetime in `T` must be
            // existentially quantified. As such, a `Default` implementation that tries to smuggle
            // a branded `Gc` pointer or `Mutation` through external state (e.g. thread
            // locals) must use unsafe code and cannot be sound in the first place, as it has no
            // way to ensure that the smuggled data has the correct `'gc` brand.
            self.cell.take()
        }
    }
    impl ?Sized {}
);

impl<T: Copy + fmt::Debug> fmt::Debug for Lock<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Lock").field(&self.cell).finish()
    }
}

impl<'gc, T: Copy + 'gc> Gc<'gc, Lock<T>> {
    #[inline]
    pub fn get(self) -> T {
        self.cell.get()
    }

    #[inline]
    pub fn set(self, mc: &Mutation<'gc>, t: T) {
        self.unlock(mc).set(t);
    }
}

unsafe impl<T: Trace> Trace for Lock<T> {
    unsafe fn trace(&mut self, tracer: &mut Visitor) {
        unsafe { self.cell.get_mut().trace(tracer) };
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::rsgc::WeakProcessor) {
        unsafe { self.cell.get_mut().process_weak_refs(weak_processor) };
    }
}
impl<T: Copy> Clone for Lock<T> {
    fn clone(&self) -> Self {
        Lock::new(self.get())
    }
}

impl<T: PartialEq + Copy> PartialEq for Lock<T> {
    fn eq(&self, other: &Self) -> bool {
        self.get() == other.get()
    }
}

impl<T: Eq + Copy> Eq for Lock<T> {}

impl<T: PartialOrd + Copy> PartialOrd for Lock<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.get().partial_cmp(&other.get())
    }
}

impl<T: Ord + Copy> Ord for Lock<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.get().cmp(&other.get())
    }
}

make_lock_wrapper!(
    #[derive(Clone, Default, Eq, PartialEq, Ord, PartialOrd)]
    locked = RefLock as GcRefLock;
    unlocked = RefCell unsafe as_ref_cell;
    impl Sized {
        #[inline]
        pub fn take(&self) -> T where T: Default {
            // See comment in `Lock::take`.
            self.cell.take()
        }
    }
    impl ?Sized {
        #[track_caller]
        #[inline]
        pub fn borrow<'a>(&'a self) -> Ref<'a, T> {
            self.cell.borrow()
        }

        #[inline]
        pub fn try_borrow<'a>(&'a self) -> Result<Ref<'a, T>, std::cell::BorrowError> {
            self.cell.try_borrow()
        }
    }
);

impl<T: fmt::Debug + ?Sized> fmt::Debug for RefLock<T> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        let mut fmt = fmt.debug_tuple("RefLock");
        match self.try_borrow() {
            Ok(borrow) => fmt.field(&borrow),
            Err(_) => {
                // The RefLock is mutably borrowed so we can't look at its value
                // here. Show a placeholder instead.
                struct BorrowedPlaceholder;

                impl fmt::Debug for BorrowedPlaceholder {
                    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                        f.write_str("<borrowed>")
                    }
                }

                fmt.field(&BorrowedPlaceholder)
            }
        }
        .finish()
    }
}

impl<'gc, T: Sized + 'gc> Gc<'gc, RefLock<T>> {
    #[track_caller]
    #[inline]
    pub fn borrow(self) -> Ref<'gc, T> {
        RefLock::borrow(self.as_ref())
    }

    #[inline]
    pub fn try_borrow(self) -> Result<Ref<'gc, T>, std::cell::BorrowError> {
        RefLock::try_borrow(self.as_ref())
    }

    #[track_caller]
    #[inline]
    pub fn borrow_mut(self, mc: &Mutation<'gc>) -> RefMut<'gc, T> {
        self.unlock(mc).borrow_mut()
    }

    #[inline]
    pub fn try_borrow_mut(
        self,
        mc: &Mutation<'gc>,
    ) -> Result<RefMut<'gc, T>, std::cell::BorrowMutError> {
        self.unlock(mc).try_borrow_mut()
    }
}

unsafe impl<T: Trace> Trace for RefLock<T> {
    unsafe fn trace(&mut self, tracer: &mut Visitor) {
        unsafe { self.cell.get_mut().trace(tracer) };
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::rsgc::WeakProcessor) {
        unsafe { self.cell.get_mut().process_weak_refs(weak_processor) };
    }
}

pub struct AtomicLock<T: NoUninit>(Atomic<T>);

impl<T: NoUninit> AtomicLock<T> {
    pub fn get(&self) -> T
    where
        T: Copy,
    {
        self.0.load(Ordering::Relaxed)
    }

    pub fn take(&self) -> T
    where
        T: Default,
    {
        self.0.swap(T::default(), Ordering::Relaxed)
    }

    pub fn into_inner(self) -> Atomic<T> {
        self.0
    }

    pub fn as_ptr(&self) -> *const T {
        &self.0 as *const Atomic<T> as *const T
    }

    /// Get a reference to the underlying atomic type.
    ///
    /// # Safety
    ///
    /// Memory is not write-barrier protected, so you must
    /// ensure that no new `Gc` pointers are adopted.
    pub unsafe fn as_atomic(&self) -> &Atomic<T> {
        &self.0
    }
}

impl<T: NoUninit> Unlock for AtomicLock<T> {
    type Unlocked = Atomic<T>;

    #[inline]
    unsafe fn unlock_unchecked(&self) -> &Self::Unlocked {
        &self.0
    }
}

impl<T: NoUninit> From<Atomic<T>> for AtomicLock<T> {
    fn from(value: Atomic<T>) -> Self {
        Self(value)
    }
}

impl<T: NoUninit> From<T> for AtomicLock<T> {
    fn from(value: T) -> Self {
        Self(Atomic::new(value))
    }
}

unsafe impl<T: NoUninit + Trace> Trace for AtomicLock<T> {
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        // SAFETY:
        //
        // One of the safety requirements for implementing `Trace` is that interior mutability
        // is not performed during `trace` or `process_weak_refs` so we can safely cast pointer
        // to mutable pointer
        unsafe {
            let value = &mut *(self.as_ptr() as *mut T);
            value.trace(visitor);
        }
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::rsgc::WeakProcessor) {
        // SAFETY:
        //
        // One of the safety requirements for implementing `Trace` is that interior mutability
        // is not performed during `trace` or `process_weak_refs` so we can safely cast pointer
        // to mutable pointer
        unsafe {
            let value = &mut *(self.as_ptr() as *mut T);
            value.process_weak_refs(weak_processor);
        }
    }
}
