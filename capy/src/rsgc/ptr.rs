//! Garbage Collected pointers

use crate::rsgc::{
    Weak,
    barrier::{Unlock, Write},
    mutator::Mutation,
    object::GCObject,
    traits::Trace,
};
use mmtk::{
    util::{Address, ObjectReference},
    vm::slot::Slot,
};
use std::{
    borrow::Borrow,
    cell::Cell,
    fmt,
    hash::Hash,
    marker::PhantomData,
    mem::MaybeUninit,
    num::NonZeroU32,
    ops::{Deref, Index},
    ptr::NonNull,
};

pub(crate) type Invariant<'a> = PhantomData<Cell<&'a ()>>;

#[repr(transparent)]
#[derive()]
pub struct Gc<'gc, T: 'gc> {
    ptr: NonNull<T>,
    pd: Invariant<'gc>,
}

unsafe impl<'gc, T: Send> Send for Gc<'gc, T> {}
unsafe impl<'gc, T: Sync> Sync for Gc<'gc, T> {}

impl<'gc, T> Copy for Gc<'gc, T> {}
impl<'gc, T> Clone for Gc<'gc, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'gc, T: PartialEq> PartialEq for Gc<'gc, T> {
    fn eq(&self, other: &Self) -> bool {
        **self == **other
    }
}

impl<'gc, T: Eq> Eq for Gc<'gc, T> {}

impl<'gc, T: PartialOrd> PartialOrd for Gc<'gc, T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        (**self).partial_cmp(&**other)
    }
}

impl<'gc, T: Ord> Ord for Gc<'gc, T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        (**self).cmp(&**other)
    }
}

impl<'gc, T> Deref for Gc<'gc, T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        unsafe { self.ptr.as_ref() }
    }
}

impl<'gc, T: Hash> Hash for Gc<'gc, T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (**self).hash(state);
    }
}

impl<'gc, T: fmt::Debug> fmt::Debug for Gc<'gc, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        (**self).fmt(f)
    }
}

impl<'gc, T: fmt::Display> fmt::Display for Gc<'gc, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        (**self).fmt(f)
    }
}

impl<'gc, T> fmt::Pointer for Gc<'gc, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <NonNull<T> as fmt::Pointer>::fmt(&self.ptr, f)
    }
}

impl<'gc, T: fmt::LowerHex> fmt::LowerHex for Gc<'gc, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        (**self).fmt(f)
    }
}

impl<'gc, T: fmt::UpperHex> fmt::UpperHex for Gc<'gc, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        (**self).fmt(f)
    }
}

impl<'gc, T> Gc<'gc, T> {
    /// Create `Gc` pointer from raw pointer.
    ///
    /// # Safety
    ///
    /// `ptr` must be valid, and coming from the GC heap.
    ///
    /// # Panics
    ///
    /// Panics if `ptr` is null.
    pub unsafe fn from_ptr(ptr: *const T) -> Self {
        Self {
            ptr: NonNull::new(ptr as _).unwrap(),
            pd: PhantomData,
        }
    }

    /// Create `Gc` pointer from `GCObject`.
    ///
    /// # Safety
    ///
    /// `obj` must be of type `T` and must be valid, and coming from the GC heap.
    ///
    /// # Panics
    ///
    /// Panics if `obj` is null.
    pub unsafe fn from_gcobj(obj: GCObject) -> Self {
        Self {
            ptr: NonNull::new(obj.to_address().to_mut_ptr()).unwrap(),
            pd: PhantomData,
        }
    }

    pub fn as_gcobj(self) -> GCObject {
        GCObject::from_objref_nullable(Some(self.to_object_reference()))
    }

    pub fn as_ptr(self) -> *const T {
        self.ptr.as_ptr()
    }

    /// Check that identities of `this` and `other` are equal. Internally
    /// simply compares raw pointers.
    pub fn ptr_eq(this: Self, other: Self) -> bool {
        this.as_ptr() == other.as_ptr()
    }

    /// Compute address based hashcode for GC object. This function is guaranteed
    /// to return the same hash even if object was moved by the GC.
    ///
    /// ## Note
    ///
    /// This function is implemented by shifting address of object right by [`LOG_BYTES_IN_ADDRESS`] bytes
    /// and then setting object hash status to `HASHED`. If object is moved by GC another word is allocated
    /// at the beginning of the object to store original hashcode.
    ///
    /// - [`LOG_BYTES_IN_ADDRESS`](mmtk::util::constants::LOG_BYTES_IN_ADDRESS);
    ///
    pub fn ptr_hash(this: Self) -> u64 {
        this.as_gcobj().hashcode()
    }

    /// Allocate `value` on the GC heap.
    pub fn new(mc: Mutation<'gc>, value: T) -> Gc<'gc, T>
    where
        T: Trace,
    {
        mc.allocate(value, mmtk::AllocationSemantics::Default)
    }

    pub fn to_object_reference(self) -> ObjectReference {
        unsafe { ObjectReference::from_raw_address_unchecked(Address::from_ptr(self.ptr.as_ptr())) }
    }

    /// Create `Gc` pointer from `ObjectReference`.
    ///
    /// # Safety
    ///
    /// `objref` must be of type `T` and must be valid, and coming from the GC heap.
    pub unsafe fn from_object_reference(objref: ObjectReference) -> Self {
        unsafe {
            Self {
                ptr: NonNull::new_unchecked(objref.to_raw_address().as_mut_ref()),
                pd: PhantomData,
            }
        }
    }

    /// Downgrade strong `Gc` pointer into `Weak` pointer. `Weak` pointers
    /// are cleared by GC if no more references exist to the object.
    pub fn downgrade(this: Self) -> Weak<'gc, T> {
        Weak {
            ptr: Some(this.ptr),
            pd: PhantomData,
        }
    }

    /// Triggers a write barrier on this `Gc`, allowing for safe mutation.
    ///
    ///
    /// At the moment this triggers object remembering barrier which is used by generatonal
    /// plans in MMTk. Technically it is a *post* write barrier (meaning it should be called after the write)
    /// but in practice it does not matter. In future, when MMTk gets LXR or any other GC where
    /// write barrier order is strictly enforced this function will be behind a feature flag.
    pub fn write(mc: Mutation<'gc>, this: Self) -> &'gc Write<T> {
        unsafe {
            mc.raw_object_reference_write(
                this.as_gcobj(),
                ObjectSlot::from_address(Address::ZERO),
                GCObject::NULL,
            );
            Write::assume(this.as_ref())
        }
    }

    pub fn compress(mc: Mutation<'gc>, this: Self) -> NarrowGc<'gc, T> {
        let thread = mc.thread();
        let addr = (this.to_object_reference().to_raw_address() - thread.heap_base().as_usize())
            >> thread.heap_shift() as usize;
        unsafe { NarrowGc::from_compressed_ptr(addr as u32) }
        // ((o.to_raw_address() - compressed_heap_base().as_usize())
        //    >> compressed_heap_shift() as usize) as u32
    }

    #[allow(clippy::should_implement_trait)]
    pub fn as_ref(self) -> &'gc T {
        unsafe { self.ptr.as_ref() }
    }
}

impl<'gc, T: Unlock + 'gc> Gc<'gc, T> {
    pub fn unlock(self, mc: Mutation<'gc>) -> &'gc T::Unlocked {
        Gc::write(mc, self);
        unsafe { self.ptr.as_ref().unlock_unchecked() }
    }
}

impl<'gc, U, T> AsRef<U> for Gc<'gc, T>
where
    T: AsRef<U>,
{
    fn as_ref(&self) -> &U {
        (**self).as_ref()
    }
}

impl<'gc, T: Trace> Gc<'gc, MaybeUninit<T>> {
    /// Assume that `self` is initialized and return `Gc` pointer to the initialized object.
    ///
    /// # Safety
    ///
    /// Same safety requirements as for [`MaybeUninit::assume_init`].
    pub unsafe fn assume_init(self) -> Gc<'gc, T> {
        Gc {
            ptr: self.ptr.cast(),
            pd: PhantomData,
        }
    }

    pub fn as_mut_ptr(self) -> *mut T {
        self.ptr.as_ptr() as *mut T
    }
}

pub struct NarrowGc<'gc, T> {
    inv: PhantomData<&'gc T>,
    ptr: NonZeroU32,
}

impl<'gc, T> NarrowGc<'gc, T> {
    pub const fn as_compressed_ptr(self) -> u32 {
        self.ptr.get()
    }

    pub unsafe fn from_compressed_ptr(ptr: u32) -> Self {
        Self {
            inv: PhantomData,
            ptr: NonZeroU32::new(ptr).expect("compressed pointer must be non-zero"),
        }
    }

    #[inline(always)]
    pub fn decompress(self, mc: Mutation<'gc>) -> Gc<'gc, T> {
        unsafe {
            let thread = mc.thread();

            let addr = thread.heap_base() + ((self.ptr.get() as usize) << thread.heap_shift());

            Gc::from_object_reference(ObjectReference::from_raw_address_unchecked(addr))
        }
    }

    #[inline(always)]
    pub fn write(mc: Mutation<'gc>, this: Self) -> &'gc Write<T> {
        let gc = this.decompress(mc);
        Gc::write(mc, gc)
    }
}

impl<'gc, T, C: Borrow<Mutation<'gc>>> Index<C> for NarrowGc<'gc, T> {
    type Output = T;
    #[inline(always)]
    fn index(&self, index: C) -> &Self::Output {
        let mc = index.borrow();
        unsafe {
            let thread = mc.thread();
            let addr = thread.heap_base() + ((self.ptr.get() as usize) << thread.heap_shift());

            addr.as_ref()
        }
    }
}

impl<'gc, T> std::fmt::Pointer for NarrowGc<'gc, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "0x{:x}", self.ptr.get())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(transparent)]
pub struct ObjectSlot {
    pub addr: Address,
}

impl<'gc, T> From<&mut Gc<'gc, T>> for ObjectSlot {
    fn from(gc: &mut Gc<'gc, T>) -> Self {
        Self::from_address(Address::from_ref(gc))
    }
}

impl<'gc, T> From<&mut NarrowGc<'gc, T>> for ObjectSlot {
    fn from(narrow_gc: &mut NarrowGc<'gc, T>) -> Self {
        Self::from_address(unsafe {
            Address::from_usize(narrow_gc as *mut NarrowGc<_> as usize | 1)
        })
    }
}

impl ObjectSlot {
    pub const fn from_address(addr: Address) -> Self {
        Self { addr }
    }

    pub const LOG_BYTES_IN_SLOT: usize = 3;
    pub const BYTES_IN_SLOT: usize = 1 << Self::LOG_BYTES_IN_SLOT;

    pub const MASK: usize = 1usize << (usize::BITS - 1);

    pub const fn is_compressed(&self) -> bool {
        self.addr.as_usize() & 1 == 1
    }

    pub const fn untagged_address(&self) -> Address {
        unsafe { Address::from_usize(self.addr.as_usize() & !1usize) }
    }

    pub fn store_null(&self) {
        unsafe { self.addr.store(0usize) }
    }
}

impl Slot for ObjectSlot {
    fn load(&self) -> Option<ObjectReference> {
        unsafe { self.addr.load() }
    }

    fn store(&self, object: ObjectReference) {
        unsafe { self.addr.store(object.to_raw_address().as_usize()) }
    }
}
