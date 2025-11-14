use std::{
    alloc::Layout,
    fmt,
    hash::Hash,
    mem::MaybeUninit,
    ops::{Deref, Index, Range, RangeFrom, RangeFull, RangeInclusive, RangeTo, RangeToInclusive},
};

use mmtk::{AllocationSemantics, util::conversions::raw_align_up};

use crate::rsgc::collection::Visitor;
use crate::rsgc::{
    Gc, Mutation, WeakProcessor,
    barrier::IndexWrite,
    object::{GCObject, VTable},
    traits::Trace,
};

/// A GC allocated array of values.
///
/// Size of array is static and can only be set at creation time.
#[repr(C)]
pub struct Array<T: Trace> {
    len: usize,
    data: [MaybeUninit<T>; 1],
}

pub type ArrayRef<'gc, T> = Gc<'gc, Array<T>>;

unsafe impl<T: Trace> Trace for Array<T> {
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        for value in self.as_slice_mut().iter_mut() {
            unsafe { value.trace(visitor) };
        }
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::rsgc::WeakProcessor) {
        for value in self.as_slice_mut().iter_mut() {
            unsafe { value.process_weak_refs(weak_processor) };
        }
    }
}

extern "C" fn array_size<T: Trace>(obj: GCObject) -> usize {
    unsafe {
        let arr = obj.to_address().as_ref::<Array<T>>();

        let layout = Layout::array::<T>(arr.len());

        raw_align_up(
            layout.unwrap().size() + size_of::<Array<T>>() - size_of::<T>(),
            align_of::<Array<T>>(),
        )
    }
}

extern "C" fn array_trace<T: Trace>(obj: GCObject, vis: &mut Visitor) {
    unsafe {
        let arr = obj.to_address().as_mut_ref::<Array<T>>();
        arr.trace(vis);
    }
}

extern "C" fn array_process_weaks<T: Trace>(obj: GCObject, weak_processor: &mut WeakProcessor) {
    unsafe {
        let arr = obj.to_address().as_mut_ref::<Array<T>>();
        arr.process_weak_refs(weak_processor);
    }
}

impl<T: Trace> Array<T> {
    pub const fn array_size(len: usize) -> usize {
        size_of::<Array<T>>() + (size_of::<T>() * len) - size_of::<T>()
    }

    pub const VTABLE: &'static VTable = &VTable {
        type_name: "Array<T>",
        instance_size: 0,
        compute_size: Some(array_size::<T>),
        compute_alignment: None,
        alignment: align_of::<Array<T>>(),
        trace: array_trace::<T>,
        weak_proc: array_process_weaks::<T>,
    };

    /// Allocates new array on heap with specified semantics, length `len` and a closure `f`
    /// which returns value for specified index.
    pub fn with_semantics<'gc, F>(
        mc: Mutation<'gc>,
        semantics: AllocationSemantics,
        len: usize,
        mut f: F,
    ) -> Gc<'gc, Self>
    where
        F: FnMut(Mutation<'gc>, usize) -> T,
    {
        let size = Self::array_size(len);
        let align = align_of::<Self>();

        let alloc = unsafe { mc.raw_allocate(size, align, Self::VTABLE, semantics) };

        // SAFETY:
        // - alloc is a valid allocation with zeroed out memory
        // - alloc can hold up to `len` elements.
        unsafe {
            let this = alloc.to_address().as_mut_ref::<Self>();
            this.len = len;

            for i in 0..len {
                this.data
                    .as_mut_ptr()
                    .add(i)
                    .write(MaybeUninit::new(f(mc, i)));
            }

            Gc::from_gcobj(alloc)
        }
    }

    pub fn with<'gc, F>(mc: Mutation<'gc>, len: usize, f: F) -> Gc<'gc, Self>
    where
        F: FnMut(Mutation<'gc>, usize) -> T,
    {
        Self::with_semantics(mc, AllocationSemantics::Default, len, f)
    }

    pub fn from_slice<'gc>(mc: Mutation<'gc>, arr: impl AsRef<[T]>) -> Gc<'gc, Self>
    where
        T: Clone,
    {
        let arr = arr.as_ref();

        Self::with(mc, arr.len(), |_, index| arr[index].clone())
    }

    pub fn from_iter<'gc>(mc: Mutation<'gc>, iter: impl IntoIterator<Item = T>) -> Gc<'gc, Self>
    where
        T: Clone,
    {
        let mut iter = iter.into_iter();
        let (lower, upper) = iter.size_hint();
        if Some(lower) == upper {
            let size = lower;

            Self::with(mc, size, |_, _| {
                iter.next().expect("Iterator should have enough elements")
            })
        } else {
            Self::from_slice(mc, iter.collect::<Vec<_>>())
        }
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    pub fn as_ptr(&self) -> *const T {
        self.data.as_ptr().cast()
    }

    pub fn as_slice(&self) -> &[T] {
        unsafe { std::slice::from_raw_parts(self.as_ptr(), self.len()) }
    }

    pub fn as_slice_mut(&mut self) -> &mut [T] {
        unsafe { std::slice::from_raw_parts_mut(self.data.as_mut_ptr() as _, self.len) }
    }
}

impl<T: Trace> AsRef<[T]> for Array<T> {
    fn as_ref(&self) -> &[T] {
        self.as_slice()
    }
}

impl<T: Trace> Deref for Array<T> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        self.as_slice()
    }
}

impl<T: Trace> Index<usize> for Array<T> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        &self.as_slice()[index]
    }
}

impl<T: Trace> Index<RangeTo<usize>> for Array<T> {
    type Output = [T];

    fn index(&self, index: RangeTo<usize>) -> &Self::Output {
        &self.as_slice()[index]
    }
}

impl<T: Trace> Index<RangeFrom<usize>> for Array<T> {
    type Output = [T];

    fn index(&self, index: RangeFrom<usize>) -> &Self::Output {
        &self.as_slice()[index]
    }
}

impl<T: Trace> Index<RangeFull> for Array<T> {
    type Output = [T];

    fn index(&self, _index: RangeFull) -> &Self::Output {
        self.as_slice()
    }
}

impl<T: Trace> Index<RangeInclusive<usize>> for Array<T> {
    type Output = [T];

    fn index(&self, index: RangeInclusive<usize>) -> &Self::Output {
        &self.as_slice()[index]
    }
}

impl<T: Trace> Index<RangeToInclusive<usize>> for Array<T> {
    type Output = [T];

    fn index(&self, index: RangeToInclusive<usize>) -> &Self::Output {
        &self.as_slice()[index]
    }
}

impl<T: Trace> Index<Range<usize>> for Array<T> {
    type Output = [T];

    fn index(&self, index: Range<usize>) -> &Self::Output {
        &self.as_slice()[index]
    }
}

unsafe impl<T: Trace> IndexWrite<usize> for Array<T> {}
unsafe impl<T: Trace> IndexWrite<Range<usize>> for Array<T> {}
unsafe impl<T: Trace> IndexWrite<RangeFrom<usize>> for Array<T> {}
unsafe impl<T: Trace> IndexWrite<RangeTo<usize>> for Array<T> {}
unsafe impl<T: Trace> IndexWrite<RangeToInclusive<usize>> for Array<T> {}
unsafe impl<T: Trace> IndexWrite<RangeInclusive<usize>> for Array<T> {}

impl<T: fmt::Debug + Trace> fmt::Debug for Array<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.as_slice())
    }
}

impl<T: Hash + Trace> Hash for Array<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        for value in self.iter() {
            value.hash(state);
        }
    }
}

impl<T: PartialEq + Trace> PartialEq for Array<T> {
    fn eq(&self, other: &Self) -> bool {
        self.as_slice().eq(other.as_slice())
    }
}

impl<T: Eq + Trace> Eq for Array<T> {}

impl<T: PartialOrd + Trace> PartialOrd for Array<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.as_slice().partial_cmp(other.as_slice())
    }
}

impl<T: Ord + Trace> Ord for Array<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.as_slice().cmp(other.as_slice())
    }
}
