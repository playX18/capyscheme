use std::cell::Cell;
use std::collections::HashSet;

use crate::rsgc::alloc::Array;
use crate::rsgc::collection::Visitor;
use crate::rsgc::{Mutation, NarrowGc, ObjectSlot};
use crate::rsgc::{
    ptr::Gc,
    weak::{Weak, WeakProcessor},
};
use atomic::Atomic;
use mmtk::{
    util::{Address, ObjectReference},
    vm::SlotVisitor,
};

/// Indicates that a type can be traced by a garbage collector.
///
/// # Safety
///
/// Implementation of this trait must trace everything that could contain
/// garbage collected pointers or other `Trace` items.
pub unsafe trait Trace {
    /// Trace each field in the type.
    ///
    /// # Safety
    ///
    /// Types like [`Gc`], [`Weak`], [`Ephemeron`] need special actions when they're traced,
    /// but those are usually handled by the GC on their own. This function essentially has to
    /// trace *every* field type of which implements `Trace` and must not skip any garbage collected
    /// pointers.
    ///
    /// There is also list of permitted and prohibited behaviors:
    /// ### Permitted
    /// - Reading memory of an object (aka accessing object immutably).
    ///   - Interior mutation is undefined behavior, even when using [`Lock`] or similar types.
    ///
    /// - Calling [Visitor] methods.
    /// ### Prohibited behavior
    /// - Forgetting fields/elements to trace
    ///   - If element/field is forgotten UB may result due to the fact that GC might move some objects around the memory or free them and forgotten pointers would be invalid after the GC cycle.
    ///
    /// - Mutation of any object data is undefined behavior. `&mut self` is here only so that GC can update pointers when necessary.
    unsafe fn trace(&mut self, visitor: &mut Visitor);

    /// Process weak references inside an object.
    ///
    /// This function can not only be used to break simple weak-references but to also implement ephemerons,
    /// resurrect objects, and so much more. To resurrect an object get a visitor by calling [`WeakProcessor::visitor`]
    /// and trace the object with it.
    ///
    /// # Safety
    ///
    /// All internal weak-refs must be processed by `weak_processor`. If there are any weak references
    /// which are not processed UB might result.
    unsafe fn process_weak_refs(&mut self, weak_processor: &mut WeakProcessor);
}

unsafe impl<'gc, T> Trace for Gc<'gc, T> {
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        visitor.visit_slot(crate::rsgc::ObjectSlot::from_address(Address::from_ref(
            self,
        )));
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut WeakProcessor) {
        // there are no weak-refs in GC pointer itself.
        let _ = weak_processor;
    }
}

unsafe impl<'gc, T> Trace for Weak<'gc, T> {
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        visitor.register_for_weak_processing();
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut WeakProcessor) {
        weak_processor.process_weak(self);
    }
}

unsafe impl<T: Trace> Trace for Option<T> {
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        let Some(value) = self.as_mut() else { return };
        unsafe { value.trace(visitor) };
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut WeakProcessor) {
        let Some(value) = self.as_mut() else { return };
        unsafe { value.process_weak_refs(weak_processor) };
    }
}

macro_rules! impl_trace_tuple {
    () => {};
    ($first:ident $(,$rest:ident)*) => {
        unsafe impl<$first: Trace, $($rest: Trace),*> Trace for ($first, $($rest),*) {
            #[allow(non_snake_case)]
            unsafe fn trace(&mut self, visitor: &mut Visitor<'_>) {

                let ($first, $($rest),*) = self;
                unsafe {
                    $first.trace(visitor);
                    $($rest.trace(visitor);)*
                }
            }

            #[allow(non_snake_case)]
            unsafe fn process_weak_refs(&mut self, weak_processor: &mut WeakProcessor) {
                let ($first, $($rest),*) = self;

                unsafe {
                    $first.process_weak_refs(weak_processor);
                    $($rest.process_weak_refs(weak_processor);)*
                }
            }
        }


        impl_trace_tuple!($($rest),*);
    }
}

impl_trace_tuple!(
    A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z, AA, AB, AC, AD,
    AE, AF, AG, AH, AI, AJ, AK, AL, AM, AN, AO, AP, AQ, AR, AS, AT, AU, AV, AW, AX, AY, AZ
);

macro_rules! impl_trace {
    ($($t:ty)*) => {
        $(
            unsafe impl Trace for $t {
                unsafe fn trace(&mut self, visitor: &mut Visitor<'_>) {
                    let _ = visitor;
                }

                unsafe fn process_weak_refs(&mut self, weak_processor: &mut WeakProcessor) {
                    let  _ = weak_processor;
                }
            }
        )*

    };
}

impl_trace!(
    ()
    bool
    u8 i8
    u16 i16
    u32 i32
    u64 i64
    usize isize
    f32 f64
    char
    String
    std::ffi::CString
    std::ffi::OsString
    ObjectReference
    u128 i128
    std::fs::File
    std::path::PathBuf
    std::net::SocketAddr
    std::net::SocketAddrV4
    std::net::SocketAddrV6
    std::net::IpAddr
    std::net::Ipv4Addr
    std::net::Ipv6Addr
    std::net::TcpStream
    std::net::TcpListener
    std::net::UdpSocket
    std::net::Shutdown
    std::collections::hash_map::RandomState
    std::num::NonZeroI8
    std::num::NonZeroI16
    std::num::NonZeroI32
    std::num::NonZeroI64
    std::num::NonZeroI128
    std::num::NonZeroU8
    std::num::NonZeroU16
    std::num::NonZeroU32
    std::num::NonZeroU64
    std::num::NonZeroU128
);

unsafe impl<T: Trace> Trace for [T] {
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        for val in self.iter_mut() {
            unsafe { val.trace(visitor) };
        }
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut WeakProcessor) {
        for val in self.iter_mut() {
            unsafe { val.process_weak_refs(weak_processor) };
        }
    }
}

unsafe impl<T: Trace, const N: usize> Trace for [T; N] {
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        for val in self.iter_mut() {
            unsafe { val.trace(visitor) };
        }
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut WeakProcessor) {
        for val in self.iter_mut() {
            unsafe { val.process_weak_refs(weak_processor) };
        }
    }
}

unsafe impl<T: Trace> Trace for Box<T> {
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        unsafe {
            (**self).trace(visitor);
        }
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut WeakProcessor) {
        unsafe {
            (**self).process_weak_refs(weak_processor);
        }
    }
}

// TODO: How to trace keys of hashmap safely?

unsafe impl<K: Trace, V: Trace, S> Trace for std::collections::HashMap<K, V, S> {
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        for (key, value) in self.iter_mut() {
            unsafe {
                let ptr = key as *const K as *mut K;
                (*ptr).trace(visitor);
                value.trace(visitor);
            }
        }
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut WeakProcessor) {
        for (key, value) in self.iter_mut() {
            unsafe {
                let ptr = key as *const K as *mut K;
                (*ptr).process_weak_refs(weak_processor);
                value.process_weak_refs(weak_processor);
            }
        }
    }
}

unsafe impl<K: Trace, V: Trace> Trace for std::collections::BTreeMap<K, V> {
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        for (key, value) in self.iter_mut() {
            unsafe {
                let ptr = key as *const K as *mut K;
                (*ptr).trace(visitor);
                value.trace(visitor);
            }
        }
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut WeakProcessor) {
        for (key, value) in self.iter_mut() {
            unsafe {
                let ptr = key as *const K as *mut K;
                (*ptr).process_weak_refs(weak_processor);
                value.process_weak_refs(weak_processor);
            }
        }
    }
}

unsafe impl<T: Trace> Trace for Vec<T> {
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        for val in self.iter_mut() {
            unsafe { val.trace(visitor) };
        }
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut WeakProcessor) {
        for val in self.iter_mut() {
            unsafe { val.process_weak_refs(weak_processor) };
        }
    }
}

unsafe impl<T: Trace> Trace for std::collections::VecDeque<T> {
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        for val in self.iter_mut() {
            unsafe { val.trace(visitor) };
        }
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut WeakProcessor) {
        for val in self.iter_mut() {
            unsafe { val.process_weak_refs(weak_processor) };
        }
    }
}

unsafe impl<T: Trace> Trace for std::collections::LinkedList<T> {
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        for val in self.iter_mut() {
            unsafe { val.trace(visitor) };
        }
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut WeakProcessor) {
        for val in self.iter_mut() {
            unsafe { val.process_weak_refs(weak_processor) };
        }
    }
}

unsafe impl<T: Trace> Trace for Cell<T> {
    unsafe fn process_weak_refs(&mut self, weak_processor: &mut WeakProcessor) {
        unsafe {
            self.get_mut().process_weak_refs(weak_processor);
        }
    }

    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        unsafe {
            self.get_mut().trace(visitor);
        }
    }
}

unsafe impl<T: Trace> Trace for Atomic<T> {
    unsafe fn process_weak_refs(&mut self, weak_processor: &mut WeakProcessor) {
        unsafe {
            let ptr = self as *mut Atomic<T> as *mut T;
            (*ptr).process_weak_refs(weak_processor);
        }
    }

    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        unsafe {
            let ptr = self as *mut Atomic<T> as *mut T;
            (*ptr).trace(visitor);
        }
    }
}

unsafe impl Trace for str {
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        let _ = visitor;
    }
    unsafe fn process_weak_refs(&mut self, weak_processor: &mut WeakProcessor) {
        let _ = weak_processor;
    }
}

unsafe impl Trace for &str {
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        let _ = visitor;
    }
    unsafe fn process_weak_refs(&mut self, weak_processor: &mut WeakProcessor) {
        let _ = weak_processor;
    }
}

unsafe impl<V: Trace> Trace for HashSet<V> {
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        unsafe {
            for value in self.iter() {
                let value = value as *const V as *mut V;
                (*value).trace(visitor);
            }
        }
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut WeakProcessor) {
        unsafe {
            for value in self.iter() {
                let value = value as *const V as *mut V;
                (*value).process_weak_refs(weak_processor);
            }
        }
    }
}

unsafe impl<'gc, T: Trace> Trace for NarrowGc<'gc, T> {
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        let addr = unsafe { Address::from_usize(self as *const Self as usize | 1) };
        visitor.visit_slot(ObjectSlot::from_address(addr));
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut WeakProcessor) {
        let _ = weak_processor;
    }
}

pub trait FromIteratorGc<'gc, A>: Sized {
    fn from_iter_gc<T>(mc: &Mutation<'gc>, iter: T) -> Gc<'gc, Self>
    where
        T: IntoIterator<Item = A>;
}

impl<'gc, A: Trace + Clone> FromIteratorGc<'gc, A> for Array<A> {
    fn from_iter_gc<T>(mc: &Mutation<'gc>, iter: T) -> Gc<'gc, Self>
    where
        T: IntoIterator<Item = A>,
    {
        Array::from_iter(mc, iter)
    }
}

pub trait IterGc<'gc> {
    type Item;

    fn collect_gc<T>(self, mc: &Mutation<'gc>) -> Gc<'gc, T>
    where
        T: FromIteratorGc<'gc, Self::Item>;
}

impl<'gc, T: Iterator<Item = A>, A: Trace> IterGc<'gc> for T {
    type Item = A;

    fn collect_gc<I>(self, mc: &Mutation<'gc>) -> Gc<'gc, I>
    where
        I: FromIteratorGc<'gc, Self::Item>,
    {
        I::from_iter_gc(mc, self)
    }
}
