use crate::rsgc::collection::{Visitor, VisitorKind};
use crate::rsgc::{Gc, mm::MemoryManager, object::GCObject, traits::Trace};
use crate::rsgc::{Mutation, ObjectSlot};
use mmtk::{
    scheduler::GCWorker,
    util::{Address, ObjectReference},
    vm::{ObjectTracer, ObjectTracerContext, SlotVisitor, slot::Slot},
};
use parking_lot::Mutex;
use std::sync::atomic::AtomicU32;
use std::{
    collections::{HashMap, HashSet},
    fmt,
    marker::PhantomData,
    ptr::NonNull,
};

pub struct Ephemeron<'gc, K, V: Trace> {
    key: Weak<'gc, K>,
    value: Option<V>,
}

impl<'gc, K, V: Trace> Ephemeron<'gc, K, V> {
    pub fn new(key: impl Into<Weak<'gc, K>>, value: V) -> Self {
        Self {
            key: key.into(),
            value: Some(value),
        }
    }

    pub fn broken() -> Self {
        Self {
            key: Weak::new(),
            value: None,
        }
    }

    pub fn is_broken(&self) -> bool {
        self.key.is_broken()
    }

    pub fn key(&self) -> Weak<'gc, K> {
        self.key
    }

    pub fn value(&self) -> Option<&V> {
        self.value.as_ref()
    }
}

unsafe impl<'gc, K, V: Trace> Trace for Ephemeron<'gc, K, V> {
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        visitor.register_for_weak_processing();
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut WeakProcessor) {
        // SAFETY: `key` is a valid weak-reference and it might be broken by weak-processor.
        unsafe { self.key.process_weak_refs(weak_processor) };

        if self.key.is_broken() {
            self.value = None;
        }

        let Some(value) = self.value.as_mut() else {
            return;
        };
        let mut vis = weak_processor.visitor();

        // SAFETY: It is perfectly safe to trace the value due to the fact
        // that even if value or it's internals were not traced by GC they
        // will be traced after this function call
        unsafe {
            value.trace(&mut vis);
        }
    }
}

impl<'gc, K: fmt::Debug, V: fmt::Debug + Trace> fmt::Debug for Ephemeron<'gc, K, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Ephemeron").finish()
    }
}

pub struct Weak<'gc, T: ?Sized> {
    pub(crate) ptr: Option<NonNull<T>>,
    pub(crate) pd: PhantomData<&'gc mut T>,
}

impl<'gc> fmt::Debug for Weak<'gc, dyn Trace> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Weak").field("ptr", &self.ptr).finish()
    }
}

impl<'gc, T: ?Sized> Copy for Weak<'gc, T> {}
impl<'gc, T: ?Sized> Clone for Weak<'gc, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'gc, T> Weak<'gc, T> {
    pub const fn new() -> Self {
        Self {
            ptr: None,
            pd: PhantomData,
        }
    }

    pub fn upgrade(self, mc: &Mutation<'gc>) -> Option<Gc<'gc, T>> {
        self.ptr.map(|ptr| unsafe {
            mc.thread().mutator_unchecked().barrier.load_weak_reference(
                ObjectReference::from_raw_address_unchecked(Address::from_ptr(ptr.as_ptr())),
            );
            Gc::from_ptr(ptr.as_ptr())
        })
    }

    /// Upgrade without applying any barriers.
    ///
    /// # Safety
    ///
    /// May cause UB if concurrent GC is in progress.
    pub unsafe fn upgrade_unchecked(self) -> Option<Gc<'gc, T>> {
        self.ptr.map(|ptr| unsafe { Gc::from_ptr(ptr.as_ptr()) })
    }

    pub fn is_broken(self) -> bool {
        self.ptr.is_none()
    }
}

impl<'gc, T> Default for Weak<'gc, T> {
    fn default() -> Self {
        Self::new()
    }
}

pub struct WeakProcessor<'a> {
    pub(crate) tracer: &'a mut dyn ObjectTracer,
    pub(crate) has_any: bool,
}

impl<'a> WeakProcessor<'a> {
    /// Process weak pointer by either breaking it or keeping it alive.
    ///
    /// If weak pointer is kept alive, pointer might be moved around memory by moving GC.
    pub fn process_weak<T: Sized>(&mut self, weak: &mut Weak<'_, T>) {
        let Some(objref) = weak
            .ptr
            .map(|p| p.as_ptr() as *const u8)
            .map(Address::from_ptr)
            .and_then(ObjectReference::from_raw_address)
        else {
            weak.ptr = None;
            return;
        };

        if objref.is_reachable() {
            let new_objref = objref.get_forwarded_object().unwrap_or(objref);

            weak.ptr = NonNull::new(new_objref.to_raw_address().to_mut_ptr());
        } else {
            weak.ptr = None;
        }
    }

    pub fn process<T: Trace>(&mut self, value: &mut T) {
        unsafe {
            value.process_weak_refs(self);
        }
    }

    /// Checks if object is live and potentially returns new reference, or old but live reference
    /// OR `GCObject::NULL` if object is dead.
    ///
    /// # Safety
    ///
    /// Caller must ensure that `object` is a valid GC object and also check the return value appropriately.
    #[must_use]
    pub unsafe fn is_live_object(&self, object: GCObject) -> GCObject {
        let Some(objref) = object.to_objref() else {
            return GCObject::NULL;
        };

        if objref.is_reachable() {
            let new = objref.get_forwarded_object().unwrap_or(objref);

            GCObject::from_objref_nullable(Some(new))
        } else {
            GCObject::NULL
        }
    }

    pub fn visitor<'b>(&'b mut self) -> Visitor<'b> {
        unsafe {
            Visitor::new(
                VisitorKind::Slot(self as &mut dyn SlotVisitor<ObjectSlot>),
                None,
            )
        }
    }
}

impl<'a> SlotVisitor<ObjectSlot> for WeakProcessor<'a> {
    fn visit_slot(&mut self, slot: ObjectSlot) {
        let Some(obj) = slot.load() else {
            return;
        };

        let new = self.tracer.trace_object(obj);
        slot.store(new);
    }
}

pub enum WeakCallback {
    Object(ObjectReference),
    Closure(Box<dyn FnOnce(&mut WeakProcessor) + Send + Sync>),
}

static TOKEN: AtomicU32 = AtomicU32::new(0);

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub struct WeakCallbackToken(u32);

type WeakCallbackMap = HashMap<WeakCallbackToken, Box<dyn Fn(&mut WeakProcessor) + Send + Sync>>;

pub(crate) struct WeakProcessingState {
    #[allow(dead_code)]
    weak_callbacks: Mutex<WeakCallbackMap>,
    objects_with_weak_refs: Mutex<HashSet<ObjectReference>>,
    roots_with_weak_refs: Mutex<Vec<*mut dyn Trace>>,
}

impl WeakProcessingState {
    pub(crate) fn new() -> Self {
        Self {
            weak_callbacks: Mutex::new(HashMap::new()),
            objects_with_weak_refs: Mutex::new(HashSet::new()),
            roots_with_weak_refs: Mutex::new(Vec::new()),
        }
    }

    pub fn add_object_with_weak_ref(&self, obj: ObjectReference) {
        self.objects_with_weak_refs.lock().insert(obj);
    }

    pub fn add_root_with_weak_ref(&self, obj: *mut dyn Trace) {
        self.roots_with_weak_refs.lock().push(obj);
    }

    pub fn run(
        &self,
        worker: &mut GCWorker<MemoryManager>,
        tracer_context: &mut impl ObjectTracerContext<MemoryManager>,
    ) -> bool {
        tracer_context.with_tracer(worker, |tracer| {
            let mut weak_processor = WeakProcessor {
                tracer,
                has_any: false,
            };

            let mut objects_with_weak_refs = self.objects_with_weak_refs.lock();

            for obj in objects_with_weak_refs.drain() {
                let obj = GCObject::from(obj);
                let header = obj.header();
                let vt = header.vtable();

                (vt.weak_proc)(obj, &mut weak_processor);
            }

            let mut roots = self.roots_with_weak_refs.lock();

            for root in roots.drain(..) {
                let obj = unsafe { &mut *root };
                unsafe {
                    obj.process_weak_refs(&mut weak_processor);
                }
            }

            for callback in self.weak_callbacks.lock().iter() {
                (callback.1)(&mut weak_processor);
            }

            weak_processor.has_any
        })
    }

    pub fn add_weak_callback<F>(&self, callback: F) -> WeakCallbackToken
    where
        F: Fn(&mut WeakProcessor) + Send + Sync + 'static,
    {
        let mut callbacks = self.weak_callbacks.lock();
        let token = WeakCallbackToken(TOKEN.fetch_add(1, std::sync::atomic::Ordering::SeqCst));
        callbacks.insert(token, Box::new(callback));
        token
    }

    pub fn remove_weak_callback(&self, token: WeakCallbackToken) {
        self.weak_callbacks.lock().remove(&token);
    }
}
