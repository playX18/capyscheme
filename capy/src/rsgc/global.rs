use mmtk::vm::RootsWorkFactory;

use crate::rsgc::{
    Mutation, ObjectSlot, Root, Rootable, Trace,
    collection::{Visitor, VisitorKind},
    scanning::RootSlotVisitor,
    sync::monitor::Monitor,
};
use std::{collections::HashMap, ptr::NonNull, sync::atomic::AtomicUsize};

type GlobalMap = HashMap<*const u8, NonNull<GlobalInner<dyn Trace>>>;

pub(crate) struct GlobalRegistry {
    globals: Monitor<GlobalMap>,
}

impl GlobalRegistry {
    pub fn new() -> Self {
        GlobalRegistry {
            globals: Monitor::new(HashMap::new()),
        }
    }

    pub fn register_global(&self, global: NonNull<GlobalInner<dyn Trace>>) {
        self.globals
            .lock()
            .insert(global.as_ptr() as *const u8, global);
    }

    pub fn unregister_global<R>(&self, global: NonNull<R>) {
        self.globals.lock().remove(&(global.as_ptr() as *const u8));
    }

    pub fn scan(&self, mut factory: impl RootsWorkFactory<ObjectSlot>) {
        let mut tracer = RootSlotVisitor::new();

        let mut visitor = unsafe { Visitor::new(VisitorKind::Slot(&mut tracer), None) };

        let globals = self.globals.lock();

        for global in globals.values() {
            let global = unsafe { &mut *global.as_ptr() };

            unsafe {
                global.root.trace(&mut visitor);
            }
            if visitor.has_weak_refs() {
                crate::rsgc::GarbageCollector::get()
                    .weak
                    .add_root_with_weak_ref(&mut global.root as *mut dyn Trace);
            }
            visitor.has_weak_refs = false;
        }

        if !tracer.set.is_empty() {
            factory.create_process_roots_work(tracer.set.into_iter().collect());
        }
    }
}

pub(crate) struct GlobalInner<R: ?Sized> {
    refcount: AtomicUsize,
    root: R,
}

/// A reference counted global root of type `R`.
pub struct Global<R>
where
    R: for<'a> Rootable<'a>,
    for<'a> Root<'a, R>: Sized + Trace,
{
    root: NonNull<GlobalInner<Root<'static, R>>>,
}

unsafe impl<R> Send for Global<R>
where
    R: for<'a> Rootable<'a>,
    for<'a> Root<'a, R>: Sized + Trace,
{
}

unsafe impl<R> Sync for Global<R>
where
    R: for<'a> Rootable<'a>,
    for<'a> Root<'a, R>: Sized + Trace,
{
}

impl<R> Global<R>
where
    R: for<'a> Rootable<'a>,
    for<'a> Root<'a, R>: Sized + Trace,
{
    /// Create a new global root. This function will trace `value` on each GC cycle and preserve
    /// it. Created root is unregistered automatically once instance of this type is dropped.
    pub fn new<'gc>(value: Root<'gc, R>) -> Self {
        let root = Box::leak(Box::new(GlobalInner {
            refcount: AtomicUsize::new(1),
            root: value,
        }));

        let root_ptr: NonNull<GlobalInner<Root<'static, R>>> = NonNull::from(root).cast();
        let dyn_ptr: NonNull<GlobalInner<dyn Trace>> = root_ptr;

        let rsgc = crate::rsgc::GarbageCollector::get();
        rsgc.global_registry.register_global(dyn_ptr);

        Self { root: root_ptr }
    }

    /// Get a temporary access to a global root which is valid for the lifetime
    /// of current heap mutation.
    pub fn fetch<'gc>(&self, mc: &Mutation<'gc>) -> &Root<'gc, R> {
        unsafe {
            let _ = mc;
            let root = &*self.root.as_ptr();
            // SAFETY: `root` should be valid for the lifetime of the mutation.
            std::mem::transmute(&root.root)
        }
    }

    fn inner(&self) -> &GlobalInner<Root<'static, R>> {
        unsafe { &*self.root.as_ptr() }
    }
}

impl<R> Drop for Global<R>
where
    R: for<'a> Rootable<'a>,
    for<'a> Root<'a, R>: Sized + Trace,
{
    fn drop(&mut self) {
        if self
            .inner()
            .refcount
            .fetch_sub(1, std::sync::atomic::Ordering::SeqCst)
            > 1
        {
            return;
        }
        let rsgc = crate::rsgc::GarbageCollector::get();
        rsgc.global_registry.unregister_global(self.root);
        unsafe {
            let _ = Box::from_raw(self.root.as_ptr());
        };
    }
}

impl<R> Clone for Global<R>
where
    R: for<'a> Rootable<'a>,
    for<'a> Root<'a, R>: Sized + Trace,
{
    fn clone(&self) -> Self {
        self.inner()
            .refcount
            .fetch_add(1, std::sync::atomic::Ordering::AcqRel);
        Self { root: self.root }
    }
}
