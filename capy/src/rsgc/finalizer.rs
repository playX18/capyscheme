use std::{
    collections::{HashMap, VecDeque, hash_map::Entry},
    hash::Hash,
    sync::Arc,
};

use mmtk::{
    scheduler::GCWorker,
    util::ObjectReference,
    vm::{ObjectTracer, ObjectTracerContext},
};

use crate::rsgc::{Gc, Trace, mm::MemoryManager, sync::monitor::Monitor};

pub trait FinalizationNotify: 'static {
    fn notify_in_processing(&self);
    fn schedule(&self);
}

/// Finalizer queue for objects that are ready to be finalized.
///
/// # Safety
///
/// Types implementing this queue must not violate rooting rules. When `object` comes into the queue,
/// you should ensure that is reachable by GC.
pub unsafe trait FinalizerQueue: 'static {
    /// Mark object finalizer as ready to run.
    fn mark_ready_to_run(&self, object: ObjectReference);

    /// Schedule execution of finalizers.
    fn schedule(&self);
}

struct Key(Arc<dyn FinalizerQueue>);

impl Hash for Key {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        Arc::as_ptr(&self.0).hash(state);
    }
}

impl PartialEq for Key {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for Key {}

struct State {
    candidates: VecDeque<(ObjectReference, usize)>,
    finalizers_queue_index: HashMap<Key, usize>,
    free_indexes: Vec<usize>,
    queues: Vec<Option<Arc<dyn FinalizerQueue>>>,
    notifiers: Vec<Arc<dyn FinalizationNotify>>,
}

impl State {
    fn new() -> Self {
        Self {
            candidates: VecDeque::new(),
            finalizers_queue_index: HashMap::new(),
            free_indexes: Vec::new(),
            queues: Vec::new(),
            notifiers: Vec::new(),
        }
    }

    fn get_finalizer_queue_index(&mut self, queue: Arc<dyn FinalizerQueue>) -> usize {
        match self.finalizers_queue_index.entry(Key(queue.clone())) {
            Entry::Vacant(entry) => {
                let index = self.free_indexes.pop().unwrap_or_else(|| {
                    self.queues.push(None);
                    self.queues.len() - 1
                });

                self.queues[index] = Some(queue);
                entry.insert(index);
                index
            }

            Entry::Occupied(entry) => *entry.get(),
        }
    }
}

pub struct Finalizers(Monitor<State>);

impl Finalizers {
    pub(crate) fn new() -> Self {
        Self(Monitor::new(State::new()))
    }

    pub fn add_notifier(&self, notifier: Arc<dyn FinalizationNotify>) {
        let mut state = self.0.lock();
        state.notifiers.push(notifier);
    }

    pub fn register_queue<FQ>(&self, queue: Arc<FQ>) -> usize
    where
        FQ: FinalizerQueue,
    {
        self.0.lock().get_finalizer_queue_index(queue)
    }

    /// Register object as a finalization candidate. Object will be associated
    /// with the given finalizer queue. Queue is triggered once GC is completed
    /// and there are objects to finalize.
    ///
    /// Returns the index of the finalizer queue.
    ///
    /// Note that `object` might be registered multiple times, FinalizerQueue implementation
    /// is responsible for handling this.

    pub fn register_finalizer<'gc, T, FQ>(&self, queue: &Arc<FQ>, object: Gc<'gc, T>) -> usize
    where
        T: Trace + Send,
        FQ: FinalizerQueue,
    {
        let mut state = self.0.lock();
        let index = state.get_finalizer_queue_index(queue.clone());
        state
            .candidates
            .push_back((object.to_object_reference(), index));
        index
    }
    /// Get finalizer queue at a specified index.
    ///
    /// # Panics
    ///
    /// Panics if queue at `index` does not exist.

    pub fn finalizer_queue(&self, index: usize) -> Arc<dyn FinalizerQueue> {
        self.0
            .lock()
            .queues
            .get(index)
            .and_then(|q| q.as_ref())
            .unwrap()
            .clone()
    }

    pub(crate) fn process(
        &self,
        worker: &mut GCWorker<MemoryManager>,
        tracer_context: &mut impl ObjectTracerContext<MemoryManager>,
    ) -> bool {
        let mut state = self.0.lock();
        let mut had_some = false;

        for notifier in state.notifiers.iter() {
            notifier.notify_in_processing();
        }

        if !state.candidates.is_empty() {
            tracer_context.with_tracer(worker, |tracer| {
                let mut new_candidates = VecDeque::new();

                while let Some((object, queue_index)) = state.candidates.pop_front() {
                    // object is reachable? Add it to new cadidates.
                    if object.is_reachable() {
                        new_candidates.push_back((
                            object.get_forwarded_object().unwrap_or(object),
                            queue_index,
                        ));
                        continue;
                    }
                    had_some = true;
                    // Object is not reachable, resurrect it and mark as ready to run.
                    let newobject = tracer.trace_object(object);
                    state.queues[queue_index]
                        .as_mut()
                        .unwrap()
                        .mark_ready_to_run(newobject);
                }

                state.candidates = new_candidates;
            });
        }

        had_some
    }

    pub(crate) fn schedule(&self) {
        let state = self.0.lock();

        for queue in state.queues.iter().flatten() {
            queue.schedule();
        }

        for notifier in state.notifiers.iter() {
            notifier.schedule();
        }
    }
}

/// Define a finalizer queue type for set of objects with the same type.
#[macro_export]
macro_rules! make_finalizer_queue {
    ($gc: lifetime => $name: ident : $t: ty {
        fn schedule(&self)
            $b: block

    }) => {
        pub struct $name {
            finalizers: ::std::sync::Mutex<::std::collections::VecDeque<$crate::object::GCObject>>,
        }

        impl $name {
            pub fn new() -> ::std::sync::Arc<Self> {
                ::std::sync::Arc::new(Self {
                    finalizers: ::std::sync::Mutex::new(::std::collections::VecDeque::new()),
                })
            }

            pub fn pop_front<$gc>(
                &self,
                mc: &$crate::mutator::Mutation<$gc>,
            ) -> Option<$crate::ptr::Gc<$gc, $t>> {
                let mut finalizers = self.finalizers.lock().unwrap();
                if let Some(object) = finalizers.pop_front() {
                    Some(unsafe { $crate::ptr::Gc::from_gcobj(object) })
                } else {
                    None
                }
            }

            pub fn pop_back<$gc>(
                &self,
                mc: &$crate::mutator::Mutation<$gc>,
            ) -> Option<$crate::ptr::Gc<$gc, $t>> {
                let mut finalizers = self.finalizers.lock().unwrap();
                if let Some(object) = finalizers.pop_back() {
                    Some(unsafe { $crate::ptr::Gc::from_gcobj(object) })
                } else {
                    None
                }
            }

            pub fn len(&self) -> usize {
                self.finalizers.lock().unwrap().len()
            }

            pub fn is_empty(&self) -> bool {
                self.finalizers.lock().unwrap().is_empty()
            }
        }

        unsafe impl $crate::finalizer::FinalizerQueue for $name {
            fn mark_ready_to_run(&self, object: $crate::mmtk::util::ObjectReference) {
                let mut finalizers = self.finalizers.lock().unwrap();
                finalizers.push_back($crate::object::GCObject::from(object));
            }

            fn schedule(&self) {
                $b
            }
        }
    };

    ($gc: lifetime => $name: ident : $t: ty) => {
        make_finalizer_queue!($gc => $name : $t {
            fn schedule(&self) {}
        });
    };

    ($name: ident : $t: ty) => {
        make_finalizer_queue!($name: $t {
            fn schedule(&self) {}
        });
    };


}
