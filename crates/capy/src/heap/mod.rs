//! Heap, precise garbage collection for Capy.
//!
//! Safe mutator-facing APIs sit on top of MMTk. Prefer [`crate::runtime::MutationContext`] for
//! allocation and rooting, this module owns the GC engine, tracing, and storage.

use constraints::add_core_constraints;
use finalizer::Finalizers;
use mm::MemoryManager;
pub(crate) use mmtk;
use mmtk::util::Address;
use mmtk::util::options::PlanSelector;
use mmtk::{MMTK, MMTKBuilder};
use parking_lot::RwLock;
use std::sync::OnceLock;
use std::sync::atomic::AtomicU32;
use std::sync::atomic::AtomicUsize;
use sync::thread::ThreadManager;
use weak::WeakProcessingState;

pub(crate) mod active_plan;
pub mod alloc;
pub mod barrier;
pub mod cell;
pub(crate) mod collection;
pub mod constraints;
pub(crate) mod finalizer;
pub mod global;
pub(crate) mod heuristics;
pub(crate) mod lab;
pub(crate) mod logging;
pub(crate) mod mm;
pub(crate) mod mutator;
pub(crate) mod object;
pub mod oop_storage;
pub mod pin;
pub mod plans;
pub mod ptr;
pub(crate) mod scanning;
pub mod space;
pub(crate) mod sync;
pub mod traits;
pub mod weak;

pub struct GarbageCollector {
    pub(crate) threads: ThreadManager,
    pub(crate) weak: WeakProcessingState,
    pub(crate) finalizers: Finalizers,
    pub(crate) global_registry: global::GlobalRegistry,
    pub(crate) constraints: RwLock<constraints::MarkingConstraintSet>,
    pub(crate) mmtk: MMTK<MemoryManager>,
}
static BASE: AtomicUsize = AtomicUsize::new(0);
static SHIFT: AtomicU32 = AtomicU32::new(0);

impl GarbageCollector {
    fn new(mut mmtk: MMTKBuilder) -> Self {
        let mut constraint_set = constraints::MarkingConstraintSet::new();
        add_core_constraints(&mut constraint_set);
        let this = Self {
            finalizers: Finalizers::new(),
            weak: WeakProcessingState::new(),
            threads: ThreadManager::new(),
            mmtk: mmtk.build(),
            global_registry: global::GlobalRegistry::new(),
            constraints: RwLock::new(constraint_set),
        };
        let heap_end = mmtk::memory_manager::last_heap_address().as_usize();
        let (heap_base, heap_shift) = if heap_end <= (4usize << 30) {
            (Address::ZERO, 0)
        } else if heap_end <= (32usize << 30) {
            (Address::ZERO, 3)
        } else {
            (mmtk::memory_manager::starting_heap_address() - 4096, 3)
        };
        if !plans::is_allowed_plan(*mmtk.options.plan) {
            mmtk.options.plan.set(PlanSelector::StickyImmix);
        }

        BASE.store(heap_base.as_usize(), std::sync::atomic::Ordering::Relaxed);
        SHIFT.store(heap_shift, std::sync::atomic::Ordering::Relaxed);
        this
    }

    /// Initialize GC with provided MMTKBuilder.
    pub fn init(mmtk_builder: MMTKBuilder) -> bool {
        if GC.get().is_some() {
            return false;
        }
        GC.get_or_init(|| Self::new(mmtk_builder));
        true
    }

    pub fn get() -> &'static Self {
        GC.get_or_init(|| {
            logging::init_rust_logger();
            let mut builder = logging::mmtk_builder();
            mm::vm_layout(&mut builder);
            Self::new(builder)
        })
    }

    pub fn add_weak_callback<F>(&self, callback: F) -> WeakCallbackToken
    where
        F: Fn(&mut WeakProcessor) + Send + Sync + 'static,
    {
        self.weak.add_weak_callback(callback)
    }

    pub fn remove_weak_callback(&self, token: WeakCallbackToken) {
        self.weak.remove_weak_callback(token);
    }

    /// Register an additional marking constraint. Panics if a collection is active.
    pub fn add_marking_constraint(&self, constraint: Box<dyn constraints::MarkingConstraint>) {
        self.constraints.write().add(constraint);
    }
}

unsafe impl Send for GarbageCollector {}
unsafe impl Sync for GarbageCollector {}

pub(crate) static GC: OnceLock<GarbageCollector> = OnceLock::new();

pub use barrier::{AsRefWrite, IndexWrite, Write};
pub use capy_derive::Trace;
pub use constraints::{
    ConstraintConcurrency, ConstraintParallelism, ConstraintVisitor, ConstraintVolatility,
    MarkingConstraint, MarkingConstraintSet, SimpleMarkingConstraint, add_marking_constraint,
    set_parallel_constraints,
};
pub use global::Global;
pub use pin::{Frame, Pin, Scope};
pub use ptr::{Gc, NarrowGc, ObjectSlot};
pub use space::Space;
pub use traits::Trace;
pub use weak::{Ephemeron, Weak, WeakCallback, WeakCallbackToken, WeakProcessor};

pub use plans::{ALLOWED_GC_PLAN_NAMES, is_allowed_plan, validate_plan};

// Hidden: required by derives, Rootable!, and `impl Into` GC helpers. Prefer MutationContext.
#[doc(hidden)]
pub use collection::Visitor;
pub(crate) use mutator::Mutator;
#[doc(hidden)]
pub use mutator::{__DynRootable, Mutation, Root, Rootable};

pub fn compressed_heap_base() -> Address {
    unsafe { Address::from_usize(BASE.load(std::sync::atomic::Ordering::Relaxed)) }
}

pub fn compressed_heap_shift() -> u32 {
    SHIFT.load(std::sync::atomic::Ordering::Relaxed)
}
