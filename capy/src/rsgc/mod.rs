//! RSGC - Rust Garbage Collection library
//!
//! A GC library based on `gc-arena` API for safe and easy to use GC API, and using MMTk under the hood
//! to perform garbage collection.

pub use mmtk;

use finalizer::Finalizers;
use mm::MemoryManager;
use mmtk::util::Address;
use mmtk::util::options::PlanSelector;
pub use mmtk::{MMTK, MMTKBuilder};
use std::sync::OnceLock;
use std::sync::atomic::AtomicU32;
use std::sync::atomic::AtomicUsize;
use sync::thread::ThreadManager;
use weak::WeakProcessingState;
pub mod active_plan;
pub mod alloc;
pub mod barrier;
pub mod cell;
pub mod collection;
pub mod finalizer;
pub mod global;
pub(crate) mod heuristics;
pub mod lab;
pub(crate) mod logging;
pub mod mm;
pub mod mutator;
pub mod object;
pub mod ptr;
pub mod scanning;
pub mod sync;
pub mod traits;
pub mod util;
pub mod weak;

pub struct GarbageCollector {
    pub(crate) threads: ThreadManager,
    pub(crate) weak: WeakProcessingState,
    pub(crate) finalizers: Finalizers,
    pub(crate) global_registry: global::GlobalRegistry,
    pub mmtk: MMTK<MemoryManager>,
}
static BASE: AtomicUsize = AtomicUsize::new(0);
static SHIFT: AtomicU32 = AtomicU32::new(0);

impl GarbageCollector {
    fn new(mmtk: MMTKBuilder) -> Self {
        let this = Self {
            finalizers: Finalizers::new(),
            weak: WeakProcessingState::new(),
            threads: ThreadManager::new(),
            mmtk: mmtk.build(),
            global_registry: global::GlobalRegistry::new(),
        };
        let heap_end = mmtk::memory_manager::last_heap_address().as_usize();
        let (heap_base, heap_shift) = if heap_end <= (4usize << 30) {
            (Address::ZERO, 0)
        } else if heap_end <= (32usize << 30) {
            (Address::ZERO, 3)
        } else {
            (mmtk::memory_manager::starting_heap_address() - 4096, 3)
        };

        match *mmtk.options.plan {
            PlanSelector::Immix | PlanSelector::MarkSweep | PlanSelector::StickyImmix => {
                CAN_PIN_OBJECTS.store(true, std::sync::atomic::Ordering::Relaxed)
            }
            _ => (),
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
}

unsafe impl Send for GarbageCollector {}
unsafe impl Sync for GarbageCollector {}

pub(crate) static GC: OnceLock<GarbageCollector> = OnceLock::new();

pub use mutator::{__DynRootable, Mutation, Mutator, Root, Rootable};

pub use barrier::{AsRefWrite, IndexWrite, Write};
pub use capy_derive::__unelide_lifetimes;
pub use capy_derive::Trace;
pub use collection::Visitor;
pub use global::*;
pub use ptr::*;
pub use traits::Trace;
pub use weak::*;

use crate::CAN_PIN_OBJECTS;

pub fn compressed_heap_base() -> Address {
    unsafe { Address::from_usize(BASE.load(std::sync::atomic::Ordering::Relaxed)) }
}

pub fn compressed_heap_shift() -> u32 {
    SHIFT.load(std::sync::atomic::Ordering::Relaxed)
}

#[cfg(test)]
mod logging_tests {
    #[test]
    fn detects_log_trace_before_runtime_startup() {
        assert!(super::logging::args_request_gc_logging([
            "capy",
            "--log-trace",
            "-c",
            "42",
        ]));
        assert!(!super::logging::args_request_gc_logging([
            "capy",
            "--",
            "--log-trace",
        ]));
        assert!(!super::logging::args_request_gc_logging([
            "capy", "-c", "42"
        ]));
    }

    #[test]
    fn gc_logging_enable_flag_tracks_cli_state() {
        super::logging::set_gc_logging_enabled(false);
        assert!(!super::logging::gc_logging_enabled());

        super::logging::set_gc_logging_enabled(true);
        assert!(super::logging::gc_logging_enabled());

        super::logging::set_gc_logging_enabled(false);
        assert!(!super::logging::gc_logging_enabled());
    }

    #[test]
    fn gc_log_formatter_uses_scheme_comment_shape() {
        assert_eq!(
            super::logging::format_log_line(log::Level::Trace, "mmtk::scheduler", "starting GC"),
            ";; TRACE(mmtk::scheduler): starting GC"
        );
    }

    #[test]
    fn capy_gc_summary_logs_pause_and_heap_delta() {
        assert_eq!(
            super::logging::format_gc_summary_line(
                12,
                std::time::Duration::from_micros(3_421),
                18_874_368,
                11_796_480,
                4_194_304,
            ),
            ";; INFO(capy::gc): GC #12: pause=3.42ms, heap=18874368 bytes (18.00 MiB) -> 11796480 bytes (11.25 MiB), freed=7077888 bytes (6.75 MiB), free=4194304 bytes (4.00 MiB), total=15990784 bytes (15.25 MiB)"
        );
    }
}
