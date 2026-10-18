//! GC sequences for generated code

pub mod alloc;
pub mod barrier;

use std::sync::OnceLock;
use std::sync::atomic::{AtomicU64, Ordering::Relaxed};

use mmtk::util::alloc::AllocatorSelector;
use mmtk::util::metadata::side_metadata::global_side_metadata_vm_base_address;
use mmtk::{AllocationSemantics, BarrierSelector};

use crate::heap::GarbageCollector;
use crate::heap::object::{ClassId, class_header_word};

/// What the active MMTk plan needs.
#[derive(Debug)]
pub struct GcPlanInfo {
    pub barrier: BarrierSelector,
    /// Base of the global side metadata, where the unlog bits are: one bit
    /// per 8 bytes, so an object's byte is `base + (obj >> 6)` and its bit
    /// `(obj >> 3) & 7`.
    pub unlog_side_metadata_base: usize,
    /// Generated code bumps the LAB itself
    pub inline_alloc: bool,
    /// Objects of this many bytes or more, header included, go to the large
    /// object space (`raw_allocate_with_header_word`).
    pub los_threshold: usize,
}

impl GcPlanInfo {
    pub fn get() -> &'static Self {
        static INFO: OnceLock<GcPlanInfo> = OnceLock::new();
        INFO.get_or_init(Self::read)
    }

    fn read() -> Self {
        let gc = GarbageCollector::get();
        let constraints = gc.mmtk.get_plan().constraints();
        let selector =
            mmtk::memory_manager::get_allocator_mapping(&gc.mmtk, AllocationSemantics::Default);
        Self {
            barrier: constraints.barrier,
            unlog_side_metadata_base: global_side_metadata_vm_base_address().as_usize(),
            inline_alloc: matches!(
                selector,
                AllocatorSelector::BumpPointer(_) | AllocatorSelector::Immix(_)
            ),
            los_threshold: crate::heap::mutator::LOS_THRESHOLD_BYTES
                .min(constraints.max_non_los_default_alloc_bytes),
        }
    }
}

/// Allocations by generated code that took the fast path, and those that
/// called `capy_op_alloc`, counted with `CAPY_TEMPLATE_ALLOC_STATS=1`.
pub static FAST_ALLOCS: AtomicU64 = AtomicU64::new(0);
pub static SLOW_ALLOCS: AtomicU64 = AtomicU64::new(0);

pub fn alloc_stats() -> bool {
    crate::utils::flags::template_alloc_stats()
}

extern "C" fn print_alloc_stats() {
    let fast = FAST_ALLOCS.load(Relaxed);
    let slow = SLOW_ALLOCS.load(Relaxed);
    log::info!(
        target: crate::heap::logging::STATS_LOG_TARGET,
        "template alloc stats: {fast} fast, {slow} slow"
    );
}

unsafe extern "C" {
    fn atexit(callback: extern "C" fn());
}

/// Install the `CAPY_TEMPLATE_ALLOC_STATS=1` exit report, once per process.
pub fn maybe_report_alloc_stats() {
    static ONCE: std::sync::Once = std::sync::Once::new();
    if alloc_stats() {
        // SAFETY: the callback only reads atomics and logs.
        ONCE.call_once(|| unsafe { atexit(print_alloc_stats) });
    }
}

#[derive(Clone, Copy)]
pub struct GcCodegen {
    info: &'static GcPlanInfo,
}

impl GcCodegen {
    pub fn new() -> Self {
        Self {
            info: GcPlanInfo::get(),
        }
    }

    /// A `GcCodegen` for a plan described by hand (snippet tests).
    #[cfg(test)]
    pub fn with_info(info: &'static GcPlanInfo) -> Self {
        Self { info }
    }

    pub fn info(&self) -> &'static GcPlanInfo {
        self.info
    }
}

impl Default for GcCodegen {
    fn default() -> Self {
        Self::new()
    }
}

/// The header word of builtin class `class_id`.
pub fn header_word(class_id: u32) -> u64 {
    class_header_word(ClassId::new(class_id).expect("builtin class id is nonzero"))
}
