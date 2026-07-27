//! Parallel / concurrent iteration state (HotSpot `OopStorage::ParState`).

use std::sync::atomic::{AtomicUsize, Ordering};

use crate::runtime::value::Value;

use super::block::ActiveArray;
use super::storage::OopStorage;

struct IterationData {
    segment_start: usize,
    segment_end: usize,
    processed: usize,
}

/// Core parallel iteration state shared by [`ParState`].
pub struct BasicParState<'a> {
    storage: &'a OopStorage,
    active_array: *mut ActiveArray,
    block_count: usize,
    next_block: AtomicUsize,
    estimated_thread_count: u32,
    concurrent: bool,
    num_dead: AtomicUsize,
}

// SAFETY: HotSpot concurrent iteration protocol.
unsafe impl Send for BasicParState<'_> {}
unsafe impl Sync for BasicParState<'_> {}

impl<'a> BasicParState<'a> {
    pub fn new(storage: &'a OopStorage, estimated_thread_count: u32, concurrent: bool) -> Self {
        assert!(estimated_thread_count > 0);
        let active_array = storage.obtain_active_array();
        let mut state = Self {
            storage,
            active_array,
            block_count: 0,
            next_block: AtomicUsize::new(0),
            estimated_thread_count,
            concurrent,
            num_dead: AtomicUsize::new(0),
        };
        state.update_concurrent_iteration_count(1);
        state.block_count = unsafe { (*active_array).block_count_acquire() };
        state
    }

    pub fn storage(&self) -> &'a OopStorage {
        self.storage
    }

    pub fn default_estimated_thread_count(concurrent: bool) -> u32 {
        // HotSpot uses ConcGCThreads / ParallelGCThreads; default to 1.
        let _ = concurrent;
        1
    }

    fn update_concurrent_iteration_count(&self, value: i32) {
        if self.concurrent {
            self.storage.update_concurrent_iteration_count(value);
        }
    }

    fn claim_next_segment(&self, data: &mut IterationData) -> bool {
        data.processed += data.segment_end - data.segment_start;
        let start = self.next_block.load(Ordering::Acquire);
        if start >= self.block_count {
            return self.finish_iteration(data);
        }
        let max_step = 10usize;
        let remaining = self.block_count - start;
        let step = max_step.min(1 + (remaining / self.estimated_thread_count as usize));
        let end = self.next_block.fetch_add(step, Ordering::Relaxed) + step;
        let start = end - step;
        let end = end.min(self.block_count);
        if start < self.block_count {
            data.segment_start = start;
            data.segment_end = end;
            true
        } else {
            self.finish_iteration(data)
        }
    }

    fn finish_iteration(&self, data: &IterationData) -> bool {
        let pct = if self.block_count == 0 {
            0.0
        } else {
            100.0 * (data.processed as f64) / (self.block_count as f64)
        };
        log::info!(
            target: "oopstorage",
            "Parallel iteration on {}: blocks = {}, processed = {} ({:.0}%)",
            self.storage.name(),
            self.block_count,
            data.processed,
            pct
        );
        false
    }

    pub fn iterate<F>(&self, mut f: F)
    where
        F: FnMut(*mut Value<'static>),
    {
        let mut data = IterationData {
            segment_start: 0,
            segment_end: 0,
            processed: 0,
        };
        while self.claim_next_segment(&mut data) {
            debug_assert!(data.segment_start < data.segment_end);
            debug_assert!(data.segment_end <= self.block_count);
            let mut i = data.segment_start;
            loop {
                let block = unsafe { (*self.active_array).at(i) };
                unsafe {
                    (*block).iterate(|p| {
                        f(p);
                        true
                    });
                }
                i += 1;
                if i >= data.segment_end {
                    break;
                }
            }
        }
    }

    pub fn num_dead(&self) -> usize {
        self.num_dead.load(Ordering::Relaxed)
    }

    pub fn increment_num_dead(&self, num_dead: usize) {
        self.num_dead.fetch_add(num_dead, Ordering::Relaxed);
    }

    pub fn report_num_dead(&self) {
        self.storage.report_num_dead(self.num_dead());
    }
}

impl Drop for BasicParState<'_> {
    fn drop(&mut self) {
        self.storage.relinquish_block_array(self.active_array);
        self.update_concurrent_iteration_count(-1);
        if self.concurrent {
            self.storage.record_needs_cleanup();
        }
    }
}

/// Parallel iteration over an [`OopStorage`].
///
/// - `CONCURRENT`: suppress empty-block deletion for the state's lifetime
/// - `CONST_ITER`: reserved for API parity (Rust always uses `*mut Value`)
pub struct ParState<'a, const CONCURRENT: bool, const CONST_ITER: bool> {
    basic: BasicParState<'a>,
}

impl<'a, const CONCURRENT: bool, const CONST_ITER: bool> ParState<'a, CONCURRENT, CONST_ITER> {
    pub fn new(storage: &'a OopStorage) -> Self {
        let estimated = BasicParState::default_estimated_thread_count(CONCURRENT);
        Self::with_estimated_threads(storage, estimated)
    }

    pub fn with_estimated_threads(storage: &'a OopStorage, estimated_thread_count: u32) -> Self {
        Self {
            basic: BasicParState::new(storage, estimated_thread_count, CONCURRENT),
        }
    }

    pub fn storage(&self) -> &'a OopStorage {
        self.basic.storage()
    }

    pub fn iterate<F>(&self, f: F)
    where
        F: FnMut(*mut Value<'static>),
    {
        self.basic.iterate(f);
    }

    pub fn oops_do<F>(&self, mut f: F)
    where
        F: FnMut(*mut Value<'static>),
    {
        self.iterate(|p| f(p));
    }

    pub fn num_dead(&self) -> usize {
        self.basic.num_dead()
    }

    pub fn increment_num_dead(&self, n: usize) {
        self.basic.increment_num_dead(n);
    }

    pub fn report_num_dead(&self) {
        self.basic.report_num_dead();
    }
}

impl<'a> ParState<'a, false, false> {
    /// Weak iteration skipping empty entries.
    pub fn weak_oops_do<F>(&self, mut f: F)
    where
        F: FnMut(*mut Value<'static>),
    {
        self.iterate(|p| {
            if !unsafe { (*p).is_empty() } {
                f(p);
            }
        });
    }

    /// Weak iteration with is-alive filter; clears dead entries.
    pub fn weak_oops_do_if_alive<A, F>(&self, mut is_alive: A, mut f: F)
    where
        A: FnMut(Value<'static>) -> bool,
        F: FnMut(*mut Value<'static>),
    {
        self.iterate(|p| {
            let v = unsafe { *p };
            if !v.is_empty() {
                if is_alive(v) {
                    f(p);
                } else {
                    unsafe { *p = Value::empty() };
                }
            }
        });
    }
}
