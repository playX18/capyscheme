//! HotSpot `SingleWriterSynchronizer` — RCU-style reader critical sections
//! with a single writer that waits for extant readers to finish.

use std::sync::atomic::{AtomicU32, Ordering};

use parking_lot::{Condvar, Mutex};

/// Counting semaphore used by [`SingleWriterSynchronizer::synchronize`].
struct Semaphore {
    count: Mutex<isize>,
    cv: Condvar,
}

impl Semaphore {
    fn new() -> Self {
        Self {
            count: Mutex::new(0),
            cv: Condvar::new(),
        }
    }

    fn signal(&self) {
        let mut count = self.count.lock();
        *count += 1;
        self.cv.notify_one();
    }

    fn wait(&self) {
        let mut count = self.count.lock();
        while *count <= 0 {
            self.cv.wait(&mut count);
        }
        *count -= 1;
    }

    fn try_wait(&self) -> bool {
        let mut count = self.count.lock();
        if *count > 0 {
            *count -= 1;
            true
        } else {
            false
        }
    }
}

/// Synchronization primitive inspired by RCU (HotSpot `SingleWriterSynchronizer`).
///
/// Any number of threads may enter critical sections. One writer at a time may
/// wait for all critical sections that were extant when the wait started.
pub struct SingleWriterSynchronizer {
    enter: AtomicU32,
    exit: [AtomicU32; 2],
    waiting_for: AtomicU32,
    wakeup: Semaphore,
    #[cfg(debug_assertions)]
    writers: AtomicU32,
}

impl SingleWriterSynchronizer {
    pub fn new() -> Self {
        Self {
            enter: AtomicU32::new(0),
            exit: [AtomicU32::new(0), AtomicU32::new(0)],
            // Initial value 1 puts waiting_for on the inactive track.
            waiting_for: AtomicU32::new(1),
            wakeup: Semaphore::new(),
            #[cfg(debug_assertions)]
            writers: AtomicU32::new(0),
        }
    }

    /// Enter a critical section. Never blocks.
    #[inline]
    pub fn enter(&self) -> u32 {
        self.enter.fetch_add(2, Ordering::Relaxed).wrapping_add(2)
    }

    /// Exit a critical section opened by [`Self::enter`].
    #[inline]
    pub fn exit(&self, enter_value: u32) {
        let exit_value = self.exit[(enter_value & 1) as usize]
            .fetch_add(2, Ordering::Release)
            .wrapping_add(2);
        if exit_value == self.waiting_for.load(Ordering::Relaxed) {
            self.wakeup.signal();
        }
    }

    /// Wait until all threads currently in a critical section have exited.
    ///
    /// # Precondition
    /// No other thread may be synchronizing on this object.
    pub fn synchronize(&self) {
        #[cfg(debug_assertions)]
        {
            assert_eq!(
                self.writers.fetch_add(1, Ordering::Relaxed),
                0,
                "multiple writers"
            );
        }

        std::sync::atomic::fence(Ordering::SeqCst);

        let mut value = self.enter.load(Ordering::Relaxed);
        let new_exit = &self.exit[((value.wrapping_add(1)) & 1) as usize];

        let old = loop {
            let old = value;
            value = value.wrapping_add(1);
            new_exit.store(value, Ordering::Relaxed);
            match self
                .enter
                .compare_exchange(old, value, Ordering::Relaxed, Ordering::Relaxed)
            {
                Ok(_) => break old,
                Err(current) => value = current,
            }
        };

        let old_exit = &self.exit[(old & 1) as usize];
        debug_assert!(!std::ptr::eq(new_exit, old_exit));

        self.waiting_for.store(old, Ordering::Relaxed);
        std::sync::atomic::fence(Ordering::SeqCst);

        while old != old_exit.load(Ordering::Acquire) {
            self.wakeup.wait();
        }
        while self.wakeup.try_wait() {}

        #[cfg(debug_assertions)]
        {
            assert_eq!(self.writers.fetch_sub(1, Ordering::Relaxed), 1);
        }
    }
}

impl Default for SingleWriterSynchronizer {
    fn default() -> Self {
        Self::new()
    }
}

/// RAII enter/exit for [`SingleWriterSynchronizer`].
pub struct CriticalSection<'a> {
    synchronizer: &'a SingleWriterSynchronizer,
    enter_value: u32,
}

impl<'a> CriticalSection<'a> {
    pub fn new(synchronizer: &'a SingleWriterSynchronizer) -> Self {
        let enter_value = synchronizer.enter();
        Self {
            synchronizer,
            enter_value,
        }
    }
}

impl Drop for CriticalSection<'_> {
    fn drop(&mut self) {
        self.synchronizer.exit(self.enter_value);
    }
}
