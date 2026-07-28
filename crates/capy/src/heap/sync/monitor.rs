use parking_lot::{Condvar, Mutex, WaitTimeoutResult};

pub struct Monitor<T = ()> {
    mutex: Mutex<T>,
    condvar: Condvar,
}

impl Default for Monitor {
    fn default() -> Self {
        Monitor {
            mutex: Mutex::new(()),
            condvar: Condvar::new(),
        }
    }
}

impl<T> Monitor<T> {
    pub const fn new(value: T) -> Self {
        Monitor {
            mutex: Mutex::new(value),
            condvar: Condvar::new(),
        }
    }

    pub fn notify_one(&self) {
        self.condvar.notify_one();
    }

    pub fn notify_all(&self) {
        self.condvar.notify_all();
    }

    pub fn lock(&self) -> MonitorGuard<'_, T> {
        MonitorGuard {
            guard: self.mutex.lock(),
            monitor: self,
        }
    }

    pub fn get_mut(&mut self) -> &mut T {
        self.mutex.get_mut()
    }

    /// Make monitor guard without locking the mutext.
    ///
    /// # Safety
    ///
    /// Read [`Mutex::make_guard_unchecked`](parking_lot::Mutex::make_guard_unchecked) documentation.
    pub unsafe fn make_guard_unchecked(&self) -> MonitorGuard<'_, T> {
        MonitorGuard {
            guard: unsafe { self.mutex.make_guard_unchecked() },
            monitor: self,
        }
    }
}

pub struct MonitorGuard<'a, T> {
    guard: parking_lot::MutexGuard<'a, T>,
    monitor: &'a Monitor<T>,
}

impl<'a, T> MonitorGuard<'a, T> {
    pub fn wait(&mut self) {
        self.monitor.condvar.wait(&mut self.guard);
    }

    pub fn wait_while<F>(&mut self, predicate: F)
    where
        F: FnMut(&mut T) -> bool,
    {
        self.monitor.condvar.wait_while(&mut self.guard, predicate);
    }

    pub fn wait_for(&mut self, timeout: std::time::Duration) -> WaitTimeoutResult {
        self.monitor.condvar.wait_for(&mut self.guard, timeout)
    }

    pub fn notify_one(&self) {
        self.monitor.notify_one();
    }

    pub fn notify_all(&self) {
        self.monitor.notify_all();
    }

    pub fn unlocked<F, R>(&mut self, f: F) -> R
    where
        F: FnOnce() -> R,
    {
        parking_lot::MutexGuard::unlocked(&mut self.guard, f)
    }

    pub fn unlock_fair(s: Self) {
        parking_lot::MutexGuard::unlock_fair(s.guard);
    }
}

pub struct MappedMonitorGuard<'a, T> {
    guard: parking_lot::MappedMutexGuard<'a, T>,
    #[allow(dead_code)]
    monitor: &'a Monitor<T>,
}

impl<'a, T> std::ops::Deref for MappedMonitorGuard<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.guard
    }
}

impl<'a, T> std::ops::DerefMut for MappedMonitorGuard<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.guard
    }
}

impl<'a, T> std::ops::Deref for MonitorGuard<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.guard
    }
}

impl<'a, T> std::ops::DerefMut for MonitorGuard<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.guard
    }
}
