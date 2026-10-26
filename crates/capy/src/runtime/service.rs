//! VM-owned background threads.

use std::io;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::{Arc, Condvar, Mutex, Once};
use std::thread::{self, JoinHandle};
use std::time::Duration;

/// A background thread whose lifecycle is managed by [`ServiceHandle`].
pub trait ServiceThread: Send + Sync + 'static {
    /// The OS thread name.
    fn name(&self) -> String;

    /// The thread's stack size in bytes, or `None` for the platform default.
    fn stack_size(&self) -> Option<usize> {
        None
    }

    /// Runs on the new thread before [`run_service`](Self::run_service).
    fn pre_run(&self) {}

    /// The thread body. Must return soon after `ctl.should_terminate()`
    /// becomes true.
    fn run_service(&self, ctl: &ServiceCtl);

    /// Wake [`run_service`](Self::run_service) so that it observes the
    /// terminate flag. Called after the flag is set, from the stopping
    /// thread.
    fn stop_service(&self) {}

    /// Runs on the service thread after `run_service` returns, even when it
    /// unwinds.
    fn post_run(&self) {}
}

pub struct ServiceCtl {
    should_terminate: AtomicBool,
    /// `pthread_self` of the service thread.
    thread: AtomicUsize,
    has_terminated: Mutex<bool>,
    terminated: Condvar,
    sleeper: Mutex<()>,
    wake: Condvar,
}

impl ServiceCtl {
    fn new() -> Self {
        Self {
            should_terminate: AtomicBool::new(false),
            thread: AtomicUsize::new(0),
            has_terminated: Mutex::new(false),
            terminated: Condvar::new(),
            sleeper: Mutex::new(()),
            wake: Condvar::new(),
        }
    }

    pub fn should_terminate(&self) -> bool {
        self.should_terminate.load(Ordering::SeqCst)
    }

    /// Sleep for up to `timeout`, returning early if termination is
    /// requested.
    pub fn sleep(&self, timeout: Duration) {
        let guard = self.sleeper.lock().unwrap_or_else(|e| e.into_inner());
        if !self.should_terminate() {
            let _ = self.wake.wait_timeout(guard, timeout);
        }
    }

    fn request_terminate(&self) {
        self.should_terminate.store(true, Ordering::SeqCst);
        drop(self.sleeper.lock().unwrap_or_else(|e| e.into_inner()));
        self.wake.notify_all();
    }

    fn finish(&self) {
        *self
            .has_terminated
            .lock()
            .unwrap_or_else(|e| e.into_inner()) = true;
        self.terminated.notify_all();
    }

    fn is_terminated(&self) -> bool {
        *self
            .has_terminated
            .lock()
            .unwrap_or_else(|e| e.into_inner())
    }

    fn wait_terminated(&self) {
        let mut done = self
            .has_terminated
            .lock()
            .unwrap_or_else(|e| e.into_inner());
        while !*done {
            done = self
                .terminated
                .wait(done)
                .unwrap_or_else(|e| e.into_inner());
        }
    }
}

/// A running service.
pub struct ServiceHandle {
    ctl: Arc<ServiceCtl>,
    wake: Box<dyn Fn() + Send + Sync>,
    join: Mutex<Option<JoinHandle<()>>>,
}

impl ServiceHandle {
    /// Ask the service to terminate and wait until it has. Idempotent, and a
    /// no-op wait when called from the service thread itself.
    pub fn stop(&self) {
        self.ctl.request_terminate();
        (self.wake)();
        let handle = self.join.lock().unwrap_or_else(|e| e.into_inner()).take();
        // SAFETY: `pthread_self` has no preconditions.
        let on_service_thread =
            self.ctl.thread.load(Ordering::SeqCst) == unsafe { libc::pthread_self() } as usize;
        match handle {
            Some(_) if on_service_thread => {}
            Some(handle) => {
                let _ = handle.join();
            }
            None if on_service_thread => {}
            None => self.ctl.wait_terminated(),
        }
    }
}

struct Registry {
    shutting_down: AtomicBool,
    services: Mutex<Vec<Arc<ServiceHandle>>>,
}

static REGISTRY: Registry = Registry {
    shutting_down: AtomicBool::new(false),
    services: Mutex::new(Vec::new()),
};

/// Whether [`shutdown_all`] has begun. Services must not be started after.
pub fn is_shutting_down() -> bool {
    REGISTRY.shutting_down.load(Ordering::SeqCst)
}

/// Spawn `service` on its own thread and register it for exit-time shutdown.
pub fn start<S: ServiceThread>(service: Arc<S>) -> io::Result<Arc<ServiceHandle>> {
    static EXIT_HOOK: Once = Once::new();
    EXIT_HOOK.call_once(|| {
        // SAFETY: `shutdown_at_exit` is a plain `extern "C"` function.
        if unsafe { libc::atexit(shutdown_at_exit) } != 0 {
            log::warn!("failed to register service shutdown hook");
        }
    });

    let ctl = Arc::new(ServiceCtl::new());
    let mut builder = thread::Builder::new().name(service.name());
    if let Some(size) = service.stack_size() {
        builder = builder.stack_size(size);
    }
    let join = builder.spawn({
        let (service, ctl) = (service.clone(), ctl.clone());
        move || {
            struct Finish<'a, S: ServiceThread>(&'a S, &'a ServiceCtl);
            impl<S: ServiceThread> Drop for Finish<'_, S> {
                fn drop(&mut self) {
                    self.0.post_run();
                    self.1.finish();
                }
            }
            ctl.thread
                .store(unsafe { libc::pthread_self() } as usize, Ordering::SeqCst);
            let _finish = Finish(&*service, &ctl);
            service.pre_run();
            service.run_service(&ctl);
        }
    })?;
    let handle = Arc::new(ServiceHandle {
        ctl,
        wake: Box::new(move || service.stop_service()),
        join: Mutex::new(Some(join)),
    });
    let mut services = REGISTRY.services.lock().unwrap_or_else(|e| e.into_inner());
    // Forget services that already exited.
    services.retain(|h| !h.ctl.is_terminated());
    services.push(handle.clone());
    Ok(handle)
}

/// Stop every registered service, most recently started first.
pub fn shutdown_all() {
    REGISTRY.shutting_down.store(true, Ordering::SeqCst);
    let services =
        std::mem::take(&mut *REGISTRY.services.lock().unwrap_or_else(|e| e.into_inner()));
    for handle in services.iter().rev() {
        handle.stop();
    }
}

extern "C" fn shutdown_at_exit() {
    shutdown_all();
}
