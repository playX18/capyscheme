use std::{
    mem::transmute,
    sync::{
        Arc, LazyLock,
        atomic::{AtomicBool, Ordering},
        mpsc::{self, RecvTimeoutError},
    },
    time::Duration,
};

use rsgc::{GarbageCollector, Mutation, Mutator, Rootable, mmtk, sync::monitor::Monitor};

pub enum VMThreadTask {
    /// Vacuum weak sets. This task will walk all live weak sets and remove
    /// any broken weak entries in them. This task is ran in mutator context
    /// thus it won't run until GC is complete.
    VacuumWeakSets,
    VacuumWeakTables,
    ClosePorts,
    MutatorTask(Box<dyn for<'gc> FnOnce(&'gc Mutation<'gc>) + Send>),
    Task(Box<dyn FnOnce() + Send>),

    /// Shutdown the VM thread. This task will cause the thread to exit.
    Shutdown,
}

pub static VM_THREAD: LazyLock<VmThread> = LazyLock::new(|| VmThread::new());

/// VM thread that handles execution of various tasks.
///
/// Mainly used for finalization tasks that are not time-critical and can be processed in the background.
pub struct VmThread {
    sender: mpsc::Sender<VMThreadTask>,
    thread_handle: Option<std::thread::JoinHandle<()>>,
    pair: Arc<Monitor<AtomicBool>>,
}

impl VmThread {
    pub fn new() -> Self {
        let (sender, receiver) = mpsc::channel::<VMThreadTask>();
        let pair = Arc::new(Monitor::new(AtomicBool::new(false)));
        let thread_pair = Arc::clone(&pair);

        let thread_handle = std::thread::spawn(move || {
            let mutator = Mutator::<Rootable!(())>::new(|_| ());
            loop {
                // Wait until there's work or a shutdown signal.
                let mut guard = thread_pair.lock();
                while !guard.load(Ordering::Relaxed) {
                    guard.wait_for(Duration::from_millis(100));
                    /*mutator.mutate(|mc, _| {
                        mmtk::memory_manager::gc_poll(&GarbageCollector::get().mmtk, unsafe {
                            transmute(mc.thread())
                        });
                    })*/
                }
                guard.store(false, Ordering::Relaxed); // Reset the flag
                drop(guard);

                // Process all available tasks
                let mut shutdown_requested = false;
                loop {
                    match receiver.recv_timeout(Duration::from_millis(100)) {
                        Ok(task) => match task {
                            VMThreadTask::ClosePorts => {
                                /*mutator.mutate(|mc, _| {
                                    super::value::port::close_ports(mc);
                                });*/
                            }
                            VMThreadTask::VacuumWeakSets => {
                                mutator.mutate(|mc, _| {
                                    super::value::weak_set::vacuum_weak_sets(mc);
                                });
                            }
                            VMThreadTask::VacuumWeakTables => {
                                mutator.mutate(|mc, _| {
                                    super::value::weak_table::vacuum_weak_tables(mc);
                                });
                            }

                            VMThreadTask::MutatorTask(task) => {
                                mutator.mutate(|mc, _| {
                                    task(mc);
                                });
                            }

                            VMThreadTask::Task(task) => {
                                task();
                            }

                            VMThreadTask::Shutdown => {
                                shutdown_requested = true;
                                break; // Exit task processing loop
                            }
                        },
                        Err(RecvTimeoutError::Timeout) => {
                            mutator.mutate(|mc, _| {
                                mmtk::memory_manager::gc_poll(
                                    &GarbageCollector::get().mmtk,
                                    unsafe {
                                        transmute::<_, rsgc::mmtk::util::VMMutatorThread>(
                                            mc.thread(),
                                        )
                                    },
                                );
                            });
                        }

                        Err(_) => break,
                    }
                }

                if shutdown_requested {
                    break; // Exit thread loop
                }
            }
        });

        VmThread {
            sender,
            thread_handle: Some(thread_handle),
            pair,
        }
    }

    /// Sends a task to the VM thread and notifies it.
    pub fn schedule_task(&self, task: VMThreadTask) {
        if self.sender.send(task).is_ok() {
            let guard = self.pair.lock();
            guard.store(true, Ordering::Relaxed);
            guard.notify_all();
        } else {
            // This might happen if the receiver (VM thread) has already panicked or exited.
            panic!("VMThread: Failed to send task. Thread might be down.");
        }
    }

    /// Signals the VM thread to shut down and waits for it to complete.
    pub fn shutdown(mut self) {
        if self.sender.send(VMThreadTask::Shutdown).is_ok() {
            let guard = self.pair.lock();
            guard.store(true, Ordering::Relaxed);
            guard.notify_all();
        }

        if let Some(handle) = self.thread_handle.take() {
            handle.join().expect("VM thread panicked");
        }
    }
}

impl Drop for VmThread {
    fn drop(&mut self) {
        if self.thread_handle.is_some() {
            if self.sender.send(VMThreadTask::Shutdown).is_ok() {
                let guard = self.pair.lock();
                guard.store(true, Ordering::Relaxed);
                guard.notify_all();
            }
        }
    }
}
