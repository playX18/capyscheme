use std::{
    collections::VecDeque,
    mem::transmute,
    sync::{Arc, Condvar, LazyLock, Mutex},
    time::Duration,
};

use super::service::{self, ServiceCtl, ServiceThread};
use crate::heap::{GarbageCollector, Mutation, Mutator, mmtk};

/// How long the thread waits for work before polling for a collection.
const WORKER_POLL_INTERVAL: Duration = Duration::from_millis(100);

pub enum Command {
    /// Vacuum weak sets. This task walks all live weak sets and removes broken
    /// entries. It runs in mutator context, so it waits until GC completes.
    VacuumWeakSets,
    VacuumWeakTables,
    ClosePorts,
    FinalizePointers,
    MutatorTask(Box<dyn for<'gc> FnOnce(&'gc Mutation<'gc>) + Send>),
    Task(Box<dyn FnOnce() + Send>),
}

pub static VM_THREAD: LazyLock<Arc<TaskThread>> = LazyLock::new(TaskThread::start);

/// Background thread for VM maintenance and finalization tasks.
pub struct TaskThread {
    queue: Mutex<VecDeque<Command>>,
    ready: Condvar,
}

impl TaskThread {
    fn start() -> Arc<Self> {
        let this = Arc::new(Self {
            queue: Mutex::new(VecDeque::new()),
            ready: Condvar::new(),
        });
        service::start(this.clone()).expect("failed to spawn VM task thread");
        this
    }

    /// Queue a task for the VM thread.
    pub fn schedule_task(&self, task: Command) {
        self.queue
            .lock()
            .unwrap_or_else(|e| e.into_inner())
            .push_back(task);
        self.ready.notify_one();
    }

    fn next_command(&self, ctl: &ServiceCtl) -> Option<Command> {
        let mut queue = self.queue.lock().unwrap_or_else(|e| e.into_inner());
        if queue.is_empty() && !ctl.should_terminate() {
            queue = self
                .ready
                .wait_timeout(queue, WORKER_POLL_INTERVAL)
                .unwrap_or_else(|e| e.into_inner())
                .0;
        }
        queue.pop_front()
    }
}

impl ServiceThread for TaskThread {
    fn name(&self) -> String {
        String::from("capy-vm")
    }

    fn run_service(&self, ctl: &ServiceCtl) {
        let mutator = Mutator::<crate::Rootable!(())>::new(|_| ());
        while !ctl.should_terminate() {
            match self.next_command(ctl) {
                Some(Command::FinalizePointers) => (),
                Some(Command::ClosePorts) => {
                    // TODO(Adel): implement port closing on thread shutdown
                }
                Some(Command::VacuumWeakSets) => {
                    mutator.mutate(|mc, _| super::value::weak_set::vacuum_weak_sets(mc));
                }
                Some(Command::VacuumWeakTables) => {
                    mutator.mutate(|mc, _| super::value::weak_table::vacuum_weak_tables(mc));
                }
                Some(Command::MutatorTask(task)) => {
                    mutator.mutate(|mc, _| task(&mc));
                }
                Some(Command::Task(task)) => task(),
                None => {
                    mutator.mutate(|mc, _| {
                        mmtk::memory_manager::gc_poll(
                            &GarbageCollector::get().mmtk,
                            // SAFETY: `mc.thread()` and `VMMutatorThread` are both
                            // thin wrappers around the same `*mut Thread` pointer.
                            // The RSGC crate guarantees layout compatibility.
                            unsafe {
                                transmute::<
                                    &crate::heap::sync::thread::Thread,
                                    crate::heap::mmtk::util::VMMutatorThread,
                                >(mc.thread())
                            },
                        );
                    });
                }
            }
        }
    }

    fn stop_service(&self) {
        // Order the terminate flag before a waiter that has not yet blocked.
        drop(self.queue.lock().unwrap_or_else(|e| e.into_inner()));
        self.ready.notify_all();
    }
}
