//! JSC-style marking constraints adapted to MMTk.
//!
//! Root-like (`GreyedByExecution`) work runs during Prepare / mutator root scan.
//! Outgrowths (`GreyedByMarking`) run in `VMRefClosure` via [`process_weak_refs`],
//! optionally sharded with [`mmtk::memory_manager::add_work_packets`].

mod builtin;
mod set;
mod solver;

pub use builtin::{add_core_constraints, add_marking_constraint};
pub use set::MarkingConstraintSet;
pub use solver::{ConstraintExecuteMode, PARALLEL_CONSTRAINTS, set_parallel_constraints};

use mmtk::util::ObjectReference;
use mmtk::vm::ObjectTracer;

/// When a constraint must be re-evaluated.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum ConstraintVolatility {
    /// Unlikely to produce; run late as a tie-breaker.
    SeldomGreyed = 0,
    /// Classic roots: GC start and after mutator resumes.
    GreyedByExecution = 1,
    /// Outgrowths: may grey more whenever objects are marked.
    GreyedByMarking = 2,
}

/// Whether the constraint may run alongside other marker work.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ConstraintConcurrency {
    Sequential,
    Concurrent,
}

/// Whether the constraint body may be sharded across GC workers.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ConstraintParallelism {
    Sequential,
    Parallel,
}

/// Visitor facade for constraint bodies (retains / greys objects).
pub struct ConstraintVisitor<'a> {
    tracer: Option<&'a mut dyn ObjectTracer>,
    visit_count: usize,
}

impl<'a> ConstraintVisitor<'a> {
    pub fn new_tracing(tracer: &'a mut dyn ObjectTracer) -> Self {
        Self {
            tracer: Some(tracer),
            visit_count: 0,
        }
    }

    pub fn new_counting() -> Self {
        Self {
            tracer: None,
            visit_count: 0,
        }
    }

    pub fn visit_count(&self) -> usize {
        self.visit_count
    }

    /// Retain / grey `object` if a tracer is present.
    pub fn trace_object(&mut self, object: ObjectReference) -> ObjectReference {
        self.visit_count += 1;
        if let Some(tracer) = self.tracer.as_mut() {
            tracer.trace_object(object)
        } else {
            object
        }
    }
}

/// A pluggable marking root or outgrowth.
pub trait MarkingConstraint: Send + Sync {
    fn abbreviated_name(&self) -> &str;
    fn name(&self) -> &str;
    fn volatility(&self) -> ConstraintVolatility;
    fn concurrency(&self) -> ConstraintConcurrency {
        ConstraintConcurrency::Concurrent
    }
    fn parallelism(&self) -> ConstraintParallelism {
        ConstraintParallelism::Sequential
    }

    /// Optional fast estimate used for wavefront ordering.
    fn quick_work_estimate(&self, _visitor: &ConstraintVisitor<'_>) -> f64 {
        0.0
    }

    fn prepare_to_execute(&self) {}

    /// Run the constraint. Visit count on `visitor` indicates retained/greyed work.
    fn execute(&self, visitor: &mut ConstraintVisitor<'_>);
}

/// Lambda-backed constraint (JSC `SimpleMarkingConstraint`).
pub struct SimpleMarkingConstraint<F>
where
    F: Fn(&mut ConstraintVisitor<'_>) + Send + Sync + 'static,
{
    abbreviated_name: &'static str,
    name: &'static str,
    volatility: ConstraintVolatility,
    concurrency: ConstraintConcurrency,
    parallelism: ConstraintParallelism,
    execute_fn: F,
    last_visit_count: std::sync::atomic::AtomicUsize,
}

impl<F> SimpleMarkingConstraint<F>
where
    F: Fn(&mut ConstraintVisitor<'_>) + Send + Sync + 'static,
{
    pub fn new(
        abbreviated_name: &'static str,
        name: &'static str,
        volatility: ConstraintVolatility,
        execute_fn: F,
    ) -> Self {
        Self {
            abbreviated_name,
            name,
            volatility,
            concurrency: ConstraintConcurrency::Concurrent,
            parallelism: ConstraintParallelism::Sequential,
            execute_fn,
            last_visit_count: std::sync::atomic::AtomicUsize::new(0),
        }
    }

    pub fn with_parallelism(mut self, parallelism: ConstraintParallelism) -> Self {
        self.parallelism = parallelism;
        self
    }

    pub fn with_concurrency(mut self, concurrency: ConstraintConcurrency) -> Self {
        self.concurrency = concurrency;
        self
    }

    pub fn last_visit_count(&self) -> usize {
        self.last_visit_count
            .load(std::sync::atomic::Ordering::Relaxed)
    }

    pub(crate) fn store_visit_count(&self, count: usize) {
        self.last_visit_count
            .store(count, std::sync::atomic::Ordering::Relaxed);
    }
}

impl<F> MarkingConstraint for SimpleMarkingConstraint<F>
where
    F: Fn(&mut ConstraintVisitor<'_>) + Send + Sync + 'static,
{
    fn abbreviated_name(&self) -> &str {
        self.abbreviated_name
    }

    fn name(&self) -> &str {
        self.name
    }

    fn volatility(&self) -> ConstraintVolatility {
        self.volatility
    }

    fn concurrency(&self) -> ConstraintConcurrency {
        self.concurrency
    }

    fn parallelism(&self) -> ConstraintParallelism {
        self.parallelism
    }

    fn quick_work_estimate(&self, _visitor: &ConstraintVisitor<'_>) -> f64 {
        self.last_visit_count() as f64
    }

    fn execute(&self, visitor: &mut ConstraintVisitor<'_>) {
        let before = visitor.visit_count();
        (self.execute_fn)(visitor);
        let delta = visitor.visit_count().saturating_sub(before);
        self.store_visit_count(delta);
    }
}
