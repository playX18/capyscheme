use crate::prelude::*;
use crate::runtime::objectmodel::ephemeron::{Ephemeron, ephemeron_header_word};
use crate::runtime::stats::{
    CompilationBreakdownPhase, begin_compilation_breakdown, end_compilation_breakdown,
    runtime_stats_enabled as stats_enabled, runtime_stats_snapshot, set_runtime_stats_enabled,
};

fn runtime_stats_breakdown_token<'gc>(ctx: MutationContext<'gc>, token: u64) -> Value<'gc> {
    if token == 0 {
        Value::new(false)
    } else {
        Number::from_u64(ctx, token).into_value(ctx)
    }
}

fn parse_runtime_stats_breakdown_token<'gc>(token: Value<'gc>) -> u64 {
    token
        .number()
        .and_then(|n| n.exact_integer_to_usize())
        .map(|n| n as u64)
        .unwrap_or(0)
}

#[scheme(path=capy)]
pub mod gc {
    #[scheme(name = "ephemeron?")]
    pub fn is_ephemeron(value: Value<'gc>) -> bool {
        value.is::<Ephemeron>()
    }

    /// Creates a new SRFI-124 Ephemeron pair. If `key` is not a heap-allocated object,
    /// error is raised. If `(eq? key value)` is #t then the ephemeron pair is effectively
    /// a weak reference.
    ///
    /// On GC cycle, if `key` is not reachable from root set, both `key` and `value`
    /// are set to `#f`. Otherwise `value` is traced normally.
    ///
    /// NOTE: value does not keep key alive if it has references to it.
    #[scheme(name = "make-ephemeron")]
    pub fn make_ephemeron(key: Value<'gc>, value: Value<'gc>) -> Value<'gc> {
        if !key.is_cell() {
            return call.raise_error(
                "make-ephemeron",
                "ephemeron key must be a heap-allocated object",
                &[key],
            );
        }

        let ephemeron = Ephemeron { key, value };

        let cell = Value::from(ctx.allocate_with(ephemeron, ephemeron_header_word()));

        cell
    }

    /// Returns key of the ephemeron. If the key is not reachable, returns #f.
    #[scheme(name = "ephemeron-key")]
    pub fn ephemeron_key(ephemeron: Gc<'gc, Ephemeron<'gc>>) -> Value<'gc> {
        if !ephemeron.key.is_cell() {
            return Value::new(false);
        }
        // SAFETY: The value has been type-checked, `as_cell_raw` returns a valid GC object
        unsafe {
            ctx.raw_weak_reference_load(ephemeron.key.as_cell_raw());
        }
        ephemeron.key
    }

    /// Returns value of the ephemeron. If the key is not reachable, returns #f.
    #[scheme(name = "ephemeron-datum")]
    pub fn ephemeron_datum(ephemeron: Gc<'gc, Ephemeron<'gc>>) -> Value<'gc> {
        if !ephemeron.value.is_cell() {
            // return value, not #f because value can be anything
            return ephemeron.value;
        }
        // SAFETY: `value` is a cell and we need to perform weak reference load.
        unsafe {
            ctx.raw_weak_reference_load(ephemeron.value.as_cell_raw());
        }

        ephemeron.value
    }

    /// A no-op function that is required by SRFI-124. We can't reliably
    /// provide reference barrier as a native procedure. GC can only happen in
    /// between function calls or continuation jumps, so this function does nothing.
    #[scheme(name = "reference-barrier")]
    pub fn reference_barrier(_x: Value<'gc>) -> Value<'gc> {
        Value::undefined()
    }

    #[scheme(name = "gc-logging-enable!")]
    pub fn gc_logging_enable(enabled: bool) -> Value<'gc> {
        crate::heap::logging::set_gc_logging_enabled(enabled);
        Value::undefined()
    }

    #[scheme(name = "runtime-stats-enable!")]
    pub fn runtime_stats_enable(enabled: bool) -> Value<'gc> {
        set_runtime_stats_enabled(&ctx.state().stats, enabled);
        Value::undefined()
    }

    #[scheme(name = "runtime-stats-enabled?")]
    pub fn runtime_stats_enabled() -> bool {
        stats_enabled()
    }

    #[scheme(name = "runtime-stats-report")]
    pub fn runtime_stats_report() -> Value<'gc> {
        let Some(snapshot) = runtime_stats_snapshot(Some(&ctx.state().stats)) else {
            return Value::new(false);
        };
        let report = Value::from(Str::new(ctx, snapshot.to_string(), true));
        report
    }

    #[scheme(name = "gc-statistics")]
    pub fn gc_statistics() -> Value<'gc> {
        let used = crate::heap::GC
            .get()
            .map(|gc| crate::heap::mmtk::memory_manager::used_bytes(&gc.mmtk))
            .unwrap_or(0);
        let (collections, bytes, reclaimed, gc_cpu_ms, gc_real_ms) =
            crate::runtime::gc_stats::snapshot(used);
        let vector = crate::vector!(
            ctx,
            collections as u64,
            bytes as u64,
            reclaimed as u64,
            gc_cpu_ms,
            gc_real_ms
        );
        Value::from(vector)
    }

    #[scheme(name = "%runtime-stats-begin-compilation")]
    pub fn runtime_stats_begin_compilation() -> Value<'gc> {
        ctx.stats.start_compilation();
        Value::undefined()
    }

    #[scheme(name = "%runtime-stats-end-compilation")]
    pub fn runtime_stats_end_compilation() -> Value<'gc> {
        ctx.stats.end_compilation();
        Value::undefined()
    }

    #[scheme(name = "%runtime-stats-begin-reader")]
    pub fn runtime_stats_begin_reader() -> Value<'gc> {
        let token = begin_compilation_breakdown(CompilationBreakdownPhase::Reader);
        runtime_stats_breakdown_token(ctx, token)
    }

    #[scheme(name = "%runtime-stats-end-reader")]
    pub fn runtime_stats_end_reader(token: Value<'gc>) -> Value<'gc> {
        end_compilation_breakdown(parse_runtime_stats_breakdown_token(token));
        Value::undefined()
    }

    #[scheme(name = "%runtime-stats-begin-psyntax")]
    pub fn runtime_stats_begin_psyntax() -> Value<'gc> {
        let token = begin_compilation_breakdown(CompilationBreakdownPhase::Psyntax);
        runtime_stats_breakdown_token(ctx, token)
    }

    #[scheme(name = "%runtime-stats-end-psyntax")]
    pub fn runtime_stats_end_psyntax(token: Value<'gc>) -> Value<'gc> {
        end_compilation_breakdown(parse_runtime_stats_breakdown_token(token));
        Value::undefined()
    }
}
