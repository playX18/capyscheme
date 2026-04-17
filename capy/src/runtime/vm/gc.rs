use crate::prelude::*;
use crate::rsgc::object::{HeapTypeInfo, VTableOf};
use crate::runtime::stats::{
    CompilationBreakdownPhase, begin_compilation_breakdown, end_compilation_breakdown,
    runtime_stats_enabled as stats_enabled, runtime_stats_snapshot, set_runtime_stats_enabled,
};

#[repr(C)]
pub struct Ephemeron<'gc> {
    key: Value<'gc>,
    value: Value<'gc>,
}

static EPHEMERON_INFO_VALUE: HeapTypeInfo = HeapTypeInfo::new(
    VTableOf::<'static, Ephemeron<'static>>::VT,
    TypeCode8::EPHEMERON.bits() as u16,
);
pub static EPHEMERON_INFO: &'static HeapTypeInfo = &EPHEMERON_INFO_VALUE;

unsafe impl<'gc> Trace for Ephemeron<'gc> {
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        visitor.register_for_weak_processing();
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut WeakProcessor) {
        unsafe {
            if self.key.is_cell() {
                let obj = self.key.as_cell_raw();
                let new_ptr = weak_processor.is_live_object(obj);

                // key is not alive anymore: break ephemeron pair
                if new_ptr.is_null() {
                    self.key = Value::new(false);
                    self.value = Value::new(false);
                    return;
                } else {
                    // key is alive: update key and trace the value
                    self.key = Value {
                        desc: EncodedValueDescriptor {
                            ptr: new_ptr.to_address().to_mut_ptr(),
                        },
                        pd: Default::default(),
                    };
                    weak_processor.visitor().trace(&mut self.value);
                }
            } else {
                // key is not a cell: impossible
                unreachable!("Ephemeron key is not a cell");
            }
        }
    }
}

unsafe impl<'gc> Tagged for Ephemeron<'gc> {
    const TC8: TypeCode8 = TypeCode8::EPHEMERON;
    const TYPE_NAME: &'static str = "#<ephemeron>";
}

fn runtime_stats_breakdown_token<'gc>(ctx: Context<'gc>, token: u64) -> Value<'gc> {
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
        nctx.return_(value.is::<Ephemeron>())
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
            return nctx.raise_error(
                "make-ephemeron",
                "ephemeron key must be a heap-allocated object",
                &[key],
            );
        }

        let ephemeron = Ephemeron { key, value };

        let cell = Gc::new_with_info(*ctx, ephemeron, EPHEMERON_INFO);

        nctx.return_(cell.into())
    }

    /// Returns key of the ephemeron. If the key is not reachable, returns #f.
    #[scheme(name = "ephemeron-key")]
    pub fn ephemeron_key(ephemeron: Gc<'gc, Ephemeron<'gc>>) -> Value<'gc> {
        if !ephemeron.key.is_cell() {
            return nctx.return_(Value::new(false));
        }
        unsafe {
            ctx.raw_weak_reference_load(ephemeron.key.as_cell_raw());
        }
        nctx.return_(ephemeron.key)
    }

    /// Returns value of the ephemeron. If the key is not reachable, returns #f.
    #[scheme(name = "ephemeron-datum")]
    pub fn ephemeron_datum(ephemeron: Gc<'gc, Ephemeron<'gc>>) -> Value<'gc> {
        if !ephemeron.value.is_cell() {
            // return value, not #f because value can be anything
            return nctx.return_(ephemeron.value);
        }
        // SAFETY: `value` is a cell and we need to perform weak reference load.
        unsafe {
            ctx.raw_weak_reference_load(ephemeron.value.as_cell_raw());
        }

        nctx.return_(ephemeron.value)
    }

    /// A no-op function that is required by SRFI-124. We can't reliably
    /// provide reference barrier as a native procedure. GC can only happen in
    /// between function calls or continuation jumps, so this function does nothing.
    #[scheme(name = "reference-barrier")]
    pub fn reference_barrier(_x: Value<'gc>) -> Value<'gc> {
        nctx.return_(Value::undefined())
    }

    #[scheme(name = "runtime-stats-enable!")]
    pub fn runtime_stats_enable(enabled: bool) -> Value<'gc> {
        set_runtime_stats_enabled(&ctx.state().stats, enabled);
        nctx.return_(Value::undefined())
    }

    #[scheme(name = "runtime-stats-enabled?")]
    pub fn runtime_stats_enabled() -> bool {
        nctx.return_(stats_enabled())
    }

    #[scheme(name = "runtime-stats-report")]
    pub fn runtime_stats_report() -> Value<'gc> {
        let Some(snapshot) = runtime_stats_snapshot(Some(&ctx.state().stats)) else {
            return nctx.return_(Value::new(false));
        };

        nctx.return_(Str::new(*ctx, snapshot.to_string(), true).into())
    }

    #[scheme(name = "%runtime-stats-begin-compilation")]
    pub fn runtime_stats_begin_compilation() -> Value<'gc> {
        ctx.stats.start_compilation();
        nctx.return_(Value::undefined())
    }

    #[scheme(name = "%runtime-stats-end-compilation")]
    pub fn runtime_stats_end_compilation() -> Value<'gc> {
        ctx.stats.end_compilation();
        nctx.return_(Value::undefined())
    }

    #[scheme(name = "%runtime-stats-begin-reader")]
    pub fn runtime_stats_begin_reader() -> Value<'gc> {
        let token = begin_compilation_breakdown(CompilationBreakdownPhase::Reader);
        nctx.return_(runtime_stats_breakdown_token(ctx, token))
    }

    #[scheme(name = "%runtime-stats-end-reader")]
    pub fn runtime_stats_end_reader(token: Value<'gc>) -> Value<'gc> {
        end_compilation_breakdown(parse_runtime_stats_breakdown_token(token));
        nctx.return_(Value::undefined())
    }

    #[scheme(name = "%runtime-stats-begin-psyntax")]
    pub fn runtime_stats_begin_psyntax() -> Value<'gc> {
        let token = begin_compilation_breakdown(CompilationBreakdownPhase::Psyntax);
        nctx.return_(runtime_stats_breakdown_token(ctx, token))
    }

    #[scheme(name = "%runtime-stats-end-psyntax")]
    pub fn runtime_stats_end_psyntax(token: Value<'gc>) -> Value<'gc> {
        end_compilation_breakdown(parse_runtime_stats_breakdown_token(token));
        nctx.return_(Value::undefined())
    }
}

#[cfg(test)]
mod tests {
    use crate::runtime;
    use crate::runtime::{
        GLOBAL_STATS, Scheme,
        stats::{TEST_RUNTIME_LOCK, runtime_stats_snapshot, set_runtime_stats_enabled},
        value::{Str, Value},
    };
    use std::time::Duration;

    fn reset_stats() {
        GLOBAL_STATS.lock().reset_for_test();
    }

    fn with_runtime(f: impl for<'gc> FnOnce(crate::runtime::Context<'gc>)) {
        let scm = Scheme::new_uninit();
        scm.enter(|ctx| {
            if crate::runtime::global::VM_GLOBALS.get().is_none() {
                runtime::init(ctx);
            }
            f(ctx);
        });
    }

    #[test]
    fn runtime_stats_scheme_apis_and_nested_compilation_bridge() {
        let _guard = TEST_RUNTIME_LOCK
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        reset_stats();

        let scm = Scheme::new_uninit();
        scm.enter(|ctx| {
            if crate::runtime::global::VM_GLOBALS.get().is_none() {
                runtime::init(ctx);
            }
        });

        let report_before_enable = scm.call(
            "capy",
            "runtime-stats-report",
            |_ctx, _args| {},
            |_ctx, result| result.unwrap() == Value::new(false),
        );
        assert!(report_before_enable);

        scm.call(
            "capy",
            "runtime-stats-enable!",
            |_ctx, args| args.push(Value::new(true)),
            |_ctx, result| {
                result.unwrap();
            },
        );

        let enabled = scm.call(
            "capy",
            "runtime-stats-enabled?",
            |_ctx, _args| {},
            |_ctx, result| result.unwrap() != Value::new(false),
        );
        assert!(enabled);

        scm.enter(|ctx| {
            let begin = ctx
                .private_ref("capy", "%runtime-stats-begin-compilation")
                .expect("begin compilation hook should be registered");
            let end = ctx
                .private_ref("capy", "%runtime-stats-end-compilation")
                .expect("end compilation hook should be registered");
            let begin_reader = ctx
                .private_ref("capy", "%runtime-stats-begin-reader")
                .expect("begin reader hook should be registered");
            let end_reader = ctx
                .private_ref("capy", "%runtime-stats-end-reader")
                .expect("end reader hook should be registered");
            let begin_psyntax = ctx
                .private_ref("capy", "%runtime-stats-begin-psyntax")
                .expect("begin psyntax hook should be registered");
            let end_psyntax = ctx
                .private_ref("capy", "%runtime-stats-end-psyntax")
                .expect("end psyntax hook should be registered");

            ctx.stats.start_execution();
            assert!(ctx.stats.execution_active());
            assert!(!ctx.stats.compilation_active());

            assert!(matches!(
                crate::runtime::vm::call_scheme(ctx, begin, []),
                crate::runtime::vm::VMResult::Ok(_)
            ));
            assert!(!ctx.stats.execution_active());
            assert!(ctx.stats.compilation_active());

            {
                ctx.stats.start_compilation();
                assert!(!ctx.stats.execution_active());
                assert!(ctx.stats.compilation_active());
                ctx.stats.end_compilation();
            }

            let reader_token = match crate::runtime::vm::call_scheme(ctx, begin_reader, []) {
                crate::runtime::vm::VMResult::Ok(token) => token,
                _ => panic!("reader start should return a token"),
            };
            std::thread::sleep(Duration::from_millis(1));
            assert!(matches!(
                crate::runtime::vm::call_scheme(ctx, end_reader, [reader_token]),
                crate::runtime::vm::VMResult::Ok(_)
            ));

            let psyntax_token = match crate::runtime::vm::call_scheme(ctx, begin_psyntax, []) {
                crate::runtime::vm::VMResult::Ok(token) => token,
                _ => panic!("psyntax start should return a token"),
            };
            std::thread::sleep(Duration::from_millis(1));
            assert!(matches!(
                crate::runtime::vm::call_scheme(ctx, end_psyntax, [psyntax_token]),
                crate::runtime::vm::VMResult::Ok(_)
            ));

            assert!(matches!(
                crate::runtime::vm::call_scheme(ctx, end, []),
                crate::runtime::vm::VMResult::Ok(_)
            ));
            assert!(ctx.stats.execution_active());
            assert!(!ctx.stats.compilation_active());

            ctx.stats.end_execution();

            let snapshot = runtime_stats_snapshot(Some(&ctx.state().stats)).unwrap();
            assert!(snapshot.compilation > Duration::ZERO);
            assert!(snapshot.reader > Duration::ZERO);
            assert!(snapshot.psyntax > Duration::ZERO);
        });

        let report = scm.call(
            "capy",
            "runtime-stats-report",
            |_ctx, _args| {},
            |_ctx, result| result.unwrap().downcast::<Str>().to_string(),
        );
        assert!(report.contains("Runtime Statistics"));
        assert!(report.contains("Reader (reader.scm)"));
        assert!(report.contains("Psyntax (psyntax.scm)"));

        scm.call(
            "capy",
            "runtime-stats-enable!",
            |_ctx, args| args.push(Value::new(false)),
            |_ctx, result| {
                result.unwrap();
            },
        );

        let enabled = scm.call(
            "capy",
            "runtime-stats-enabled?",
            |_ctx, _args| {},
            |_ctx, result| result.unwrap() != Value::new(false),
        );
        assert!(!enabled);
    }

    #[test]
    fn disabling_runtime_stats_freezes_current_snapshot() {
        let _guard = TEST_RUNTIME_LOCK
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        reset_stats();
        with_runtime(|ctx| {
            set_runtime_stats_enabled(&ctx.state().stats, true);
            ctx.stats.start_execution();
            std::thread::sleep(Duration::from_millis(2));
            set_runtime_stats_enabled(&ctx.state().stats, false);

            let frozen = runtime_stats_snapshot(Some(&ctx.state().stats))
                .unwrap()
                .execution;
            std::thread::sleep(Duration::from_millis(2));
            ctx.stats.end_execution();

            let after = runtime_stats_snapshot(Some(&ctx.state().stats))
                .unwrap()
                .execution;
            assert_eq!(after, frozen);
        });
    }
}
