//! Stack traces and other debugging entry points.
//!
//! The Scheme stack holds real frames, so a backtrace is a walk over it:
//! each frame names its closure, and each return address maps to a source
//! location through the source map of the code block it points into.

use crate::{
    compiler::bytecode::disasm,
    prelude::*,
    runtime::{
        MutationContext,
        interp::stack,
        value::{Closure, Str, Symbol, Value, Vector, proc::lookup_srcmap},
        vm::{control, thunks::make_io_error},
    },
    static_symbols,
};
/// Frames reported by a backtrace at most.
const BACKTRACE_LIMIT: usize = 64;

/// One frame of a backtrace: `#(source procedure arguments)`, the shape the
/// Scheme stack-trace printer expects. Arguments are not recoverable from a
/// frame, so the list is empty.
fn backtrace_frames<'gc>(ctx: MutationContext<'gc>) -> Vec<Value<'gc>> {
    // SAFETY: the stack state is current whenever native code runs.
    let frames = unsafe { stack::backtrace(ctx, BACKTRACE_LIMIT) };
    frames
        .into_iter()
        .map(|(procedure, resume)| {
            let source = match procedure.try_as::<Closure>() {
                Some(closure) => {
                    let code_block = closure.code_block;
                    code_block
                        .pc_of(resume)
                        .and_then(|pc| code_block.source_at_exact(pc))
                        .map(|hit| hit.span)
                        .filter(|source| valid_stacktrace_source(*source))
                        .unwrap_or_else(|| stacktrace_source_for_closure(ctx, closure))
                }
                None => Value::new(false),
            };
            Vector::from_slice(ctx, &[source, procedure, Value::null()]).into()
        })
        .collect()
}

pub(crate) fn init_debug<'gc>(ctx: MutationContext<'gc>) {
    debug_ops::register(ctx);
}

#[scheme(path=capy)]
pub mod debug_ops {
    #[scheme(name = "print-stacktrace")]
    pub fn print_stacktrace(_args: &'gc [Value<'gc>]) -> () {
        print_stacktraces_impl(ctx);

        ()
    }

    /// Compile the procedure `proc` with the baseline JIT now.
    /// True if it has compiled code afterwards.
    #[scheme(name = "%jit-compile!")]
    pub fn jit_compile(proc: Value<'gc>) -> bool {
        #[cfg(all(not(windows), any(target_arch = "x86_64", target_arch = "aarch64")))]
        if let Some(closure) = proc.try_as::<Closure>()
            && crate::runtime::codegen::template_image::runs_template(ctx)
        {
            let code_block = closure.code_block;
            if code_block.kind != crate::runtime::value::CodeBlockKind::Bytecode {
                return false;
            }
            if !code_block
                .jit_entry
                .is_decided(std::sync::atomic::Ordering::Relaxed)
                && let Err(err) = crate::runtime::codegen::jit::compile(ctx, code_block)
            {
                log::warn!(target: crate::heap::logging::JIT_LOG_TARGET, " {err}");
            }
            return code_block
                .jit_entry
                .entry(std::sync::atomic::Ordering::Relaxed)
                .is_some();
        }
        let _ = proc;
        false
    }

    /// The number of procedures with live compiled code.
    #[scheme(name = "%jit-compiled-count")]
    pub fn jit_compiled_count() -> usize {
        #[cfg(all(not(windows), any(target_arch = "x86_64", target_arch = "aarch64")))]
        return crate::runtime::codegen::jit::compiled_count();
        #[cfg(not(all(not(windows), any(target_arch = "x86_64", target_arch = "aarch64"))))]
        0
    }

    /// Native disassembly of `proc`'s compiled code, or `#f` when it has
    /// none (native, uncompiled, or another engine).
    #[scheme(name = "%jit-disassembly")]
    pub fn jit_disassembly(proc: Value<'gc>) -> Value<'gc> {
        #[cfg(all(not(windows), any(target_arch = "x86_64", target_arch = "aarch64")))]
        if let Some(closure) = proc.try_as::<Closure>()
            && let Some(listing) = crate::runtime::codegen::jit::listing_for(closure.code_block)
        {
            return Str::new(ctx, listing, true).into();
        }
        let _ = proc;
        Value::new(false)
    }

    /// The number of frames in the continuation `k` and below it
    /// (`S_continuation_depth`).
    #[scheme(name = "%continuation-depth")]
    pub fn continuation_depth(k: Value<'gc>) -> Value<'gc> {
        if !control::is_reified_continuation_value(k) {
            return call.wrong_argument_violation(
                "%continuation-depth",
                "expected a continuation",
                Some(k),
                Some(0),
                1,
                &[k],
            );
        }
        // Free var 1 of a reified continuation is the captured `Continuation` (or
        // `#f` for the null continuation).
        let continuation = k.downcast::<Closure>()[1].get();
        // SAFETY: the captured stack is intact while Scheme runs, and the
        // walk only reads return addresses.
        let depth = unsafe { stack::continuation_depth(continuation) };
        (depth).into_value(ctx)
    }

    /// The continuation holding frame `n` of `k`, counting the top frame
    /// as 0 and linked to the frames below it, or `#f` when `k` holds
    /// fewer than `n + 1` frames (`S_single_continuation`).
    #[scheme(name = "%single-continuation")]
    pub fn single_continuation(k: Value<'gc>, n: usize) -> Value<'gc> {
        if !control::is_reified_continuation_value(k) {
            return call.wrong_argument_violation(
                "%single-continuation",
                "expected a continuation",
                Some(k),
                Some(0),
                2,
                &[k],
            );
        }
        let clos = k.downcast::<Closure>();
        let continuation = clos[1].get();
        let winders = clos[2].get();
        let marks = clos[3].get();
        // SAFETY: as above, splitting only allocates continuation objects,
        // and allocation never collects.
        let single = unsafe { stack::single_continuation(ctx, continuation, n) };
        if single == Value::new(false) {
            return single;
        }
        // The split piece is multi-shot, wrap it as `call/cc` does, with
        // the captured winders and marks.
        let proc = control::make_closure_reified_cc(ctx, [single, winders, marks]);
        proc.into()
    }

    /// The current stack as a list of `#(source procedure arguments)`
    /// frames, innermost first.
    #[scheme(name = "shadow-stack")]
    pub fn shadow_stack() -> Value<'gc> {
        let frames = backtrace_frames(ctx)
            .into_iter()
            .rev()
            .fold(Value::null(), |acc, frame| Value::cons(ctx, frame, acc));
        frames
    }

    /// Attempt to resolve proper name for a given address
    #[scheme(name = "resolve-address-name")]
    pub fn resolve_address_name(addr: u64) -> Value<'gc> {
        let mut result = Value::new(false);
        backtrace::resolve(addr as _, |sym| {
            if let Some(name) = sym.name() {
                result = Symbol::from_str(ctx, &name.to_string()).into();
            }
        });
        result
    }

    /// Bytecode listing of a procedure.
    #[scheme(name = "disassembly")]
    pub fn disassembly(proc: Gc<'gc, Closure<'gc>>) -> Result<Value<'gc>, Value<'gc>> {
        if proc.code_block.is_native() {
            return Err(make_io_error(
                ctx,
                "disassembly",
                Str::new(ctx, "procedure is native and has no bytecode", true).into(),
                &[],
            ));
        }
        let code_block = proc.code_block;
        let srcmap = code_block.srcmap.get();
        let rendered =
            disasm::disassemble_with_spans(code_block.code(), &code_block.buffer.pool(), &|pc| {
                lookup_srcmap(srcmap, pc)
            });
        Ok(Str::new(ctx, rendered, true).into())
    }
}

pub(crate) fn stacktrace_source_for_closure<'gc>(
    ctx: MutationContext<'gc>,
    closure: Gc<'gc, Closure<'gc>>,
) -> Value<'gc> {
    let code_metadata = closure.code_block.metadata.get();
    metadata_source(ctx, code_metadata)
        .or_else(|| metadata_source(ctx, closure.meta.get()))
        .unwrap_or_else(|| Value::new(false))
}

fn metadata_source<'gc>(ctx: MutationContext<'gc>, metadata: Value<'gc>) -> Option<Value<'gc>> {
    if !metadata.is_pair() {
        return None;
    }

    let source = metadata.assq(Symbol::from_str(ctx, "source").into())?.cdr();
    valid_stacktrace_source(source).then_some(source)
}

fn valid_stacktrace_source<'gc>(source: Value<'gc>) -> bool {
    if !source.is::<Vector>() {
        return false;
    }
    let source = source.downcast::<Vector>();
    if source.len() < 3 {
        return false;
    }

    source[0].get().is::<Str>() && source[1].get().is_int32() && source[2].get().is_int32()
}

pub const DEBUG_STACKTRACE_KEY: &str = "stacktrace-key b8bec3ca-8174-4219-a964-b1aa2aa53ed5";

static_symbols!(SYM_STACKTRACE_KEY = DEBUG_STACKTRACE_KEY);

pub fn print_stacktraces_impl<'gc>(ctx: MutationContext<'gc>) {
    if !log::log_enabled!(log::Level::Trace) {
        return;
    }
    for frame in backtrace_frames(ctx) {
        let frame = frame.downcast::<Vector>();
        let source = frame[0].get();
        let location = if valid_stacktrace_source(source) {
            let source = source.downcast::<Vector>();
            format!(
                "{}:{}:{}",
                source[0].get().downcast::<Str>(),
                source[1].get(),
                source[2].get()
            )
        } else {
            "<unknown>".to_string()
        };
        let procedure = frame[1].get();
        let name = match procedure.try_as::<Closure>() {
            Some(closure) => closure
                .name(ctx)
                .map_or("<anonymous>".to_string(), |name| name.to_string()),
            None => procedure.to_string(),
        };
        log::trace!(" at {location}: {name}(...)");
    }
}
