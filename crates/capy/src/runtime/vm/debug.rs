use crate::{
    disassembly::{DisassemblySource, SourceAnnotation, SourceRangeAnnotation},
    expander::{sym_column, sym_filename, sym_line},
    prelude::*,
    runtime::{
        Context,
        value::{Closure, IntoValue, Str, Symbol, Value, Vector},
        vm::thunks::make_io_error,
    },
    static_symbols,
};

pub struct ShadowStack<'gc> {
    buffer: Vec<Option<ShadowFrame<'gc>>>,
    head: usize,
    tail: usize,
    len: usize,
    capacity: usize,
}

pub struct ShadowFrame<'gc> {
    pub ip: u64,
    pub rator: Value<'gc>,
    pub rands: Vec<Value<'gc>>,
    pub meta: Value<'gc>,
}

impl<'gc> ShadowStack<'gc> {
    pub fn new(capacity: usize) -> Self {
        Self {
            buffer: (0..capacity).map(|_| None).collect(),
            head: 0,
            tail: 0,
            len: 0,
            capacity,
        }
    }

    pub fn for_each_mut(&mut self, mut f: impl FnMut(&mut ShadowFrame<'gc>)) {
        for i in 0..self.len {
            let index = (self.head + i) % self.capacity;
            if let Some(frame) = &mut self.buffer[index] {
                f(frame);
            }
        }
    }

    pub fn for_each_recent(&self, mut f: impl FnMut(&ShadowFrame<'gc>)) {
        for i in 0..self.len {
            let index = (self.tail + self.capacity - 1 - i) % self.capacity;
            if let Some(frame) = &self.buffer[index] {
                f(frame);
            }
        }
    }

    pub fn is_full(&self) -> bool {
        self.len == self.capacity
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn capacity(&self) -> usize {
        self.capacity
    }

    pub fn push(&mut self, frame: ShadowFrame<'gc>) {
        if self.is_full() {
            self.head = (self.head + 1) % self.capacity;
        } else {
            self.len += 1;
        }
        self.buffer[self.tail] = Some(frame);
        self.tail = (self.tail + 1) % self.capacity;
    }

    pub fn pop(&mut self) -> Option<ShadowFrame<'gc>> {
        if self.is_empty() {
            return None;
        }

        let item = self.buffer[self.head].take();

        self.head = (self.head + 1) % self.capacity;
        self.len -= 1;

        item
    }

    pub fn clear(&mut self) {
        self.buffer.iter_mut().for_each(|slot| *slot = None);
        self.head = 0;
        self.tail = 0;
        self.len = 0;
    }
}

pub(crate) fn init_debug<'gc>(ctx: Context<'gc>) {
    debug_ops::register(ctx);
}

#[scheme(path=capy)]
pub mod debug_ops {
    #[scheme(name = "print-stacktrace")]
    pub fn print_stacktrace(_args: &'gc [Value<'gc>]) -> () {
        print_stacktraces_impl(nctx.ctx);

        nctx.return_(())
    }

    /// Capture and return the current stacktrace as a list of frames.
    #[scheme(name = "shadow-stack")]
    pub fn shadow_stack() -> Value<'gc> {
        let ctx = nctx.ctx;
        let state = ctx.state();
        // SAFETY: Preconditions verified by the surrounding code
        let shadow_stack = unsafe { &mut *state.shadow_stack.get() };

        let mut frames = Vec::new();
        shadow_stack.for_each_recent(|frame| {
            let rands = frame
                .rands
                .iter()
                .copied()
                .rev()
                .fold(Value::null(), |acc, rand| Value::cons(ctx, rand, acc));
            let frame_vec = Vector::from_slice(
                ctx,
                &[frame.ip.into_value(ctx), frame.meta, frame.rator, rands],
            );

            frames.push(frame_vec);
        });

        let frames = frames.into_iter().rev().fold(Value::null(), |acc, frame| {
            Value::cons(ctx, frame.into(), acc)
        });

        nctx.return_(frames)
    }

    /// Attempt to resolve proper name for a given address
    #[scheme(name = "resolve-address-name")]
    pub fn resolve_address_name(addr: u64) -> Value<'gc> {
        let mut result = Value::new(false);
        backtrace::resolve(addr as _, |sym| {
            if let Some(name) = sym.name() {
                result = Symbol::from_str(nctx.ctx, &name.to_string()).into();
            }
        });
        nctx.return_(result)
    }

    #[scheme(name = "disassembly")]
    pub fn disassembly(proc: Gc<'gc, Closure<'gc>>) -> Result<Value<'gc>, Value<'gc>> {
        let ctx = nctx.ctx;
        let bytes = proc.code_block.unlinked.code();
        if bytes.is_empty() {
            return nctx.return_(Err(make_io_error(
                ctx,
                "disassembly",
                Str::new(ctx, "procedure has no unlinked code bytes", true).into(),
                &[],
            )));
        }

        let function_source = proc.source(ctx).map(|(file, line, column)| {
            SourceAnnotation::new(file.to_string(), line, column, None, None)
        });
        let source =
            disassembly_source_from_metadata(ctx, function_source, proc.code_block.metadata.get());
        let rendered = match crate::disassembly::disassemble_host_with_annotations(
            bytes,
            proc.code_block.entrypoint.as_usize() as u64,
            source.as_ref(),
        ) {
            Ok(rendered) => rendered,
            Err(err) => {
                return nctx.return_(Err(make_io_error(
                    ctx,
                    "disassembly",
                    Str::new(ctx, err, true).into(),
                    &[],
                )));
            }
        };

        nctx.return_(Ok(Str::new(ctx, rendered, true).into()))
    }
}

fn disassembly_source_from_metadata<'gc>(
    ctx: Context<'gc>,
    function_source: Option<SourceAnnotation>,
    metadata: Value<'gc>,
) -> Option<DisassemblySource> {
    let ranges = source_map_from_metadata(ctx, metadata);
    if function_source.is_none() && ranges.is_empty() {
        return None;
    }
    Some(DisassemblySource::new(function_source, ranges))
}

fn source_map_from_metadata<'gc>(
    ctx: Context<'gc>,
    metadata: Value<'gc>,
) -> Vec<SourceRangeAnnotation> {
    if !metadata.is_pair() {
        return Vec::new();
    }

    let key = Symbol::from_str(ctx, "source-map").into();
    let Some(source_map) = metadata.assq(key) else {
        return Vec::new();
    };

    let mut source_map = source_map.cdr();
    let mut ranges = Vec::new();
    while source_map.is_pair() {
        if let Some(range) = source_range_from_metadata_record(source_map.car()) {
            ranges.push(range);
        }
        source_map = source_map.cdr();
    }
    ranges
}

pub(crate) fn stacktrace_source_for_closure<'gc>(
    ctx: Context<'gc>,
    closure: Gc<'gc, Closure<'gc>>,
) -> Value<'gc> {
    let code_metadata = closure.code_block.metadata.get();
    metadata_source(ctx, code_metadata)
        .or_else(|| source_map_primary_source(ctx, code_metadata))
        .or_else(|| {
            let closure_metadata = closure.meta.get();
            metadata_source(ctx, closure_metadata)
                .or_else(|| source_map_primary_source(ctx, closure_metadata))
        })
        .unwrap_or_else(|| Value::new(false))
}

fn metadata_source<'gc>(ctx: Context<'gc>, metadata: Value<'gc>) -> Option<Value<'gc>> {
    if !metadata.is_pair() {
        return None;
    }

    let source = metadata.assq(Symbol::from_str(ctx, "source").into())?.cdr();
    valid_stacktrace_source(source).then_some(source)
}

fn source_map_primary_source<'gc>(ctx: Context<'gc>, metadata: Value<'gc>) -> Option<Value<'gc>> {
    if !metadata.is_pair() {
        return None;
    }

    let key = Symbol::from_str(ctx, "source-map").into();
    let mut source_map = metadata.assq(key)?.cdr();
    while source_map.is_pair() {
        let record = source_map.car();
        if record.is::<Vector>() {
            let record = record.downcast::<Vector>();
            if record.len() >= 3 {
                let source = record[2].get();
                if valid_stacktrace_source(source) {
                    return Some(source);
                }
            }
        }
        source_map = source_map.cdr();
    }

    None
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

fn source_range_from_metadata_record<'gc>(record: Value<'gc>) -> Option<SourceRangeAnnotation> {
    if !record.is::<Vector>() {
        return None;
    }
    let record = record.downcast::<Vector>();
    if record.len() < 3 {
        return None;
    }

    let start = value_u32(record[0].get())?;
    let end = value_u32(record[1].get())?;
    if start >= end {
        return None;
    }
    let source = source_annotation_from_value(record[2].get())?;
    Some(SourceRangeAnnotation::new(start, end, source))
}

fn source_annotation_from_value<'gc>(source: Value<'gc>) -> Option<SourceAnnotation> {
    if !source.is::<Vector>() {
        return None;
    }
    let source = source.downcast::<Vector>();
    if source.len() < 3 {
        return None;
    }

    let file = source[0].get();
    let line = value_u32(source[1].get())?;
    let column = value_u32(source[2].get())?;
    if !file.is::<Str>() {
        return None;
    }
    let end_line = if source.len() >= 4 {
        optional_u32(source[3].get())
    } else {
        None
    };
    let end_column = if source.len() >= 5 {
        optional_u32(source[4].get())
    } else {
        None
    };

    Some(SourceAnnotation::new(
        file.to_string(),
        line,
        column,
        end_line,
        end_column,
    ))
}

fn optional_u32<'gc>(value: Value<'gc>) -> Option<u32> {
    if value == Value::new(false) {
        None
    } else {
        value_u32(value)
    }
}

fn value_u32<'gc>(value: Value<'gc>) -> Option<u32> {
    if !value.is_int32() {
        return None;
    }
    u32::try_from(value.as_int32()).ok()
}

pub const DEBUG_STACKTRACE_KEY: &str = "stacktrace-key b8bec3ca-8174-4219-a964-b1aa2aa53ed5";

static_symbols!(SYM_STACKTRACE_KEY = DEBUG_STACKTRACE_KEY);

pub fn print_stacktraces_impl<'gc>(ctx: Context<'gc>) {
    let state = ctx.state();
    // SAFETY: Preconditions verified by the surrounding code
    let shadow_stack = unsafe { &mut *state.shadow_stack.get() };
    backtrace::trace(|_| {
        shadow_stack.for_each_mut(|frame| {
            /*backtrace::resolve(frame.ip as _, |symbol| {
                loc = Some((
                    symbol.filename().map(|f| f.to_string_lossy().to_string()),
                    symbol.lineno(),
                    symbol.colno(),
                ));
            });

            let mut buf = String::new();

            if let Some(loc) = loc {
                buf.push_str("  at ");
                if let Some(file) = loc.0 {
                    buf.push_str(&file);
                } else {
                    buf.push_str("<unknown file>");
                }
                if let Some(line) = loc.1 {
                    buf.push(':');
                    buf.push_str(&line.to_string());
                }
                if let Some(col) = loc.2 {
                    buf.push(':');
                    buf.push_str(&col.to_string());
                }
            }

            buf.push_str(" in ");
            if frame.rator.is::<Closure>() {
                let clos = frame.rator.downcast::<Closure>();
                if let Some(name) = clos.name(ctx) {
                    buf.push_str(&name.to_string());
                } else {
                    buf.push_str("<anonymous>");
                }
            } else {
                buf.push_str(&frame.rator.to_string());
            }
            buf.push('(');
            for (i, rand) in frame.rands.iter().enumerate() {
                if i > 0 {
                    buf.push_str(", ");
                }
                buf.push_str(&rand.to_string());
            }
            buf.push(')');
            println!("{}", buf);*/
            let src = if frame.meta.is::<Vector>() {
                let v = frame.meta.downcast::<Vector>();
                let filename = v[0].get().downcast::<Str>();
                let line = v[1].get();
                let col = v[2].get();
                format!("{}:{}:{}", filename, line, col)
            } else if frame.meta.is_pair() {
                let filename = frame
                    .meta
                    .assq(sym_filename(ctx).into())
                    .expect("infallible allocation callback")
                    .downcast::<Str>();
                let line = frame
                    .meta
                    .assq(sym_line(ctx).into())
                    .expect("invariant holds");
                let col = frame
                    .meta
                    .assq(sym_column(ctx).into())
                    .expect("invariant holds");
                format!("{}:{}:{}", filename, line, col)
            } else {
                "<unknown>".to_string()
            };

            let proc_name = if frame.rator.is::<Closure>() {
                let name = frame.rator.downcast::<Closure>().name(ctx);
                name.map_or("<anonymous>".to_string(), |n| n.to_string())
            } else {
                frame.rator.to_string()
            };

            log::trace!(" at {}: {}(...)", src, proc_name,);
        });

        false
    });
}
