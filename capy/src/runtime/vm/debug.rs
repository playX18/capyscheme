use crate::{
    expander::{sym_column, sym_filename, sym_line},
    prelude::*,
    runtime::{
        Context,
        value::{Closure, IntoValue, Str, Symbol, Value, Vector},
    },
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
                *ctx,
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
}
pub fn print_stacktraces_impl<'gc>(ctx: Context<'gc>) {
    let state = ctx.state();
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
                    .unwrap()
                    .downcast::<Str>();
                let line = frame.meta.assq(sym_line(ctx).into()).unwrap();
                let col = frame.meta.assq(sym_column(ctx).into()).unwrap();
                format!("{}:{}:{}", filename, line, col)
            } else {
                format!("<unknown>")
            };

            let proc_name = if frame.rator.is::<Closure>() {
                let name = frame.rator.downcast::<Closure>().name(ctx);
                name.map_or("<anonymous>".to_string(), |n| n.to_string())
            } else {
                frame.rator.to_string()
            };

            println!(" at {}: {}(...)", src, proc_name,);
        });

        false
    });
}
