use std::{io::BufRead, path::PathBuf};

use rsgc::{Collect, Gc};

use crate::runtime::{
    Context,
    value::{Pair, Tagged, TypeCode8, Value, Vector},
};

pub struct Reader<'gc, T: BufRead> {
    ctx: Context<'gc>,
    filename: Option<PathBuf>,
    buf: T,
    line: usize,
    column: usize,
    saved_line: usize,
    saved_column: usize,
    fold_case: bool,
    mode: ReaderMode,
    /// Is this reader tolerant to errors?
    tolerant: bool,
}

#[derive(Debug, Clone, Copy)]
pub enum ReaderMode {
    RnRS,
    R5RS,
    R6RS,
    R7RS,
}

impl<'gc, T: BufRead> Reader<'gc, T> {
    pub fn new(ctx: Context<'gc>, buf: T) -> Self {
        Self {
            ctx,
            filename: None,
            buf,
            line: 1,
            column: 0,
            saved_line: 1,
            saved_column: 0,
            fold_case: false,
            mode: ReaderMode::R7RS,
            tolerant: false,
        }
    }

    pub fn set_filename(&mut self, filename: PathBuf) {
        self.filename = Some(filename);
    }

    pub fn mark(&mut self) {
        self.saved_line = self.line;
        self.saved_column = self.column;
    }

    /// Lookahead the next UTF-8 character in the buffer without consuming it.
    pub fn lookahead_char(&mut self) -> Option<std::io::Result<char>> {
        use std::str;
        let buf = match self.buf.fill_buf() {
            Ok(b) => b,
            Err(e) => return Some(Err(e)),
        };
        if buf.is_empty() {
            return None;
        }
        // Try to decode the next UTF-8 char
        let max_len = buf.len().min(4); // UTF-8 chars are at most 4 bytes
        for len in 1..=max_len {
            if let Ok(s) = str::from_utf8(&buf[..len]) {
                if let Some(ch) = s.chars().next() {
                    return Some(Ok(ch));
                }
            }
        }
        // If we get here, the buffer does not contain a valid UTF-8 char yet
        Some(Err(std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            "Invalid UTF-8 sequence in lookahead_char",
        )))
    }
}

#[derive(Collect)]
#[collect(no_drop)]
pub struct Annotation<'gc> {
    pub source: Value<'gc>,
    pub expr: Value<'gc>,
    pub stripped: Value<'gc>,
}

unsafe impl<'gc> Tagged for Annotation<'gc> {
    const TC8: crate::runtime::value::TypeCode8 = TypeCode8::ANNOTATION;
}

pub fn strip_annotations<'gc>(ctx: Context<'gc>, expr: Value<'gc>) -> Value<'gc> {
    if expr.is::<Annotation<'gc>>() {
        let ann = expr.downcast::<Annotation<'gc>>();
        strip_annotations(ctx, ann.expr)
    } else if expr.is::<Pair>() {
        Pair::new(
            ctx,
            strip_annotations(ctx, expr.car()),
            strip_annotations(ctx, expr.cdr()),
        )
        .into()
    } else if expr.is::<Vector>() {
        let orig = expr.downcast::<Vector>();
        let v = Vector::new(&ctx, orig.len(), Value::eof());
        for i in 0..orig.len() {
            Gc::write(&ctx, v)[i].write(strip_annotations(ctx, orig[i]));
        }
        v.into()
    } else {
        expr
    }
}
