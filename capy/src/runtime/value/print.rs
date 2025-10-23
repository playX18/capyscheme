use super::*;
use std::collections::HashMap;
use std::fmt::Formatter;
pub struct ValueFmt<'gc, 'a, 'b> {
    pub table: HashMap<Value<'gc>, std::result::Result<i32, Value<'gc>>>,
    pub fmt: &'b mut Formatter<'a>,
}

impl<'gc, 'a, 'b> ValueFmt<'gc, 'a, 'b> {
    pub fn new(fmt: &'b mut Formatter<'a>) -> Self {
        Self {
            table: HashMap::new(),
            fmt,
        }
    }

    fn print_cdr(
        &mut self,
        x: Value<'gc>,
        slashify: bool,
        level: usize,
        length: usize,
    ) -> std::fmt::Result {
        if x.is_null() {
            write!(self.fmt, ")")
        } else if !slashify && length == 0 {
            write!(self.fmt, " ...)")
        } else if x.is_pair() {
            write!(self.fmt, " ")?;
            self.print(x.car(), slashify, level)?;
            write!(self.fmt, " ")?;
            self.print_cdr(x.cdr(), slashify, level, length.saturating_sub(1))
        } else {
            write!(self.fmt, " . ")?;
            self.print(x, slashify, level)?;
            write!(self.fmt, ")")
        }
    }

    fn is_interesting(&self, x: Value<'gc>) -> bool {
        x.is_pair() || x.is::<Vector>() || x.is::<ByteVector>() || x.is::<Syntax<'gc>>()
    }

    pub fn print(&mut self, x: Value<'gc>, slashify: bool, level: usize) -> std::fmt::Result {
        if self.is_interesting(x) {
            if let Some(id) = self.table.get(&x).cloned() {
                match id {
                    Ok(n) => return write!(self.fmt, "#{n}#"),

                    Err(obj) => {
                        let c = self.table.len();
                        self.table.insert(x, Ok(c as i32));
                        write!(self.fmt, "#{}=", c)?;
                        self.print(obj, slashify, level)?;
                        return Ok(());
                    }
                }
            } else {
                self.table.insert(x, Err(x));
            }
        }
        if !slashify && level == 0 {
            write!(self.fmt, "...")
        } else if !x.is_pair() {
            self.patom(x, slashify, level)
        } else {
            write!(self.fmt, "(")?;
            self.print(x.car(), slashify, level.saturating_sub(1))?;
            self.print_cdr(x.cdr(), slashify, level.saturating_sub(1), usize::MAX)
        }
    }

    pub fn patom(&mut self, x: Value<'gc>, slashify: bool, level: usize) -> std::fmt::Result {
        if x.is_null() {
            write!(self.fmt, "()")
        } else if x == Value::new(false) {
            write!(self.fmt, "#f")
        } else if x == Value::new(true) {
            write!(self.fmt, "#t")
        } else if x.is::<Symbol>() {
            write!(self.fmt, "{}", x.downcast::<Symbol>())
        } else if x.is::<Str>() {
            let s = x.downcast::<Str>();
            if slashify {
                write!(self.fmt, "\"{s:?}\"")
            } else {
                write!(self.fmt, "{s}")
            }
        } else if x.is_number() {
            let n = x.number().unwrap();
            write!(self.fmt, "{}", n)
        } else if x == Value::undefined() {
            write!(self.fmt, "#<unspecified>")
        } else if x == Value::eof() {
            write!(self.fmt, "#<eof>")
        } else if x.is::<ByteVector>() {
            let bv = x.downcast::<ByteVector>();
            write!(self.fmt, "#u8(")?;
            for (i, byte) in bv.iter().enumerate() {
                if i > 0 {
                    write!(self.fmt, " ")?;
                }
                write!(self.fmt, "{byte}")?;
            }
            write!(self.fmt, ")")
        } else if x.is::<Vector>() {
            let vec = x.downcast::<Vector>();
            write!(self.fmt, "#(")?;
            for (i, item) in vec.iter().enumerate() {
                if i > 0 && i < vec.len() - 1 {
                    write!(self.fmt, " ")?;
                }
                self.print(item.get(), slashify, level.saturating_sub(1))?;
            }
            write!(self.fmt, ")")
        } else if x.is::<HashTable<'gc>>() {
            let _ht = x.downcast::<HashTable<'gc>>();
            write!(self.fmt, "#<hashtable>",)
        } else if x.is::<Closure<'gc>>() {
            let closure = x.downcast::<Closure<'gc>>();
            write!(self.fmt, "#<procedure {}>", closure.code)
        } else if x.is::<Tuple>() {
            let tuple = x.downcast::<Tuple>();
            write!(self.fmt, "#tuple(")?;
            for (i, item) in tuple.iter().enumerate() {
                if i > 0 && i < tuple.len() - 1 {
                    write!(self.fmt, " ")?;
                }
                self.print(item.get(), slashify, level.saturating_sub(1))?;
            }
            write!(self.fmt, ")")
        } else if x.is::<Syntax>() {
            let syntax = x.downcast::<Syntax<'gc>>();
            write!(self.fmt, "#<syntax {}>", syntax.expr())
        } else if x.is::<Module>() {
            let x = x.downcast::<Module>();
            write!(self.fmt, "#<module {}>", x.name())
        } else {
            write!(self.fmt, "#<unknown {:x}>", x.bits())
        }
    }
}
