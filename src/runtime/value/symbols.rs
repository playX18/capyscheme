use std::{
    cell::Cell,
    fmt::Write,
    hash::Hasher,
    sync::{Once, OnceLock, atomic::AtomicU64},
};

use rsgc::{Collect, Gc, Rootable, context::Mutation, mutator::Global};

use crate::runtime::{Context, vm::inherents::Inherent};

use super::*;
use weak_set::WeakSet;
#[derive(Collect)]
#[collect(no_drop)]
pub struct Symbol<'gc> {
    pub(crate) inherent: Inherent,
    pub(super) stringbuf: Gc<'gc, Stringbuf>,
    pub(super) hash: Cell<u64>,
}

unsafe impl<'gc> Tagged for Symbol<'gc> {
    const TC8: TypeCode8 = TypeCode8::SYMBOL;
    const TC16: &'static [TypeCode16] = &[SYMBOL_TC16_UNINTERNED, SYMBOL_TC16_INTERNED];
}

pub const SYMBOL_TC16_UNINTERNED: TypeCode16 =
    TypeCode16(TypeCode8::SYMBOL.bits() as u16 + 0 * 256);
pub const SYMBOL_TC16_INTERNED: TypeCode16 = TypeCode16(TypeCode8::SYMBOL.bits() as u16 + 1 * 256);

pub static SYMBOL_TABLE: OnceLock<Global<Rootable!(Gc<'_, WeakSet<'_>>)>> = OnceLock::new();

static ONCE: Once = Once::new();

pub fn init_symbols<'gc>(mc: &Mutation<'gc>) {
    ONCE.call_once(|| {
        let symbol_table = WeakSet::new(mc, 128);
        let _ = SYMBOL_TABLE.set(Global::new(symbol_table));
    });
}

fn lookup_interned_symbol<'gc>(
    mc: &Mutation<'gc>,
    name: Value<'gc>,
    raw_hash: u64,
) -> Option<Value<'gc>> {
    let name = name.downcast::<String<'gc>>();
    SYMBOL_TABLE
        .get()
        .unwrap()
        .fetch(mc)
        .lookup(mc, raw_hash, |_, sym| {
            let sym = sym.downcast::<Symbol<'_>>();
            let mut n = sym.len();
            while n != 0 {
                if sym.get(n) != name.get(n) {
                    return false;
                }
                n -= 1;
            }

            true
        })
}

fn symbool_lookup_predicate<'gc>(
    mc: &Mutation<'gc>,
    sym: Value<'gc>,
    other: Gc<'gc, Symbol<'gc>>,
) -> bool {
    let sym = sym.downcast::<Symbol<'_>>();

    if sym.hash.get() == other.hash.get() && sym.len() == other.len() {
        if sym.is_narrow() {
            return other.is_narrow() && sym.chars() == other.chars();
        } else {
            return sym.to_str(mc) == other.to_str(mc);
        }
    }

    false
}

impl<'gc> Symbol<'gc> {
    pub fn to_str(&self, mc: &Mutation<'gc>) -> Gc<'gc, String<'gc>> {
        self.substring(mc, 0, self.len())
    }

    pub fn from_string(mc: Context<'gc>, str: Gc<'gc, String<'gc>>) -> Gc<'gc, Symbol<'gc>> {
        let mut hasher = simplehash::CityHasher64::new();
        str.hash(&mut hasher);
        let hash = hasher.finish();

        let symbol = lookup_interned_symbol(&mc, str.into_value(mc), hash);

        if let Some(symbol) = symbol {
            return symbol.downcast();
        } else {
            let symbol = Self::new(&mc, str, hash, None);

            SYMBOL_TABLE
                .get()
                .unwrap()
                .fetch(&mc)
                .add(
                    &mc,
                    hash,
                    |mc, sym| symbool_lookup_predicate(mc, sym, symbol),
                    Value::new(symbol),
                )
                .downcast()
        }
    }

    pub fn from_string_uninterned(
        mc: &Mutation<'gc>,
        str: Gc<'gc, String<'gc>>,
    ) -> Gc<'gc, Symbol<'gc>> {
        let mut hasher = simplehash::CityHasher64::new();
        str.hash(&mut hasher);
        let hash = hasher.finish();

        let this = Self::new(mc, str, hash, None);

        this.set_user_header(SYMBOL_TC16_UNINTERNED.into());

        this
    }

    pub fn from_str(mc: Context<'gc>, str: &str) -> Gc<'gc, Symbol<'gc>> {
        let str = String::new(&mc, str, false);
        Self::from_string(mc, str)
    }

    pub fn inherent(&self) -> Inherent {
        self.inherent
    }

    pub(crate) fn new_inherent(mc: Context<'gc>, str: &str, i: Inherent) -> Gc<'gc, Symbol<'gc>> {
        let str = String::new(&mc, str, false);
        let this = Self::from_string(mc, str);
        unsafe {
            this.as_ptr().as_mut().inherent = i;
        }
        this
    }

    pub fn gensym(mc: Context<'gc>, prefix: Option<Gc<'gc, String<'gc>>>) -> Gc<'gc, Symbol<'gc>> {
        static GENSYM_COUNTER: AtomicU64 = AtomicU64::new(0);

        let string = match prefix {
            Some(prefix) => {
                format!(
                    "{prefix}{}",
                    GENSYM_COUNTER.fetch_add(1, std::sync::atomic::Ordering::SeqCst)
                )
            }
            None => {
                format!(
                    " g{}",
                    GENSYM_COUNTER.fetch_add(1, std::sync::atomic::Ordering::SeqCst)
                )
            }
        };

        let str = String::new(&mc, string, false);

        Symbol::from_string(mc, str)
    }
}

impl<'gc> String<'gc> {
    pub fn to_symbol(self: Gc<'gc, Self>, mc: Context<'gc>) -> Gc<'gc, Symbol<'gc>> {
        Symbol::from_string(mc, self)
    }

    pub fn to_symbol_uninterned(self: Gc<'gc, Self>, mc: &Mutation<'gc>) -> Gc<'gc, Symbol<'gc>> {
        Symbol::from_string_uninterned(mc, self)
    }
}
impl<'gc> std::fmt::Display for Symbol<'gc> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(chars) = self.chars() {
            unsafe {
                f.write_str(std::str::from_utf8_unchecked(chars))?;
            }

            Ok(())
        } else if let Some(chars) = self.wide_chars() {
            for &c in chars.iter() {
                f.write_char(c)?;
            }
            Ok(())
        } else {
            unreachable!()
        }
    }
}
/*
#[cfg(test)]
mod tests {
    use rsgc::Mutator;

    use crate::runtime;

    use super::*;

    #[test]
    fn test_symbols() {
        let m = Mutator::<Rootable!((Value<'_>, Value<'_>))>::new(|mc| {
            runtime::init(mc);

            let str = String::new(mc, "hello", false);
            let str_emoji = String::new(mc, "👋", false);
            let x = Symbol::from_string(mc, str);
            let y = Symbol::from_string(mc, str_emoji);

            assert!(Gc::ptr_eq(x, Symbol::from_string(mc, str)));
            assert!(Gc::ptr_eq(y, Symbol::from_string(mc, str_emoji)));

            (Value::new(x), Value::new(y))
        });

        m.request_gc();

        m.mutate(|mc, (orig_x, orig_y)| {
            let str = String::new(mc, "hello", false);
            let str_emoji = String::new(mc, "👋", false);
            let x = Symbol::from_string(mc, str);
            let y = Symbol::from_string(mc, str_emoji);

            assert!(Gc::ptr_eq(x, orig_x.downcast()));
            assert!(Gc::ptr_eq(y, orig_y.downcast()));
        });
    }
}*/

impl<'gc> PartialEq<&str> for Symbol<'gc> {
    fn eq(&self, other: &&str) -> bool {
        if let Some(chars) = self.chars() {
            unsafe {
                return std::str::from_utf8_unchecked(chars) == *other;
            }
        } else if let Some(wide_chars) = self.wide_chars() {
            let count = other.chars().count();
            if count != wide_chars.len() {
                return false;
            }

            let mut i = 0;
            for c in other.chars() {
                if c != wide_chars[i] {
                    return false;
                }
                i += 1;
            }
            return true;
        } else {
            unreachable!()
        }
    }
}
