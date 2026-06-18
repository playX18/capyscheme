//! Symbols, interning, and keywords.

use std::{
    borrow::Cow,
    cell::Cell,
    fmt::Write,
    hash::Hasher,
    sync::{Once, OnceLock, atomic::AtomicU64},
};

use crate::rsgc::object::{ClassId, builtin_class_ids, class_header_word};
use crate::rsgc::{Gc, Global, Mutation};
use crate::runtime::{Context, value::*};

#[derive(Trace)]
#[collect(no_drop)]
#[repr(C, align(8))]
/// A Scheme symbol backed by an immutable string buffer.
pub struct Symbol<'gc> {
    pub(crate) stringbuf: Gc<'gc, Stringbuf>,
    pub(crate) hash: Cell<u64>,
    pub(crate) prefix_offset: u16,
}

// SAFETY: `gc` for `Symbol` upholds all trait invariants
unsafe impl<'gc> ClassTagged for Symbol<'gc> {
    const CLASS_IDS: &'static [u32] = &[crate::rsgc::object::builtin_class_ids::SYMBOL];
    const TYPE_NAME: &'static str = "symbol";
}

type RootedSymbolTable = crate::Rootable!(Gc<'_, WeakSet<'_>>);
pub type RootedSymbol = crate::Rootable!(Gc<'_, Symbol<'_>>);

pub static SYMBOL_TABLE: OnceLock<Global<RootedSymbolTable>> = OnceLock::new();

static ONCE: Once = Once::new();

#[allow(dead_code)]
pub(crate) fn symbol_table<'gc>(mc: Mutation<'gc>) -> Gc<'gc, WeakSet<'gc>> {
    *SYMBOL_TABLE.get().unwrap().fetch(mc)
}

pub fn init_symbols<'gc>(mc: Mutation<'gc>) {
    ONCE.call_once(|| {
        let symbol_table = WeakSet::new(mc, 31);
        let _ = SYMBOL_TABLE.set(Global::new(symbol_table));
    });
}

fn lookup_interned_symbol<'gc>(
    mc: Mutation<'gc>,
    name: Value<'gc>,
    raw_hash: u64,
) -> Option<Value<'gc>> {
    let name = name.downcast::<Str<'gc>>();
    WeakSet::lookup(
        *SYMBOL_TABLE.get().unwrap().fetch(mc),
        mc,
        raw_hash,
        |_, sym| {
            if !sym.is::<Symbol>() {
                panic!("Non-symbol in symbol table: {sym:?}");
            }
            let sym = sym.downcast::<Symbol<'_>>();

            if sym.len() != name.len() {
                return false;
            }

            for n in 0..sym.len() {
                if sym.get(n) != name.get(n) {
                    return false;
                }
            }

            true
        },
    )
}

fn symbool_lookup_predicate<'gc>(
    mc: Mutation<'gc>,
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
    /// Returns this symbol as an immutable string.
    pub fn to_str(&self, mc: Mutation<'gc>) -> Gc<'gc, Str<'gc>> {
        self.substring(mc, 0, self.len())
    }

    /// Interns a string and returns the canonical symbol for its contents.
    pub fn from_string(mc: Context<'gc>, str: Gc<'gc, Str<'gc>>) -> Gc<'gc, Symbol<'gc>> {
        let mut hasher = simplehash::CityHasher64::new();
        str.hash(&mut hasher);
        let hash = hasher.finish();

        let symbol = lookup_interned_symbol(*mc, str.into_value(mc), hash);

        if let Some(symbol) = symbol {
            symbol.downcast()
        } else {
            let sym = Self::new::<true>(*mc, str, hash, None);

            WeakSet::add(
                *SYMBOL_TABLE.get().unwrap().fetch(*mc),
                *mc,
                hash,
                |mc, s| symbool_lookup_predicate(mc, s, sym),
                Value::new(sym),
            )
            .downcast()
        }
    }

    /// Allocates a fresh uninterned symbol from a string.
    pub fn from_string_uninterned(
        mc: Mutation<'gc>,
        str: Gc<'gc, Str<'gc>>,
        prefix_offset: Option<u16>,
    ) -> Gc<'gc, Symbol<'gc>> {
        let mut hasher = simplehash::CityHasher64::new();
        str.hash(&mut hasher);
        let hash = hasher.finish();

        Self::new::<false>(mc, str, hash, prefix_offset)
    }

    /// Interns a UTF-8 string slice.
    pub fn from_str(mc: Context<'gc>, str: &str) -> Gc<'gc, Symbol<'gc>> {
        let str = Str::new(*mc, str, false);
        Self::from_string(mc, str)
    }

    /// Allocates an uninterned symbol from a UTF-8 string slice.
    pub fn from_str_uninterned(
        mc: Mutation<'gc>,
        str: &str,
        prefix_offset: Option<u16>,
    ) -> Gc<'gc, Symbol<'gc>> {
        let str = Str::new(mc, str, false);
        Self::from_string_uninterned(mc, str, prefix_offset)
    }

    /// Generates a fresh symbol with an optional prefix.
    pub fn gensym(mc: Context<'gc>, prefix: Option<Gc<'gc, Str<'gc>>>) -> Gc<'gc, Symbol<'gc>> {
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

        let str = Str::new(*mc, string, false);

        Symbol::from_string(mc, str)
    }

    pub fn is_interned(&self) -> bool {
        !heap_header(self).private_variant_flag()
    }

    pub fn is_uninterned(&self) -> bool {
        heap_header(self).private_variant_flag()
    }

    pub fn uninterned_suffix(&self, ctx: Context<'gc>) -> Gc<'gc, Str<'gc>> {
        let offset = self.prefix_offset;
        if offset == 0 {
            Str::new(*ctx, "", true)
        } else {
            self.substring(*ctx, offset as usize, self.len() - offset as usize)
        }
    }

    pub fn uninterned_prefix(&self, ctx: Context<'gc>) -> Gc<'gc, Str<'gc>> {
        let offset = self.prefix_offset;
        if offset == 0 {
            Str::new(*ctx, "", true)
        } else {
            self.substring(*ctx, 0, offset as usize)
        }
    }

    pub fn as_str(&self) -> Cow<'gc, str> {
        if let Some(ascii) = self.chars() {
            // SAFETY: The byte content is known to be valid UTF-8
            let s = unsafe { std::str::from_utf8_unchecked(ascii) };
            Cow::Borrowed(s)
        } else if let Some(wide) = self.wide_chars() {
            let s: String = wide.iter().collect();
            Cow::Owned(s)
        } else {
            unreachable!()
        }
    }
}

impl<'gc> Str<'gc> {
    pub fn as_str(&self) -> Cow<'gc, str> {
        if let Some(ascii) = self.chars() {
            // SAFETY: The byte content is known to be valid UTF-8
            let s = unsafe { std::str::from_utf8_unchecked(ascii) };
            Cow::Borrowed(s)
        } else if let Some(wide) = self.wide_chars() {
            let s: String = wide.iter().collect();
            Cow::Owned(s)
        } else {
            unreachable!()
        }
    }

    pub fn to_symbol(self: Gc<'gc, Self>, mc: Context<'gc>) -> Gc<'gc, Symbol<'gc>> {
        Symbol::from_string(mc, self)
    }

    pub fn to_symbol_uninterned(self: Gc<'gc, Self>, mc: Mutation<'gc>) -> Gc<'gc, Symbol<'gc>> {
        Symbol::from_string_uninterned(mc, self, None)
    }

    pub fn to_symbol_uninterned_with_prefix(
        self: Gc<'gc, Self>,
        mc: Mutation<'gc>,
        prefix_offset: u16,
    ) -> Gc<'gc, Symbol<'gc>> {
        Symbol::from_string_uninterned(mc, self, Some(prefix_offset))
    }
}
impl<'gc> std::fmt::Display for Symbol<'gc> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(chars) = self.chars() {
            // SAFETY: The byte content is known to be valid UTF-8
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

impl<'gc> std::fmt::Debug for Symbol<'gc> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

impl<'gc> PartialEq for Symbol<'gc> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }
}

impl<'gc> Eq for Symbol<'gc> {}

impl<'gc> PartialEq<str> for Symbol<'gc> {
    fn eq(&self, str: &str) -> bool {
        if let Some(bytes) = self.chars()
            && str.is_ascii()
        {
            return str.as_bytes() == bytes;
        }

        if str.chars().count() != self.len() {
            return false;
        }

        let wide = self.wide_chars().unwrap();

        str.chars().zip(wide.iter()).all(|(c, &w)| c == w)
    }
}

#[macro_export]
macro_rules! static_symbols {
    ($(
        $(#[$outer:meta])*
        $v: vis $name: ident = $value: expr
    )*) => {
        $(
            $(#[$outer])*
            pub static $name: ::std::sync::OnceLock<
                $crate::rsgc::global::Global<$crate::runtime::value::RootedSymbol>
            > = ::std::sync::OnceLock::new();
            paste::paste! {
                pub fn [<$name: lower>] <'gc>(mc: $crate::runtime::Context<'gc>) ->
                    $crate::rsgc::Gc<'gc, $crate::runtime::value::Symbol<'gc>>
                {
                    *$name.get_or_init(|| $crate::rsgc::global::Global::new($crate::runtime::value::Symbol::from_str(mc, $value))).fetch(*mc)
                }
            }
        )*
    };
}

/// A self-evaluating symbol wrapper.
#[derive(Trace)]
#[repr(C)]
pub struct Keyword<'gc> {
    pub symbol: Gc<'gc, Symbol<'gc>>,
}

fn keyword_header_word() -> u64 {
    class_header_word(ClassId::new(builtin_class_ids::KEYWORD).unwrap())
}

// SAFETY: `gc` for `Keyword` upholds all trait invariants
unsafe impl<'gc> ClassTagged for Keyword<'gc> {
    const CLASS_IDS: &'static [u32] = &[crate::rsgc::object::builtin_class_ids::KEYWORD];
    const TYPE_NAME: &'static str = "keyword";
}

impl<'gc> Keyword<'gc> {
    /// Allocates a keyword for `symbol`.
    pub fn from_symbol(mc: Mutation<'gc>, symbol: Gc<'gc, Symbol<'gc>>) -> Gc<'gc, Keyword<'gc>> {
        Gc::new_with_header_word(mc, Keyword { symbol }, keyword_header_word())
    }

    pub fn to_symbol(&self) -> Gc<'gc, Symbol<'gc>> {
        self.symbol
    }
}
