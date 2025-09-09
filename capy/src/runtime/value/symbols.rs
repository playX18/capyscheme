use std::{
    cell::Cell,
    fmt::Write,
    hash::Hasher,
    sync::{Once, OnceLock, atomic::AtomicU64},
};

use easy_bitfield::{BitField, BitFieldTrait};
use rsgc::{Gc, Global, Mutation, Rootable};

use crate::runtime::{Context, value::*};

pub type SymbolPrefixOffsetBits = BitField<u64, u16, { TypeBits::NEXT_BIT }, 16, false>;

#[derive(Trace)]
#[collect(no_drop)]
#[repr(C, align(8))]
pub struct Symbol<'gc> {
    pub(crate) header: ScmHeader,
    pub(crate) stringbuf: Gc<'gc, Stringbuf>,
    pub(crate) hash: Cell<u64>,
}

unsafe impl<'gc> Tagged for Symbol<'gc> {
    const TC16: &'static [super::TypeCode16] = &[SYMBOL_TC16_INTERNED, SYMBOL_TC16_UNINTERNED];
    const TC8: TypeCode8 = TypeCode8::SYMBOL;
    const TYPE_NAME: &'static str = "symbol";
}

pub const SYMBOL_TC16_UNINTERNED: TypeCode16 =
    TypeCode16(TypeCode8::SYMBOL.bits() as u16 + 0 * 256);
pub const SYMBOL_TC16_INTERNED: TypeCode16 = TypeCode16(TypeCode8::SYMBOL.bits() as u16 + 1 * 256);

pub static SYMBOL_TABLE: OnceLock<Global<Rootable!(Gc<'_, WeakSet<'_>>)>> = OnceLock::new();

static ONCE: Once = Once::new();

pub fn init_symbols<'gc>(mc: &Mutation<'gc>) {
    ONCE.call_once(|| {
        let symbol_table = WeakSet::new(mc, 31);
        let _ = SYMBOL_TABLE.set(Global::new(symbol_table));
    });
}

fn lookup_interned_symbol<'gc>(
    mc: &Mutation<'gc>,
    name: Value<'gc>,
    raw_hash: u64,
) -> Option<Value<'gc>> {
    let name = name.downcast::<Str<'gc>>();
    WeakSet::lookup(
        *SYMBOL_TABLE.get().unwrap().fetch(mc),
        mc,
        raw_hash,
        |_, sym| {
            let sym = sym.downcast::<Symbol<'_>>();
            let mut n = sym.len();
            while n != 0 {
                if sym.get(n) != name.get(n) {
                    return false;
                }
                n -= 1;
            }

            true
        },
    )
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
    pub fn to_str(&self, mc: &Mutation<'gc>) -> Gc<'gc, Str<'gc>> {
        self.substring(mc, 0, self.len())
    }

    pub fn from_string(mc: Context<'gc>, str: Gc<'gc, Str<'gc>>) -> Gc<'gc, Symbol<'gc>> {
        let mut hasher = simplehash::CityHasher64::new();
        str.hash(&mut hasher);
        let hash = hasher.finish();

        let symbol = lookup_interned_symbol(&mc, str.into_value(mc), hash);

        if let Some(symbol) = symbol {
            return symbol.downcast();
        } else {
            let symbol = Self::new::<true>(&mc, str, hash, None);

            WeakSet::add(
                *SYMBOL_TABLE.get().unwrap().fetch(&mc),
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
        str: Gc<'gc, Str<'gc>>,
        prefix_offset: Option<u16>,
    ) -> Gc<'gc, Symbol<'gc>> {
        let mut hasher = simplehash::CityHasher64::new();
        str.hash(&mut hasher);
        let hash = hasher.finish();

        let this = Self::new::<false>(mc, str, hash, prefix_offset);

        this
    }

    pub fn from_str(mc: Context<'gc>, str: &str) -> Gc<'gc, Symbol<'gc>> {
        let str = Str::new(&mc, str, false);
        Self::from_string(mc, str)
    }

    pub fn from_str_uninterned(
        mc: &Mutation<'gc>,
        str: &str,
        prefix_offset: Option<u16>,
    ) -> Gc<'gc, Symbol<'gc>> {
        let str = Str::new(&mc, str, false);
        Self::from_string_uninterned(mc, str, prefix_offset)
    }

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

        let str = Str::new(&mc, string, false);

        Symbol::from_string(mc, str)
    }

    pub fn is_interned(&self) -> bool {
        TypeBits::decode(self.header.word) == SYMBOL_TC16_INTERNED.bits()
    }

    pub fn is_uninterned(&self) -> bool {
        TypeBits::decode(self.header.word) == SYMBOL_TC16_UNINTERNED.bits()
    }

    pub fn uninterned_suffix(&self, ctx: Context<'gc>) -> Gc<'gc, Str<'gc>> {
        let offset = SymbolPrefixOffsetBits::decode(self.header.word);
        if offset == 0 {
            Str::new(&ctx, "", true)
        } else {
            self.substring(&ctx, offset as usize, self.len() - offset as usize)
        }
    }

    pub fn uninterned_prefix(&self, ctx: Context<'gc>) -> Gc<'gc, Str<'gc>> {
        let offset = SymbolPrefixOffsetBits::decode(self.header.word);
        if offset == 0 {
            Str::new(&ctx, "", true)
        } else {
            self.substring(&ctx, 0, offset as usize)
        }
    }
}

impl<'gc> Str<'gc> {
    pub fn to_symbol(self: Gc<'gc, Self>, mc: Context<'gc>) -> Gc<'gc, Symbol<'gc>> {
        Symbol::from_string(mc, self)
    }

    pub fn to_symbol_uninterned(self: Gc<'gc, Self>, mc: &Mutation<'gc>) -> Gc<'gc, Symbol<'gc>> {
        Symbol::from_string_uninterned(mc, self, None)
    }

    pub fn to_symbol_uninterned_with_prefix(
        self: Gc<'gc, Self>,
        mc: &Mutation<'gc>,
        prefix_offset: u16,
    ) -> Gc<'gc, Symbol<'gc>> {
        Symbol::from_string_uninterned(mc, self, Some(prefix_offset))
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
                $crate::rsgc::global::Global<$crate::rsgc::Rootable!(
                    $crate::rsgc::Gc<'_, $crate::runtime::value::Symbol<'_>>
                )>> = ::std::sync::OnceLock::new();
            paste::paste! {
                pub fn [<$name: lower>] <'gc>(mc: $crate::runtime::Context<'gc>) ->
                    $crate::rsgc::Gc<'gc, $crate::runtime::value::Symbol<'gc>>
                {
                    *$name.get_or_init(|| $crate::rsgc::global::Global::new($crate::runtime::value::Symbol::from_str(mc, $value))).fetch(&mc)
                }
            }
        )*
    };
}
