use std::cmp::Ordering;
use std::sync::LazyLock;

use unicode_general_category::GeneralCategory;
use unicode_general_category::get_general_category;
use unicode_normalization::*;

use crate::global;
use crate::heap::Gc;
use crate::prelude::*;
use crate::runtime::prelude::*;
use crate::runtime::vm::vector::Endianness;

pub static LOCALE: LazyLock<icu::locale::Locale> = LazyLock::new(|| {
    let locale = sys_locale::get_locale().unwrap_or_else(|| "en-US".to_string());
    locale
        .parse()
        .unwrap_or_else(|_| icu::locale::Locale::try_from_str("en-US").expect("invariant holds"))
});

type OptionalIndex = Option<usize>;

fn utf16_bom_type(bv: &[u8]) -> Option<Endianness> {
    if bv.len() < 2 {
        return None;
    }

    if bv[0] == 0xFF && bv[1] == 0xFE {
        Some(Endianness::Little)
    } else if bv[0] == 0xFE && bv[1] == 0xFF {
        Some(Endianness::Big)
    } else {
        None
    }
}

fn u8_to_u16(input: &[u8], is_little_endian: bool) -> Vec<u16> {
    let mut output: Vec<u16> = Vec::with_capacity(input.len() / 2);

    for chunk in input.as_chunks::<2>().0 {
        let value = if is_little_endian {
            u16::from_le_bytes([chunk[0], chunk[1]])
        } else {
            u16::from_be_bytes([chunk[0], chunk[1]])
        };
        output.push(value);
    }
    output
}

fn utf32_bom_type(bv: &[u8]) -> Option<Endianness> {
    if bv.len() < 4 {
        return None;
    }

    if bv[0] == 0xFF && bv[1] == 0xFE && bv[2] == 0x00 && bv[3] == 0x00 {
        Some(Endianness::Little)
    } else if bv[0] == 0x00 && bv[1] == 0x00 && bv[2] == 0xFE && bv[3] == 0xFF {
        Some(Endianness::Big)
    } else {
        None
    }
}

/// Helper for chained string ordering comparisons (>, >=, <, <=, and their -ci variants).
fn string_compare_chain<'gc>(
    call: &Call<'_, 'gc>,
    rest: &[Value<'gc>],
    name: &str,
    case_insensitive: bool,
    accept: &[Ordering],
) -> Value<'gc> {
    if rest.is_empty() {
        return call.wrong_argument_violation(
            name,
            "at least 1 argument required",
            None,
            None,
            0,
            rest,
        );
    }

    if rest.len() < 2 {
        return Value::new(true);
    }

    let s1 = rest[0];
    if !s1.is::<Str>() {
        return call.wrong_argument_violation(
            name,
            "expected a string",
            Some(s1),
            Some(1),
            rest.len(),
            rest,
        );
    }
    let mut s1 = s1.downcast::<Str>();

    for (i, val) in rest.iter().enumerate().skip(1) {
        if !val.is::<Str>() {
            return call.wrong_argument_violation(
                name,
                "expected a string",
                Some(*val),
                Some(i + 1),
                rest.len(),
                rest,
            );
        }
        let s2 = val.downcast::<Str>();
        let cmp = Str::compare(&s1, &s2, case_insensitive, None, None, None, None);
        if !cmp.is_some_and(|o| accept.contains(&o)) {
            return Value::new(false);
        }
        s1 = s2;
    }

    Value::new(true)
}

#[scheme(path=capy)]
mod string_ops {
    use crate::runtime::vm::vector::{Endianness, sym_big, sym_little};

    #[scheme(name = "string=?")]
    pub fn string_equal(rest: &'gc [Value<'gc>]) -> Value<'gc> {
        if rest.len() < 2 {
            return (true).into();
        }

        let s1 = rest[0];
        if !s1.is::<Str>() {
            return call.wrong_argument_violation(
                "string=?",
                "expected a string",
                Some(s1),
                Some(1),
                rest.len(),
                rest,
            );
        }
        let s1 = s1.downcast::<Str>();
        for (i, val) in rest.iter().enumerate().skip(1) {
            if !val.is::<Str>() {
                return call.wrong_argument_violation(
                    "string=?",
                    "expected a string",
                    Some(*val),
                    Some(i + 1),
                    rest.len(),
                    rest,
                );
            }
            let s2 = val.downcast::<Str>();
            if s1 != s2 {
                return (false).into();
            }
        }
        (true).into()
    }

    #[scheme(name = "string-ci=?")]
    pub fn string_ci_equal(rest: &'gc [Value<'gc>]) -> Value<'gc> {
        if rest.len() < 2 {
            return (true).into();
        }

        let s1 = rest[0];
        let s2 = rest[1];
        if !s1.is::<Str>() {
            return call.wrong_argument_violation(
                "string-ci=?",
                "expected a string",
                Some(s1),
                Some(1),
                rest.len(),
                rest,
            );
        }

        if !s2.is::<Str>() {
            return call.wrong_argument_violation(
                "string-ci=?",
                "expected a string",
                Some(s2),
                Some(2),
                rest.len(),
                rest,
            );
        }

        let mut s1 = s1.downcast::<Str>();
        let mut s2 = s2.downcast::<Str>();

        let cm = icu::casemap::CaseMapperBorrowed::new();
        for (i, r) in rest.iter().skip(2).copied().enumerate() {
            let s1str = s1.as_str();
            let s2str = s2.as_str();
            let s1l = cm.fold_string(&s1str);
            let s2l = cm.fold_string(&s2str);
            if s1l != s2l {
                return (false).into();
            }
            if !r.is::<Str>() {
                return call.wrong_argument_violation(
                    "string-ci=?",
                    "expected a string",
                    Some(r),
                    Some(i + 2),
                    rest.len(),
                    rest,
                );
            }
            s1 = s2;
            s2 = r.downcast::<Str>();
        }

        let s1str = s1.as_str();
        let s2str = s2.as_str();
        let s1l = cm.fold_string(&s1str);
        let s2l = cm.fold_string(&s2str);

        let cmp = if s1l == s2l {
            Some(Ordering::Equal)
        } else {
            None
        };

        Value::new(matches!(cmp, Some(Ordering::Equal)))
    }

    #[scheme(name = "string>?")]
    pub fn string_gt(rest: &'gc [Value<'gc>]) -> Value<'gc> {
        string_compare_chain(call, rest, "string>?", false, &[Ordering::Greater])
    }

    #[scheme(name = "string-ci>?")]
    pub fn string_ci_gt(rest: &'gc [Value<'gc>]) -> Value<'gc> {
        string_compare_chain(call, rest, "string-ci>?", true, &[Ordering::Greater])
    }

    #[scheme(name = "string>=?")]
    pub fn string_ge(rest: &'gc [Value<'gc>]) -> Value<'gc> {
        string_compare_chain(
            call,
            rest,
            "string>=?",
            false,
            &[Ordering::Greater, Ordering::Equal],
        )
    }

    #[scheme(name = "string-ci>=?")]
    pub fn string_ci_ge(rest: &'gc [Value<'gc>]) -> Value<'gc> {
        string_compare_chain(
            call,
            rest,
            "string-ci>=?",
            true,
            &[Ordering::Greater, Ordering::Equal],
        )
    }

    #[scheme(name = "string<?")]
    pub fn string_lt(rest: &'gc [Value<'gc>]) -> Value<'gc> {
        string_compare_chain(call, rest, "string<?", false, &[Ordering::Less])
    }

    #[scheme(name = "string-ci<?")]
    pub fn string_ci_lt(rest: &'gc [Value<'gc>]) -> Value<'gc> {
        string_compare_chain(call, rest, "string-ci<?", true, &[Ordering::Less])
    }

    #[scheme(name = "string<=?")]
    pub fn string_le(rest: &'gc [Value<'gc>]) -> Value<'gc> {
        string_compare_chain(
            call,
            rest,
            "string<=?",
            false,
            &[Ordering::Less, Ordering::Equal],
        )
    }

    #[scheme(name = "string-ci<=?")]
    pub fn string_ci_le(rest: &'gc [Value<'gc>]) -> Value<'gc> {
        string_compare_chain(
            call,
            rest,
            "string-ci<=?",
            true,
            &[Ordering::Less, Ordering::Equal],
        )
    }

    #[scheme(name = "string-length")]
    pub fn string_length(str: Gc<'gc, Str<'gc>>) -> usize {
        let len = str.len();
        len
    }

    #[scheme(name = "string-append")]
    pub fn string_append(strs: &'gc [Value<'gc>]) -> Result<Gc<'gc, Str<'gc>>, Value<'gc>> {
        if strs.is_empty() {
            let s = Str::new(ctx, "", false);
            return Ok(s);
        }

        if strs.len() == 1 {
            if !strs[0].is::<Str>() {
                call.wrong_argument_violation(
                    "string-append",
                    "expected a string",
                    Some(strs[0]),
                    Some(1),
                    1,
                    &[strs[0]],
                );
                return Err(Value::undefined());
            }

            return Ok(strs[0].downcast::<Str>());
        }

        // Size the buffer first: one allocation, no growth, and no
        // per-argument temporary string. Type errors raise with the same
        // argument index as the appending loop below did (no partial
        // state escapes either way). Wide lengths are upper bounds (4
        // bytes per char), so the capacity never under-fits.
        let mut total = 0usize;
        for (i, str) in strs.iter().enumerate() {
            if !str.is::<Str>() {
                call.wrong_argument_violation(
                    "string-append",
                    "expected a string",
                    Some(*str),
                    Some(i),
                    strs.len(),
                    strs,
                );
                return Err(Value::undefined());
            }
            let str = str.downcast::<Str>();
            total += if str.is_narrow() {
                str.len()
            } else {
                str.len() * 4
            };
        }

        use std::fmt::Write as _;
        let mut buffer = String::with_capacity(total);
        for str in strs.iter() {
            write!(buffer, "{}", str.downcast::<Str>()).unwrap();
        }
        let s = Ok(Str::new(ctx, &buffer, false));
        s
    }

    #[scheme(name = "symbol-append")]
    pub fn symbol_append(syms: &'gc [Value<'gc>]) -> Result<Gc<'gc, Symbol<'gc>>, Value<'gc>> {
        if syms.is_empty() {
            let s = Symbol::from_str(ctx, "");
            return Ok(s);
        }

        if syms.len() == 1 {
            if !syms[0].is::<Symbol>() {
                call.wrong_argument_violation(
                    "symbol-append",
                    "expected a symbol",
                    Some(syms[0]),
                    Some(1),
                    1,
                    &[syms[0]],
                );
                return Err(Value::undefined());
            }

            return Ok(syms[0].downcast::<Symbol>());
        }

        let mut buffer = String::new();

        for (i, sym) in syms.iter().enumerate() {
            if !sym.is::<Symbol>() {
                call.wrong_argument_violation(
                    "symbol-append",
                    "expected a symbol",
                    Some(*sym),
                    Some(i),
                    syms.len(),
                    syms,
                );
                return Err(Value::undefined());
            }

            buffer.push_str(&sym.downcast::<Symbol>().to_string());
        }
        let s = Ok(Symbol::from_str(ctx, &buffer));
        s
    }

    #[scheme(name = "symbol->string")]
    pub fn symbol_to_string(sym: Gc<'gc, Symbol<'gc>>) -> Gc<'gc, Str<'gc>> {
        let s = sym.to_str(ctx);

        s
    }

    #[scheme(name = "string->symbol")]
    pub fn string_to_symbol(str: Gc<'gc, Str<'gc>>) -> Gc<'gc, Symbol<'gc>> {
        let sym = Symbol::from_string(ctx, str);
        sym
    }

    #[scheme(name = "string->uninterned-symbol")]
    pub fn string_to_uninterned_symbol(str: Gc<'gc, Str<'gc>>) -> Gc<'gc, Symbol<'gc>> {
        let sym = Symbol::from_string_uninterned(ctx, str, None);
        sym
    }

    #[scheme(name = "uninterned-symbol?")]
    pub fn is_uninterned_symbol(sym: Gc<'gc, Symbol<'gc>>) -> bool {
        sym.is_uninterned()
    }

    #[scheme(name = "string-fill!")]
    pub fn string_fill_destructive(
        str: Gc<'gc, Str<'gc>>,
        ch: char,
        start: Option<usize>,
        end: Option<usize>,
    ) -> Value<'gc> {
        let start = start.unwrap_or(0);
        let end = end.unwrap_or(str.len());

        if start > end {
            return call.wrong_argument_violation(
                "string-fill!",
                "start greater than end",
                None,
                None,
                3,
                &[str.into(), start.into_value(ctx), end.into_value(ctx)],
            );
        }

        if end > str.len() {
            return call.wrong_argument_violation(
                "string-fill!",
                "end out of bounds",
                None,
                None,
                3,
                &[str.into(), end.into_value(ctx)],
            );
        }

        for i in start..end {
            if !Str::set(str, ctx, i, ch) {
                return call.wrong_argument_violation(
                    "string-fill!",
                    "string is immutable",
                    None,
                    None,
                    3,
                    &[str.into(), ch.into_value(ctx)],
                );
            }
        }

        Value::undefined()
    }

    /// prints all arguments using their `Display` implementation, followed by a newline.
    #[scheme(name = ":print")]
    pub fn print(args: &'gc [Value<'gc>]) -> () {
        for arg in args.iter() {
            print!("{}", arg);
        }
        println!();
        ()
    }

    #[scheme(name = "substring")]
    pub fn substring(
        str: Gc<'gc, Str<'gc>>,
        start: usize,
        end: Option<usize>,
    ) -> Result<Gc<'gc, Str<'gc>>, Value<'gc>> {
        let end = end.unwrap_or(str.len());
        if start > end || end > str.len() {
            call.wrong_argument_violation(
                "substring",
                "invalid start or end",
                None,
                None,
                3,
                &[str.into(), start.into_value(ctx), end.into_value(ctx)],
            );
            return Err(Value::undefined());
        }

        let s = Str::substring(str, ctx, start, end);

        Ok(s)
    }

    #[scheme(name = "string?")]
    pub fn is_string(val: Value<'gc>) -> bool {
        val.is::<Str>()
    }

    #[scheme(name = "symbol?")]
    pub fn is_symbol(val: Value<'gc>) -> bool {
        val.is::<Symbol>()
    }

    #[scheme(name = "string-prefix?")]
    pub fn string_prefix(prefix: Gc<'gc, Str<'gc>>, str: Gc<'gc, Str<'gc>>) -> bool {
        str.to_string().starts_with(&prefix.to_string())
    }

    #[scheme(name = "string-copy")]
    pub fn string_copy(s1: Gc<'gc, Str<'gc>>) -> Gc<'gc, Str<'gc>> {
        let s2 = Str::substring_copy(s1, ctx, 0, s1.len());
        s2
    }

    #[scheme(name = "string-contains")]
    pub fn string_contains(
        s1: StringRef<'gc>,
        s2: StringRef<'gc>,
        start1: OptionalIndex,
        end1: OptionalIndex,
        start2: OptionalIndex,
        end2: OptionalIndex,
    ) -> Value<'gc> {
        let mut cstart1 = if let Some(start1) = start1 {
            if start1 >= s1.len() {
                return call.wrong_argument_violation(
                    "string-contains",
                    "start1 out of bounds",
                    None,
                    None,
                    3,
                    &[s1.into(), s2.into(), start1.into_value(ctx)],
                );
            }
            start1
        } else {
            0
        };

        let cend1 = if let Some(end1) = end1 {
            if end1 > s1.len() {
                return call.wrong_argument_violation(
                    "string-contains",
                    "end1 out of bounds",
                    None,
                    None,
                    4,
                    &[s1.into(), s2.into(), end1.into_value(ctx)],
                );
            }
            end1
        } else {
            s1.len()
        };

        if cstart1 > cend1 {
            return call.wrong_argument_violation(
                "string-contains",
                "start1 greater than end1",
                None,
                None,
                3,
                &[
                    s1.into(),
                    s2.into(),
                    cstart1.into_value(ctx),
                    cend1.into_value(ctx),
                ],
            );
        }

        let cstart2 = if let Some(start2) = start2 {
            if start2 >= s2.len() {
                return call.wrong_argument_violation(
                    "string-contains",
                    "start2 out of bounds",
                    None,
                    None,
                    5,
                    &[s1.into(), s2.into(), start2.into_value(ctx)],
                );
            }
            start2
        } else {
            0
        };

        let cend2 = if let Some(end2) = end2 {
            if end2 > s2.len() {
                return call.wrong_argument_violation(
                    "string-contains",
                    "end2 out of bounds",
                    None,
                    None,
                    6,
                    &[s1.into(), s2.into(), end2.into_value(ctx)],
                );
            }
            end2
        } else {
            s2.len()
        };

        let len2 = cend2 - cstart2;

        if cend1 - cstart1 >= len2 {
            while cstart1 <= cend1 - len2 {
                let mut i = cstart1;
                let mut j = cstart2;

                while i < cend1 && j < cend2 && s1.get(i) == s2.get(j) {
                    i += 1;
                    j += 1;
                }

                if j == cend2 {
                    return cstart1.into_value(ctx);
                }
                cstart1 += 1;
            }
        }

        Value::new(false)
    }

    #[scheme(name = "string-contains-ci")]
    pub fn string_contains_ci(
        s1: StringRef<'gc>,
        s2: StringRef<'gc>,
        start1: OptionalIndex,
        end1: OptionalIndex,
        start2: OptionalIndex,
        end2: OptionalIndex,
    ) -> Value<'gc> {
        let mut cstart1 = if let Some(start1) = start1 {
            if start1 >= s1.len() {
                return call.wrong_argument_violation(
                    "string-contains-ci",
                    "start1 out of bounds",
                    None,
                    None,
                    3,
                    &[s1.into(), s2.into(), start1.into_value(ctx)],
                );
            }
            start1
        } else {
            0
        };

        let cend1 = if let Some(end1) = end1 {
            if end1 > s1.len() {
                return call.wrong_argument_violation(
                    "string-contains-ci",
                    "end1 out of bounds",
                    None,
                    None,
                    4,
                    &[s1.into(), s2.into(), end1.into_value(ctx)],
                );
            }
            end1
        } else {
            s1.len()
        };

        if cstart1 > cend1 {
            return call.wrong_argument_violation(
                "string-contains-ci",
                "start1 greater than end1",
                None,
                None,
                3,
                &[
                    s1.into(),
                    s2.into(),
                    cstart1.into_value(ctx),
                    cend1.into_value(ctx),
                ],
            );
        }

        let cstart2 = if let Some(start2) = start2 {
            if start2 >= s2.len() {
                return call.wrong_argument_violation(
                    "string-contains-ci",
                    "start2 out of bounds",
                    None,
                    None,
                    5,
                    &[s1.into(), s2.into(), start2.into_value(ctx)],
                );
            }
            start2
        } else {
            0
        };

        let cend2 = if let Some(end2) = end2 {
            if end2 > s2.len() {
                return call.wrong_argument_violation(
                    "string-contains-ci",
                    "end2 out of bounds",
                    None,
                    None,
                    6,
                    &[s1.into(), s2.into(), end2.into_value(ctx)],
                );
            }
            end2
        } else {
            s2.len()
        };

        let len2 = cend2 - cstart2;

        if cend1 - cstart1 >= len2 {
            while cstart1 <= cend1 - len2 {
                let mut i = cstart1;
                let mut j = cstart2;

                while i < cend1
                    && j < cend2
                    && s1
                        .get(i)
                        .expect("infallible allocation callback")
                        .to_lowercase()
                        .eq(s2.get(j).expect("index in range").to_lowercase())
                {
                    i += 1;
                    j += 1;
                }

                if j == cend2 {
                    return cstart1.into_value(ctx);
                }
                cstart1 += 1;
            }
        }

        Value::new(false)
    }

    #[scheme(name = "string-null?")]
    pub fn string_null(str: Gc<'gc, Str<'gc>>) -> bool {
        str.is_empty()
    }
    #[scheme(name = "string-split")]
    pub fn string_split(str: Gc<'gc, Str<'gc>>, ch: char) -> Value<'gc> {
        let parts = str
            .to_string()
            .split(ch)
            .rev()
            .fold(Value::null(), |acc, part| {
                Value::cons(ctx, Str::new(ctx, part, false).into(), acc)
            });
        parts
    }

    #[scheme(name = "string")]
    pub fn string(chars: &'gc [Value<'gc>]) -> Result<Gc<'gc, Str<'gc>>, Value<'gc>> {
        let mut buffer = String::with_capacity(chars.len());

        for (i, ch_val) in chars.iter().enumerate() {
            if !ch_val.is_char() {
                call.wrong_argument_violation(
                    "string",
                    "expected a character",
                    Some(*ch_val),
                    Some(i + 1),
                    chars.len(),
                    chars,
                );
                return Err(Value::undefined());
            }
            let ch = ch_val.char();
            buffer.push(ch);
        }

        let s = Str::new(ctx, &buffer, false);
        Ok(s)
    }

    #[scheme(name = "make-string")]
    pub fn make_string(length: usize, fill_char: Option<char>) -> Gc<'gc, Str<'gc>> {
        let ch = fill_char.unwrap_or(0x20u8 as char);
        let s = Str::from_char(ctx, ch, length);
        s
    }

    #[scheme(name = "string-ref")]
    pub fn string_ref(str: Gc<'gc, Str<'gc>>, index: usize) -> Result<char, Value<'gc>> {
        if index >= str.len() {
            call.wrong_argument_violation(
                "string-ref",
                "index out of bounds",
                None,
                None,
                2,
                &[str.into(), index.into_value(ctx)],
            );
            return Err(Value::undefined());
        }

        let ch = str.get(index).expect("index in range"); // safe due to the bounds check above

        Ok(ch)
    }

    #[scheme(name = "string-set!")]
    pub fn string_set(str: Gc<'gc, Str<'gc>>, index: usize, ch: char) -> Result<(), Value<'gc>> {
        if index >= str.len() {
            call.wrong_argument_violation(
                "string-set!",
                "index out of bounds",
                None,
                None,
                3,
                &[str.into(), index.into_value(ctx), ch.into_value(ctx)],
            );
            return Err(Value::undefined());
        }

        if !Str::set(str, ctx, index, ch) {
            call.wrong_argument_violation(
                "string-set!",
                "string is immutable",
                None,
                None,
                3,
                &[str.into(), index.into_value(ctx), ch.into_value(ctx)],
            );
            return Err(Value::undefined());
        }

        Ok(())
    }

    #[scheme(name = "utf8->string")]
    pub fn utf8_to_string(bytes: Gc<'gc, ByteVector>) -> Result<Gc<'gc, Str<'gc>>, Value<'gc>> {
        match std::str::from_utf8(bytes.as_slice()) {
            Ok(s) => {
                let str = Str::new(ctx, s, false);
                Ok(str)
            }
            Err(_) => {
                call.wrong_argument_violation(
                    "utf8->string",
                    "invalid UTF-8 bytevector",
                    None,
                    None,
                    1,
                    &[bytes.into()],
                );
                Err(Value::undefined())
            }
        }
    }

    #[scheme(name = "string->utf8")]
    pub fn string_to_utf8(str: Gc<'gc, Str<'gc>>) -> Gc<'gc, ByteVector> {
        let bytes = str.to_string().into_bytes();
        // Default (movable): general conversion. FFI paths use string->utf8/nul,
        // string->pointer, or bytevector->pointer (which copies to NonMoving).
        let bytevec = ByteVector::from_slice(ctx, &bytes, true);
        bytevec
    }

    #[scheme(name = "string->utf8/nul")]
    pub fn string_to_utf8_nul(str: Gc<'gc, Str<'gc>>) -> Gc<'gc, ByteVector> {
        let mut bytes = str.to_string().into_bytes();
        bytes.push(0); // NUL terminator
        let bytevec = ByteVector::from_slice(ctx, &bytes, false);
        bytevec
    }

    #[scheme(name = "utf16->string")]
    pub fn utf16_to_string(
        bytes: Gc<'gc, ByteVector>,
        endian: Option<Endianness>,
        mandatory: Option<bool>,
    ) -> Result<Gc<'gc, Str<'gc>>, Value<'gc>> {
        let mut endianness = None;
        let mut skip_bom = false;

        if mandatory.is_none() {
            endianness = utf16_bom_type(&bytes[..]);
            if endianness.is_some() {
                skip_bom = true;
            }
        }

        let endianness_mandatory = mandatory.is_some_and(|m| m);

        if endianness_mandatory || endianness.is_none() {
            endianness = endian;
        }

        let is_little = endianness == Some(Endianness::Little);
        let skip_size = if skip_bom { 2 } else { 0 };

        let utf16_data: Vec<u16> = u8_to_u16(&bytes[skip_size..], is_little);

        let s = String::from_utf16_lossy(&utf16_data[..]);

        let str = Str::new(ctx, &s, false);
        Ok(str)
    }

    #[scheme(name = "string->utf16")]
    pub fn string_to_utf16(
        str: Gc<'gc, Str<'gc>>,
        endian: Option<Endianness>,
    ) -> Gc<'gc, ByteVector> {
        let endian = endian.unwrap_or(Endianness::Big);
        let utf16_data: Vec<u16> = str.to_string().encode_utf16().collect();
        let mut bytes = Vec::with_capacity(utf16_data.len() * 2);
        for code_unit in utf16_data {
            let ebytes = match endian {
                Endianness::Little => code_unit.to_le_bytes(),
                Endianness::Big => code_unit.to_be_bytes(),
            };
            bytes.extend_from_slice(&ebytes);
        }

        let bytevec = ByteVector::from_slice(ctx, &bytes, true);
        bytevec
    }

    #[scheme(name = "string->utf32")]
    pub fn string_to_utf32(str: Gc<'gc, Str<'gc>>, endian: Option<Value<'gc>>) -> Value<'gc> {
        let endian = if endian == Some(sym_little(ctx).into()) {
            Endianness::Little
        } else if endian == Some(sym_big(ctx).into()) || endian.is_none() {
            Endianness::Big // default
        } else {
            return call.wrong_argument_violation(
                "string->utf32",
                "endian must be 'little, 'big, or omitted",
                endian,
                Some(2),
                2,
                &[str.into(), endian.expect("invariant holds")],
            );
        };

        let utf32_data: Vec<u32> = str
            .to_string()
            .chars()
            .map(|ch| match endian {
                Endianness::Little => ch as u32,
                Endianness::Big => (ch as u32).to_be(),
            })
            .collect();
        let mut bytes = Vec::with_capacity(utf32_data.len() * 4);
        for code_point in utf32_data {
            bytes.extend_from_slice(&code_point.to_le_bytes());
        }

        let bytevec = ByteVector::from_slice(ctx, &bytes, true);
        (bytevec).into()
    }

    #[scheme(name = "utf32->string")]
    pub fn utf32_to_string(
        bytes: Gc<'gc, ByteVector>,
        endian: Option<Endianness>,
        mandatory: Option<bool>,
    ) -> Result<Gc<'gc, Str<'gc>>, Value<'gc>> {
        let mut endianness = None;
        let mut skip_bom = false;

        if mandatory.is_none() {
            endianness = utf32_bom_type(&bytes[..]);
            if endianness.is_some() {
                skip_bom = true;
            }
        }

        let endianness_mandatory = mandatory.is_some_and(|m| m);

        if endianness_mandatory || endianness.is_none() {
            endianness = endian;
        }

        let is_little = endianness == Some(Endianness::Little);
        let skip_size = if skip_bom { 4 } else { 0 };

        let data = &bytes[skip_size..];
        let bv = bytes;
        let mut s = String::new();
        let mut i = 0;

        loop {
            if i + 4 > data.len() {
                break;
            }

            let bytes = &data[i..i + 4];
            let value = match is_little {
                true => u32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]),
                false => u32::from_be_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]),
            };

            match char::from_u32(value) {
                Some(ch) => s.push(ch),
                None => {
                    call.wrong_argument_violation(
                        "utf32->string",
                        "invalid UTF-32 code point",
                        None,
                        None,
                        1,
                        &[bv.into()],
                    );
                    return Err(Value::undefined());
                }
            }
            i += 4;
        }
        let str = Str::new(ctx, &s, false);

        Ok(str)
    }

    #[scheme(name = "char-general-category")]
    pub fn char_general_category(ch: char) -> Gc<'gc, Symbol<'gc>> {
        use unicode_general_category::get_general_category;

        let category = get_general_category(ch);
        let category_str = match category {
            unicode_general_category::GeneralCategory::UppercaseLetter => "Lu",
            unicode_general_category::GeneralCategory::LowercaseLetter => "Ll",
            unicode_general_category::GeneralCategory::TitlecaseLetter => "Lt",
            unicode_general_category::GeneralCategory::ModifierLetter => "Lm",
            unicode_general_category::GeneralCategory::OtherLetter => "Lo",
            unicode_general_category::GeneralCategory::NonspacingMark => "Mn",
            unicode_general_category::GeneralCategory::SpacingMark => "Mc",
            unicode_general_category::GeneralCategory::EnclosingMark => "Me",
            unicode_general_category::GeneralCategory::DecimalNumber => "Nd",
            unicode_general_category::GeneralCategory::LetterNumber => "Nl",
            unicode_general_category::GeneralCategory::OtherNumber => "No",
            unicode_general_category::GeneralCategory::ConnectorPunctuation => "Pc",
            unicode_general_category::GeneralCategory::DashPunctuation => "Pd",
            unicode_general_category::GeneralCategory::OpenPunctuation => "Ps",
            unicode_general_category::GeneralCategory::ClosePunctuation => "Pe",
            unicode_general_category::GeneralCategory::InitialPunctuation => "Pi",
            unicode_general_category::GeneralCategory::FinalPunctuation => "Pf",
            unicode_general_category::GeneralCategory::OtherPunctuation => "Po",
            unicode_general_category::GeneralCategory::MathSymbol => "Sm",
            unicode_general_category::GeneralCategory::CurrencySymbol => "Sc",
            unicode_general_category::GeneralCategory::ModifierSymbol => "Sk",
            unicode_general_category::GeneralCategory::OtherSymbol => "So",
            unicode_general_category::GeneralCategory::SpaceSeparator => "Zs",
            unicode_general_category::GeneralCategory::LineSeparator => "Zl",
            unicode_general_category::GeneralCategory::ParagraphSeparator => "Zp",
            unicode_general_category::GeneralCategory::Control => "Cc",
            unicode_general_category::GeneralCategory::Format => "Cf",
            unicode_general_category::GeneralCategory::Surrogate => "Cs",
            unicode_general_category::GeneralCategory::PrivateUse => "Co",
            unicode_general_category::GeneralCategory::Unassigned => "Cn",
            _ => "Cn", // Shouldn't happen
        };

        let sym = Symbol::from_str(ctx, category_str);
        sym
    }

    #[scheme(name = "list->string")]
    pub fn list_to_string(lst: Value<'gc>) -> Result<Gc<'gc, Str<'gc>>, Value<'gc>> {
        if !lst.is_list() {
            call.wrong_argument_violation(
                "list->string",
                "expected a list",
                Some(lst),
                Some(1),
                1,
                &[lst],
            );
            return Err(Value::undefined());
        }
        let mut walk = lst;
        let mut buffer = String::new();

        while walk.is_pair() {
            let ch_val = walk.car();
            if !ch_val.is_char() {
                call.wrong_argument_violation(
                    "list->string",
                    "expected a list of characters",
                    Some(ch_val),
                    Some(1),
                    1,
                    &[lst],
                );
                return Err(Value::undefined());
            }
            let ch = ch_val.char();
            buffer.push(ch);
            walk = walk.cdr();
        }

        if !walk.is_null() {
            call.wrong_argument_violation(
                "list->string",
                "expected a proper list",
                Some(walk),
                Some(1),
                1,
                &[lst],
            );
            return Err(Value::undefined());
        }

        let s = Str::new(ctx, &buffer, false);
        Ok(s)
    }

    #[scheme(name = "char=?")]
    pub fn char_equal(rest: &'gc [Value<'gc>]) -> Value<'gc> {
        if rest.len() < 2 {
            return (true).into();
        }

        let ch1_val = rest[0];
        if !ch1_val.is_char() {
            return call.wrong_argument_violation(
                "char=?",
                "expected a character",
                Some(ch1_val),
                Some(1),
                rest.len(),
                rest,
            );
        }
        let ch1 = ch1_val.char();

        for (i, val) in rest.iter().enumerate().skip(1) {
            if !val.is_char() {
                return call.wrong_argument_violation(
                    "char=?",
                    "expected a character",
                    Some(*val),
                    Some(i + 1),
                    rest.len(),
                    rest,
                );
            }
            let ch2 = val.char();
            if ch1 != ch2 {
                return (false).into();
            }
        }
        (true).into()
    }

    #[scheme(name = "char-ci=?")]
    pub fn char_ci_equal(rest: &'gc [Value<'gc>]) -> Value<'gc> {
        if rest.len() < 2 {
            return (true).into();
        }

        let ch1_val = rest[0];
        if !ch1_val.is_char() {
            return call.wrong_argument_violation(
                "char-ci=?",
                "expected a character",
                Some(ch1_val),
                Some(1),
                rest.len(),
                rest,
            );
        }
        let cm = icu::casemap::CaseMapperBorrowed::new();

        let ch1 = cm.simple_fold(ch1_val.char());

        for (i, val) in rest.iter().enumerate().skip(1) {
            if !val.is_char() {
                return call.wrong_argument_violation(
                    "char-ci=?",
                    "expected a character",
                    Some(*val),
                    Some(i + 1),
                    rest.len(),
                    rest,
                );
            }
            let ch2 = cm.simple_fold(val.char());
            if ch1 != ch2 {
                return (false).into();
            }
        }
        (true).into()
    }

    #[scheme(name = "char<?")]
    pub fn char_lt(rest: &'gc [Value<'gc>]) -> Value<'gc> {
        if rest.is_empty() {
            return call.wrong_argument_violation(
                "char<?",
                "at least 1 argument required",
                None,
                None,
                0,
                rest,
            );
        }

        if rest.len() < 2 {
            return (true).into();
        }

        let ch1_val = rest[0];
        if !ch1_val.is_char() {
            return call.wrong_argument_violation(
                "char<?",
                "expected a character",
                Some(ch1_val),
                Some(1),
                rest.len(),
                rest,
            );
        }

        let mut ch1 = ch1_val.char();

        for (i, val) in rest.iter().enumerate().skip(1) {
            if !val.is_char() {
                return call.wrong_argument_violation(
                    "char<?",
                    "expected a character",
                    Some(*val),
                    Some(i + 1),
                    rest.len(),
                    rest,
                );
            }
            let ch2 = val.char();
            if ch1 >= ch2 {
                return (false).into();
            }
            ch1 = ch2;
        }
        (true).into()
    }

    #[scheme(name = "char-ci<?")]
    pub fn char_ci_lt(rest: &'gc [Value<'gc>]) -> Value<'gc> {
        if rest.is_empty() {
            return call.wrong_argument_violation(
                "char-ci<?",
                "at least 1 argument required",
                None,
                None,
                0,
                rest,
            );
        }

        if rest.len() < 2 {
            return (true).into();
        }

        let ch1_val = rest[0];
        if !ch1_val.is_char() {
            return call.wrong_argument_violation(
                "char-ci<?",
                "expected a character",
                Some(ch1_val),
                Some(1),
                rest.len(),
                rest,
            );
        }

        let cm = icu::casemap::CaseMapperBorrowed::new();

        let mut ch1 = cm.simple_fold(ch1_val.char());

        for (i, val) in rest.iter().enumerate().skip(1) {
            if !val.is_char() {
                return call.wrong_argument_violation(
                    "char-ci<?",
                    "expected a character",
                    Some(*val),
                    Some(i + 1),
                    rest.len(),
                    rest,
                );
            }
            let ch2 = cm.simple_fold(val.char());
            if ch1 >= ch2 {
                return (false).into();
            }
            ch1 = ch2;
        }
        (true).into()
    }

    #[scheme(name = "char>?")]
    pub fn char_gt(rest: &'gc [Value<'gc>]) -> Value<'gc> {
        if rest.is_empty() {
            return call.wrong_argument_violation(
                "char>?",
                "at least 1 argument required",
                None,
                None,
                0,
                rest,
            );
        }

        if rest.len() < 2 {
            return (true).into();
        }

        let ch1_val = rest[0];
        if !ch1_val.is_char() {
            return call.wrong_argument_violation(
                "char>?",
                "expected a character",
                Some(ch1_val),
                Some(1),
                rest.len(),
                rest,
            );
        }

        let mut ch1 = ch1_val.char();

        for (i, val) in rest.iter().enumerate().skip(1) {
            if !val.is_char() {
                return call.wrong_argument_violation(
                    "char>?",
                    "expected a character",
                    Some(*val),
                    Some(i + 1),
                    rest.len(),
                    rest,
                );
            }
            let ch2 = val.char();
            if ch1 <= ch2 {
                return (false).into();
            }
            ch1 = ch2;
        }
        (true).into()
    }

    #[scheme(name = "char-ci>?")]
    pub fn char_ci_gt(rest: &'gc [Value<'gc>]) -> Value<'gc> {
        if rest.is_empty() {
            return call.wrong_argument_violation(
                "char-ci>?",
                "at least 1 argument required",
                None,
                None,
                0,
                rest,
            );
        }

        if rest.len() < 2 {
            return (true).into();
        }

        let ch1_val = rest[0];
        if !ch1_val.is_char() {
            return call.wrong_argument_violation(
                "char-ci>?",
                "expected a character",
                Some(ch1_val),
                Some(1),
                rest.len(),
                rest,
            );
        }

        let cm = icu::casemap::CaseMapperBorrowed::new();

        let mut ch1 = cm.simple_fold(ch1_val.char());

        for (i, val) in rest.iter().enumerate().skip(1) {
            if !val.is_char() {
                return call.wrong_argument_violation(
                    "char-ci>?",
                    "expected a character",
                    Some(*val),
                    Some(i + 1),
                    rest.len(),
                    rest,
                );
            }
            let ch2 = cm.simple_fold(val.char());
            if ch1 <= ch2 {
                return (false).into();
            }
            ch1 = ch2;
        }
        (true).into()
    }

    #[scheme(name = "char<=?")]
    pub fn char_le(rest: &'gc [Value<'gc>]) -> Value<'gc> {
        if rest.is_empty() {
            return call.wrong_argument_violation(
                "char<=?",
                "at least 1 argument required",
                None,
                None,
                0,
                rest,
            );
        }

        if rest.len() < 2 {
            return (true).into();
        }

        let ch1_val = rest[0];
        if !ch1_val.is_char() {
            return call.wrong_argument_violation(
                "char<=?",
                "expected a character",
                Some(ch1_val),
                Some(1),
                rest.len(),
                rest,
            );
        }
        let mut ch1 = ch1_val.char();

        for (i, val) in rest.iter().enumerate().skip(1) {
            if !val.is_char() {
                return call.wrong_argument_violation(
                    "char<=?",
                    "expected a character",
                    Some(*val),
                    Some(i + 1),
                    rest.len(),
                    rest,
                );
            }
            let ch2 = val.char();
            if ch1 > ch2 {
                return (false).into();
            }
            ch1 = ch2;
        }
        (true).into()
    }

    #[scheme(name = "char-ci<=?")]
    pub fn char_ci_le(rest: &'gc [Value<'gc>]) -> Value<'gc> {
        if rest.is_empty() {
            return call.wrong_argument_violation(
                "char-ci<=?",
                "at least 1 argument required",
                None,
                None,
                0,
                rest,
            );
        }

        if rest.len() < 2 {
            return (true).into();
        }

        let ch1_val = rest[0];
        if !ch1_val.is_char() {
            return call.wrong_argument_violation(
                "char-ci<=?",
                "expected a character",
                Some(ch1_val),
                Some(1),
                rest.len(),
                rest,
            );
        }

        let cm = icu::casemap::CaseMapperBorrowed::new();

        let mut ch1 = cm.simple_fold(ch1_val.char());

        for (i, val) in rest.iter().enumerate().skip(1) {
            if !val.is_char() {
                return call.wrong_argument_violation(
                    "char-ci<=?",
                    "expected a character",
                    Some(*val),
                    Some(i + 1),
                    rest.len(),
                    rest,
                );
            }
            let ch2 = cm.simple_fold(val.char());
            if ch1 > ch2 {
                return (false).into();
            }
            ch1 = ch2;
        }
        (true).into()
    }

    #[scheme(name = "char>=?")]
    pub fn char_ge(rest: &'gc [Value<'gc>]) -> Value<'gc> {
        if rest.is_empty() {
            return call.wrong_argument_violation(
                "char>=?",
                "at least 1 argument required",
                None,
                None,
                0,
                rest,
            );
        }

        if rest.len() < 2 {
            return (true).into();
        }

        let ch1_val = rest[0];
        if !ch1_val.is_char() {
            return call.wrong_argument_violation(
                "char>=?",
                "expected a character",
                Some(ch1_val),
                Some(1),
                rest.len(),
                rest,
            );
        }
        let mut ch1 = ch1_val.char();

        for (i, val) in rest.iter().enumerate().skip(1) {
            if !val.is_char() {
                return call.wrong_argument_violation(
                    "char>=?",
                    "expected a character",
                    Some(*val),
                    Some(i + 1),
                    rest.len(),
                    rest,
                );
            }
            let ch2 = val.char();
            if ch1 < ch2 {
                return (false).into();
            }
            ch1 = ch2;
        }
        (true).into()
    }

    #[scheme(name = "char-ci>=?")]
    pub fn char_ci_ge(rest: &'gc [Value<'gc>]) -> Value<'gc> {
        if rest.is_empty() {
            return call.wrong_argument_violation(
                "char-ci>=?",
                "at least 1 argument required",
                None,
                None,
                0,
                rest,
            );
        }

        if rest.len() < 2 {
            return (true).into();
        }

        let ch1_val = rest[0];
        if !ch1_val.is_char() {
            return call.wrong_argument_violation(
                "char-ci>=?",
                "expected a character",
                Some(ch1_val),
                Some(1),
                rest.len(),
                rest,
            );
        }
        let cm = icu::casemap::CaseMapperBorrowed::new();
        let mut ch1 = cm.simple_fold(ch1_val.char());

        for (i, val) in rest.iter().enumerate().skip(1) {
            if !val.is_char() {
                return call.wrong_argument_violation(
                    "char-ci>=?",
                    "expected a character",
                    Some(*val),
                    Some(i + 1),
                    rest.len(),
                    rest,
                );
            }
            let ch2 = cm.simple_fold(val.char());
            if ch1 < ch2 {
                return (false).into();
            }
            ch1 = ch2;
        }
        (true).into()
    }

    #[scheme(name = "char-title-case?")]
    pub fn char_title_case(ch: char) -> bool {
        let category = get_general_category(ch);
        category == GeneralCategory::TitlecaseLetter
    }

    #[scheme(name = "char-lower-case?")]
    pub fn char_lower_case(ch: char) -> bool {
        ch.is_lowercase()
    }

    #[scheme(name = "char-upper-case?")]
    pub fn char_upper_case(ch: char) -> bool {
        ch.is_uppercase()
    }

    #[scheme(name = "char-whitespace?")]
    pub fn char_whitespace(ch: char) -> bool {
        ch.is_whitespace()
    }

    #[scheme(name = "char-numeric?")]
    pub fn char_numericp(ch: char) -> bool {
        ch.is_numeric()
    }

    #[scheme(name = "char-alphabetic?")]
    pub fn char_alphabeticp(ch: char) -> bool {
        ch.is_alphabetic()
    }

    #[scheme(name = "string-downcase")]
    pub fn string_downcase(str: Gc<'gc, Str<'gc>>) -> Gc<'gc, Str<'gc>> {
        let cm = icu::casemap::CaseMapperBorrowed::new();
        let str = str.to_string();
        let lowercased = cm.lowercase_to_string(&str, &icu::locale::langid!("und"));
        let s = Str::new(ctx, &lowercased, false);
        s
    }

    #[scheme(name = "string-foldcase")]
    pub fn string_foldcase(str: Gc<'gc, Str<'gc>>) -> Gc<'gc, Str<'gc>> {
        let cm = icu::casemap::CaseMapperBorrowed::new();
        let str = str.to_string();
        let folded = cm.fold_string(&str);
        let s = Str::new(ctx, &folded, false);
        s
    }

    #[scheme(name = "string-upcase")]
    pub fn string_upcase(str: Gc<'gc, Str<'gc>>) -> Gc<'gc, Str<'gc>> {
        let str = str.to_string();
        let cm = icu::casemap::CaseMapperBorrowed::new();
        let uppercased = cm.uppercase_to_string(&str, &icu::locale::langid!("und"));
        let s = Str::new(ctx, &uppercased, false);
        s
    }

    #[scheme(name = "string-titlecase")]
    pub fn string_titlecase(str: Gc<'gc, Str<'gc>>) -> Gc<'gc, Str<'gc>> {
        let mut result = String::new();
        let mut prev_was_delimiter = true;

        for ch in str.to_string().chars() {
            if prev_was_delimiter {
                result.extend(ch.to_uppercase());
            } else {
                result.extend(ch.to_lowercase());
            }
            prev_was_delimiter =
                ch.is_whitespace() || get_general_category(ch) == GeneralCategory::DashPunctuation;
        }

        let s = Str::new(ctx, &result, false);
        s
    }

    #[scheme(name = "char-upcase")]
    pub fn char_upcase(ch: char) -> char {
        let cm = icu::casemap::CaseMapperBorrowed::new();
        let uppercased = cm.simple_uppercase(ch);
        uppercased
    }

    #[scheme(name = "char-downcase")]
    pub fn char_downcase(ch: char) -> char {
        let cm = icu::casemap::CaseMapperBorrowed::new();
        let lowercased = cm.simple_lowercase(ch);
        lowercased
    }

    #[scheme(name = "char-foldcase")]
    pub fn char_foldcase(ch: char) -> char {
        let cm = icu::casemap::CaseMapperBorrowed::new();
        let folded = cm.simple_fold(ch);
        folded
    }

    #[scheme(name = "char-titlecase")]
    pub fn char_titlecase(ch: char) -> char {
        let cm = icu::casemap::CaseMapperBorrowed::new();
        let titlecased = cm.simple_titlecase(ch);
        titlecased
    }

    #[scheme(name = "string-normalize-nfc")]
    pub fn string_normalize_nfc(str: Gc<'gc, Str<'gc>>) -> Gc<'gc, Str<'gc>> {
        use unicode_normalization::UnicodeNormalization;

        let normalized: String = str.to_string().nfc().collect();
        let s = Str::new(ctx, &normalized, false);
        s
    }

    #[scheme(name = "string-normalize-nfd")]
    pub fn string_normalize_nfd(str: Gc<'gc, Str<'gc>>) -> Gc<'gc, Str<'gc>> {
        use unicode_normalization::UnicodeNormalization;

        let normalized: String = str.to_string().nfd().collect();
        let s = Str::new(ctx, &normalized, false);
        s
    }

    #[scheme(name = "string-normalize-nfkc")]
    pub fn string_normalize_nfkc(str: Gc<'gc, Str<'gc>>) -> Gc<'gc, Str<'gc>> {
        use unicode_normalization::UnicodeNormalization;

        let normalized: String = str.to_string().nfkc().collect();
        let s = Str::new(ctx, &normalized, false);
        s
    }

    #[scheme(name = "string-normalize-nfkd")]
    pub fn string_normalize_nfkd(str: Gc<'gc, Str<'gc>>) -> Gc<'gc, Str<'gc>> {
        use unicode_normalization::UnicodeNormalization;

        let normalized: String = str.to_string().nfkd().collect();
        let s = Str::new(ctx, &normalized, false);
        s
    }

    #[scheme(name = "keyword?")]
    pub fn keywordp(val: Value<'gc>) -> bool {
        val.is::<Keyword>()
    }

    #[scheme(name = "keyword=?")]
    pub fn keyword_equal(kw1: Gc<'gc, Keyword<'gc>>, kw2: Gc<'gc, Keyword<'gc>>) -> bool {
        Gc::ptr_eq(kw1, kw2)
    }

    #[scheme(name = "symbol->keyword")]
    pub fn symbol_to_keyword(sym: Gc<'gc, Symbol<'gc>>) -> Gc<'gc, Keyword<'gc>> {
        let globals = ctx.globals().keyword_map.get().downcast::<HashTable>();

        if let Some(kw) = globals.get(ctx, sym) {
            return kw.downcast::<Keyword>();
        }

        let kw = Keyword::from_symbol(ctx, sym);
        globals.put(ctx, sym, kw);
        kw
    }

    #[scheme(name = "keyword->symbol")]
    pub fn keyword_to_symbol(kw: Gc<'gc, Keyword<'gc>>) -> Gc<'gc, Symbol<'gc>> {
        kw.symbol
    }

    #[scheme(name = "keyword->string")]
    pub fn keyword_to_string(kw: Gc<'gc, Keyword<'gc>>) -> Gc<'gc, Str<'gc>> {
        let sym = kw.symbol;
        let s = sym.to_str(ctx);
        s
    }
}

pub fn init_strings<'gc>(ctx: MutationContext<'gc>) {
    let _ = &*LOCALE;
    let _ = string_ops::register(ctx);
}
