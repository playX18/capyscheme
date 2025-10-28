use rsgc::Gc;

use crate::native_fn;
use crate::runtime::prelude::*;
use std::cmp::Ordering;
use std::sync::LazyLock;
use unicode_general_category::GeneralCategory;
use unicode_general_category::get_general_category;
use unicode_normalization::*;

pub static LOCALE: LazyLock<icu::locale::Locale> = LazyLock::new(|| {
    let locale = sys_locale::get_locale().unwrap_or_else(|| "en-US".to_string());
    locale
        .parse()
        .unwrap_or_else(|_| icu::locale::Locale::try_from_str("en-US").unwrap())
});

native_fn!(
    register_str_fns:

    pub ("string=?") fn string_equal<'gc>(nctx, rest: &'gc [Value<'gc>]) -> bool {
        if rest.len() < 2 {
            return nctx.return_(true);
        }

        let s1 = rest[0];
        if !s1.is::<Str>() {
            return nctx.wrong_argument_violation("string=?", "expected a string", Some(s1), Some(1), rest.len(), rest);
        }
        let s1 = s1.downcast::<Str>();
        for (i, val) in rest.iter().enumerate().skip(1) {
            if !val.is::<Str>() {
                return nctx.wrong_argument_violation("string=?", "expected a string", Some(*val), Some(i + 1), rest.len(), rest);
            }
            let s2 = val.downcast::<Str>();
            if s1 != s2 {
                return nctx.return_(false);
            }
        }
        nctx.return_(true)
    }

    pub ("string-ci=?") fn string_ci_equal<'gc>(nctx, rest: &'gc [Value<'gc>]) -> bool {
        if rest.len() < 2 {
            return nctx.return_(true);
        }

        let s1 = rest[0];
        let s2 = rest[1];
        if !s1.is::<Str>() {
            return nctx.wrong_argument_violation("string-ci=?", "expected a string", Some(s1), Some(1), rest.len(), rest);
        }

        if !s2.is::<Str>() {
            return nctx.wrong_argument_violation("string-ci=?", "expected a string", Some(s2), Some(2), rest.len(), rest);
        }

        let mut s1 = s1.downcast::<Str>();
        let mut s2 = s2.downcast::<Str>();

        for (i, r) in rest.iter().skip(2).copied().enumerate() {
            let cmp = Str::compare(&s1, &s2, true, 0, 0, s1.len(), s2.len());
            if !matches!(cmp, Some(Ordering::Equal)) {
                return nctx.return_(false);
            }
            if !r.is::<Str>() {
                return nctx.wrong_argument_violation("string-ci=?", "expected a string", Some(r), Some(i + 2), rest.len(), rest);
            }
            s1 = s2;
            s2 = r.downcast::<Str>();
        }

        let cmp = Str::compare(&s1, &s2, true, 0, 0, s1.len(), s2.len());

        nctx.return_(matches!(cmp, Some(Ordering::Equal)))
    }

    pub ("string>?") fn string_gt<'gc>(nctx, rest: &'gc [Value<'gc>]) -> bool {
        if rest.len() == 0 {
            return nctx.wrong_argument_violation("string>?", "at least 1 argument required", None, None, 0, rest);
        }

        if rest.len() < 2 {
            return nctx.return_(true);
        }

        let s1 = rest[0];
        if !s1.is::<Str>() {
            return nctx.wrong_argument_violation("string>?", "expected a string", Some(s1), Some(1), rest.len(), rest);
        }
        let mut s1 = s1.downcast::<Str>();

        for (i, val) in rest.iter().enumerate().skip(1) {
            if !val.is::<Str>() {
                return nctx.wrong_argument_violation("string>?", "expected a string", Some(*val), Some(i + 1), rest.len(), rest);
            }
            let s2 = val.downcast::<Str>();
            let cmp = Str::compare(&s1, &s2, false, 0, 0, s1.len(), s2.len());
            if !matches!(cmp, Some(Ordering::Greater)) {
                return nctx.return_(false);
            }
            s1 = s2;
        }

        nctx.return_(true)
    }

    pub ("string-ci>?") fn string_ci_gt<'gc>(nctx, rest: &'gc [Value<'gc>]) -> bool {
        if rest.len() == 0 {
            return nctx.wrong_argument_violation("string-ci>?", "at least 1 argument required", None, None, 0, rest);
        }

        if rest.len() < 2 {
            return nctx.return_(true);
        }

        let s1 = rest[0];
        if !s1.is::<Str>() {
            return nctx.wrong_argument_violation("string-ci>?", "expected a string", Some(s1), Some(1), rest.len(), rest);
        }
        let mut s1 = s1.downcast::<Str>();

        for (i, val) in rest.iter().enumerate().skip(1) {
            if !val.is::<Str>() {
                return nctx.wrong_argument_violation("string-ci>?", "expected a string", Some(*val), Some(i + 1), rest.len(), rest);
            }
            let s2 = val.downcast::<Str>();
            let cmp = Str::compare(&s1, &s2, true, 0, 0, s1.len(), s2.len());
            if !matches!(cmp, Some(Ordering::Greater)) {
                return nctx.return_(false);
            }
            s1 = s2;
        }

        nctx.return_(true)
    }

    pub ("string>=?") fn string_ge<'gc>(nctx, rest: &'gc [Value<'gc>]) -> bool {
        if rest.len() == 0 {
            return nctx.wrong_argument_violation("string>=?", "at least 1 argument required", None, None, 0, rest);
        }

        if rest.len() < 2 {
            return nctx.return_(true);
        }

        let s1 = rest[0];
        if !s1.is::<Str>() {
            return nctx.wrong_argument_violation("string>=?", "expected a string", Some(s1), Some(1), rest.len(), rest);
        }
        let mut s1 = s1.downcast::<Str>();

        for (i, val) in rest.iter().enumerate().skip(1) {
            if !val.is::<Str>() {
                return nctx.wrong_argument_violation("string>=?", "expected a string", Some(*val), Some(i + 1), rest.len(), rest);
            }
            let s2 = val.downcast::<Str>();
            let cmp = Str::compare(&s1, &s2, false, 0, 0, s1.len(), s2.len());
            if !matches!(cmp, Some(Ordering::Greater) | Some(Ordering::Equal)) {
                return nctx.return_(false);
            }
            s1 = s2;
        }

        nctx.return_(true)
    }

    pub ("string-ci>=?") fn string_ci_ge<'gc>(nctx, rest: &'gc [Value<'gc>]) -> bool {
        if rest.len() == 0 {
            return nctx.wrong_argument_violation("string-ci>=?", "at least 1 argument required", None, None, 0, rest);
        }

        if rest.len() < 2 {
            return nctx.return_(true);
        }

        let s1 = rest[0];
        if !s1.is::<Str>() {
            return nctx.wrong_argument_violation("string-ci>=?", "expected a string", Some(s1), Some(1), rest.len(), rest);
        }
        let mut s1 = s1.downcast::<Str>();

        for (i, val) in rest.iter().enumerate().skip(1) {
            if !val.is::<Str>() {
                return nctx.wrong_argument_violation("string-ci>=?", "expected a string", Some(*val), Some(i + 1), rest.len(), rest);
            }
            let s2 = val.downcast::<Str>();
            let cmp = Str::compare(&s1, &s2, true, 0, 0, s1.len(), s2.len());
            if !matches!(cmp, Some(Ordering::Greater) | Some(Ordering::Equal)) {
                return nctx.return_(false);
            }
            s1 = s2;
        }

        nctx.return_(true)
    }

    pub ("string<?") fn string_lt<'gc>(nctx, rest: &'gc [Value<'gc>]) -> bool {
        if rest.len() == 0 {
            return nctx.wrong_argument_violation("string<?", "at least 1 argument required", None, None, 0, rest);
        }

        if rest.len() < 2 {
            return nctx.return_(true);
        }

        let s1 = rest[0];
        if !s1.is::<Str>() {
            return nctx.wrong_argument_violation("string<?", "expected a string", Some(s1), Some(1), rest.len(), rest);
        }
        let mut s1 = s1.downcast::<Str>();

        for (i, val) in rest.iter().enumerate().skip(1) {
            if !val.is::<Str>() {
                return nctx.wrong_argument_violation("string<?", "expected a string", Some(*val), Some(i + 1), rest.len(), rest);
            }
            let s2 = val.downcast::<Str>();
            let cmp = Str::compare(&s1, &s2, false, 0, 0, s1.len(), s2.len());
            if !matches!(cmp, Some(Ordering::Less)) {
                return nctx.return_(false);
            }
            s1 = s2;
        }

        nctx.return_(true)
    }

    pub ("string-ci<?") fn string_ci_lt<'gc>(nctx, rest: &'gc [Value<'gc>]) -> bool {
        if rest.len() == 0 {
            return nctx.wrong_argument_violation("string-ci<?", "at least 1 argument required", None, None, 0, rest);
        }

        if rest.len() < 2 {
            return nctx.return_(true);
        }

        let s1 = rest[0];
        if !s1.is::<Str>() {
            return nctx.wrong_argument_violation("string-ci<?", "expected a string", Some(s1), Some(1), rest.len(), rest);
        }
        let mut s1 = s1.downcast::<Str>();

        for (i, val) in rest.iter().enumerate().skip(1) {
            if !val.is::<Str>() {
                return nctx.wrong_argument_violation("string-ci<?", "expected a string", Some(*val), Some(i + 1), rest.len(), rest);
            }
            let s2 = val.downcast::<Str>();
            let cmp = Str::compare(&s1, &s2, true, 0, 0, s1.len(), s2.len());
            if !matches!(cmp, Some(Ordering::Less)) {
                return nctx.return_(false);
            }
            s1 = s2;
        }

        nctx.return_(true)
    }

    pub ("string<=?") fn string_le<'gc>(nctx, rest: &'gc [Value<'gc>]) -> bool {
        if rest.len() == 0 {
            return nctx.wrong_argument_violation("string<=?", "at least 1 argument required", None, None, 0, rest);
        }

        if rest.len() < 2 {
            return nctx.return_(true);
        }

        let s1 = rest[0];
        if !s1.is::<Str>() {
            return nctx.wrong_argument_violation("string<=?", "expected a string", Some(s1), Some(1), rest.len(), rest);
        }
        let mut s1 = s1.downcast::<Str>();

        for (i, val) in rest.iter().enumerate().skip(1) {
            if !val.is::<Str>() {
                return nctx.wrong_argument_violation("string<=?", "expected a string", Some(*val), Some(i + 1), rest.len(), rest);
            }
            let s2 = val.downcast::<Str>();
            let cmp = Str::compare(&s1, &s2, false, 0, 0, s1.len(), s2.len());
            if !matches!(cmp, Some(Ordering::Less) | Some(Ordering::Equal)) {
                return nctx.return_(false);
            }
            s1 = s2;
        }

        nctx.return_(true)
    }

    pub ("string-ci<=?") fn string_ci_le<'gc>(nctx, rest: &'gc [Value<'gc>]) -> bool {
        if rest.len() == 0 {
            return nctx.wrong_argument_violation("string-ci<=?", "at least 1 argument required", None, None, 0, rest);
        }

        if rest.len() < 2 {
            return nctx.return_(true);
        }

        let s1 = rest[0];
        if !s1.is::<Str>() {
            return nctx.wrong_argument_violation("string-ci<=?", "expected a string", Some(s1), Some(1), rest.len(), rest);
        }
        let mut s1 = s1.downcast::<Str>();

        for (i, val) in rest.iter().enumerate().skip(1) {
            if !val.is::<Str>() {
                return nctx.wrong_argument_violation("string-ci<=?", "expected a string", Some(*val), Some(i + 1), rest.len(), rest);
            }
            let s2 = val.downcast::<Str>();
            let cmp = Str::compare(&s1, &s2, true, 0, 0, s1.len(), s2.len());
            if !matches!(cmp, Some(Ordering::Less) | Some(Ordering::Equal)) {
                return nctx.return_(false);
            }
            s1 = s2;
        }

        nctx.return_(true)
    }

    pub ("string-length") fn string_length<'gc>(nctx, str: Gc<'gc, Str<'gc>>) -> usize {
        let len = str.len();
        nctx.return_(len)
    }

    pub ("string-append") fn string_append<'gc>(nctx, strs: &'gc [Value<'gc>]) -> Result<Gc<'gc, Str<'gc>>, Value<'gc>> {
        if strs.len() == 0 {
            let s = Str::new(&nctx.ctx, "", false);
            return nctx.return_(Ok(s));
        }

        if strs.len() == 1 {
            if !strs[0].is::<Str>() {
                return nctx.wrong_argument_violation("string-append", "expected a string", Some(strs[0]), Some(1), 1, &[strs[0]]);
            }

            return nctx.return_(Ok(strs[0].downcast::<Str>()));
        }


        let mut buffer = String::new();

        for (i, str) in strs.iter().enumerate() {
            if !str.is::<Str>() {
                return nctx.wrong_argument_violation("string-append", "expected a string", Some(*str), Some(i), strs.len(), &strs);
            }

            buffer.push_str(&str.downcast::<Str>().to_string());
        }
        let s = Ok(Str::new(&nctx.ctx, &buffer, false));
        nctx.return_(s)
    }


    pub ("symbol-append") fn symbol_append<'gc>(nctx, syms: &'gc [Value<'gc>]) -> Result<Gc<'gc, Symbol<'gc>>, Value<'gc>> {
        if syms.len() == 0 {
            let s = Symbol::from_str(nctx.ctx, "");
            return nctx.return_(Ok(s));
        }

        if syms.len() == 1 {
            if !syms[0].is::<Symbol>() {
                return nctx.wrong_argument_violation("symbol-append", "expected a symbol", Some(syms[0]), Some(1), 1, &[syms[0]]);
            }

            return nctx.return_(Ok(syms[0].downcast::<Symbol>()));
        }


        let mut buffer = String::new();

        for (i, sym) in syms.iter().enumerate() {
            if !sym.is::<Symbol>() {
                return nctx.wrong_argument_violation("symbol-append", "expected a symbol", Some(*sym), Some(i), syms.len(), &syms);
            }

            buffer.push_str(&sym.downcast::<Symbol>().to_string());
        }
        let s = Ok(Symbol::from_str(nctx.ctx, &buffer));
        nctx.return_(s)
    }

    pub ("symbol->string") fn symbol_to_string<'gc>(nctx, sym: Gc<'gc, Symbol<'gc>>) -> Gc<'gc, Str<'gc>> {
        let s = sym.to_str(&nctx.ctx);

        nctx.return_(s)
    }

    pub ("string->symbol") fn string_to_symbol<'gc>(nctx, str: Gc<'gc, Str<'gc>>) -> Gc<'gc, Symbol<'gc>> {
        let sym = Symbol::from_string(nctx.ctx, str);
        nctx.return_(sym)
    }

    pub ("string->uninterned-symbol") fn string_to_uninterned_symbol<'gc>(nctx, str: Gc<'gc, Str<'gc>>) -> Gc<'gc, Symbol<'gc>> {
        let sym = Symbol::from_string_uninterned(&nctx.ctx, str, None);
        nctx.return_(sym)
    }

    pub ("uninterned-symbol?") fn is_uninterned_symbol<'gc>(nctx, sym: Gc<'gc, Symbol<'gc>>) -> bool {
        nctx.return_(sym.is_uninterned())
    }

    pub ("string-fill!") fn string_fill_destructive<'gc>(
        nctx,
        str: Gc<'gc, Str<'gc>>,
        ch: char,
        start: Option<usize>,
        end: Option<usize>
    ) -> Value<'gc> {
        let start = start.unwrap_or(0);
        let end = end.unwrap_or(str.len());

        if start > end {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation(
                "string-fill!",
                "start greater than end",
                None,
                None,
                3,
                &[str.into(), start.into_value(ctx), end.into_value(ctx)]
            );
        }

        if end > str.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation(
                "string-fill!",
                "end out of bounds",
                None,
                None,
                3,
                &[str.into(), end.into_value(ctx)]
            );
        }

        for i in start..end {
            Str::set(str, &nctx.ctx, i, ch);
        }

        nctx.return_(Value::undefined())
    }

    /// prints all arguments using their `Display` implementation, followed by a newline.
    pub (":print") fn print<'gc>(nctx, args: &'gc [Value<'gc>]) -> () {
        for arg in args.iter() {
            print!("{}", arg);
        }
        println!();
        nctx.return_(())
    }

    pub ("substring") fn substring<'gc>(nctx, str: Gc<'gc, Str<'gc>>, start: usize, end: Option<usize>) -> Result<Gc<'gc, Str<'gc>>, Value<'gc>> {
        let end = end.unwrap_or(str.len());
        if start > end || end > str.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation(
                "substring",
                "invalid start or end",
                None,
                None,
                3,
                &[str.into(), start.into_value(ctx), end.into_value(ctx)]
            );
        }

        let s = Str::substring(str, &nctx.ctx, start, end);

        nctx.return_(Ok(s))
    }

    pub ("string?") fn is_string<'gc>(nctx, val: Value<'gc>) -> bool {
        nctx.return_(val.is::<Str>())
    }

    pub ("symbol?") fn is_symbol<'gc>(nctx, val: Value<'gc>) -> bool {
        nctx.return_(val.is::<Symbol>())
    }

    pub ("string-prefix?") fn string_prefix<'gc>(nctx, prefix: Gc<'gc, Str<'gc>>, str: Gc<'gc, Str<'gc>>) -> bool {
        nctx.return_(str.to_string().starts_with(&prefix.to_string()))
    }

    pub ("string-copy") fn string_copy<'gc>(
        nctx,
        s1: Gc<'gc, Str<'gc>>
    ) -> Gc<'gc, Str<'gc>> {
        let s2 = Str::substring_copy(s1, &nctx.ctx, 0, s1.len());
        nctx.return_(s2)
    }

    pub ("string-contains") fn string_contains<'gc>(
        nctx,
        s1: Gc<'gc, Str<'gc>>,
        s2: Gc<'gc, Str<'gc>>,
        start1: Option<usize>,
        end1: Option<usize>,
        start2: Option<usize>,
        end2: Option<usize>
    ) -> Value<'gc> {
        let ctx = nctx.ctx;
        let mut cstart1 = if let Some(start1) = start1 {
            if start1 >= s1.len() {
                let ctx = nctx.ctx;
                return nctx.wrong_argument_violation(
                    "string-contains",
                    "start1 out of bounds",
                    None,
                    None,
                    3,
                    &[s1.into(), s2.into(), start1.into_value(ctx)]
                );
            }
            start1
        } else {
            0
        };

        let cend1 = if let Some(end1) = end1 {
            if end1 > s1.len() {
                let ctx = nctx.ctx;
                return nctx.wrong_argument_violation(
                    "string-contains",
                    "end1 out of bounds",
                    None,
                    None,
                    4,
                    &[s1.into(), s2.into(), end1.into_value(ctx)]
                );
            }
            end1
        } else {
            s1.len()
        };

        if cstart1 > cend1 {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation(
                "string-contains",
                "start1 greater than end1",
                None,
                None,
                3,
                &[s1.into(), s2.into(), cstart1.into_value(ctx), cend1.into_value(ctx)]
            );
        }

        let cstart2 = if let Some(start2) = start2 {
            if start2 >= s2.len() {
                let ctx = nctx.ctx;
                return nctx.wrong_argument_violation(
                    "string-contains",
                    "start2 out of bounds",
                    None,
                    None,
                    5,
                    &[s1.into(), s2.into(), start2.into_value(ctx)]
                );
            }
            start2
        } else {
            0
        };

        let cend2 = if let Some(end2) = end2 {
            if end2 > s2.len() {
                let ctx = nctx.ctx;
                return nctx.wrong_argument_violation(
                    "string-contains",
                    "end2 out of bounds",
                    None,
                    None,
                    6,
                    &[s1.into(), s2.into(), end2.into_value(ctx)]
                );
            }
            end2
        } else {
            s2.len()
        };

        let len2 = cend2 - cstart2;

        if cend1 - cstart1 >= len2
        {
            while cstart1 <= cend1 - len2 {
                let mut i = cstart1;
                let mut j = cstart2;

                while i < cend1
                    && j < cend2
                    && s1.get(i) == s2.get(j)
                {
                    i += 1;
                    j += 1;
                }

                if j == cend2 {
                    return nctx.return_(cstart1.into_value(ctx));
                }
                cstart1 += 1;
            }
        }

        nctx.return_(Value::new(false))
    }

    pub ("string-contains-ci") fn string_contains_ci<'gc>(
        nctx,
        s1: Gc<'gc, Str<'gc>>,
        s2: Gc<'gc, Str<'gc>>,
        start1: Option<usize>,
        end1: Option<usize>,
        start2: Option<usize>,
        end2: Option<usize>
    ) -> Value<'gc> {
        let ctx = nctx.ctx;
        let mut cstart1 = if let Some(start1) = start1 {
            if start1 >= s1.len() {
                let ctx = nctx.ctx;
                return nctx.wrong_argument_violation(
                    "string-contains-ci",
                    "start1 out of bounds",
                    None,
                    None,
                    3,
                    &[s1.into(), s2.into(), start1.into_value(ctx)]
                );
            }
            start1
        } else {
            0
        };

        let cend1 = if let Some(end1) = end1 {
            if end1 > s1.len() {
                let ctx = nctx.ctx;
                return nctx.wrong_argument_violation(
                    "string-contains-ci",
                    "end1 out of bounds",
                    None,
                    None,
                    4,
                    &[s1.into(), s2.into(), end1.into_value(ctx)]
                );
            }
            end1
        } else {
            s1.len()
        };

        if cstart1 > cend1 {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation(
                "string-contains-ci",
                "start1 greater than end1",
                None,
                None,
                3,
                &[s1.into(), s2.into(), cstart1.into_value(ctx), cend1.into_value(ctx)]
            );
        }

        let cstart2 = if let Some(start2) = start2 {
            if start2 >= s2.len() {
                let ctx = nctx.ctx;
                return nctx.wrong_argument_violation(
                    "string-contains-ci",
                    "start2 out of bounds",
                    None,
                    None,
                    5,
                    &[s1.into(), s2.into(), start2.into_value(ctx)]
                );
            }
            start2
        } else {
            0
        };

        let cend2 = if let Some(end2) = end2 {
            if end2 > s2.len() {
                let ctx = nctx.ctx;
                return nctx.wrong_argument_violation(
                    "string-contains-ci",
                    "end2 out of bounds",
                    None,
                    None,
                    6,
                    &[s1.into(), s2.into(), end2.into_value(ctx)]
                );
            }
            end2
        } else {
            s2.len()
        };

        let len2 = cend2 - cstart2;

        if cend1 - cstart1 >= len2
        {
            while cstart1 <= cend1 - len2 {
                let mut i = cstart1;
                let mut j = cstart2;

                while i < cend1
                    && j < cend2
                    && s1.get(i).unwrap().to_lowercase().eq(s2.get(j).unwrap().to_lowercase())
                {
                    i += 1;
                    j += 1;
                }

                if j == cend2 {
                    return nctx.return_(cstart1.into_value(ctx));
                }
                cstart1 += 1;
            }
        }

        nctx.return_(Value::new(false))
    }

    pub ("string-null?") fn string_null<'gc>(nctx, str: Gc<'gc, Str<'gc>>) -> bool {
        nctx.return_(str.len() == 0)
    }

    pub ("string-split") fn string_split<'gc>(nctx, str: Gc<'gc, Str<'gc>>, ch: char) -> Value<'gc> {
        let parts = str.to_string().split(ch).rev().fold(Value::null(), |acc, part| {
            Value::cons(
                nctx.ctx,
                Str::new(&nctx.ctx, part, false).into(),
                acc
            )
        });
        nctx.return_(parts)
    }

    pub ("string") fn string<'gc>(nctx, chars: &'gc [Value<'gc>]) -> Result<Gc<'gc, Str<'gc>>, Value<'gc>> {
        let mut buffer = String::with_capacity(chars.len());

        for (i, ch_val) in chars.iter().enumerate() {
            if !ch_val.is_char() {
                return nctx.wrong_argument_violation("string", "expected a character", Some(*ch_val), Some(i + 1), chars.len(), chars);
            }
            let ch = ch_val.char();
            buffer.push(ch);
        }

        let s = Str::new(&nctx.ctx, &buffer, false);
        nctx.return_(Ok(s))
    }

    pub ("make-string") fn make_string<'gc>(nctx, length: usize, fill_char: Option<char>) -> Gc<'gc, Str<'gc>> {
        let ch = fill_char.unwrap_or('\0');
        let s = Str::from_char(&nctx.ctx, ch, length);
        nctx.return_(s)
    }

    pub ("string-ref") fn string_ref<'gc>(nctx, str: Gc<'gc, Str<'gc>>, index: usize) -> Result<char, Value<'gc>> {
        if index >= str.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation(
                "string-ref",
                "index out of bounds",
                None,
                None,
                2,
                &[str.into(), index.into_value(ctx)]
            );
        }

        let ch = str.get(index).unwrap(); // safe due to the bounds check above

        nctx.return_(Ok(ch))
    }

    pub ("string-set!") fn string_set<'gc>(nctx, str: Gc<'gc, Str<'gc>>, index: usize, ch: char) -> Result<(), Value<'gc>> {
        if index >= str.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation(
                "string-set!",
                "index out of bounds",
                None,
                None,
                3,
                &[str.into(), index.into_value(ctx), ch.into_value(ctx)]
            );
        }

        Str::set(str, &nctx.ctx, index, ch);

        nctx.return_(Ok(()))
    }

    pub ("utf8->string") fn utf8_to_string<'gc>(nctx, bytes: Gc<'gc, ByteVector>) -> Result<Gc<'gc, Str<'gc>>, Value<'gc>> {
        match std::str::from_utf8(&bytes.as_slice()[..]) {
            Ok(s) => {
                let str = Str::new(&nctx.ctx, s, false);
                nctx.return_(Ok(str))
            },
            Err(_) => {

                nctx.wrong_argument_violation(
                    "utf8->string",
                    "invalid UTF-8 bytevector",
                    None,
                    None,
                    1,
                    &[bytes.into()]
                )
            }
        }
    }

    pub ("string->utf8") fn string_to_utf8<'gc>(nctx, str: Gc<'gc, Str<'gc>>) -> Gc<'gc, ByteVector> {
        let bytes = str.to_string().into_bytes();
        let bytevec = ByteVector::from_slice(&nctx.ctx, &bytes, true);
        nctx.return_(bytevec)
    }

    pub ("string->utf8/nul") fn string_to_utf8_nul<'gc>(nctx, str: Gc<'gc, Str<'gc>>) -> Gc<'gc, ByteVector> {
        let mut bytes = str.to_string().into_bytes();
        bytes.push(0); // NUL terminator
        let bytevec = ByteVector::from_slice(&nctx.ctx, &bytes, true);
        nctx.return_(bytevec)
    }

    pub ("utf16->string") fn utf16_to_string<'gc>(nctx, bytes: Gc<'gc, ByteVector>) -> Result<Gc<'gc, Str<'gc>>, Value<'gc>> {
        let utf16_data: Vec<u16> = bytes.as_slice()
            .chunks(2)
            .map(|chunk| {
                if chunk.len() == 2 {
                    u16::from_le_bytes([chunk[0], chunk[1]])
                } else {
                    // Handle odd-length byte vectors by padding with zero
                    u16::from_le_bytes([chunk[0], 0])
                }
            })
            .collect();

        match String::from_utf16(&utf16_data[..]) {
            Ok(s) => {
                let str = Str::new(&nctx.ctx, &s, false);
                nctx.return_(Ok(str))
            },
            Err(_) => {

                nctx.wrong_argument_violation(
                    "utf16->string",
                    "invalid UTF-16 bytevector",
                    None,
                    None,
                    1,
                    &[bytes.into()]
                )
            }
        }
    }

    pub ("string->utf16") fn string_to_utf16<'gc>(nctx, str: Gc<'gc, Str<'gc>>) -> Gc<'gc, ByteVector> {
        let utf16_data: Vec<u16> = str.to_string().encode_utf16().collect();
        let mut bytes = Vec::with_capacity(utf16_data.len() * 2);
        for code_unit in utf16_data {
            bytes.extend_from_slice(&code_unit.to_le_bytes());
        }

        let bytevec = ByteVector::from_slice(&nctx.ctx, &bytes, true);
        nctx.return_(bytevec)
    }

    pub ("string->utf32") fn string_to_utf32<'gc>(nctx, str: Gc<'gc, Str<'gc>>) -> Gc<'gc, ByteVector> {
        let utf32_data: Vec<u32> = str.to_string().chars().map(|ch| ch as u32).collect();
        let mut bytes = Vec::with_capacity(utf32_data.len() * 4);
        for code_point in utf32_data {
            bytes.extend_from_slice(&code_point.to_le_bytes());
        }

        let bytevec = ByteVector::from_slice(&nctx.ctx, &bytes, true);
        nctx.return_(bytevec)
    }

    pub ("utf32->string") fn utf32_to_string<'gc>(nctx, bytes: Gc<'gc, ByteVector>) -> Result<Gc<'gc, Str<'gc>>, Value<'gc>> {
        let utf32_data: Vec<u32> = bytes.as_slice()
            .chunks(4)
            .map(|chunk| {
                if chunk.len() == 4 {
                    u32::from_le_bytes([chunk[0], chunk[1], chunk[2], chunk[3]])
                } else {
                    // Handle incomplete code points by padding with zeros
                    let mut padded = [0u8; 4];
                    for i in 0..chunk.len() {
                        padded[i] = chunk[i];
                    }
                    u32::from_le_bytes(padded)
                }
            })
            .collect();

        let chars_result: Result<String, _> = utf32_data.iter().map(|&cp| {
            std::char::from_u32(cp).ok_or(())
        }).collect::<Result<String, _>>();

        match chars_result {
            Ok(s) => {
                let str = Str::new(&nctx.ctx, &s, false);
                nctx.return_(Ok(str))
            },
            Err(_) => {
                nctx.wrong_argument_violation(
                    "utf32->string",
                    "invalid UTF-32 bytevector",
                    None,
                    None,
                    1,
                    &[bytes.into()]
                )
            }
        }
    }


    pub ("char-general-category") fn char_general_category<'gc>(nctx, ch: char) -> Gc<'gc, Symbol<'gc>> {
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

        let sym = Symbol::from_str(nctx.ctx, category_str);
        nctx.return_(sym)
    }


    pub ("list->string") fn list_to_string<'gc>(nctx, lst: Value<'gc>) -> Result<Gc<'gc, Str<'gc>>, Value<'gc>> {
        if !lst.is_list() {
            return nctx.wrong_argument_violation("list->string", "expected a list", Some(lst), Some(1), 1, &[lst]);
        }
        let mut walk = lst;
        let mut buffer = String::new();

        while walk.is_pair() {
            let ch_val = walk.car();
            if !ch_val.is_char() {
                return nctx.wrong_argument_violation("list->string", "expected a list of characters", Some(ch_val), Some(1), 1, &[lst]);
            }
            let ch = ch_val.char();
            buffer.push(ch);
            walk = walk.cdr();
        }

        if !walk.is_null() {
            return nctx.wrong_argument_violation("list->string", "expected a proper list", Some(walk), Some(1), 1, &[lst]);
        }

        let s = Str::new(&nctx.ctx, &buffer, false);
        nctx.return_(Ok(s))
    }

    pub ("char=?") fn char_equal<'gc>(nctx, rest: &'gc [Value<'gc>]) -> bool {
        if rest.len() < 2 {
            return nctx.return_(true);
        }

        let ch1_val = rest[0];
        if !ch1_val.is_char() {
            return nctx.wrong_argument_violation("char=?", "expected a character", Some(ch1_val), Some(1), rest.len(), rest);
        }
        let ch1 = ch1_val.char();

        for (i, val) in rest.iter().enumerate().skip(1) {
            if !val.is_char() {
                return nctx.wrong_argument_violation("char=?", "expected a character", Some(*val), Some(i + 1), rest.len(), rest);
            }
            let ch2 = val.char();
            if ch1 != ch2 {
                return nctx.return_(false);
            }
        }
        nctx.return_(true)
    }

    pub ("char-ci=?") fn char_ci_equal<'gc>(nctx, rest: &'gc [Value<'gc>]) -> bool {
        if rest.len() < 2 {
            return nctx.return_(true);
        }

        let ch1_val = rest[0];
        if !ch1_val.is_char() {
            return nctx.wrong_argument_violation("char-ci=?", "expected a character", Some(ch1_val), Some(1), rest.len(), rest);
        }
        let cm = icu::casemap::CaseMapperBorrowed::new();

        let ch1 = cm.simple_fold(ch1_val.char());

        for (i, val) in rest.iter().enumerate().skip(1) {
            if !val.is_char() {
                return nctx.wrong_argument_violation("char-ci=?", "expected a character", Some(*val), Some(i + 1), rest.len(), rest);
            }
            let ch2 = cm.simple_fold(val.char());
            if ch1 != ch2 {
                return nctx.return_(false);
            }
        }
        nctx.return_(true)
    }

    pub ("char<?") fn char_lt<'gc>(nctx, rest: &'gc [Value<'gc>]) -> bool {
        if rest.len() == 0 {
            return nctx.wrong_argument_violation("char<?", "at least 1 argument required", None, None, 0, rest);
        }

        if rest.len() < 2 {
            return nctx.return_(true);
        }

        let ch1_val = rest[0];
        if !ch1_val.is_char() {
            return nctx.wrong_argument_violation("char<?", "expected a character", Some(ch1_val), Some(1), rest.len(), rest);
        }


        let mut ch1 = ch1_val.char();

        for (i, val) in rest.iter().enumerate().skip(1) {
            if !val.is_char() {
                return nctx.wrong_argument_violation("char<?", "expected a character", Some(*val), Some(i + 1), rest.len(), rest);
            }
            let ch2 = val.char();
            if ch1 >= ch2 {
                return nctx.return_(false);
            }
            ch1 = ch2;
        }
        nctx.return_(true)
    }

    pub ("char-ci<?") fn char_ci_lt<'gc>(nctx, rest: &'gc [Value<'gc>]) -> bool {
        if rest.len() == 0 {
            return nctx.wrong_argument_violation("char-ci<?", "at least 1 argument required", None, None, 0, rest);
        }

        if rest.len() < 2 {
            return nctx.return_(true);
        }

        let ch1_val = rest[0];
        if !ch1_val.is_char() {
            return nctx.wrong_argument_violation("char-ci<?", "expected a character", Some(ch1_val), Some(1), rest.len(), rest);
        }

        let cm = icu::casemap::CaseMapperBorrowed::new();

        let mut ch1 = cm.simple_fold(ch1_val.char());

        for (i, val) in rest.iter().enumerate().skip(1) {
            if !val.is_char() {
                return nctx.wrong_argument_violation("char-ci<?", "expected a character", Some(*val), Some(i + 1), rest.len(), rest);
            }
            let ch2 = cm.simple_fold(val.char());
            if ch1 >= ch2 {
                return nctx.return_(false);
            }
            ch1 = ch2;
        }
        nctx.return_(true)
    }

    pub ("char>?") fn char_gt<'gc>(nctx, rest: &'gc [Value<'gc>]) -> bool {
        if rest.len() == 0 {
            return nctx.wrong_argument_violation("char>?", "at least 1 argument required", None, None, 0, rest);
        }

        if rest.len() < 2 {
            return nctx.return_(true);
        }

        let ch1_val = rest[0];
        if !ch1_val.is_char() {
            return nctx.wrong_argument_violation("char>?", "expected a character", Some(ch1_val), Some(1), rest.len(), rest);
        }

        let mut ch1 = ch1_val.char();

        for (i, val) in rest.iter().enumerate().skip(1) {
            if !val.is_char() {
                return nctx.wrong_argument_violation("char>?", "expected a character", Some(*val), Some(i + 1), rest.len(), rest);
            }
            let ch2 = val.char();
            if ch1 <= ch2 {
                return nctx.return_(false);
            }
            ch1 = ch2;
        }
        nctx.return_(true)
    }

    pub ("char-ci>?") fn char_ci_gt<'gc>(nctx, rest: &'gc [Value<'gc>]) -> bool {
        if rest.len() == 0 {
            return nctx.wrong_argument_violation("char-ci>?", "at least 1 argument required", None, None, 0, rest);
        }

        if rest.len() < 2 {
            return nctx.return_(true);
        }

        let ch1_val = rest[0];
        if !ch1_val.is_char() {
            return nctx.wrong_argument_violation("char-ci>?", "expected a character", Some(ch1_val), Some(1), rest.len(), rest);
        }

        let cm = icu::casemap::CaseMapperBorrowed::new();

        let mut ch1 = cm.simple_fold(ch1_val.char());

        for (i, val) in rest.iter().enumerate().skip(1) {
            if !val.is_char() {
                return nctx.wrong_argument_violation("char-ci>?", "expected a character", Some(*val), Some(i + 1), rest.len(), rest);
            }
            let ch2 = cm.simple_fold(val.char());
            if ch1 <= ch2 {
                return nctx.return_(false);
            }
            ch1 = ch2;
        }
        nctx.return_(true)
    }

    pub ("char<=?") fn char_le<'gc>(nctx, rest: &'gc [Value<'gc>]) -> bool {
        if rest.len() == 0 {
            return nctx.wrong_argument_violation("char<=?", "at least 1 argument required", None, None, 0, rest);
        }

        if rest.len() < 2 {
            return nctx.return_(true);
        }

        let ch1_val = rest[0];
        if !ch1_val.is_char() {
            return nctx.wrong_argument_violation("char<=?", "expected a character", Some(ch1_val), Some(1), rest.len(), rest);
        }
        let mut ch1 = ch1_val.char();

        for (i, val) in rest.iter().enumerate().skip(1) {
            if !val.is_char() {
                return nctx.wrong_argument_violation("char<=?", "expected a character", Some(*val), Some(i + 1), rest.len(), rest);
            }
            let ch2 = val.char();
            if ch1 > ch2 {
                return nctx.return_(false);
            }
            ch1 = ch2;
        }
        nctx.return_(true)
    }

    pub ("char-ci<=?") fn char_ci_le<'gc>(nctx, rest: &'gc [Value<'gc>]) -> bool {
        if rest.len() == 0 {
            return nctx.wrong_argument_violation("char-ci<=?", "at least 1 argument required", None, None, 0, rest);
        }

        if rest.len() < 2 {
            return nctx.return_(true);
        }

        let ch1_val = rest[0];
        if !ch1_val.is_char() {
            return nctx.wrong_argument_violation("char-ci<=?", "expected a character", Some(ch1_val), Some(1), rest.len(), rest);
        }

        let cm = icu::casemap::CaseMapperBorrowed::new();

        let mut ch1 = cm.simple_fold(ch1_val.char());

        for (i, val) in rest.iter().enumerate().skip(1) {
            if !val.is_char() {
                return nctx.wrong_argument_violation("char-ci<=?", "expected a character", Some(*val), Some(i + 1), rest.len(), rest);
            }
            let ch2 = cm.simple_fold(val.char());
            if ch1 > ch2 {
                return nctx.return_(false);
            }
            ch1 = ch2;
        }
        nctx.return_(true)
    }

    pub ("char>=?") fn char_ge<'gc>(nctx, rest: &'gc [Value<'gc>]) -> bool {
        if rest.len() == 0 {
            return nctx.wrong_argument_violation("char>=?", "at least 1 argument required", None, None, 0, rest);
        }

        if rest.len() < 2 {
            return nctx.return_(true);
        }

        let ch1_val = rest[0];
        if !ch1_val.is_char() {
            return nctx.wrong_argument_violation("char>=?", "expected a character", Some(ch1_val), Some(1), rest.len(), rest);
        }
        let mut ch1 = ch1_val.char();

        for (i, val) in rest.iter().enumerate().skip(1) {
            if !val.is_char() {
                return nctx.wrong_argument_violation("char>=?", "expected a character", Some(*val), Some(i + 1), rest.len(), rest);
            }
            let ch2 = val.char();
            if ch1 < ch2 {
                return nctx.return_(false);
            }
            ch1 = ch2;
        }
        nctx.return_(true)
    }

    pub ("char-ci>=?") fn char_ci_ge<'gc>(nctx, rest: &'gc [Value<'gc>]) -> bool {
        if rest.len() == 0 {
            return nctx.wrong_argument_violation("char-ci>=?", "at least 1 argument required", None, None, 0, rest);
        }

        if rest.len() < 2 {
            return nctx.return_(true);
        }

        let ch1_val = rest[0];
        if !ch1_val.is_char() {
            return nctx.wrong_argument_violation("char-ci>=?", "expected a character", Some(ch1_val), Some(1), rest.len(), rest);
        }
        let cm = icu::casemap::CaseMapperBorrowed::new();
        let mut ch1 = cm.simple_fold(ch1_val.char());

        for (i, val) in rest.iter().enumerate().skip(1) {
            if !val.is_char() {
                return nctx.wrong_argument_violation("char-ci>=?", "expected a character", Some(*val), Some(i + 1), rest.len(), rest);
            }
            let ch2 = cm.simple_fold(val.char());
            if ch1 < ch2 {
                return nctx.return_(false);
            }
            ch1 = ch2;
        }
        nctx.return_(true)
    }


    pub ("char-title-case?") fn char_title_case<'gc>(nctx, ch: char) -> bool {


        let category = get_general_category(ch);
        nctx.return_(category == GeneralCategory::TitlecaseLetter)
    }

    pub ("char-lower-case?") fn char_lower_casep<'gc>(nctx, ch: char) -> bool {
        nctx.return_(ch.is_lowercase())
    }

    pub ("char-upper-case?") fn char_upper_casep<'gc>(nctx, ch: char) -> bool {
        nctx.return_(ch.is_uppercase())
    }

    pub ("char-whitespace?") fn char_whitespacep<'gc>(nctx, ch: char) -> bool {
        nctx.return_(ch.is_whitespace())
    }

    pub ("char-numeric?") fn char_numericp<'gc>(nctx, ch: char) -> bool {
        nctx.return_(ch.is_numeric())
    }

    pub ("char-alphabetic?") fn char_alphabeticp<'gc>(nctx, ch: char) -> bool {
        nctx.return_(ch.is_alphabetic())
    }

    pub ("string-downcase") fn string_downcase<'gc>(nctx, str: Gc<'gc, Str<'gc>>) -> Gc<'gc, Str<'gc>> {
        let cm = icu::casemap::CaseMapperBorrowed::new();
        let str = str.to_string();
        let lowercased = cm.lowercase_to_string(&str, &icu::locale::langid!("und"));
        let s = Str::new(&nctx.ctx, &lowercased, false);
        nctx.return_(s)
    }

    pub ("string-foldcase") fn string_foldcase<'gc>(nctx, str: Gc<'gc, Str<'gc>>) -> Gc<'gc, Str<'gc>> {
        let cm = icu::casemap::CaseMapperBorrowed::new();
        let str = str.to_string();
        let folded = cm.fold_string(&str);
        let s = Str::new(&nctx.ctx, &folded, false);
        nctx.return_(s)
    }

    pub ("string-upcase") fn string_upcase<'gc>(nctx, str: Gc<'gc, Str<'gc>>) -> Gc<'gc, Str<'gc>> {
        let str = str.to_string();
        let cm = icu::casemap::CaseMapperBorrowed::new();
        let uppercased = cm.uppercase_to_string(&str, &icu::locale::langid!("und"));
        let s = Str::new(&nctx.ctx, &uppercased, false);
        nctx.return_(s)
    }

    pub ("string-titlecase") fn string_titlecase<'gc>(nctx, str: Gc<'gc, Str<'gc>>) -> Gc<'gc, Str<'gc>> {
        let mut result = String::new();
        let mut prev_was_delimiter = true;

        for ch in str.to_string().chars() {
            if prev_was_delimiter {
                result.extend(ch.to_uppercase());
            } else {
                result.extend(ch.to_lowercase());
            }
            prev_was_delimiter = ch.is_whitespace() || get_general_category(ch) == GeneralCategory::DashPunctuation;
        }

        let s = Str::new(&nctx.ctx, &result, false);
        nctx.return_(s)
    }

    pub ("char-upcase") fn char_upcase<'gc>(nctx, ch: char) -> char {
        let cm = icu::casemap::CaseMapperBorrowed::new();
        let uppercased = cm.simple_uppercase(ch);
        nctx.return_(uppercased)
    }

    pub ("char-downcase") fn char_downcase<'gc>(nctx, ch: char) -> char {
        let cm = icu::casemap::CaseMapperBorrowed::new();
        let lowercased = cm.simple_lowercase(ch);
        nctx.return_(lowercased)
    }

    pub ("char-foldcase") fn char_foldcase<'gc>(nctx, ch: char) -> char {
        let cm = icu::casemap::CaseMapperBorrowed::new();
        let folded = cm.simple_fold(ch);
        nctx.return_(folded)
    }

    pub ("char-titlecase") fn char_titlecase<'gc>(nctx, ch: char) -> char {
        let cm = icu::casemap::CaseMapperBorrowed::new();
        let titlecased = cm.simple_titlecase(ch);
        nctx.return_(titlecased)
    }

    pub ("string-normalize-nfc") fn string_normalize_nfc<'gc>(nctx, str: Gc<'gc, Str<'gc>>) -> Gc<'gc, Str<'gc>> {
        use unicode_normalization::UnicodeNormalization;

        let normalized: String = str.to_string().nfc().collect();
        let s = Str::new(&nctx.ctx, &normalized, false);
        nctx.return_(s)
    }

    pub ("string-normalize-nfd") fn string_normalize_nfd<'gc>(nctx, str: Gc<'gc, Str<'gc>>) -> Gc<'gc, Str<'gc>> {
        use unicode_normalization::UnicodeNormalization;

        let normalized: String = str.to_string().nfd().collect();
        let s = Str::new(&nctx.ctx, &normalized, false);
        nctx.return_(s)
    }

    pub ("string-normalize-nfkc") fn string_normalize_nfkc<'gc>(nctx, str: Gc<'gc, Str<'gc>>) -> Gc<'gc, Str<'gc>> {
        use unicode_normalization::UnicodeNormalization;

        let normalized: String = str.to_string().nfkc().collect();
        let s = Str::new(&nctx.ctx, &normalized, false);
        nctx.return_(s)
    }

    pub ("string-normalize-nfkd") fn string_normalize_nfkd<'gc>(nctx, str: Gc<'gc, Str<'gc>>) -> Gc<'gc, Str<'gc>> {
        use unicode_normalization::UnicodeNormalization;

        let normalized: String = str.to_string().nfkd().collect();
        let s = Str::new(&nctx.ctx, &normalized, false);
        nctx.return_(s)
    }

);

pub fn init_strings<'gc>(ctx: Context<'gc>) {
    // SAFETY: for compatibility with C libraries that depend on locale
    unsafe {
        libc::setlocale(libc::LC_ALL, b"\0".as_ptr() as *const i8);
    }
    let _ = &*LOCALE;
    let _ = register_str_fns(ctx);
}
