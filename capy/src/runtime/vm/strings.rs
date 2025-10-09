use rsgc::Gc;

use crate::native_fn;

use crate::runtime::prelude::*;

native_fn!(
    register_str_fns:

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
                todo!()
            }

            return nctx.return_(Ok(strs[0].downcast::<Str>()));
        }


        let mut buffer = String::new();

        for str in strs.iter() {
            if !str.is::<Str>() {
                todo!()
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
                todo!()
            }

            return nctx.return_(Ok(syms[0].downcast::<Symbol>()));
        }


        let mut buffer = String::new();

        for sym in syms.iter() {
            if !sym.is::<Symbol>() {
                todo!()
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

    /// prints all arguments using their `Display` implementation, followed by a newline.
    pub (":print") fn print<'gc>(nctx, args: &'gc [Value<'gc>]) -> () {
        for arg in args.iter() {
            print!("{}", arg);
        }
        println!();
        nctx.return_(())
    }

    pub ("substring") fn substring<'gc>(nctx, str: Gc<'gc, Str<'gc>>, start: usize, end: usize) -> Result<Gc<'gc, Str<'gc>>, Value<'gc>> {
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
        let bytevec = ByteVector::from_slice(&nctx.ctx, &bytes);
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

        let bytevec = ByteVector::from_slice(&nctx.ctx, &bytes);
        nctx.return_(bytevec)
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

    pub ("char-whitespace?") fn char_whitespace<'gc>(nctx, ch: char) -> bool {
        nctx.return_(ch.is_whitespace())
    }

    pub ("char-alphabetic?") fn char_alphabetic<'gc>(nctx, ch: char) -> bool {
        nctx.return_(ch.is_alphabetic())
    }

    pub ("char-numeric?") fn char_numeric<'gc>(nctx, ch: char) -> bool {
        nctx.return_(ch.is_numeric())
    }

    pub ("char-upper-case?") fn char_upper_case<'gc>(nctx, ch: char) -> bool {
        nctx.return_(ch.is_uppercase())
    }

    pub ("char-lower-case?") fn char_lower_case<'gc>(nctx, ch: char) -> bool {
        nctx.return_(ch.is_lowercase())
    }

    pub ("char-title-case?") fn char_title_case<'gc>(nctx, ch: char) -> bool {
        use unicode_general_category::get_general_category;
        use unicode_general_category::GeneralCategory;

        let category = get_general_category(ch);
        nctx.return_(category == GeneralCategory::TitlecaseLetter)
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



);

pub fn init_strings<'gc>(ctx: Context<'gc>) {
    let _ = register_str_fns(ctx);
}
