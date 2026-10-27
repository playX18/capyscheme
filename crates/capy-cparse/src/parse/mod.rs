//! C declaration parser: specifiers, declarators, records, enums and the top-level loop.

mod expr;
mod stmt;

use crate::consteval::ConstVal;
use crate::pp::Severity;
use crate::token::{Kind, Loc, Punct, Token, Word};
use crate::types::*;
use std::rc::Rc;

pub struct PErr {
    pub msg: String,
    pub loc: Loc,
}

pub type PR<T> = Result<T, PErr>;

pub struct Parser {
    pub u: Unit,
    pub(crate) toks: Vec<Token>,
    pub(crate) pos: usize,
    eof: Token,
    cur_pack: Option<u64>,
    pack_stack: Vec<Option<u64>>,
    pub std_c23: bool,
    /// Parse the bodies of `inline` functions (others are skipped).
    pub parse_inline_bodies: bool,
    pub(crate) fn_depth: u32,
}

#[derive(Default, Clone)]
pub(crate) struct Specs {
    storage: Option<Storage>,
    inline: bool,
    ty: Option<Type>,
    attrs: Attrs,
    is_const: bool,
    align: Option<u64>,
    loc: Loc,
}

#[derive(Default)]
pub(crate) struct DeclInfo {
    /// Parameters (with names) of the function declarator attached directly to the name.
    params: Option<Vec<Param>>,
    variadic: bool,
}

/// Qualifiers and annotations that never affect the type.
fn is_ignorable(w: Option<Word>) -> bool {
    matches!(
        w,
        Some(
            Word::Volatile
                | Word::Restrict
                | Word::IgnoredQualifier
                | Word::Noreturn
                | Word::Extension
        )
    )
}

/// Attribute names, with the `__name__` spelling folded onto `name`.
#[derive(Clone, Copy, PartialEq, Eq)]
enum AttrName {
    Packed,
    Aligned,
    Mode,
    VectorSize,
    ExtVector,
    Noreturn,
    Deprecated,
    Unavailable,
    WeakImport,
    Visibility,
    Availability,
    Cleanup,
}

impl AttrName {
    fn lookup(s: &str) -> Option<AttrName> {
        Some(match s.trim_matches('_') {
            "packed" => AttrName::Packed,
            "aligned" | "align" => AttrName::Aligned,
            "mode" => AttrName::Mode,
            "vector_size" => AttrName::VectorSize,
            "ext_vector_type" | "neon_vector_type" => AttrName::ExtVector,
            "noreturn" => AttrName::Noreturn,
            "deprecated" => AttrName::Deprecated,
            "unavailable" => AttrName::Unavailable,
            "weak_import" => AttrName::WeakImport,
            "visibility" => AttrName::Visibility,
            "availability" => AttrName::Availability,
            "cleanup" => AttrName::Cleanup,
            _ => return None,
        })
    }
}

/// Operand of `__attribute__((mode(..)))`.
enum Mode {
    Bytes(u64),
    /// `word`, `pointer` and `unwind_word`.
    Word,
    Float(FloatKind),
}

impl Mode {
    fn lookup(s: &str) -> Option<Mode> {
        Some(match s.trim_matches('_') {
            "QI" | "byte" => Mode::Bytes(1),
            "HI" => Mode::Bytes(2),
            "SI" => Mode::Bytes(4),
            "DI" => Mode::Bytes(8),
            "TI" => Mode::Bytes(16),
            "word" | "pointer" | "unwind_word" => Mode::Word,
            "SF" => Mode::Float(FloatKind::Float),
            "DF" => Mode::Float(FloatKind::Double),
            "XF" => Mode::Float(FloatKind::LongDouble),
            "TF" => Mode::Float(FloatKind::Float128),
            "HF" => Mode::Float(FloatKind::Half),
            _ => return None,
        })
    }
}

/// First argument of `#pragma pack`.
#[derive(Clone, Copy, PartialEq, Eq)]
enum PackOp {
    Push,
    Pop,
}

impl PackOp {
    fn lookup(s: &str) -> Option<PackOp> {
        match s {
            "push" => Some(PackOp::Push),
            "pop" => Some(PackOp::Pop),
            _ => None,
        }
    }
}

impl Parser {
    pub fn new(u: Unit) -> Parser {
        let mut eof = Token::new(Kind::Other, "");
        eof.bol = true;
        Parser {
            u,
            toks: Vec::new(),
            pos: 0,
            eof,
            cur_pack: None,
            pack_stack: Vec::new(),
            std_c23: false,
            parse_inline_bodies: true,
            fn_depth: 0,
        }
    }

    pub(crate) fn tok(&self) -> &Token {
        self.toks.get(self.pos).unwrap_or(&self.eof)
    }

    pub(crate) fn tok_at(&self, n: usize) -> &Token {
        self.toks.get(self.pos + n).unwrap_or(&self.eof)
    }

    pub(crate) fn at_eof(&self) -> bool {
        self.pos >= self.toks.len()
    }

    pub(crate) fn bump(&mut self) -> Token {
        let t = self.tok().clone();
        if self.pos < self.toks.len() {
            self.pos += 1;
        }
        t
    }

    pub(crate) fn err<T>(&self, msg: impl Into<String>) -> PR<T> {
        Err(PErr {
            msg: msg.into(),
            loc: self.tok().loc,
        })
    }

    pub(crate) fn at_n(&self, n: usize, p: Punct) -> bool {
        self.tok_at(n).is_punct(p)
    }

    pub(crate) fn at(&self, p: Punct) -> bool {
        self.at_n(0, p)
    }

    pub(crate) fn at_word_n(&self, n: usize, w: Word) -> bool {
        self.tok_at(n).is_word(w)
    }

    pub(crate) fn at_word(&self, w: Word) -> bool {
        self.at_word_n(0, w)
    }

    /// Reserved word at the cursor, if the token is one.
    pub(crate) fn word(&self) -> Option<Word> {
        self.tok().word
    }

    pub(crate) fn eat(&mut self, p: Punct) -> bool {
        let hit = self.at(p);
        self.pos += hit as usize;
        hit
    }

    pub(crate) fn eat_word(&mut self, w: Word) -> bool {
        let hit = self.at_word(w);
        self.pos += hit as usize;
        hit
    }

    pub(crate) fn expect(&mut self, p: Punct) -> PR<()> {
        if self.eat(p) {
            Ok(())
        } else {
            self.err(format!(
                "expected '{}' but found '{}'",
                p.spelling(),
                self.tok().text
            ))
        }
    }

    pub(crate) fn expect_word(&mut self, w: Word) -> PR<()> {
        if self.eat_word(w) {
            Ok(())
        } else {
            self.err(format!(
                "expected a keyword but found '{}'",
                self.tok().text
            ))
        }
    }

    pub(crate) fn ident(&mut self) -> PR<Rc<str>> {
        let t = self.tok();
        if t.kind == Kind::Ident {
            Ok(self.bump().text)
        } else {
            self.err(format!("expected identifier but found '{}'", t.text))
        }
    }

    /// Consumes a balanced bracket group starting at the current `(`, `[` or `{`.
    pub(crate) fn skip_balanced(&mut self) {
        let mut depth = 0i32;
        while !self.at_eof() {
            match self.bump().punct() {
                Some(Punct::LParen | Punct::LBracket | Punct::LBrace) => depth += 1,
                Some(Punct::RParen | Punct::RBracket | Punct::RBrace) => {
                    depth -= 1;
                    if depth <= 0 {
                        return;
                    }
                }
                _ => {}
            }
        }
    }

    /// Tokens strictly inside the bracket group at the cursor; consumes the group.
    pub(crate) fn take_group(&mut self) -> Vec<Token> {
        let start = self.pos;
        self.skip_balanced();
        let end = self.pos.saturating_sub(1).max(start + 1);
        self.toks[(start + 1).min(self.toks.len())..end.min(self.toks.len())].to_vec()
    }

    /// Runs `f` over `toks` as the whole input, restoring the previous input afterwards.
    pub(crate) fn with_tokens<T>(
        &mut self,
        toks: Vec<Token>,
        f: impl FnOnce(&mut Parser) -> PR<T>,
    ) -> PR<T> {
        let saved = (
            std::mem::replace(&mut self.toks, toks),
            std::mem::replace(&mut self.pos, 0),
        );
        let r = f(self);
        let leftover = !self.at_eof();
        self.toks = saved.0;
        self.pos = saved.1;
        match r {
            Ok(_) if leftover => Err(PErr {
                msg: "unexpected trailing tokens".into(),
                loc: Loc::default(),
            }),
            r => r,
        }
    }

    pub(crate) fn push_scope(&mut self) {
        self.u.scopes.push(Default::default());
        self.u.tags.push(Default::default());
    }

    pub(crate) fn pop_scope(&mut self) {
        self.u.scopes.pop();
        self.u.tags.pop();
    }

    fn global(&self) -> bool {
        self.u.scopes.len() == 1
    }

    pub(crate) fn is_type_start_at(&self, n: usize) -> bool {
        use Word::*;
        let t = self.tok_at(n);
        if t.kind != Kind::Ident {
            return false;
        }
        match t.word {
            Some(
                Void | Char | Short | Int | Long | Float | Double | Signed | Unsigned | Bool
                | Complex | Imaginary | Struct | Union | Enum | Const | Volatile | Restrict
                | Typeof | Int128 | Int128T | UInt128T | BitInt | VaList | Float16 | BFloat16
                | Float32 | Float64 | Float128 | Attribute | Atomic | Extension | NullptrT,
            ) => true,
            Some(BoolName) => self.std_c23 || self.u.is_typedef_name(&t.text),
            _ => self.u.is_typedef_name(&t.text),
        }
    }

    pub(crate) fn is_type_start(&self) -> bool {
        self.is_type_start_at(0)
    }

    pub(crate) fn is_decl_start(&self) -> bool {
        let t = self.tok();
        if t.kind != Kind::Ident {
            return false;
        }
        if self.is_type_start() {
            // `T:` is a label even when T names a type.
            return !self.at_n(1, Punct::Colon);
        }
        matches!(
            t.word,
            Some(
                Word::Typedef
                    | Word::Extern
                    | Word::Static
                    | Word::Register
                    | Word::Auto
                    | Word::ThreadLocal
                    | Word::Constexpr
                    | Word::Inline
                    | Word::Noreturn
                    | Word::StaticAssert
                    | Word::Alignas
                    | Word::AutoType
            )
        )
    }

    fn const_from_tokens(&mut self, toks: Vec<Token>) -> Option<i128> {
        let e = self.with_tokens(toks, |p| p.parse_assign()).ok()?;
        match self.u.eval(&e).ok()? {
            ConstVal::Int(v, _) => Some(v),
            _ => None,
        }
    }

    /// Parses any run of `__attribute__((..))`, `[[..]]` and `__declspec(..)`.
    pub(crate) fn attributes(&mut self, a: &mut Attrs) -> bool {
        let mut any = false;
        loop {
            if self.at_word(Word::Attribute) && self.at_n(1, Punct::LParen) {
                any = true;
                self.bump();
                let inner = self.take_group();
                // inner is `( a, b(x), c )`
                if inner.first().is_some_and(|t| t.is_punct(Punct::LParen)) {
                    let mut depth = 0;
                    let mut end = inner.len();
                    for (i, t) in inner.iter().enumerate() {
                        if t.is_punct(Punct::LParen) {
                            depth += 1;
                        } else if t.is_punct(Punct::RParen) {
                            depth -= 1;
                            if depth == 0 {
                                end = i;
                                break;
                            }
                        }
                    }
                    self.attr_list(&inner[1..end], a);
                }
            } else if self.at(Punct::LBracket) && self.at_n(1, Punct::LBracket) {
                any = true;
                self.bump();
                let group = self.take_group(); // `[ ... ]` content of the outer, i.e. `[attrs]`
                if group.first().is_some_and(|t| t.is_punct(Punct::LBracket))
                    && group.last().is_some_and(|t| t.is_punct(Punct::RBracket))
                {
                    self.attr_list(&group[1..group.len() - 1], a);
                }
            } else if self.at_word(Word::Declspec) && self.at_n(1, Punct::LParen) {
                any = true;
                self.bump();
                self.skip_balanced();
            } else {
                return any;
            }
        }
    }

    fn attr_list(&mut self, toks: &[Token], a: &mut Attrs) {
        let mut i = 0;
        while i < toks.len() {
            // Split at top-level commas.
            let mut j = i;
            let mut depth = 0;
            while j < toks.len() {
                if toks[j].is_punct(Punct::LParen) {
                    depth += 1;
                } else if toks[j].is_punct(Punct::RParen) {
                    depth -= 1;
                } else if toks[j].is_punct(Punct::Comma) && depth == 0 {
                    break;
                }
                j += 1;
            }
            self.attr_one(&toks[i..j], a);
            i = j + 1;
        }
    }

    fn attr_one(&mut self, toks: &[Token], a: &mut Attrs) {
        // Skip a scope prefix such as `gnu::`.
        let mut k = 0;
        while k + 1 < toks.len() && toks[k + 1].is_punct(Punct::ColonColon) {
            k += 2;
        }
        let Some(name) = toks.get(k).and_then(|t| AttrName::lookup(&t.text)) else {
            return;
        };
        let args: Vec<Token> = match toks.get(k + 1) {
            Some(t) if t.is_punct(Punct::LParen) && toks.len() >= k + 3 => {
                toks[k + 2..toks.len() - 1].to_vec()
            }
            _ => Vec::new(),
        };
        match name {
            AttrName::Packed => a.packed = true,
            AttrName::Aligned => {
                a.aligned = Some(if args.is_empty() {
                    16
                } else {
                    self.const_from_tokens(args).unwrap_or(16) as u64
                })
            }
            AttrName::Mode => a.mode = args.first().map(|t| t.text.trim_matches('_').to_string()),
            AttrName::VectorSize => a.vector_size = self.const_from_tokens(args).map(|v| v as u64),
            AttrName::ExtVector => a.ext_vector = self.const_from_tokens(args).map(|v| v as u64),
            AttrName::Noreturn => a.noreturn = true,
            AttrName::Deprecated => a.deprecated = true,
            AttrName::Unavailable => a.unavailable = true,
            AttrName::WeakImport => a.weak_import = true,
            AttrName::Visibility => {
                a.visibility = args.first().map(|t| t.text.trim_matches('"').to_string())
            }
            AttrName::Availability => {
                if args
                    .iter()
                    .any(|t| AttrName::lookup(&t.text) == Some(AttrName::Unavailable))
                {
                    a.unavailable = true;
                }
            }
            AttrName::Cleanup => a.cleanup = true,
        }
    }

    /// Skips pointer-level qualifiers and attributes; returns whether `const` was seen.
    fn pointer_quals(&mut self, a: &mut Attrs) -> bool {
        let mut is_const = false;
        loop {
            if self.attributes(a) {
                continue;
            }
            if self.tok().kind != Kind::Ident {
                return is_const;
            }
            match self.word() {
                Some(Word::Const) => {
                    is_const = true;
                    self.bump();
                }
                w if is_ignorable(w) => {
                    self.bump();
                }
                Some(Word::Atomic) if !self.at_n(1, Punct::LParen) => {
                    self.bump();
                }
                _ => return is_const,
            }
        }
    }

    pub(crate) fn decl_specs(&mut self) -> PR<Specs> {
        let mut s = Specs {
            loc: self.tok().loc,
            ..Default::default()
        };
        let (mut signed, mut unsigned, mut short, mut long) = (false, false, false, 0u8);
        let (mut ch, mut int, mut float, mut double, mut void, mut boolean) =
            (false, false, false, false, false, false);
        let (mut complex, mut int128, mut other): (bool, bool, Option<Type>) = (false, false, None);
        let mut seen_type = false;

        loop {
            if self.attributes(&mut s.attrs) {
                continue;
            }
            let t = self.tok().clone();
            if t.kind != Kind::Ident {
                break;
            }
            match t.word {
                Some(Word::Typedef) => s.storage = Some(Storage::Typedef),
                Some(Word::Extern) => s.storage = Some(Storage::Extern),
                Some(Word::Static) => s.storage = Some(Storage::Static),
                Some(Word::Register) => s.storage = Some(Storage::Register),
                Some(Word::Auto) => s.storage = Some(Storage::Auto),
                Some(Word::ThreadLocal) => s.storage = Some(Storage::ThreadLocal),
                Some(Word::Constexpr) => {
                    s.is_const = true;
                    s.storage.get_or_insert(Storage::Static);
                }
                Some(Word::Inline) => s.inline = true,
                Some(Word::Noreturn) => s.attrs.noreturn = true,
                Some(Word::Const) => s.is_const = true,
                w if is_ignorable(w) => {}
                Some(Word::Atomic) if !self.at_n(1, Punct::LParen) => {}
                Some(Word::Alignas) => {
                    self.bump();
                    if self.at(Punct::LParen) {
                        let inner = self.take_group();
                        let names_type = inner.first().is_some_and(|t| {
                            self.u.is_typedef_name(&t.text)
                                || matches!(
                                    t.word,
                                    Some(
                                        Word::Struct
                                            | Word::Union
                                            | Word::Enum
                                            | Word::Int
                                            | Word::Char
                                            | Word::Long
                                            | Word::Short
                                            | Word::Double
                                            | Word::Float
                                            | Word::Unsigned
                                            | Word::Signed
                                    )
                                )
                        });
                        let al = if names_type {
                            self.with_tokens(inner, |p| {
                                let ty = p.type_name()?;
                                Ok(p.u.size_align(&ty).map(|x| x.1 as i128))
                            })
                            .ok()
                            .flatten()
                        } else {
                            self.const_from_tokens(inner)
                        };
                        if let Some(a) = al {
                            s.align = s.align.max(Some(a as u64));
                        }
                    }
                    continue;
                }
                Some(Word::Signed) => signed = true,
                Some(Word::Unsigned) => unsigned = true,
                Some(Word::Short) => short = true,
                Some(Word::Long) => long += 1,
                Some(Word::Char) => ch = true,
                Some(Word::Int) => int = true,
                Some(Word::Float) => float = true,
                Some(Word::Double) => double = true,
                Some(Word::Void) => void = true,
                Some(Word::Bool) => boolean = true,
                Some(Word::BoolName)
                    if (self.std_c23 || self.u.is_typedef_name("bool")) && !seen_type =>
                {
                    match self.u.lookup("bool") {
                        Some(Ident::Typedef(t)) => other = Some(t.clone()),
                        _ => boolean = true,
                    }
                }
                Some(Word::Complex | Word::Imaginary) => complex = true,
                Some(Word::Int128) => int128 = true,
                Some(Word::Int128T) => int128 = true,
                Some(Word::UInt128T) => {
                    int128 = true;
                    unsigned = true;
                }
                Some(Word::Float16) => other = Some(Type::Float(FloatKind::Half)),
                Some(Word::BFloat16) => other = Some(Type::Float(FloatKind::BFloat16)),
                Some(Word::Float32) => other = Some(Type::Float(FloatKind::Float)),
                Some(Word::Float64) => other = Some(Type::Float(FloatKind::Double)),
                Some(Word::Float128) => other = Some(Type::Float(FloatKind::Float128)),
                Some(Word::VaList) => other = Some(Type::VaList),
                Some(Word::NullptrT) if !self.u.is_typedef_name("nullptr_t") => {
                    other = Some(Type::NullPtr)
                }
                Some(Word::BitInt) => {
                    self.bump();
                    let inner = self.take_group();
                    let n = self.const_from_tokens(inner).unwrap_or(0) as u32;
                    other = Some(Type::BitInt(n, !unsigned));
                    seen_type = true;
                    continue;
                }
                Some(Word::Struct | Word::Union) => {
                    other = Some(self.record_spec()?);
                    seen_type = true;
                    continue;
                }
                Some(Word::Enum) => {
                    other = Some(self.enum_spec()?);
                    seen_type = true;
                    continue;
                }
                Some(Word::Typeof) => {
                    self.bump();
                    let inner = self.take_group();
                    let ty = self.with_tokens(inner, |p| {
                        if p.is_type_start() {
                            p.type_name()
                        } else {
                            let e = p.parse_expr()?;
                            Ok(p.u.type_of(&e).unwrap_or_else(|| {
                                Type::Unsupported("typeof of untypable expression".into())
                            }))
                        }
                    });
                    other = Some(ty.unwrap_or_else(|e| {
                        Type::Unsupported(format!("typeof: {}", e.msg).into())
                    }));
                    seen_type = true;
                    continue;
                }
                Some(Word::Atomic) => {
                    self.bump();
                    let inner = self.take_group();
                    let ty = self.with_tokens(inner, |p| p.type_name());
                    other = Some(match ty {
                        Ok(t) => Type::Atomic(Box::new(t)),
                        Err(e) => Type::Unsupported(format!("_Atomic: {}", e.msg).into()),
                    });
                    seen_type = true;
                    continue;
                }
                Some(Word::AutoType) => other = Some(Type::Unsupported("__auto_type".into())),
                _ => {
                    // A typedef name, but only before any other type specifier.
                    let counted = signed
                        || unsigned
                        || short
                        || long > 0
                        || ch
                        || int
                        || float
                        || double
                        || void
                        || boolean
                        || complex
                        || int128
                        || other.is_some();
                    if !counted && self.u.is_typedef_name(&t.text) {
                        if let Some(Ident::Typedef(ty)) = self.u.lookup(&t.text) {
                            other = Some(ty.clone());
                        }
                    } else {
                        break;
                    }
                }
            }
            seen_type = true;
            self.bump();
        }

        let int_like = signed || unsigned || short || long > 0 || ch || int || int128;
        s.ty = if let Some(t) = other {
            if complex {
                match t {
                    Type::Float(k) => Some(Type::Complex(k)),
                    t => Some(t),
                }
            } else {
                Some(t)
            }
        } else if void {
            Some(Type::Void)
        } else if boolean {
            Some(Type::Bool)
        } else if float || double {
            let k = if float {
                FloatKind::Float
            } else if long > 0 {
                FloatKind::LongDouble
            } else {
                FloatKind::Double
            };
            Some(if complex {
                Type::Complex(k)
            } else {
                Type::Float(k)
            })
        } else if complex {
            Some(Type::Complex(FloatKind::Double))
        } else if int_like {
            Some(Type::Int(if int128 {
                if unsigned {
                    IntKind::UInt128
                } else {
                    IntKind::Int128
                }
            } else if ch {
                if unsigned {
                    IntKind::UChar
                } else if signed {
                    IntKind::SChar
                } else {
                    IntKind::Char
                }
            } else if short {
                if unsigned {
                    IntKind::UShort
                } else {
                    IntKind::Short
                }
            } else if long >= 2 {
                if unsigned {
                    IntKind::ULongLong
                } else {
                    IntKind::LongLong
                }
            } else if long == 1 {
                if unsigned {
                    IntKind::ULong
                } else {
                    IntKind::Long
                }
            } else if unsigned {
                IntKind::UInt
            } else {
                IntKind::Int
            }))
        } else {
            None
        };
        if let (Some(t), true) = (s.ty.take(), true) {
            s.ty = Some(self.apply_type_attrs(t, &mut s.attrs));
        }
        Ok(s)
    }

    /// Applies `mode`, `vector_size` and `ext_vector_type` to a type.
    pub(crate) fn apply_type_attrs(&mut self, t: Type, a: &mut Attrs) -> Type {
        let mut t = t;
        if let Some(m) = a.mode.take().as_deref().and_then(Mode::lookup) {
            let unsigned = matches!(&t, Type::Int(k) if self.u.abi.is_unsigned(*k));
            match m {
                Mode::Float(f) => t = Type::Float(f),
                Mode::Bytes(_) | Mode::Word => {
                    let abi = &self.u.abi;
                    let bytes = match m {
                        Mode::Bytes(b) => b,
                        _ => abi.ptr,
                    };
                    let k = match bytes {
                        1 => IntKind::SChar,
                        2 => IntKind::Short,
                        4 => IntKind::Int,
                        8 => {
                            if abi.long == 8 {
                                IntKind::Long
                            } else {
                                IntKind::LongLong
                            }
                        }
                        _ => IntKind::Int128,
                    };
                    t = Type::Int(if unsigned { k.to_unsigned() } else { k });
                }
            }
        }
        if let Some(n) = a.vector_size.take() {
            t = Type::Vector(Box::new(t), n);
        }
        if let Some(n) = a.ext_vector.take() {
            if let Some((es, _)) = self.u.size_align(&t) {
                let n = if n == 3 { 4 } else { n };
                t = Type::Vector(Box::new(t), es * n);
            }
        }
        t
    }

    fn record_spec(&mut self) -> PR<Type> {
        let kind = if self.bump().is_word(Word::Struct) {
            RecKind::Struct
        } else {
            RecKind::Union
        };
        let mut attrs = Attrs::default();
        self.attributes(&mut attrs);
        let loc = self.tok().loc;
        let tag = if self.tok().kind == Kind::Ident && !self.at(Punct::LBrace) {
            Some(self.ident()?)
        } else {
            None
        };
        self.attributes(&mut attrs);

        let defining = self.at(Punct::LBrace);
        let id = match &tag {
            Some(t) => {
                let cur = self.u.tags.last().and_then(|s| s.get(t)).copied();
                match cur {
                    Some(TagRef::Rec(id)) if !(defining && self.u.records[id].complete) => id,
                    _ if !defining => match self.u.lookup_tag(t) {
                        Some(TagRef::Rec(id)) => id,
                        _ => self.new_record(kind, tag.clone(), loc, true),
                    },
                    _ => self.new_record(kind, tag.clone(), loc, true),
                }
            }
            None => self.new_record(kind, None, loc, false),
        };
        if !defining {
            return Ok(Type::Record(id));
        }

        self.bump(); // {
        self.push_scope();
        let mut fields = Vec::new();
        loop {
            if self.eat(Punct::RBrace) {
                break;
            }
            if self.at_eof() {
                self.pop_scope();
                return self.err("unterminated struct or union");
            }
            if self.eat(Punct::Semi) {
                continue;
            }
            if self.at_word(Word::StaticAssert) {
                self.static_assert()?;
                continue;
            }
            let specs = self.decl_specs()?;
            let Some(base) = specs.ty.clone() else {
                self.pop_scope();
                return self.err("expected a type in member declaration");
            };
            if self.at(Punct::Semi) && matches!(base, Type::Record(_)) {
                // Anonymous struct/union member.
                fields.push(Field {
                    name: None,
                    ty: base,
                    bits: None,
                    align_attr: specs.align,
                    packed: specs.attrs.packed,
                });
                self.bump();
                continue;
            }
            loop {
                let mut name = None;
                let mut ty = base.clone();
                let mut fattrs = specs.attrs.clone();
                if !self.at(Punct::Colon) {
                    let (n, t, _) = self.declarator(base.clone(), true)?;
                    name = n;
                    ty = t;
                }
                let mut bits = None;
                if self.eat(Punct::Colon) {
                    let e = self.parse_assign()?;
                    bits = match self.u.eval(&e) {
                        Ok(ConstVal::Int(v, _)) if v >= 0 => Some(v as u32),
                        _ => return self.err("bit-field width is not a non-negative constant"),
                    };
                }
                self.attributes(&mut fattrs);
                let ty = self.apply_type_attrs(ty, &mut fattrs);
                fields.push(Field {
                    name,
                    ty,
                    bits,
                    align_attr: fattrs.aligned.or(specs.align),
                    packed: fattrs.packed,
                });
                if !self.eat(Punct::Comma) {
                    break;
                }
            }
            self.expect(Punct::Semi)?;
        }
        self.pop_scope();
        let mut tail = Attrs::default();
        self.attributes(&mut tail);
        attrs.merge(tail);

        let rec = &mut self.u.records[id];
        rec.fields = fields;
        rec.complete = true;
        rec.packed = attrs.packed;
        rec.align_attr = attrs.aligned;
        rec.pack = self.cur_pack;
        rec.loc = loc;
        let layout = self.u.compute_layout(id);
        self.u.records[id].layout = Some(layout);
        self.u.decls.push(Decl::Record(id));
        Ok(Type::Record(id))
    }

    fn new_record(
        &mut self,
        kind: RecKind,
        tag: Option<Rc<str>>,
        loc: Loc,
        register: bool,
    ) -> RecId {
        let id = self.u.records.len();
        self.u.records.push(Record {
            kind,
            tag: tag.clone(),
            fields: Vec::new(),
            complete: false,
            packed: false,
            align_attr: None,
            pack: None,
            layout: None,
            loc,
        });
        if let (Some(t), true) = (tag, register) {
            self.u.tags.last_mut().unwrap().insert(t, TagRef::Rec(id));
        }
        id
    }

    fn enum_spec(&mut self) -> PR<Type> {
        self.bump(); // enum
        let mut attrs = Attrs::default();
        self.attributes(&mut attrs);
        let loc = self.tok().loc;
        let tag = if self.tok().kind == Kind::Ident && !self.at(Punct::Colon) {
            Some(self.ident()?)
        } else {
            None
        };
        self.attributes(&mut attrs);
        let mut fixed = None;
        if self.eat(Punct::Colon) {
            let s = self.decl_specs()?;
            fixed = s.ty;
        }
        let defining = self.at(Punct::LBrace);
        let existing = tag.as_ref().and_then(|t| {
            let found = if defining {
                self.u.tags.last().and_then(|s| s.get(t)).copied()
            } else {
                self.u.lookup_tag(t)
            };
            match found {
                Some(TagRef::Enum(id)) => Some(id),
                _ => None,
            }
        });
        let id = match existing {
            Some(id) if !(defining && self.u.enums[id].complete) => id,
            _ => {
                let id = self.u.enums.len();
                self.u.enums.push(EnumDef {
                    tag: tag.clone(),
                    fixed: fixed.clone(),
                    consts: Vec::new(),
                    complete: false,
                    loc,
                });
                if let Some(t) = &tag {
                    self.u
                        .tags
                        .last_mut()
                        .unwrap()
                        .insert(t.clone(), TagRef::Enum(id));
                }
                id
            }
        };
        if fixed.is_some() {
            self.u.enums[id].fixed = fixed;
        }
        if !defining {
            return Ok(Type::Enum(id));
        }
        self.bump(); // {
        let mut next: i128 = 0;
        loop {
            if self.eat(Punct::RBrace) {
                break;
            }
            let name = self.ident()?;
            let mut a = Attrs::default();
            self.attributes(&mut a);
            if self.eat(Punct::Assign) {
                let e = self.parse_assign()?;
                match self.u.eval(&e) {
                    Ok(ConstVal::Int(v, _)) => next = v,
                    _ => {
                        let loc = self.tok().loc;
                        self.u.diag(
                            Severity::Warning,
                            loc,
                            format!("enumerator '{name}' value is not constant"),
                        );
                    }
                }
            }
            self.attributes(&mut a);
            self.u.enums[id].consts.push((name.clone(), next));
            self.u
                .scopes
                .last_mut()
                .unwrap()
                .insert(name, Ident::EnumConst(next, id));
            next += 1;
            if !self.eat(Punct::Comma) {
                self.expect(Punct::RBrace)?;
                break;
            }
        }
        self.attributes(&mut attrs);
        self.u.enums[id].complete = true;
        self.u.enums[id].loc = loc;
        self.u.decls.push(Decl::Enum(id));
        Ok(Type::Enum(id))
    }

    /// Is the `(` at the cursor the start of a nested declarator (as opposed to parameters)?
    fn nested_declarator_follows(&self) -> bool {
        let t = self.tok_at(1);
        match t.kind {
            Kind::Punct(p) => matches!(
                p,
                Punct::Star | Punct::Caret | Punct::LParen | Punct::LBracket
            ),
            Kind::Ident => {
                matches!(t.word, Some(Word::Attribute | Word::Declspec))
                    || is_ignorable(t.word)
                    || (!self.is_type_start_at(1)
                        && !matches!(t.word, Some(Word::Register | Word::Void)))
            }
            _ => false,
        }
    }

    /// Parses a (possibly abstract) declarator applied to `base`.
    pub(crate) fn declarator(
        &mut self,
        base: Type,
        abstract_ok: bool,
    ) -> PR<(Option<Rc<str>>, Type, DeclInfo)> {
        let mut ty = base;
        let mut scratch = Attrs::default();
        self.attributes(&mut scratch);
        loop {
            if self.eat(Punct::Star) {
                self.pointer_quals(&mut scratch);
                ty = ty.ptr();
            } else if self.eat(Punct::Caret) {
                self.pointer_quals(&mut scratch);
                ty = Type::Void.ptr();
            } else {
                break;
            }
        }
        self.attributes(&mut scratch);

        if self.at(Punct::LParen) && self.nested_declarator_follows() {
            let open = self.pos;
            self.skip_balanced();
            let mut dummy = DeclInfo::default();
            let outer = self.suffixes(ty, &mut dummy)?;
            let end = self.pos;
            self.pos = open + 1;
            let r = self.declarator(outer, abstract_ok)?;
            self.expect(Punct::RParen)?;
            self.pos = end;
            return Ok(r);
        }
        let mut info = DeclInfo::default();
        let name = if self.tok().kind == Kind::Ident
            && !is_ignorable(self.word())
            && !matches!(self.word(), Some(Word::Asm | Word::Attribute))
        {
            Some(self.bump().text)
        } else if abstract_ok {
            None
        } else {
            return self.err(format!(
                "expected a declarator name but found '{}'",
                self.tok().text
            ));
        };
        self.attributes(&mut scratch);
        let ty = self.suffixes(ty, &mut info)?;
        Ok((name, ty, info))
    }

    /// Array and function suffixes, applied right-to-left onto `ty`.
    fn suffixes(&mut self, ty: Type, info: &mut DeclInfo) -> PR<Type> {
        enum Suf {
            Arr(ArrLen),
            Fn(Vec<Param>, bool, bool),
        }
        let mut sufs = Vec::new();
        let mut first = true;
        loop {
            if self.at(Punct::LBracket) && !(self.at_n(1, Punct::LBracket) && first_attr_like(self))
            {
                self.bump();
                let mut s = Attrs::default();
                loop {
                    if self.eat_word(Word::Static)
                        || self.eat_word(Word::Const)
                        || is_ignorable(self.word()) && self.tok().kind == Kind::Ident && {
                            self.bump();
                            true
                        }
                    {
                        continue;
                    }
                    if !self.attributes(&mut s) {
                        break;
                    }
                }
                let len = if self.eat(Punct::RBracket) {
                    ArrLen::Incomplete
                } else if self.at(Punct::Star) && self.at_n(1, Punct::RBracket) {
                    self.bump();
                    self.bump();
                    ArrLen::Variable
                } else {
                    let e = self.parse_assign()?;
                    self.expect(Punct::RBracket)?;
                    match self.u.eval(&e) {
                        Ok(ConstVal::Int(v, _)) if v >= 0 => ArrLen::Fixed(v as u64),
                        _ => ArrLen::Variable,
                    }
                };
                sufs.push(Suf::Arr(len));
            } else if self.at(Punct::LParen) {
                self.bump();
                let (params, variadic, proto) = self.params()?;
                sufs.push(Suf::Fn(params, variadic, proto));
            } else if self.attributes(&mut Attrs::default()) {
                // attributes between suffixes
            } else {
                break;
            }
            if first {
                if let Some(Suf::Fn(p, v, _)) = sufs.first() {
                    info.params = Some(p.clone());
                    info.variadic = *v;
                }
                first = false;
            }
        }
        let mut t = ty;
        for s in sufs.into_iter().rev() {
            t = match s {
                Suf::Arr(len) => Type::Array(Box::new(t), len),
                Suf::Fn(params, variadic, prototyped) => Type::Func(Rc::new(FuncType {
                    ret: t,
                    params: params.into_iter().map(|p| p.ty).collect(),
                    variadic,
                    prototyped,
                })),
            };
        }
        Ok(t)
    }

    /// Parameter list after `(`; consumes the closing `)`.
    fn params(&mut self) -> PR<(Vec<Param>, bool, bool)> {
        if self.eat(Punct::RParen) {
            return Ok((Vec::new(), false, self.std_c23));
        }
        if self.at_word(Word::Void) && self.at_n(1, Punct::RParen) {
            self.bump();
            self.bump();
            return Ok((Vec::new(), false, true));
        }
        // K&R identifier list.
        if self.tok().kind == Kind::Ident
            && !self.is_type_start()
            && !is_ignorable(self.word())
            && (self.at_n(1, Punct::Comma) || self.at_n(1, Punct::RParen))
        {
            let mut ps = Vec::new();
            loop {
                let n = self.ident()?;
                ps.push(Param {
                    name: Some(n),
                    ty: Type::Int(IntKind::Int),
                });
                if !self.eat(Punct::Comma) {
                    break;
                }
            }
            self.expect(Punct::RParen)?;
            return Ok((ps, false, false));
        }
        let mut params = Vec::new();
        let mut variadic = false;
        loop {
            if self.eat(Punct::Ellipsis) {
                variadic = true;
                break;
            }
            let specs = self.decl_specs()?;
            let base = specs.ty.clone().unwrap_or(Type::Int(IntKind::Int));
            let (name, mut ty, _) = self.declarator(base, true)?;
            let mut a = specs.attrs.clone();
            self.attributes(&mut a);
            ty = self.apply_type_attrs(ty, &mut a);
            ty = match ty {
                Type::Array(e, _) => Type::Ptr(e),
                f @ Type::Func(_) => Type::Ptr(Box::new(f)),
                t => t,
            };
            params.push(Param { name, ty });
            if !self.eat(Punct::Comma) {
                break;
            }
        }
        self.expect(Punct::RParen)?;
        Ok((params, variadic, true))
    }

    /// `type-name` (specifiers plus abstract declarator).
    pub(crate) fn type_name(&mut self) -> PR<Type> {
        let mut specs = self.decl_specs()?;
        let Some(base) = specs.ty.clone() else {
            return self.err("expected a type name");
        };
        let (_, ty, _) = self.declarator(base, true)?;
        Ok(self.apply_type_attrs(ty, &mut specs.attrs))
    }

    fn static_assert(&mut self) -> PR<()> {
        let loc = self.tok().loc;
        self.bump();
        let inner = self.take_group();
        self.eat(Punct::Semi);
        let mut parts: Vec<Vec<Token>> = vec![Vec::new()];
        let mut depth = 0;
        for t in inner {
            if t.is_punct(Punct::LParen) {
                depth += 1;
            } else if t.is_punct(Punct::RParen) {
                depth -= 1;
            }
            if t.is_punct(Punct::Comma) && depth == 0 {
                parts.push(Vec::new());
            } else {
                parts.last_mut().unwrap().push(t);
            }
        }
        let cond = parts.remove(0);
        let e = self.with_tokens(cond, |p| p.parse_assign());
        if let Ok(e) = e {
            if let Ok(ConstVal::Int(0, _)) = self.u.eval(&e) {
                let msg = parts
                    .first()
                    .and_then(|m| m.first())
                    .map(|t| t.text.to_string())
                    .unwrap_or_default();
                self.u.diag(
                    Severity::Warning,
                    loc,
                    format!("static assertion failed {msg}"),
                );
            }
        }
        Ok(())
    }

    fn pragma_pack(&mut self) {
        // `__capy_pragma_pack ( args ) ;`
        self.bump();
        let args = if self.at(Punct::LParen) {
            self.take_group()
        } else {
            Vec::new()
        };
        self.eat(Punct::Semi);
        let op = args.first().and_then(|t| PackOp::lookup(&t.text));
        let num = args
            .iter()
            .find(|t| t.kind == Kind::Number)
            .and_then(|t| t.text.parse::<u64>().ok());
        match (op, args.is_empty()) {
            (Some(PackOp::Push), _) => {
                self.pack_stack.push(self.cur_pack);
                if num.is_some() {
                    self.cur_pack = num;
                }
            }
            (Some(PackOp::Pop), _) => {
                if let Some(p) = self.pack_stack.pop() {
                    self.cur_pack = p;
                }
            }
            (None, true) => self.cur_pack = None,
            (None, false) if num.is_some() => self.cur_pack = num,
            _ => {}
        }
    }

    /// Parses `toks` as one assignment-expression (macro bodies).
    pub fn expr_from_tokens(&mut self, toks: Vec<Token>) -> Result<crate::ast::Expr, String> {
        self.with_tokens(toks, |p| p.parse_assign())
            .map_err(|e| e.msg)
    }

    /// Parses `toks` as a type-name (macro bodies such as `unsigned long`).
    pub fn type_from_tokens(&mut self, toks: Vec<Token>) -> Result<Type, String> {
        self.with_tokens(toks, |p| p.type_name()).map_err(|e| e.msg)
    }

    /// Parses the whole token stream as a translation unit.
    pub fn parse_translation_unit(&mut self, toks: Vec<Token>) {
        self.toks = toks;
        self.pos = 0;
        while !self.at_eof() {
            if self.eat(Punct::Semi) {
                continue;
            }
            if self.at_word(Word::CapyPragmaPack) {
                self.pragma_pack();
                continue;
            }
            if self.at_word(Word::StaticAssert) {
                let start = self.pos;
                if self.static_assert().is_err() {
                    self.recover(start);
                }
                continue;
            }
            if self.at_word(Word::Asm) && self.at_n(1, Punct::LParen) {
                self.bump();
                self.skip_balanced();
                self.eat(Punct::Semi);
                continue;
            }
            let start = self.pos;
            if let Err(e) = self.external_decl() {
                self.u.diag(Severity::Error, e.loc, e.msg);
                self.u.scopes.truncate(1);
                self.u.tags.truncate(1);
                self.fn_depth = 0;
                self.recover(start);
            }
        }
    }

    /// Skips to the end of the declaration that began at `start`.
    fn recover(&mut self, start: usize) {
        if self.pos <= start {
            self.pos = start;
        }
        let mut depth = 0i32;
        let mut p = start;
        // Resynchronise from the failure point, not the start, but respect nesting from start.
        while p < self.pos.max(start) && p < self.toks.len() {
            match self.toks[p].punct() {
                Some(Punct::LBrace | Punct::LParen | Punct::LBracket) => depth += 1,
                Some(Punct::RBrace | Punct::RParen | Punct::RBracket) => depth = (depth - 1).max(0),
                _ => {}
            }
            p += 1;
        }
        self.pos = p.max(start + 1).min(self.toks.len());
        while !self.at_eof() {
            match self.bump().punct() {
                Some(Punct::LBrace | Punct::LParen | Punct::LBracket) => depth += 1,
                Some(Punct::RParen | Punct::RBracket) => depth = (depth - 1).max(0),
                Some(Punct::RBrace) => {
                    depth = (depth - 1).max(0);
                    if depth == 0 {
                        self.eat(Punct::Semi);
                        return;
                    }
                }
                Some(Punct::Semi) if depth == 0 => return,
                _ => {}
            }
        }
    }

    fn asm_label(&mut self) -> Option<String> {
        if self.at_word(Word::Asm) && self.at_n(1, Punct::LParen) {
            self.bump();
            let g = self.take_group();
            let s: String = g
                .iter()
                .filter(|t| t.kind == Kind::Str)
                .map(|t| t.text.trim_matches('"').to_string())
                .collect();
            return Some(s);
        }
        None
    }

    fn external_decl(&mut self) -> PR<()> {
        let mut specs = self.decl_specs()?;
        let loc = specs.loc;
        if specs.ty.is_none() {
            if specs.storage.is_none() && !specs.inline {
                return self.err(format!("unexpected '{}' at file scope", self.tok().text));
            }
            specs.ty = Some(Type::Int(IntKind::Int));
        }
        let base = specs.ty.clone().unwrap();
        if self.eat(Punct::Semi) {
            return Ok(());
        }
        let storage = specs.storage.unwrap_or(Storage::None);
        loop {
            let (name, ty, info) = self.declarator(base.clone(), false)?;
            let name = name.unwrap();
            let nloc = self
                .toks
                .get(self.pos.saturating_sub(1))
                .map_or(loc, |t| t.loc);
            let mut attrs = specs.attrs.clone();
            self.attributes(&mut attrs);
            let label = self.asm_label();
            self.attributes(&mut attrs);
            let ty = self.apply_type_attrs(ty, &mut attrs);

            if storage == Storage::Typedef {
                let reg = if self.global() {
                    Type::Typedef(name.clone())
                } else {
                    ty.clone()
                };
                self.u
                    .scopes
                    .last_mut()
                    .unwrap()
                    .insert(name.clone(), Ident::Typedef(reg));
                if self.global() {
                    self.u.typedefs.insert(name.clone(), ty.clone());
                    self.u.decls.push(Decl::Typedef {
                        name,
                        ty,
                        loc: nloc,
                    });
                }
            } else if let Type::Func(ft) = &ty {
                let params = info.params.unwrap_or_else(|| {
                    ft.params
                        .iter()
                        .map(|t| Param {
                            name: None,
                            ty: t.clone(),
                        })
                        .collect()
                });
                self.u
                    .scopes
                    .last_mut()
                    .unwrap()
                    .insert(name.clone(), Ident::Func(ty.clone()));
                let mut f = FuncDecl {
                    name: name.clone(),
                    ret: ft.ret.clone(),
                    params,
                    variadic: ft.variadic,
                    storage,
                    inline: specs.inline,
                    asm_label: label,
                    attrs,
                    body: None,
                    has_body: false,
                    loc: nloc,
                };
                // K&R declarations between ) and {.
                while self.tok().kind == Kind::Ident && self.is_decl_start() {
                    let _ = self.decl_specs()?;
                    while !self.eat(Punct::Semi) && !self.at_eof() {
                        self.bump();
                    }
                }
                if self.at(Punct::LBrace) {
                    f.has_body = true;
                    if f.inline && self.parse_inline_bodies {
                        let start = self.pos;
                        match self.function_body(&f.params) {
                            Ok(b) => f.body = Some(b),
                            Err(e) => {
                                self.u.diag(
                                    Severity::Warning,
                                    e.loc,
                                    format!("in body of {}: {}", f.name, e.msg),
                                );
                                self.u.scopes.truncate(1);
                                self.u.tags.truncate(1);
                                self.fn_depth = 0;
                                self.pos = start;
                                self.skip_balanced();
                            }
                        }
                    } else {
                        self.skip_balanced();
                    }
                    self.u.decls.push(Decl::Func(f));
                    return Ok(());
                }
                self.u.decls.push(Decl::Func(f));
            } else {
                let mut init = None;
                if self.eat(Punct::Assign) {
                    if self.at(Punct::LBrace) {
                        self.skip_balanced();
                    } else {
                        let e = self.parse_assign()?;
                        init = self.u.eval(&e).ok();
                    }
                }
                self.u
                    .scopes
                    .last_mut()
                    .unwrap()
                    .insert(name.clone(), Ident::Var(ty.clone()));
                self.u.decls.push(Decl::Var(VarDecl {
                    name,
                    ty,
                    storage: if specs.align.is_some() && storage == Storage::None {
                        Storage::None
                    } else {
                        storage
                    },
                    asm_label: label,
                    init,
                    is_const: specs.is_const,
                    attrs,
                    loc: nloc,
                }));
            }
            if self.eat(Punct::Comma) {
                continue;
            }
            self.expect(Punct::Semi)?;
            return Ok(());
        }
    }
}

fn first_attr_like(p: &Parser) -> bool {
    // `[[` begins a C23 attribute only if it is followed by something other than a constant
    // expression start that makes `a[[0]]`-style nonsense; treat `[[` as an attribute.
    p.at(Punct::LBracket) && p.at_n(1, Punct::LBracket)
}
