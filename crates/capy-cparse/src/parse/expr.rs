//! Expression parsing.

use super::{PR, Parser};
use crate::ast::*;
use crate::token::{Kind, Punct, Word};
use crate::types::*;
use std::rc::Rc;

impl Parser {
    pub(crate) fn parse_expr(&mut self) -> PR<Expr> {
        let mut e = self.parse_assign()?;
        while self.eat(Punct::Comma) {
            let r = self.parse_assign()?;
            e = Expr::Comma(Box::new(e), Box::new(r));
        }
        Ok(e)
    }

    pub(crate) fn parse_assign(&mut self) -> PR<Expr> {
        let lhs = self.parse_cond()?;
        if let Some(op) = self.tok().punct().and_then(BinOp::from_assign) {
            self.bump();
            let rhs = self.parse_assign()?;
            return Ok(Expr::Assign(op, Box::new(lhs), Box::new(rhs)));
        }
        Ok(lhs)
    }

    fn parse_cond(&mut self) -> PR<Expr> {
        let c = self.parse_binary(0)?;
        if !self.eat(Punct::Question) {
            return Ok(c);
        }
        let a = if self.at(Punct::Colon) { c.clone() } else { self.parse_expr()? };
        self.expect(Punct::Colon)?;
        let b = self.parse_cond()?;
        Ok(Expr::Cond(Box::new(c), Box::new(a), Box::new(b)))
    }

    fn parse_binary(&mut self, min: u8) -> PR<Expr> {
        let mut lhs = self.parse_cast()?;
        loop {
            let Some((op, p)) = self.tok().punct().and_then(BinOp::from_punct) else { break };
            if p <= min {
                break;
            }
            self.bump();
            let rhs = self.parse_binary(p)?;
            lhs = Expr::Binary(op, Box::new(lhs), Box::new(rhs));
        }
        Ok(lhs)
    }

    fn parse_cast(&mut self) -> PR<Expr> {
        if self.at(Punct::LParen) && self.is_type_start_at(1) {
            let save = self.pos;
            self.bump();
            match self.type_name() {
                Ok(ty) if self.eat(Punct::RParen) => {
                    if self.at(Punct::LBrace) {
                        let init = self.init_list()?;
                        return self.postfix_tail(Expr::CompoundLit(ty, init));
                    }
                    let inner = self.parse_cast()?;
                    return Ok(Expr::Cast(ty, Box::new(inner)));
                }
                _ => self.pos = save,
            }
        }
        self.parse_unary()
    }

    fn parse_unary(&mut self) -> PR<Expr> {
        let t = self.tok().clone();
        match t.punct() {
            Some(p @ (Punct::Inc | Punct::Dec)) => {
                self.bump();
                let e = self.parse_unary()?;
                let op = if p == Punct::Inc { UnOp::PreInc } else { UnOp::PreDec };
                return Ok(Expr::Unary(op, Box::new(e)));
            }
            Some(p @ (Punct::Amp | Punct::Star | Punct::Plus | Punct::Minus | Punct::Tilde | Punct::Bang)) => {
                self.bump();
                let e = self.parse_cast()?;
                let op = match p {
                    Punct::Amp => UnOp::AddrOf,
                    Punct::Star => UnOp::Deref,
                    Punct::Plus => UnOp::Plus,
                    Punct::Minus => UnOp::Neg,
                    Punct::Tilde => UnOp::BitNot,
                    _ => UnOp::Not,
                };
                return Ok(Expr::Unary(op, Box::new(e)));
            }
            Some(Punct::AmpAmp) => {
                self.bump();
                self.ident()?;
                return Ok(Expr::Unsupported("address of label".into()));
            }
            _ => {}
        }
        if t.kind == Kind::Ident {
            match t.word {
                Some(Word::Sizeof) => {
                    self.bump();
                    if self.at(Punct::LParen) && self.is_type_start_at(1) {
                        let save = self.pos;
                        self.bump();
                        if let Ok(ty) = self.type_name() {
                            if self.eat(Punct::RParen) && !self.at(Punct::LBrace) {
                                return Ok(Expr::SizeofType(ty));
                            }
                        }
                        self.pos = save;
                    }
                    let e = self.parse_unary()?;
                    return Ok(Expr::SizeofExpr(Box::new(e)));
                }
                Some(Word::Alignof) => {
                    self.bump();
                    if self.at(Punct::LParen) && self.is_type_start_at(1) {
                        self.bump();
                        let ty = self.type_name()?;
                        self.expect(Punct::RParen)?;
                        return Ok(Expr::AlignofType(ty));
                    }
                    let e = self.parse_unary()?;
                    return Ok(match self.u.type_of(&e) {
                        Some(t) => Expr::AlignofType(t),
                        None => Expr::Unsupported("alignof of untypable expression".into()),
                    });
                }
                Some(Word::Extension) => {
                    self.bump();
                    return self.parse_cast();
                }
                _ => {}
            }
        }
        self.parse_postfix()
    }

    fn parse_postfix(&mut self) -> PR<Expr> {
        let e = self.parse_primary()?;
        self.postfix_tail(e)
    }

    fn postfix_tail(&mut self, mut e: Expr) -> PR<Expr> {
        loop {
            if self.eat(Punct::LBracket) {
                let i = self.parse_expr()?;
                self.expect(Punct::RBracket)?;
                e = Expr::Index(Box::new(e), Box::new(i));
            } else if self.eat(Punct::LParen) {
                let mut args = Vec::new();
                if !self.eat(Punct::RParen) {
                    loop {
                        args.push(self.parse_assign()?);
                        if !self.eat(Punct::Comma) {
                            break;
                        }
                    }
                    self.expect(Punct::RParen)?;
                }
                e = Expr::Call(Box::new(e), args);
            } else if self.eat(Punct::Dot) {
                let n = self.ident()?;
                e = Expr::Member(Box::new(e), n);
            } else if self.eat(Punct::Arrow) {
                let n = self.ident()?;
                e = Expr::Arrow(Box::new(e), n);
            } else if self.at(Punct::Inc) || self.at(Punct::Dec) {
                let op = if self.bump().is_punct(Punct::Inc) { IncDec::Inc } else { IncDec::Dec };
                e = Expr::PostInc(op, Box::new(e));
            } else {
                return Ok(e);
            }
        }
    }

    fn parse_primary(&mut self) -> PR<Expr> {
        let t = self.tok().clone();
        match t.kind {
            Kind::Number => {
                self.bump();
                match self.number_literal(&t.text) {
                    Ok(e) => Ok(e),
                    Err(m) => self.err(m),
                }
            }
            Kind::Char => {
                self.bump();
                Ok(self.char_literal(&t.text))
            }
            Kind::Str => {
                let mut bytes = Vec::new();
                while self.tok().kind == Kind::Str {
                    let s = self.bump();
                    bytes.extend(decode_string(&s.text));
                }
                Ok(Expr::Str(bytes))
            }
            Kind::Punct(Punct::LParen) => {
                self.bump();
                if self.at(Punct::LBrace) {
                    let stmts = self.block_stmts()?;
                    self.expect(Punct::RParen)?;
                    return Ok(Expr::StmtExpr(stmts));
                }
                let e = self.parse_expr()?;
                self.expect(Punct::RParen)?;
                Ok(e)
            }
            Kind::Ident => self.ident_primary(),
            _ => self.err(format!("unexpected '{}' in expression", t.text)),
        }
    }

    fn ident_primary(&mut self) -> PR<Expr> {
        let t = self.bump();
        // A real variable or constant shadows any keyword-like meaning.
        if self.u.lookup(&t.text).is_none() {
            match t.word {
                Some(Word::True) => return Ok(Expr::Bool(true)),
                Some(Word::False) => return Ok(Expr::Bool(false)),
                Some(Word::Nullptr) => return Ok(Expr::Nullptr),
                Some(Word::FuncName) => return Ok(Expr::Str(Vec::new())),
                _ => {}
            }
        }
        match t.word {
            Some(Word::Generic) => {
                self.expect(Punct::LParen)?;
                let ctrl = self.parse_assign()?;
                let mut arms = Vec::new();
                while self.eat(Punct::Comma) {
                    if self.eat_word(Word::Default) {
                        self.expect(Punct::Colon)?;
                        arms.push((None, self.parse_assign()?));
                    } else {
                        let ty = self.type_name()?;
                        self.expect(Punct::Colon)?;
                        arms.push((Some(ty), self.parse_assign()?));
                    }
                }
                self.expect(Punct::RParen)?;
                Ok(Expr::Generic(Box::new(ctrl), arms))
            }
            Some(Word::BuiltinOffsetof) if self.at(Punct::LParen) => {
                self.bump();
                let ty = self.type_name()?;
                self.expect(Punct::Comma)?;
                let mut steps = vec![OffsetStep::Field(self.ident()?)];
                loop {
                    if self.eat(Punct::Dot) {
                        steps.push(OffsetStep::Field(self.ident()?));
                    } else if self.eat(Punct::LBracket) {
                        steps.push(OffsetStep::Index(self.parse_expr()?));
                        self.expect(Punct::RBracket)?;
                    } else {
                        break;
                    }
                }
                self.expect(Punct::RParen)?;
                Ok(Expr::OffsetOf(ty, steps))
            }
            Some(Word::BuiltinVaArg) => {
                self.expect(Punct::LParen)?;
                self.parse_assign()?;
                self.expect(Punct::Comma)?;
                self.type_name()?;
                self.expect(Punct::RParen)?;
                Ok(Expr::Unsupported("va_arg".into()))
            }
            Some(Word::BuiltinTypesCompatible) => {
                self.expect(Punct::LParen)?;
                let a = self.type_name()?;
                self.expect(Punct::Comma)?;
                let b = self.type_name()?;
                self.expect(Punct::RParen)?;
                Ok(Expr::Int(self.u.types_compatible(&a, &b) as i128, IntKind::Int))
            }
            _ => {
                if self.fn_depth > 0 {
                    let ty = match self.u.lookup(&t.text) {
                        Some(Ident::Var(ty)) | Some(Ident::Func(ty)) => Some(ty.clone()),
                        _ => None,
                    };
                    if let Some(ty) = ty {
                        return Ok(Expr::Typed(ty, Box::new(Expr::Ident(t.text))));
                    }
                }
                Ok(Expr::Ident(t.text))
            }
        }
    }

    /// `{ ... }` initializer list; the cursor is on `{`.
    pub(crate) fn init_list(&mut self) -> PR<Vec<(Vec<Designator>, Init)>> {
        self.expect(Punct::LBrace)?;
        let mut items = Vec::new();
        loop {
            if self.eat(Punct::RBrace) {
                return Ok(items);
            }
            let mut desig = Vec::new();
            loop {
                if self.eat(Punct::Dot) {
                    desig.push(Designator::Field(self.ident()?));
                } else if self.at(Punct::LBracket) {
                    self.bump();
                    let i = self.parse_assign()?;
                    if self.eat(Punct::Ellipsis) {
                        self.parse_assign()?;
                    }
                    self.expect(Punct::RBracket)?;
                    desig.push(Designator::Index(i));
                } else {
                    break;
                }
            }
            if !desig.is_empty() {
                self.expect(Punct::Assign)?;
            }
            let v = if self.at(Punct::LBrace) { Init::List(self.init_list()?) } else { Init::Expr(self.parse_assign()?) };
            items.push((desig, v));
            if !self.eat(Punct::Comma) {
                self.expect(Punct::RBrace)?;
                return Ok(items);
            }
        }
    }

    pub(crate) fn number_literal(&self, text: &str) -> Result<Expr, String> {
        let s: String = text.chars().filter(|&c| c != '\'').collect();
        let lower = s.to_ascii_lowercase();
        let is_hex = lower.starts_with("0x");
        let is_float = if is_hex { lower.contains('p') || (lower.contains('.')) } else { lower.contains('.') || lower.contains('e') };
        if is_float {
            let (body, kind) = if let Some(b) = lower.strip_suffix("f128") {
                (b, FloatKind::Float128)
            } else if let Some(b) = lower.strip_suffix("f16") {
                (b, FloatKind::Half)
            } else if let Some(b) = lower.strip_suffix("f64") {
                (b, FloatKind::Double)
            } else if let Some(b) = lower.strip_suffix("f32") {
                (b, FloatKind::Float)
            } else if let Some(b) = lower.strip_suffix('f') {
                if is_hex && !lower.contains('p') { (lower.as_str(), FloatKind::Double) } else { (b, FloatKind::Float) }
            } else if let Some(b) = lower.strip_suffix('l') {
                (b, FloatKind::LongDouble)
            } else {
                (lower.as_str(), FloatKind::Double)
            };
            let v = if is_hex { parse_hex_float(body)? } else { body.parse::<f64>().map_err(|_| format!("bad floating constant '{text}'"))? };
            let v = if kind == FloatKind::Float { v as f32 as f64 } else { v };
            return Ok(Expr::Float(v, kind));
        }
        let body = lower.trim_end_matches(['u', 'l']);
        let suffix = &lower[body.len()..];
        if lower.ends_with("wb") {
            return Ok(Expr::Unsupported("_BitInt literal".into()));
        }
        let unsigned_suffix = suffix.contains('u');
        let longs = suffix.matches('l').count();
        let (digits, radix) = if let Some(h) = body.strip_prefix("0x") {
            (h, 16)
        } else if let Some(b) = body.strip_prefix("0b") {
            (b, 2)
        } else if body.len() > 1 && body.starts_with('0') {
            (&body[1..], 8)
        } else {
            (body, 10)
        };
        let v = u128::from_str_radix(digits, radix).map_err(|_| format!("bad integer constant '{text}'"))?;
        let decimal = radix == 10;
        let abi = &self.u.abi;
        let fits = |k: IntKind| {
            let bits = abi.int_size(k) * 8;
            if abi.is_unsigned(k) { bits >= 128 || v < (1u128 << bits) } else { bits >= 128 || v < (1u128 << (bits - 1)) }
        };
        let candidates: &[IntKind] = match (unsigned_suffix, longs, decimal) {
            (false, 0, true) => &[IntKind::Int, IntKind::Long, IntKind::LongLong],
            (false, 0, false) => &[IntKind::Int, IntKind::UInt, IntKind::Long, IntKind::ULong, IntKind::LongLong, IntKind::ULongLong],
            (true, 0, _) => &[IntKind::UInt, IntKind::ULong, IntKind::ULongLong],
            (false, 1, true) => &[IntKind::Long, IntKind::LongLong],
            (false, 1, false) => &[IntKind::Long, IntKind::ULong, IntKind::LongLong, IntKind::ULongLong],
            (true, 1, _) => &[IntKind::ULong, IntKind::ULongLong],
            (false, _, true) => &[IntKind::LongLong],
            (false, _, false) => &[IntKind::LongLong, IntKind::ULongLong],
            (true, _, _) => &[IntKind::ULongLong],
        };
        let k = candidates
            .iter()
            .copied()
            .find(|k| fits(*k))
            .unwrap_or(if v > u64::MAX as u128 { IntKind::UInt128 } else { IntKind::ULongLong });
        Ok(Expr::Int(v as i128, k))
    }

    fn char_literal(&self, text: &str) -> Expr {
        let (prefix, rest) = text.split_at(text.find('\'').unwrap_or(0));
        let inner = rest.trim_matches('\'');
        let vals = decode_chars(inner);
        let kind = match prefix {
            "L" => if self.u.abi.wchar_signed { IntKind::Int } else { IntKind::UInt },
            "u" => IntKind::UShort,
            "U" => IntKind::UInt,
            "u8" => IntKind::UChar,
            _ => IntKind::Int,
        };
        let mut v: i128 = 0;
        if prefix.is_empty() && vals.len() > 1 {
            for c in &vals {
                v = (v << 8) | (*c as i128 & 0xff);
            }
            v = self.u.wrap(v, IntKind::Int);
        } else if let Some(c) = vals.first() {
            v = *c as i128;
            if prefix.is_empty() && v < 256 && self.u.abi.char_signed {
                v = v as u8 as i8 as i128;
            }
        }
        Expr::Char(v, kind)
    }
}

fn parse_hex_float(s: &str) -> Result<f64, String> {
    let s = s.trim_start_matches("0x");
    let (mant, exp) = s.split_once('p').unwrap_or((s, "0"));
    let (ip, fp) = mant.split_once('.').unwrap_or((mant, ""));
    let mut v = 0f64;
    for c in ip.chars() {
        v = v * 16.0 + c.to_digit(16).ok_or("bad hex float")? as f64;
    }
    let mut scale = 1.0 / 16.0;
    for c in fp.chars() {
        v += c.to_digit(16).ok_or("bad hex float")? as f64 * scale;
        scale /= 16.0;
    }
    let e: i32 = exp.parse().map_err(|_| "bad hex float exponent")?;
    Ok(v * 2f64.powi(e))
}

/// Decodes the escapes of a (prefix-stripped, unquoted) character sequence into code points.
fn decode_chars(inner: &str) -> Vec<u32> {
    let mut out = Vec::new();
    let mut it = inner.chars().peekable();
    while let Some(c) = it.next() {
        if c != '\\' {
            out.push(c as u32);
            continue;
        }
        let v = match it.next() {
            Some('n') => 10,
            Some('t') => 9,
            Some('r') => 13,
            Some('a') => 7,
            Some('b') => 8,
            Some('f') => 12,
            Some('v') => 11,
            Some('e') | Some('E') => 27,
            Some('x') => {
                let mut h = 0u32;
                while let Some(d) = it.peek().and_then(|c| c.to_digit(16)) {
                    h = h.wrapping_mul(16).wrapping_add(d);
                    it.next();
                }
                h
            }
            Some(u @ ('u' | 'U')) => {
                let n = if u == 'u' { 4 } else { 8 };
                let mut h = 0u32;
                for _ in 0..n {
                    if let Some(d) = it.peek().and_then(|c| c.to_digit(16)) {
                        h = h * 16 + d;
                        it.next();
                    }
                }
                h
            }
            Some(d @ '0'..='7') => {
                let mut o = d.to_digit(8).unwrap();
                for _ in 0..2 {
                    if let Some(d) = it.peek().and_then(|c| c.to_digit(8)) {
                        o = o * 8 + d;
                        it.next();
                    }
                }
                o
            }
            Some(other) => other as u32,
            None => 0,
        };
        out.push(v);
    }
    out
}

fn decode_string(text: &str) -> Vec<u8> {
    let start = text.find('"').unwrap_or(0);
    let inner = text[start..].trim_matches('"');
    let mut out = Vec::new();
    let mut it = inner.chars().peekable();
    while let Some(c) = it.next() {
        if c != '\\' {
            let mut b = [0; 4];
            out.extend_from_slice(c.encode_utf8(&mut b).as_bytes());
            continue;
        }
        match it.next() {
            Some('x') => {
                let mut h = 0u32;
                while let Some(d) = it.peek().and_then(|c| c.to_digit(16)) {
                    h = h.wrapping_mul(16).wrapping_add(d);
                    it.next();
                }
                out.push(h as u8);
            }
            Some(d @ '0'..='7') => {
                let mut o = d.to_digit(8).unwrap();
                for _ in 0..2 {
                    if let Some(d) = it.peek().and_then(|c| c.to_digit(8)) {
                        o = o * 8 + d;
                        it.next();
                    }
                }
                out.push(o as u8);
            }
            Some(u @ ('u' | 'U')) => {
                let n = if u == 'u' { 4 } else { 8 };
                let mut h = 0u32;
                for _ in 0..n {
                    if let Some(d) = it.peek().and_then(|c| c.to_digit(16)) {
                        h = h * 16 + d;
                        it.next();
                    }
                }
                if let Some(ch) = char::from_u32(h) {
                    let mut b = [0; 4];
                    out.extend_from_slice(ch.encode_utf8(&mut b).as_bytes());
                }
            }
            Some(e) => {
                let v = match e {
                    'n' => 10,
                    't' => 9,
                    'r' => 13,
                    'a' => 7,
                    'b' => 8,
                    'f' => 12,
                    'v' => 11,
                    'e' | 'E' => 27,
                    other => other as u32,
                };
                out.push(v as u8);
            }
            None => {}
        }
    }
    out
}

#[allow(dead_code)]
fn _unused(_: Rc<str>) {}
