//! `#if` expression evaluation and `#embed`.

use super::{Preprocessor, join};
use crate::ast::BinOp;
use crate::token::{Kind, Loc, Punct, Token, Word};

fn num(v: i64) -> Token {
    Token::new(Kind::Number, v.to_string())
}

/// Index of the `)` matching the `(` at `open`.
fn matching(toks: &[Token], open: usize) -> Option<usize> {
    let mut depth = 0;
    for (i, t) in toks.iter().enumerate().skip(open) {
        if t.is_punct(Punct::LParen) {
            depth += 1;
        } else if t.is_punct(Punct::RParen) {
            depth -= 1;
            if depth == 0 {
                return Some(i);
            }
        }
    }
    None
}

impl Preprocessor {
    /// Replaces `defined`, `__has_include` and the feature builtins by their values.
    fn resolve_operators(&mut self, toks: &[Token]) -> Result<Vec<Token>, String> {
        let mut out = Vec::new();
        let mut i = 0;
        while i < toks.len() {
            let t = &toks[i];
            let Some(word) = t.word.filter(|_| t.kind == Kind::Ident) else {
                out.push(t.clone());
                i += 1;
                continue;
            };
            let call = toks.get(i + 1).is_some_and(|t| t.is_punct(Punct::LParen));
            match word {
                Word::Defined => {
                    let (n, next) = if call {
                        let close = matching(toks, i + 1).ok_or("unterminated defined(")?;
                        (toks.get(i + 2), close + 1)
                    } else {
                        (toks.get(i + 1), i + 2)
                    };
                    let n = n.ok_or("defined without a name")?;
                    let builtin = n.word.is_some_and(Word::is_pp_builtin);
                    out.push(num((builtin || self.is_defined(&n.text)) as i64));
                    i = next;
                }
                Word::HasInclude | Word::HasIncludeNext | Word::HasEmbed if call => {
                    let close = matching(toks, i + 1).ok_or("unterminated __has_include(")?;
                    let inner = &toks[i + 2..close];
                    let found = match self.header_name(inner) {
                        Some((h, quoted)) => {
                            let r = self.resolve_include(&h, quoted, word == Word::HasIncludeNext);
                            match (word, r) {
                                (Word::HasEmbed, Some((p, _))) => {
                                    if std::fs::metadata(&p).map_or(true, |m| m.len() == 0) { 2 } else { 1 }
                                }
                                (_, r) => r.is_some() as i64,
                            }
                        }
                        None => 0,
                    };
                    out.push(num(found));
                    i = close + 1;
                }
                Word::Query(q) if call => {
                    let close = matching(toks, i + 1).ok_or("unterminated feature query")?;
                    let arg = join(&toks[i + 2..close]);
                    let v = self.oracle.query(q, &arg);
                    out.push(num(v));
                    i = close + 1;
                }
                _ => {
                    out.push(t.clone());
                    i += 1;
                }
            }
        }
        Ok(out)
    }

    pub(crate) fn eval_if(&mut self, toks: &[Token], _loc: Loc) -> Result<i64, String> {
        if toks.is_empty() {
            return Err("#if with no expression".into());
        }
        let pre = self.resolve_operators(toks)?;
        let expanded = self.expand_isolated(pre);
        let resolved = self.resolve_operators(&expanded)?;
        let mut p = Eval { toks: &resolved, i: 0 };
        let v = p.comma(true)?;
        if p.i < resolved.len() {
            return Err(format!("unexpected token '{}'", resolved[p.i].text));
        }
        Ok(v.bits as i64)
    }

    pub(crate) fn read_embed(&mut self, rest: &[Token], _loc: Loc) -> Result<Vec<Token>, String> {
        let hdr_end = if rest.first().is_some_and(|t| t.is_punct(Punct::Lt)) {
            rest.iter().position(|t| t.is_punct(Punct::Gt)).map_or(rest.len(), |p| p + 1)
        } else {
            1.min(rest.len())
        };
        let (name, quoted) = self.header_name(&rest[..hdr_end]).ok_or("malformed resource name")?;
        let (path, _) = self.resolve_include(&name, quoted, false).ok_or_else(|| format!("'{name}' file not found"))?;
        let mut limit = usize::MAX;
        let (mut prefix, mut suffix, mut if_empty): (Vec<Token>, Vec<Token>, Vec<Token>) = Default::default();
        let mut i = hdr_end;
        while i < rest.len() {
            let pname = rest[i].text.trim_matches('_').to_string();
            let args = if rest.get(i + 1).is_some_and(|t| t.is_punct(Punct::LParen)) {
                let close = matching(rest, i + 1).ok_or("unterminated embed parameter")?;
                let a = rest[i + 2..close].to_vec();
                i = close + 1;
                a
            } else {
                i += 1;
                Vec::new()
            };
            match pname.as_str() {
                "limit" => {
                    let e = self.expand_isolated(args);
                    limit = self.eval_if(&e, Loc::default())? as usize;
                }
                "prefix" => prefix = args,
                "suffix" => suffix = args,
                "if_empty" => if_empty = args,
                _ => {}
            }
        }
        self.deps.push(path.clone());
        let mut bytes = std::fs::read(&path).map_err(|e| e.to_string())?;
        bytes.truncate(limit);
        if bytes.is_empty() {
            return Ok(if_empty);
        }
        let mut out = prefix;
        for (n, b) in bytes.iter().enumerate() {
            if n > 0 {
                out.push(Token::punct_tok(Punct::Comma));
            }
            out.push(num(*b as i64));
        }
        out.extend(suffix);
        Ok(out)
    }
}

#[derive(Clone, Copy)]
struct Val {
    bits: u64,
    unsigned: bool,
}

impl Val {
    fn s(v: i64) -> Val {
        Val { bits: v as u64, unsigned: false }
    }
    fn b(v: bool) -> Val {
        Val::s(v as i64)
    }
    fn truthy(self) -> bool {
        self.bits != 0
    }
}

struct Eval<'a> {
    toks: &'a [Token],
    i: usize,
}

type R = Result<Val, String>;

impl Eval<'_> {
    fn peek(&self) -> Option<Punct> {
        self.toks.get(self.i).and_then(Token::punct)
    }

    fn eat(&mut self, p: Punct) -> bool {
        if self.peek() == Some(p) {
            self.i += 1;
            true
        } else {
            false
        }
    }

    fn comma(&mut self, ev: bool) -> R {
        let mut v = self.cond(ev)?;
        while self.eat(Punct::Comma) {
            v = self.cond(ev)?;
        }
        Ok(v)
    }

    fn cond(&mut self, ev: bool) -> R {
        let c = self.binary(0, ev)?;
        if self.eat(Punct::Question) {
            let a = self.comma(ev && c.truthy())?;
            if !self.eat(Punct::Colon) {
                return Err("expected ':'".into());
            }
            let b = self.cond(ev && !c.truthy())?;
            let unsigned = a.unsigned || b.unsigned;
            let r = if c.truthy() { a } else { b };
            return Ok(Val { bits: r.bits, unsigned });
        }
        Ok(c)
    }

    fn binary(&mut self, min: u8, ev: bool) -> R {
        let mut lhs = self.unary(ev)?;
        while let Some((op, p)) = self.peek().and_then(BinOp::from_punct) {
            if p <= min {
                break;
            }
            self.i += 1;
            let short = match op {
                BinOp::LAnd => !lhs.truthy(),
                BinOp::LOr => lhs.truthy(),
                _ => false,
            };
            let rhs = self.binary(p, ev && !short)?;
            lhs = apply(op, lhs, rhs, ev && !short)?;
        }
        Ok(lhs)
    }

    fn unary(&mut self, ev: bool) -> R {
        let Some(t) = self.toks.get(self.i) else { return Err("unexpected end of expression".into()) };
        self.i += 1;
        match t.kind {
            Kind::Number => parse_int(&t.text),
            Kind::Char => parse_char(&t.text),
            Kind::Ident => Ok(Val::s(t.is_word(Word::True) as i64)),
            Kind::Punct(Punct::LParen) => {
                let v = self.comma(ev)?;
                if !self.eat(Punct::RParen) {
                    return Err("expected ')'".into());
                }
                Ok(v)
            }
            Kind::Punct(Punct::Plus) => self.unary(ev),
            Kind::Punct(Punct::Minus) => {
                let v = self.unary(ev)?;
                Ok(Val { bits: v.bits.wrapping_neg(), unsigned: v.unsigned })
            }
            Kind::Punct(Punct::Tilde) => {
                let v = self.unary(ev)?;
                Ok(Val { bits: !v.bits, unsigned: v.unsigned })
            }
            Kind::Punct(Punct::Bang) => Ok(Val::b(!self.unary(ev)?.truthy())),
            _ => Err(format!("unexpected token '{}'", t.text)),
        }
    }
}

fn apply(op: BinOp, a: Val, b: Val, ev: bool) -> R {
    use BinOp::*;
    let unsigned = a.unsigned || b.unsigned;
    let (x, y) = (a.bits, b.bits);
    let cmp = |f: fn(i64, i64) -> bool, g: fn(u64, u64) -> bool| Val::b(if unsigned { g(x, y) } else { f(x as i64, y as i64) });
    Ok(match op {
        LOr => Val::b(a.truthy() || b.truthy()),
        LAnd => Val::b(a.truthy() && b.truthy()),
        BitOr => Val { bits: x | y, unsigned },
        BitXor => Val { bits: x ^ y, unsigned },
        BitAnd => Val { bits: x & y, unsigned },
        Eq => Val::b(x == y),
        Ne => Val::b(x != y),
        Lt => cmp(|p, q| p < q, |p, q| p < q),
        Gt => cmp(|p, q| p > q, |p, q| p > q),
        Le => cmp(|p, q| p <= q, |p, q| p <= q),
        Ge => cmp(|p, q| p >= q, |p, q| p >= q),
        Shl => {
            if (b.unsigned || (y as i64) >= 0) && y < 64 {
                Val { bits: x << y, unsigned: a.unsigned }
            } else {
                Val { bits: 0, unsigned: a.unsigned }
            }
        }
        Shr => {
            let v = if y >= 64 {
                if !a.unsigned && (x as i64) < 0 { u64::MAX } else { 0 }
            } else if a.unsigned {
                x >> y
            } else {
                ((x as i64) >> y) as u64
            };
            Val { bits: v, unsigned: a.unsigned }
        }
        Add => Val { bits: x.wrapping_add(y), unsigned },
        Sub => Val { bits: x.wrapping_sub(y), unsigned },
        Mul => Val { bits: x.wrapping_mul(y), unsigned },
        Div | Rem => {
            if y == 0 {
                if ev {
                    return Err("division by zero".into());
                }
                return Ok(Val::s(0));
            }
            let bits = match (op, unsigned) {
                (Div, true) => x / y,
                (_, true) => x % y,
                (Div, false) => (x as i64).wrapping_div(y as i64) as u64,
                (_, false) => (x as i64).wrapping_rem(y as i64) as u64,
            };
            Val { bits, unsigned }
        }
    })
}

fn parse_int(text: &str) -> R {
    let s: String = text.chars().filter(|&c| c != '\'').collect();
    let lower = s.to_ascii_lowercase();
    let body = lower.trim_end_matches(['u', 'l']);
    let suffix_unsigned = lower[body.len()..].contains('u');
    let (digits, radix) = if let Some(h) = body.strip_prefix("0x") {
        (h, 16)
    } else if let Some(b) = body.strip_prefix("0b") {
        (b, 2)
    } else if body.len() > 1 && body.starts_with('0') {
        (&body[1..], 8)
    } else {
        (body, 10)
    };
    let v = u128::from_str_radix(digits, radix).map_err(|_| format!("invalid integer constant '{text}'"))?;
    if v > u64::MAX as u128 {
        return Err(format!("integer constant '{text}' is too large"));
    }
    // Unsuffixed decimal values that overflow intmax_t are treated as unsigned too.
    Ok(Val { bits: v as u64, unsigned: suffix_unsigned || v > i64::MAX as u128 })
}

fn parse_char(text: &str) -> R {
    let body = text.trim_start_matches(|c| c != '\'');
    let inner = body.strip_prefix('\'').and_then(|b| b.strip_suffix('\'')).ok_or("bad character constant")?;
    let mut chars = inner.chars().peekable();
    let mut value: i64 = 0;
    let mut n = 0;
    while let Some(c) = chars.next() {
        let v: u32 = if c == '\\' {
            match chars.next() {
                Some('n') => 10,
                Some('t') => 9,
                Some('r') => 13,
                Some('a') => 7,
                Some('b') => 8,
                Some('f') => 12,
                Some('v') => 11,
                Some('x') => {
                    let mut h = 0;
                    while let Some(d) = chars.peek().and_then(|c| c.to_digit(16)) {
                        h = h * 16 + d;
                        chars.next();
                    }
                    h
                }
                Some(d @ '0'..='7') => {
                    let mut o = d.to_digit(8).unwrap();
                    for _ in 0..2 {
                        if let Some(d) = chars.peek().and_then(|c| c.to_digit(8)) {
                            o = o * 8 + d;
                            chars.next();
                        }
                    }
                    o
                }
                Some(other) => other as u32,
                None => 0,
            }
        } else {
            c as u32
        };
        value = if v > 0xff { v as i64 } else { (value << 8) | v as i64 };
        n += 1;
    }
    // Plain `char` constants are signed on all supported hosts.
    if n == 1 && !text.starts_with(['L', 'u', 'U']) {
        value = value as i8 as i64;
    }
    Ok(Val::s(value))
}
