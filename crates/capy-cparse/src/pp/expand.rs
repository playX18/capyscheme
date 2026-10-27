//! Macro expansion (Prosser's hide-set algorithm).

use super::{Macro, Preprocessor, Severity};
use crate::lex::lex;
use crate::token::{Kind, Loc, Punct, Token, Word, hide_add, hide_intersect, hide_union};
use std::rc::Rc;

enum Expanded {
    Out(Token),
    Pushed,
}

fn placemarker() -> Token {
    Token::new(Kind::Other, "")
}

/// Tokens from a macro body are attributed to the place the macro was invoked, like a
/// compiler's expansion location; otherwise declarations built from `__pure2` and friends
/// would appear to come from the header that defines those macros.
fn at_call(t: &Token, call: &Token) -> Token {
    let mut t = t.clone();
    t.loc = call.loc;
    t
}

fn is_placemarker(t: &Token) -> bool {
    t.kind == Kind::Other && t.text.is_empty()
}

impl Preprocessor {
    /// Next fully macro-expanded token.
    pub(crate) fn expand_next(&mut self) -> Option<Token> {
        loop {
            let t = self.get()?;
            if t.kind != Kind::Ident {
                return Some(t);
            }
            match self.try_expand(t) {
                Expanded::Out(t) => return Some(t),
                Expanded::Pushed => {}
            }
        }
    }

    /// Fully expands `toks` without reading anything beyond them.
    pub(crate) fn expand_isolated(&mut self, toks: Vec<Token>) -> Vec<Token> {
        self.isolated += 1;
        let base = self.pending.len();
        let saved = std::mem::replace(&mut self.floor, base);
        self.pending.extend(toks.into_iter().rev());
        let mut out = Vec::new();
        while let Some(t) = self.expand_next() {
            out.push(t);
        }
        self.floor = saved;
        self.isolated -= 1;
        out
    }

    fn push_back(&mut self, toks: Vec<Token>) {
        self.pending.extend(toks.into_iter().rev());
    }

    fn try_expand(&mut self, t: Token) -> Expanded {
        if t.hidden(&t.text) {
            return Expanded::Out(t);
        }
        let Some(m) = self.macros.get(&t.text).cloned() else {
            return self.builtin(t);
        };
        match &m.params {
            None => {
                let hs = hide_add(&t.hide, &m.name);
                let out = self.substitute(&m, &[], &hs, &t);
                self.push_back(out);
                Expanded::Pushed
            }
            Some(_) => {
                let Some(next) = self.get() else { return Expanded::Out(t) };
                if !next.is_punct(Punct::LParen) {
                    self.unget(next);
                    return Expanded::Out(t);
                }
                match self.collect_args(&m) {
                    Ok((args, rparen)) => {
                        let hs = hide_add(&hide_intersect(&t.hide, &rparen.hide), &m.name);
                        let out = self.substitute(&m, &args, &hs, &t);
                        self.push_back(out);
                        Expanded::Pushed
                    }
                    Err(e) => {
                        self.diag(Severity::Error, t.loc, format!("{}: {e}", t.text));
                        Expanded::Out(t)
                    }
                }
            }
        }
    }

    fn collect_args(&mut self, m: &Macro) -> Result<(Vec<Vec<Token>>, Token), String> {
        let nparams = m.params.as_ref().map_or(0, |p| p.len());
        let mut args: Vec<Vec<Token>> = vec![Vec::new()];
        let mut depth = 0;
        let rparen = loop {
            let Some(mut t) = self.get() else { return Err("unterminated argument list".into()) };
            if t.is_punct(Punct::LParen) {
                depth += 1;
            } else if t.is_punct(Punct::RParen) {
                if depth == 0 {
                    break t;
                }
                depth -= 1;
            } else if t.is_punct(Punct::Comma) && depth == 0 && !(m.variadic && args.len() == nparams) {
                args.push(Vec::new());
                continue;
            }
            t.bol = false;
            args.last_mut().unwrap().push(t);
        };
        if nparams == 0 {
            if args.len() == 1 && args[0].is_empty() {
                args.clear();
            } else {
                return Err("macro takes no arguments".into());
            }
        } else if args.len() < nparams {
            if m.variadic && args.len() == nparams - 1 {
                args.push(Vec::new());
            } else {
                return Err(format!("expected {nparams} arguments, got {}", args.len()));
            }
        } else if args.len() > nparams {
            return Err(format!("expected {nparams} arguments, got {}", args.len()));
        }
        Ok((args, rparen))
    }

    fn substitute(&mut self, m: &Macro, args: &[Vec<Token>], hs: &crate::token::HideSet, call: &Token) -> Vec<Token> {
        let va_nonempty = m.variadic && args.last().is_some_and(|a| !a.is_empty());
        let body = resolve_va_opt(&m.body, va_nonempty);
        let params: &[Rc<str>] = m.params.as_deref().unwrap_or(&[]);
        let param_idx = |t: &Token| -> Option<usize> {
            if t.kind == Kind::Ident && !params.is_empty() { params.iter().position(|p| *p == t.text) } else { None }
        };
        let mut expanded: Vec<Option<Vec<Token>>> = vec![None; params.len()];
        let mut out: Vec<Token> = Vec::new();
        let mut i = 0;
        while i < body.len() {
            let t = &body[i];
            if m.params.is_some() && t.is_punct(Punct::Hash) {
                if let Some(p) = body.get(i + 1).and_then(&param_idx) {
                    out.push(stringify(&args[p], t.loc));
                    i += 2;
                    continue;
                }
            }
            if t.is_punct(Punct::HashHash) && i > 0 && i + 1 < body.len() {
                let rhs = &body[i + 1];
                let (rhs_toks, rhs_is_va) = match param_idx(rhs) {
                    Some(p) => (args[p].clone(), m.variadic && p == params.len() - 1),
                    None => (vec![at_call(rhs, call)], false),
                };
                if rhs_is_va && body[i - 1].is_punct(Punct::Comma) {
                    // GNU: `, ## __VA_ARGS__` swallows the comma when there are no variadic args.
                    if rhs_toks.is_empty() {
                        out.pop();
                    } else {
                        out.extend(rhs_toks);
                    }
                } else if !rhs_toks.is_empty() {
                    let mut it = rhs_toks.into_iter();
                    let first = it.next().unwrap();
                    match out.pop() {
                        Some(l) if !is_placemarker(&l) => {
                            let pasted = self.paste(l, first);
                            out.extend(pasted);
                        }
                        _ => out.push(first),
                    }
                    out.extend(it);
                }
                i += 2;
                continue;
            }
            if let Some(p) = param_idx(t) {
                if body.get(i + 1).is_some_and(|n| n.is_punct(Punct::HashHash)) {
                    if args[p].is_empty() {
                        out.push(placemarker());
                    } else {
                        out.extend(args[p].iter().cloned());
                    }
                } else {
                    if expanded[p].is_none() {
                        let e = self.expand_isolated(args[p].clone());
                        expanded[p] = Some(e);
                    }
                    out.extend(expanded[p].clone().unwrap());
                }
                i += 1;
                continue;
            }
            out.push(at_call(t, call));
            i += 1;
        }
        out.retain(|t| !is_placemarker(t));
        for t in &mut out {
            t.hide = hide_union(&t.hide, hs);
            t.bol = false;
        }
        if let Some(first) = out.first_mut() {
            first.space = call.space;
        }
        out
    }

    fn paste(&mut self, l: Token, r: Token) -> Vec<Token> {
        let text = format!("{}{}", l.text, r.text);
        let mut toks = lex(&text, l.loc.file);
        if toks.len() == 1 {
            let mut t = toks.pop().unwrap();
            t.loc = l.loc;
            t.space = l.space;
            t.bol = false;
            t.hide = hide_intersect(&l.hide, &r.hide);
            vec![t]
        } else {
            self.diag(Severity::Error, l.loc, format!("pasting \"{}\" and \"{}\" does not give a valid token", l.text, r.text));
            vec![l, r]
        }
    }

    fn builtin(&mut self, t: Token) -> Expanded {
        let mk = |kind: Kind, text: String| {
            let mut n = Token::new(kind, text);
            n.loc = t.loc;
            n.space = t.space;
            n
        };
        let Some(word) = t.word else { return Expanded::Out(t) };
        let out = match word {
            Word::Line => mk(Kind::Number, t.loc.line.to_string()),
            Word::File | Word::BaseFile => {
                let name = self.file_name(t.loc.file).display().to_string();
                mk(Kind::Str, format!("\"{}\"", name.replace('\\', "\\\\").replace('"', "\\\"")))
            }
            Word::Counter => {
                self.counter += 1;
                mk(Kind::Number, (self.counter - 1).to_string())
            }
            Word::IncludeLevel => mk(Kind::Number, self.frames.len().saturating_sub(1).to_string()),
            Word::Date => mk(Kind::Str, "\"Jan  1 1970\"".into()),
            Word::Time => mk(Kind::Str, "\"00:00:00\"".into()),
            Word::PragmaOperator => return self.pragma_operator(t),
            _ => return Expanded::Out(t),
        };
        Expanded::Out(out)
    }

    fn pragma_operator(&mut self, t: Token) -> Expanded {
        let open = self.get();
        let s = self.get();
        let close = self.get();
        match (open, s, close) {
            (Some(o), Some(s), Some(c)) if o.is_punct(Punct::LParen) && s.kind == Kind::Str && c.is_punct(Punct::RParen) => {
                let inner = s.text.trim_start_matches(|c| c != '"').trim_matches('"').replace("\\\"", "\"").replace("\\\\", "\\");
                let toks = lex(&inner, t.loc.file);
                self.do_pragma(&toks, t.loc);
                Expanded::Pushed
            }
            (o, s, c) => {
                for x in [c, s, o].into_iter().flatten() {
                    self.unget(x);
                }
                Expanded::Out(t)
            }
        }
    }
}

fn resolve_va_opt(body: &[Token], va_nonempty: bool) -> Vec<Token> {
    if !body.iter().any(|t| t.is_word(Word::VaOpt)) {
        return body.to_vec();
    }
    let mut out = Vec::new();
    let mut i = 0;
    while i < body.len() {
        if body[i].is_word(Word::VaOpt) && body.get(i + 1).is_some_and(|t| t.is_punct(Punct::LParen)) {
            let mut depth = 0;
            let mut j = i + 1;
            while j < body.len() {
                if body[j].is_punct(Punct::LParen) {
                    depth += 1;
                } else if body[j].is_punct(Punct::RParen) {
                    depth -= 1;
                    if depth == 0 {
                        break;
                    }
                }
                j += 1;
            }
            if va_nonempty {
                out.extend(resolve_va_opt(&body[i + 2..j.min(body.len())], va_nonempty));
            }
            i = j + 1;
        } else {
            out.push(body[i].clone());
            i += 1;
        }
    }
    out
}

fn stringify(arg: &[Token], loc: Loc) -> Token {
    let mut s = String::from("\"");
    for (i, t) in arg.iter().enumerate() {
        if i > 0 && t.space {
            s.push(' ');
        }
        if matches!(t.kind, Kind::Str | Kind::Char) {
            for c in t.text.chars() {
                if c == '"' || c == '\\' {
                    s.push('\\');
                }
                s.push(c);
            }
        } else {
            s.push_str(&t.text);
        }
    }
    s.push('"');
    let mut t = Token::new(Kind::Str, s);
    t.loc = loc;
    t
}
