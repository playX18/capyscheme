//! The C preprocessor
mod cond;
mod expand;
pub mod oracle;

use crate::lex::lex;
use crate::token::{Kind, Loc, Punct, Token};
use oracle::{FeatureOracle, StaticOracle};
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::rc::Rc;

#[derive(Clone, Debug)]
pub struct Macro {
    pub name: Rc<str>,
    /// `None` for object-like macros.
    pub params: Option<Vec<Rc<str>>>,
    pub variadic: bool,
    pub body: Vec<Token>,
    pub loc: Loc,
    /// Defined in a file under a system include directory (or predefined).
    pub from_system: bool,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Severity {
    Note,
    Warning,
    Error,
}

#[derive(Clone, Debug)]
pub struct Diag {
    pub severity: Severity,
    pub message: String,
    pub file: String,
    pub line: u32,
}

#[derive(Clone, Default)]
pub struct Config {
    /// `-iquote` directories: searched for `"..."` includes only.
    pub quote_dirs: Vec<PathBuf>,
    /// `-I` directories.
    pub include_dirs: Vec<PathBuf>,
    /// System directories (searched after `-I`).
    pub system_dirs: Vec<PathBuf>,
    /// macOS `-F` style framework directories.
    pub framework_dirs: Vec<PathBuf>,
    /// Compiler-predefined macros: `(name-with-params, body)`.
    pub predefined: Vec<(String, String)>,
    /// `-D` / `-U` in command-line order; `None` body means `-U`.
    pub defines: Vec<(String, Option<String>)>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Directive {
    If,
    Ifdef,
    Ifndef,
    Elif,
    Elifdef,
    Elifndef,
    Else,
    Endif,
    Define,
    Undef,
    Include,
    IncludeNext,
    Error,
    Warning,
    Pragma,
    Embed,
    /// Accepted and ignored: `#line`, `#ident`, `#sccs`, `#assert`, `#unassert`.
    Ignored,
}

impl Directive {
    fn lookup(name: &str) -> Option<Directive> {
        use Directive::*;
        Some(match name {
            "if" => If,
            "ifdef" => Ifdef,
            "ifndef" => Ifndef,
            "elif" => Elif,
            "elifdef" => Elifdef,
            "elifndef" => Elifndef,
            "else" => Else,
            "endif" => Endif,
            "define" => Define,
            "undef" => Undef,
            "include" | "import" => Include,
            "include_next" => IncludeNext,
            "error" => Error,
            "warning" => Warning,
            "pragma" => Pragma,
            "embed" => Embed,
            "line" | "ident" | "sccs" | "assert" | "unassert" => Ignored,
            _ => return None,
        })
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum PragmaKind {
    Once,
    PushMacro,
    PopMacro,
    Pack,
}

impl PragmaKind {
    fn lookup(name: &str) -> Option<PragmaKind> {
        Some(match name {
            "once" => PragmaKind::Once,
            "push_macro" => PragmaKind::PushMacro,
            "pop_macro" => PragmaKind::PopMacro,
            "pack" => PragmaKind::Pack,
            _ => return None,
        })
    }
}

struct Cond {
    /// This branch is being emitted.
    active: bool,
    /// Some branch of this conditional has already been taken.
    taken: bool,
    /// Enclosing context is emitting.
    parent_active: bool,
    seen_else: bool,
}

struct Frame {
    toks: Rc<Vec<Token>>,
    pos: usize,
    path: PathBuf,
    /// Index into the include search list where this file was found, for `#include_next`.
    search_idx: Option<usize>,
    conds: Vec<Cond>,
    guard: Option<Rc<str>>,
    guard_closed: bool,
    other_seen: bool,
    system: bool,
}

pub struct Preprocessor {
    cfg: Config,
    search: Vec<PathBuf>,
    pub(crate) macros: HashMap<Rc<str>, Rc<Macro>>,
    pushed_macros: HashMap<Rc<str>, Vec<Option<Rc<Macro>>>>,
    frames: Vec<Frame>,
    pending: Vec<Token>,
    /// Lower bound of `pending` when expanding in isolation (macro arguments).
    floor: usize,
    isolated: u32,
    files: Vec<PathBuf>,
    file_cache: HashMap<PathBuf, Rc<Vec<Token>>>,
    once: HashSet<PathBuf>,
    guards: HashMap<PathBuf, Rc<str>>,
    deps: Vec<PathBuf>,
    pub diags: Vec<Diag>,
    counter: i64,
    oracle: Box<dyn FeatureOracle>,
    /// Tokens produced by `#pragma pack` and similar, emitted into the output stream.
    out_extra: Vec<Token>,
}

impl Preprocessor {
    pub fn new(cfg: Config) -> Preprocessor {
        let mut search = cfg.include_dirs.clone();
        search.extend(cfg.system_dirs.iter().cloned());
        let mut pp = Preprocessor {
            cfg,
            search,
            macros: HashMap::new(),
            pushed_macros: HashMap::new(),
            frames: Vec::new(),
            pending: Vec::new(),
            floor: 0,
            isolated: 0,
            files: vec![PathBuf::from("<command line>")],
            file_cache: HashMap::new(),
            once: HashSet::new(),
            guards: HashMap::new(),
            deps: Vec::new(),
            diags: Vec::new(),
            counter: 0,
            oracle: Box::new(StaticOracle::default()),
            out_extra: Vec::new(),
        };
        pp.load_predefined();
        pp
    }

    pub fn set_oracle(&mut self, o: Box<dyn FeatureOracle>) {
        self.oracle = o;
    }

    fn load_predefined(&mut self) {
        let mut src = String::new();
        for (n, b) in &self.cfg.predefined {
            src.push_str(&format!("#define {n} {b}\n"));
        }
        for (n, b) in &self.cfg.defines {
            match b {
                Some(b) => src.push_str(&format!("#define {n} {b}\n")),
                None => src.push_str(&format!("#undef {n}\n")),
            }
        }
        let toks = Rc::new(lex(&src, 0));
        self.frames.push(Frame::new(
            toks,
            PathBuf::from("<command line>"),
            None,
            true,
        ));
        while self.get().is_some() {}
        self.frames.clear();
        // Predefined macros count as system macros.
        for m in self.macros.values_mut() {
            Rc::make_mut(m).from_system = true;
        }
    }

    pub fn macros(&self) -> impl Iterator<Item = &Rc<Macro>> {
        self.macros.values()
    }

    /// The macro's replacement list, fully expanded with the macro itself blocked.
    pub fn expand_body(&mut self, name: &str) -> Vec<Token> {
        let Some(m) = self.macros.get(name).cloned() else {
            return Vec::new();
        };
        let hs = crate::token::hide_add(&None, &m.name);
        let toks: Vec<Token> = m
            .body
            .iter()
            .cloned()
            .map(|mut t| {
                t.hide = crate::token::hide_union(&t.hide, &hs);
                t
            })
            .collect();
        self.expand_isolated(toks)
    }

    pub fn is_defined(&self, name: &str) -> bool {
        self.macros.contains_key(name)
    }

    /// Every file read so far (absolute paths), in first-read order.
    pub fn dependencies(&self) -> &[PathBuf] {
        &self.deps
    }

    /// Every file known to the preprocessor, indexed by `Loc::file`.
    pub fn files(&self) -> &[PathBuf] {
        &self.files
    }

    pub fn file_name(&self, id: u32) -> &Path {
        self.files
            .get(id as usize)
            .map_or(Path::new("?"), |p| p.as_path())
    }

    /// Preprocesses `src` as if it were a file named `name`, returning the expanded tokens.
    pub fn preprocess_str(&mut self, name: &str, src: &str) -> Vec<Token> {
        let path = PathBuf::from(name);
        let id = self.file_id(&path);
        let toks = Rc::new(lex(src, id));
        self.frames.push(Frame::new(toks, path, None, false));
        self.run()
    }

    pub fn preprocess_file(&mut self, path: &Path) -> Result<Vec<Token>, String> {
        let toks = self.load_file(path)?;
        self.frames
            .push(Frame::new(toks, path.to_path_buf(), None, false));
        Ok(self.run())
    }

    fn run(&mut self) -> Vec<Token> {
        let mut out = Vec::new();
        while let Some(t) = self.expand_next() {
            out.push(t);
        }
        out
    }

    pub(crate) fn diag(&mut self, severity: Severity, loc: Loc, message: impl Into<String>) {
        let file = self.file_name(loc.file).display().to_string();
        self.diags.push(Diag {
            severity,
            message: message.into(),
            file,
            line: loc.line,
        });
    }

    fn file_id(&mut self, p: &Path) -> u32 {
        if let Some(i) = self.files.iter().position(|f| f == p) {
            return i as u32;
        }
        self.files.push(p.to_path_buf());
        (self.files.len() - 1) as u32
    }

    fn load_file(&mut self, path: &Path) -> Result<Rc<Vec<Token>>, String> {
        if let Some(t) = self.file_cache.get(path) {
            return Ok(t.clone());
        }
        let bytes = std::fs::read(path).map_err(|e| format!("{}: {e}", path.display()))?;
        let src = String::from_utf8_lossy(&bytes);
        let id = self.file_id(path);
        let toks = Rc::new(lex(&src, id));
        self.file_cache.insert(path.to_path_buf(), toks.clone());
        if !self.deps.iter().any(|d| d == path) {
            self.deps.push(path.to_path_buf());
        }
        Ok(toks)
    }

    // ---- token input -------------------------------------------------------------------

    /// Next token after directive processing, without macro expansion.
    pub(crate) fn get(&mut self) -> Option<Token> {
        loop {
            if self.pending.len() > self.floor {
                return self.pending.pop();
            }
            if self.isolated > 0 {
                return None;
            }
            if !self.out_extra.is_empty() {
                let mut v = std::mem::take(&mut self.out_extra);
                v.reverse();
                self.pending.extend(v);
                continue;
            }
            let Some(frame) = self.frames.last_mut() else {
                return None;
            };
            if frame.pos >= frame.toks.len() {
                self.pop_frame();
                continue;
            }
            let t = frame.toks[frame.pos].clone();
            frame.pos += 1;
            if t.bol && t.is_punct(Punct::Hash) {
                self.directive();
                continue;
            }
            if frame.conds.last().is_some_and(|c| !c.active) {
                continue;
            }
            frame.note_token();
            return Some(t);
        }
    }

    pub(crate) fn unget(&mut self, t: Token) {
        self.pending.push(t);
    }

    fn pop_frame(&mut self) {
        let f = self.frames.pop().unwrap();
        if let (Some(g), true, false) = (&f.guard, f.guard_closed, f.other_seen) {
            self.guards.insert(f.path.clone(), g.clone());
        }
    }

    /// Reads the rest of the current directive line (tokens up to the next line start).
    fn line_tokens(&mut self) -> Vec<Token> {
        let Some(frame) = self.frames.last_mut() else {
            return Vec::new();
        };
        let mut v = Vec::new();
        while frame.pos < frame.toks.len() && !frame.toks[frame.pos].bol {
            v.push(frame.toks[frame.pos].clone());
            frame.pos += 1;
        }
        v
    }

    fn skipping(&self) -> bool {
        self.frames
            .last()
            .is_some_and(|f| f.conds.last().is_some_and(|c| !c.active))
    }

    // ---- directives --------------------------------------------------------------------

    fn directive(&mut self) {
        let hash_loc;
        let line;
        {
            let f = self.frames.last().unwrap();
            hash_loc = f.toks[f.pos - 1].loc;
        }
        line = self.line_tokens();
        let Some(name_tok) = line.first() else { return };
        let rest = &line[1..];
        let skipping = self.skipping();
        let Some(dir) = Directive::lookup(&name_tok.text) else {
            if !skipping {
                self.diag(
                    Severity::Warning,
                    hash_loc,
                    format!("unknown directive #{}", name_tok.text),
                );
            }
            return;
        };
        let first_directive = {
            let f = self.frames.last().unwrap();
            f.pos == line.len() + 1 && f.guard.is_none() && !f.other_seen
        };

        // Conditionals are processed even while skipping, to keep nesting right.
        match dir {
            Directive::If | Directive::Ifdef | Directive::Ifndef => {
                let parent_active = !skipping;
                let v = if !parent_active {
                    false
                } else {
                    match dir {
                        Directive::If => self.eval_cond(rest, hash_loc),
                        Directive::Ifdef => rest.first().is_some_and(|t| self.is_defined(&t.text)),
                        _ => rest.first().is_some_and(|t| !self.is_defined(&t.text)),
                    }
                };
                let f = self.frames.last_mut().unwrap();
                if dir == Directive::Ifndef && first_directive && f.conds.is_empty() {
                    if let Some(t) = rest.first() {
                        f.guard = Some(t.text.clone());
                    }
                } else if f.conds.is_empty() {
                    f.note_other();
                }
                f.conds.push(Cond {
                    active: v,
                    taken: v,
                    parent_active,
                    seen_else: false,
                });
                return;
            }
            Directive::Elif | Directive::Elifdef | Directive::Elifndef => {
                let (parent_active, taken) = match self.frames.last().unwrap().conds.last() {
                    Some(c) => (c.parent_active, c.taken),
                    None => {
                        self.diag(Severity::Error, hash_loc, "#elif without #if");
                        return;
                    }
                };
                let v = if !parent_active || taken {
                    false
                } else {
                    match dir {
                        Directive::Elif => self.eval_cond(rest, hash_loc),
                        Directive::Elifdef => {
                            rest.first().is_some_and(|t| self.is_defined(&t.text))
                        }
                        _ => rest.first().is_some_and(|t| !self.is_defined(&t.text)),
                    }
                };
                let c = self.frames.last_mut().unwrap().conds.last_mut().unwrap();
                c.active = v;
                c.taken |= v;
                self.frames.last_mut().unwrap().note_directive_in_cond();
                return;
            }
            Directive::Else => {
                let Some(c) = self.frames.last_mut().unwrap().conds.last_mut() else {
                    self.diag(Severity::Error, hash_loc, "#else without #if");
                    return;
                };
                c.active = c.parent_active && !c.taken;
                c.taken = true;
                c.seen_else = true;
                self.frames.last_mut().unwrap().note_directive_in_cond();
                return;
            }
            Directive::Endif => {
                let f = self.frames.last_mut().unwrap();
                if f.conds.pop().is_none() {
                    self.diag(Severity::Error, hash_loc, "#endif without #if");
                    return;
                }
                if f.conds.is_empty() && f.guard.is_some() && !f.guard_closed {
                    f.guard_closed = true;
                } else {
                    f.note_directive_in_cond();
                }
                return;
            }
            _ => {}
        }
        if skipping {
            return;
        }
        self.frames.last_mut().unwrap().note_other();
        match dir {
            Directive::Define => self.do_define(rest, hash_loc),
            Directive::Undef => {
                if let Some(t) = rest.first() {
                    self.macros.remove(&t.text);
                }
            }
            Directive::Include => self.do_include(rest, hash_loc, false),
            Directive::IncludeNext => self.do_include(rest, hash_loc, true),
            Directive::Error => {
                self.diag(Severity::Error, hash_loc, format!("#error {}", join(rest)))
            }
            Directive::Warning => self.diag(
                Severity::Warning,
                hash_loc,
                format!("#warning {}", join(rest)),
            ),
            Directive::Pragma => self.do_pragma(rest, hash_loc),
            Directive::Embed => self.do_embed(rest, hash_loc),
            Directive::Ignored => {}
            _ => unreachable!("conditional directives are handled above"),
        }
    }

    fn eval_cond(&mut self, toks: &[Token], loc: Loc) -> bool {
        match self.eval_if(toks, loc) {
            Ok(v) => v != 0,
            Err(e) => {
                self.diag(Severity::Error, loc, format!("in #if: {e}"));
                false
            }
        }
    }

    fn do_define(&mut self, rest: &[Token], loc: Loc) {
        let Some(name) = rest.first().filter(|t| t.kind == Kind::Ident) else {
            self.diag(Severity::Error, loc, "macro name must be an identifier");
            return;
        };
        let mut body_start = 1;
        let mut params = None;
        let mut variadic = false;
        if rest
            .get(1)
            .is_some_and(|t| t.is_punct(Punct::LParen) && !t.space)
        {
            let mut ps: Vec<Rc<str>> = Vec::new();
            let mut i = 2;
            loop {
                match rest.get(i) {
                    Some(t) if t.is_punct(Punct::RParen) => {
                        i += 1;
                        break;
                    }
                    Some(t) if t.is_punct(Punct::Ellipsis) => {
                        variadic = true;
                        ps.push("__VA_ARGS__".into());
                        i += 1;
                    }
                    Some(t) if t.kind == Kind::Ident => {
                        ps.push(t.text.clone());
                        i += 1;
                        // GNU named variadic: `args...`
                        if rest.get(i).is_some_and(|t| t.is_punct(Punct::Ellipsis)) {
                            variadic = true;
                            i += 1;
                        }
                    }
                    Some(t) if t.is_punct(Punct::Comma) => i += 1,
                    _ => {
                        self.diag(Severity::Error, loc, "malformed macro parameter list");
                        return;
                    }
                }
            }
            params = Some(ps);
            body_start = i;
        }
        let mut body: Vec<Token> = rest[body_start..].to_vec();
        if let Some(b) = body.first_mut() {
            b.space = false;
        }
        let from_system = self.frames.last().is_some_and(|f| f.system);
        let m = Macro {
            name: name.text.clone(),
            params,
            variadic,
            body,
            loc: name.loc,
            from_system,
        };
        self.macros.insert(name.text.clone(), Rc::new(m));
    }

    fn do_pragma(&mut self, rest: &[Token], loc: Loc) {
        let Some(kind) = rest.first().and_then(|t| PragmaKind::lookup(&t.text)) else {
            return;
        };
        match kind {
            PragmaKind::Once => {
                if let Some(f) = self.frames.last() {
                    self.once.insert(f.path.clone());
                }
            }
            PragmaKind::PushMacro | PragmaKind::PopMacro => {
                // #pragma push_macro("NAME")
                let Some(arg) = rest.iter().find(|t| t.kind == Kind::Str) else {
                    return;
                };
                let name: Rc<str> = arg.text.trim_matches('"').into();
                if kind == PragmaKind::PushMacro {
                    let cur = self.macros.get(&name).cloned();
                    self.pushed_macros.entry(name).or_default().push(cur);
                } else if let Some(Some(prev)) = self.pushed_macros.get_mut(&name).map(|v| v.pop())
                {
                    match prev {
                        Some(m) => self.macros.insert(name, m),
                        None => self.macros.remove(&name),
                    };
                }
            }
            PragmaKind::Pack => {
                // Surfaced to the parser as `__capy_pragma_pack ( args ) ;`.
                let mut v = vec![Token::ident("__capy_pragma_pack")];
                v.extend(rest[1..].iter().cloned());
                v.push(Token::punct_tok(Punct::Semi));
                for t in &mut v {
                    t.loc = loc;
                }
                self.out_extra.extend(v);
            }
        }
    }

    fn do_embed(&mut self, rest: &[Token], loc: Loc) {
        match self.read_embed(rest, loc) {
            Ok(toks) => self.out_extra.extend(toks),
            Err(e) => self.diag(Severity::Error, loc, format!("#embed: {e}")),
        }
    }

    // ---- includes ----------------------------------------------------------------------

    /// Parses `"file"` or `<file>` from directive tokens (macro-expanding if needed).
    fn header_name(&mut self, rest: &[Token]) -> Option<(String, bool)> {
        let first = rest.first()?;
        if first.kind == Kind::Str {
            return Some((first.text.trim_matches('"').to_string(), true));
        }
        if first.is_punct(Punct::Lt) {
            let mut s = String::new();
            for t in &rest[1..] {
                if t.is_punct(Punct::Gt) {
                    return Some((s, false));
                }
                if t.space && !s.is_empty() {
                    s.push(' ');
                }
                s.push_str(&t.text);
            }
            return None;
        }
        // Computed include: macro-expand and retry.
        let expanded = self.expand_isolated(rest.to_vec());
        if expanded
            .first()
            .is_some_and(|t| t.kind == Kind::Str || t.is_punct(Punct::Lt))
        {
            return self.header_name(&expanded);
        }
        None
    }

    fn do_include(&mut self, rest: &[Token], loc: Loc, next: bool) {
        let Some((name, quoted)) = self.header_name(rest) else {
            self.diag(Severity::Error, loc, "malformed #include");
            return;
        };
        let Some((path, idx)) = self.resolve_include(&name, quoted, next) else {
            self.diag(Severity::Error, loc, format!("'{name}' file not found"));
            return;
        };
        if self.once.contains(&path) {
            return;
        }
        if let Some(g) = self.guards.get(&path) {
            if self.macros.contains_key(g) {
                return;
            }
        }
        if self.frames.len() >= 200 {
            self.diag(Severity::Error, loc, "#include nested too deeply");
            return;
        }
        match self.load_file(&path) {
            Ok(toks) => {
                let system = self.is_system_path(&path);
                self.frames.push(Frame::new(toks, path, idx, system));
            }
            Err(e) => self.diag(Severity::Error, loc, e),
        }
    }

    fn is_system_path(&self, p: &Path) -> bool {
        self.cfg
            .system_dirs
            .iter()
            .chain(&self.cfg.framework_dirs)
            .any(|d| p.starts_with(d))
    }

    pub(crate) fn resolve_include(
        &self,
        name: &str,
        quoted: bool,
        next: bool,
    ) -> Option<(PathBuf, Option<usize>)> {
        if Path::new(name).is_absolute() {
            return Path::new(name)
                .is_file()
                .then(|| (PathBuf::from(name), None));
        }
        let mut start = 0;
        if next {
            if let Some(i) = self.frames.last().and_then(|f| f.search_idx) {
                start = i + 1;
            }
        } else if quoted {
            if let Some(dir) = self.frames.last().and_then(|f| f.path.parent()) {
                let p = dir.join(name);
                if p.is_file() {
                    return Some((p, None));
                }
            }
            for d in &self.cfg.quote_dirs {
                let p = d.join(name);
                if p.is_file() {
                    return Some((p, None));
                }
            }
        }
        for (i, d) in self.search.iter().enumerate().skip(start) {
            let p = d.join(name);
            if p.is_file() {
                return Some((p, Some(i)));
            }
        }
        if let Some((fw, rest)) = name.split_once('/') {
            for d in &self.cfg.framework_dirs {
                for sub in ["Headers", "PrivateHeaders"] {
                    let p = d.join(format!("{fw}.framework")).join(sub).join(rest);
                    if p.is_file() {
                        return Some((p, None));
                    }
                }
            }
        }
        None
    }
}

impl Frame {
    fn new(toks: Rc<Vec<Token>>, path: PathBuf, search_idx: Option<usize>, system: bool) -> Frame {
        Frame {
            toks,
            pos: 0,
            path,
            search_idx,
            conds: Vec::new(),
            guard: None,
            guard_closed: false,
            other_seen: false,
            system,
        }
    }

    fn note_token(&mut self) {
        self.note_other();
    }

    /// Something other than the guard wrapper is present at file scope.
    fn note_other(&mut self) {
        if self.conds.is_empty() {
            if self.guard.is_some() && self.guard_closed {
                self.other_seen = true;
            } else if self.guard.is_none() {
                self.other_seen = true;
            }
        }
    }

    fn note_directive_in_cond(&mut self) {
        // `#else`/`#elif` at the guard's own level means it is not a plain guard.
        if self.conds.len() == 1 && self.guard.is_some() && !self.guard_closed {
            self.guard = None;
            self.other_seen = true;
        }
    }
}

fn join(toks: &[Token]) -> String {
    let mut s = String::new();
    for t in toks {
        if t.space && !s.is_empty() {
            s.push(' ');
        }
        s.push_str(&t.text);
    }
    s
}

/// Renders tokens back to source text, with a space wherever the input had one.
pub fn render(toks: &[Token]) -> String {
    join(toks)
}
