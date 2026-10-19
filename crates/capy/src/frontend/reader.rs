use crate::{
    frontend::error::LexicalError,
    heap::{Trace, object::builtin_class_ids},
};
use tree_sitter::Node;

use crate::{
    expander::add_source,
    frontend::{Directives, num::parse_number},
    global, list,
    runtime::{
        MutationContext,
        value::{ByteVector, ClassTagged, IntoValue, Keyword, Str, Symbol, Value, Vector},
        vm::syntax::Syntax,
    },
};

#[derive(Trace)]
#[collect(no_drop)]
#[repr(C, align(8))]
pub struct Annotation<'gc> {
    pub expression: Value<'gc>,
    pub stripped: Value<'gc>,
    pub source: Value<'gc>,
    pub start_point: (u32, u32),
    pub end_point: (u32, u32),
}

// SAFETY: `gc` for `Annotation` upholds all trait invariants
unsafe impl<'gc> ClassTagged for Annotation<'gc> {
    const CLASS_IDS: &'static [u32] = &[builtin_class_ids::ANNOTATION];
    const TYPE_NAME: &'static str = "#<annotation>";
}

pub struct TreeSitter<'a, 'gc> {
    ctx: MutationContext<'gc>,
    text: &'a str,
    tree: tree_sitter::Tree,
    source_file: Value<'gc>,
    wrap_stx: bool,
    directives: Directives,
}

impl<'a, 'gc> TreeSitter<'a, 'gc> {
    pub fn new(
        ctx: MutationContext<'gc>,
        text: &'a str,
        source_file: Value<'gc>,
        wrap_stx: bool,
    ) -> Self {
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&tree_sitter::Language::new(tree_sitter_scheme::LANGUAGE))
            .expect("tree-sitter scheme language should be valid");

        let tree = parser
            .parse(text, None)
            .expect("tree-sitter failed to parse input text");

        Self {
            wrap_stx,
            ctx,
            text,
            tree,
            source_file,
            directives: Directives::empty(),
        }
    }

    pub fn directives(&self) -> Directives {
        self.directives
    }

    pub fn ctx(&self) -> MutationContext<'gc> {
        self.ctx
    }

    pub fn text(&self) -> &'a str {
        self.text
    }

    pub fn tree(&self) -> &tree_sitter::Tree {
        &self.tree
    }

    pub fn source_file(&self) -> Value<'gc> {
        self.source_file
    }

    pub fn annotate(&self, expression: Value<'gc>, node: &Node) {
        self.annotate_span(
            expression,
            (
                node.start_position().row as u32 + 1,
                node.start_position().column as u32,
            ),
            (
                node.end_position().row as u32 + 1,
                node.end_position().column as u32,
            ),
        );
    }

    pub fn annotate_span(&self, expression: Value<'gc>, start: (u32, u32), end: (u32, u32)) {
        if expression.is_immediate() || expression.is_number() {
            return;
        }
        if expression.is::<Symbol>() && expression.downcast::<Symbol>().is_interned() {
            return;
        }
        if expression.is::<Keyword>() {
            return;
        }

        add_source(
            self.ctx,
            expression,
            self.source_file,
            start.0 as i32,
            start.1 as i32,
            end.0 as i32,
            end.1 as i32,
        );
    }

    pub fn text_of(&self, node: &Node) -> &str {
        node.utf8_text(self.text.as_bytes())
            .expect("Failed to get text from node")
    }

    pub fn is_symbol_of(&self, node: &Node, symbol: &str) -> bool {
        node.kind() == "symbol" && self.text_of(node) == symbol
    }

    fn is_intertoken(kind: &str) -> bool {
        matches!(kind, "comment" | "block_comment" | "sexp_comment")
    }

    fn abbrev_name(prefix: &str) -> Option<&'static str> {
        match prefix {
            "'" => Some("quote"),
            "`" => Some("quasiquote"),
            "," => Some("unquote"),
            ",@" => Some("unquote-splicing"),
            "#'" => Some("syntax"),
            "#`" => Some("quasisyntax"),
            "#," => Some("unsyntax"),
            "#,@" => Some("unsyntax-splicing"),
            _ => None,
        }
    }

    fn parse_abbrev(&self, node: &Node, prefix: &str) -> Result<Value<'gc>, LexicalError<'gc>> {
        let mut cursor = node.walk();
        let children: Vec<Node> = node.children(&mut cursor).collect();
        let span = Self::span_of(node, node);
        let (value, _) = self.parse_abbrev_children(&children, 0, prefix, span)?;
        Ok(value)
    }

    fn parse_abbrev_children(
        &self,
        children: &[Node],
        start: usize,
        prefix: &str,
        span: ((u32, u32), (u32, u32)),
    ) -> Result<(Value<'gc>, usize), LexicalError<'gc>> {
        let err = || LexicalError::InvalidSyntax { span: span.0 };
        let name = Self::abbrev_name(prefix).ok_or_else(err)?;
        let mut next = start + 1;
        while next < children.len()
            && children[next].is_named()
            && Self::is_intertoken(children[next].kind())
        {
            next += 1;
        }
        let datum = children.get(next).filter(|datum| {
            datum.is_named() && datum.kind() != "ERROR" && datum.kind() != "MISSING"
        });
        let Some(datum) = datum else {
            return Err(err());
        };
        next += 1;
        let expr = self.parse_lexeme(datum)?;

        let tag: Value<'gc> = Symbol::from_str(self.ctx, name).into();
        Ok((
            self.wrap_span(span.0, span.1, list!(self.ctx, tag, expr)),
            next,
        ))
    }

    fn span_of(first: &Node, last: &Node) -> ((u32, u32), (u32, u32)) {
        (
            (
                first.start_position().row as u32 + 1,
                first.start_position().column as u32,
            ),
            (
                last.end_position().row as u32 + 1,
                last.end_position().column as u32,
            ),
        )
    }

    fn parse_unit(
        &self,
        children: &[Node],
        i: usize,
        span: ((u32, u32), (u32, u32)),
    ) -> Result<(Value<'gc>, usize), LexicalError<'gc>> {
        let child = &children[i];
        if child.is_named() {
            return Ok((self.parse_lexeme(child)?, i + 1));
        }
        Err(LexicalError::InvalidSyntax { span: span.0 })
    }

    fn parse_symbol_text(
        &self,
        text: &str,
        src: ((u32, u32), (u32, u32)),
    ) -> Result<String, LexicalError<'gc>> {
        if !(text.starts_with('|') && text.ends_with('|') && text.len() >= 2) {
            return Ok(text.to_string());
        }

        let mut result = String::new();
        let mut chars = text[1..text.len() - 1].chars();

        while let Some(ch) = chars.next() {
            if ch != '\\' {
                result.push(ch);
                continue;
            }

            match chars.next() {
                Some('x') => {
                    let mut hex_digits = String::new();
                    let mut terminated = false;

                    for hex_ch in chars.by_ref() {
                        if hex_ch == ';' {
                            terminated = true;
                            break;
                        }

                        if !hex_ch.is_ascii_hexdigit() {
                            return Err(LexicalError::InvalidSyntax { span: src.0 });
                        }

                        hex_digits.push(hex_ch);
                    }

                    if !terminated || hex_digits.is_empty() {
                        return Err(LexicalError::InvalidSyntax { span: src.0 });
                    }

                    let code_point = u32::from_str_radix(&hex_digits, 16)
                        .map_err(|_| LexicalError::InvalidSyntax { span: src.0 })?;
                    let Some(escaped) = char::from_u32(code_point) else {
                        return Err(LexicalError::InvalidSyntax { span: src.0 });
                    };
                    result.push(escaped);
                }
                Some('a') => result.push('\u{07}'),
                Some('b') => result.push('\u{08}'),
                Some('t') => result.push('\t'),
                Some('n') => result.push('\n'),
                Some('r') => result.push('\r'),
                Some(escaped) => result.push(escaped),
                None => return Err(LexicalError::InvalidSyntax { span: src.0 }),
            }
        }

        Ok(result)
    }

    pub fn read_program(&self) -> Result<Vec<Value<'gc>>, LexicalError<'gc>> {
        let root_node = self.tree.root_node();
        let start = root_node.start_position();
        let end = root_node.end_position();

        let start_str = format!("{}:{}", start.row + 1, start.column + 1);
        let end_str = format!("{}:{}", end.row + 1, end.column + 1);

        assert_eq!(
            root_node.kind(),
            "program",
            "Root node is not a program, but {}, file: {}, location: {}, end: {}",
            self.text_of(&root_node),
            self.source_file,
            start_str,
            end_str
        );
        let mut cursor = self.tree.walk();

        let mut result = Vec::new();
        for child in root_node.children(&mut cursor) {
            if Self::is_intertoken(child.kind()) {
                continue;
            }
            result.push(self.parse_lexeme(&child)?);
        }

        Ok(result)
    }

    pub fn parse_compound_node(
        &self,
        node: &Node,
        src: ((u32, u32), (u32, u32)),
    ) -> Result<Value<'gc>, LexicalError<'gc>> {
        let mut cursor = node.walk();
        let children: Vec<Node> = node.children(&mut cursor).collect();

        let _ = src;
        let span = Self::span_of(node, node);
        let (value, _) = self.parse_compound_children(&children, 0, span)?;
        Ok(value)
    }

    fn parse_compound_children(
        &self,
        children: &[Node],
        start: usize,
        span: ((u32, u32), (u32, u32)),
    ) -> Result<(Value<'gc>, usize), LexicalError<'gc>> {
        let opener = children.get(start).map(|c| self.text_of(c)).unwrap_or("");
        let (terminator, typ) = match opener {
            "(" => (")", CompoundType::List),
            "[" => ("]", CompoundType::List),
            "#(" => (")", CompoundType::Vector),
            "#u8(" | "#vu8(" => (")", CompoundType::Bytevector),
            _ => {
                return Err(LexicalError::InvalidSyntax { span: span.0 });
            }
        };

        let mut head = Value::null();

        let mut prev = Value::null();

        let mut i = start + 1;
        loop {
            let Some(child) = children.get(i) else {
                return Err(LexicalError::UnclosedList {
                    start_at: span.0,
                    end_at: span.1,
                });
            };
            match child.kind() {
                x if x == terminator => match typ {
                    CompoundType::Vector => {
                        let vec = Value::list_to_vector(head, self.ctx);

                        return Ok((self.wrap_span(span.0, span.1, vec.into()), i + 1));
                    }

                    CompoundType::Bytevector => {
                        let len = Value::list_length(head);

                        let bv = ByteVector::new::<false>(self.ctx, len, true);

                        let mut vec = Vec::with_capacity(len);

                        let mut current = head;

                        while !current.is_null() {
                            let byte = current.car().as_int32();
                            vec.push(byte as u8);
                            current = current.cdr();
                        }
                        // SAFETY: Mutable access is exclusive and goes through GC write barrier
                        unsafe {
                            bv.as_slice_mut_unchecked().copy_from_slice(&vec);
                        }

                        return Ok((self.wrap_span(span.0, span.1, bv.into()), i + 1));
                    }

                    CompoundType::List => {
                        return Ok((self.wrap_span(span.0, span.1, head), i + 1));
                    }
                },

                x if x == ")" || x == "]" => {
                    let end = child.end_position();
                    return Err(LexicalError::UnclosedList {
                        start_at: span.0,
                        end_at: (end.row as u32 + 1, end.column as u32),
                    });
                }

                "dot" => {
                    if matches!(typ, CompoundType::List) {
                        let tail_pos = children[i + 1..]
                            .iter()
                            .position(|next| !next.is_named() || !Self::is_intertoken(next.kind()))
                            .map(|pos| pos + i + 1)
                            .ok_or(LexicalError::ImproperListMissingTail { span: span.0 })?;
                        let tail = &children[tail_pos];
                        if tail.kind() == terminator {
                            return Err(LexicalError::ImproperListMultiple {
                                span: span.0,
                                dot_spans: vec![Value::null()],
                                multi_tails: vec![Value::null()],
                            });
                        }
                        if !tail.is_named() {
                            return Err(LexicalError::InvalidSyntax { span: span.0 });
                        }

                        let tail = self.parse_lexeme(tail)?;

                        if prev.is_pair() {
                            prev.set_cdr(self.ctx, tail);
                        }

                        return Ok((self.wrap_span(span.0, span.1, head), tail_pos + 1));
                    } else {
                        return Err(LexicalError::InvalidSyntax { span: span.0 });
                    }
                }

                _ => {
                    if child.is_named() && Self::is_intertoken(child.kind()) {
                        i += 1;
                        continue;
                    }
                    let (lexeme, next) = self.parse_unit(children, i, span)?;

                    let new_prev = Value::cons(self.ctx, lexeme, Value::null());

                    if prev.is_pair() {
                        prev.set_cdr(self.ctx, new_prev);
                    }
                    prev = new_prev;

                    if !head.is_pair() {
                        head = new_prev;
                    }

                    i = next;
                    continue;
                }
            }
        }
    }

    fn parse_lexeme(&self, node: &Node) -> Result<Value<'gc>, LexicalError<'gc>> {
        let text = self.text_of(node);

        // 1-based lines per the reader convention (see [`annotate`]).
        let src = (
            (
                node.start_position().row as u32 + 1,
                node.start_position().column as u32,
            ),
            (
                node.end_position().row as u32 + 1,
                node.end_position().column as u32,
            ),
        );

        match node.kind() {
            "comment" => Ok(Value::null()),
            "symbol" => {
                let text = self.parse_symbol_text(text, src)?;
                let symbol = Symbol::from_str(self.ctx, &text);

                Ok(self.wrap(node, symbol.into()))
            }

            "string" => {
                let content = &text[1..text.len() - 1]; // Remove quotes
                let mut result = String::new();
                let mut chars = content.chars();

                while let Some(ch) = chars.next() {
                    if ch == '\\' {
                        match chars.next() {
                            Some('n') => result.push('\n'),
                            Some('t') => result.push('\t'),
                            Some('r') => result.push('\r'),
                            Some('\\') => result.push('\\'),
                            Some('"') => result.push('"'),
                            Some('a') => result.push('\u{07}'), // alarm
                            Some('b') => result.push('\u{08}'), // backspace
                            Some('v') => result.push('\u{0B}'), // vertical tab
                            Some('f') => result.push('\u{0C}'), // form feed
                            Some('x') => {
                                // Hex escape sequence \xHH
                                let mut hex_digits = String::new();
                                for _ in 0..2 {
                                    if let Some(hex_ch) = chars.next() {
                                        if hex_ch.is_ascii_hexdigit() {
                                            hex_digits.push(hex_ch);
                                        } else {
                                            return Err(LexicalError::InvalidSyntax {
                                                span: src.0,
                                            });
                                        }
                                    } else {
                                        return Err(LexicalError::InvalidSyntax { span: src.0 });
                                    }
                                }
                                if let Ok(code_point) = u32::from_str_radix(&hex_digits, 16) {
                                    if let Some(unicode_char) = char::from_u32(code_point) {
                                        result.push(unicode_char);
                                    } else {
                                        return Err(LexicalError::InvalidSyntax { span: src.0 });
                                    }
                                } else {
                                    return Err(LexicalError::InvalidSyntax { span: src.0 });
                                }
                            }
                            // \uHHHH
                            // Character code given by four hexadecimal digits. For example \u0100 for a capital A with macron (U+0100).
                            Some('u') => {
                                let mut hex_digits = String::new();
                                for _ in 0..4 {
                                    if let Some(hex_ch) = chars.next() {
                                        if hex_ch.is_ascii_hexdigit() {
                                            hex_digits.push(hex_ch);
                                        } else {
                                            return Err(LexicalError::InvalidSyntax {
                                                span: src.0,
                                            });
                                        }
                                    } else {
                                        return Err(LexicalError::InvalidSyntax { span: src.0 });
                                    }
                                }
                                if let Ok(code_point) = u32::from_str_radix(&hex_digits, 16) {
                                    if let Some(unicode_char) = char::from_u32(code_point) {
                                        result.push(unicode_char);
                                    } else {
                                        return Err(LexicalError::InvalidSyntax { span: src.0 });
                                    }
                                } else {
                                    return Err(LexicalError::InvalidSyntax { span: src.0 });
                                }
                            }

                            // \UHHHHHH
                            // Character code given by six hexadecimal digits. For example \U010402.
                            Some('U') => {
                                let mut hex_digits = String::new();
                                for _ in 0..6 {
                                    if let Some(hex_ch) = chars.next() {
                                        if hex_ch.is_ascii_hexdigit() {
                                            hex_digits.push(hex_ch);
                                        } else {
                                            return Err(LexicalError::InvalidSyntax {
                                                span: src.0,
                                            });
                                        }
                                    } else {
                                        return Err(LexicalError::InvalidSyntax { span: src.0 });
                                    }
                                }
                                if let Ok(code_point) = u32::from_str_radix(&hex_digits, 16) {
                                    if let Some(unicode_char) = char::from_u32(code_point) {
                                        result.push(unicode_char);
                                    } else {
                                        return Err(LexicalError::InvalidSyntax { span: src.0 });
                                    }
                                } else {
                                    return Err(LexicalError::InvalidSyntax { span: src.0 });
                                }
                            }
                            Some(other) => {
                                return Err(LexicalError::InvalidCharacter {
                                    source: src.0,
                                    character: other,
                                });
                            }
                            None => {
                                return Err(LexicalError::UnclosedString { source: src.0 });
                            }
                        }
                    } else {
                        result.push(ch);
                    }
                }

                // Source string literals are immutable (R6RS): all readers of
                // program text go through here, runtime `read` builds its
                // strings from mutable primitives instead.
                let string = Str::new(self.ctx, &result, true);
                Ok(self.wrap(node, string.into()))
            }

            "number" => {
                let n = parse_number(text)
                    .map_err(|e| LexicalError::InvalidNumber {
                        span: src.0,
                        error: e,
                    })?
                    .to_vm_number(self.ctx)
                    .into_value(self.ctx);

                // Numbers are immediates without table identity, the syntax
                // wrap is their only span channel (see [`wrap`]).
                Ok(self.wrap(node, n))
            }

            "boolean" => {
                let value = if text == "#t" {
                    Value::new(true)
                } else if text == "#f" {
                    Value::new(false)
                } else {
                    return Err(LexicalError::InvalidSyntax { span: src.0 });
                };

                Ok(self.wrap(node, value))
            }

            "quote" => self.parse_abbrev(node, "'"),

            "syntax_quote" => self.parse_abbrev(node, "#'"),

            "quasisyntax" => self.parse_abbrev(node, "#`"),

            "unsyntax" => self.parse_abbrev(node, "#,"),

            "unsyntax_splicing" => self.parse_abbrev(node, "#,@"),

            "quasiquote" => self.parse_abbrev(node, "`"),

            "unquote" => self.parse_abbrev(node, ","),

            "unquote_splicing" => self.parse_abbrev(node, ",@"),

            "list" => {
                let list = self.parse_compound_node(node, src)?;
                Ok(self.wrap(node, list))
            }

            "character" => {
                let rest = text
                    .strip_prefix("#\\")
                    .expect("character literal should start with #\\");

                if let Some(hex_part) = rest.strip_prefix("x") {
                    let end_of_hex = hex_part
                        .find(|c: char| !c.is_ascii_hexdigit())
                        .unwrap_or(hex_part.len());

                    if end_of_hex == 0 {
                        return Ok(self.wrap(node, Value::new('x')));
                    }

                    let hex_str = &hex_part[..end_of_hex];
                    let code_point = u32::from_str_radix(hex_str, 16).map_err(|_| {
                        LexicalError::InvalidCharacter {
                            source: src.0,
                            character: 'x',
                        }
                    })?;
                    let ch = char::from_u32(code_point).ok_or({
                        LexicalError::InvalidCharacter {
                            source: src.0,
                            character: 'x',
                        }
                    })?;

                    return Ok(self.wrap(node, Value::new(ch)));
                }

                const NAMED_CHARS: &[(&str, char)] = &[
                    ("newline", '\n'),
                    ("linefeed", '\n'),
                    ("space", ' '),
                    ("tab", '\t'),
                    ("return", '\r'),
                    ("escape", '\u{1B}'),
                    ("alarm", '\u{07}'),
                    ("backspace", '\u{08}'),
                    ("delete", '\u{7F}'),
                    ("null", '\0'),
                ];

                for (name, character) in NAMED_CHARS {
                    if rest.strip_prefix(name).is_some() {
                        return Ok(self.wrap(node, Value::new(*character)));
                    }
                }

                let character = rest.chars().next().ok_or(LexicalError::InvalidCharacter {
                    source: src.0,
                    character: ' ',
                })?;

                Ok(self.wrap(node, Value::new(character)))
            }

            "vector" => {
                let vec = self.parse_compound_node(node, src)?;
                Ok(self.wrap(node, vec))
            }

            "byte_vector" => {
                let bv = self.parse_compound_node(node, src)?;
                Ok(self.wrap(node, bv))
            }

            "ERROR" | "MISSING" => Err(LexicalError::InvalidSyntax { span: src.0 }),

            "directive" => Ok(Value::null()),
            // `#n=` labels and `#n#` references need shared or cyclic
            // structure, which this reader cannot build: reject them
            // loudly rather than misreading the data.
            "dot" | "datum_label" | "datum_reference" => {
                Err(LexicalError::InvalidSyntax { span: src.0 })
            }
            "keyword" => {
                let kw = self.ctx.keyword(&self.text_of(node)[2..]);
                Ok(self.wrap(node, kw.into()))
            }

            _ => Err(LexicalError::InvalidSyntax { span: src.0 }),
        }
    }

    pub fn wrap(&self, node: &Node, val: Value<'gc>) -> Value<'gc> {
        // 1-based lines per the reader convention (see [`annotate`]).
        self.wrap_span(
            (
                node.start_position().row as u32 + 1,
                node.start_position().column as u32,
            ),
            (
                node.end_position().row as u32 + 1,
                node.end_position().column as u32,
            ),
            val,
        )
    }

    pub fn wrap_span(&self, start: (u32, u32), end: (u32, u32), val: Value<'gc>) -> Value<'gc> {
        self.annotate_span(val, start, end);
        if !self.wrap_stx || val.is::<Syntax>() {
            return val;
        }
        global!(
            empty_wrap<'gc>: Value<'gc> = (ctx) list!(ctx, Value::null());
        );
        let filename = self.source_file;
        let v = Vector::from_slice(
            self.ctx,
            &[
                filename,
                (start.0 as i32).into(),
                (start.1 as i32).into(),
                (end.0 as i32).into(),
                (end.1 as i32).into(),
                Value::new(false),
                Value::new(false),
                Value::from(Symbol::from_str(self.ctx, "read")),
                Value::null(),
            ],
        );

        let stx = Syntax::new(
            self.ctx,
            val,
            empty_wrap(self.ctx),
            Value::new(false),
            v.into(),
            Value::null(),
        );
        stx.into()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompoundType {
    Vector,
    List,
    Bytevector,
}
