use std::fmt::Display;

use rsgc::Trace;
use tree_sitter::Node;

use crate::{
    expander::add_source,
    frontend::{
        Directives,
        num::{NumberParseError, parse_number},
    },
    global, list,
    runtime::{
        Context,
        value::{ByteVector, IntoValue, ScmHeader, Str, Symbol, Tagged, TypeCode8, Value, Vector},
        vm::syntax::Syntax,
    },
};

#[derive(Trace)]
#[collect(no_drop)]
#[repr(C, align(8))]
pub struct Annotation<'gc> {
    pub header: ScmHeader,
    pub expression: Value<'gc>,
    pub stripped: Value<'gc>,
    pub source: Value<'gc>,
    pub start_point: (u32, u32),
    pub end_point: (u32, u32),
}

unsafe impl<'gc> Tagged for Annotation<'gc> {
    const TC8: TypeCode8 = TypeCode8::ANNOTATION;
    const TYPE_NAME: &'static str = "#<annotation>";
}

#[derive(Debug, Clone)]
pub enum LexicalError<'gc> {
    InvalidCharacter {
        source: (u32, u32),
        character: char,
    },
    UnclosedString {
        source: (u32, u32),
    },
    UnclosedList {
        start_at: (u32, u32),
        end_at: (u32, u32),
    },

    ImproperListMultiple {
        span: (u32, u32),
        dot_spans: Vec<Value<'gc>>,
        multi_tails: Vec<Value<'gc>>,
    },

    ImproperListMissingTail {
        span: (u32, u32),
    },

    InvalidSyntax {
        span: (u32, u32),
    },

    InvalidNumber {
        span: (u32, u32),
        error: NumberParseError,
    },

    ExpectedPair {
        span: (u32, u32),
    },
}

impl<'gc> LexicalError<'gc> {
    pub fn to_string(&self, file: impl Display) -> String {
        match self {
            LexicalError::InvalidCharacter { source, character } => format!(
                "Invalid character '{}' at {}:{}:{}",
                character, file, source.0, source.1
            ),
            LexicalError::UnclosedString { source } => format!(
                "Unclosed string starting at {}:{}:{}",
                file, source.0, source.1
            ),
            LexicalError::UnclosedList { start_at, end_at } => format!(
                "Unclosed list starting from {}:{}:{} to {}:{}:{}",
                file, start_at.0, start_at.1, file, end_at.0, end_at.1
            ),
            LexicalError::ImproperListMultiple {
                span,
                dot_spans: _,
                multi_tails: _,
            } => format!(
                "Improper list with multiple tails at {}:{}:{}",
                file, span.0, span.1
            ),
            LexicalError::ImproperListMissingTail { span } => format!(
                "Improper list missing tail at {}:{}:{}",
                file, span.0, span.1,
            ),
            LexicalError::InvalidSyntax { span } => {
                format!("Invalid syntax at {}:{}:{}", file, span.0, span.1)
            }
            LexicalError::InvalidNumber { span, error } => {
                format!(
                    "Invalid number at {}:{}:{}: {}",
                    file, span.0, span.1, error
                )
            }
            LexicalError::ExpectedPair { span } => {
                format!("Expected pair at {}:{}:{}", file, span.0, span.1)
            }
        }
    }
}

pub struct TreeSitter<'a, 'gc> {
    ctx: Context<'gc>,
    text: &'a str,
    tree: tree_sitter::Tree,
    source_file: Value<'gc>,
    #[allow(dead_code)]
    wrap_stx: bool,
    directives: Directives,
}

impl<'a, 'gc> TreeSitter<'a, 'gc> {
    pub fn new(ctx: Context<'gc>, text: &'a str, source_file: Value<'gc>, wrap_stx: bool) -> Self {
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&tree_sitter::Language::new(tree_sitter_scheme::LANGUAGE))
            .unwrap();

        let tree = parser.parse(text, None).unwrap();

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

    pub fn ctx(&self) -> Context<'gc> {
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
        if expression.is_immediate() || expression.is_number() {
            return;
        }
        let start_point = node.start_position();

        add_source(
            self.ctx,
            expression,
            self.source_file,
            start_point.row as i32,
            start_point.column as i32,
        );
    }

    pub fn text_of(&self, node: &Node) -> &str {
        node.utf8_text(self.text.as_bytes())
            .expect("Failed to get text from node")
    }

    pub fn is_symbol_of(&self, node: &Node, symbol: &str) -> bool {
        node.kind() == "symbol" && self.text_of(node) == symbol
    }

    pub fn read_program(&self) -> Result<Vec<Value<'gc>>, LexicalError<'gc>> {
        let root_node = self.tree.root_node();
        assert_eq!(
            root_node.kind(),
            "program",
            "Root node is not a program, file: {}",
            self.source_file
        );
        let mut cursor = self.tree.walk();

        let mut result = Vec::new();
        for child in root_node.children(&mut cursor) {
            if child.kind() == "comment" {
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

        typ: CompoundType,
    ) -> Result<Value<'gc>, LexicalError<'gc>> {
        let mut cursor = node.walk();
        let mut children = node.children(&mut cursor);

        let terminator = match self.text_of(&children.next().unwrap()) {
            "(" => ")",
            "#(" => ")",
            "#u8(" => ")",
            "[" => "]",
            _ => {
                return Err(LexicalError::InvalidSyntax { span: src.0 });
            }
        };

        let mut head = Value::null();

        let mut prev = Value::null();

        loop {
            match children.next() {
                None => {
                    return Err(LexicalError::UnclosedList {
                        start_at: src.0,
                        end_at: src.1,
                    });
                }
                Some(child) => match child.kind() {
                    x if x == terminator => match typ {
                        CompoundType::Vector => {
                            let vec = Value::list_to_vector(head, self.ctx);

                            self.annotate(vec.into(), &child);
                            return Ok(self.wrap(node, vec.into()));
                        }

                        CompoundType::Bytevector => {
                            let len = Value::list_length(head);

                            let bv = ByteVector::new::<false>(&self.ctx, len, true);

                            let mut vec = Vec::with_capacity(len);

                            let mut current = head;

                            while !current.is_null() {
                                let byte = current.as_int32();
                                vec.push(byte as u8);
                                current = current.cdr();
                            }
                            unsafe {
                                bv.as_slice_mut_unchecked().copy_from_slice(&vec);
                            }

                            self.annotate(bv.into(), &child);

                            return Ok(self.wrap(node, bv.into()));
                        }

                        CompoundType::List => {
                            self.annotate(head, &child);
                            return Ok(self.wrap(node, head));
                        }
                    },

                    x if x == ")" || x == "]" => {
                        let end = child.end_position();
                        return Err(LexicalError::UnclosedList {
                            start_at: src.0,
                            end_at: (end.row as u32, end.column as u32),
                        });
                    }

                    "symbol" if self.text_of(&child) == "." => {
                        if matches!(typ, CompoundType::List) {
                            let Some(next) = children.next() else {
                                return Err(LexicalError::ImproperListMissingTail { span: src.0 });
                            };
                            if next.kind() == terminator {
                                return Err(LexicalError::ImproperListMultiple {
                                    span: src.0,
                                    dot_spans: vec![Value::null()],
                                    multi_tails: vec![Value::null()],
                                });
                            }

                            let tail = self.parse_lexeme(&next)?;

                            if prev.is_pair() {
                                prev.set_cdr(self.ctx, tail);
                            }

                            self.annotate(head, &next);

                            return Ok(self.wrap(node, head));
                        } else {
                            return Err(LexicalError::InvalidSyntax { span: src.0 });
                        }
                    }

                    _ => {
                        if child.kind() == "comment" {
                            continue;
                        }
                        let lexeme = self.parse_lexeme(&child)?;

                        let new_prev = Value::cons(self.ctx, lexeme, Value::null());

                        if prev.is_pair() {
                            prev.set_cdr(self.ctx, new_prev);
                        }
                        prev = new_prev;

                        if !head.is_pair() {
                            head = new_prev;
                        }

                        continue;
                    }
                },
            }
        }
    }

    fn parse_lexeme(&self, node: &Node) -> Result<Value<'gc>, LexicalError<'gc>> {
        let text = self.text_of(node);

        let src = (
            (
                node.start_position().row as u32,
                node.start_position().column as u32,
            ),
            (
                node.end_position().row as u32,
                node.end_position().column as u32,
            ),
        );

        match node.kind() {
            "comment" => Ok(Value::null()),
            "symbol" => {
                let symbol = Symbol::from_str(self.ctx, text);

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

                let string = Str::new(&self.ctx, &result, false);
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

                Ok(n)
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

            "quote" => {
                let mut cursor = node.walk();
                let child = node.children(&mut cursor).skip(1).next().unwrap();

                let expr = self.parse_lexeme(&child)?;

                self.annotate(expr, &node);

                let quote: Value<'gc> = Symbol::from_str(self.ctx, "quote").into();
                Ok(self.wrap(node, list!(self.ctx, quote, expr).into()))
            }

            "syntax" => {
                let mut cursor = node.walk();
                let child = node.children(&mut cursor).skip(1).next().unwrap();

                let expr = self.parse_lexeme(&child)?;

                self.annotate(expr, &node);

                let syntax: Value<'gc> = Symbol::from_str(self.ctx, "syntax").into();
                Ok(self.wrap(node, list!(self.ctx, syntax, expr).into()))
            }

            "quasisyntax" => {
                let mut cursor = node.walk();
                let child = node.children(&mut cursor).skip(1).next().unwrap();

                let expr = self.parse_lexeme(&child)?;

                self.annotate(expr, &node);

                let quasisyntax: Value<'gc> = Symbol::from_str(self.ctx, "quasisyntax").into();
                Ok(self.wrap(node, list!(self.ctx, quasisyntax, expr).into()))
            }

            "unsyntax" => {
                let mut cursor = node.walk();
                let child = node.children(&mut cursor).skip(1).next().unwrap();

                let expr = self.parse_lexeme(&child)?;

                self.annotate(expr, &node);

                let unsyntax: Value<'gc> = Symbol::from_str(self.ctx, "unsyntax").into();
                Ok(self.wrap(node, list!(self.ctx, unsyntax, expr).into()))
            }

            "unsyntax_splicing" => {
                let mut cursor = node.walk();
                let child = node.children(&mut cursor).skip(1).next().unwrap();

                let expr = self.parse_lexeme(&child)?;

                self.annotate(expr, &node);

                let unsyntax_splicing: Value<'gc> =
                    Symbol::from_str(self.ctx, "unsyntax-splicing").into();
                Ok(self.wrap(node, list!(self.ctx, unsyntax_splicing, expr).into()))
            }

            "quasiquote" => {
                let mut cursor = node.walk();
                let child = node.children(&mut cursor).skip(1).next().unwrap();

                let expr = self.parse_lexeme(&child)?;

                self.annotate(expr, &node);

                let quasiquote: Value<'gc> = Symbol::from_str(self.ctx, "quasiquote").into();
                Ok(self.wrap(node, list!(self.ctx, quasiquote, expr).into()))
            }

            "unquote" => {
                let mut cursor = node.walk();
                let child = node.children(&mut cursor).skip(1).next().unwrap();

                let expr = self.parse_lexeme(&child)?;

                self.annotate(expr, &node);

                let unquote: Value<'gc> = Symbol::from_str(self.ctx, "unquote").into();
                Ok(self.wrap(node, list!(self.ctx, unquote, expr).into()))
            }

            "unquote_splicing" => {
                let mut cursor = node.walk();
                let child = node.children(&mut cursor).skip(1).next().unwrap();

                let expr = self.parse_lexeme(&child)?;

                self.annotate(expr, &node);

                let unquote_splicing: Value<'gc> =
                    Symbol::from_str(self.ctx, "unquote-splicing").into();
                Ok(self.wrap(node, list!(self.ctx, unquote_splicing, expr).into()))
            }

            "list" => {
                let list = self.parse_compound_node(node, src, CompoundType::List)?;
                Ok(self.wrap(node, list))
            }

            "character" => {
                let rest = text.strip_prefix("#\\").unwrap();

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
                    let ch = char::from_u32(code_point).ok_or_else(|| {
                        LexicalError::InvalidCharacter {
                            source: src.0,
                            character: 'x',
                        }
                    })?;

                    return Ok(self.wrap(node, Value::new(ch).into()));
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
                    if let Some(_) = rest.strip_prefix(name) {
                        return Ok(self.wrap(node, Value::new(*character).into()));
                    }
                }

                let character =
                    rest.chars()
                        .next()
                        .ok_or_else(|| LexicalError::InvalidCharacter {
                            source: src.0,
                            character: ' ',
                        })?;

                return Ok(self.wrap(node, Value::new(character).into()));
            }

            "vector" => {
                let vec = self.parse_compound_node(node, src, CompoundType::Vector)?;
                Ok(self.wrap(node, vec))
            }

            "#u8" => {
                let bv = self.parse_compound_node(node, src, CompoundType::Bytevector)?;
                Ok(self.wrap(node, bv))
            }

            "#vu8" => {
                let bv = self.parse_compound_node(node, src, CompoundType::Bytevector)?;
                Ok(self.wrap(node, bv))
            }

            "directive" => {
                return Ok(Value::null());
            }

            _ => todo!(
                "Unhandled node kind: {}: {} at {} in {}",
                node.kind(),
                self.text_of(node),
                node.start_position().row,
                self.source_file
            ),
        }
    }

    pub fn wrap(&self, node: &Node, val: Value<'gc>) -> Value<'gc> {
        if !self.wrap_stx || val.is::<Syntax>() {
            return val;
        }
        global!(
            empty_wrap<'gc>: Value<'gc> = (ctx) list!(ctx, Value::null());
        );
        let filename = self.source_file;
        let line = node.start_position().row as i32;
        let column = node.start_position().column as i32;
        let v = Vector::from_slice(&self.ctx, &[filename, line.into(), column.into()]);

        let stx = Syntax::new(
            self.ctx,
            val,
            empty_wrap(self.ctx),
            Value::new(false),
            v.into(),
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
