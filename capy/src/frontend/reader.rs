use std::fmt::Display;

use rsgc::Trace;
use tree_sitter::Node;

use crate::{
    expander::add_source,
    frontend::num::{NumberParseError, parse_number},
    list,
    runtime::{
        Context,
        value::{ByteVector, IntoValue, ScmHeader, Str, Symbol, Tagged, TypeCode8, Value},
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
}

impl<'a, 'gc> TreeSitter<'a, 'gc> {
    pub fn new(ctx: Context<'gc>, text: &'a str, source_file: Value<'gc>) -> Self {
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&tree_sitter::Language::new(tree_sitter_scheme::LANGUAGE))
            .unwrap();

        let tree = parser.parse(text, None).unwrap();

        Self {
            ctx,
            text,
            tree,
            source_file,
        }
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
        assert_eq!(root_node.kind(), "program", "Root node is not a program");
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
                            return Ok(vec.into());
                        }

                        CompoundType::Bytevector => {
                            let len = Value::list_length(head);

                            let bv = ByteVector::new::<false>(&self.ctx, len);

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

                            return Ok(bv.into());
                        }

                        CompoundType::List => {
                            self.annotate(head, &child);
                            return Ok(head.into());
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

                            return Ok(head);
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

                Ok(symbol.into())
            }

            "string" => {
                let text = &text[1..text.len() - 1];
                let string = Str::new(&self.ctx, text, false);

                Ok(string.into())
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

                Ok(value)
            }

            "quote" => {
                let mut cursor = node.walk();
                let child = node.children(&mut cursor).skip(1).next().unwrap();

                let expr = self.parse_lexeme(&child)?;

                self.annotate(expr, &node);

                let quote: Value<'gc> = Symbol::from_str(self.ctx, "quote").into();
                Ok(list!(self.ctx, quote, expr).into())
            }

            "syntax" => {
                let mut cursor = node.walk();
                let child = node.children(&mut cursor).skip(1).next().unwrap();

                let expr = self.parse_lexeme(&child)?;

                self.annotate(expr, &node);

                let syntax: Value<'gc> = Symbol::from_str(self.ctx, "syntax").into();
                Ok(list!(self.ctx, syntax, expr).into())
            }

            "quasisyntax" => {
                let mut cursor = node.walk();
                let child = node.children(&mut cursor).skip(1).next().unwrap();

                let expr = self.parse_lexeme(&child)?;

                self.annotate(expr, &node);

                let quasisyntax: Value<'gc> = Symbol::from_str(self.ctx, "quasisyntax").into();
                Ok(list!(self.ctx, quasisyntax, expr).into())
            }

            "unsyntax" => {
                let mut cursor = node.walk();
                let child = node.children(&mut cursor).skip(1).next().unwrap();

                let expr = self.parse_lexeme(&child)?;

                self.annotate(expr, &node);

                let unsyntax: Value<'gc> = Symbol::from_str(self.ctx, "unsyntax").into();
                Ok(list!(self.ctx, unsyntax, expr).into())
            }

            "unsyntax_splicing" => {
                let mut cursor = node.walk();
                let child = node.children(&mut cursor).skip(1).next().unwrap();

                let expr = self.parse_lexeme(&child)?;

                self.annotate(expr, &node);

                let unsyntax_splicing: Value<'gc> =
                    Symbol::from_str(self.ctx, "unsyntax-splicing").into();
                Ok(list!(self.ctx, unsyntax_splicing, expr).into())
            }

            "quasiquote" => {
                let mut cursor = node.walk();
                let child = node.children(&mut cursor).skip(1).next().unwrap();

                let expr = self.parse_lexeme(&child)?;

                self.annotate(expr, &node);

                let quasiquote: Value<'gc> = Symbol::from_str(self.ctx, "quasiquote").into();
                Ok(list!(self.ctx, quasiquote, expr).into())
            }

            "unquote" => {
                let mut cursor = node.walk();
                let child = node.children(&mut cursor).skip(1).next().unwrap();

                let expr = self.parse_lexeme(&child)?;

                self.annotate(expr, &node);

                let unquote: Value<'gc> = Symbol::from_str(self.ctx, "unquote").into();
                Ok(list!(self.ctx, unquote, expr).into())
            }

            "unquote_splicing" => {
                let mut cursor = node.walk();
                let child = node.children(&mut cursor).skip(1).next().unwrap();

                let expr = self.parse_lexeme(&child)?;

                self.annotate(expr, &node);

                let unquote_splicing: Value<'gc> =
                    Symbol::from_str(self.ctx, "unquote-splicing").into();
                Ok(list!(self.ctx, unquote_splicing, expr).into())
            }

            "list" => {
                let list = self.parse_compound_node(node, src, CompoundType::List)?;
                Ok(list)
            }

            "character" => {
                let rest = text.strip_prefix("#\\").unwrap();

                if let Some(hex_part) = rest.strip_prefix("x") {
                    let end_of_hex = hex_part
                        .find(|c: char| !c.is_ascii_hexdigit())
                        .unwrap_or(hex_part.len());

                    if end_of_hex == 0 {
                        return Err(LexicalError::InvalidCharacter {
                            source: src.0,
                            character: 'x',
                        });
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

                    return Ok(Value::new(ch).into());
                }

                const NAMED_CHARS: &[(&str, char)] = &[
                    ("newline", '\n'),
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
                        return Ok(Value::new(*character));
                    }
                }

                let character =
                    rest.chars()
                        .next()
                        .ok_or_else(|| LexicalError::InvalidCharacter {
                            source: src.0,
                            character: ' ',
                        })?;

                return Ok(Value::new(character).into());
            }

            "vector" => {
                let vec = self.parse_compound_node(node, src, CompoundType::Vector)?;
                Ok(vec)
            }

            _ => todo!("Unhandled node kind: {}", node.kind()),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompoundType {
    Vector,
    List,
    Bytevector,
}
