use rsgc::{Gc, Trace};
use tree_sitter::Node;

use crate::{
    frontend::num::{NumberParseError, parse_number},
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

    pub fn annotate(
        &self,
        expression: Value<'gc>,
        stripped: Value<'gc>,
        node: &Node,
    ) -> Gc<'gc, Annotation<'gc>> {
        let start_point = node.start_position();
        let end_point = node.end_position();

        Gc::new(
            &self.ctx,
            Annotation {
                header: ScmHeader::with_type_bits(TypeCode8::ANNOTATION.bits() as u16),
                expression,
                stripped,
                source: self.source_file,
                start_point: (start_point.row as u32, start_point.column as u32),
                end_point: (end_point.row as u32, end_point.column as u32),
            },
        )
    }

    pub fn text_of(&self, node: &Node) -> &str {
        node.utf8_text(self.text.as_bytes())
            .expect("Failed to get text from node")
    }

    pub fn is_symbol_of(&self, node: &Node, symbol: &str) -> bool {
        node.kind() == "symbol" && self.text_of(node) == symbol
    }

    pub fn read_program(&self) -> Result<Vec<(Value<'gc>, Value<'gc>)>, LexicalError<'gc>> {
        let root_node = self.tree.root_node();
        assert_eq!(root_node.kind(), "program", "Root node is not a program");
        let mut cursor = self.tree.walk();

        let mut result = Vec::new();
        for child in root_node.children(&mut cursor) {
            result.push(self.parse_lexeme(&child)?);
        }

        Ok(result)
    }

    pub fn parse_compound_node(
        &self,
        node: &Node,
        src: ((u32, u32), (u32, u32)),
        terminator: &str,
        typ: CompoundType,
    ) -> Result<(Value<'gc>, Value<'gc>), LexicalError<'gc>> {
        let mut cursor = node.walk();
        let mut children = node.children(&mut cursor).skip(1);

        let mut head = Value::null();
        let mut head_stripped = Value::null();

        let mut prev = Value::null();
        let mut prev_stripped = Value::null();

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
                            let vec_stripped = Value::list_to_vector(head_stripped, self.ctx);
                            let annotate = self.annotate(vec.into(), vec_stripped.into(), &child);
                            return Ok((annotate.into(), vec_stripped.into()));
                        }

                        CompoundType::Bytevector => {
                            let len = Value::list_length(head);

                            let bv = ByteVector::new::<false>(&self.ctx, len);

                            let mut vec = Vec::with_capacity(len);

                            let mut current = head_stripped;

                            while !current.is_null() {
                                let byte = current.as_int32();
                                vec.push(byte as u8);
                                current = current.cdr();
                            }
                            unsafe {
                                bv.as_slice_mut_unchecked().copy_from_slice(&vec);
                            }

                            let bv_anon = self.annotate(bv.into(), bv.into(), &child);

                            return Ok((bv_anon.into(), bv.into()));
                        }

                        CompoundType::List => {
                            let annotated = self.annotate(head, head_stripped, &child);
                            return Ok((annotated.into(), head_stripped.into()));
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

                            let (tail, tail_stripped) = self.parse_lexeme(&next)?;

                            if prev.is_pair() {
                                prev.set_cdr(self.ctx, tail);
                                prev_stripped.set_cdr(self.ctx, tail_stripped);
                            }

                            return Ok((
                                self.annotate(head, head_stripped, &next).into(),
                                head_stripped,
                            ));
                        } else {
                            return Err(LexicalError::InvalidSyntax { span: src.0 });
                        }
                    }

                    _ => {
                        let (lexeme, lexeme_stripped) = self.parse_lexeme(&child)?;

                        let new_prev = Value::cons(self.ctx, lexeme, Value::null());
                        let new_prev_stripped =
                            Value::cons(self.ctx, lexeme_stripped, Value::null());

                        if prev.is_pair() {
                            prev.set_cdr(self.ctx, new_prev);
                            prev_stripped.set_cdr(self.ctx, new_prev_stripped);
                        }
                        prev = new_prev;
                        prev_stripped = new_prev_stripped;

                        if !head.is_pair() {
                            head = new_prev;
                            head_stripped = new_prev_stripped;
                        }

                        continue;
                    }
                },
            }
        }
    }

    fn parse_lexeme(&self, node: &Node) -> Result<(Value<'gc>, Value<'gc>), LexicalError<'gc>> {
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
            "symbol" => {
                let symbol = Symbol::from_str(self.ctx, text);
                let annotated = self.annotate(symbol.into(), symbol.into(), node);
                Ok((annotated.into(), symbol.into()))
            }

            "string" => {
                let string = Str::new(&self.ctx, text, false);
                let annotated = self.annotate(string.into(), string.into(), node);
                Ok((annotated.into(), string.into()))
            }

            "number" => {
                let n = parse_number(text)
                    .map_err(|e| LexicalError::InvalidNumber {
                        span: src.0,
                        error: e,
                    })?
                    .to_vm_number(self.ctx)
                    .into_value(self.ctx);

                let annotated = self.annotate(n.into(), n.into(), node);
                Ok((annotated.into(), n.into_value(self.ctx)))
            }

            "list" => {
                let (list, stripped) =
                    self.parse_compound_node(node, src, ")", CompoundType::List)?;
                Ok((list, stripped))
            }

            "vector" => {
                let (vec, stripped) =
                    self.parse_compound_node(node, src, "]", CompoundType::Vector)?;
                Ok((vec, stripped))
            }

            _ => Err(LexicalError::InvalidSyntax { span: src.0 }),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompoundType {
    Vector,
    List,
    Bytevector,
}
