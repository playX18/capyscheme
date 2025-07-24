use tree_sitter::*;

use crate::{
    Error, LexicalError,
    ast::{Datum, DatumValue, INTERNER, P, Symbol},
    number::parse_number,
    source::{SourceId, SourceManager, Span},
};

pub struct TreeSitter<'a> {
    #[allow(dead_code)]
    manager: &'a SourceManager,
    source_id: SourceId,
    text: &'a str,
    tree: Tree,
}

impl<'a> TreeSitter<'a> {
    pub fn root_span(&self) -> Span {
        let root_node = self.tree.root_node();
        Span::new(
            Some(self.source_id()),
            root_node.start_byte(),
            root_node.end_byte(),
            root_node.start_position(),
            root_node.end_position(),
        )
    }

    pub fn new(source: SourceId, manager: &'a SourceManager) -> Self {
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&tree_sitter::Language::new(tree_sitter_scheme::LANGUAGE))
            .unwrap();
        let text = manager.text_for(source).expect("Source text not found");
        let tree = parser.parse(text, None).unwrap();

        Self {
            source_id: source,
            manager,
            text,
            tree,
        }
    }

    pub fn text_of(&self, node: &Node) -> &str {
        node.utf8_text(self.text.as_bytes())
            .expect("Failed to get text of node")
    }

    pub fn source_id(&self) -> SourceId {
        self.source_id
    }

    pub fn span_for(&self, node: &Node) -> Span {
        Span::new(
            Some(self.source_id()),
            node.start_byte(),
            node.end_byte(),
            node.start_position(),
            node.end_position(),
        )
    }

    /// Parse entire Scheme program and return a vector of `Datum` nodes.
    ///
    /// Does not return `Result` because errors are resolved independently. Run `extract_errors`
    /// on the result of this function to get a list of errors.
    pub fn parse_program(&self) -> Vec<P<Datum>> {
        let root_node = self.tree.root_node();
        assert_eq!(root_node.kind(), "program", "Root node is not a program");
        let mut cursor = self.tree.walk();

        let mut result = Vec::new();
        for child in root_node.children(&mut cursor) {
            if let Some(datum) = self.parse_node(&child) {
                result.push(datum);
            }
        }

        result
    }

    pub fn is_symbol_of(&self, node: &Node, symbol: &str) -> bool {
        node.kind() == "symbol" && self.text_of(node) == symbol
    }

    pub fn parse_node(&self, node: &Node) -> Option<P<Datum>> {
        let span = self.span_for(node);

        match node.kind() {
            "comment" => None,
            "string" => {
                let text = self.text_of(node);
                return Some(Datum::new_at(
                    span,
                    DatumValue::Str(INTERNER.get_or_intern(text)),
                ));
            }
            "keyword" | "symbol" => {
                let text = self.text_of(node);
                return Some(Datum::new_at(span, DatumValue::Symbol(Symbol::new(text))));
            }

            "number" => {
                let text = self.text_of(node);
                match parse_number(text) {
                    Ok(number) => {
                        return Some(Datum::new_at(span, DatumValue::Number(number)));
                    }

                    Err(number_error) => {
                        return Some(Datum::new_at(
                            span,
                            DatumValue::Error {
                                error: Box::new(Error::lexical(LexicalError::InvalidNumber {
                                    span: span,
                                    error: number_error,
                                })),
                                exprs: vec![],
                            },
                        ));
                    }
                }
            }

            "list" => {
                let mut elements = Vec::new();
                let mut tail = None;
                let mut dot_found = false;
                let mut parse_error = false;

                let mut multi_tails = Vec::new();
                let mut multi_dots = Vec::new();

                let mut cursor = node.walk();

                for child_node in node.children(&mut cursor) {
                    if self.is_symbol_of(&child_node, ".") {
                        if dot_found {
                            multi_dots.push(self.span_for(&child_node));
                            parse_error = true;
                        }

                        dot_found = true;
                        continue;
                    }

                    if child_node.is_named() {
                        let current_datum = self.parse_node(&child_node);
                        match current_datum {
                            Some(datum) => {
                                if dot_found {
                                    if tail.is_some() {
                                        multi_tails.push(self.span_for(&child_node));
                                        parse_error = true;
                                    } else {
                                        tail = Some(datum);
                                    }
                                } else {
                                    elements.push(datum);
                                }
                            }
                            None => continue,
                        }
                    } else {
                        let kind = child_node.kind();
                        match kind {
                            "(" | ")" | "[" | "]" => {
                                // Ignore parentheses and brackets
                            }
                            "." => {
                                if dot_found {
                                    multi_dots.push(self.span_for(&child_node));
                                    parse_error = true;
                                } else {
                                    dot_found = true;
                                }
                            }

                            _ => {
                                multi_dots.push(self.span_for(&child_node));
                                parse_error = true;
                                break;
                            }
                        }
                    }
                }

                if parse_error {
                    if let Some(tail) = tail.take() {
                        elements.push(tail);
                    }
                    return Some(Datum::new_at(
                        span,
                        DatumValue::Error {
                            error: Box::new(Error::lexical(LexicalError::ImproperListMultiple {
                                span: span,
                                dot_spans: multi_dots,
                                multi_tails: multi_tails,
                            })),
                            exprs: elements,
                        },
                    ));
                }

                Some(Datum::make_list_with(
                    elements,
                    tail.unwrap_or(Datum::new_at(span, DatumValue::Null)),
                    Some(span),
                ))
            }

            "boolean" => {
                let text = self.text_of(node);
                let value = match text {
                    "#t" => DatumValue::True,
                    "#f" => DatumValue::False,
                    _ => unreachable!("Unexpected boolean text: {}", text),
                };
                Some(Datum::new_at(span, value))
            }

            "quote" => {
                let children = node.child(1).unwrap();
                let sym = Datum::new_at(span, DatumValue::Symbol(Symbol::new("quote")));

                let quoted_value = self.parse_node(&children);
                if let Some(quoted_value) = quoted_value {
                    Some(Datum::make_list(vec![sym, quoted_value], Some(span)))
                } else {
                    Some(Datum::new_at(
                        span,
                        DatumValue::Error {
                            error: Box::new(Error::lexical(LexicalError::InvalidSyntax { span })),
                            exprs: vec![],
                        },
                    ))
                }
            }

            "unquote" => {
                let children = node.child(1).unwrap();
                let sym = Datum::new_at(span, DatumValue::Symbol(Symbol::new("unquote")));

                let unquoted_value = self.parse_node(&children);
                if let Some(unquoted_value) = unquoted_value {
                    Some(Datum::make_list(vec![sym, unquoted_value], Some(span)))
                } else {
                    Some(Datum::new_at(
                        span,
                        DatumValue::Error {
                            error: Box::new(Error::lexical(LexicalError::InvalidSyntax { span })),
                            exprs: vec![],
                        },
                    ))
                }
            }

            "quasiquote" => {
                let children = node.child(1).unwrap();
                let sym = Datum::new_at(span, DatumValue::Symbol(Symbol::new("quasiquote")));

                let quasiquoted_value = self.parse_node(&children);
                if let Some(quasiquoted_value) = quasiquoted_value {
                    Some(Datum::make_list(vec![sym, quasiquoted_value], Some(span)))
                } else {
                    Some(Datum::new_at(
                        span,
                        DatumValue::Error {
                            error: Box::new(Error::lexical(LexicalError::InvalidSyntax { span })),
                            exprs: vec![],
                        },
                    ))
                }
            }

            "unquote_splicing" => {
                let children = node.child(1).unwrap();
                let sym = Datum::new_at(span, DatumValue::Symbol(Symbol::new("unquote-splicing")));

                let unquoted_value = self.parse_node(&children);
                if let Some(unquoted_value) = unquoted_value {
                    Some(Datum::make_list(vec![sym, unquoted_value], Some(span)))
                } else {
                    Some(Datum::new_at(
                        span,
                        DatumValue::Error {
                            error: Box::new(Error::lexical(LexicalError::InvalidSyntax { span })),
                            exprs: vec![],
                        },
                    ))
                }
            }

            _ => {
                let mut children = Vec::new();
                let mut cursor = node.walk();
                for child in node.children(&mut cursor).filter(|c| c.is_named()) {
                    children.push(self.parse_node(&child));
                }

                if node
                    .child(0)
                    .as_ref()
                    .map(|x| self.text_of(x) == "(" && !x.is_named())
                    .unwrap_or(false)
                {
                    let last = node.child(node.child_count() - 1);
                    let last_span = last.map_or(span, |n| self.span_for(&n));
                    Some(Datum::new_at(
                        span,
                        DatumValue::Error {
                            error: Box::new(Error::lexical(LexicalError::UnclosedList {
                                start_at: span,
                                end_at: last_span,
                            })),
                            exprs: children.into_iter().flatten().collect(),
                        },
                    ))
                } else {
                    Some(Datum::new_at(
                        span,
                        DatumValue::Error {
                            error: Box::new(Error::lexical(LexicalError::InvalidSyntax { span })),
                            exprs: children.into_iter().flatten().collect(),
                        },
                    ))
                }
            }
        }
    }
}
