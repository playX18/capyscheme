pub mod ast;

pub mod number;
pub mod parser;
pub mod source;

use std::marker::PhantomData;
use std::rc::Rc;

//pub mod parser;
use ariadne::Label;
use ariadne::ReportKind;
use rsgc::Trace;
pub use tree_sitter::Point;
pub use tree_sitter::Range;

use ast::*;
use number::NumberParseError;
use source::SourceManager;
use source::Span;

#[derive(Debug, Clone)]
pub enum SyntaxError {
    Expected {
        span: Span,
        expected: String,
        found: String,
    },

    Invalid {
        span: Span,
        message: String,
    },

    MacroMismatchedRepetitionPatterns {
        span: Span,
        expected: String,
    },

    NoExpansion {
        span: Span,
    },
}

impl SyntaxError {
    pub fn report(&self) -> ariadne::Report<'_, Span> {
        let mut report = ariadne::Report::build(ReportKind::Error, Span::default());
        match self {
            SyntaxError::Expected {
                span,
                expected,
                found,
            } => {
                report = report
                    .with_message(format!("Syntax error occured"))
                    .with_label(
                        Label::new(*span)
                            .with_message(format!("Expected {}, found {}", expected, found)),
                    );
            }

            SyntaxError::Invalid { span, message } => {
                report = report
                    .with_message(format!("Syntax error occured"))
                    .with_label(Label::new(*span).with_message(message.clone()));
            }

            SyntaxError::MacroMismatchedRepetitionPatterns { span, expected } => {
                report = report
                    .with_message(format!("Macro repetition patterns mismatch"))
                    .with_label(Label::new(*span).with_message(format!("unexpected {expected}",)));
            }

            SyntaxError::NoExpansion { span } => {
                report = report
                    .with_message("Macro expansion error")
                    .with_label(Label::new(*span).with_message("No expansion found for input"));
            }
        }
        report.finish()
    }
}

#[derive(Debug, Clone)]
pub enum LexicalError {
    InvalidCharacter {
        span: Span,
        character: char,
    },
    UnclosedString {
        span: Span,
    },
    UnclosedList {
        start_at: Span,
        end_at: Span,
    },

    ImproperListMultiple {
        span: Span,
        dot_spans: Vec<Span>,
        multi_tails: Vec<Span>,
    },

    ImproperListMissingTail {
        span: Span,
    },

    InvalidSyntax {
        span: Span,
    },

    InvalidNumber {
        span: Span,
        error: NumberParseError,
    },

    ExpectedPair {
        span: Span,
    },
}

impl LexicalError {
    pub fn report(&self) -> ariadne::Report<'_, Span> {
        let mut report = ariadne::Report::build(ReportKind::Error, Span::default());
        match self {
            LexicalError::ExpectedPair { span } => {
                report = report
                    .with_message("Expected a pair here")
                    .with_label(Label::new(*span));
            }

            LexicalError::InvalidCharacter { span, character } => {
                report = report
                    .with_message(format!("Invalid character: '{}'", character))
                    .with_label(Label::new(*span).with_message("Invalid character found here"));
            }

            LexicalError::UnclosedString { span } => {
                report = report
                    .with_message("Unclosed string found")
                    .with_label(Label::new(*span));
            }

            LexicalError::UnclosedList { start_at, end_at } => {
                report = report
                    .with_message("Unclosed list found")
                    .with_label(Label::new(*start_at).with_message("List started here"))
                    .with_label(Label::new(*end_at).with_message("List should have ended here"));
            }

            LexicalError::ImproperListMultiple {
                span,
                dot_spans,
                multi_tails,
            } => {
                report = report.with_message("Improper list has multiple '.' or multiple tail values but only one expected")
                    .with_label(Label::new(*span));
                for dot_span in dot_spans {
                    report =
                        report.with_label(Label::new(*dot_span).with_message("Dot found here"));
                }

                for multi_tail in multi_tails {
                    report = report
                        .with_label(Label::new(*multi_tail).with_message("Tail value found here"));
                }
            }

            LexicalError::ImproperListMissingTail { span } => {
                report = report
                    .with_message("Improper list missing tail")
                    .with_label(Label::new(*span));
            }

            LexicalError::InvalidSyntax { span } => {
                report = report
                    .with_message("Invalid syntax")
                    .with_label(Label::new(*span));
            }

            LexicalError::InvalidNumber { span, error } => {
                report = report
                    .with_message(format!("Invalid number: {}", error))
                    .with_label(Label::new(*span));
            }
        }
        report.finish()
    }
}

#[derive(Debug, Clone)]
pub enum Error<'gc> {
    Multiple(Vec<Box<Error<'gc>>>),
    Io(Rc<std::io::Error>),

    Lexical(LexicalError),

    Syntax(SyntaxError),

    Unused(PhantomData<&'gc ()>),
}

unsafe impl<'gc> Trace for Error<'gc> {
    unsafe fn trace(&mut self, visitor: &mut rsgc::collection::Visitor) {
        let _ = visitor;
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut rsgc::WeakProcessor) {}
}

impl<'gc> PartialEq for Error<'gc> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }
}

impl<'gc> Eq for Error<'gc> {}

impl<'gc> Error<'gc> {
    pub fn build_reports(&'gc self, result: &mut Vec<ariadne::Report<'gc, Span>>) {
        match self {
            Error::Syntax(syntax_error) => {
                result.push(syntax_error.report());
            }

            Error::Io(io_error) => {
                result.push(
                    ariadne::Report::build(ReportKind::Error, Span::default())
                        .with_message(format!("I/O error: {}", io_error))
                        .finish(),
                );
            }

            Error::Multiple(errors) => {
                for error in errors {
                    error.build_reports(result);
                }
            }

            Error::Lexical(lexical_error) => {
                result.push(lexical_error.report());
            }

            _ => unreachable!(),
        }
    }

    pub fn io(io_error: std::io::Error) -> Self {
        Error::Io(Rc::new(io_error))
    }

    pub fn lexical(lexical_error: LexicalError) -> Self {
        Error::Lexical(lexical_error)
    }

    pub fn syntax(syntax_error: SyntaxError) -> Self {
        Error::Syntax(syntax_error)
    }

    pub fn multiple(errors: Vec<Box<Error<'gc>>>) -> Self {
        Error::Multiple(errors)
    }

    pub fn single(error: Box<Error<'gc>>) -> Self {
        Error::Multiple(vec![error])
    }

    pub fn append(self, other: Self) -> Self {
        match (self, other) {
            (Error::Multiple(mut errors1), Error::Multiple(errors2)) => {
                errors1.extend(errors2);
                Error::Multiple(errors1)
            }
            (Error::Multiple(mut errors), other) => {
                errors.push(Box::new(other));
                Error::Multiple(errors)
            }
            (other, Error::Multiple(mut errors)) => {
                errors.insert(0, Box::new(other));
                Error::Multiple(errors)
            }
            (left, right) => Error::Multiple(vec![Box::new(left), Box::new(right)]),
        }
    }
}

pub fn handle_error_and_exit(error: Box<Error>, src: &SourceManager) -> ! {
    let mut reports = Vec::new();
    error.build_reports(&mut reports);

    for report in reports {
        report.eprint(src).unwrap();
    }

    std::process::exit(1);
}
