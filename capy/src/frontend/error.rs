use std::fmt::Display;

use crate::prelude::Value;

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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NumberParseError {
    InvalidFormat(String),
    InvalidRadix(String),
    InvalidExactness(String),
    InvalidDigit(char, u32),
    EmptyNumber,
    MultipleDecimalPoints,
    InvalidComplex(String),
}

impl std::fmt::Display for NumberParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NumberParseError::InvalidFormat(s) => write!(f, "Invalid number format: {}", s),
            NumberParseError::InvalidRadix(s) => write!(f, "Invalid radix: {}", s),
            NumberParseError::InvalidExactness(s) => write!(f, "Invalid exactness: {}", s),
            NumberParseError::InvalidDigit(c, radix) => {
                write!(f, "Invalid digit '{}' for radix {}", c, radix)
            }
            NumberParseError::EmptyNumber => write!(f, "Empty number"),
            NumberParseError::MultipleDecimalPoints => {
                write!(f, "Multiple decimal points in number")
            }
            NumberParseError::InvalidComplex(s) => write!(f, "Invalid complex number: {}", s),
        }
    }
}

impl std::error::Error for NumberParseError {}
