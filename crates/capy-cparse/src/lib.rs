//! C99–C26 lexer, preprocessor, parser and layout engine for generating FFI bindings.

pub mod ast;
pub mod consteval;
pub mod layout;
pub mod lex;
pub mod parse;

pub mod driver;
pub mod pp;
pub mod probe;
pub mod emit;
pub mod token;
pub mod types;
