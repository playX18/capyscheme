use crate::ast::{Identifier, Symbol};

pub enum Binding {
    Begin,
    Define,
    DefineSyntax,
    DefineSyntaxParameter,
    LocalSyntax,
    EvalWhen,
    Global,
    Lexical(P<Symbol>),
    Ellispsi(P<Identifier>),
}