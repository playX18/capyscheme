use std::collections::HashSet;

use crate::protocol::{CompletionFact, DocumentFacts, SymbolFact, SymbolKindFact};

pub(super) fn append_symbol_completions(facts: &mut DocumentFacts) {
    let mut seen = facts
        .completions
        .iter()
        .map(|item| item.label.clone())
        .collect::<HashSet<_>>();
    for symbol in &facts.symbols {
        if seen.insert(symbol.name.clone()) {
            facts.completions.push(completion_from_symbol(symbol));
        }
    }
}

pub(super) fn append_keyword_completions(facts: &mut DocumentFacts) {
    const KEYWORDS: &[&str] = &[
        "and",
        "begin",
        "case",
        "cond",
        "define",
        "define-library",
        "define-record-type",
        "define-syntax",
        "else",
        "export",
        "if",
        "import",
        "lambda",
        "let",
        "let*",
        "let*-values",
        "let-values",
        "letrec",
        "letrec*",
        "library",
        "or",
        "quasiquote",
        "quote",
        "receive",
        "set!",
        "syntax-case",
        "syntax-rules",
        "unquote",
        "unless",
        "when",
    ];

    let mut seen = facts
        .completions
        .iter()
        .map(|item| item.label.clone())
        .collect::<HashSet<_>>();
    for keyword in KEYWORDS {
        if seen.insert((*keyword).into()) {
            facts.completions.push(CompletionFact {
                label: (*keyword).into(),
                detail: Some("syntax".into()),
                documentation: None,
                kind: SymbolKindFact::Keyword,
                source_module: None,
                source_file: None,
                source_name: None,
            });
        }
    }
}

fn completion_from_symbol(symbol: &SymbolFact) -> CompletionFact {
    CompletionFact {
        label: symbol.name.clone(),
        detail: symbol.detail.clone(),
        documentation: None,
        kind: symbol.kind,
        source_module: None,
        source_file: None,
        source_name: None,
    }
}
