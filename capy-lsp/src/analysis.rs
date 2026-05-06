use lsp_types::{Range, Url};

mod completions;
#[cfg(test)]
mod diagnostics;
mod imports;
#[cfg(test)]
mod symbols;
#[cfg(test)]
mod syntax;

use crate::{document::word_at_position, protocol::DocumentFacts};

use completions::{append_keyword_completions, append_symbol_completions};
#[cfg(test)]
use diagnostics::balance_diagnostics;
pub(crate) use imports::import_symbols;
#[cfg(test)]
use symbols::{collect_references, collect_symbols};

#[cfg(test)]
pub fn analyze_syntax(uri: &Url, text: &str) -> DocumentFacts {
    let mut facts = DocumentFacts::default();
    facts.diagnostics = balance_diagnostics(text);
    facts.symbols = collect_symbols(text);
    append_symbol_completions(&mut facts);
    append_keyword_completions(&mut facts);
    facts.references = collect_references(uri, text, &facts.symbols);
    facts
}

pub fn fill_missing_facts(_uri: &Url, _text: &str, facts: &mut DocumentFacts) {
    if facts.completions.is_empty() {
        append_symbol_completions(facts);
    }

    append_symbol_completions(facts);
    append_keyword_completions(facts);
}

pub fn symbol_at(
    facts: &DocumentFacts,
    text: &str,
    position: lsp_types::Position,
) -> Option<(String, Range)> {
    let (name, range) = word_at_position(text, position)?;
    if facts.symbols.iter().any(|symbol| symbol.name == name)
        || facts
            .references
            .iter()
            .any(|reference| reference.name == name)
    {
        Some((name, range))
    } else {
        Some((name, range))
    }
}

#[cfg(test)]
mod tests {
    use lsp_types::{Position, Range, Url};

    use crate::protocol::{DocumentFacts, ImportFact, SymbolFact, SymbolKindFact};

    use super::{analyze_syntax, fill_missing_facts, import_symbols};

    #[test]
    fn finds_define_symbols_and_references() {
        let uri = Url::parse("file:///tmp/test.scm").unwrap();
        let facts = analyze_syntax(&uri, "(define (square x) (* x x))\n(square 4)\n");
        assert!(facts.symbols.iter().any(|symbol| symbol.name == "square"));
        assert!(
            facts
                .references
                .iter()
                .any(|reference| reference.name == "square")
        );
    }

    #[test]
    fn reports_unclosed_lists() {
        let uri = Url::parse("file:///tmp/test.scm").unwrap();
        let facts = analyze_syntax(&uri, "(define x 1\n");
        assert_eq!(facts.diagnostics.len(), 1);
    }

    #[test]
    fn fills_missing_completions_from_symbols() {
        let uri = Url::parse("file:///tmp/test.scm").unwrap();
        let mut facts = DocumentFacts {
            symbols: vec![SymbolFact {
                name: "answer".into(),
                kind: SymbolKindFact::Variable,
                range: Default::default(),
                selection_range: Default::default(),
                detail: Some("define".into()),
                container: None,
            }],
            ..DocumentFacts::default()
        };

        fill_missing_facts(&uri, "(define answer 42)\n", &mut facts);

        assert!(facts.completions.iter().any(|item| item.label == "answer"));
        assert!(facts.symbols.iter().any(|symbol| symbol.name == "answer"));
        assert!(
            facts
                .symbols
                .iter()
                .all(|symbol| symbol.range == Default::default())
        );
    }

    #[test]
    fn preserves_tree_il_local_completions_without_source_range_repair() {
        let uri = Url::parse("file:///tmp/test.scm").unwrap();
        let text = "(define (square x) (let ((y x)) (+ y x)))\n";
        let mut facts = DocumentFacts {
            symbols: vec![
                SymbolFact {
                    name: "square".into(),
                    kind: SymbolKindFact::Function,
                    range: Default::default(),
                    selection_range: Default::default(),
                    detail: Some("define".into()),
                    container: None,
                },
                SymbolFact {
                    name: "x".into(),
                    kind: SymbolKindFact::Variable,
                    range: Default::default(),
                    selection_range: Default::default(),
                    detail: Some("lambda parameter".into()),
                    container: None,
                },
                SymbolFact {
                    name: "y".into(),
                    kind: SymbolKindFact::Variable,
                    range: Default::default(),
                    selection_range: Default::default(),
                    detail: Some("let binding".into()),
                    container: None,
                },
            ],
            ..DocumentFacts::default()
        };

        fill_missing_facts(&uri, text, &mut facts);

        assert!(facts.completions.iter().any(|item| item.label == "x"));
        assert!(facts.completions.iter().any(|item| item.label == "y"));
        assert!(
            facts
                .symbols
                .iter()
                .filter(|symbol| symbol.name == "x" || symbol.name == "y")
                .all(|symbol| symbol.range == Default::default())
        );
    }

    #[test]
    fn import_symbols_prefer_worker_ranges() {
        let worker_range = Range {
            start: Position {
                line: 3,
                character: 4,
            },
            end: Position {
                line: 3,
                character: 12,
            },
        };
        let facts = DocumentFacts {
            imports: vec![ImportFact {
                name: "demo lib".into(),
                range: worker_range,
                resolved: true,
                error: None,
                file: Some("/tmp/demo/lib.sls".into()),
            }],
            ..DocumentFacts::default()
        };

        let symbols = import_symbols(&facts);

        assert_eq!(symbols[0].range, worker_range);
        assert_eq!(symbols[0].selection_range, worker_range);
    }

    #[test]
    fn import_symbols_do_not_infer_missing_ranges_from_text() {
        let facts = DocumentFacts {
            imports: vec![ImportFact {
                name: "demo lib".into(),
                range: Range::default(),
                resolved: true,
                error: None,
                file: None,
            }],
            ..DocumentFacts::default()
        };

        let symbols = import_symbols(&facts);

        assert_eq!(symbols[0].range, Range::default());
        assert_eq!(symbols[0].selection_range, Range::default());
    }
}
