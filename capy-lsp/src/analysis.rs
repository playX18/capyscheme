use std::collections::{HashMap, HashSet};

use lsp_types::{Range, Url};

mod completions;
mod diagnostics;
mod imports;
mod symbols;
mod syntax;

use crate::{
    document::{range_for_bytes, word_at_position},
    protocol::{DocumentFacts, SymbolFact},
};

use completions::{append_keyword_completions, append_symbol_completions};
use diagnostics::balance_diagnostics;
pub(crate) use imports::import_symbols;
use symbols::{collect_references, collect_symbols};
use syntax::{is_identifier, tokenize_code};

pub fn analyze_syntax(uri: &Url, text: &str) -> DocumentFacts {
    let mut facts = DocumentFacts::default();
    facts.diagnostics = balance_diagnostics(text);
    facts.symbols = collect_symbols(text);
    append_symbol_completions(&mut facts);
    append_keyword_completions(&mut facts);
    facts.references = collect_references(uri, text, &facts.symbols);
    facts
}

pub fn fill_missing_facts(uri: &Url, text: &str, facts: &mut DocumentFacts) {
    let needs_symbol_ranges = symbols_have_default_ranges(&facts.symbols);
    let needs_references = facts.references.is_empty();
    let needs_completions = facts.completions.is_empty();

    let fallback = if needs_symbol_ranges || needs_references || needs_completions {
        Some(analyze_syntax(uri, text))
    } else {
        None
    };

    if let Some(fallback) = &fallback {
        if facts.symbols.is_empty() && !fallback.symbols.is_empty() {
            facts.symbols = fallback.symbols.clone();
        } else if needs_symbol_ranges {
            fill_symbol_ranges(text, &mut facts.symbols, &fallback.symbols);
            append_missing_symbols(&mut facts.symbols, &fallback.symbols);
        }
    }

    if needs_references {
        facts.references = collect_references(uri, text, &facts.symbols);
        if facts.references.is_empty()
            && let Some(fallback) = &fallback
        {
            facts.references = fallback.references.clone();
        }
    } else {
        append_unscoped_fallback_references(uri, text, facts);
    }

    if needs_completions {
        append_symbol_completions(facts);
        if facts.completions.is_empty() {
            if let Some(fallback) = &fallback {
                facts.completions = fallback.completions.clone();
            }
        }
    }

    append_symbol_completions(facts);
    append_keyword_completions(facts);
}

fn append_unscoped_fallback_references(uri: &Url, text: &str, facts: &mut DocumentFacts) {
    let scoped_names = facts
        .references
        .iter()
        .map(|reference| reference.name.clone())
        .collect::<HashSet<_>>();
    let mut seen = facts
        .references
        .iter()
        .map(|reference| (reference.name.clone(), reference.range))
        .collect::<HashSet<_>>();
    for reference in collect_references(uri, text, &facts.symbols) {
        if scoped_names.contains(&reference.name) {
            continue;
        }
        if seen.insert((reference.name.clone(), reference.range)) {
            facts.references.push(reference);
        }
    }
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

fn symbols_have_default_ranges(symbols: &[SymbolFact]) -> bool {
    symbols
        .iter()
        .any(|symbol| placeholder_range(symbol.range) || placeholder_range(symbol.selection_range))
}

fn placeholder_range(range: Range) -> bool {
    range == Range::default()
}

fn fill_symbol_ranges(text: &str, symbols: &mut [SymbolFact], fallback_symbols: &[SymbolFact]) {
    let mut token_ranges = identifier_ranges(text);
    let mut token_indices = HashMap::<String, usize>::new();

    for symbol in symbols {
        if !placeholder_range(symbol.range) && !placeholder_range(symbol.selection_range) {
            continue;
        }

        if let Some(fallback) = fallback_symbols.iter().find(|fallback| {
            fallback.name == symbol.name
                && fallback.kind == symbol.kind
                && !placeholder_range(fallback.selection_range)
        }) {
            symbol.range = fallback.range;
            symbol.selection_range = fallback.selection_range;
            continue;
        }

        let ranges = token_ranges.remove(&symbol.name).unwrap_or_default();
        let next = token_indices.entry(symbol.name.clone()).or_default();
        if let Some(range) = ranges
            .get(*next)
            .copied()
            .or_else(|| ranges.first().copied())
        {
            symbol.range = range;
            symbol.selection_range = range;
            *next += 1;
        }
        if !ranges.is_empty() {
            token_ranges.insert(symbol.name.clone(), ranges);
        }
    }
}

fn append_missing_symbols(symbols: &mut Vec<SymbolFact>, fallback_symbols: &[SymbolFact]) {
    let mut seen = symbols
        .iter()
        .map(|symbol| (symbol.name.clone(), symbol.kind))
        .collect::<HashSet<_>>();
    for symbol in fallback_symbols {
        if seen.insert((symbol.name.clone(), symbol.kind)) {
            symbols.push(symbol.clone());
        }
    }
}

fn identifier_ranges(text: &str) -> HashMap<String, Vec<Range>> {
    let mut ranges = HashMap::<String, Vec<Range>>::new();
    for token in tokenize_code(text) {
        if is_identifier(&token.text) {
            ranges
                .entry(token.text.clone())
                .or_default()
                .push(range_for_bytes(text, token.start, token.end));
        }
    }
    ranges
}

#[cfg(test)]
mod tests {
    use lsp_types::Url;

    use crate::protocol::{DocumentFacts, SymbolFact, SymbolKindFact};

    use super::{analyze_syntax, fill_missing_facts};

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
                .all(|symbol| symbol.range != Default::default())
        );
    }

    #[test]
    fn preserves_tree_il_local_completions_when_filling_ranges() {
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
                .all(|symbol| symbol.range != Default::default())
        );
    }
}
