use lsp_types::Range;

use crate::{
    document::range_for_bytes,
    protocol::{DocumentFacts, SymbolFact, SymbolKindFact},
};

use super::syntax::tokenize_code;

pub(crate) fn import_symbols(text: &str, facts: &DocumentFacts) -> Vec<SymbolFact> {
    facts
        .imports
        .iter()
        .map(|import| {
            let range = if import.range != Range::default() {
                import.range
            } else {
                range_for_import_name(text, &import.name).unwrap_or_default()
            };
            SymbolFact {
                name: import.name.clone(),
                kind: SymbolKindFact::Module,
                range,
                selection_range: range,
                detail: import_detail(import),
                container: None,
            }
        })
        .collect()
}

fn import_detail(import: &crate::protocol::ImportFact) -> Option<String> {
    if let Some(file) = &import.file {
        Some(format!("import: {file}"))
    } else if let Some(error) = &import.error {
        Some(format!("unresolved import: {error}"))
    } else if !import.resolved {
        Some("unresolved import".into())
    } else {
        Some("import".into())
    }
}

fn range_for_import_name(text: &str, name: &str) -> Option<Range> {
    let parts = name.split(' ').collect::<Vec<_>>();
    if parts.is_empty() {
        return None;
    }

    let tokens = tokenize_code(text);
    for idx in 0..tokens.len() {
        if tokens.get(idx)?.text != "(" || tokens.get(idx + 1)?.text != "import" {
            continue;
        }
        for candidate in (idx + 2)..tokens.len() {
            let end = candidate + parts.len();
            if end > tokens.len() {
                break;
            }
            if tokens[candidate..end]
                .iter()
                .map(|token| token.text.as_str())
                .eq(parts.iter().copied())
            {
                return Some(range_for_bytes(
                    text,
                    tokens[candidate].start,
                    tokens[end - 1].end,
                ));
            }
        }
    }
    None
}
