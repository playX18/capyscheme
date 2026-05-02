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
            let range = range_for_import_name(text, &import.name).unwrap_or_default();
            SymbolFact {
                name: import.name.clone(),
                kind: SymbolKindFact::Module,
                range,
                selection_range: range,
                detail: import
                    .file
                    .as_ref()
                    .map(|file| format!("import: {file}"))
                    .or_else(|| Some("unresolved import".into())),
                container: None,
            }
        })
        .collect()
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
