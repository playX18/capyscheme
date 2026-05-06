use crate::protocol::{DocumentFacts, SymbolFact, SymbolKindFact};

pub(crate) fn import_symbols(facts: &DocumentFacts) -> Vec<SymbolFact> {
    facts
        .imports
        .iter()
        .map(|import| {
            let range = import.range;
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
