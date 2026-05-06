use std::collections::HashMap;

use lsp_types::Url;

use crate::{config::LspConfig, document::Document, protocol::SymbolFact};

#[derive(Debug, Clone)]
pub(crate) struct WorkspaceIndex {
    pub(crate) workspace_epoch: u64,
    pub(crate) config_fingerprint: u64,
    symbols: Vec<IndexedSymbol>,
}

#[derive(Debug, Clone)]
pub(crate) struct IndexedSymbol {
    pub(crate) uri: Url,
    pub(crate) symbol: SymbolFact,
}

impl WorkspaceIndex {
    pub(crate) fn build(
        _config: &LspConfig,
        workspace_epoch: u64,
        config_fingerprint: u64,
        open_documents: &HashMap<Url, Document>,
    ) -> Self {
        let symbols = open_documents
            .iter()
            .filter_map(|(uri, document)| {
                document
                    .facts
                    .as_ref()
                    .filter(|facts| {
                        facts.matches(document.version, workspace_epoch, config_fingerprint)
                    })
                    .map(|facts| (uri, &facts.facts.symbols))
            })
            .flat_map(|(uri, symbols)| {
                symbols.iter().cloned().map(|symbol| IndexedSymbol {
                    uri: uri.clone(),
                    symbol,
                })
            })
            .collect();
        Self {
            workspace_epoch,
            config_fingerprint,
            symbols,
        }
    }

    pub(crate) fn matches(&self, workspace_epoch: u64, config_fingerprint: u64) -> bool {
        self.workspace_epoch == workspace_epoch && self.config_fingerprint == config_fingerprint
    }

    pub(crate) fn symbols(&self) -> &[IndexedSymbol] {
        &self.symbols
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use lsp_types::Url;
    use tempfile::tempdir;

    use crate::{
        config::LspConfig,
        document::{CachedDocumentFacts, Document},
        protocol::{DocumentFacts, SymbolFact, SymbolKindFact},
    };

    use super::WorkspaceIndex;

    #[test]
    fn does_not_tokenize_unopened_workspace_files() {
        let dir = tempdir().unwrap();

        let config = LspConfig::default_for_root(dir.path());
        let index = WorkspaceIndex::build(&config, 3, config.fingerprint(), &HashMap::new());

        assert!(index.matches(3, config.fingerprint()));
        assert!(index.symbols().is_empty());
    }

    #[test]
    fn indexes_current_cached_open_document_facts() {
        let dir = tempdir().unwrap();
        let path = dir.path().join("open.scm");
        let uri = Url::from_file_path(&path).unwrap();
        let config = LspConfig::default_for_root(dir.path());
        let fingerprint = config.fingerprint();
        let facts = DocumentFacts {
            symbols: vec![SymbolFact {
                name: "tree-il-name".into(),
                kind: SymbolKindFact::Variable,
                range: Default::default(),
                selection_range: Default::default(),
                detail: Some("define".into()),
                container: None,
            }],
            ..DocumentFacts::default()
        };
        let mut documents = HashMap::new();
        let mut document = Document::new(uri.clone(), 7, "(define ignored-source 2)\n".into());
        document.facts = Some(CachedDocumentFacts::new(facts, 7, 1, fingerprint));
        documents.insert(uri, document);

        let index = WorkspaceIndex::build(&config, 1, fingerprint, &documents);

        assert!(
            index
                .symbols()
                .iter()
                .any(|entry| entry.symbol.name == "tree-il-name")
        );
        assert!(
            index
                .symbols()
                .iter()
                .all(|entry| entry.symbol.name != "ignored-source")
        );
    }
}
