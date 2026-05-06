use std::{
    collections::{HashMap, HashSet},
    fs,
    path::{Path, PathBuf},
};

use lsp_types::Url;

use crate::{analysis, config::LspConfig, document::Document, protocol::SymbolFact};

const MAX_INDEXED_FILES: usize = 5000;

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
        config: &LspConfig,
        workspace_epoch: u64,
        config_fingerprint: u64,
        open_documents: &HashMap<Url, Document>,
    ) -> Self {
        let mut builder = IndexBuilder {
            config,
            seen_paths: HashSet::new(),
            seen_uris: HashSet::new(),
            symbols: Vec::new(),
            file_count: 0,
        };

        for (uri, document) in open_documents {
            builder.index_text(uri.clone(), &document.text);
        }

        for root in config.effective_load_path() {
            builder.scan_path(&root);
        }

        Self {
            workspace_epoch,
            config_fingerprint,
            symbols: builder.symbols,
        }
    }

    pub(crate) fn matches(&self, workspace_epoch: u64, config_fingerprint: u64) -> bool {
        self.workspace_epoch == workspace_epoch && self.config_fingerprint == config_fingerprint
    }

    pub(crate) fn symbols(&self) -> &[IndexedSymbol] {
        &self.symbols
    }
}

struct IndexBuilder<'a> {
    config: &'a LspConfig,
    seen_paths: HashSet<PathBuf>,
    seen_uris: HashSet<Url>,
    symbols: Vec<IndexedSymbol>,
    file_count: usize,
}

impl IndexBuilder<'_> {
    fn scan_path(&mut self, path: &Path) {
        if self.file_count >= MAX_INDEXED_FILES || should_skip_path(path) {
            return;
        }

        let Ok(metadata) = fs::symlink_metadata(path) else {
            return;
        };
        if metadata.file_type().is_symlink() {
            return;
        }
        if metadata.is_dir() {
            self.scan_dir(path);
        } else if metadata.is_file() {
            self.index_file(path);
        }
    }

    fn scan_dir(&mut self, dir: &Path) {
        let key = fs::canonicalize(dir).unwrap_or_else(|_| dir.to_path_buf());
        if !self.seen_paths.insert(key) {
            return;
        }

        let Ok(entries) = fs::read_dir(dir) else {
            return;
        };
        for entry in entries.flatten() {
            if self.file_count >= MAX_INDEXED_FILES {
                break;
            }
            self.scan_path(&entry.path());
        }
    }

    fn index_file(&mut self, path: &Path) {
        if !has_indexed_extension(path, &self.config.extensions) {
            return;
        }
        let Ok(uri) = Url::from_file_path(path) else {
            return;
        };
        if self.seen_uris.contains(&uri) {
            return;
        }
        let Ok(text) = fs::read_to_string(path) else {
            return;
        };
        self.index_text(uri, &text);
        self.file_count += 1;
    }

    fn index_text(&mut self, uri: Url, text: &str) {
        if !self.seen_uris.insert(uri.clone()) {
            return;
        }
        let facts = analysis::analyze_syntax(&uri, text);
        self.symbols
            .extend(facts.symbols.into_iter().map(|symbol| IndexedSymbol {
                uri: uri.clone(),
                symbol,
            }));
    }
}

fn has_indexed_extension(path: &Path, extensions: &[String]) -> bool {
    let Some(extension) = path.extension().and_then(|extension| extension.to_str()) else {
        return false;
    };
    extensions
        .iter()
        .any(|configured| configured.trim_start_matches('.') == extension)
}

fn should_skip_path(path: &Path) -> bool {
    path.file_name()
        .and_then(|name| name.to_str())
        .is_some_and(|name| matches!(name, ".git" | "target" | ".direnv" | "node_modules"))
}

#[cfg(test)]
mod tests {
    use std::{collections::HashMap, fs};

    use lsp_types::Url;
    use tempfile::tempdir;

    use crate::{config::LspConfig, document::Document};

    use super::WorkspaceIndex;

    #[test]
    fn indexes_unopened_workspace_files_by_config_extensions() {
        let dir = tempdir().unwrap();
        fs::write(dir.path().join("one.scm"), "(define visible 1)\n").unwrap();
        fs::write(dir.path().join("two.txt"), "(define hidden 2)\n").unwrap();

        let config = LspConfig::default_for_root(dir.path());
        let index = WorkspaceIndex::build(&config, 3, config.fingerprint(), &HashMap::new());

        assert!(index.matches(3, config.fingerprint()));
        assert!(
            index
                .symbols()
                .iter()
                .any(|entry| entry.symbol.name == "visible")
        );
        assert!(
            index
                .symbols()
                .iter()
                .all(|entry| entry.symbol.name != "hidden")
        );
    }

    #[test]
    fn indexes_open_document_text_before_disk_text() {
        let dir = tempdir().unwrap();
        let path = dir.path().join("open.scm");
        fs::write(&path, "(define disk-name 1)\n").unwrap();
        let uri = Url::from_file_path(&path).unwrap();
        let mut documents = HashMap::new();
        documents.insert(
            uri.clone(),
            Document::new(uri, 7, "(define memory-name 2)\n".into()),
        );

        let config = LspConfig::default_for_root(dir.path());
        let index = WorkspaceIndex::build(&config, 1, config.fingerprint(), &documents);

        assert!(
            index
                .symbols()
                .iter()
                .any(|entry| entry.symbol.name == "memory-name")
        );
        assert!(
            index
                .symbols()
                .iter()
                .all(|entry| entry.symbol.name != "disk-name")
        );
    }
}
