use std::{
    collections::HashSet,
    fs,
    path::{Path, PathBuf},
};

use lsp_types::{CompletionItem, CompletionItemKind, Position};

use crate::document::position_to_byte;

const MAX_MODULE_COMPLETION_FILES: usize = 5000;

#[derive(Debug, Clone, PartialEq, Eq)]
struct ModuleCandidate {
    label: String,
    module_prefix: String,
    source_root: PathBuf,
    source_file: PathBuf,
}

pub(crate) fn import_module_completion_items(
    load_path: &[PathBuf],
    extensions: &[String],
    text: &str,
    position: Position,
    seen_labels: &mut HashSet<String>,
) -> Vec<CompletionItem> {
    let Some(prefix) = import_module_prefix(text, position) else {
        return Vec::new();
    };

    module_candidates(load_path, extensions)
        .into_iter()
        .filter(|candidate| candidate.module_prefix.starts_with(&prefix))
        .filter(|candidate| seen_labels.insert(candidate.label.clone()))
        .map(|candidate| CompletionItem {
            label: candidate.label,
            kind: Some(CompletionItemKind::MODULE),
            detail: Some(format!("Source: {}", candidate.source_root.display())),
            documentation: Some(lsp_types::Documentation::String(format!(
                "File: {}",
                candidate.source_file.display()
            ))),
            ..CompletionItem::default()
        })
        .collect()
}

fn module_candidates(load_path: &[PathBuf], extensions: &[String]) -> Vec<ModuleCandidate> {
    let mut builder = ModuleCandidateBuilder {
        extensions,
        seen_roots: HashSet::new(),
        seen_labels: HashSet::new(),
        candidates: Vec::new(),
        file_count: 0,
    };

    for root in load_path {
        builder.scan_root(root);
    }

    builder.candidates
}

struct ModuleCandidateBuilder<'a> {
    extensions: &'a [String],
    seen_roots: HashSet<PathBuf>,
    seen_labels: HashSet<String>,
    candidates: Vec<ModuleCandidate>,
    file_count: usize,
}

impl ModuleCandidateBuilder<'_> {
    fn scan_root(&mut self, root: &Path) {
        if self.file_count >= MAX_MODULE_COMPLETION_FILES {
            return;
        }
        let key = fs::canonicalize(root).unwrap_or_else(|_| root.to_path_buf());
        if !self.seen_roots.insert(key) {
            return;
        }
        self.scan_path(root, root);
    }

    fn scan_path(&mut self, root: &Path, path: &Path) {
        if self.file_count >= MAX_MODULE_COMPLETION_FILES || should_skip_path(path) {
            return;
        }
        let Ok(metadata) = fs::symlink_metadata(path) else {
            return;
        };
        if metadata.file_type().is_symlink() {
            return;
        }
        if metadata.is_dir() {
            let Ok(entries) = fs::read_dir(path) else {
                return;
            };
            for entry in entries.flatten() {
                self.scan_path(root, &entry.path());
            }
        } else if metadata.is_file() {
            self.add_file(root, path);
        }
    }

    fn add_file(&mut self, root: &Path, path: &Path) {
        if !has_configured_extension(path, self.extensions) {
            return;
        }
        let Some(module_name) = module_name_for_path(root, path) else {
            return;
        };
        let label = format!("({module_name})");
        if self.seen_labels.insert(label.clone()) {
            self.candidates.push(ModuleCandidate {
                label,
                module_prefix: module_name,
                source_root: root.to_path_buf(),
                source_file: path.to_path_buf(),
            });
        }
        self.file_count += 1;
    }
}

fn module_name_for_path(root: &Path, path: &Path) -> Option<String> {
    let relative = path.strip_prefix(root).ok()?;
    let mut parts = Vec::new();
    for component in relative.components() {
        let text = component.as_os_str().to_str()?;
        parts.push(text.to_string());
    }
    let last = parts.last_mut()?;
    let stem = Path::new(last).file_stem()?.to_str()?;
    *last = stem.to_string();
    if parts.iter().any(|part| part.is_empty()) {
        return None;
    }
    Some(parts.join(" "))
}

fn has_configured_extension(path: &Path, extensions: &[String]) -> bool {
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

fn import_module_prefix(text: &str, position: Position) -> Option<String> {
    let byte = position_to_byte(text, position);
    let tokens = tokenize_prefix(&text[..byte.min(text.len())]);
    let mut stack = Vec::<Frame>::new();

    for token in tokens {
        match token.as_str() {
            "(" => stack.push(Frame::default()),
            ")" => {
                stack.pop();
            }
            atom => {
                if let Some(frame) = stack.last_mut() {
                    if frame.head.is_none() {
                        frame.head = Some(atom.to_string());
                    }
                    frame.atoms.push(atom.to_string());
                }
            }
        }
    }

    if !stack
        .iter()
        .any(|frame| frame.head.as_deref() == Some("import"))
    {
        return None;
    }

    let Some(frame) = stack.last() else {
        return Some(String::new());
    };
    if frame.head.as_deref() == Some("import") {
        return Some(
            frame
                .atoms
                .iter()
                .skip(1)
                .cloned()
                .collect::<Vec<_>>()
                .join(" "),
        );
    }
    Some(frame.atoms.join(" "))
}

#[derive(Debug, Default)]
struct Frame {
    head: Option<String>,
    atoms: Vec<String>,
}

fn tokenize_prefix(text: &str) -> Vec<String> {
    let mut tokens = Vec::new();
    let mut iter = text.char_indices().peekable();
    let mut in_comment = false;
    while let Some((start, ch)) = iter.next() {
        if in_comment {
            if ch == '\n' {
                in_comment = false;
            }
            continue;
        }
        if ch == ';' {
            in_comment = true;
            continue;
        }
        if ch.is_whitespace() {
            continue;
        }
        if matches!(ch, '(' | '[') {
            tokens.push("(".into());
            continue;
        }
        if matches!(ch, ')' | ']') {
            tokens.push(")".into());
            continue;
        }
        if ch == '"' {
            let mut escape = false;
            for (_, next) in iter.by_ref() {
                if escape {
                    escape = false;
                } else if next == '\\' {
                    escape = true;
                } else if next == '"' {
                    break;
                }
            }
            continue;
        }

        let mut end = start + ch.len_utf8();
        while let Some((idx, next)) = iter.peek().copied() {
            if next.is_whitespace()
                || matches!(next, '(' | ')' | '[' | ']' | '"' | '\'' | '`' | ',' | ';')
            {
                break;
            }
            iter.next();
            end = idx + next.len_utf8();
        }
        tokens.push(text[start..end].into());
    }
    tokens
}

#[cfg(test)]
mod tests {
    use std::fs;

    use lsp_types::Position;
    use tempfile::tempdir;

    use super::{import_module_completion_items, import_module_prefix};

    #[test]
    fn completes_modules_from_load_path_files_inside_import() {
        let dir = tempdir().unwrap();
        fs::create_dir_all(dir.path().join("foo")).unwrap();
        fs::write(dir.path().join("foo").join("bar.scm"), "").unwrap();
        fs::write(dir.path().join("ignored.txt"), "").unwrap();

        let load_path = vec![dir.path().to_path_buf()];
        let extensions = vec!["scm".into()];
        let text = "(import (fo";
        let mut seen = std::collections::HashSet::new();
        let items = import_module_completion_items(
            &load_path,
            &extensions,
            text,
            Position {
                line: 0,
                character: text.len() as u32,
            },
            &mut seen,
        );

        assert!(items.iter().any(|item| item.label == "(foo bar)"));
        assert!(items.iter().all(|item| item.label != "(ignored)"));
    }

    #[test]
    fn skips_module_completions_outside_import() {
        let text = "(define fo";
        assert_eq!(
            import_module_prefix(
                text,
                Position {
                    line: 0,
                    character: text.len() as u32,
                },
            ),
            None
        );
    }

    #[test]
    fn filters_by_multi_part_module_prefix() {
        let text = "(import (foo ba";
        assert_eq!(
            import_module_prefix(
                text,
                Position {
                    line: 0,
                    character: text.len() as u32,
                },
            ),
            Some("foo ba".into())
        );
    }
}
