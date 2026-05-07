use lsp_types::{
    CompletionItemKind, Diagnostic, DocumentSymbol, Location, MarkupContent, MarkupKind,
    NumberOrString, SymbolInformation, SymbolKind, Url,
};
use std::path::Path;

use crate::protocol::{
    CompletionFact, DiagnosticFact, DiagnosticSeverityFact, LocationFact, SymbolFact,
    SymbolKindFact,
};

use super::signature::callable_signature;

pub(super) fn to_diagnostic(fact: DiagnosticFact) -> Diagnostic {
    Diagnostic::new(
        fact.range,
        Some(match fact.severity {
            DiagnosticSeverityFact::Error => lsp_types::DiagnosticSeverity::ERROR,
            DiagnosticSeverityFact::Warning => lsp_types::DiagnosticSeverity::WARNING,
            DiagnosticSeverityFact::Information => lsp_types::DiagnosticSeverity::INFORMATION,
            DiagnosticSeverityFact::Hint => lsp_types::DiagnosticSeverity::HINT,
        }),
        fact.code.map(NumberOrString::String),
        fact.source,
        fact.message,
        None,
        None,
    )
}

pub(super) fn to_location(location: &LocationFact) -> Location {
    Location::new(location.uri.clone(), location.range)
}

pub(super) fn format_hover_content(
    name: &str,
    detail: &str,
    documentation: Option<&str>,
) -> MarkupContent {
    let lines = detail_lines(detail);
    if let Some(content) = format_import_hover_content(name, name, &lines, documentation) {
        return content;
    }

    let mut value = format!("`{}`", markdown_inline_code(name));
    if !lines.is_empty() {
        value.push_str("\n\n");
        value.push_str(
            &lines
                .into_iter()
                .map(markdown_text)
                .collect::<Vec<_>>()
                .join("\n"),
        );
    }
    let docs = detail_lines(documentation.unwrap_or(""));
    if !docs.is_empty() {
        value.push_str("\n\n");
        value.push_str(
            &docs
                .into_iter()
                .map(markdown_text)
                .collect::<Vec<_>>()
                .join("\n"),
        );
    }

    MarkupContent {
        kind: MarkupKind::Markdown,
        value,
    }
}

pub(super) fn format_completion_hover_content(item: &CompletionFact) -> MarkupContent {
    let detail = completion_fact_detail(item).unwrap_or_else(|| "identifier".into());
    let lines = detail_lines(&detail);
    let declaration = completion_hover_declaration(item, &detail);
    if let Some(content) = format_import_hover_content(
        &item.label,
        &declaration,
        &lines,
        item.documentation.as_deref(),
    ) {
        return content;
    }

    format_hover_content(&item.label, &detail, item.documentation.as_deref())
}

fn format_import_hover_content(
    name: &str,
    declaration: &str,
    lines: &[&str],
    documentation: Option<&str>,
) -> Option<MarkupContent> {
    let metadata = ImportHoverMetadata::from_lines(lines);
    if metadata.is_empty() {
        return None;
    }

    let title = metadata
        .module
        .as_deref()
        .or(metadata.defined_in.as_deref())
        .unwrap_or(name);
    let mut value = format!(
        "`{}`\n\n```scheme\n{}\n```",
        markdown_inline_code(title),
        markdown_scheme_code(declaration)
    );

    if let Some(module) = metadata.defined_in.as_deref() {
        value.push_str("\n\nDefined in `");
        value.push_str(&markdown_inline_code(module));
        value.push('`');
    }

    if let Some(module) = metadata.reexported_from.as_deref() {
        value.push_str("\n\nRe-exported from `");
        value.push_str(&markdown_inline_code(module));
        value.push('`');
    }

    if let Some(file) = metadata.file.as_deref() {
        value.push_str("\n\nSource: `");
        value.push_str(&markdown_inline_code(display_file_name(file)));
        value.push('`');
    }

    if !metadata.notes.is_empty() {
        value.push_str("\n\n");
        value.push_str(
            &metadata
                .notes
                .iter()
                .map(|line| markdown_text(line))
                .collect::<Vec<_>>()
                .join("\n"),
        );
    }

    let docs = detail_lines(documentation.unwrap_or(""));
    if !docs.is_empty() {
        value.push_str("\n\n");
        value.push_str(
            &docs
                .into_iter()
                .map(markdown_text)
                .collect::<Vec<_>>()
                .join("\n"),
        );
    }

    Some(MarkupContent {
        kind: MarkupKind::Markdown,
        value,
    })
}

fn completion_hover_declaration(item: &CompletionFact, detail: &str) -> String {
    match item.kind {
        SymbolKindFact::Function | SymbolKindFact::Macro => {
            let signature_text = match item.documentation.as_deref() {
                Some(documentation) if !documentation.is_empty() => {
                    format!("{detail}\n{documentation}")
                }
                _ => detail.to_string(),
            };
            callable_signature(&item.label, Some(&signature_text))
                .map(|signature| signature.label)
                .unwrap_or_else(|| format!("({} ...)", item.label))
        }
        SymbolKindFact::Module => format!("(import ({}))", item.label),
        SymbolKindFact::Variable | SymbolKindFact::Keyword => item.label.clone(),
    }
}

#[derive(Default)]
struct ImportHoverMetadata<'a> {
    defined_in: Option<&'a str>,
    reexported_from: Option<&'a str>,
    module: Option<&'a str>,
    file: Option<&'a str>,
    notes: Vec<&'a str>,
}

impl<'a> ImportHoverMetadata<'a> {
    fn from_lines(lines: &'a [&'a str]) -> Self {
        let mut metadata = Self::default();
        for line in lines {
            if let Some(value) = line.strip_prefix("defined in ") {
                metadata.defined_in = Some(value);
            } else if let Some(value) = line.strip_prefix("re-exported from ") {
                metadata.reexported_from = Some(value);
            } else if let Some(value) = line.strip_prefix("module: ") {
                metadata.module = Some(value);
            } else if let Some(value) = line.strip_prefix("file: ") {
                metadata.file = Some(value);
            } else {
                metadata.notes.push(line);
            }
        }
        metadata
    }

    fn is_empty(&self) -> bool {
        self.defined_in.is_none()
            && self.reexported_from.is_none()
            && self.module.is_none()
            && self.file.is_none()
    }
}

fn detail_lines(detail: &str) -> Vec<&str> {
    detail
        .lines()
        .map(str::trim)
        .filter(|line| !line.is_empty())
        .collect()
}

fn display_file_name(file: &str) -> &str {
    Path::new(file)
        .file_name()
        .and_then(|name| name.to_str())
        .unwrap_or(file)
}

pub(super) fn completion_fact_detail(item: &CompletionFact) -> Option<String> {
    let mut parts = Vec::new();
    if let Some(detail) = item.detail.as_ref().filter(|detail| !detail.is_empty()) {
        parts.push(detail.clone());
    }
    if let Some(module) = item
        .source_module
        .as_ref()
        .filter(|module| !module.is_empty())
    {
        parts.push(format!("module: {module}"));
    }
    if let Some(source_name) = item
        .source_name
        .as_ref()
        .filter(|source_name| !source_name.is_empty() && *source_name != &item.label)
    {
        parts.push(format!("source: {source_name}"));
    }
    if let Some(file) = item.source_file.as_ref().filter(|file| !file.is_empty()) {
        parts.push(format!("file: {file}"));
    }
    if parts.is_empty() {
        None
    } else {
        Some(parts.join("\n"))
    }
}

pub(super) fn to_completion_kind(kind: SymbolKindFact) -> CompletionItemKind {
    match kind {
        SymbolKindFact::Module => CompletionItemKind::MODULE,
        SymbolKindFact::Function => CompletionItemKind::FUNCTION,
        SymbolKindFact::Variable => CompletionItemKind::VARIABLE,
        SymbolKindFact::Macro => CompletionItemKind::FUNCTION,
        SymbolKindFact::Keyword => CompletionItemKind::KEYWORD,
    }
}

pub(super) fn to_symbol_kind(kind: SymbolKindFact) -> SymbolKind {
    match kind {
        SymbolKindFact::Module => SymbolKind::MODULE,
        SymbolKindFact::Function => SymbolKind::FUNCTION,
        SymbolKindFact::Variable => SymbolKind::VARIABLE,
        SymbolKindFact::Macro => SymbolKind::FUNCTION,
        SymbolKindFact::Keyword => SymbolKind::KEY,
    }
}

#[allow(deprecated)]
pub(super) fn to_document_symbol(symbol: &SymbolFact) -> DocumentSymbol {
    DocumentSymbol {
        name: symbol.name.clone(),
        detail: symbol.detail.clone(),
        kind: to_symbol_kind(symbol.kind),
        tags: None,
        deprecated: None,
        range: symbol.range,
        selection_range: symbol.selection_range,
        children: None,
    }
}

#[allow(deprecated)]
pub(super) fn to_symbol_information(uri: Url, symbol: &SymbolFact) -> SymbolInformation {
    SymbolInformation {
        name: symbol.name.clone(),
        kind: to_symbol_kind(symbol.kind),
        tags: None,
        deprecated: None,
        location: Location::new(uri, symbol.selection_range),
        container_name: symbol.container.clone(),
    }
}

fn markdown_inline_code(text: &str) -> String {
    text.replace('`', "\\`")
}

fn markdown_text(text: &str) -> String {
    text.replace('\\', "\\\\")
        .replace('`', "\\`")
        .replace('*', "\\*")
        .replace('_', "\\_")
        .replace('[', "\\[")
        .replace(']', "\\]")
}

fn markdown_scheme_code(text: &str) -> String {
    text.replace("```", "`\\`\\`")
}

#[cfg(test)]
mod tests {
    use crate::protocol::{CompletionFact, SymbolKindFact};

    use super::format_completion_hover_content;

    #[test]
    fn hover_content_formats_import_metadata_as_declaration_card() {
        let content = format_completion_hover_content(&CompletionFact {
            label: "match".into(),
            detail: Some("defined in srfi srfi-257".into()),
            documentation: Some("(match value clause ...)\nPattern matching form.".into()),
            kind: SymbolKindFact::Function,
            source_module: Some("srfi srfi-257".into()),
            source_file: Some("/home/adel/.local/share/capy/lib/srfi/srfi-257.scm".into()),
            source_name: Some("match".into()),
        });

        assert!(
            content
                .value
                .starts_with("`srfi srfi-257`\n\n```scheme\n(match value clause ...)\n```")
        );
        assert!(!content.value.contains("\n- "));
        assert!(content.value.contains("Defined in `srfi srfi-257`"));
        assert!(content.value.contains("srfi-257.scm"));
        assert!(
            !content
                .value
                .contains("/home/adel/.local/share/capy/lib/srfi/srfi-257.scm")
        );
    }

    #[test]
    fn hover_content_formats_imported_variables_without_call_shape() {
        let content = format_completion_hover_content(&CompletionFact {
            label: "dep-a".into(),
            detail: Some("defined in dep".into()),
            documentation: None,
            kind: SymbolKindFact::Variable,
            source_module: Some("dep".into()),
            source_file: Some("/tmp/capy-lsp-refresh-test/dep.sls".into()),
            source_name: Some("dep-a".into()),
        });

        assert!(content.value.starts_with("`dep`\n\n```scheme\ndep-a\n```"));
        assert!(!content.value.contains("(dep-a ...)"));
    }
}
