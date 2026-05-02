use lsp_types::{
    CompletionItemKind, Diagnostic, DocumentSymbol, Location, MarkupContent, MarkupKind,
    SymbolInformation, SymbolKind, Url,
};

use crate::protocol::{
    CompletionFact, DiagnosticFact, DiagnosticSeverityFact, LocationFact, SymbolFact,
    SymbolKindFact,
};

pub(super) fn to_diagnostic(fact: DiagnosticFact) -> Diagnostic {
    Diagnostic::new(
        fact.range,
        Some(match fact.severity {
            DiagnosticSeverityFact::Error => lsp_types::DiagnosticSeverity::ERROR,
            DiagnosticSeverityFact::Warning => lsp_types::DiagnosticSeverity::WARNING,
            DiagnosticSeverityFact::Information => lsp_types::DiagnosticSeverity::INFORMATION,
            DiagnosticSeverityFact::Hint => lsp_types::DiagnosticSeverity::HINT,
        }),
        None,
        fact.source,
        fact.message,
        None,
        None,
    )
}

pub(super) fn to_location(location: &LocationFact) -> Location {
    Location::new(location.uri.clone(), location.range)
}

pub(super) fn format_hover_content(name: &str, detail: &str) -> MarkupContent {
    let mut value = format!("`{}`", markdown_inline_code(name));
    let lines = detail
        .lines()
        .map(str::trim)
        .filter(|line| !line.is_empty())
        .collect::<Vec<_>>();
    if !lines.is_empty() {
        value.push_str("\n\n");
        value.push_str(
            &lines
                .into_iter()
                .map(|line| format!("- {}", markdown_text(line)))
                .collect::<Vec<_>>()
                .join("\n"),
        );
    }

    MarkupContent {
        kind: MarkupKind::Markdown,
        value,
    }
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
