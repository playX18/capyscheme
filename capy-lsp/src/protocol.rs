use lsp_types::{Range, Url};
use serde::{Deserialize, Serialize};
use serde_json::Value;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WorkerRequest {
    pub id: u64,
    pub method: String,
    pub params: Value,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WorkerResponse {
    pub id: u64,
    pub ok: bool,
    #[serde(default)]
    pub result: Option<Value>,
    #[serde(default)]
    pub error: Option<Value>,
    #[serde(default)]
    pub shutdown: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct DocumentFacts {
    #[serde(default)]
    pub diagnostics: Vec<DiagnosticFact>,
    #[serde(default)]
    pub symbols: Vec<SymbolFact>,
    #[serde(default)]
    pub references: Vec<ReferenceFact>,
    #[serde(default)]
    pub completions: Vec<CompletionFact>,
    #[serde(default)]
    pub imports: Vec<ImportFact>,
    #[serde(default)]
    pub actions: Vec<DocumentActionFact>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DocumentActionFact {
    pub action: String,
    pub title: String,
    pub range: Range,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DiagnosticFact {
    pub range: Range,
    pub severity: DiagnosticSeverityFact,
    pub message: String,
    #[serde(default)]
    pub source: Option<String>,
    #[serde(default)]
    pub code: Option<String>,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum DiagnosticSeverityFact {
    Error,
    Warning,
    Information,
    Hint,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SymbolFact {
    pub name: String,
    pub kind: SymbolKindFact,
    pub range: Range,
    #[serde(rename = "selectionRange", alias = "selection_range")]
    pub selection_range: Range,
    #[serde(default)]
    pub detail: Option<String>,
    #[serde(default)]
    pub container: Option<String>,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, Hash)]
#[serde(rename_all = "kebab-case")]
pub enum SymbolKindFact {
    #[serde(alias = "library")]
    Module,
    Function,
    #[serde(alias = "record")]
    Variable,
    #[serde(alias = "syntax")]
    Macro,
    Keyword,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReferenceFact {
    pub name: String,
    pub uri: Url,
    pub range: Range,
    #[serde(default)]
    pub definition: Option<LocationFact>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LocationFact {
    pub uri: Url,
    pub range: Range,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CompletionFact {
    pub label: String,
    #[serde(default)]
    pub detail: Option<String>,
    #[serde(default)]
    pub documentation: Option<String>,
    pub kind: SymbolKindFact,
    #[serde(default, rename = "sourceModule", alias = "source_module")]
    pub source_module: Option<String>,
    #[serde(default, rename = "sourceFile", alias = "source_file")]
    pub source_file: Option<String>,
    #[serde(default, rename = "sourceName", alias = "source_name")]
    pub source_name: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ImportFact {
    pub name: String,
    #[serde(default)]
    pub range: Range,
    #[serde(default)]
    pub resolved: bool,
    #[serde(default)]
    pub error: Option<String>,
    #[serde(default)]
    pub file: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ActionOutput {
    pub title: String,
    pub language: String,
    pub content: String,
}
