use std::{
    collections::{HashMap, HashSet},
    env, fs,
    ops::ControlFlow,
    path::PathBuf,
    sync::{
        Arc, Mutex,
        atomic::{AtomicU64, Ordering},
    },
};

mod actions;
mod convert;
mod navigation;
mod semantic;
mod signature;
mod util;

use async_lsp::{
    ClientSocket, ErrorCode, ResponseError, client_monitor::ClientProcessMonitorLayer,
    concurrency::ConcurrencyLayer, panic::CatchUnwindLayer, router::Router, server::LifecycleLayer,
    tracing::TracingLayer,
};
use lsp_types::{
    CompletionItem, CompletionOptions, CompletionResponse, DiagnosticOptions,
    DiagnosticServerCapabilities, DocumentDiagnosticReport, DocumentDiagnosticReportResult,
    DocumentHighlight, DocumentHighlightKind, DocumentSymbolResponse, Documentation,
    ExecuteCommandOptions, ExecuteCommandParams, FullDocumentDiagnosticReport,
    GotoDefinitionResponse, Hover, HoverContents, HoverProviderCapability, InitializeParams,
    InitializeResult, InsertTextFormat, Location, OneOf, Position, PublishDiagnosticsParams, Range,
    RelatedFullDocumentDiagnosticReport, SemanticToken, SemanticTokens, SemanticTokensFullOptions,
    SemanticTokensOptions, SemanticTokensResult, SemanticTokensServerCapabilities,
    ServerCapabilities, ServerInfo, SignatureHelp, SignatureHelpOptions,
    TextDocumentSyncCapability, TextDocumentSyncKind, TextDocumentSyncOptions,
    TextDocumentSyncSaveOptions, Url, WorkDoneProgressOptions, WorkspaceSymbolResponse,
    notification, request,
};
use serde_json::Value;
use tokio::{
    task::JoinHandle,
    time::{Duration, sleep},
};
use tower::ServiceBuilder;
use tracing::Level;

use crate::{
    analysis,
    config::{ConfigFingerprint, LspConfig},
    document::{CachedDocumentFacts, Document, word_at_position},
    module_completions::import_module_completion_items,
    protocol::{DiagnosticFact, DiagnosticSeverityFact, DocumentFacts},
    worker::WorkerPool,
    workspace_index::WorkspaceIndex,
};

use actions::{ActionCommandArgs, action_definitions, known_action};
use convert::{
    completion_fact_detail, format_completion_hover_content, format_hover_content,
    to_completion_kind, to_diagnostic, to_document_symbol, to_location, to_symbol_information,
};
use navigation::{
    push_location, reference_targets_definition, selected_reference_definition,
    selected_symbol_definition,
};
use semantic::{semantic_token_type, semantic_tokens_legend};
use signature::{
    callable_signature, completion_detail, completion_snippet, enclosing_call,
    signature_information,
};
use util::{
    contains_position, file_name, io_error, io_response_error, lock, workspace_root_from_initialize,
};

const DID_CHANGE_DIAGNOSTIC_DEBOUNCE: Duration = Duration::from_millis(150);
const DID_SAVE_DIAGNOSTIC_DEBOUNCE: Duration = Duration::from_millis(50);

#[derive(Debug, Clone, Copy)]
struct AnalysisRequestContext {
    document_version: i32,
    workspace_epoch: u64,
    config_fingerprint: ConfigFingerprint,
}

#[derive(Debug, Clone)]
struct AnalysisSnapshot {
    text: String,
    facts: DocumentFacts,
    context: AnalysisRequestContext,
}

#[derive(Debug, Clone, Copy)]
enum DiagnosticDelay {
    Open,
    Change,
    Save,
}

impl DiagnosticDelay {
    fn duration(self) -> Duration {
        match self {
            Self::Open => Duration::ZERO,
            Self::Change => DID_CHANGE_DIAGNOSTIC_DEBOUNCE,
            Self::Save => DID_SAVE_DIAGNOSTIC_DEBOUNCE,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{AnalysisRequestContext, analysis_context_matches};

    #[test]
    fn analysis_context_accepts_matching_version_epoch_and_config() {
        let context = AnalysisRequestContext {
            document_version: 7,
            workspace_epoch: 11,
            config_fingerprint: 13,
        };

        assert!(analysis_context_matches(context, Some(7), 11, Some(13)));
    }

    #[test]
    fn analysis_context_rejects_stale_inputs() {
        let context = AnalysisRequestContext {
            document_version: 7,
            workspace_epoch: 11,
            config_fingerprint: 13,
        };

        assert!(!analysis_context_matches(context, Some(8), 11, Some(13)));
        assert!(!analysis_context_matches(context, Some(7), 12, Some(13)));
        assert!(!analysis_context_matches(context, Some(7), 11, Some(14)));
    }

    #[test]
    fn analysis_context_allows_missing_document_or_config_only_for_zero_context() {
        let disk_context = AnalysisRequestContext {
            document_version: 0,
            workspace_epoch: 1,
            config_fingerprint: 0,
        };
        let open_context = AnalysisRequestContext {
            document_version: 2,
            workspace_epoch: 1,
            config_fingerprint: 3,
        };

        assert!(analysis_context_matches(disk_context, None, 1, None));
        assert!(!analysis_context_matches(open_context, None, 1, Some(3)));
        assert!(!analysis_context_matches(open_context, Some(2), 1, None));
    }
}

fn watched_scheme_file(uri: &Url) -> bool {
    uri.path()
        .rsplit_once('.')
        .is_some_and(|(_, extension)| matches!(extension, "scm" | "sls" | "sld"))
}

fn analysis_context_matches(
    context: AnalysisRequestContext,
    current_document_version: Option<i32>,
    current_workspace_epoch: u64,
    current_config_fingerprint: Option<ConfigFingerprint>,
) -> bool {
    if current_workspace_epoch != context.workspace_epoch {
        return false;
    }

    match current_config_fingerprint {
        Some(fingerprint) if fingerprint == context.config_fingerprint => {}
        None if context.config_fingerprint == 0 => {}
        _ => return false,
    }

    match current_document_version {
        Some(version) => version == context.document_version,
        None => context.document_version == 0,
    }
}

pub async fn run() -> async_lsp::Result<()> {
    let (server, _) = async_lsp::MainLoop::new_server(|client| {
        let service = Arc::new(LanguageService::new(client.clone()));
        let mut router = Router::new(ServerState { service });

        router
            .request::<request::Initialize, _>(|state, params| {
                let service = state.service.clone();
                async move { Ok(service.initialize(params)) }
            })
            .request::<request::Shutdown, _>(|state, ()| {
                let service = state.service.clone();
                async move {
                    service.shutdown().await;
                    Ok(())
                }
            })
            .notification::<notification::Exit>(|state, ()| {
                let service = state.service.clone();
                tokio::spawn(async move {
                    service.shutdown().await;
                });
                ControlFlow::Break(Ok(()))
            })
            .request::<request::DocumentDiagnosticRequest, _>(|state, params| {
                let service = state.service.clone();
                async move { Ok(service.document_diagnostics(params.text_document.uri).await) }
            })
            .request::<request::HoverRequest, _>(|state, params| {
                let service = state.service.clone();
                async move {
                    Ok(service
                        .hover(
                            params.text_document_position_params.text_document.uri,
                            params.text_document_position_params.position,
                        )
                        .await)
                }
            })
            .request::<request::GotoDefinition, _>(|state, params| {
                let service = state.service.clone();
                async move {
                    Ok(service
                        .definition(
                            params.text_document_position_params.text_document.uri,
                            params.text_document_position_params.position,
                        )
                        .await)
                }
            })
            .request::<request::References, _>(|state, params| {
                let service = state.service.clone();
                async move {
                    Ok(service
                        .references(
                            params.text_document_position.text_document.uri,
                            params.text_document_position.position,
                            params.context.include_declaration,
                        )
                        .await)
                }
            })
            .request::<request::Completion, _>(|state, params| {
                let service = state.service.clone();
                async move {
                    Ok(service
                        .completion(
                            params.text_document_position.text_document.uri,
                            params.text_document_position.position,
                        )
                        .await)
                }
            })
            .request::<request::SignatureHelpRequest, _>(|state, params| {
                let service = state.service.clone();
                async move {
                    Ok(service
                        .signature_help(
                            params.text_document_position_params.text_document.uri,
                            params.text_document_position_params.position,
                        )
                        .await)
                }
            })
            .request::<request::DocumentSymbolRequest, _>(|state, params| {
                let service = state.service.clone();
                async move { Ok(service.document_symbols(params.text_document.uri).await) }
            })
            .request::<request::WorkspaceSymbolRequest, _>(|state, params| {
                let service = state.service.clone();
                async move { Ok(service.workspace_symbols(params.query).await) }
            })
            .request::<request::DocumentHighlightRequest, _>(|state, params| {
                let service = state.service.clone();
                async move {
                    Ok(service
                        .document_highlight(
                            params.text_document_position_params.text_document.uri,
                            params.text_document_position_params.position,
                        )
                        .await)
                }
            })
            .request::<request::SemanticTokensFullRequest, _>(|state, params| {
                let service = state.service.clone();
                async move { Ok(service.semantic_tokens(params.text_document.uri).await) }
            })
            .request::<request::ExecuteCommand, _>(|state, params| {
                let service = state.service.clone();
                async move { service.execute_command(params).await }
            })
            .notification::<notification::Initialized>(|_, _| ControlFlow::Continue(()))
            .notification::<notification::DidOpenTextDocument>(|state, params| {
                state.service.did_open(params);
                ControlFlow::Continue(())
            })
            .notification::<notification::DidChangeTextDocument>(|state, params| {
                state.service.did_change(params);
                ControlFlow::Continue(())
            })
            .notification::<notification::DidSaveTextDocument>(|state, params| {
                state.service.did_save(params);
                ControlFlow::Continue(())
            })
            .notification::<notification::DidCloseTextDocument>(|state, params| {
                state.service.did_close(params);
                ControlFlow::Continue(())
            })
            .notification::<notification::DidChangeConfiguration>(|state, _| {
                let service = state.service.clone();
                service.refresh_open_documents();
                ControlFlow::Continue(())
            })
            .notification::<notification::DidChangeWatchedFiles>(|state, params| {
                state.service.did_change_watched_files(params);
                ControlFlow::Continue(())
            });

        ServiceBuilder::new()
            .layer(TracingLayer::default())
            .layer(LifecycleLayer::default())
            .layer(CatchUnwindLayer::default())
            .layer(ConcurrencyLayer::default())
            .layer(ClientProcessMonitorLayer::new(client))
            .service(router)
    });

    tracing_subscriber::fmt()
        .with_max_level(Level::INFO)
        .with_ansi(false)
        .with_writer(std::io::stderr)
        .init();

    #[cfg(unix)]
    let (stdin, stdout) = (
        async_lsp::stdio::PipeStdin::lock_tokio().map_err(io_error)?,
        async_lsp::stdio::PipeStdout::lock_tokio().map_err(io_error)?,
    );

    #[cfg(not(unix))]
    let (stdin, stdout) = (
        tokio_util::compat::TokioAsyncReadCompatExt::compat(tokio::io::stdin()),
        tokio_util::compat::TokioAsyncWriteCompatExt::compat_write(tokio::io::stdout()),
    );

    server.run_buffered(stdin, stdout).await
}

#[derive(Clone)]
struct ServerState {
    service: Arc<LanguageService>,
}

struct LanguageService {
    client: ClientSocket,
    workspace_root: Mutex<PathBuf>,
    documents: Mutex<HashMap<Url, Document>>,
    diagnostic_tasks: Mutex<HashMap<Url, JoinHandle<()>>>,
    workspace_index: Mutex<Option<WorkspaceIndex>>,
    workspace_epoch: AtomicU64,
    worker_pool: Arc<WorkerPool>,
}

impl LanguageService {
    fn new(client: ClientSocket) -> Self {
        Self {
            client,
            workspace_root: Mutex::new(env::current_dir().unwrap_or_else(|_| PathBuf::from("."))),
            documents: Mutex::new(HashMap::new()),
            diagnostic_tasks: Mutex::new(HashMap::new()),
            workspace_index: Mutex::new(None),
            workspace_epoch: AtomicU64::new(0),
            worker_pool: Arc::new(WorkerPool::default()),
        }
    }

    fn current_epoch(&self) -> u64 {
        self.workspace_epoch.load(Ordering::Acquire)
    }

    fn bump_epoch(&self) -> u64 {
        *lock(&self.workspace_index) = None;
        self.workspace_epoch.fetch_add(1, Ordering::AcqRel) + 1
    }

    fn initialize(&self, params: InitializeParams) -> InitializeResult {
        if let Some(root) = workspace_root_from_initialize(&params) {
            let mut workspace_root = lock(&self.workspace_root);
            if *workspace_root != root {
                *workspace_root = root;
                self.bump_epoch();
            }
        }

        InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Options(
                    TextDocumentSyncOptions {
                        open_close: Some(true),
                        change: Some(TextDocumentSyncKind::INCREMENTAL),
                        save: Some(TextDocumentSyncSaveOptions::Supported(true)),
                        ..TextDocumentSyncOptions::default()
                    },
                )),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(false),
                    trigger_characters: Some(vec![" ".into(), "-".into(), "?".into(), "!".into()]),
                    ..CompletionOptions::default()
                }),
                signature_help_provider: Some(SignatureHelpOptions {
                    trigger_characters: Some(vec![" ".into(), "(".into()]),
                    retrigger_characters: Some(vec![" ".into()]),
                    work_done_progress_options: WorkDoneProgressOptions::default(),
                }),
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                document_highlight_provider: Some(OneOf::Left(true)),
                document_symbol_provider: Some(OneOf::Left(true)),
                workspace_symbol_provider: Some(OneOf::Left(true)),
                diagnostic_provider: Some(DiagnosticServerCapabilities::Options(
                    DiagnosticOptions {
                        identifier: Some("capy-lsp".into()),
                        inter_file_dependencies: true,
                        workspace_diagnostics: false,
                        work_done_progress_options: WorkDoneProgressOptions::default(),
                    },
                )),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensOptions(
                        SemanticTokensOptions {
                            work_done_progress_options: WorkDoneProgressOptions::default(),
                            legend: semantic_tokens_legend(),
                            range: Some(false),
                            full: Some(SemanticTokensFullOptions::Bool(true)),
                        },
                    ),
                ),
                execute_command_provider: Some(ExecuteCommandOptions {
                    commands: action_definitions()
                        .into_iter()
                        .map(|action| action.command.into())
                        .collect(),
                    work_done_progress_options: WorkDoneProgressOptions::default(),
                }),
                ..ServerCapabilities::default()
            },
            server_info: Some(ServerInfo {
                name: "capy-lsp".into(),
                version: Some(env!("CARGO_PKG_VERSION").into()),
            }),
        }
    }

    async fn shutdown(&self) {
        let _ = self.client.clone();
    }

    fn did_open(self: &Arc<Self>, params: lsp_types::DidOpenTextDocumentParams) {
        let document = params.text_document;
        let uri = document.uri.clone();
        lock(&self.documents).insert(
            document.uri.clone(),
            Document::new(document.uri, document.version, document.text),
        );
        self.schedule_publish_diagnostics(uri, DiagnosticDelay::Open);
    }

    fn did_change(self: &Arc<Self>, params: lsp_types::DidChangeTextDocumentParams) {
        let uri = params.text_document.uri.clone();
        {
            let mut documents = lock(&self.documents);
            if let Some(document) = documents.get_mut(&params.text_document.uri) {
                document.apply_changes(params.text_document.version, params.content_changes);
            } else {
                let text = params
                    .content_changes
                    .last()
                    .map(|change| change.text.clone())
                    .unwrap_or_default();
                documents.insert(
                    params.text_document.uri.clone(),
                    Document::new(params.text_document.uri, params.text_document.version, text),
                );
            }
        }

        self.schedule_publish_diagnostics(uri, DiagnosticDelay::Change);
    }

    fn did_save(self: &Arc<Self>, params: lsp_types::DidSaveTextDocumentParams) {
        let uri = params.text_document.uri.clone();
        if let Some(text) = params.text {
            if let Some(document) = lock(&self.documents).get_mut(&params.text_document.uri) {
                document.text = text;
                document.facts = None;
            }
        }
        self.schedule_publish_diagnostics(uri, DiagnosticDelay::Save);
    }

    fn did_close(self: &Arc<Self>, params: lsp_types::DidCloseTextDocumentParams) {
        let uri = params.text_document.uri;
        lock(&self.documents).remove(&uri);
        if let Some(task) = lock(&self.diagnostic_tasks).remove(&uri) {
            task.abort();
        }
        self.publish_empty_diagnostics(uri);
    }

    fn did_change_watched_files(self: &Arc<Self>, params: lsp_types::DidChangeWatchedFilesParams) {
        for event in &params.changes {
            if file_name(&event.uri).as_deref() == Some("lsp-config.scm") {
                self.refresh_open_documents();
                return;
            }
        }

        if params
            .changes
            .iter()
            .any(|event| watched_scheme_file(&event.uri))
        {
            self.refresh_open_documents();
        }
    }

    fn refresh_open_documents(self: &Arc<Self>) {
        self.bump_epoch();
        let uris = {
            let mut documents = lock(&self.documents);
            for document in documents.values_mut() {
                document.facts = None;
            }
            documents.keys().cloned().collect::<Vec<_>>()
        };
        for uri in uris {
            self.schedule_publish_diagnostics(uri, DiagnosticDelay::Save);
        }
    }

    fn schedule_publish_diagnostics(self: &Arc<Self>, uri: Url, delay: DiagnosticDelay) {
        if let Some(task) = lock(&self.diagnostic_tasks).remove(&uri) {
            task.abort();
        }
        let service = self.clone();
        let task_uri = uri.clone();
        let task = tokio::spawn(async move {
            let duration = delay.duration();
            if !duration.is_zero() {
                sleep(duration).await;
            }
            service.publish_diagnostics(uri).await;
        });
        lock(&self.diagnostic_tasks).insert(task_uri, task);
    }

    async fn publish_diagnostics(self: Arc<Self>, uri: Url) {
        let Some(snapshot) = self.analysis_snapshot(&uri).await else {
            return;
        };
        if !self.is_result_still_current(&uri, snapshot.context) {
            return;
        }
        let diagnostics = snapshot
            .facts
            .diagnostics
            .into_iter()
            .map(to_diagnostic)
            .collect();
        let _ =
            self.client
                .notify::<notification::PublishDiagnostics>(PublishDiagnosticsParams::new(
                    uri,
                    diagnostics,
                    Some(snapshot.context.document_version),
                ));
    }

    fn publish_empty_diagnostics(&self, uri: Url) {
        let _ =
            self.client
                .notify::<notification::PublishDiagnostics>(PublishDiagnosticsParams::new(
                    uri,
                    Vec::new(),
                    None,
                ));
    }

    async fn document_diagnostics(&self, uri: Url) -> DocumentDiagnosticReportResult {
        let facts = self
            .analysis_snapshot(&uri)
            .await
            .map(|snapshot| snapshot.facts)
            .unwrap_or_default();
        DocumentDiagnosticReportResult::Report(DocumentDiagnosticReport::Full(
            RelatedFullDocumentDiagnosticReport {
                related_documents: None,
                full_document_diagnostic_report: FullDocumentDiagnosticReport {
                    result_id: None,
                    items: facts.diagnostics.into_iter().map(to_diagnostic).collect(),
                },
            },
        ))
    }

    async fn hover(&self, uri: Url, position: Position) -> Option<Hover> {
        let AnalysisSnapshot { text, facts, .. } = self.analysis_snapshot(&uri).await?;
        let (name, range) = analysis::symbol_at(&facts, &text, position)?;
        let completion = facts.completions.iter().find(|item| item.label == name);
        let detail = facts
            .symbols
            .iter()
            .find(|symbol| symbol.name == name)
            .and_then(|symbol| symbol.detail.clone())
            .or_else(|| completion.and_then(completion_fact_detail))
            .unwrap_or_else(|| "identifier".into());
        let documentation = facts
            .completions
            .iter()
            .find(|item| item.label == name)
            .and_then(|item| item.documentation.as_deref());
        let contents = completion
            .map(format_completion_hover_content)
            .unwrap_or_else(|| format_hover_content(&name, &detail, documentation));

        Some(Hover {
            contents: HoverContents::Markup(contents),
            range: Some(range),
        })
    }

    async fn definition(&self, uri: Url, position: Position) -> Option<GotoDefinitionResponse> {
        let AnalysisSnapshot { text, facts, .. } = self.analysis_snapshot(&uri).await?;
        let (name, _) = word_at_position(&text, position)?;
        if let Some(reference) = facts.references.iter().find(|reference| {
            reference.name == name && contains_position(reference.range, position)
        }) {
            if let Some(definition) = &reference.definition {
                return Some(GotoDefinitionResponse::Scalar(to_location(definition)));
            }
        }
        if let Some(location) = self.imported_completion_location(&facts, &name).await {
            return Some(GotoDefinitionResponse::Scalar(location));
        }
        facts
            .symbols
            .iter()
            .find(|symbol| symbol.name == name)
            .map(|symbol| {
                GotoDefinitionResponse::Scalar(Location::new(uri, symbol.selection_range))
            })
    }

    async fn references(
        &self,
        uri: Url,
        position: Position,
        include_declaration: bool,
    ) -> Option<Vec<Location>> {
        let AnalysisSnapshot { text, facts, .. } = self.analysis_snapshot(&uri).await?;
        let (name, _) = word_at_position(&text, position)?;
        let mut locations = Vec::new();
        let mut seen = HashSet::new();
        let selected_definition = selected_reference_definition(&facts, &name, position)
            .or_else(|| selected_symbol_definition(&facts, &uri, &name, position));

        if include_declaration {
            if let Some(definition) = &selected_definition {
                push_location(&mut locations, &mut seen, to_location(definition));
            } else {
                for symbol in facts.symbols.iter().filter(|symbol| symbol.name == name) {
                    push_location(
                        &mut locations,
                        &mut seen,
                        Location::new(uri.clone(), symbol.selection_range),
                    );
                }
            }
        }
        for reference in facts.references.iter().filter(|reference| {
            reference.name == name
                && selected_definition
                    .as_ref()
                    .is_none_or(|definition| reference_targets_definition(reference, definition))
        }) {
            push_location(
                &mut locations,
                &mut seen,
                Location::new(reference.uri.clone(), reference.range),
            );
        }

        Some(locations)
    }

    async fn completion(&self, uri: Url, position: Position) -> Option<CompletionResponse> {
        let AnalysisSnapshot { text, facts, .. } = self.analysis_snapshot(&uri).await?;
        let prefix = word_at_position(&text, position)
            .map(|(word, _)| word)
            .unwrap_or_default();
        let mut seen = HashSet::new();
        let mut items = Vec::new();
        for item in facts.completions {
            if !prefix.is_empty() && !item.label.starts_with(&prefix) {
                continue;
            }
            if seen.insert(item.label.clone()) {
                let detail = completion_fact_detail(&item);
                let signature = callable_signature(&item.label, detail.as_deref());
                let insert_text = signature
                    .as_ref()
                    .and_then(|signature| completion_snippet(item.kind, &item.label, signature));
                let insert_text_format = insert_text.as_ref().map(|_| InsertTextFormat::SNIPPET);
                items.push(CompletionItem {
                    label: item.label,
                    kind: Some(to_completion_kind(item.kind)),
                    detail,
                    documentation: item
                        .documentation
                        .filter(|documentation| !documentation.is_empty())
                        .map(|documentation| Documentation::String(documentation)),
                    insert_text,
                    insert_text_format,
                    ..CompletionItem::default()
                });
            }
        }
        if let Some((load_path, extensions)) = self.worker_load_path_for_uri(&uri).await {
            items.extend(import_module_completion_items(
                &load_path,
                &extensions,
                &text,
                position,
                &mut seen,
            ));
        }
        Some(CompletionResponse::Array(items))
    }

    async fn signature_help(&self, uri: Url, position: Position) -> Option<SignatureHelp> {
        let AnalysisSnapshot { text, facts, .. } = self.analysis_snapshot(&uri).await?;
        let call = enclosing_call(&text, position)?;
        let documentation = facts
            .completions
            .iter()
            .find(|item| item.label == call.name)
            .and_then(|item| item.documentation.as_deref());
        let detail = completion_detail(&facts, &call.name).or_else(|| {
            facts
                .symbols
                .iter()
                .find(|symbol| symbol.name == call.name)
                .and_then(|symbol| symbol.detail.clone())
        });
        let signature = callable_signature(&call.name, detail.as_deref())?;
        Some(SignatureHelp {
            signatures: vec![signature_information(&signature, documentation)],
            active_signature: Some(0),
            active_parameter: Some(
                call.active_parameter
                    .min(signature.params.len().saturating_sub(1) as u32),
            ),
        })
    }

    async fn document_symbols(&self, uri: Url) -> Option<DocumentSymbolResponse> {
        let AnalysisSnapshot { facts, .. } = self.analysis_snapshot(&uri).await?;
        let import_symbols = analysis::import_symbols(&facts);
        Some(DocumentSymbolResponse::Nested(
            facts
                .symbols
                .iter()
                .chain(import_symbols.iter())
                .map(to_document_symbol)
                .collect(),
        ))
    }

    async fn workspace_symbols(&self, query: String) -> Option<WorkspaceSymbolResponse> {
        let query = query.to_lowercase();
        let open_uris: HashSet<Url> = lock(&self.documents).keys().cloned().collect();
        let mut symbols = Vec::new();
        for uri in &open_uris {
            let Some(facts) = self.facts_for(&uri).await else {
                continue;
            };
            for symbol in facts.symbols {
                if query.is_empty() || symbol.name.to_lowercase().contains(&query) {
                    symbols.push(to_symbol_information(uri.clone(), &symbol));
                }
            }
        }

        if let Some(index) = self.current_workspace_index() {
            for entry in index.symbols() {
                if open_uris.contains(&entry.uri) {
                    continue;
                }
                if query.is_empty() || entry.symbol.name.to_lowercase().contains(&query) {
                    symbols.push(to_symbol_information(entry.uri.clone(), &entry.symbol));
                }
            }
        }
        Some(WorkspaceSymbolResponse::Flat(symbols))
    }

    fn current_workspace_index(&self) -> Option<WorkspaceIndex> {
        let workspace_root = lock(&self.workspace_root).clone();
        let config = LspConfig::discover(None, &workspace_root).ok()?;
        let config_fingerprint = config.fingerprint();
        let workspace_epoch = self.current_epoch();

        if let Some(index) = lock(&self.workspace_index).as_ref()
            && index.matches(workspace_epoch, config_fingerprint)
        {
            return Some(index.clone());
        }

        let documents = lock(&self.documents).clone();
        let index = WorkspaceIndex::build(&config, workspace_epoch, config_fingerprint, &documents);
        *lock(&self.workspace_index) = Some(index.clone());
        Some(index)
    }

    async fn document_highlight(
        &self,
        uri: Url,
        position: Position,
    ) -> Option<Vec<DocumentHighlight>> {
        let AnalysisSnapshot { text, facts, .. } = self.analysis_snapshot(&uri).await?;
        let (name, _) = word_at_position(&text, position)?;
        let mut highlights = Vec::new();
        let selected_definition = selected_reference_definition(&facts, &name, position)
            .or_else(|| selected_symbol_definition(&facts, &uri, &name, position));
        if let Some(definition) = &selected_definition {
            highlights.push(DocumentHighlight {
                range: definition.range,
                kind: Some(DocumentHighlightKind::WRITE),
            });
        } else {
            for symbol in facts.symbols.iter().filter(|symbol| symbol.name == name) {
                highlights.push(DocumentHighlight {
                    range: symbol.selection_range,
                    kind: Some(DocumentHighlightKind::WRITE),
                });
            }
        }
        for reference in facts.references.iter().filter(|reference| {
            reference.name == name
                && selected_definition
                    .as_ref()
                    .is_none_or(|definition| reference_targets_definition(reference, definition))
        }) {
            highlights.push(DocumentHighlight {
                range: reference.range,
                kind: Some(DocumentHighlightKind::READ),
            });
        }
        Some(highlights)
    }

    async fn semantic_tokens(&self, uri: Url) -> Option<SemanticTokensResult> {
        let facts = self.facts_for(&uri).await?;
        let mut symbols = facts.symbols;
        symbols.sort_by_key(|symbol| {
            (
                symbol.selection_range.start.line,
                symbol.selection_range.start.character,
                symbol.selection_range.end.character,
            )
        });

        let mut data = Vec::new();
        let mut last_line = 0;
        let mut last_start = 0;
        for (idx, symbol) in symbols.iter().enumerate() {
            let range = symbol.selection_range;
            if range.start.line != range.end.line {
                continue;
            }
            let delta_line = if idx == 0 {
                range.start.line
            } else {
                range.start.line.saturating_sub(last_line)
            };
            let delta_start = if delta_line == 0 {
                range.start.character.saturating_sub(last_start)
            } else {
                range.start.character
            };
            data.push(SemanticToken {
                delta_line,
                delta_start,
                length: range
                    .end
                    .character
                    .saturating_sub(range.start.character)
                    .max(1),
                token_type: semantic_token_type(symbol.kind),
                token_modifiers_bitset: 1,
            });
            last_line = range.start.line;
            last_start = range.start.character;
        }

        Some(SemanticTokensResult::Tokens(SemanticTokens {
            result_id: None,
            data,
        }))
    }

    async fn execute_command(
        &self,
        params: ExecuteCommandParams,
    ) -> Result<Option<Value>, ResponseError> {
        let mut args = params.arguments.into_iter();
        let first_arg = args.next().ok_or_else(|| {
            ResponseError::new(
                ErrorCode::INVALID_PARAMS,
                "Capy actions require one argument object",
            )
        })?;
        let mut action_args =
            serde_json::from_value::<ActionCommandArgs>(first_arg).map_err(|err| {
                ResponseError::new(
                    ErrorCode::INVALID_PARAMS,
                    format!("invalid Capy action arguments: {err}"),
                )
            })?;
        if action_args.action.is_empty() {
            action_args.action = params.command.clone();
        }
        if !known_action(&action_args.action) {
            return Err(ResponseError::new(
                ErrorCode::INVALID_PARAMS,
                format!("unknown Capy action: {}", action_args.action),
            ));
        }

        let (text, version) = {
            let documents = lock(&self.documents);
            if let Some(document) = documents.get(&action_args.uri) {
                (document.text.clone(), document.version)
            } else {
                let path = action_args.uri.to_file_path().map_err(|_| {
                    ResponseError::new(
                        ErrorCode::INVALID_PARAMS,
                        "Capy action URI must be a file URI",
                    )
                })?;
                (fs::read_to_string(path).map_err(io_response_error)?, 0)
            }
        };
        let workspace_root = lock(&self.workspace_root).clone();
        let path = action_args.uri.to_file_path().ok();
        let config = LspConfig::discover(path.as_deref(), &workspace_root).map_err(|err| {
            ResponseError::new(
                ErrorCode::INVALID_PARAMS,
                format!("invalid lsp-config.scm: {err}"),
            )
        })?;
        let workspace_epoch = self.current_epoch();
        let config_fingerprint = config.fingerprint();
        let context = AnalysisRequestContext {
            document_version: version,
            workspace_epoch,
            config_fingerprint,
        };
        let output = self
            .worker_pool
            .run_action(
                &config,
                &action_args.uri,
                path.as_deref(),
                version,
                &text,
                &action_args.action,
                action_args.range,
                workspace_epoch,
                config_fingerprint,
            )
            .await
            .map_err(io_response_error)?;
        if !self.is_result_still_current(&action_args.uri, context) {
            return Err(ResponseError::new(
                ErrorCode::CONTENT_MODIFIED,
                "document changed before Capy action completed",
            ));
        }

        serde_json::to_value(output)
            .map(Some)
            .map_err(|err| ResponseError::new(ErrorCode::INTERNAL_ERROR, err.to_string()))
    }

    async fn facts_for(&self, uri: &Url) -> Option<DocumentFacts> {
        self.analysis_snapshot(uri)
            .await
            .map(|snapshot| snapshot.facts)
    }

    fn config_for_uri(&self, uri: &Url) -> Option<LspConfig> {
        let workspace_root = lock(&self.workspace_root).clone();
        let path = uri.to_file_path().ok();
        LspConfig::discover(path.as_deref(), &workspace_root).ok()
    }

    async fn worker_load_path_for_uri(&self, uri: &Url) -> Option<(Vec<PathBuf>, Vec<String>)> {
        let config = self.config_for_uri(uri)?;
        match self.worker_pool.load_path(&config).await {
            Ok(load_path) => Some((load_path.load_path, load_path.extensions)),
            Err(err) => {
                tracing::warn!(uri = %uri, error = %err, "capy-lsp-vm load-path request failed");
                Some((config.effective_load_path(), config.extensions))
            }
        }
    }

    async fn analysis_snapshot(&self, uri: &Url) -> Option<AnalysisSnapshot> {
        let workspace_root = lock(&self.workspace_root).clone();
        let path = uri.to_file_path().ok();
        let config = match LspConfig::discover(path.as_deref(), &workspace_root) {
            Ok(config) => config,
            Err(err) => {
                let (text, version) = self.document_snapshot(uri)?;
                let mut facts = DocumentFacts::default();
                facts.diagnostics.push(DiagnosticFact {
                    range: Range::default(),
                    severity: DiagnosticSeverityFact::Error,
                    message: format!("invalid lsp-config.scm: {err}"),
                    source: Some("capy-lsp".into()),
                    code: None,
                });
                analysis::fill_missing_facts(uri, &text, &mut facts);
                let context = AnalysisRequestContext {
                    document_version: version,
                    workspace_epoch: self.current_epoch(),
                    config_fingerprint: 0,
                };
                if let Some(document) = lock(&self.documents).get_mut(uri)
                    && document.version == version
                {
                    document.facts = Some(CachedDocumentFacts::new(
                        facts.clone(),
                        context.document_version,
                        context.workspace_epoch,
                        context.config_fingerprint,
                    ));
                }
                return Some(AnalysisSnapshot {
                    text,
                    facts,
                    context,
                });
            }
        };
        let config_fingerprint = config.fingerprint();
        let (text, version, cached) = {
            let documents = lock(&self.documents);
            if let Some(document) = documents.get(uri) {
                (
                    document.text.clone(),
                    document.version,
                    document.facts.clone(),
                )
            } else {
                let path = uri.to_file_path().ok()?;
                (fs::read_to_string(path).ok()?, 0, None)
            }
        };

        if let Some(facts) = cached {
            if facts.matches(version, self.current_epoch(), config_fingerprint) {
                let context = AnalysisRequestContext {
                    document_version: version,
                    workspace_epoch: self.current_epoch(),
                    config_fingerprint,
                };
                return Some(AnalysisSnapshot {
                    text,
                    facts: facts.facts,
                    context,
                });
            }
        }

        let context = AnalysisRequestContext {
            document_version: version,
            workspace_epoch: self.current_epoch(),
            config_fingerprint,
        };
        let mut facts = match self
            .worker_pool
            .analyze(
                &config,
                uri,
                path.as_deref(),
                version,
                &text,
                context.workspace_epoch,
                context.config_fingerprint,
            )
            .await
        {
            Ok(facts) => facts,
            Err(err) => {
                tracing::warn!(uri = %uri, error = %err, "capy-lsp-vm analysis failed");
                let mut facts = DocumentFacts::default();
                facts.diagnostics.push(DiagnosticFact {
                    range: Range::default(),
                    severity: DiagnosticSeverityFact::Error,
                    message: format!("capy-lsp-vm analysis failed: {err}"),
                    source: Some("capy-lsp".into()),
                    code: Some("config".into()),
                });
                facts
            }
        };
        analysis::fill_missing_facts(uri, &text, &mut facts);

        if !self.is_result_still_current(uri, context) {
            return None;
        }

        if let Some(document) = lock(&self.documents).get_mut(uri) {
            if document.version == version {
                document.facts = Some(CachedDocumentFacts::new(
                    facts.clone(),
                    context.document_version,
                    context.workspace_epoch,
                    context.config_fingerprint,
                ));
            }
        }
        Some(AnalysisSnapshot {
            text,
            facts,
            context,
        })
    }

    fn document_snapshot(&self, uri: &Url) -> Option<(String, i32)> {
        if let Some(document) = lock(&self.documents).get(uri) {
            return Some((document.text.clone(), document.version));
        }
        let path = uri.to_file_path().ok()?;
        Some((fs::read_to_string(path).ok()?, 0))
    }

    fn is_result_still_current(&self, uri: &Url, context: AnalysisRequestContext) -> bool {
        let workspace_root = lock(&self.workspace_root).clone();
        let path = uri.to_file_path().ok();
        let current_config_fingerprint =
            if let Ok(config) = LspConfig::discover(path.as_deref(), &workspace_root) {
                Some(config.fingerprint())
            } else {
                None
            };
        let current_document_version = lock(&self.documents)
            .get(uri)
            .map(|document| document.version);
        analysis_context_matches(
            context,
            current_document_version,
            self.current_epoch(),
            current_config_fingerprint,
        )
    }

    async fn imported_completion_location(
        &self,
        facts: &DocumentFacts,
        name: &str,
    ) -> Option<Location> {
        let item = facts.completions.iter().find(|item| {
            item.label == name && (item.source_file.is_some() || item.source_module.is_some())
        })?;
        let file = item.source_file.as_ref()?;
        let uri = Url::from_file_path(file).ok()?;

        let source_name = item.source_name.as_deref().unwrap_or(name);
        if let Some(target_facts) = self.facts_for(&uri).await {
            if let Some(symbol) = target_facts
                .symbols
                .iter()
                .find(|symbol| symbol.name == source_name)
            {
                return Some(Location::new(uri, symbol.selection_range));
            }
        }

        Some(Location::new(uri, Range::default()))
    }
}
