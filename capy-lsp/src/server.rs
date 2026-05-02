use std::{
    collections::{HashMap, HashSet},
    env, fs,
    ops::ControlFlow,
    path::{Path, PathBuf},
    sync::{Arc, Mutex},
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
    DocumentHighlight, DocumentHighlightKind, DocumentSymbolResponse, ExecuteCommandOptions,
    ExecuteCommandParams, FullDocumentDiagnosticReport, GotoDefinitionResponse, Hover,
    HoverContents, HoverProviderCapability, InitializeParams, InitializeResult, InsertTextFormat,
    Location, OneOf, Position, PublishDiagnosticsParams, Range,
    RelatedFullDocumentDiagnosticReport, SemanticToken, SemanticTokens, SemanticTokensFullOptions,
    SemanticTokensOptions, SemanticTokensResult, SemanticTokensServerCapabilities,
    ServerCapabilities, ServerInfo, SignatureHelp, SignatureHelpOptions,
    TextDocumentSyncCapability, TextDocumentSyncKind, TextDocumentSyncOptions,
    TextDocumentSyncSaveOptions, Url, WorkDoneProgressOptions, WorkspaceSymbolResponse,
    notification, request,
};
use serde_json::Value;
use tower::ServiceBuilder;
use tracing::Level;

use crate::{
    analysis,
    config::LspConfig,
    document::{Document, word_at_position},
    protocol::{DiagnosticSeverityFact, DocumentFacts},
    worker::WorkerPool,
};

use actions::{ActionCommandArgs, action_definitions, known_action};
use convert::{
    completion_fact_detail, format_hover_content, to_completion_kind, to_diagnostic,
    to_document_symbol, to_location, to_symbol_information,
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
                state.service.mark_all_dirty();
                let service = state.service.clone();
                tokio::spawn(async move {
                    service.worker_pool.restart_all().await;
                });
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
    worker_pool: Arc<WorkerPool>,
}

impl LanguageService {
    fn new(client: ClientSocket) -> Self {
        Self {
            client,
            workspace_root: Mutex::new(env::current_dir().unwrap_or_else(|_| PathBuf::from("."))),
            documents: Mutex::new(HashMap::new()),
            worker_pool: Arc::new(WorkerPool::default()),
        }
    }

    fn initialize(&self, params: InitializeParams) -> InitializeResult {
        if let Some(root) = workspace_root_from_initialize(&params) {
            *lock(&self.workspace_root) = root;
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
        self.worker_pool.shutdown_all().await;
        let _ = self.client.clone();
    }

    fn did_open(self: &Arc<Self>, params: lsp_types::DidOpenTextDocumentParams) {
        let document = params.text_document;
        let uri = document.uri.clone();
        lock(&self.documents).insert(
            document.uri.clone(),
            Document::new(document.uri, document.version, document.text),
        );
        self.schedule_publish_diagnostics(uri);
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

        self.schedule_publish_diagnostics(uri);
    }

    fn did_save(self: &Arc<Self>, params: lsp_types::DidSaveTextDocumentParams) {
        let uri = params.text_document.uri.clone();
        if let Some(text) = params.text {
            if let Some(document) = lock(&self.documents).get_mut(&params.text_document.uri) {
                document.text = text;
                document.dirty = true;
            }
        }
        self.schedule_publish_diagnostics(uri);
    }

    fn did_close(self: &Arc<Self>, params: lsp_types::DidCloseTextDocumentParams) {
        let uri = params.text_document.uri;
        lock(&self.documents).remove(&uri);
        self.publish_empty_diagnostics(uri);
    }

    fn did_change_watched_files(self: &Arc<Self>, params: lsp_types::DidChangeWatchedFilesParams) {
        let mut config_roots = Vec::new();
        let mut dirty_uris = Vec::new();
        {
            let mut documents = lock(&self.documents);
            for event in &params.changes {
                if file_name(&event.uri).as_deref() == Some("lsp-config.scm") {
                    if let Some(root) = event
                        .uri
                        .to_file_path()
                        .ok()
                        .and_then(|path| path.parent().map(Path::to_path_buf))
                    {
                        config_roots.push(root);
                    }
                    for document in documents.values_mut() {
                        document.dirty = true;
                    }
                    dirty_uris.extend(documents.keys().cloned());
                } else if let Some(document) = documents.get_mut(&event.uri) {
                    document.dirty = true;
                    dirty_uris.push(event.uri.clone());
                }
            }
        }

        let pool = self.worker_pool.clone();
        tokio::spawn(async move {
            if config_roots.is_empty() {
                return;
            }
            for root in config_roots {
                pool.restart_root(&root).await;
            }
        });

        for uri in dirty_uris {
            self.schedule_publish_diagnostics(uri);
        }
    }

    fn mark_all_dirty(&self) {
        for document in lock(&self.documents).values_mut() {
            document.dirty = true;
        }
    }

    fn schedule_publish_diagnostics(self: &Arc<Self>, uri: Url) {
        let service = self.clone();
        tokio::spawn(async move {
            service.publish_diagnostics(uri).await;
        });
    }

    async fn publish_diagnostics(self: Arc<Self>, uri: Url) {
        let facts = self.facts_for(&uri).await.unwrap_or_default();
        let version = lock(&self.documents)
            .get(&uri)
            .map(|document| document.version);
        let diagnostics = facts.diagnostics.into_iter().map(to_diagnostic).collect();
        let _ =
            self.client
                .notify::<notification::PublishDiagnostics>(PublishDiagnosticsParams::new(
                    uri,
                    diagnostics,
                    version,
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
        let facts = self.facts_for(&uri).await.unwrap_or_default();
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
        let (text, facts) = self.text_and_facts(&uri).await?;
        let (name, range) = analysis::symbol_at(&facts, &text, position)?;
        let detail = facts
            .symbols
            .iter()
            .find(|symbol| symbol.name == name)
            .and_then(|symbol| symbol.detail.clone())
            .or_else(|| {
                facts
                    .references
                    .iter()
                    .find(|reference| reference.name == name)
                    .and_then(|_| completion_detail(&facts, &name))
            })
            .or_else(|| completion_detail(&facts, &name))
            .unwrap_or_else(|| "identifier".into());

        Some(Hover {
            contents: HoverContents::Markup(format_hover_content(&name, &detail)),
            range: Some(range),
        })
    }

    async fn definition(&self, uri: Url, position: Position) -> Option<GotoDefinitionResponse> {
        let (text, facts) = self.text_and_facts(&uri).await?;
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
        let (text, facts) = self.text_and_facts(&uri).await?;
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
        let (text, facts) = self.text_and_facts(&uri).await?;
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
                    insert_text,
                    insert_text_format,
                    ..CompletionItem::default()
                });
            }
        }
        Some(CompletionResponse::Array(items))
    }

    async fn signature_help(&self, uri: Url, position: Position) -> Option<SignatureHelp> {
        let (text, facts) = self.text_and_facts(&uri).await?;
        let call = enclosing_call(&text, position)?;
        let detail = completion_detail(&facts, &call.name).or_else(|| {
            facts
                .symbols
                .iter()
                .find(|symbol| symbol.name == call.name)
                .and_then(|symbol| symbol.detail.clone())
        });
        let signature = callable_signature(&call.name, detail.as_deref())?;
        Some(SignatureHelp {
            signatures: vec![signature_information(&signature)],
            active_signature: Some(0),
            active_parameter: Some(
                call.active_parameter
                    .min(signature.params.len().saturating_sub(1) as u32),
            ),
        })
    }

    async fn document_symbols(&self, uri: Url) -> Option<DocumentSymbolResponse> {
        let (text, facts) = self.text_and_facts(&uri).await?;
        let import_symbols = analysis::import_symbols(&text, &facts);
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
        let uris: Vec<Url> = lock(&self.documents).keys().cloned().collect();
        let mut symbols = Vec::new();
        for uri in uris {
            let Some(facts) = self.facts_for(&uri).await else {
                continue;
            };
            for symbol in facts.symbols {
                if query.is_empty() || symbol.name.to_lowercase().contains(&query) {
                    symbols.push(to_symbol_information(uri.clone(), &symbol));
                }
            }
        }
        Some(WorkspaceSymbolResponse::Flat(symbols))
    }

    async fn document_highlight(
        &self,
        uri: Url,
        position: Position,
    ) -> Option<Vec<DocumentHighlight>> {
        let (text, facts) = self.text_and_facts(&uri).await?;
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
            )
            .await
            .map_err(io_response_error)?;

        serde_json::to_value(output)
            .map(Some)
            .map_err(|err| ResponseError::new(ErrorCode::INTERNAL_ERROR, err.to_string()))
    }

    async fn text_and_facts(&self, uri: &Url) -> Option<(String, DocumentFacts)> {
        let text = self.document_text(uri)?;
        let facts = self.facts_for(uri).await?;
        Some((text, facts))
    }

    fn document_text(&self, uri: &Url) -> Option<String> {
        if let Some(document) = lock(&self.documents).get(uri) {
            return Some(document.text.clone());
        }
        uri.to_file_path()
            .ok()
            .and_then(|path| fs::read_to_string(path).ok())
    }

    async fn facts_for(&self, uri: &Url) -> Option<DocumentFacts> {
        let (text, version, cached, dirty) = {
            let documents = lock(&self.documents);
            if let Some(document) = documents.get(uri) {
                (
                    document.text.clone(),
                    document.version,
                    document.facts.clone(),
                    document.dirty,
                )
            } else {
                let path = uri.to_file_path().ok()?;
                (fs::read_to_string(path).ok()?, 0, None, true)
            }
        };

        if !dirty {
            if let Some(facts) = cached {
                return Some(facts);
            }
        }

        let workspace_root = lock(&self.workspace_root).clone();
        let path = uri.to_file_path().ok();
        let mut facts = match LspConfig::discover(path.as_deref(), &workspace_root) {
            Ok(config) => match self
                .worker_pool
                .analyze(&config, uri, path.as_deref(), version, &text)
                .await
            {
                Ok(facts) => facts,
                Err(err) => {
                    tracing::warn!(uri = %uri, error = %err, "capy-lsp-vm analysis failed");
                    analysis::analyze_syntax(uri, &text)
                }
            },
            Err(err) => {
                let mut facts = analysis::analyze_syntax(uri, &text);
                facts.diagnostics.push(crate::protocol::DiagnosticFact {
                    range: Range::default(),
                    severity: DiagnosticSeverityFact::Error,
                    message: format!("invalid lsp-config.scm: {err}"),
                    source: Some("capy-lsp".into()),
                });
                facts
            }
        };
        analysis::fill_missing_facts(uri, &text, &mut facts);

        if let Some(document) = lock(&self.documents).get_mut(uri) {
            if document.version == version {
                document.facts = Some(facts.clone());
                document.dirty = false;
            }
        }
        Some(facts)
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
