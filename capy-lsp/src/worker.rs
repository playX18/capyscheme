use std::{
    env, io,
    path::{Path, PathBuf},
    process::Stdio,
};

use serde_json::json;
use tokio::{
    io::{AsyncBufReadExt, AsyncWriteExt, BufReader, Lines},
    process::{Child, ChildStdin, ChildStdout, Command},
    time::{Duration, timeout},
};

use crate::{
    config::LspConfig,
    protocol::{ActionOutput, DocumentFacts, WorkerRequest, WorkerResponse},
};

const WORKER_TIMEOUT: Duration = Duration::from_secs(10);

#[derive(Debug, Default)]
pub struct WorkerPool;

impl WorkerPool {
    pub async fn analyze(
        &self,
        config: &LspConfig,
        uri: &lsp_types::Url,
        path: Option<&Path>,
        version: i32,
        text: &str,
        workspace_epoch: u64,
        config_fingerprint: u64,
    ) -> io::Result<DocumentFacts> {
        let mut worker = Worker::spawn(config).await?;
        let request = json!({
            "uri": uri,
            "path": path.map(|path| path.to_string_lossy().into_owned()),
            "version": version,
            "text": text,
            "workspaceEpoch": workspace_epoch,
            "configFingerprint": config_fingerprint,
            "configRoot": config.root.to_string_lossy(),
            "loadPath": config.effective_load_path().iter().map(|path| path.to_string_lossy().into_owned()).collect::<Vec<_>>(),
            "compiledLoadPath": config.compiled_load_path.iter().map(|path| path.to_string_lossy().into_owned()).collect::<Vec<_>>(),
            "extensions": config.extensions,
            "defaultModule": config.default_module,
        });

        let result = worker
            .request("analyze-document", request)
            .await
            .and_then(|value| serde_json::from_value(value).map_err(invalid_data));
        worker.shutdown().await;
        result
    }

    pub async fn run_action(
        &self,
        config: &LspConfig,
        uri: &lsp_types::Url,
        path: Option<&Path>,
        version: i32,
        text: &str,
        action: &str,
        range: Option<lsp_types::Range>,
        workspace_epoch: u64,
        config_fingerprint: u64,
    ) -> io::Result<ActionOutput> {
        let mut worker = Worker::spawn(config).await?;
        let request = json!({
            "uri": uri,
            "path": path.map(|path| path.to_string_lossy().into_owned()),
            "version": version,
            "text": text,
            "action": action,
            "range": range,
            "workspaceEpoch": workspace_epoch,
            "configFingerprint": config_fingerprint,
            "configRoot": config.root.to_string_lossy(),
            "loadPath": config.effective_load_path().iter().map(|path| path.to_string_lossy().into_owned()).collect::<Vec<_>>(),
            "compiledLoadPath": config.compiled_load_path.iter().map(|path| path.to_string_lossy().into_owned()).collect::<Vec<_>>(),
            "extensions": config.extensions,
            "defaultModule": config.default_module,
        });

        let result = worker
            .request("run-action", request)
            .await
            .and_then(|value| serde_json::from_value(value).map_err(invalid_data));
        worker.shutdown().await;
        result
    }
}

#[derive(Debug)]
struct Worker {
    child: Child,
    stdin: ChildStdin,
    stdout: Lines<BufReader<ChildStdout>>,
    next_id: u64,
    #[allow(dead_code)]
    broken: bool,
}

impl Worker {
    async fn spawn(config: &LspConfig) -> io::Result<Self> {
        let executable = worker_executable()?;
        let mut command = Command::new(executable);
        command
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::inherit())
            .current_dir(&config.root)
            .env("CAPY_LOAD_PATH", config.load_path_env());
        if let Some(path) = config.compiled_load_path_env() {
            command.env("CAPY_LOAD_COMPILED_PATH", path);
        }
        for (key, value) in &config.env {
            command.env(key, value);
        }
        for (key, value) in &config.gc_env {
            command.env(key, value);
        }

        let mut child = command.spawn()?;
        let stdin = child
            .stdin
            .take()
            .ok_or_else(|| io::Error::new(io::ErrorKind::BrokenPipe, "worker stdin unavailable"))?;
        let stdout = child.stdout.take().ok_or_else(|| {
            io::Error::new(io::ErrorKind::BrokenPipe, "worker stdout unavailable")
        })?;
        Ok(Self {
            child,
            stdin,
            stdout: BufReader::new(stdout).lines(),
            next_id: 1,
            broken: false,
        })
    }

    async fn shutdown(&mut self) {
        if !self.broken {
            let _ = self.request("shutdown", json!({})).await;
        }
        let _ = self.child.kill().await;
    }

    async fn request(
        &mut self,
        method: impl Into<String>,
        params: serde_json::Value,
    ) -> io::Result<serde_json::Value> {
        if self.broken {
            return Err(io::Error::new(
                io::ErrorKind::BrokenPipe,
                "capy-lsp-vm worker is no longer usable",
            ));
        }

        let result = self.request_inner(method, params).await;
        if result.is_err() {
            self.broken = true;
            let _ = self.child.kill().await;
        }
        result
    }

    async fn request_inner(
        &mut self,
        method: impl Into<String>,
        params: serde_json::Value,
    ) -> io::Result<serde_json::Value> {
        let id = self.next_id;
        self.next_id += 1;
        let request = WorkerRequest {
            id,
            method: method.into(),
            params,
        };
        let line = serde_json::to_string(&request).map_err(invalid_data)?;
        self.stdin.write_all(line.as_bytes()).await?;
        self.stdin.write_all(b"\n").await?;
        self.stdin.flush().await?;

        let response = timeout(WORKER_TIMEOUT, self.stdout.next_line())
            .await
            .map_err(|_| io::Error::new(io::ErrorKind::TimedOut, "capy-lsp-vm timed out"))??;
        let response = response
            .ok_or_else(|| io::Error::new(io::ErrorKind::UnexpectedEof, "worker exited"))?;
        let response: WorkerResponse = serde_json::from_str(&response).map_err(invalid_data)?;
        if response.id != id {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!(
                    "worker response id mismatch: expected {id}, got {}",
                    response.id
                ),
            ));
        }
        if response.ok {
            Ok(response.result.unwrap_or(serde_json::Value::Null))
        } else {
            Err(io::Error::new(
                io::ErrorKind::Other,
                response
                    .error
                    .map(worker_error_message)
                    .unwrap_or_else(|| "worker request failed".into()),
            ))
        }
    }
}

fn worker_error_message(error: serde_json::Value) -> String {
    if let Some(message) = error.as_str() {
        return message.to_string();
    }
    if let Some(message) = error.get("message").and_then(serde_json::Value::as_str) {
        return message.to_string();
    }
    error.to_string()
}

fn worker_executable() -> io::Result<PathBuf> {
    if let Some(path) = env::var_os("CAPY_LSP_VM") {
        return validate_worker_executable(PathBuf::from(path), "CAPY_LSP_VM");
    }
    if let Ok(current_exe) = env::current_exe()
        && let Some(dir) = current_exe.parent()
    {
        let sibling = dir.join("capy-lsp-vm");
        if sibling.exists() {
            return validate_worker_executable(sibling, "sibling capy-lsp-vm");
        }
    }
    let repo_bin = PathBuf::from("bin/capy-lsp-vm");
    if repo_bin.exists() {
        return validate_worker_executable(repo_bin, "repository capy-lsp-vm");
    }
    validate_worker_executable(PathBuf::from("capy-lsp-vm"), "PATH capy-lsp-vm")
}

fn validate_worker_executable(path: PathBuf, source: &str) -> io::Result<PathBuf> {
    if path
        .file_name()
        .is_some_and(|name| name == "capy-lsp" || name == "capy-lsp.exe")
    {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            format!(
                "{source} points to the capy-lsp server ({}) instead of the capy-lsp-vm worker",
                path.display()
            ),
        ));
    }
    Ok(path)
}

fn invalid_data(err: impl std::error::Error + Send + Sync + 'static) -> io::Error {
    io::Error::new(io::ErrorKind::InvalidData, err)
}
