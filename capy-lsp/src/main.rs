mod analysis;
mod config;
mod document;
mod protocol;
mod server;
mod worker;
mod workspace_index;

#[tokio::main(flavor = "current_thread")]
async fn main() {
    if let Err(err) = server::run().await {
        eprintln!("capy-lsp: {err}");
        std::process::exit(1);
    }
}
