mod analysis;
mod config;
mod document;
mod protocol;
mod server;
mod worker;

#[tokio::main(flavor = "current_thread")]
async fn main() {
    if let Err(err) = server::run().await {
        eprintln!("capy-lsp: {err}");
        std::process::exit(1);
    }
}
