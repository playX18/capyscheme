// A build script reads Cargo's environment directly: `capy::utils::flags` is not
// available here, and these variables are Cargo's, not the runtime's.
#![allow(clippy::disallowed_methods)]

fn main() {
    // `capyc --exe` adds a segment to a copy of these binaries, which needs room among the
    // load commands
    if std::env::var("CARGO_CFG_TARGET_OS").as_deref() == Ok("macos") {
        println!("cargo:rustc-link-arg-bins=-Wl,-headerpad,0x1000");
    }
    println!("cargo:rerun-if-changed=build.rs");
}
