#![allow(clippy::disallowed_methods)]

use std::env;

fn main() {
    let origin = if env::var("CARGO_CFG_TARGET_OS").as_deref() == Ok("macos") {
        "@loader_path/.."
    } else {
        "$ORIGIN/.."
    };
    println!("cargo:rustc-link-arg-cdylib=-Wl,-rpath,{origin}");
}
