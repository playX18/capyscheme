use std::env;

fn main() {
    println!("cargo:rerun-if-env-changed=CAPY_SYSROOT");
    let sysroot = env::var("CAPY_SYSROOT").unwrap_or_else(|_| "/usr/local".to_string());
    println!("cargo:rustc-env=CAPY_SYSROOT={}", sysroot);
}
