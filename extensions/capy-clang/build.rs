fn main() {
    // Add our app's build directory to the lib search path.
    println!("cargo:rustc-link-search=target/x86_64-unknown-linux-gnu/release");
    // Add the app's dependency directory to the lib search path.
    // This is may be required if the app depends on any external "derive"
    // crates like the `dlopen_derive` crate that we add later.
    println!("cargo:rustc-link-search=target/x86_64-unknown-linux-gnu/release");
    // Link to the `capy` crate library. This tells cargo to actually link
    // to the `capy` crate that we include using `extern crate app;`.
    println!("cargo:rustc-link-lib=capy");
}
