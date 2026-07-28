use std::env;
use std::path::PathBuf;

fn main() {
    println!("cargo:rerun-if-env-changed=CAPY_LIB_DIR");
    println!("cargo:rerun-if-changed=../../c/capy.h");

    let lib_dir = env::var_os("CAPY_LIB_DIR")
        .map(PathBuf::from)
        .or_else(|| {
            // Workspace target/<triple>/<profile>
            let manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").ok()?);
            let profile = env::var("PROFILE").unwrap_or_else(|_| "debug".into());
            let target = env::var("TARGET").ok()?;
            // crates/capy-sni-sys → workspace root
            let root = manifest_dir.parent()?.parent()?;
            let candidate = root.join("target").join(&target).join(&profile);
            if candidate.join("libcapy.so").exists()
                || candidate.join("libcapy.dylib").exists()
                || candidate.join("capy.dll").exists()
            {
                return Some(candidate);
            }
            // Fallback: target/<profile> (host)
            let host = root.join("target").join(&profile);
            if host.join("libcapy.so").exists()
                || host.join("libcapy.dylib").exists()
                || host.join("capy.dll").exists()
            {
                Some(host)
            } else {
                Some(candidate)
            }
        })
        .expect("CAPY_LIB_DIR or workspace target dir");

    println!("cargo:rustc-link-search=native={}", lib_dir.display());
    println!("cargo:rustc-link-lib=dylib=capy");
    // Prefer RUNPATH so LD_LIBRARY_PATH can still override when needed.
    // Include $ORIGIN (portable/stage next to binary) and $ORIGIN/../lib (FHS).
    println!("cargo:rustc-link-arg=-Wl,--enable-new-dtags");
    println!(
        "cargo:rustc-link-arg=-Wl,-rpath,{}",
        "$ORIGIN:$ORIGIN/../lib"
    );
}
