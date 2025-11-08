use std::env;

fn main() {
    let crate_dir = env::var("CARGO_MANIFEST_DIR").unwrap();

    cbindgen::Builder::new()
        .with_crate(crate_dir)
        .with_cpp_compat(false)
        .with_language(cbindgen::Language::C)
        .with_std_types(true)
        .exclude_item("returnaddress")
        .exclude_item("BOX_VTABLE")
        .exclude_item("PAIR_VTABLE")
        .exclude_item("TUPLE_VTABLE")
        .exclude_item("VECTOR_VTABLE")
        .with_define("GCObject", "void*", "<GCObject>")
        .generate()
        .expect("Unable to generate bindings")
        .write_to_file("capy-priv.h");
}
