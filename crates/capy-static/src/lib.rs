//! The runtime as a static library.

use capy::runtime::vm::dl::{StaticExtension, StaticOnLoad, register_static_extensions};
use std::ffi::{CStr, c_char, c_int};

#[repr(C)]
struct TableRow {
    key: *const c_char,
    init: Option<StaticOnLoad>,
}

unsafe extern "C" {
    /// Defined by the generated C file.
    fn capy_static_extension_table() -> *const TableRow;
}

fn registered_extensions() -> Vec<StaticExtension> {
    let mut extensions = Vec::new();
    // SAFETY: the generated table is a static array that ends with a null key.
    unsafe {
        let mut row = capy_static_extension_table();
        while !row.is_null() && !(*row).key.is_null() {
            let key = CStr::from_ptr((*row).key)
                .to_str()
                .expect("extension key is UTF-8");
            let init = (*row).init.expect("extension without an entry point");
            extensions.push(StaticExtension { key, init });
            row = row.add(1);
        }
    }
    extensions
}

#[unsafe(no_mangle)]
pub extern "C" fn main(_argc: c_int, _argv: *const *const c_char) -> c_int {
    register_static_extensions(&registered_extensions());
    capy_cli::run_cli("enter")
}
