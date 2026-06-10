use std::collections::HashMap;
use std::sync::LazyLock;

use crate::rsgc::mmtk::util::Address;
use parking_lot::Mutex;

use super::ffi::*;
use crate::prelude::*;
use crate::runtime::prelude::*;
use crate::runtime::vm::VMResult;

#[allow(dead_code)]
pub(crate) struct DynLib {
    pub path: Option<std::path::PathBuf>,
    #[allow(dead_code)]
    pub handle: Address,
    pub flags: i32,
}

pub(crate) static OPEN_HANDLES: LazyLock<Mutex<HashMap<Address, DynLib>>> =
    LazyLock::new(|| Mutex::new(HashMap::new()));

#[allow(dead_code)]
pub(crate) struct Extension {
    pub path: std::path::PathBuf,
    pub handle: Address,
}

pub(crate) static LOADED_EXTENSIONS: LazyLock<Mutex<Vec<Extension>>> =
    LazyLock::new(|| Mutex::new(Vec::new()));

#[scheme(path=capy)]
mod dl_ops {
    use std::path::PathBuf;

    #[scheme(name = "dlopen")]
    pub fn dlopen(name: Value<'gc>, flags: i32) -> Value<'gc> {
        let handle = if name == Value::new(false) {
            // SAFETY: FFI: arguments follow POSIX dlopen requirements
            unsafe { libc::dlopen(std::ptr::null(), flags) }
        } else if name.is::<Str>() {
            let s = name.downcast::<Str>().to_string();
            let cstr = std::ffi::CString::new(s).unwrap();
            // SAFETY: FFI: arguments follow POSIX dlopen requirements
            unsafe { libc::dlopen(cstr.as_ptr(), flags) }
        } else {
            return nctx.wrong_argument_violation(
                "dlopen",
                "expected string or false",
                None,
                None,
                2,
                &[name, flags.into()],
            );
        };

        if handle.is_null() {
            // SAFETY: FFI: dlerror call is safe after a failed dlopen/dlsym
            let dlerror = unsafe {
                let err_ptr = libc::dlerror();
                if err_ptr.is_null() {
                    "unknown error".to_string()
                } else {
                    let cstr = std::ffi::CStr::from_ptr(err_ptr);
                    cstr.to_string_lossy().into_owned()
                }
            };
            return nctx.raise_error(
                "dlopen",
                &format!("file: {name}, error: {dlerror}"),
                &[name, flags.into()],
            );
        }
        OPEN_HANDLES.lock().insert(
            Address::from_ptr(handle),
            DynLib {
                handle: Address::from_ptr(handle),
                path: if name.is::<Str>() {
                    Some(PathBuf::from(name.downcast::<Str>().to_string()))
                } else {
                    None
                },
                flags,
            },
        );
        let ptr =
            Gc::new_with_header_word(*nctx.ctx, Pointer::new(handle as _), pointer_header_word());
        nctx.return_(ptr.into())
    }
    #[scheme(name = "dlclose")]
    pub fn dlclose(handle: Gc<'gc, Pointer>) -> Value<'gc> {
        let handle_ptr = handle.value();

        // SAFETY: FFI: handle was obtained from a previous dlopen
        let result = unsafe { libc::dlclose(handle_ptr as _) };
        if result != 0 {
            // SAFETY: FFI: dlerror call is safe after a failed dlopen/dlsym
            let dlerror = unsafe {
                let err_ptr = libc::dlerror();
                if err_ptr.is_null() {
                    "unknown error".to_string()
                } else {
                    let cstr = std::ffi::CStr::from_ptr(err_ptr);
                    cstr.to_string_lossy().into_owned()
                }
            };
            return nctx.raise_error(
                "dlclose",
                &format!("handle: {handle:p}, error: {dlerror}"),
                &[handle.into()],
            );
        }

        OPEN_HANDLES
            .lock()
            .remove(&Address::from_ptr(handle_ptr as _));

        nctx.return_(Value::new(true))
    }

    #[scheme(name = "dlsym")]
    pub fn dlsym(handle: Gc<'gc, Pointer>, symbol: Value<'gc>) -> Value<'gc> {
        let handle_ptr = handle.value();

        let symbol_cstr = if symbol.is::<Str>() {
            let s = symbol.downcast::<Str>().to_string();
            std::ffi::CString::new(s).unwrap()
        } else {
            return nctx.wrong_argument_violation(
                "dlsym",
                "expected string",
                None,
                None,
                2,
                &[handle.into(), symbol],
            );
        };

        // SAFETY: FFI: handle is valid, symbol name is NUL-terminated
        let sym_ptr = unsafe { libc::dlsym(handle_ptr as _, symbol_cstr.as_ptr()) };
        if sym_ptr.is_null() {
            // SAFETY: FFI: dlerror call is safe after a failed dlopen/dlsym
            let dlerror = unsafe {
                let err_ptr = libc::dlerror();
                if err_ptr.is_null() {
                    "unknown error".to_string()
                } else {
                    let cstr = std::ffi::CStr::from_ptr(err_ptr);
                    cstr.to_string_lossy().into_owned()
                }
            };
            return nctx.raise_error(
                "dlsym",
                &format!(
                    "handle: {handle:p}, symbol: {}, error: {dlerror}",
                    symbol_cstr.to_string_lossy()
                ),
                &[handle.into(), symbol],
            );
        }

        let ptr =
            Gc::new_with_header_word(*nctx.ctx, Pointer::new(sym_ptr as _), pointer_header_word());
        nctx.return_(ptr.into())
    }

    #[scheme(name = "load-native-extension")]
    pub fn load_native_extension(path: StringRef<'gc>) -> Result<Value<'gc>, Value<'gc>> {
        // SAFETY: FFI: arguments follow POSIX dlopen requirements
        let handle = unsafe {
            libc::dlopen(
                std::ffi::CString::new(path.to_string()).unwrap().as_ptr(),
                libc::RTLD_NOW | libc::RTLD_LOCAL,
            )
        };
        if handle.is_null() {
            // SAFETY: FFI: dlerror call is safe after a failed dlopen/dlsym
            let dlerror = unsafe {
                let err_ptr = libc::dlerror();
                if err_ptr.is_null() {
                    "unknown error".to_string()
                } else {
                    let cstr = std::ffi::CStr::from_ptr(err_ptr);
                    cstr.to_string_lossy().into_owned()
                }
            };
            return nctx.raise_error(
                "load-native-extension",
                &format!("failed to load {}: {}", path, dlerror),
                &[path.into()],
            );
        }
        // SAFETY: FFI: handle is valid, symbol name is NUL-terminated
        let init: extern "C-unwind" fn(Context<'gc>) -> VMResult<'gc> = unsafe {
            let symbol = libc::dlsym(handle, c"capy_register_extension".as_ptr() as _);
            if symbol.is_null() {
                let dlerror = {
                    let err_ptr = libc::dlerror();
                    if err_ptr.is_null() {
                        "unknown error".to_string()
                    } else {
                        let cstr = std::ffi::CStr::from_ptr(err_ptr);
                        cstr.to_string_lossy().into_owned()
                    }
                };
                return nctx.raise_error(
                    "load-native-extension",
                    &format!(
                        "failed to find symbol 'capy_register_extension' in {path}: {dlerror}"
                    ),
                    &[path.into()],
                );
            }
            std::mem::transmute(symbol)
        };

        let result = init(nctx.ctx);
        match result {
            VMResult::Ok(v) => {
                LOADED_EXTENSIONS.lock().push(Extension {
                    path: PathBuf::from(path.to_string()),
                    handle: Address::from_ptr(handle),
                });
                nctx.return_(Ok(v))
            }
            VMResult::Err(e) => nctx.return_(Err(e)),
        }
    }
}
pub(crate) fn init_dl<'gc>(ctx: Context<'gc>) {
    dl_ops::register(ctx);
}
