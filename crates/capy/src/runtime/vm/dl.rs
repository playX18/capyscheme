use std::collections::HashMap;
use std::sync::LazyLock;

use crate::heap::mmtk::util::Address;
use parking_lot::Mutex;

use super::ffi::*;
use crate::prelude::*;
use crate::runtime::prelude::*;
use crate::runtime::vm::ExecutionResult;

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
            let cstr = std::ffi::CString::new(s).expect("invariant holds");
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
            Gc::new_with_header_word(nctx.ctx, Pointer::new(handle as _), pointer_header_word());
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
            std::ffi::CString::new(s).expect("invariant holds")
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
            Gc::new_with_header_word(nctx.ctx, Pointer::new(sym_ptr as _), pointer_header_word());
        nctx.return_(ptr.into())
    }

    #[scheme(name = "load-native-extension")]
    pub fn load_native_extension(path: StringRef<'gc>) -> Result<Value<'gc>, Value<'gc>> {
        // SAFETY: FFI: arguments follow POSIX dlopen requirements
        let handle = unsafe {
            libc::dlopen(
                std::ffi::CString::new(path.to_string())
                    .expect("invariant holds")
                    .as_ptr(),
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
        // Prefer C SNI_OnLoad(SniEnv*, void*) → int; fall back to legacy Rust ABI.
        // SAFETY: handle is valid.
        let sni_onload = unsafe { libc::dlsym(handle, c"SNI_OnLoad".as_ptr() as _) };
        if !sni_onload.is_null() {
            type OnLoad = unsafe extern "C" fn(
                *mut crate::api::SniEnv<'static>,
                *mut libc::c_void,
            ) -> libc::c_int;
            // SAFETY: symbol resolved from the extension.
            let init: OnLoad = unsafe { std::mem::transmute(sni_onload) };
            let mut env = crate::api::SniEnv::new(nctx.ctx);
            let env_ptr = env.as_ptr();
            // SAFETY: extension OnLoad contract.
            let rc = unsafe { init(env_ptr, std::ptr::null_mut()) };
            if rc != 0 {
                return nctx.raise_error(
                    "load-native-extension",
                    &format!("SNI_OnLoad failed with code {rc} in {path}"),
                    &[path.into()],
                );
            }
            LOADED_EXTENSIONS.lock().push(Extension {
                path: PathBuf::from(path.to_string()),
                handle: Address::from_ptr(handle),
            });
            return nctx.return_(Ok(Value::new(true)));
        }

        // SAFETY: FFI: handle is valid, symbol name is NUL-terminated
        let init: extern "C-unwind" fn(Context<'gc>) -> ExecutionResult<'gc> = unsafe {
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
                        "failed to find SNI_OnLoad or capy_register_extension in {path}: {dlerror}"
                    ),
                    &[path.into()],
                );
            }
            std::mem::transmute(symbol)
        };

        let result = init(nctx.ctx);
        match result {
            ExecutionResult::Ok(v) => {
                LOADED_EXTENSIONS.lock().push(Extension {
                    path: PathBuf::from(path.to_string()),
                    handle: Address::from_ptr(handle),
                });
                nctx.return_(Ok(v))
            }
            ExecutionResult::Err(e) => nctx.return_(Err(e)),
        }
    }
}
pub(crate) fn init_dl<'gc>(ctx: Context<'gc>) {
    dl_ops::register(ctx);
}
