use super::ffi::*;
use crate::prelude::*;
use crate::runtime::prelude::*;
use crate::runtime::vm::VMResult;

#[scheme(path=capy)]
mod dl_ops {
    #[scheme(name = "dlopen")]
    pub fn dlopen(name: Value<'gc>, flags: i32) -> Value<'gc> {
        let handle = if name == Value::new(false) {
            unsafe { libc::dlopen(std::ptr::null(), flags) }
        } else if name.is::<Str>() {
            let s = name.downcast::<Str>().to_string();
            let cstr = std::ffi::CString::new(s).unwrap();
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
                &[name.into(), flags.into()],
            );
        }

        let ptr = Gc::new(&nctx.ctx, Pointer::new(handle as _));
        nctx.return_(ptr.into())
    }
    #[scheme(name = "dlclose")]
    pub fn dlclose(handle: Gc<'gc, Pointer>) -> Value<'gc> {
        let handle_ptr = handle.value();

        let result = unsafe { libc::dlclose(handle_ptr as _) };
        if result != 0 {
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

        let sym_ptr = unsafe { libc::dlsym(handle_ptr as _, symbol_cstr.as_ptr()) };
        if sym_ptr.is_null() {
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

        let ptr = Gc::new(&nctx.ctx, Pointer::new(sym_ptr as _));
        nctx.return_(ptr.into())
    }

    #[scheme(name = "load-native-extension")]
    pub fn load_native_extension(path: StringRef<'gc>) -> Result<Value<'gc>, Value<'gc>> {
        let handle = unsafe {
            libc::dlopen(
                std::ffi::CString::new(path.to_string()).unwrap().as_ptr(),
                libc::RTLD_NOW | libc::RTLD_LOCAL,
            )
        };
        if handle.is_null() {
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
                &format!("failed to load {}: {}", path.to_string(), dlerror),
                &[path.into()],
            );
        }
        let init: extern "C-unwind" fn(&Context<'gc>) -> VMResult<'gc> = unsafe {
            let symbol = libc::dlsym(handle, b"capy_register_extension\0".as_ptr() as _);
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

        let result = init(&nctx.ctx);
        match result {
            VMResult::Ok(v) => nctx.return_(Ok(v)),
            VMResult::Err(e) => nctx.return_(Err(e)),
            _ => todo!(),
        }
    }
}
pub(crate) fn init_dl<'gc>(ctx: Context<'gc>) {
    dl_ops::register(ctx);
}
