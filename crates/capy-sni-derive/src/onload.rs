//! `#[onload]` — emit C `SNI_OnLoad` for native extension entry.

use proc_macro2::TokenStream;
use quote::quote;
use syn::{FnArg, ItemFn, LitInt, ReturnType, Type, parse::Parser};

pub fn expand_onload(attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut frame_capacity: i32 = 32;
    if !attr.is_empty() {
        let parser = syn::meta::parser(|meta| {
            if meta.path.is_ident("frame") {
                let value: LitInt = meta.value()?.parse()?;
                frame_capacity = value.base10_parse()?;
                Ok(())
            } else {
                Err(meta.error("unknown #[onload] attribute; expected `frame = N`"))
            }
        });
        if let Err(e) = parser.parse2(attr) {
            return e.to_compile_error();
        }
    }

    let func: ItemFn = match syn::parse2(item) {
        Ok(f) => f,
        Err(e) => return e.to_compile_error(),
    };

    if func.sig.asyncness.is_some() {
        return quote! { compile_error!("#[onload] does not support async fn"); };
    }

    let env_ok = func.sig.inputs.first().is_some_and(|arg| match arg {
        FnArg::Typed(pat) => is_env_ty(&pat.ty),
        _ => false,
    });
    if !env_ok {
        return quote! {
            compile_error!("#[onload] function must take `&mut Env` (or `&mut Env<'_>`) as its first argument");
        };
    }

    let returns_result = matches!(&func.sig.output, ReturnType::Type(_, ty) if is_result_ty(ty));
    if !matches!(func.sig.output, ReturnType::Default)
        && !returns_result
        && !matches!(&func.sig.output, ReturnType::Type(_, ty) if is_unit_ty(ty))
    {
        return quote! {
            compile_error!("#[onload] return type must be `()` or `Result<(), i32>`");
        };
    }

    let user_ident = &func.sig.ident;
    let call_ts = if returns_result {
        quote! {
            match #user_ident(&mut env) {
                Ok(()) => 0,
                Err(code) => code as ::core::ffi::c_int,
            }
        }
    } else {
        quote! {
            #user_ident(&mut env);
            0
        }
    };

    let frame = frame_capacity;
    quote! {
        #func

        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn SNI_OnLoad(
            env: *mut ::capy_sni_sys::SniEnv,
            _reserved: *mut ::core::ffi::c_void,
        ) -> ::core::ffi::c_int {
            if env.is_null() {
                return -1;
            }
            let outcome = ::std::panic::catch_unwind(::std::panic::AssertUnwindSafe(|| {
                // SAFETY: env is the live SniEnv* passed by load-native-extension.
                let mut env = unsafe { ::capy_sni::Env::from_raw(env) };
                if env.push_local_frame(#frame).is_err() {
                    return -1;
                }
                let code = { #call_ts };
                let _ = env.pop_local_frame(::capy_sni::Ref::null());
                code
            }));
            match outcome {
                Ok(code) => code,
                Err(_) => {
                    eprintln!("capy-sni: panic in SNI_OnLoad; returning -1");
                    -1
                }
            }
        }
    }
}

fn is_env_ty(ty: &Type) -> bool {
    let Type::Reference(r) = ty else {
        return false;
    };
    match &*r.elem {
        Type::Path(p) => p.path.segments.last().is_some_and(|s| s.ident == "Env"),
        _ => false,
    }
}

fn is_result_ty(ty: &Type) -> bool {
    match ty {
        Type::Path(p) => p.path.segments.last().is_some_and(|s| s.ident == "Result"),
        _ => false,
    }
}

fn is_unit_ty(ty: &Type) -> bool {
    matches!(ty, Type::Tuple(t) if t.elems.is_empty())
}
