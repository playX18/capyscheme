//! `#[onload]`, emit C `SNI_OnLoad` for native extension entry.

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
    let symbol = onload_symbol();
    // A statically linked extension has its entry renamed and is not asked for a version, and
    // several of them in one executable could not all define the same symbol.
    let version = if symbol == "SNI_OnLoad" {
        quote! {
            #[unsafe(export_name = "SNI_ABIVersion")]
            pub extern "C" fn SNI_ABIVersion() -> u32 {
                ::capy_sni_sys::SNI_ABI_VERSION
            }
        }
    } else {
        quote! {}
    };
    quote! {
        #func
        #version

        #[unsafe(export_name = #symbol)]
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
                let _ = unsafe { env.pop_local_frame(::capy_sni::Ref::null()) };
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

fn onload_symbol() -> String {
    // The environment of the compiler, which cargo sets for the crate being built.
    #[allow(clippy::disallowed_methods)]
    match std::env::var("CAPY_SNI_ONLOAD_SYMBOL") {
        Ok(symbol) if !symbol.is_empty() => symbol,
        _ => "SNI_OnLoad".to_string(),
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

#[cfg(test)]
mod tests {
    use super::*;
    use quote::quote;

    fn expand(attr: TokenStream, item: TokenStream) -> String {
        expand_onload(attr, item).to_string()
    }

    #[test]
    fn emits_the_entry_point_and_the_version() {
        let out = expand(quote! {}, quote! { fn init(env: &mut Env<'_>) {} });
        assert!(out.contains("SNI_OnLoad"), "{out}");
        assert!(out.contains("SNI_ABIVersion"), "{out}");
        assert!(out.contains("SNI_ABI_VERSION"), "{out}");
        assert!(
            out.contains("catch_unwind"),
            "a panic must not cross the C boundary: {out}"
        );
    }

    #[test]
    fn the_frame_attribute_sets_the_capacity() {
        let out = expand(
            quote! { frame = 7 },
            quote! { fn init(env: &mut Env<'_>) {} },
        );
        assert!(out.contains("push_local_frame (7"), "{out}");
    }

    #[test]
    fn the_default_frame_is_32() {
        let out = expand(quote! {}, quote! { fn init(env: &mut Env<'_>) {} });
        assert!(out.contains("push_local_frame (32"), "{out}");
    }

    #[test]
    fn unknown_attribute_is_a_compile_error() {
        let out = expand(
            quote! { depth = 3 },
            quote! { fn init(env: &mut Env<'_>) {} },
        );
        assert!(out.contains("compile_error"), "{out}");
    }

    #[test]
    fn the_first_argument_must_be_an_env() {
        let out = expand(quote! {}, quote! { fn init(n: i32) {} });
        assert!(out.contains("compile_error"), "{out}");
    }

    #[test]
    fn async_functions_are_refused() {
        let out = expand(quote! {}, quote! { async fn init(env: &mut Env<'_>) {} });
        assert!(out.contains("compile_error"), "{out}");
    }

    #[test]
    fn a_result_return_turns_an_error_into_the_exit_code() {
        let out = expand(
            quote! {},
            quote! { fn init(env: &mut Env<'_>) -> Result<(), i32> { Ok(()) } },
        );
        assert!(out.contains("Err (code)"), "{out}");
        let bad = expand(
            quote! {},
            quote! { fn init(env: &mut Env<'_>) -> String { todo() } },
        );
        assert!(bad.contains("compile_error"), "{bad}");
    }
}
