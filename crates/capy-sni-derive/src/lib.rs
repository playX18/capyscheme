//! `#[scheme]` / `#[cps]` / `#[onload]` attribute macros for `capy-sni` embedders.

mod cps;
mod onload;

use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{FnArg, ItemFn, ItemImpl, ItemStruct, ItemTrait, ReturnType, Type, token::Comma};

/// CPS transform: `cps::call` / `raise` / `guard` become continuation legs.
#[proc_macro_attribute]
pub fn cps(attr: TokenStream, item: TokenStream) -> TokenStream {
    cps::expand_cps(attr.into(), item.into()).into()
}

/// Emit `SNI_OnLoad` for a native extension entry function.
///
/// ```ignore
/// #[onload(frame = 32)]
/// fn init(env: &mut Env<'_>) {
///     scheme_register_foo(env);
/// }
/// ```
#[proc_macro_attribute]
pub fn onload(attr: TokenStream, item: TokenStream) -> TokenStream {
    onload::expand_onload(attr.into(), item.into()).into()
}

fn register_name(ident: &str) -> String {
    let mut out = String::new();
    for (i, c) in ident.chars().enumerate() {
        if c.is_uppercase() {
            if i > 0 {
                out.push('_');
            }
            out.extend(c.to_lowercase());
        } else if c == '-' {
            out.push('_');
        } else {
            out.push(c);
        }
    }
    out
}

fn kebab(s: &str) -> String {
    register_name(s).replace('_', "-")
}

/// An argument is the SNI environment if its type is a reference whose last
/// path segment is `Env` (`&mut Env`, `&capy_sni::Env<'_>`, ...).
fn is_env_ty(ty: &Type) -> bool {
    let Type::Reference(r) = ty else {
        return false;
    };
    match &*r.elem {
        Type::Path(p) => p.path.segments.last().is_some_and(|s| s.ident == "Env"),
        _ => false,
    }
}

/// A return type is Result-like if its last path segment is `Result`
/// (`Result`, `std::result::Result`, `anyhow::Result`, ...).
fn is_result_ty(ty: &Type) -> bool {
    match ty {
        Type::Path(p) => p.path.segments.last().is_some_and(|s| s.ident == "Result"),
        _ => false,
    }
}

struct Attrs {
    name: Option<String>,
    module: Option<String>,
    modes: Vec<String>,
    supers: Vec<String>,
}

fn parse_attrs(attr: TokenStream) -> Attrs {
    let mut out = Attrs {
        name: None,
        module: None,
        modes: Vec::new(),
        supers: Vec::new(),
    };
    // Custom parser instead of Punctuated<Meta, Comma>: `Meta` rejects
    // keyword idents, so a mode like `abstract` would fail the whole parse
    // and silently drop every attr.
    let parser = |input: syn::parse::ParseStream| -> syn::Result<()> {
        while !input.is_empty() {
            let key: syn::Ident = input.call(syn::ext::IdentExt::parse_any)?;
            if input.peek(syn::Token![=]) {
                input.parse::<syn::Token![=]>()?;
                let value: syn::LitStr = input.parse()?;
                match key.to_string().as_str() {
                    "name" => out.name = Some(value.value()),
                    "module" => out.module = Some(value.value()),
                    "supers" => {
                        out.supers = value
                            .value()
                            .split(',')
                            .map(|x| x.trim().to_string())
                            .filter(|x| !x.is_empty())
                            .collect();
                    }
                    _ => {}
                }
            } else {
                out.modes.push(key.to_string());
            }
            if input.peek(Comma) {
                input.parse::<Comma>()?;
            }
        }
        Ok(())
    };
    let _ = syn::parse::Parser::parse2(parser, attr.into());
    out
}

/// Attribute macro for `capy-sni` embedders.
#[proc_macro_attribute]
pub fn scheme(attr: TokenStream, item: TokenStream) -> TokenStream {
    let attrs = parse_attrs(attr);

    if let Ok(fun) = syn::parse::<ItemFn>(item.clone()) {
        return handle_fn(attrs, fun);
    }
    if let Ok(st) = syn::parse::<ItemStruct>(item.clone()) {
        return handle_struct(attrs, st);
    }
    if let Ok(tr) = syn::parse::<ItemTrait>(item.clone()) {
        return handle_trait(attrs, tr);
    }
    if let Ok(im) = syn::parse::<ItemImpl>(item.clone()) {
        return handle_impl(attrs, im);
    }

    syn::Error::new(
        proc_macro2::Span::call_site(),
        "#[scheme] supports fn, struct, trait, and impl",
    )
    .to_compile_error()
    .into()
}

/// Arg unpack shared by fn and method trampolines: `from_scm_args` on
/// `args_expr`, assertion violation + `Ref::null()` on mismatch.
fn unpack_args(
    scm_name: &str,
    arity_ty: &proc_macro2::TokenStream,
    arg_pats: &[syn::Pat],
    args_expr: proc_macro2::TokenStream,
) -> proc_macro2::TokenStream {
    if arg_pats.is_empty() {
        return quote! {};
    }
    quote! {
        let (#(#arg_pats,)*): #arity_ty = match <#arity_ty as ::capy_sni::FromScmArgs>::from_scm_args(__env, #args_expr) {
            Ok(v) => v,
            Err(e) => {
                __env.assertion_violation(#scm_name, &e.to_string(), &[]);
                return ::capy_sni::Ref::null();
            }
        };
    }
}

/// Return conversion shared by fn and method trampolines: `IntoScm`, with
/// Result-returning callees raising on `Err`.
fn return_conv(
    ret_is_result: bool,
    ret_is_unit: bool,
    call: proc_macro2::TokenStream,
) -> proc_macro2::TokenStream {
    if ret_is_result {
        quote! {
            match #call {
                Ok(v) => ::capy_sni::IntoScm::into_scm(v, __env),
                Err(e) => {
                    let r = ::capy_sni::IntoScm::into_scm(e, __env);
                    let _ = __env.raise(r);
                    ::capy_sni::Ref::null()
                }
            }
        }
    } else if ret_is_unit {
        quote! {
            let _ = #call;
            ::capy_sni::IntoScm::into_scm((), __env)
        }
    } else {
        quote! {
            let __ret = #call;
            ::capy_sni::IntoScm::into_scm(__ret, __env)
        }
    }
}

struct SplitInputs {
    has_env: bool,
    arg_tys: Vec<Type>,
    arg_pats: Vec<syn::Pat>,
}

/// Split typed fn inputs into the env flag and the Scheme-visible arg
/// types/patterns. Returns a compile error on a bare receiver.
fn split_inputs<'a>(
    inputs: impl IntoIterator<Item = &'a FnArg>,
) -> Result<SplitInputs, TokenStream> {
    let mut has_env = false;
    let mut arg_tys = Vec::new();
    let mut arg_pats = Vec::new();
    for input in inputs {
        match input {
            FnArg::Typed(pt) => {
                if is_env_ty(&pt.ty) {
                    has_env = true;
                    continue;
                }
                arg_tys.push((*pt.ty).clone());
                arg_pats.push((*pt.pat).clone());
            }
            FnArg::Receiver(r) => {
                return Err(syn::Error::new_spanned(r, "use impl block for methods")
                    .to_compile_error()
                    .into());
            }
        }
    }
    Ok(SplitInputs {
        has_env,
        arg_tys,
        arg_pats,
    })
}

fn handle_fn(attrs: Attrs, mut fun: ItemFn) -> TokenStream {
    let is_cps = attrs.modes.iter().any(|m| m == "cps");
    let is_unsafe_nest = attrs.modes.iter().any(|m| m == "unsafe");
    if is_cps && is_unsafe_nest {
        return syn::Error::new_spanned(
            &fun.sig.ident,
            "#[scheme] cannot combine `cps` and `unsafe` modes",
        )
        .to_compile_error()
        .into();
    }
    for m in &attrs.modes {
        if !matches!(m.as_str(), "cps" | "unsafe") {
            // modes on fn are only cps/unsafe; ignore unknown for forward compat
            // (struct modes are validated elsewhere)
            if matches!(m.as_str(), "pod" | "record" | "class" | "abstract") {
                return syn::Error::new_spanned(
                    &fun.sig.ident,
                    format!("#[scheme] mode `{m}` is not valid on fn"),
                )
                .to_compile_error()
                .into();
            }
        }
    }

    if is_cps {
        // Rewrite body onto __cps::run; requires `&mut Env` as first arg.
        let SplitInputs { has_env, .. } = match split_inputs(&fun.sig.inputs) {
            Ok(v) => v,
            Err(e) => return e,
        };
        if !has_env {
            return syn::Error::new_spanned(
                &fun.sig,
                "#[scheme(cps)] requires `&mut Env` as the first argument",
            )
            .to_compile_error()
            .into();
        }
        let expanded = cps::expand_cps(proc_macro2::TokenStream::new(), quote! { #fun });
        fun = match syn::parse2(expanded) {
            Ok(f) => f,
            Err(e) => return e.to_compile_error().into(),
        };
    }

    let ident = &fun.sig.ident;
    let scm_name = attrs
        .name
        .clone()
        .unwrap_or_else(|| kebab(&ident.to_string()));
    let module = attrs.module.clone().unwrap_or_else(|| "capy".to_string());
    let register = format_ident!("scheme_register_{}", register_name(&ident.to_string()));
    let trampoline = format_ident!("_sni_tramp_{}", ident);

    let SplitInputs {
        has_env,
        arg_tys,
        arg_pats,
    } = match split_inputs(&fun.sig.inputs) {
        Ok(v) => v,
        Err(e) => return e,
    };

    let leaf_doc = if is_cps {
        quote! {
            #[doc = " SNI CPS native: may nest into Scheme via `cps::call` / `raise` / `guard`."]
        }
    } else if is_unsafe_nest {
        quote! {
            #[doc = " SNI unsafe native: may call `env.call_n` (nests). Prefer `#[scheme(cps)]`."]
        }
    } else {
        quote! {
            #[doc = " SNI leaf native: must not call `env.call_n` or `cps::call` (no nest)."]
        }
    };

    let call_args = if has_env {
        quote! { __env, #(#arg_pats),* }
    } else {
        quote! { #(#arg_pats),* }
    };

    let arity_ty = quote! { (#(#arg_tys,)*) };
    let unpack = unpack_args(&scm_name, &arity_ty, &arg_pats, quote! { __args });

    let ret_is_result = match &fun.sig.output {
        ReturnType::Type(_, ty) => is_result_ty(ty),
        ReturnType::Default => false,
    };
    let body_ret = return_conv(
        ret_is_result,
        matches!(fun.sig.output, ReturnType::Default),
        quote! { #ident(#call_args) },
    );

    let expanded = quote! {
        #leaf_doc
        #fun

        unsafe extern "C" fn #trampoline(
            __env_ptr: *mut ::capy_sni_sys::SniEnv,
            __argc: i32,
            __argv: *const ::capy_sni::ScmRef,
        ) -> ::capy_sni::ScmRef {
            unsafe {
                ::capy_sni::__scheme_trampoline(__env_ptr, __argc, __argv, |__env, __args| {
                    let __arity = <#arity_ty as ::capy_sni::FromScmArgs>::ARITY;
                    if !__arity.is_valid(__args.len()) {
                        __env.assertion_violation(#scm_name, "wrong number of arguments", &[]);
                        return ::capy_sni::Ref::null();
                    }
                    #unpack
                    #body_ret
                })
            }
        }

        pub fn #register(env: &mut ::capy_sni::Env) {
            let proc = env.new_native_procedure(#trampoline);
            if env.define(#module, #scm_name, proc).is_err() {
                panic!(
                    "failed to define procedure `{}` in module `{}`",
                    #scm_name, #module
                );
            }
        }
    };
    expanded.into()
}

fn handle_struct(attrs: Attrs, st: ItemStruct) -> TokenStream {
    const STRUCT_MODES: [&str; 3] = ["pod", "record", "class"];
    if let Some(bad) = attrs
        .modes
        .iter()
        .find(|m| !STRUCT_MODES.contains(&m.as_str()))
    {
        return syn::Error::new_spanned(
            &st,
            format!(
                "#[scheme] on struct: unknown mode `{bad}`; valid modes are `pod`, `record`, `class`"
            ),
        )
        .to_compile_error()
        .into();
    }

    let ident = &st.ident;
    let scm_name = attrs
        .name
        .clone()
        .unwrap_or_else(|| kebab(&ident.to_string()));
    let module = attrs.module.clone().unwrap_or_else(|| "capy".to_string());
    let register = format_ident!("scheme_register_{}", register_name(&ident.to_string()));

    let is_pod = attrs.modes.iter().any(|m| m == "pod");
    let is_record = attrs.modes.iter().any(|m| m == "record");
    let is_class = attrs.modes.iter().any(|m| m == "class");

    let field_name_strs: Vec<String> = st
        .fields
        .iter()
        .filter_map(|f| f.ident.as_ref().map(|i| kebab(&i.to_string())))
        .collect();

    if is_pod {
        let expanded = quote! {
            #st

            impl ::capy_sni::PodRecord for #ident {
                const TYPE_NAME: &'static str = #scm_name;
            }

            impl<'env> ::capy_sni::TryFromScm<'env> for #ident {
                fn try_from_scm(
                    env: &mut ::capy_sni::Env<'env>,
                    value: ::capy_sni::Ref<'env>,
                ) -> Result<Self, ::capy_sni::ConversionError> {
                    let view = <Self as ::capy_sni::PodRecord>::from_scm(env, value)
                        .ok_or(::capy_sni::ConversionError::type_mismatch(0, #scm_name))?;
                    Ok(*view)
                }
            }

            impl<'env> ::capy_sni::IntoScm<'env> for #ident {
                fn into_scm(self, env: &mut ::capy_sni::Env<'env>) -> ::capy_sni::Ref<'env> {
                    <Self as ::capy_sni::PodRecord>::new_scm(env, self)
                }
            }

            pub fn #register(env: &mut ::capy_sni::Env) {
                let pod_type = match env.register_pod_type(
                    #scm_name,
                    ::std::mem::size_of::<#ident>(),
                    ::std::mem::align_of::<#ident>(),
                    None,
                ) {
                    Ok(t) => t,
                    Err(_) => panic!("failed to register pod type `{}`", #scm_name),
                };
                if env.define(#module, #scm_name, pod_type).is_err() {
                    panic!(
                        "failed to define pod type `{}` in module `{}`",
                        #scm_name, #module
                    );
                }
            }

            impl #ident {
                pub fn scheme_class<'env>(env: &mut ::capy_sni::Env<'env>) -> ::capy_sni::Ref<'env> {
                    env.public_ref(#module, #scm_name)
                        .unwrap_or(::capy_sni::Ref::null())
                }
            }
        };
        return expanded.into();
    }

    if is_record {
        let fields_lit = &field_name_strs;
        let expanded = quote! {
            #st

            pub fn #register(env: &mut ::capy_sni::Env) {
                let name = env
                    .intern_symbol(#scm_name)
                    .unwrap_or_else(|_| panic!("failed to intern symbol `{}`", #scm_name));
                let fields = env
                    .mutable_field_vector(&[#(#fields_lit),*])
                    .unwrap_or_else(|_| panic!("failed to build field vector for `{}`", #scm_name));
                let rtd = env.make_rtd(
                    name,
                    ::capy_sni::Ref::null(),
                    ::capy_sni::Ref::null(),
                    false,
                    false,
                    fields,
                );
                let rcd = env.make_rcd(rtd, ::capy_sni::Ref::null(), ::capy_sni::Ref::null());
                let ctor = env.record_constructor(rcd);
                if env.define(#module, #scm_name, rtd).is_err() {
                    panic!(
                        "failed to define record type `{}` in module `{}`",
                        #scm_name, #module
                    );
                }
                if env.define(#module, concat!("make-", #scm_name), ctor).is_err() {
                    panic!(
                        "failed to define constructor `make-{}` in module `{}`",
                        #scm_name, #module
                    );
                }
            }
        };
        return expanded.into();
    }

    if is_class {
        let fields_lit = &field_name_strs;
        let supers = &attrs.supers;
        let supers_q = if supers.is_empty() {
            quote! {
                let obj = env
                    .builtin_class("<object>")
                    .unwrap_or_else(|_| panic!("failed to resolve builtin class `<object>`"));
                env.list(&[obj])
            }
        } else {
            quote! {
                let mut supers_v = Vec::new();
                #(
                    {
                        let n = #supers;
                        let c = if n.starts_with('<') {
                            env.builtin_class(n)
                                .unwrap_or_else(|_| panic!("failed to resolve builtin class `{}`", n))
                        } else {
                            env.public_ref(#module, n).unwrap_or(::capy_sni::Ref::null())
                        };
                        supers_v.push(c);
                    }
                )*
                env.list(&supers_v)
            }
        };
        let expanded = quote! {
            #st

            pub fn #register(env: &mut ::capy_sni::Env) {
                let name = env
                    .intern_symbol(#scm_name)
                    .unwrap_or_else(|_| panic!("failed to intern symbol `{}`", #scm_name));
                let mut slot_refs = Vec::new();
                #(
                    slot_refs.push(
                        env.intern_symbol(#fields_lit)
                            .unwrap_or_else(|_| panic!("failed to intern slot `{}`", #fields_lit)),
                    );
                )*
                let slots = env.list(&slot_refs);
                let supers = { #supers_q };
                let cls = env.make_class(name, slots, supers);
                if env.define(#module, #scm_name, cls).is_err() {
                    panic!(
                        "failed to define class `{}` in module `{}`",
                        #scm_name, #module
                    );
                }
            }

            impl #ident {
                pub fn scheme_class<'env>(env: &mut ::capy_sni::Env<'env>) -> ::capy_sni::Ref<'env> {
                    env.public_ref(#module, #scm_name)
                        .unwrap_or(::capy_sni::Ref::null())
                }
            }
        };
        return expanded.into();
    }

    syn::Error::new_spanned(
        &st,
        "#[scheme] on struct requires pod, record, or class mode",
    )
    .to_compile_error()
    .into()
}

fn handle_trait(attrs: Attrs, tr: ItemTrait) -> TokenStream {
    if let Some(bad) = attrs.modes.iter().find(|m| m.as_str() != "abstract") {
        return syn::Error::new_spanned(
            &tr,
            format!("#[scheme] on trait: unknown mode `{bad}`; the only valid mode is `abstract`"),
        )
        .to_compile_error()
        .into();
    }

    let ident = &tr.ident;
    let scm_name = attrs.name.unwrap_or_else(|| kebab(&ident.to_string()));
    let module = attrs.module.unwrap_or_else(|| "capy".to_string());
    let register = format_ident!("scheme_register_{}", register_name(&ident.to_string()));

    let method_regs: Vec<_> = tr
        .items
        .iter()
        .filter_map(|it| {
            if let syn::TraitItem::Fn(m) = it {
                let mid = &m.sig.ident;
                let mname = kebab(&mid.to_string());
                Some(quote! {
                    {
                        let gname = env
                            .intern_symbol(#mname)
                            .unwrap_or_else(|_| panic!("failed to intern symbol `{}`", #mname));
                        let g = env.make_generic(gname, 1);
                        if env.define(#module, #mname, g).is_err() {
                            panic!(
                                "failed to define generic `{}` in module `{}`",
                                #mname, #module
                            );
                        }
                    }
                })
            } else {
                None
            }
        })
        .collect();

    let expanded = quote! {
        #tr

        pub fn #register(env: &mut ::capy_sni::Env) {
            let name = env
                .intern_symbol(#scm_name)
                .unwrap_or_else(|_| panic!("failed to intern symbol `{}`", #scm_name));
            let supers = {
                let obj = env
                    .builtin_class("<object>")
                    .unwrap_or_else(|_| panic!("failed to resolve builtin class `<object>`"));
                env.list(&[obj])
            };
            let cls = env.make_abstract_class(name, supers);
            if env.define(#module, #scm_name, cls).is_err() {
                panic!(
                    "failed to define abstract class `{}` in module `{}`",
                    #scm_name, #module
                );
            }
            #(#method_regs)*
        }
    };
    expanded.into()
}

fn handle_impl(attrs: Attrs, im: ItemImpl) -> TokenStream {
    let self_ty = &im.self_ty;
    let trait_path = match &im.trait_ {
        Some((_, path, _)) => path,
        None => {
            return syn::Error::new_spanned(&im, "#[scheme] impl requires a trait")
                .to_compile_error()
                .into();
        }
    };
    let module = attrs.module.unwrap_or_else(|| "capy".to_string());
    let trait_ident = trait_path
        .segments
        .last()
        .map(|s| s.ident.clone())
        .unwrap_or_else(|| format_ident!("Unknown"));
    let ty_name = quote!(#self_ty)
        .to_string()
        .chars()
        .filter(|c| c.is_alphanumeric() || *c == '_')
        .collect::<String>();
    let register = format_ident!(
        "scheme_register_{}_for_{}",
        kebab(&trait_ident.to_string()).replace('-', "_"),
        kebab(&ty_name).replace('-', "_")
    );

    let mut method_blocks: Vec<proc_macro2::TokenStream> = Vec::new();
    for it in &im.items {
        let syn::ImplItem::Fn(m) = it else {
            continue;
        };
        let mid = &m.sig.ident;
        let mname = kebab(&mid.to_string());
        let tramp = format_ident!(
            "_sni_method_{}_{}_for_{}",
            kebab(&trait_ident.to_string()).replace('-', "_"),
            kebab(&mid.to_string()).replace('-', "_"),
            kebab(&ty_name).replace('-', "_")
        );

        let recv_mut = match m.sig.inputs.first() {
            Some(FnArg::Receiver(r)) if r.reference.is_some() => r.mutability.is_some(),
            _ => {
                method_blocks.push(
                    syn::Error::new_spanned(
                        &m.sig,
                        "#[scheme] trait methods must take `&self` or `&mut self`",
                    )
                    .to_compile_error(),
                );
                continue;
            }
        };

        let SplitInputs {
            has_env,
            arg_tys,
            arg_pats,
        } = match split_inputs(m.sig.inputs.iter().skip(1)) {
            Ok(v) => v,
            Err(e) => return e,
        };

        let call_args = if has_env {
            quote! { __env, #(#arg_pats),* }
        } else {
            quote! { #(#arg_pats),* }
        };
        let arity_ty = quote! { (#(#arg_tys,)*) };
        let unpack = unpack_args(&mname, &arity_ty, &arg_pats, quote! { &__args[2..] });
        let required_argc = 1 + arg_tys.len() as i32;

        let get_receiver = if recv_mut {
            quote! {
                let __this = match <#self_ty as ::capy_sni::PodRecord>::from_scm_mut(__env, __args[1]) {
                    Some(r) => r,
                    None => {
                        __env.assertion_violation(#mname, "receiver type mismatch", &[]);
                        return ::capy_sni::Ref::null();
                    }
                };
            }
        } else {
            quote! {
                let __this = match <#self_ty as ::capy_sni::PodRecord>::from_scm(__env, __args[1]) {
                    Some(r) => r,
                    None => {
                        __env.assertion_violation(#mname, "receiver type mismatch", &[]);
                        return ::capy_sni::Ref::null();
                    }
                };
            }
        };

        let ret_is_result = match &m.sig.output {
            ReturnType::Type(_, ty) => is_result_ty(ty),
            ReturnType::Default => false,
        };
        let body_ret = return_conv(
            ret_is_result,
            matches!(m.sig.output, ReturnType::Default),
            quote! { <#self_ty as #trait_path>::#mid(__this, #call_args) },
        );

        method_blocks.push(quote! {
            unsafe extern "C" fn #tramp(
                __env_ptr: *mut ::capy_sni_sys::SniEnv,
                __argc: i32,
                __argv: *const ::capy_sni::ScmRef,
            ) -> ::capy_sni::ScmRef {
                unsafe {
                    ::capy_sni::__scheme_trampoline(__env_ptr, __argc, __argv, |__env, __args| {
                        // Generic dispatch prepends the next-method descriptor:
                        // __args is [next, receiver, method-args...], matching
                        // the `lambda* (next arg ...)` convention of define-method.
                        if __args.len() < 2 {
                            __env.assertion_violation(#mname, "missing receiver argument", &[]);
                            return ::capy_sni::Ref::null();
                        }
                        let __arity = <#arity_ty as ::capy_sni::FromScmArgs>::ARITY;
                        if !__arity.is_valid(__args.len() - 2) {
                            __env.assertion_violation(#mname, "wrong number of arguments", &[]);
                            return ::capy_sni::Ref::null();
                        }
                        #unpack
                        // Take the receiver view last: it borrows the env until
                        // the method call, so all `&mut Env` work (arg unpack)
                        // must happen before it.
                        #get_receiver
                        #body_ret
                    })
                }
            }
            {
                let body = env.new_native_procedure(#tramp);
                let generic = match env.public_ref(#module, #mname) {
                    Some(g) => g,
                    None => panic!(
                        "failed to register method `{}`: generic not found in module `{}` (register the trait first)",
                        #mname, #module
                    ),
                };
                // Pod values are bytevectors, so the method specializes on
                // <bytevector>; the trampoline's PodRecord::from_scm performs
                // the actual pod type check on the receiver.
                let cls = env.builtin_class("<bytevector>").unwrap_or_else(|_| {
                    panic!(
                        "failed to register method `{}`: builtin class <bytevector> not found",
                        #mname
                    )
                });
                let specs = env.list(&[cls]);
                if env.add_method(generic, specs, #required_argc, body, false).is_err() {
                    panic!("failed to add method `{}` to its generic", #mname);
                }
            }
        });
    }

    let expanded = quote! {
        #im

        pub fn #register(env: &mut ::capy_sni::Env) {
            #(#method_blocks)*
        }
    };
    expanded.into()
}
