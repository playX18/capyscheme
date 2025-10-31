use quote::ToTokens;
use syn::{Meta, punctuated::Punctuated};

/// A procedural macro to convert Rust items into Scheme compatible items.
///
/// When applied to a function, it generates a trampoline function that can be
/// called from the Scheme runtime, handling argument conversion and return value
/// conversion.
///
/// When applied to constants it generates code to register the constant in the Scheme
/// environment.
///
/// When applied on a module it processes all items within the module accordingly.
///
/// When applied on struct or enum, it generates code to create corresponding Scheme
/// record type and set of constructors and accessors.
#[proc_macro_attribute]
pub fn scheme(
    input: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input!(input as Input);
    let item = syn::parse_macro_input!(item as syn::Item);

    match item {
        syn::Item::Fn(fun) => fun::handle(input, fun)
            .map(|mut x| x.to_tokens())
            .unwrap_or_else(|e| e.to_compile_error())
            .into(),
        syn::Item::Static(static_) => var::handle(input, static_)
            .map(|x| x.to_tokens())
            .unwrap_or_else(|e| e.to_compile_error())
            .into(),
        syn::Item::Mod(module) => module::handle(input, module)
            .map(|mut x| x.to_tokens())
            .unwrap_or_else(|e| e.to_compile_error())
            .into(),

        _ => panic!("#[scheme] can only be applied to functions"),
    }
}

mod fun;
mod module;
mod structure;
mod var;

pub(crate) struct Input {
    pub meta: Punctuated<Meta, syn::token::Comma>,
}

impl syn::parse::Parse for Input {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let meta = Punctuated::parse_terminated(input)?;
        Ok(Input { meta })
    }
}

#[derive(Clone, PartialEq, Eq)]
pub(crate) enum ModulePath {
    CurrentModule,
    Concrete(syn::Path),
}

impl ModulePath {
    /// Given a path to module, produces code to resolve it.
    pub fn resolve(&self) -> proc_macro2::TokenStream {
        match self {
            ModulePath::CurrentModule => quote::quote! {
                current_module(ctx).get(ctx).downcast::<Module>()
            },

            ModulePath::Concrete(path) => {
                let path = path
                    .segments
                    .iter()
                    .map(|seg| seg.ident.to_string())
                    .collect::<Vec<_>>()
                    .join(" ");

                quote::quote! {
                    ctx.ensure_module(#path)
                }
            }
        }
    }
}

pub(crate) fn get_module_path(
    meta: &Punctuated<Meta, syn::token::Comma>,
) -> syn::Result<ModulePath> {
    let mut module = None;
    for meta in meta {
        match meta {
            syn::Meta::NameValue(kv) if kv.path.is_ident("module") => {
                if let syn::Expr::Path(ref expr_path) = kv.value {
                    if module.is_some() {
                        return Err(syn::Error::new_spanned(
                            kv.clone(),
                            "Duplicate 'module_path' attribute for #[scheme]",
                        ));
                    }
                    module = Some(ModulePath::Concrete(expr_path.path.clone()));
                } else {
                    return Err(syn::Error::new_spanned(
                        kv.clone(),
                        "Expected path for 'module' attribute",
                    ));
                }
            }

            _ => continue,
        }
    }

    Ok(module.unwrap_or(ModulePath::CurrentModule))
}

pub(crate) fn get_name(
    key: Option<&str>,
    meta: &Punctuated<Meta, syn::token::Comma>,
) -> syn::Result<Option<syn::LitStr>> {
    let mut name = None;
    let key = key.unwrap_or("name");
    for meta in meta {
        match meta {
            syn::Meta::NameValue(nv) if nv.path.is_ident(key) => {
                if let syn::Expr::Lit(lit) = &nv.value
                    && let syn::Lit::Str(litstr) = &lit.lit
                {
                    if name.is_some() {
                        return Err(syn::Error::new_spanned(
                            nv,
                            "Duplicate 'name' attribute for #[scheme]",
                        ));
                    }
                    name = Some(litstr.clone());
                } else {
                    return Err(syn::Error::new_spanned(
                        nv,
                        "Expected string literal for 'name'",
                    ));
                }
            }
            _ => {}
        }
    }

    Ok(name)
}

pub(crate) fn get_ident(
    key: Option<&str>,
    meta: &Punctuated<Meta, syn::token::Comma>,
) -> syn::Result<Option<syn::Ident>> {
    let mut ident = None;
    let key = key.unwrap_or("name");
    for meta in meta {
        match meta {
            syn::Meta::NameValue(nv) if nv.path.is_ident(key) => {
                if let syn::Expr::Path(expr_path) = &nv.value {
                    if expr_path.path.segments.len() != 1 {
                        return Err(syn::Error::new_spanned(
                            nv,
                            "Expected single identifier for 'name'",
                        ));
                    }
                    let id = expr_path.path.segments.first().unwrap().ident.clone();
                    if ident.is_some() {
                        return Err(syn::Error::new_spanned(
                            nv,
                            "Duplicate 'name' attribute for #[scheme]",
                        ));
                    }
                    ident = Some(id);
                } else {
                    return Err(syn::Error::new_spanned(
                        nv,
                        "Expected identifier for 'name'",
                    ));
                }
            }
            _ => {}
        }
    }

    Ok(ident)
}

pub(crate) fn get_path(
    key: Option<&str>,
    meta: &Punctuated<Meta, syn::token::Comma>,
) -> syn::Result<Option<syn::Path>> {
    let mut path = None;
    let key = key.unwrap_or("path");
    for meta in meta {
        match meta {
            syn::Meta::NameValue(nv) if nv.path.is_ident(key) => {
                if let syn::Expr::Path(expr_path) = &nv.value {
                    if path.is_some() {
                        return Err(syn::Error::new_spanned(
                            nv,
                            "Duplicate 'path' attribute for #[scheme]",
                        ));
                    }
                    path = Some(expr_path.path.clone());
                } else {
                    return Err(syn::Error::new_spanned(nv, "Expected path for 'path'"));
                }
            }
            _ => {}
        }
    }

    Ok(path)
}

impl std::fmt::Debug for ModulePath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ModulePath::CurrentModule => write!(f, "(current-module)"),
            ModulePath::Concrete(path) => write!(f, "{}", path.to_token_stream()),
        }
    }
}
