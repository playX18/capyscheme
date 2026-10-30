//! Function-related attribute handling.
//!
//! Handles `#[scheme]` and `#[continuation]` attributes on `fn` items.
//!

use quote::{ToTokens, quote, quote_spanned};
use syn::spanned::Spanned;

use crate::{Input, ModulePath};

pub fn handle(Input { meta: args }: Input, fun: syn::ItemFn) -> syn::Result<FunctionDefinition> {
    let mut name = None;
    let mut module = None;
    let mut is_continuation = false;
    let mut is_unsafe = false;

    for meta in args {
        match meta {
            syn::Meta::NameValue(kv) if kv.path.is_ident("name") => {
                if let syn::Expr::Lit(ref lit) = kv.value
                    && let syn::Lit::Str(ref lit_str) = lit.lit
                {
                    if name.is_some() {
                        return Err(syn::Error::new_spanned(
                            kv.clone(),
                            "Duplicate 'name' attribute for #[scheme]",
                        ));
                    }
                    name = Some(lit_str.clone());
                } else {
                    return Err(syn::Error::new_spanned(
                        kv.clone(),
                        "Expected string literal for 'name' attribute",
                    ));
                }
            }

            syn::Meta::Path(path) if path.is_ident("continuation") => {
                is_continuation = true;
            }

            syn::Meta::Path(path) if path.is_ident("unsafe") => {
                is_unsafe = true;
            }

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

            _ => {
                return Err(syn::Error::new_spanned(
                    meta.clone(),
                    format!(
                        "Unknown attribute for #[scheme]: {}",
                        meta.to_token_stream()
                    ),
                ));
            }
        }
    }

    let module = module.unwrap_or(ModulePath::Capy);
    let name = name.unwrap_or_else(|| {
        let ident_str = fun.sig.ident.to_string();
        let ident_str = ident_str.replace('_', "-");
        syn::LitStr::new(&ident_str, fun.sig.ident.span())
    });

    let gc_lifetime = syn::Lifetime::new("'gc", proc_macro2::Span::call_site());
    let scheme_attribute = SchemeAttribute {
        module_path: module,
        name,
        is_unsafe,
    };

    let call = syn::Ident::new("call", proc_macro2::Span::call_site());
    let mut function_definition = FunctionDefinition {
        is_continuation,
        call,
        gc_lifetime,
        scheme_attribute,
        transformed_function: fun,
    };

    function_definition.build();

    Ok(function_definition)
}

#[allow(dead_code)]
pub struct SchemeAttribute {
    /// Path in a Scheme module where this function should be defined.
    ///
    /// When `Capy`, the function is defined in the same module
    /// that loaded the Rust code.
    pub module_path: ModulePath,
    /// Name under which this function should be defined in Scheme.
    ///
    /// If not provided, kebab-case version of the Rust function name is used.
    pub name: syn::LitStr,
    /// When true (`#[scheme(unsafe)]`), the body may nest into Scheme via
    /// [`crate::runtime::sni`] or unsafe [`call_scheme`]. Safe leaves must not
    /// increase nest level or trigger GC.
    pub is_unsafe: bool,
}
/// A result of processing a function with `#[scheme]` or `#[continuation]`.
///
///
pub struct FunctionDefinition {
    is_continuation: bool,
    pub call: syn::Ident,
    pub gc_lifetime: syn::Lifetime,
    pub scheme_attribute: SchemeAttribute,
    pub transformed_function: syn::ItemFn,
}

impl FunctionDefinition {
    pub fn is_continuation(&self) -> bool {
        self.is_continuation
    }

    pub fn build(&mut self) {
        // The body's declared output is kept as-is: `Call::run` converts it.
        // Only the `'gc` lifetime is injected for `Value<'gc>` argument types.
        let lifetime = &self.gc_lifetime;
        self.transformed_function
            .sig
            .generics
            .params
            .push(syn::parse2(quote! { #lifetime }).expect("parsing generic parameter"));
    }

    fn trampoline_body(&self) -> proc_macro2::TokenStream {
        let gc_lifetime = &self.gc_lifetime;
        let arg_tys = self.transformed_function.sig.inputs.iter().map(|arg| {
            if let syn::FnArg::Typed(pat_type) = arg {
                pat_type.ty.clone()
            } else {
                panic!("#[scheme] does not support method receivers (self)")
            }
        });
        let arg_pats = self.transformed_function.sig.inputs.iter().map(|arg| {
            if let syn::FnArg::Typed(pat_type) = arg {
                pat_type.pat.clone()
            } else {
                panic!("#[scheme] does not support method receivers (self)")
            }
        });
        let scm_name = &self.scheme_attribute.name;
        let real_name = &self.transformed_function.sig.ident;
        let arg_tys1 = arg_tys.clone();
        let arg_tys2 = arg_tys.clone();
        let arg_tys3 = arg_tys;
        let arg_pats1 = arg_pats.clone();
        let arg_pats2 = arg_pats;
        let body = quote_spanned! {
            self.transformed_function.span() =>
            {
                Call::invoke(ctx, sfp, |call| {
                    type Args<#gc_lifetime> = (#(#arg_tys1),*);

                    let arity = <Args<#gc_lifetime> as FromValues>::ARITY;

                    if !arity.is_valid(call.args.len()) {
                        call.wrong_number_of_arguments_violation(
                            #scm_name,
                            arity.min,
                            arity.max,
                            call.args.len(),
                            call.args
                        );
                        return call.finish_flagged();
                    }

                    let (#(#arg_pats1),*): (#(#arg_tys2),*) = match <(#(#arg_tys3),*) as FromValues>::from_values(call.ctx, &mut 0, call.args) {
                        Ok(args) => args,
                        Err(err) => {
                            call.conversion_error(#scm_name, err);
                            return call.finish_flagged();
                        }
                    };

                    call.run(|call| #real_name(call, #(#arg_pats2),*))
                })
            }
        };

        body
    }

    fn make_trampoline(&mut self) -> syn::ItemFn {
        let body = self.trampoline_body();
        let tokens = if self.is_continuation() {
            let raw_name = syn::Ident::new(
                &format!("_raw_scm_cont_{}", self.transformed_function.sig.ident),
                self.transformed_function.sig.ident.span(),
            );

            quote_spanned! {
                self.transformed_function.span() =>
                #[allow(unused_parens, unused_mut)]
                extern "C" fn #raw_name<'gc>(
                    ctx: MutationContext<'gc>,
                    sfp: Slot<'gc>,
                ) -> ReturnCode
                {
                    #body
                }
            }
        } else {
            let raw_name = syn::Ident::new(
                &format!("_raw_scm_proc_{}", self.transformed_function.sig.ident),
                self.transformed_function.sig.ident.span(),
            );

            quote_spanned! {
                self.transformed_function.span() =>
                #[allow(unused_parens, unused_mut)]
                extern "C" fn #raw_name<'gc>(
                    ctx: MutationContext<'gc>,
                    sfp: Slot<'gc>,
                ) -> ReturnCode
                {
                    #body
                }
            }
        };

        syn::parse2(tokens).expect("parsing trampoline function")
    }

    pub fn raw_name(&self) -> syn::Ident {
        if self.is_continuation() {
            syn::Ident::new(
                &format!("_raw_scm_cont_{}", self.transformed_function.sig.ident),
                self.transformed_function.sig.ident.span(),
            )
        } else {
            syn::Ident::new(
                &format!("_raw_scm_proc_{}", self.transformed_function.sig.ident),
                self.transformed_function.sig.ident.span(),
            )
        }
    }

    pub fn meta_name(&self) -> syn::Ident {
        syn::Ident::new(
            &format!(
                "_SCHEME_META_{}",
                self.transformed_function
                    .sig
                    .ident
                    .to_string()
                    .to_uppercase()
            ),
            self.transformed_function.sig.ident.span(),
        )
    }

    pub fn get_meta_name(&self) -> syn::Ident {
        syn::Ident::new(
            &format!(
                "_scheme_meta_{}",
                self.transformed_function
                    .sig
                    .ident
                    .to_string()
                    .to_lowercase()
            ),
            self.transformed_function.sig.ident.span(),
        )
    }

    pub fn emit_tokens(&mut self) -> proc_macro2::TokenStream {
        let trampoline = self.make_trampoline();
        let call = &self.call;
        let lifetime = &self.gc_lifetime;
        self.transformed_function.sig.inputs.insert(
            0,
            syn::parse2(quote! {
                #call: &Call<'_, #lifetime>
            })
            .expect("parsing function argument"),
        );
        let orig_body = &self.transformed_function.block;
        let ctx = syn::Ident::new("ctx", proc_macro2::Span::call_site());
        let leaf_doc = if self.scheme_attribute.is_unsafe {
            "\
# Safety (`#[scheme(unsafe)]`)

This leaf may increase nest level (re-enter Scheme). Keep live Values rooted via \
`capy::runtime::sni` across any nest. Prefer CPS `call.tail` when possible."
        } else {
            "\
# GC contract (`#[scheme]`)

Safe leaves must not increase nest level or trigger GC. Allocation may schedule \
collection but must not collect inline. To nest into Scheme, use \
`capy::runtime::sni::call_function` from a `#[scheme(unsafe)]` function, or stay \
in CPS with `call.tail`."
        };
        let new_body = quote! {{
            let #ctx = #call.ctx;
            #orig_body
        }};
        self.transformed_function.block = syn::parse2(new_body).expect("parsing new function body");
        self.transformed_function
            .attrs
            .push(syn::parse_quote!(#[doc = #leaf_doc]));
        let func = &self.transformed_function;
        let vis = &func.vis;

        let doc = func
            .attrs
            .iter()
            .filter_map(|attr| {
                if let syn::Meta::NameValue(kv) = attr.meta.clone()
                    && kv.path.is_ident("doc")
                {
                    if let syn::Expr::Lit(ref lit) = kv.value
                        && let syn::Lit::Str(ref lit_str) = lit.lit
                    {
                        Some(lit_str.value())
                    } else {
                        None
                    }
                } else {
                    None
                }
            })
            .collect::<Vec<_>>()
            .join("\n");

        let meta_static_name = syn::Ident::new(
            &format!("_SCHEME_META_{}", func.sig.ident.to_string().to_uppercase()),
            func.sig.ident.span(),
        );

        let get_meta_name = syn::Ident::new(
            &format!("_scheme_meta_{}", func.sig.ident.to_string().to_lowercase()),
            func.sig.ident.span(),
        );

        let make_closure_name = syn::Ident::new(
            &format!("make_closure_{}", func.sig.ident.to_string().to_lowercase()),
            func.sig.ident.span(),
        );

        let make_static_closure_name = syn::Ident::new(
            &format!(
                "make_static_closure_{}",
                func.sig.ident.to_string().to_lowercase()
            ),
            func.sig.ident.span(),
        );

        let typ_fn = if self.is_continuation {
            "continuation"
        } else {
            "function"
        };

        let make_closure_doc = format!(
            "Make a closure for the Scheme {typ_fn} `{name}` with provided free variables.",
            name = self.scheme_attribute.name.value()
        );

        let make_static_closure_doc = format!(
            "Make a static closure for the Scheme {typ_fn} `{name}` with provided free variables.",
            name = self.scheme_attribute.name.value()
        );

        let raw_name = if self.is_continuation() {
            syn::Ident::new(
                &format!("_raw_scm_cont_{}", func.sig.ident),
                func.sig.ident.span(),
            )
        } else {
            syn::Ident::new(
                &format!("_raw_scm_proc_{}", func.sig.ident),
                func.sig.ident.span(),
            )
        };

        let scm_name = &self.scheme_attribute.name;

        let register_name = syn::Ident::new(
            &format!("register_{}", func.sig.ident),
            func.sig.ident.span(),
        );

        let module = self.scheme_attribute.module_path.resolve();

        let (make, make_static) = if self.is_continuation() {
            (
                syn::Ident::new("make_native_continuation", func.sig.ident.span()),
                syn::Ident::new("make_static_continuation", func.sig.ident.span()),
            )
        } else {
            (
                syn::Ident::new("make_native_closure", func.sig.ident.span()),
                syn::Ident::new("make_static_closure", func.sig.ident.span()),
            )
        };

        quote_spanned! {
            func.span() =>

            global!(
                #meta_static_name <'gc> : Value<'gc> = (ctx) {
                    let loc = vector!(ctx,
                        ctx.str(file!()),
                        Value::new(line!() as i32),
                        Value::new(column!() as i32)
                    );

                    let props = list!(ctx,
                        Value::cons(ctx, ctx.intern("source"), loc.into()),
                        Value::cons(ctx, ctx.intern("documentation"), ctx.str(#doc))
                    );

                    props
                };
            );

            #[doc = #make_closure_doc]
            #vis fn #make_closure_name<'gc>(
                ctx: MutationContext<'gc>,
                free_vars: impl IntoIterator<Item = Value<'gc>>,
            ) -> ClosureRef<'gc>
            {
                ctx.#make(
                    #raw_name,
                    free_vars,
                    #get_meta_name(ctx),
                )
            }

            #[doc = #make_static_closure_doc]
            #vis fn #make_static_closure_name<'gc>(
                ctx: MutationContext<'gc>,
            ) -> ClosureRef<'gc>
            {
                ctx.#make_static(
                    #raw_name,
                    #get_meta_name(ctx),
                )
            }

            #vis fn #register_name<'gc>(
                ctx: MutationContext<'gc>,
            ) {
                let module = #module;
                let closure = #make_static_closure_name(ctx);
                let _ = module.define_rs(
                    ctx,
                    #scm_name,
                    closure,
                );
            }

            #trampoline
            #func
        }
    }

    pub fn register_call(&self) -> proc_macro2::TokenStream {
        let register_name = syn::Ident::new(
            &format!("register_{}", self.transformed_function.sig.ident),
            self.transformed_function.sig.ident.span(),
        );

        quote_spanned! {
            self.transformed_function.span() =>
            #register_name(ctx);
        }
    }
}
