use quote::quote_spanned;
use syn::spanned::Spanned;

use crate::{Input, ModulePath, fun::FunctionDefinition, get_path, var::Variable};

pub enum ModuleItem {
    Function(FunctionDefinition),
    Variable(Variable),
    Use(syn::ItemUse),
}

fn take_scheme_attr(attrs: &mut Vec<syn::Attribute>) -> syn::Result<Option<Input>> {
    for i in 0..attrs.len() {
        if let syn::Meta::List(ref list) = attrs[i].meta
            && list.path.is_ident("scheme")
        {
            let list = list.clone();
            let _ = attrs.remove(i);
            return syn::parse2::<Input>(list.tokens.clone()).map(Some);
        }
    }
    Ok(None)
}

pub struct Module {
    pub rust_name: syn::Ident,
    pub path: syn::Path,
    pub items: Vec<ModuleItem>,
}

/// Convert Rust module into Scheme module. Collect all items within the module
/// and process them accordingly.
pub fn handle(Input { meta: args }: Input, input: syn::ItemMod) -> syn::Result<Module> {
    let module_name = get_path(Some("path"), &args)?.ok_or_else(|| {
        syn::Error::new_spanned(
            &input,
            "Module path must be specified using `path` argument",
        )
    })?;
    let content = if let Some((_, items)) = input.content {
        items
    } else {
        return Err(syn::Error::new_spanned(
            input,
            "#[scheme] can only be applied to inline modules",
        ));
    };

    let mut registrations = Vec::new();

    for item in content {
        match item {
            syn::Item::Fn(mut fun) => {
                let args = take_scheme_attr(&mut fun.attrs)?
                    .ok_or_else(|| {
                        syn::Error::new_spanned(
                            &fun,
                            "Function within #[scheme] module must have #[scheme] attribute",
                        )
                    })?
                    .meta;
                let registration = crate::fun::handle(Input { meta: args }, fun)?;
                if registration.scheme_attribute.module_path != ModulePath::CurrentModule {
                    return Err(syn::Error::new_spanned(
                        &registration.transformed_function,
                        "Function within #[scheme] module cannot specify module path",
                    ));
                }
                registrations.push(ModuleItem::Function(registration));
            }
            syn::Item::Static(mut static_) => {
                let args = take_scheme_attr(&mut static_.attrs)?
                    .ok_or_else(|| {
                        syn::Error::new_spanned(
                            &static_,
                            "Static variable within #[scheme] module must have #[scheme] attribute",
                        )
                    })?
                    .meta;
                let registration = crate::var::handle(Input { meta: args }, static_)?;
                if registration.module_path != ModulePath::CurrentModule {
                    return Err(syn::Error::new_spanned(
                        &registration.item,
                        "Static variable within #[scheme] module cannot specify module path",
                    ));
                }

                registrations.push(ModuleItem::Variable(registration));
            }
            syn::Item::Use(use_) => {
                registrations.push(ModuleItem::Use(use_));
            }
            _ => {
                return Err(syn::Error::new_spanned(
                    item,
                    "#[scheme] can only be applied to functions or static variables within modules",
                ));
            }
        }
    }

    Ok(Module {
        rust_name: input.ident,
        path: module_name,
        items: registrations,
    })
}

impl Module {
    pub fn to_tokens(&mut self) -> proc_macro2::TokenStream {
        let Module {
            rust_name,
            path,
            items,
        } = self;

        let register_calls = items
            .iter()
            .filter_map(|item| match item {
                ModuleItem::Function(fun) => Some(fun.register_call()),
                ModuleItem::Variable(var) => Some(var.register_call()),
                ModuleItem::Use(_) => None,
            })
            .collect::<Vec<_>>();
        let exports = items
            .iter()
            .filter_map(|item| {
                let name = match item {
                    ModuleItem::Variable(var) => Some(&var.name),
                    ModuleItem::Function(fun) => Some(&fun.scheme_attribute.name),
                    _ => None,
                };

                name.map(|name| {
                    quote_spanned! {
                        name.span() =>
                        module.export_one(ctx, ctx.intern(#name));
                    }
                })
            })
            .collect::<Vec<_>>();
        let item_tokens = items.iter_mut().map(|item| match item {
            ModuleItem::Function(fun) => fun.to_tokens(),
            ModuleItem::Variable(var) => var.to_tokens(),
            ModuleItem::Use(use_) => quote_spanned! {
                use_.span() =>
                #use_
            },
        });

        let mut items = proc_macro2::TokenStream::new();
        for item in item_tokens {
            items.extend(item);
        }

        let path = path
            .segments
            .iter()
            .map(|seg| seg.ident.to_string())
            .collect::<Vec<_>>()
            .join(" ");
        let hint = exports.len();
        items.extend(quote_spanned! {
            rust_name.span() =>

            pub fn register<'gc>(ctx: Context<'gc>) -> ModuleRef<'gc> {
                let old = current_module(ctx).get(ctx);
                let module = ctx.define_module(#path, Some(#hint));
                current_module(ctx).set(ctx, module.into());
                #(#register_calls)*
                #(#exports)*

                current_module(ctx).set(ctx, old.into());
                module
            }
        });

        quote_spanned! {
            rust_name.span() =>
            pub mod #rust_name {
                use super::*;
                #items
            }
        }
    }
}
