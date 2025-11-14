use quote::quote_spanned;
use syn::spanned::Spanned;

use crate::{Input, ModulePath};

pub struct Variable {
    pub item: syn::ItemStatic,
    pub name: syn::LitStr,
    pub initializer: syn::Ident,
    pub getter: syn::Ident,
    pub setter: syn::Ident,
    pub module_path: ModulePath,
    pub vis: syn::Visibility,
    pub init: Box<syn::Expr>,
    pub typ: Box<syn::Type>,
}

impl Variable {
    pub fn new(
        item: syn::ItemStatic,
        name: syn::LitStr,
        initializer: syn::Ident,
        getter: syn::Ident,
        setter: syn::Ident,
        module_path: ModulePath,
        vis: syn::Visibility,
        init: Box<syn::Expr>,
        typ: Box<syn::Type>,
    ) -> Self {
        Self {
            item,
            name,
            initializer,
            getter,
            setter,
            module_path,
            vis,
            init,
            typ,
        }
    }

    pub fn register_call(&self) -> proc_macro2::TokenStream {
        let initializer = &self.initializer;
        quote_spanned! {
            self.item.span() =>
            #initializer(ctx);
        }
    }

    pub fn to_tokens(&self) -> proc_macro2::TokenStream {
        let Variable {
            name,
            initializer,
            getter,
            setter,
            module_path,
            vis,
            init,
            typ,
            item,
        } = self;

        let loc_static = syn::Ident::new(
            &format!("LOC_{}_VAR", item.ident.to_string().to_uppercase()),
            item.ident.span(),
        );

        let resolution = module_path.resolve();
        quote_spanned! {
            item.span() =>

            #vis static #loc_static: ::std::sync::OnceLock<Global<
                crate::Rootable!(
                    VariableRef<'_>
                )
            >> = ::std::sync::OnceLock::new();


            /// Initializer for the Scheme variable.
            ///
            /// Returns true if the variable was initialized successfully, false if it was already initialized.
            #[allow(non_snake_case)]
            #vis fn #initializer<'gc>(ctx: Context<'gc>) -> bool {
                let module = #resolution;
                let init: #typ = #init;
                let var = module.define_rs(
                    ctx,
                    #name,
                    init
                );
                #loc_static.set(Global::new(var)).is_ok()
            }

            #[allow(non_snake_case)]
            #vis fn #getter<'gc>(ctx: Context<'gc>) -> Value<'gc> {
                let var = #loc_static.get().unwrap_or_else(|| panic!("Variable '{}' is not initialized", #name)).fetch(&ctx);
                var.get()
            }

            #[allow(non_snake_case)]
            #vis fn #setter<'gc>(
                ctx: Context<'gc>,
                value: Value<'gc>,
            ) {
                let var = #loc_static.get().unwrap_or_else(|| panic!("Variable '{}' is not initialized", #name)).fetch(&ctx);
                var.set(ctx, value);
            }
        }
    }
}

pub fn handle(input: Input, item: syn::ItemStatic) -> Result<Variable, syn::Error> {
    let name = super::get_name(Some("name"), &input.meta)?;
    let initializer = super::get_ident(Some("init"), &input.meta)?
        .unwrap_or_else(|| syn::Ident::new(&format!("init_{}", item.ident), item.ident.span()));

    let getter = super::get_ident(Some("get"), &input.meta)?
        .unwrap_or_else(|| syn::Ident::new(&format!("get_{}", item.ident), item.ident.span()));

    let setter = super::get_ident(Some("set"), &input.meta)?
        .unwrap_or_else(|| syn::Ident::new(&format!("set_{}", item.ident), item.ident.span()));

    let name = name.unwrap_or_else(|| {
        let name = item.ident.to_string();
        let kebab_name = name.replace('_', "-");

        syn::LitStr::new(&kebab_name, item.ident.span())
    });

    let module_path = super::get_module_path(&input.meta)?;

    let vis = item.vis.clone();
    let init = item.expr.clone();
    let typ = item.ty.clone();
    Ok(Variable::new(
        item,
        name,
        initializer,
        getter,
        setter,
        module_path,
        vis,
        init,
        typ,
    ))
}
