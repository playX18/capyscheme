use quote::quote_spanned;
use syn::spanned::Spanned;

use crate::Input;

pub fn handle(input: Input, item: syn::ItemStatic) -> Result<proc_macro2::TokenStream, syn::Error> {
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

    let loc_static = syn::Ident::new(
        &format!("LOC_{}_VAR", item.ident.to_string().to_uppercase()),
        item.ident.span(),
    );

    let vis = &item.vis;

    let resolution = module_path.resolve();
    let init = &item.expr;
    let typ = &item.ty;
    Ok(quote_spanned! {
        item.span() =>

        #vis static #loc_static: ::std::sync::OnceLock<Global<
            Rootable!(
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
    })
}
