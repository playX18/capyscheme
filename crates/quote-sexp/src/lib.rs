use quote::ToTokens;
use syn::{parse::Parse, punctuated::Punctuated, Ident};

enum SExp {
    Literal(syn::Lit),
    Null,
    Unquote(syn::Expr),
    UnquoteSplicing(syn::Expr),

    Quote(Box<SExp>),
    Cons(Box<SExp>, Box<SExp>),
    Symbol(syn::Ident),

    Vec(Vec<SExp>),
    Wildcard,
}

impl Parse for SExp {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {

        if input.peek(syn::Token![let]) {
            let _ = input.parse::<syn::Token![let]>()?;
            return Ok(SExp::Symbol(Ident::new("let", proc_macro2::Span::call_site())));
        }

        if input.peek(syn::Token![_]) {
            input.parse::<syn::Token![_]>()?;
            return Ok(SExp::Wildcard);
        }

        if input.peek(syn::Token![#]) {
            input.parse::<syn::Token![#]>()?;
            let inner: SExp = input.parse()?;
            return Ok(SExp::Quote(Box::new(inner)));
        }

        if input.peek(syn::Token![::]) {
            input.parse::<syn::Token![::]>()?;
            let rest: SExp = input.parse()?;
            return Ok(SExp::Cons(Box::new(SExp::Null), Box::new(rest)));
        }
        if input.peek(syn::Lit) {
            let lit: syn::Lit = input.parse()?;

            return Ok(SExp::Literal(lit));
        } else if input.peek(syn::Ident) {
            let ident: syn::Ident = input.parse()?;
            return Ok(SExp::Symbol(ident));
        } else if input.peek(syn::Token![,]) {
            let _ = input.parse::<syn::Token![,]>()?;
            if input.peek(syn::Token![@]) {
              
                input.parse::<syn::Token![@]>()?;
                let expr: syn::Expr = input.parse()?;
                return Ok(SExp::UnquoteSplicing(expr));
            } else {
                input.parse::<syn::Token![#]>()?;
                let expr: syn::Expr = input.parse()?;
                return Ok(SExp::Unquote(expr));
            }
        } else if input.peek(syn::Token![#]) {
            let inner;
            syn::parenthesized!(inner in input);

            let mut elements = Vec::new();

            while !inner.is_empty() {
                elements.push(inner.parse()?);
            }

            Ok(SExp::Vec(elements))
        } else {
            let inner;
            syn::parenthesized!(inner in input);

            if inner.is_empty() {
                return Ok(SExp::Null);
            }

            let mut expressions: Vec<SExp> = Vec::new();
            let mut append = SExp::Null;

            while !inner.is_empty() {
                if inner.peek(syn::Token![::]) {
                    inner.parse::<syn::Token![::]>()?;
                    append = inner.parse()?;
                    break;
                } else {

                    expressions.push(inner.parse()?);
                }
            }

            Ok(expressions
                .into_iter()
                .rev()
                .fold(append, |cdr, car| SExp::Cons(Box::new(car), Box::new(cdr))))
        }
    }
}

impl ToTokens for SExp {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {

            SExp::Quote(quoted) => {
                let inner = quoted.to_token_stream();
                tokens.extend(quote::quote! {
                    ::capyc::ast::Datum::make_list(
                        [::capyc::ast::Datum::make_symbol("quote", None), #inner],
                        None 
                    )
                }); 
            }
            SExp::Wildcard => {
                tokens.extend(quote::quote! { ::capyc::ast::Datum::make_symbol("_", None) })
            }
            SExp::Literal(lit) => tokens.extend(quote::quote! {
                ::capyc::ast::P(::capyc::ast::Datum::from(#lit))
            }),
            SExp::Null => tokens
                .extend(quote::quote! { ::capyc::ast::Datum::new(::capyc::ast::DatumValue::Null) }),
            SExp::Unquote(expr) => tokens.extend(quote::quote! { #expr }),
            SExp::UnquoteSplicing(expr) => tokens.extend(quote::quote! { #expr }),
            SExp::Cons(head, tail) => {
                tokens.extend(quote::quote! {
                    ::capyc::ast::Datum::cons(#head, #tail, None)
                });
            }
            SExp::Symbol(ident) => {
                let id = ident.to_string();
                tokens.extend(quote::quote! {
                    ::capyc::ast::Datum::make_symbol(#id, None)
                });
            }
            SExp::Vec(vec) => {
                let mut inner = proc_macro2::TokenStream::new();
                for item in vec {
                    item.to_tokens(&mut inner);
                }
                tokens.extend(quote::quote! { [#inner] });
            }
        }
    }
}

#[proc_macro]
pub fn quote(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let s_exp: SExp = syn::parse_macro_input!(input as SExp);
    let mut tokens = proc_macro2::TokenStream::new();
    s_exp.to_tokens(&mut tokens);

    quote::quote! {
        {
            thread_local! {
                static QUOTE_SEXP: ::std::cell::LazyCell<::capyc::ast::P<::capyc::ast::Datum>> = ::std::cell::LazyCell::new(|| {
                    #tokens
                });
            }

            QUOTE_SEXP.with(|s| (*s).clone())
        }
    }.into()
}
 struct MatchArm {
    pat: SExp,
    arrow: syn::Token![=>],
    body: syn::Expr,
}

struct MatchInput {
    expr: syn::Expr,
    arms: Punctuated<MatchArm, syn::Token![,]>,
}

impl Parse for MatchArm {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let pat: SExp = input.parse()?;

        let arrow: syn::Token![=>] = input.parse()?;
        let body: syn::Expr = input.parse()?;
        Ok(MatchArm { pat, arrow, body })
    }
}

impl Parse for MatchInput {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        
        let expr: syn::Expr = input.parse()?;
       
        let inner;
        syn::braced!(inner in input);
        
        let arms: Punctuated<MatchArm, syn::Token![,]> =
            inner.parse_terminated(MatchArm::parse, syn::Token![,])?;
        Ok(MatchInput { expr, arms })
    }
}

fn simple_match_pat(
    datum_exp: proc_macro2::TokenStream,
    pat: &SExp,
    kt: proc_macro2::TokenStream,
    kf: proc_macro2::TokenStream,
) -> proc_macro2::TokenStream {
    match pat {
        SExp::Wildcard => kt,
        SExp::Symbol(var) => {
            quote::quote! {
                let #var = #datum_exp.clone();
                #kt
            }
        }

        SExp::Null => {
            quote::quote! {
                if #datum_exp.is_null() {
                    #kt
                } else {
                    #kf
                }
            }
        }

        SExp::Literal(lit) => {
            quote::quote! {
                if &**#datum_exp == &::capyc::ast::Datum::from(#lit) {
                    #kt
                } else {
                    #kf
                }
            }
        }

        SExp::Cons(p_head, p_tail) => {
            let head = Ident::new("__head", proc_macro2::Span::call_site());
            let tail = Ident::new("__tail", proc_macro2::Span::call_site());
            let tail_kt = simple_match_pat(quote::quote! { #tail }, p_tail, kt, kf.clone());

            let head_kt = simple_match_pat(quote::quote! { #head }, p_head, tail_kt, kf.clone());

            quote::quote! {
                if let Some((#head, #tail)) = #datum_exp.try_pair() {
                    #head_kt
                } else {
                    #kf
                }
            }
        }

        SExp::Quote(inner) => {
            let inner_tokens = quote::quote! { #inner };
            quote::quote! {
                if &#inner_tokens == #datum_exp {
                    #kt
                } else {
                    #kf
                }
            }
        }

        SExp::Vec(vec) => {
            let mut inner_tokens = proc_macro2::TokenStream::new();
            for item in vec {
                inner_tokens.extend(item.to_token_stream());
            }
            quote::quote! {
                if let ::capyc::ast::DatumValue::Vec(ref v) = #datum_exp.value() {
                    if v == &[#inner_tokens] {
                        #kt
                    } else {
                        #kf
                    }
                } else {
                    #kf
                }
            }
        }

        SExp::Unquote(expr) => {
            quote::quote! {
                if (#expr)(#datum_exp) {
                    #kt
                } else {
                    #kf
                }
            }
        }
        _ => todo!(),
    }
}
#[proc_macro]
pub fn simple_match(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input!(input as MatchInput);

    let expr_to_match = &input.expr;

    let final_kf = quote::quote! { panic!("No match found") };
    let datum = Ident::new("__datum", proc_macro2::Span::call_site());
    let logic = input.arms.iter().fold(
        final_kf.clone(),
        |kf, arm| {
            let pat = &arm.pat;
            let body = &arm.body;
            let kt = body.to_token_stream();
            simple_match_pat(datum.to_token_stream(), pat, kt, kf)
        },
    );

    let expanded = quote::quote! {
        {
            let #datum = &(#expr_to_match);
            #logic
        }
    };

    expanded.into()
}

