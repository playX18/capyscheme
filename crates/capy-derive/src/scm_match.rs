//! Pattern matching on Scheme values.

use std::collections::{BTreeMap, BTreeSet};

use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote};
use syn::{
    Expr, Ident, LitBool, LitChar, LitFloat, LitInt, LitStr, Result, Token, parenthesized,
    parse::{Parse, ParseStream, discouraged::Speculative},
};

pub(crate) fn expand(input: proc_macro::TokenStream) -> Result<TokenStream> {
    let input = syn::parse::<MatchInput>(input)?;
    let mut expander = Expander::default();

    let ctx_ident = expander.fresh_ident("__capy_scm_match_ctx");
    let value_ident = expander.fresh_ident("__capy_scm_match_value");
    let label = syn::Lifetime::new("'__capy_scm_match", Span::call_site());

    let arm_tokens = input
        .arms
        .iter()
        .map(|arm| expander.expand_arm(arm, &ctx_ident, &value_ident, &label))
        .collect::<Result<Vec<_>>>()?;
    let literal_helpers = expander.literal_helpers();
    let ctx = input.ctx;
    let value = input.value;

    Ok(quote! {
        {
            #(#literal_helpers)*

            let #ctx_ident = #ctx;
            let #value_ident = #value;

            #label: {
                #(#arm_tokens)*
                panic!("scm_match!: non-exhaustive match");
            }
        }
    })
}

struct MatchInput {
    ctx: Expr,
    value: Expr,
    arms: Vec<MatchArm>,
}

impl Parse for MatchInput {
    fn parse(input: ParseStream) -> Result<Self> {
        let ctx = input.parse()?;
        input.parse::<Token![,]>()?;
        let value = input.parse()?;
        input.parse::<Token![,]>()?;

        let content;
        syn::braced!(content in input);

        let mut arms = Vec::new();
        while !content.is_empty() {
            arms.push(content.parse()?);
            if content.is_empty() {
                break;
            }
            content.parse::<Token![,]>()?;
        }

        if arms.is_empty() {
            return Err(input.error("scm_match! requires at least one match arm"));
        }

        Ok(Self { ctx, value, arms })
    }
}

struct MatchArm {
    pattern: Pattern,
    guard: Option<Expr>,
    body: Expr,
}

impl Parse for MatchArm {
    fn parse(input: ParseStream) -> Result<Self> {
        let pattern = input.parse()?;
        let guard = if input.peek(Token![if]) {
            input.parse::<Token![if]>()?;
            Some(input.parse()?)
        } else {
            None
        };
        input.parse::<Token![=>]>()?;
        let body = input.parse()?;

        Ok(Self {
            pattern,
            guard,
            body,
        })
    }
}

enum Pattern {
    Wildcard,
    Bind(Ident),
    Null,
    Bool(LitBool),
    Int(LitInt),
    Float(LitFloat),
    Char(LitChar),
    String(LitStr),
    Symbol(LitStr),
    List(ListPattern),
}

struct ListPattern {
    elems: Vec<Pattern>,
    tail: Option<Box<Pattern>>,
}

impl Parse for Pattern {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(Token![_]) {
            input.parse::<Token![_]>()?;
            return Ok(Self::Wildcard);
        }

        if input.peek(syn::token::Paren) {
            let content;
            parenthesized!(content in input);
            if content.is_empty() {
                return Ok(Self::Null);
            }

            let mut elems = Vec::new();
            let mut tail = None;
            while !content.is_empty() {
                if content.peek(Token![.]) {
                    content.parse::<Token![.]>()?;
                    tail = Some(Box::new(content.parse()?));
                    if !content.is_empty() {
                        return Err(content.error("unexpected tokens after dotted tail pattern"));
                    }
                    break;
                }

                elems.push(content.parse()?);
            }

            return Ok(Self::List(ListPattern { elems, tail }));
        }

        {
            let fork = input.fork();
            if let Ok(proc_macro2::TokenTree::Punct(punct)) = fork.parse::<proc_macro2::TokenTree>()
                && punct.as_char() == '\''
            {
                input.advance_to(&fork);
                if input.peek(Ident) {
                    let ident: Ident = input.parse()?;
                    return Ok(Self::Symbol(LitStr::new(&ident.to_string(), ident.span())));
                }

                return Err(input.error("expected a symbol identifier after `'`"));
            }
        }

        if input.peek(LitStr) {
            return Ok(Self::String(input.parse()?));
        }

        if input.peek(LitChar) {
            return Ok(Self::Char(input.parse()?));
        }

        if input.peek(LitBool) {
            return Ok(Self::Bool(input.parse()?));
        }

        if input.peek(LitFloat) {
            return Ok(Self::Float(input.parse()?));
        }

        if input.peek(LitInt) {
            return Ok(Self::Int(input.parse()?));
        }

        if input.peek(Ident) {
            let ident: Ident = input.parse()?;
            if ident == "sym" && input.peek(syn::token::Paren) {
                let content;
                parenthesized!(content in input);
                let lit: LitStr = content.parse()?;
                if !content.is_empty() {
                    return Err(content.error("expected a single string literal in sym(...)"));
                }
                return Ok(Self::Symbol(lit));
            }

            return Ok(Self::Bind(ident));
        }

        Err(input.error("unsupported scm_match! pattern"))
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum LiteralKind {
    Symbol,
    String,
}

#[derive(Default)]
struct Expander {
    counter: usize,
    literals: BTreeMap<(LiteralKind, String), Ident>,
}

impl Expander {
    fn fresh_ident(&mut self, prefix: &str) -> Ident {
        let ident = format_ident!("{prefix}_{}", self.counter, span = Span::call_site());
        self.counter += 1;
        ident
    }

    fn expand_arm(
        &mut self,
        arm: &MatchArm,
        ctx_ident: &Ident,
        value_ident: &Ident,
        label: &syn::Lifetime,
    ) -> Result<TokenStream> {
        let mut names = BTreeSet::new();
        arm.pattern.collect_binds(&mut names)?;

        let checker = self.check_pattern(&arm.pattern, quote!(#value_ident), ctx_ident)?;
        let binders = self.bind_pattern(&arm.pattern, quote!(#value_ident));
        let guard = arm
            .guard
            .as_ref()
            .map_or_else(|| quote!(true), |expr| quote!(#expr));
        let body = &arm.body;

        Ok(quote! {
            {
                if #checker {
                    #(#binders)*
                    if #guard {
                        break #label (#body);
                    }
                }
            }
        })
    }

    fn check_pattern(
        &mut self,
        pattern: &Pattern,
        value: TokenStream,
        ctx_ident: &Ident,
    ) -> Result<TokenStream> {
        Ok(match pattern {
            Pattern::Wildcard | Pattern::Bind(_) => quote!(true),
            Pattern::Null => quote!(#value.is_null()),
            Pattern::Bool(lit) => {
                let expected = lit.value;
                quote!(#value.is_bool() && #value.as_bool() == #expected)
            }
            Pattern::Int(lit) => {
                let expected = lit.base10_parse::<i32>()?;
                quote!(#value.is_int32() && #value.as_int32() == #expected)
            }
            Pattern::Float(lit) => {
                let expected = lit.base10_parse::<f64>()?;
                quote!(#value.is_flonum() && #value.as_flonum() == #expected)
            }
            Pattern::Char(lit) => {
                let expected = lit.value();
                quote!(#value.is_char() && #value.char() == #expected)
            }
            Pattern::String(lit) => {
                let helper = self.literal_helper(LiteralKind::String, lit.value());
                quote!(#value.r5rs_equal(#helper(#ctx_ident)))
            }
            Pattern::Symbol(lit) => {
                let helper = self.literal_helper(LiteralKind::Symbol, lit.value());
                quote!(#value == #helper(#ctx_ident))
            }
            Pattern::List(list) => self.check_list_pattern(list, value, ctx_ident)?,
        })
    }

    fn check_list_pattern(
        &mut self,
        list: &ListPattern,
        value: TokenStream,
        ctx_ident: &Ident,
    ) -> Result<TokenStream> {
        self.check_list_steps(&list.elems, list.tail.as_deref(), value, ctx_ident)
    }

    fn check_list_steps(
        &mut self,
        elems: &[Pattern],
        tail: Option<&Pattern>,
        current: TokenStream,
        ctx_ident: &Ident,
    ) -> Result<TokenStream> {
        if let Some((head, rest)) = elems.split_first() {
            let car = self.fresh_ident("__capy_scm_car");
            let cdr = self.fresh_ident("__capy_scm_cdr");
            let head_check = self.check_pattern(head, quote!(#car), ctx_ident)?;
            let rest_check = self.check_list_steps(rest, tail, quote!(#cdr), ctx_ident)?;

            Ok(quote! {
                #current.is_pair() && {
                    let #car = #current.car();
                    let #cdr = #current.cdr();
                    #head_check && #rest_check
                }
            })
        } else if let Some(tail) = tail {
            self.check_pattern(tail, current, ctx_ident)
        } else {
            Ok(quote!(#current.is_null()))
        }
    }

    fn bind_pattern(&mut self, pattern: &Pattern, value: TokenStream) -> Vec<TokenStream> {
        match pattern {
            Pattern::Wildcard
            | Pattern::Null
            | Pattern::Bool(_)
            | Pattern::Int(_)
            | Pattern::Float(_)
            | Pattern::Char(_)
            | Pattern::String(_)
            | Pattern::Symbol(_) => Vec::new(),
            Pattern::Bind(ident) => vec![quote!(let #ident = #value;)],
            Pattern::List(list) => self.bind_list_pattern(list, value),
        }
    }

    fn bind_list_pattern(&mut self, list: &ListPattern, value: TokenStream) -> Vec<TokenStream> {
        let mut bindings = Vec::new();
        let mut current = self.fresh_ident("__capy_scm_bind_list");
        bindings.push(quote!(let #current = #value;));

        for elem in &list.elems {
            let car = self.fresh_ident("__capy_scm_bind_car");
            let next_current = self.fresh_ident("__capy_scm_bind_list");
            bindings.push(quote!(let #car = #current.car();));
            bindings.extend(self.bind_pattern(elem, quote!(#car)));
            bindings.push(quote!(let #next_current = #current.cdr();));
            current = next_current;
        }

        if let Some(tail) = &list.tail {
            bindings.extend(self.bind_pattern(tail, quote!(#current)));
        }

        bindings
    }

    fn literal_helper(&mut self, kind: LiteralKind, value: String) -> Ident {
        if let Some(existing) = self.literals.get(&(kind, value.clone())) {
            return existing.clone();
        }

        let prefix = match kind {
            LiteralKind::Symbol => "__capy_scm_symbol",
            LiteralKind::String => "__capy_scm_string",
        };
        let ident = self.fresh_ident(prefix);
        self.literals.insert((kind, value), ident.clone());
        ident
    }

    fn literal_helpers(&self) -> Vec<TokenStream> {
        self.literals
			.iter()
			.map(|((kind, value), ident)| {
				let init = match kind {
					LiteralKind::Symbol => quote!(ctx.intern(#value)),
					LiteralKind::String => quote!(ctx.str(#value)),
				};

				quote! {
					#[allow(non_snake_case)]
					fn #ident<'gc>(ctx: crate::runtime::Context<'gc>) -> crate::runtime::value::Value<'gc> {
						static VALUE: ::std::sync::OnceLock<
							crate::runtime::global::Global<
								crate::Rootable!(crate::runtime::value::Value<'_>)
							>
						> = ::std::sync::OnceLock::new();

						VALUE
							.get_or_init(|| crate::runtime::global::Global::new(ctx, #init))
							.fetch(ctx)
					}
				}
			})
			.collect()
    }
}

impl Pattern {
    fn collect_binds(&self, names: &mut BTreeSet<String>) -> Result<()> {
        match self {
            Pattern::Bind(ident) => {
                let key = ident.to_string();
                if !names.insert(key) {
                    return Err(syn::Error::new_spanned(
                        ident,
                        "duplicate binding in scm_match! pattern",
                    ));
                }
            }
            Pattern::List(ListPattern { elems, tail }) => {
                for elem in elems {
                    elem.collect_binds(names)?;
                }
                if let Some(tail) = tail {
                    tail.collect_binds(names)?;
                }
            }
            Pattern::Wildcard
            | Pattern::Null
            | Pattern::Bool(_)
            | Pattern::Int(_)
            | Pattern::Float(_)
            | Pattern::Char(_)
            | Pattern::String(_)
            | Pattern::Symbol(_) => {}
        }

        Ok(())
    }
}
