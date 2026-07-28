//! `#[cps]` — control-flow rewrite onto SNI continuations.

use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::{
    Expr, ExprCall, ExprIf, ExprLoop, ExprMatch, ExprReturn, ExprWhile, FnArg, Ident, ItemFn, Pat,
    ReturnType, Stmt, Type, parse_quote, visit::Visit,
};

pub fn expand_cps(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut func: ItemFn = match syn::parse2(item) {
        Ok(f) => f,
        Err(e) => return e.to_compile_error(),
    };

    if func.sig.asyncness.is_some() {
        return quote! {
            compile_error!("#[cps] does not support async fn");
        };
    }

    let ret_ty = match &func.sig.output {
        ReturnType::Default => quote! { () },
        ReturnType::Type(_, ty) => quote! { #ty },
    };

    let bootstrap = match classify_receiver(&func) {
        ReceiverKind::Scm(scm_pat) => Bootstrap::Scm {
            scm_pat,
            ret_ty: ret_ty.clone(),
        },
        ReceiverKind::Env(env_pat) => Bootstrap::Env {
            env_pat,
            ret_ty: ret_ty.clone(),
        },
        ReceiverKind::Bad => {
            return quote! {
                compile_error!(
                    "#[cps] requires first argument `&Scm` (wraps enter) or `&mut Env` (already entered)"
                );
            };
        }
    };

    if let Err(e) = check_unsupported(&func.block) {
        return e;
    }

    let body = match transform_block(&func.block, &ret_ty) {
        Ok(ts) => ts,
        Err(e) => return e,
    };

    let block = match bootstrap {
        Bootstrap::Scm { scm_pat, ret_ty } => {
            quote! {
                {
                    let #scm_pat = #scm_pat;
                    #scm_pat.enter(|env| {
                        ::capy_sni::__cps::run::<#ret_ty>(env, ::std::boxed::Box::new(move |env, c| {
                            #body
                        }))
                    })
                }
            }
        }
        Bootstrap::Env { env_pat, ret_ty } => {
            quote! {
                {
                    ::capy_sni::__cps::run::<#ret_ty>(#env_pat, ::std::boxed::Box::new(move |env, c| {
                        let _ = env;
                        #body
                    }))
                }
            }
        }
    };

    func.block = parse_quote!({ #block });
    quote! { #func }
}

enum ReceiverKind {
    Scm(Pat),
    Env(Pat),
    Bad,
}

enum Bootstrap {
    Scm { scm_pat: Pat, ret_ty: TokenStream },
    Env { env_pat: Pat, ret_ty: TokenStream },
}

fn classify_receiver(func: &ItemFn) -> ReceiverKind {
    let Some(FnArg::Typed(arg)) = func.sig.inputs.first() else {
        return ReceiverKind::Bad;
    };
    let pat = (*arg.pat).clone();
    match &*arg.ty {
        Type::Reference(r) => {
            let inner = r.elem.as_ref();
            if type_ends_with(inner, "Scm") && r.mutability.is_none() {
                ReceiverKind::Scm(pat)
            } else if type_ends_with(inner, "Env") && r.mutability.is_some() {
                ReceiverKind::Env(pat)
            } else {
                ReceiverKind::Bad
            }
        }
        _ => ReceiverKind::Bad,
    }
}

fn type_ends_with(ty: &Type, name: &str) -> bool {
    match ty {
        Type::Path(p) => p
            .path
            .segments
            .last()
            .map(|s| s.ident == name)
            .unwrap_or(false),
        _ => false,
    }
}

fn check_unsupported(block: &syn::Block) -> Result<(), TokenStream> {
    struct V {
        err: Option<TokenStream>,
        in_guard: bool,
    }
    impl<'ast> Visit<'ast> for V {
        fn visit_expr(&mut self, e: &'ast Expr) {
            if self.err.is_some() {
                return;
            }
            match e {
                Expr::Await(_) => {
                    self.err = Some(quote! {
                        compile_error!("#[cps] does not support .await");
                    });
                    return;
                }
                Expr::Try(_) => {
                    self.err = Some(quote! {
                        compile_error!("#[cps] does not support `?`; use match");
                    });
                    return;
                }
                Expr::Call(c) => {
                    if let Expr::Path(p) = c.func.as_ref()
                        && path_is_cps_op(&p.path, "guard")
                    {
                        let old = self.in_guard;
                        self.in_guard = true;
                        syn::visit::visit_expr(self, e);
                        self.in_guard = old;
                        return;
                    }
                }
                Expr::Closure(_) if !self.in_guard && closure_has_suspend_raw(e) => {
                    self.err = Some(quote! {
                        compile_error!(
                            "#[cps] does not support closures that suspend (except cps::guard)"
                        );
                    });
                    return;
                }
                _ => {}
            }
            syn::visit::visit_expr(self, e);
        }
    }
    let mut v = V {
        err: None,
        in_guard: false,
    };
    v.visit_block(block);
    match v.err {
        Some(e) => Err(e),
        None => Ok(()),
    }
}

fn closure_has_suspend_raw(e: &Expr) -> bool {
    struct Has(bool);
    impl<'ast> Visit<'ast> for Has {
        fn visit_expr(&mut self, e: &'ast Expr) {
            if self.0 {
                return;
            }
            if suspend_kind(e).is_some() {
                self.0 = true;
                return;
            }
            syn::visit::visit_expr(self, e);
        }
    }
    let mut h = Has(false);
    h.visit_expr(e);
    h.0
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum SuspendKind {
    Call,
    Raise,
    Guard,
}

fn path_is_cps_op(path: &syn::Path, op: &str) -> bool {
    let segs: Vec<_> = path.segments.iter().map(|s| s.ident.to_string()).collect();
    match segs.as_slice() {
        [a, b] if (a == "cps" || a == "cps_ops") && b == op => true,
        [a, b, c] if a == "capy_sni" && (b == "cps" || b == "cps_ops") && c == op => true,
        _ => false,
    }
}

fn suspend_kind(expr: &Expr) -> Option<SuspendKind> {
    let Expr::Call(ExprCall { func, .. }) = expr else {
        return None;
    };
    let Expr::Path(p) = func.as_ref() else {
        return None;
    };
    if path_is_cps_op(&p.path, "call") {
        Some(SuspendKind::Call)
    } else if path_is_cps_op(&p.path, "raise") {
        Some(SuspendKind::Raise)
    } else if path_is_cps_op(&p.path, "guard") {
        Some(SuspendKind::Guard)
    } else {
        None
    }
}

fn expr_has_suspend(expr: &Expr) -> bool {
    struct Has(bool);
    impl<'ast> Visit<'ast> for Has {
        fn visit_expr(&mut self, e: &'ast Expr) {
            if self.0 {
                return;
            }
            if suspend_kind(e).is_some() {
                self.0 = true;
                return;
            }
            syn::visit::visit_expr(self, e);
        }
    }
    let mut h = Has(false);
    h.visit_expr(expr);
    h.0
}

fn stmt_has_suspend(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Local(l) => l
            .init
            .as_ref()
            .map(|i| expr_has_suspend(&i.expr))
            .unwrap_or(false),
        Stmt::Expr(e, _) => expr_has_suspend(e),
        Stmt::Item(_) | Stmt::Macro(_) => false,
    }
}

/// What to do after a statement sequence.
#[derive(Clone)]
enum Tail {
    /// Function body: `finish` with last expr / `()`.
    Finish,
    /// Run this token stream after the stmts (join point).
    Cont(TokenStream),
}

fn transform_block(block: &syn::Block, ret_ty: &TokenStream) -> Result<TokenStream, TokenStream> {
    transform_stmts(&block.stmts, ret_ty, Tail::Finish)
}

fn transform_stmts(
    stmts: &[Stmt],
    ret_ty: &TokenStream,
    tail: Tail,
) -> Result<TokenStream, TokenStream> {
    transform_stmts_seeded(stmts, ret_ty, tail, Vec::new())
}

fn transform_stmts_seeded(
    stmts: &[Stmt],
    ret_ty: &TokenStream,
    tail: Tail,
    mut live_refs: Vec<Ident>,
) -> Result<TokenStream, TokenStream> {
    if stmts.is_empty() {
        return match &tail {
            Tail::Finish => Ok(quote! {
                return ::capy_sni::__cps::finish::<#ret_ty>(c, ());
            }),
            Tail::Cont(ts) => Ok(ts.clone()),
        };
    }

    for (i, stmt) in stmts.iter().enumerate() {
        if let Some(ts) = try_split_stmt(stmt, &stmts[i + 1..], ret_ty, &tail, &live_refs)? {
            let prefix = &stmts[..i];
            return Ok(quote! {
                #(#prefix)*
                #ts
            });
        }
        collect_ref_bindings_from_stmt(stmt, &mut live_refs);
    }

    match &tail {
        Tail::Finish => {
            let (head, last) = stmts.split_at(stmts.len().saturating_sub(1));
            if let Some(Stmt::Expr(e, None)) = last.first() {
                if expr_has_suspend(e) {
                    return transform_tail_expr(e, ret_ty);
                }
                return Ok(quote! {
                    #(#head)*
                    return ::capy_sni::__cps::finish::<#ret_ty>(c, #e);
                });
            }
            if let Some(Stmt::Expr(e, Some(_))) = last.first() {
                return Ok(quote! {
                    #(#head)*
                    #e;
                    return ::capy_sni::__cps::finish::<#ret_ty>(c, ());
                });
            }
            Ok(quote! {
                #(#stmts)*
                return ::capy_sni::__cps::finish::<#ret_ty>(c, ());
            })
        }
        Tail::Cont(after) => Ok(quote! {
            #(#stmts)*
            #after
        }),
    }
}

fn try_split_stmt(
    stmt: &Stmt,
    rest: &[Stmt],
    ret_ty: &TokenStream,
    tail: &Tail,
    live_before: &[Ident],
) -> Result<Option<TokenStream>, TokenStream> {
    match stmt {
        Stmt::Local(local) => {
            let Some(init) = &local.init else {
                return Ok(None);
            };
            let expr = &init.expr;
            if let Some(kind) = suspend_kind(expr) {
                let pat = &local.pat;
                let then_body = transform_stmts(rest, ret_ty, tail.clone())?;
                let live = live_refs_across(live_before, rest);
                return Ok(Some(expand_suspend(
                    kind,
                    expr,
                    Some(pat),
                    then_body,
                    ret_ty,
                    &live,
                )?));
            }
            if expr_has_suspend(expr) {
                return Err(quote! {
                    compile_error!(
                        "#[cps] suspend must be the entire `let` initializer"
                    );
                });
            }
            Ok(None)
        }
        Stmt::Expr(expr, semi) => {
            match expr {
                Expr::If(i)
                    if expr_has_suspend(expr)
                        || rest.iter().any(stmt_has_suspend)
                        || !rest.is_empty()
                        || matches!(tail, Tail::Cont(_)) =>
                {
                    return Ok(Some(transform_if(i, rest, ret_ty, tail)?));
                }
                Expr::While(w) if expr_has_suspend(expr) => {
                    return Ok(Some(transform_while(w, rest, ret_ty, tail)?));
                }
                Expr::Loop(l) if expr_has_suspend(expr) => {
                    return Ok(Some(transform_loop(l, rest, ret_ty, tail)?));
                }
                Expr::Match(m)
                    if expr_has_suspend(expr)
                        || rest.iter().any(stmt_has_suspend)
                        || !rest.is_empty()
                        || matches!(tail, Tail::Cont(_)) =>
                {
                    return Ok(Some(transform_match(m, rest, ret_ty, tail)?));
                }
                Expr::Return(r) => {
                    return Ok(Some(transform_return(r, ret_ty)?));
                }
                _ => {}
            }

            if let Some(kind) = suspend_kind(expr) {
                let then_body = if semi.is_some() || !rest.is_empty() {
                    let rest_body = transform_stmts(rest, ret_ty, tail.clone())?;
                    quote! {
                        let _ = c.result();
                        #rest_body
                    }
                } else {
                    match tail {
                        Tail::Finish => quote! {
                            return ::capy_sni::__cps::finish::<#ret_ty>(c, c.result());
                        },
                        Tail::Cont(after) => quote! {
                            let _ = c.result();
                            #after
                        },
                    }
                };
                let live = live_refs_across(live_before, rest);
                return Ok(Some(expand_suspend(
                    kind, expr, None, then_body, ret_ty, &live,
                )?));
            }

            if expr_has_suspend(expr) {
                if semi.is_none() && rest.is_empty() && matches!(tail, Tail::Finish) {
                    return Ok(Some(transform_tail_expr(expr, ret_ty)?));
                }
                return Err(quote! {
                    compile_error!(
                        "#[cps] suspend must be a full statement / let-rhs / tail expression"
                    );
                });
            }

            Ok(None)
        }
        _ => Ok(None),
    }
}

fn is_ref_ty(ty: &Type) -> bool {
    match ty {
        Type::Path(p) => p.path.segments.last().is_some_and(|s| s.ident == "Ref"),
        Type::Reference(r) => is_ref_ty(&r.elem),
        _ => false,
    }
}

fn push_pat_idents(pat: &Pat, out: &mut Vec<Ident>) {
    match pat {
        Pat::Ident(i) => out.push(i.ident.clone()),
        Pat::Type(t) => push_pat_idents(&t.pat, out),
        Pat::Tuple(t) => {
            for p in &t.elems {
                push_pat_idents(p, out);
            }
        }
        Pat::TupleStruct(t) => {
            for p in &t.elems {
                push_pat_idents(p, out);
            }
        }
        Pat::Struct(s) => {
            for f in &s.fields {
                push_pat_idents(&f.pat, out);
            }
        }
        Pat::Or(o) => {
            for p in &o.cases {
                push_pat_idents(p, out);
            }
        }
        Pat::Reference(r) => push_pat_idents(&r.pat, out),
        Pat::Paren(p) => push_pat_idents(&p.pat, out),
        _ => {}
    }
}

fn is_likely_ref_producing_expr(expr: &Expr) -> bool {
    if suspend_kind(expr).is_some() {
        return true;
    }
    match expr {
        Expr::MethodCall(m) => {
            let name = m.method.to_string();
            matches!(
                name.as_str(),
                "public_ref"
                    | "private_ref"
                    | "fixnum"
                    | "flonum"
                    | "bool"
                    | "null"
                    | "unspecified"
                    | "new_local_ref"
                    | "intern_symbol"
                    | "cons"
                    | "car"
                    | "cdr"
                    | "vector_ref"
                    | "make_vector"
                    | "list"
            ) || name.starts_with("make_")
                || name.ends_with("_ref")
        }
        Expr::Call(c) => {
            if let Expr::Path(p) = c.func.as_ref() {
                path_is_cps_op(&p.path, "call") || path_is_cps_op(&p.path, "raise")
            } else {
                false
            }
        }
        Expr::Try(t) => is_likely_ref_producing_expr(&t.expr),
        Expr::Paren(p) => is_likely_ref_producing_expr(&p.expr),
        Expr::Reference(r) => is_likely_ref_producing_expr(&r.expr),
        _ => false,
    }
}

fn collect_ref_bindings_from_stmt(stmt: &Stmt, out: &mut Vec<Ident>) {
    match stmt {
        Stmt::Local(local) => {
            let mut add = false;
            if let Pat::Type(pt) = &local.pat
                && is_ref_ty(&pt.ty)
            {
                add = true;
            }
            if let Some(init) = &local.init
                && is_likely_ref_producing_expr(&init.expr)
            {
                add = true;
            }
            if add {
                push_pat_idents(&local.pat, out);
            }
        }
        Stmt::Expr(Expr::If(i), _) => {
            if let Expr::Let(el) = i.cond.as_ref() {
                if is_likely_ref_producing_expr(&el.expr)
                    || matches!(&*el.expr, Expr::MethodCall(_))
                {
                    push_pat_idents(&el.pat, out);
                }
            }
        }
        _ => {}
    }
}

fn idents_used_in_stmts(stmts: &[Stmt]) -> Vec<Ident> {
    struct Collect(Vec<Ident>);
    impl<'ast> Visit<'ast> for Collect {
        fn visit_path(&mut self, p: &'ast syn::Path) {
            if p.segments.len() == 1
                && let Some(s) = p.segments.first()
            {
                self.0.push(s.ident.clone());
            }
            syn::visit::visit_path(self, p);
        }
    }
    let mut c = Collect(Vec::new());
    for s in stmts {
        c.visit_stmt(s);
    }
    c.0
}

fn live_refs_across(known: &[Ident], rest: &[Stmt]) -> Vec<Ident> {
    let mut used = idents_used_in_stmts(rest);
    used.sort_by_key(|i| i.to_string());
    used.dedup_by_key(|i| i.to_string());
    known
        .iter()
        .filter(|k| used.iter().any(|u| u == *k))
        .cloned()
        .collect()
}

fn root_reload_ts(live: &[Ident], ret_ty: &TokenStream) -> (TokenStream, TokenStream) {
    if live.is_empty() {
        return (quote! {}, quote! {});
    }
    let slots: Vec<_> = live
        .iter()
        .enumerate()
        .map(|(i, _)| format_ident!("__cps_root_{i}"))
        .collect();
    let root = quote! {
        #(
            let #slots = ::capy_sni::__cps::root_ref::<#ret_ty>(env, c, #live);
        )*
    };
    let reload = quote! {
        #(
            let #live = unsafe {
                ::capy_sni::__cps::reload_ref::<#ret_ty>(env, c, #slots)
            };
        )*
    };
    (root, reload)
}

fn expand_suspend(
    kind: SuspendKind,
    expr: &Expr,
    bind: Option<&Pat>,
    then_body: TokenStream,
    ret_ty: &TokenStream,
    live_refs: &[Ident],
) -> Result<TokenStream, TokenStream> {
    let Expr::Call(call) = expr else {
        return Err(quote! { compile_error!("internal: expected call"); });
    };

    let (root_ts, reload_ts) = root_reload_ts(live_refs, ret_ty);

    match kind {
        SuspendKind::Call => {
            if call.args.len() != 2 {
                return Err(quote! {
                    compile_error!("cps::call expects (proc, &[args])");
                });
            }
            let proc = &call.args[0];
            let args = &call.args[1];
            let bind_ts = if let Some(pat) = bind {
                quote! { let #pat = unsafe { ::capy_sni::Ref::from_raw(c.result()) }; }
            } else {
                quote! { let _ = c.result(); }
            };
            Ok(quote! {
                {
                    let __cps_proc: ::capy_sni::Ref<'_> = #proc;
                    let __cps_args: &[::capy_sni::Ref<'_>] = #args;
                    #root_ts
                    return ::capy_sni::__cps::suspend_call::<#ret_ty>(
                        env,
                        __cps_proc,
                        __cps_args,
                        c,
                        ::std::boxed::Box::new(move |env, c| {
                            #reload_ts
                            #bind_ts
                            #then_body
                        }),
                    );
                }
            })
        }
        SuspendKind::Raise => {
            if call.args.len() != 1 {
                return Err(quote! {
                    compile_error!("cps::raise expects (obj)");
                });
            }
            let obj = &call.args[0];
            let bind_ts = if let Some(pat) = bind {
                quote! { let #pat = unsafe { ::capy_sni::Ref::from_raw(c.result()) }; }
            } else {
                quote! { let _ = c.result(); }
            };
            Ok(quote! {
                {
                    let __cps_obj: ::capy_sni::Ref<'_> = #obj;
                    #root_ts
                    return ::capy_sni::__cps::suspend_raise::<#ret_ty>(
                        env,
                        __cps_obj,
                        c,
                        ::std::boxed::Box::new(move |env, c| {
                            #reload_ts
                            #bind_ts
                            #then_body
                        }),
                    );
                }
            })
        }
        SuspendKind::Guard => expand_guard(call, bind, then_body, ret_ty, live_refs),
    }
}

fn expand_guard(
    call: &ExprCall,
    bind: Option<&Pat>,
    then_body: TokenStream,
    outer_ret_ty: &TokenStream,
    live_refs: &[Ident],
) -> Result<TokenStream, TokenStream> {
    if call.args.len() != 2 {
        return Err(quote! {
            compile_error!("cps::guard expects (handler, body) closures");
        });
    }
    let handler = &call.args[0];
    let body = &call.args[1];

    let (handler_pat, handler_expr, handler_ret) = match handler {
        Expr::Closure(c) => {
            let pat = c
                .inputs
                .first()
                .cloned()
                .unwrap_or_else(|| parse_quote! { _exn });
            let ret = match &c.output {
                ReturnType::Type(_, ty) => quote! { #ty },
                ReturnType::Default => {
                    return Err(quote! {
                        compile_error!(
                            "cps::guard handler must have an explicit return type, e.g. |exn| -> i32 { ... }"
                        );
                    });
                }
            };
            (pat, c.body.as_ref().clone(), ret)
        }
        _ => {
            return Err(quote! {
                compile_error!("cps::guard handler must be a closure |exn| ...");
            });
        }
    };

    let (body_expr, body_ret) = match body {
        Expr::Closure(c) => {
            let ret = match &c.output {
                ReturnType::Type(_, ty) => quote! { #ty },
                ReturnType::Default => {
                    return Err(quote! {
                        compile_error!(
                            "cps::guard body must have an explicit return type, e.g. || -> i32 { ... }"
                        );
                    });
                }
            };
            (c.body.as_ref().clone(), ret)
        }
        _ => {
            return Err(quote! {
                compile_error!("cps::guard body must be a closure || ...");
            });
        }
    };

    let handler_block = expr_as_block(&handler_expr);
    let body_block = expr_as_block(&body_expr);
    let handler_ts = transform_block(&handler_block, &handler_ret)?;
    let body_ts = transform_block(&body_block, &body_ret)?;

    let bind_ts = if let Some(pat) = bind {
        quote! { let #pat = __guard_result; }
    } else {
        quote! { let __guard_result = __guard_result; }
    };

    let (root_ts, reload_ts) = root_reload_ts(live_refs, outer_ret_ty);

    // Nested `__cps::run` under `with_exception_handler` (same shape as
    // `sni_guard_k`) while keeping typed `finish`. Live outer Refs are rooted.
    Ok(quote! {
        {
            #root_ts
            let __guard_result = env.with_exception_handler(
                |env, #handler_pat| {
                    #reload_ts
                    let __v = ::capy_sni::__cps::run::<#handler_ret>(
                        env,
                        ::std::boxed::Box::new(move |env, c| {
                            #handler_ts
                        }),
                    );
                    ::capy_sni::IntoScm::into_scm(__v, env)
                },
                |env| {
                    #reload_ts
                    ::capy_sni::__cps::run::<#body_ret>(
                        env,
                        ::std::boxed::Box::new(move |env, c| {
                            #body_ts
                        }),
                    )
                },
            );
            #bind_ts
            #then_body
        }
    })
}

fn expr_as_block(expr: &Expr) -> syn::Block {
    match expr {
        Expr::Block(b) => b.block.clone(),
        other => parse_quote! {{ #other }},
    }
}

fn transform_tail_expr(expr: &Expr, ret_ty: &TokenStream) -> Result<TokenStream, TokenStream> {
    if let Some(kind) = suspend_kind(expr) {
        return expand_suspend(
            kind,
            expr,
            None,
            quote! {
                return ::capy_sni::__cps::finish::<#ret_ty>(c, c.result());
            },
            ret_ty,
            &[],
        );
    }
    match expr {
        Expr::If(i) => transform_if(i, &[], ret_ty, &Tail::Finish),
        Expr::Match(m) => transform_match(m, &[], ret_ty, &Tail::Finish),
        Expr::While(w) => transform_while(w, &[], ret_ty, &Tail::Finish),
        Expr::Loop(l) => transform_loop(l, &[], ret_ty, &Tail::Finish),
        Expr::Block(b) => transform_stmts(&b.block.stmts, ret_ty, Tail::Finish),
        Expr::Return(r) => transform_return(r, ret_ty),
        _ => {
            if expr_has_suspend(expr) {
                Err(quote! {
                    compile_error!("#[cps] unsupported suspend nesting in expression");
                })
            } else {
                Ok(quote! {
                    return ::capy_sni::__cps::finish::<#ret_ty>(c, #expr);
                })
            }
        }
    }
}

fn transform_return(r: &ExprReturn, ret_ty: &TokenStream) -> Result<TokenStream, TokenStream> {
    match &r.expr {
        Some(e) => {
            if let Some(kind) = suspend_kind(e) {
                expand_suspend(
                    kind,
                    e,
                    None,
                    quote! {
                        return ::capy_sni::__cps::finish::<#ret_ty>(c, c.result());
                    },
                    ret_ty,
                    &[],
                )
            } else if expr_has_suspend(e) {
                Err(quote! {
                    compile_error!("#[cps] return value suspend must be cps::call/raise/guard");
                })
            } else {
                Ok(quote! {
                    return ::capy_sni::__cps::finish::<#ret_ty>(c, #e);
                })
            }
        }
        None => Ok(quote! {
            return ::capy_sni::__cps::finish::<#ret_ty>(c, ());
        }),
    }
}

fn make_join(rest: &[Stmt], ret_ty: &TokenStream, tail: &Tail) -> Result<TokenStream, TokenStream> {
    transform_stmts(rest, ret_ty, tail.clone())
}

fn transform_if(
    i: &ExprIf,
    rest: &[Stmt],
    ret_ty: &TokenStream,
    tail: &Tail,
) -> Result<TokenStream, TokenStream> {
    if expr_has_suspend(&i.cond) {
        return Err(quote! {
            compile_error!("#[cps] does not support suspend in if conditions; bind first");
        });
    }
    let join = make_join(rest, ret_ty, tail)?;
    let branch_tail = Tail::Cont(join);
    let cond = &i.cond;

    let mut then_seed = Vec::new();
    if let Expr::Let(el) = i.cond.as_ref() {
        if is_likely_ref_producing_expr(&el.expr) || matches!(&*el.expr, Expr::MethodCall(_)) {
            push_pat_idents(&el.pat, &mut then_seed);
        }
    }

    let then_body =
        transform_stmts_seeded(&i.then_branch.stmts, ret_ty, branch_tail.clone(), then_seed)?;

    let else_body = if let Some((_, else_expr)) = &i.else_branch {
        match else_expr.as_ref() {
            Expr::Block(b) => transform_stmts(&b.block.stmts, ret_ty, branch_tail)?,
            Expr::If(nested) => transform_if(nested, &[], ret_ty, &branch_tail)?,
            other => {
                if expr_has_suspend(other) {
                    return Err(quote! {
                        compile_error!("#[cps] unsupported else expression with suspend");
                    });
                }
                let after = match &branch_tail {
                    Tail::Cont(ts) => ts.clone(),
                    Tail::Finish => quote! {
                        return ::capy_sni::__cps::finish::<#ret_ty>(c, #other);
                    },
                };
                quote! {{
                    let _ = #other;
                    #after
                }}
            }
        }
    } else {
        match &branch_tail {
            Tail::Cont(ts) => ts.clone(),
            Tail::Finish => quote! {
                return ::capy_sni::__cps::finish::<#ret_ty>(c, ());
            },
        }
    };

    Ok(quote! {
        if #cond {
            #then_body
        } else {
            #else_body
        }
    })
}

fn transform_while(
    w: &ExprWhile,
    rest: &[Stmt],
    ret_ty: &TokenStream,
    tail: &Tail,
) -> Result<TokenStream, TokenStream> {
    if expr_has_suspend(&w.cond) {
        return Err(quote! {
            compile_error!("#[cps] does not support suspend in while conditions; bind first");
        });
    }
    let join = make_join(rest, ret_ty, tail)?;
    let cond = &w.cond;
    let body_ts = transform_stmts_in_loop(&w.body.stmts, ret_ty)?;

    Ok(quote! {
        {
            let __cps_after = ::capy_sni::__cps::new_slot();
            let __cps_header = ::capy_sni::__cps::new_slot();
            let __cps_after2 = __cps_after.clone();
            let __cps_header2 = __cps_header.clone();
            ::capy_sni::__cps::set_slot(
                &__cps_after,
                ::std::boxed::Box::new(move |env, c| {
                    #join
                }),
            );
            ::capy_sni::__cps::set_slot(
                &__cps_header,
                ::std::boxed::Box::new(move |env, c| {
                    if !(#cond) {
                        return ::capy_sni::__cps::yield_to::<#ret_ty>(c, &__cps_after2);
                    }
                    #body_ts
                    return ::capy_sni::__cps::yield_to::<#ret_ty>(c, &__cps_header2);
                }),
            );
            return ::capy_sni::__cps::yield_to::<#ret_ty>(c, &__cps_header);
        }
    })
}

fn transform_loop(
    l: &ExprLoop,
    rest: &[Stmt],
    ret_ty: &TokenStream,
    tail: &Tail,
) -> Result<TokenStream, TokenStream> {
    let join = make_join(rest, ret_ty, tail)?;
    let body_ts = transform_stmts_in_loop(&l.body.stmts, ret_ty)?;

    Ok(quote! {
        {
            let __cps_after = ::capy_sni::__cps::new_slot();
            let __cps_header = ::capy_sni::__cps::new_slot();
            let __cps_after2 = __cps_after.clone();
            let __cps_header2 = __cps_header.clone();
            ::capy_sni::__cps::set_slot(
                &__cps_after,
                ::std::boxed::Box::new(move |env, c| {
                    #join
                }),
            );
            ::capy_sni::__cps::set_slot(
                &__cps_header,
                ::std::boxed::Box::new(move |env, c| {
                    let _ = &__cps_after2;
                    #body_ts
                    return ::capy_sni::__cps::yield_to::<#ret_ty>(c, &__cps_header2);
                }),
            );
            return ::capy_sni::__cps::yield_to::<#ret_ty>(c, &__cps_header);
        }
    })
}

fn transform_stmts_in_loop(
    stmts: &[Stmt],
    ret_ty: &TokenStream,
) -> Result<TokenStream, TokenStream> {
    transform_stmts_in_loop_inner(stmts, ret_ty)
}

fn transform_stmts_in_loop_inner(
    stmts: &[Stmt],
    ret_ty: &TokenStream,
) -> Result<TokenStream, TokenStream> {
    if stmts.is_empty() {
        return Ok(quote! {});
    }
    for (i, stmt) in stmts.iter().enumerate() {
        if let Stmt::Expr(Expr::Break(b), _) = stmt {
            if b.expr.is_some() {
                return Err(quote! {
                    compile_error!("#[cps] break with value is not supported yet");
                });
            }
            let prefix = &stmts[..i];
            return Ok(quote! {
                #(#prefix)*
                return ::capy_sni::__cps::yield_to::<#ret_ty>(c, &__cps_after2);
            });
        }
        if let Stmt::Expr(Expr::Continue(_), _) = stmt {
            let prefix = &stmts[..i];
            return Ok(quote! {
                #(#prefix)*
                return ::capy_sni::__cps::yield_to::<#ret_ty>(c, &__cps_header2);
            });
        }

        if let Stmt::Local(local) = stmt
            && let Some(init) = &local.init
        {
            if let Some(kind) = suspend_kind(&init.expr) {
                let pat = &local.pat;
                let then = transform_stmts_in_loop_inner(&stmts[i + 1..], ret_ty)?;
                let prefix = &stmts[..i];
                let sus = expand_suspend(kind, &init.expr, Some(pat), then, ret_ty, &[])?;
                return Ok(quote! {
                    #(#prefix)*
                    #sus
                });
            }
            if expr_has_suspend(&init.expr) {
                return Err(quote! {
                    compile_error!("#[cps] suspend must be entire let initializer in loop");
                });
            }
        }

        if let Stmt::Expr(expr, _) = stmt {
            if let Some(kind) = suspend_kind(expr) {
                let then = transform_stmts_in_loop_inner(&stmts[i + 1..], ret_ty)?;
                let prefix = &stmts[..i];
                let sus = expand_suspend(
                    kind,
                    expr,
                    None,
                    quote! { let _ = c.result(); #then },
                    ret_ty,
                    &[],
                )?;
                return Ok(quote! {
                    #(#prefix)*
                    #sus
                });
            }
            if let Expr::If(iff) = expr
                && (expr_has_suspend(expr)
                    || stmts[i + 1..].iter().any(|s| {
                        matches!(s, Stmt::Expr(Expr::Break(_) | Expr::Continue(_), _))
                            || stmt_has_suspend(s)
                    }))
            {
                let rest = &stmts[i + 1..];
                let join = transform_stmts_in_loop_inner(rest, ret_ty)?;
                let cond = &iff.cond;
                if expr_has_suspend(cond) {
                    return Err(quote! {
                        compile_error!("#[cps] no suspend in if condition");
                    });
                }
                let then_inner =
                    transform_stmts(&iff.then_branch.stmts, ret_ty, Tail::Cont(join.clone()))?;
                let else_inner = if let Some((_, e)) = &iff.else_branch {
                    match e.as_ref() {
                        Expr::Block(b) => {
                            transform_stmts(&b.block.stmts, ret_ty, Tail::Cont(join.clone()))?
                        }
                        _ => quote! { let _ = #e; #join },
                    }
                } else {
                    join
                };
                let prefix = &stmts[..i];
                return Ok(quote! {
                    #(#prefix)*
                    if #cond {
                        #then_inner
                    } else {
                        #else_inner
                    }
                });
            }
        }
    }
    Ok(quote! { #(#stmts)* })
}

fn transform_match(
    m: &ExprMatch,
    rest: &[Stmt],
    ret_ty: &TokenStream,
    tail: &Tail,
) -> Result<TokenStream, TokenStream> {
    if expr_has_suspend(&m.expr) {
        return Err(quote! {
            compile_error!("#[cps] does not support suspend in match scrutinee; bind first");
        });
    }
    let join = make_join(rest, ret_ty, tail)?;
    let branch_tail = Tail::Cont(join);
    let scrutinee = &m.expr;
    let mut arm_ts = Vec::new();
    for arm in &m.arms {
        if let Some((_, g)) = &arm.guard
            && expr_has_suspend(g)
        {
            return Err(quote! {
                compile_error!("#[cps] no suspend in match guards");
            });
        }
        let pat = &arm.pat;
        let guard = &arm.guard;
        let body = arm.body.as_ref();
        let body_ts = match body {
            Expr::Block(b) => transform_stmts(&b.block.stmts, ret_ty, branch_tail.clone())?,
            other => {
                if let Some(kind) = suspend_kind(other) {
                    let then = match &branch_tail {
                        Tail::Finish => quote! {
                            return ::capy_sni::__cps::finish::<#ret_ty>(c, c.result());
                        },
                        Tail::Cont(after) => quote! {
                            let __m = c.result();
                            let _ = __m;
                            #after
                        },
                    };
                    expand_suspend(kind, other, None, then, ret_ty, &[])?
                } else if expr_has_suspend(other) {
                    return Err(quote! {
                        compile_error!("#[cps] unsupported match arm expression");
                    });
                } else {
                    match &branch_tail {
                        Tail::Finish => quote! {
                            return ::capy_sni::__cps::finish::<#ret_ty>(c, #other);
                        },
                        Tail::Cont(after) => quote! {{
                            let _ = #other;
                            #after
                        }},
                    }
                }
            }
        };
        let guard_ts = if let Some((if_token, g)) = guard {
            quote! { #if_token #g }
        } else {
            quote! {}
        };
        arm_ts.push(quote! {
            #pat #guard_ts => { #body_ts }
        });
    }
    Ok(quote! {
        match #scrutinee {
            #(#arm_ts,)*
        }
    })
}
