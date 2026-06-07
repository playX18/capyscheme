//! Compile TreeIL to CPS terms that are ready to be compiled to bytecode.
//!
//! Unlike `compile_cps`, this module keeps high-level primitive calls intact.
//! The bytecode compiler can then map operations like `string?`, `car`, or
//! `vector-ref` to bytecode instructions without first seeing low-level
//! primitive expansions.

use crate::cps::builder::CPSBuilder;
use crate::cps::term::{Atom, BranchHint, Cont, Func, FuncRef, Term, TermRef};
use crate::expander::core::{
    LVarRef, LetStyle, Proc, TermKind, TermRef as CoreTermRef, seq_from_slice,
};
use crate::rsgc::alloc::array::Array;
use crate::rsgc::cell::Lock;
use crate::rsgc::{Gc, barrier};
use crate::runtime::Context;
use crate::runtime::value::Value;
use crate::{list, with_cps};
use std::cell::Cell;

type ArgContinuation<'a, 'gc> =
    Box<dyn FnOnce(&mut CPSBuilder<'gc>, Atom<'gc>) -> TermRef<'gc> + 'a>;
type ArgsContinuation<'a, 'gc> =
    Box<dyn FnOnce(&mut CPSBuilder<'gc>, Vec<Atom<'gc>>) -> TermRef<'gc> + 'a>;

pub fn cps_func<'gc>(
    builder: &mut CPSBuilder<'gc>,
    proc: &Proc<'gc>,
    binding: LVarRef<'gc>,
) -> FuncRef<'gc> {
    let old_meta = builder.current_meta;
    builder.current_meta = proc.meta;
    let return_cont = builder.fresh_variable("return");

    let body = convert(builder, proc.body, return_cont);
    builder.current_meta = old_meta;
    Gc::new(
        *builder.ctx,
        Func {
            meta: proc.meta,
            args: proc.args,
            name: proc.name,

            binding,
            source: proc.source,
            return_cont,
            variadic: proc.variadic,
            body: Lock::new(body),
            free_vars: Lock::new(None),
        },
    )
}

pub fn cps_toplevel<'gc>(ctx: Context<'gc>, forms: &[CoreTermRef<'gc>]) -> FuncRef<'gc> {
    let form = if forms.len() == 1 {
        forms[0]
    } else if forms.is_empty() {
        panic!("Cannot compile an empty program to CPS")
    } else {
        let source = forms
            .iter()
            .find(|f| f.source() != Value::new(false))
            .map(|f| f.source())
            .unwrap_or(Value::new(false));

        let seq = seq_from_slice(ctx, forms);
        barrier::field!(Gc::write(*ctx, seq), super::core::Term, source)
            .unlock()
            .set(source);
        seq
    };

    let proc = Proc {
        args: Array::from_slice(*ctx, []),
        name: Value::new(false),
        body: form,
        source: form.source(),
        variadic: None,
        meta: Value::null(),
    };

    let mut builder = CPSBuilder::new(ctx);

    let bind = builder.fresh_variable("toplevel");

    cps_func(&mut builder, &proc, bind)
}

pub fn toplevel_box<'gc>(
    cps: &mut CPSBuilder<'gc>,
    src: Value<'gc>,
    name: Value<'gc>,
    bound: bool,
    have_var: impl FnOnce(&mut CPSBuilder<'gc>, Atom<'gc>) -> TermRef<'gc>,
) -> TermRef<'gc> {
    match cps.current_topbox_scope {
        None => {
            if bound {
                with_cps!(cps;
                    let module = #% "current-module" () @ src;
                    let variable = #% "lookup-bound" (Atom::Local(module), Atom::Constant(name)) @ src;
                    # have_var(cps, Atom::Local(variable))
                )
            } else {
                with_cps!(cps;
                    let module = #% "current-module" () @ src;
                    let variable = #% "lookup" (Atom::Local(module), Atom::Constant(name)) @ src;
                    # have_var(cps, Atom::Local(variable))
                )
            }
        }

        Some(scope) => {
            with_cps!(cps;
                letk kbox (box_) = have_var(cps, Atom::Local(box_));
                # cached_toplevel_box(cps, kbox, src, (scope as i32).into(), name, bound)
            )
        }
    }
}

pub fn module_box<'gc>(
    cps: &mut CPSBuilder<'gc>,
    val_proc: impl FnOnce(&mut CPSBuilder<'gc>, Atom<'gc>) -> TermRef<'gc>,
    module: Value<'gc>,
    name: Value<'gc>,
    public: bool,
    bound: bool,
    src: Value<'gc>,
) -> TermRef<'gc> {
    let _ = bound;
    with_cps!(cps;
        letk kbox (var) = val_proc(cps, Atom::Local(var));
        # cached_module_box(cps, kbox, src, module, name, public,)
    )
}

pub fn capture_toplevel_scope<'gc>(
    cps: &mut CPSBuilder<'gc>,
    src: Value<'gc>,
    scope_id: u32,
    fk: impl FnOnce(&mut CPSBuilder<'gc>) -> TermRef<'gc>,
) -> TermRef<'gc> {
    with_cps!(cps;
        let module = #% "current-module" () @ src;
        # cache_current_module(cps, fk, src, Atom::Constant(Value::new(scope_id as i32)), Atom::Local(module))
    )
}

pub fn cached_toplevel_box<'gc>(
    cps: &mut CPSBuilder<'gc>,
    k: LVarRef<'gc>,
    src: Value<'gc>,
    scope: Value<'gc>,
    name: Value<'gc>,
    bound: bool,
) -> TermRef<'gc> {
    let cache_key = Value::cons(cps.ctx, scope, name);
    with_cps!(cps;
        let cached = #% "cache-ref" (cache_key) @ src;
        let is_heap_obj = #% "heap-object?" (cached) @ src;
        // do not inline results of the cache lookup, this can blow up the code size
        letk noinline merge (cached) = with_cps!(cps; continue k (cached));
        letk cold kinit () = with_cps!(cps;
            let module = #%"cache-ref" (Atom::Constant(scope)) @ src;
            # reify_lookup(cps, src, module, name, bound, |cps, var| {
                with_cps!(cps;
                    let _k = #%"cache-set!"(cache_key, var) @ src;
                    continue merge(var)
                )
            })
        );
        letk kok () = with_cps!(cps; continue merge (cached));
        if is_heap_obj => kok | kinit
    )
}

pub fn cached_module_box<'gc>(
    cps: &mut CPSBuilder<'gc>,
    k: LVarRef<'gc>,
    src: Value<'gc>,
    module: Value<'gc>,
    name: Value<'gc>,
    public: bool,
) -> TermRef<'gc> {
    let cache_key = Value::cons(cps.ctx, module, Value::cons(cps.ctx, name, public.into()));

    with_cps!(cps;
        let cache_entry = #% "cache-ref" (cache_key) @ src;
        let is_heap_obj = #% "heap-object?" (cache_entry) @ src;
        // do not inline results of the cache lookup, this can blow up the code size
        letk noinline merge (cached) = with_cps!(cps; continue k (cached));
        letk cold kinit () = if public {
            with_cps!(cps;
                let var = #% "lookup-bound-public" (Atom::Constant(module), Atom::Constant(name)) @ src;
                let _k = #% "cache-set!" (cache_key, Atom::Local(var)) @ src;
                continue merge (Atom::Local(var))
            )
        } else {
            with_cps!(cps;
                let var = #% "lookup-bound-private" (Atom::Constant(module), Atom::Constant(name)) @ src;
                let _k = #% "cache-set!" (cache_key, Atom::Local(var)) @ src;
                continue merge (Atom::Local(var))
            )
        };

        letk kok () = with_cps!(cps; continue merge (cache_entry));

        if is_heap_obj => kok | kinit
    )
}

pub fn cached_module_boxk<'gc>(
    cps: &mut CPSBuilder<'gc>,
    fk: impl FnOnce(&mut CPSBuilder<'gc>, LVarRef<'gc>) -> TermRef<'gc>,
    src: Value<'gc>,
    module: Value<'gc>,
    name: Value<'gc>,
    public: bool,
) -> TermRef<'gc> {
    with_cps!(cps;
        letk kbox (var) = fk(cps, var);
        # cached_toplevel_box(cps, kbox, src, module, name, public)
    )
}

pub fn cache_current_module<'gc>(
    cps: &mut CPSBuilder<'gc>,
    fk: impl FnOnce(&mut CPSBuilder<'gc>) -> TermRef<'gc>,
    src: Value<'gc>,
    scope: Atom<'gc>,
    module: Atom<'gc>,
) -> TermRef<'gc> {
    with_cps!(cps;
        let _k = #% "cache-set!" (scope, module) @ src;
        # fk(cps)
    )
}

pub fn reify_lookup<'gc>(
    cps: &mut CPSBuilder<'gc>,
    src: Value<'gc>,
    mod_var: LVarRef<'gc>,
    name: Value<'gc>,
    assert_bound: bool,
    have_var: impl FnOnce(&mut CPSBuilder<'gc>, Atom<'gc>) -> TermRef<'gc>,
) -> TermRef<'gc> {
    if assert_bound {
        with_cps!(cps;
            let variable = #%"lookup-bound" (Atom::Local(mod_var), Atom::Constant(name)) @ src;
            #have_var(cps, Atom::Local(variable))
        )
    } else {
        with_cps!(cps;
            let variable = #%"lookup" (Atom::Local(mod_var), Atom::Constant(name)) @ src;
            #have_var(cps, Atom::Local(variable))
        )
    }
}

pub fn reify_resolve_module<'gc>(
    cps: &mut CPSBuilder<'gc>,
    k: LVarRef<'gc>,
    src: Value<'gc>,
    module: Value<'gc>,
    public: bool,
) -> TermRef<'gc> {
    with_cps!(cps;
        let resolved = #% "resolve-module" (Atom::Constant(public.into()), Atom::Constant(module)) @ src;
        continue k (resolved)
    )
}

fn is_zero_valued<'gc>(exp: CoreTermRef<'gc>) -> bool {
    match &exp.kind {
        TermKind::Values(vals) if vals.is_empty() => true,
        TermKind::ToplevelSet(_, _, _)
        | TermKind::ModuleSet(_, _, _, _)
        | TermKind::Define(_, _, _) => true,
        TermKind::Seq(_, tail) => is_zero_valued(*tail),
        TermKind::Receive(_, _, _, body) => is_zero_valued(*body),
        TermKind::Let(let_) => is_zero_valued(let_.body),
        _ => false,
    }
}

fn is_single_valued<'gc>(exp: CoreTermRef<'gc>) -> bool {
    match &exp.kind {
        TermKind::Values(vals) if vals.len() == 1 => true,
        TermKind::ToplevelRef(..) | TermKind::ModuleRef(..) | TermKind::LRef(..) => true,
        TermKind::Seq(_, tail) => is_single_valued(*tail),
        TermKind::Receive(_, _, _, body) => is_single_valued(*body),
        TermKind::Let(let_) => is_single_valued(let_.body),
        TermKind::Const(_) => true,
        _ => false,
    }
}

pub fn convert_arg<'gc, 'a>(
    cps: &mut CPSBuilder<'gc>,
    exp: CoreTermRef<'gc>,
    k: ArgContinuation<'a, 'gc>,
) -> TermRef<'gc> {
    let src = exp.source();
    match exp.kind {
        TermKind::LRef(var) => k(cps, Atom::Local(var)),

        _ if is_single_valued(exp) => {
            with_cps!(cps;
                letk karg (arg) @ src = k(cps, Atom::Local(arg));
                # {
                    convert(cps, exp, karg, )
            }
            )
        }

        _ => {
            with_cps!(cps;
                letk karg (arg & rest) @ src = k(cps, Atom::Local(arg));
                # convert(cps, exp, karg, )
            )
        }
    }
}

pub fn convert_args<'gc, 'a>(
    cps: &mut CPSBuilder<'gc>,
    exps: &'a [CoreTermRef<'gc>],
    fk: ArgsContinuation<'a, 'gc>,
) -> TermRef<'gc> {
    if exps.is_empty() {
        return fk(cps, Vec::new());
    }

    let exp = exps[0];
    let exps = &exps[1..];
    exps.is_empty();
    convert_arg(
        cps,
        exp,
        Box::new(move |cps, arg| {
            convert_args(
                cps,
                exps,
                Box::new(move |cps, mut args| {
                    let mut all_args = Vec::with_capacity(args.len() + 1);
                    all_args.push(arg);
                    all_args.append(&mut args);
                    fk(cps, all_args)
                }),
            )
        }),
    )
}

pub fn convert<'gc>(
    cps: &mut CPSBuilder<'gc>,
    exp: CoreTermRef<'gc>,
    k: LVarRef<'gc>,
) -> TermRef<'gc> {
    let src = exp.source();

    match &exp.kind {
        TermKind::Values(vals) => convert_args(
            cps,
            vals,
            Box::new(move |cps, args| {
                with_cps!(cps;
                    continue k args ...
                )
            }),
        ),

        TermKind::If(test, consequent, alternate) => convert_arg(
            cps,
            *test,
            Box::new(move |cps, test| {
                with_cps!(cps;
                    letk kconseq () = convert(cps, *consequent, k,);
                    letk kalt () = convert(cps, *alternate, k,);
                    if test => kconseq | kalt
                )
            }),
        ),

        TermKind::LSet(..) => unreachable!(),
        TermKind::LRef(var) => {
            with_cps!(cps;
                continue k (Atom::Local(*var)) @ src
            )
        }

        TermKind::Const(c) => {
            with_cps!(cps;
                continue k (Atom::Constant(*c)) @ src
            )
        }

        TermKind::PrimRef(name) => {
            let module = list!(cps.ctx, cps.ctx.intern("capy"));

            module_box(
                cps,
                |cps, var| {
                    with_cps!(cps;
                        let val = #% "variable-ref" (var) @ src;
                        continue k(Atom::Local(val))
                    )
                },
                module,
                *name,
                true,
                true,
                src,
            )
        }

        TermKind::ToplevelRef(_, name) => toplevel_box(cps, src, *name, true, |cps, var| {
            with_cps!(cps;
                let val = #% "variable-ref" (var) @ src;
                continue k(Atom::Local(val))
            )
        }),

        TermKind::ModuleRef(module, name, public) => module_box(
            cps,
            |cps, var| {
                with_cps!(cps;
                    let val = #% "variable-ref" (var) @ src;
                    continue k(Atom::Local(val))
                )
            },
            *module,
            *name,
            *public,
            false,
            src,
        ),

        TermKind::ToplevelSet(_, name, exp) => convert_arg(
            cps,
            *exp,
            Box::new(move |cps, atom| {
                toplevel_box(cps, src, *name, false, |cps, var| {
                    with_cps!(cps;
                        let _val = #% "variable-set!" (var, atom) @ src;
                        continue k(_val) @ src
                    )
                })
            }),
        ),

        TermKind::ModuleSet(module, name, public, exp) => convert_arg(
            cps,
            *exp,
            Box::new(move |cps, exp| {
                module_box(
                    cps,
                    |cps, var| {
                        with_cps!(cps;
                            let _val = #% "variable-set!" (var, exp) @ src;
                            continue k(_val) @ src
                        )
                    },
                    *module,
                    *name,
                    *public,
                    true,
                    src,
                )
            }),
        ),

        TermKind::Proc(proc) => {
            if cps.current_topbox_scope.is_some() {
                let tmp = cps.fresh_variable("proc");
                let func = cps_func(cps, proc, tmp);
                let body = with_cps!(cps;
                    continue k (Atom::Local(tmp)) @ src
                );

                return Gc::new(
                    *cps.ctx,
                    Term::Fix(Array::from_slice(*cps.ctx, [func]), body),
                );
            }

            let prev = cps.current_topbox_scope;
            cps.enter_scope();
            let id = cps.current_topbox_scope.unwrap();
            capture_toplevel_scope(cps, src, id, |cps| {
                let form = convert(cps, exp, k);
                cps.current_topbox_scope = prev;
                form
            })
        }

        TermKind::Define(_module, name, exp) => convert_arg(
            cps,
            *exp,
            Box::new(move |cps, atom| {
                with_cps! {cps;
                    let _rv = #% "define" (Atom::Constant(*name), atom) @ src;
                     continue k (_rv) @ src
                }
            }),
        ),

        TermKind::Call(proc, args) => convert_arg(
            cps,
            *proc,
            Box::new(move |cps, proc| {
                convert_args(
                    cps,
                    args,
                    Box::new(move |cps, args| {
                        with_cps!(cps;
                            proc(k, args ...) @ src
                        )
                    }),
                )
            }),
        ),

        TermKind::PrimCall(prim, args) => {
            let prim = *prim;
            convert_args(
                cps,
                args,
                Box::new(move |cps, args| {
                    with_cps!(cps;
                        let atom = #% prim args ... @ src;
                        continue k (atom) @ src
                    )
                }),
            )
        }

        TermKind::Let(let_) => {
            assert!(!matches!(
                let_.style,
                LetStyle::LetRec | LetStyle::LetRecStar | LetStyle::LetStar
            ));

            let mut body = convert(cps, let_.body, k);

            for (binding, expr) in let_.lhs.iter().zip(let_.rhs.iter()) {
                let single = Array::from_slice(*cps.ctx, [*binding]);
                if is_single_valued(*expr) {
                    body = with_cps!(cps;
                        letk r#let (single...) @ src = body;
                        # convert(cps, *expr, r#let,)
                    );
                } else {
                    body = with_cps!(cps;
                        letk r#let (single... & rest) @ src = body;
                        # convert(cps, *expr, r#let,)
                    );
                }
            }

            body
        }

        TermKind::Seq(head, etail) => {
            with_cps!(cps;
                letk ktail (&vals) = convert(cps, *etail, k,);
                # convert(cps, *head, ktail,)
            )
        }

        TermKind::Fix(fix) => {
            if cps.current_topbox_scope.is_some() {
                let funcs = fix
                    .lhs
                    .iter()
                    .zip(fix.rhs.iter())
                    .map(|(binding, func)| cps_func(cps, func, *binding))
                    .collect::<Vec<_>>();

                let body = convert(cps, fix.body, k);

                return Gc::new(
                    *cps.ctx,
                    Term::Fix(Array::from_slice(*cps.ctx, &funcs), body),
                );
            }

            let prev = cps.current_topbox_scope;
            cps.enter_scope();
            let id = cps.current_topbox_scope.unwrap();
            capture_toplevel_scope(cps, src, id, |cps| {
                let form = convert(cps, exp, k);
                cps.current_topbox_scope = prev;
                form
            })
        }

        TermKind::Receive(vars, variadic, producer, consumer) => {
            let consumer_k_var = cps.fresh_variable("consumer");

            let consumer_k = Cont {
                meta: cps.current_meta,
                name: Value::new(false),
                binding: consumer_k_var,
                args: *vars,
                variadic: *variadic,
                noinline: false,

                body: Lock::new(convert(cps, *consumer, k)),

                source: src,
                free_vars: Lock::new(None),
                reified: Cell::new(false),
                cold: false,
            };

            let letk_body = convert(cps, *producer, consumer_k_var);
            let consumer_k = Gc::new(*cps.ctx, consumer_k);

            Gc::new(
                *cps.ctx,
                Term::Letk(Array::from_slice(*cps.ctx, [consumer_k]), letk_body),
            )
        }

        TermKind::WithContinuationMark(key, mark, result) => {
            let thunk = Gc::new(
                *cps.ctx,
                Proc {
                    args: Array::from_slice(*cps.ctx, []),
                    name: Value::new(false),
                    source: src,
                    variadic: None,
                    body: *result,
                    meta: cps.current_meta,
                },
            );
            let thunk = Gc::new(
                *cps.ctx,
                crate::expander::core::Term {
                    kind: TermKind::Proc(thunk),
                    source: src.into(),
                },
            );

            let mref = Gc::new(
                *cps.ctx,
                crate::expander::core::Term {
                    kind: TermKind::PrimRef(cps.ctx.intern("call-with-continuation-mark")),
                    source: src.into(),
                },
            );

            let call = Gc::new(
                *cps.ctx,
                crate::expander::core::Term {
                    source: src.into(),
                    kind: TermKind::Call(mref, Array::from_slice(*cps.ctx, [*key, *mark, thunk])),
                },
            );

            convert(cps, call, k)
        }
    }
}
