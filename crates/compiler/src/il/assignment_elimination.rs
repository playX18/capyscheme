//! Assignment Elimination
//!
//! Removes `lset` forms from the IL. Replaces them with boxed values and `unbox`/`box-set!` operations.

use std::collections::HashMap;

use crate::{
    ast::{Datum, INTERNER, P},
    il::term::{Fix, IForm, ITerm, LVar, Let, LetStyle, Proc, ProcCase},
};

pub fn assignment_elimination(form: P<IForm>) -> P<IForm> {
    form.count_refs();
    rewrite(form, &mut HashMap::new())
}

fn rewrite(form: P<IForm>, subst: &mut HashMap<P<LVar>, P<LVar>>) -> P<IForm> {
    match &form.term {
        ITerm::LRef(lvar) => {
            if let Some(lvar) = subst.get(lvar).cloned() {
                let prim = INTERNER.get_or_intern("unbox");

                P(IForm {
                    span: form.span,
                    term: ITerm::PrimApp(
                        prim,
                        vec![P(IForm {
                            span: form.span,
                            term: ITerm::LRef(lvar.clone()),
                        })],
                    ),
                })
            } else {
                form.clone()
            }
        }

        ITerm::LSet(lvar, value) => {
            let lvar = subst.get(lvar).cloned().unwrap_or_else(|| lvar.clone());
            let prim = INTERNER.get_or_intern("box-set!");
            let value = rewrite(value.clone(), subst);
            P(IForm {
                span: form.span,
                term: ITerm::PrimApp(
                    prim,
                    vec![
                        P(IForm {
                            span: form.span,
                            term: ITerm::LRef(lvar.clone()),
                        }),
                        value,
                    ],
                ),
            })
        }

        ITerm::App(proc, args) => {
            let new_args: Vec<P<IForm>> =
                args.iter().map(|arg| rewrite(arg.clone(), subst)).collect();
            P(IForm {
                span: form.span,
                term: ITerm::App(rewrite(proc.clone(), subst), new_args),
            })
        }

        ITerm::Define(var, value) => {
            let new_value = rewrite(value.clone(), subst);
            P(IForm {
                span: form.span,
                term: ITerm::Define(var.clone(), new_value),
            })
        }

        ITerm::Fix(fix) => {
            let funcs = fix
                .procedures
                .iter()
                .map(|proc| {
                    let cases = proc
                        .cases
                        .iter()
                        .map(|case| {
                            let mut mutated_args = case
                                .args
                                .iter()
                                .cloned()
                                .filter(|arg| arg.is_mutated())
                                .collect::<Vec<_>>();
                            if let Some(variadic) =
                                case.variadic.as_ref().filter(|v| v.is_mutated())
                            {
                                mutated_args.push(variadic.clone());
                            }

                            let args = case.args.clone();
                            let variadic = case.variadic.clone();

                            if mutated_args.is_empty() {
                                return ProcCase {
                                    loc: case.loc,
                                    args,
                                    variadic,
                                    body: rewrite(case.body.clone(), subst),
                                };
                            }

                            let boxed_args = mutated_args
                                .iter()
                                .map(|arg| {
                                    LVar::new(
                                        Datum::make_symbol(&format!("&{}", arg.name), None),
                                        None,
                                    )
                                })
                                .collect::<Vec<_>>();
                            let box_init = mutated_args
                                .iter()
                                .map(|arg| {
                                    let prim = INTERNER.get_or_intern("box");
                                    P(IForm {
                                        span: form.span,
                                        term: ITerm::PrimApp(
                                            prim,
                                            vec![P(IForm {
                                                span: form.span,
                                                term: ITerm::LRef(arg.clone()),
                                            })],
                                        ),
                                    })
                                })
                                .collect::<Vec<_>>();

                            subst.extend(
                                mutated_args
                                    .iter()
                                    .zip(boxed_args.iter())
                                    .map(|(old, new)| (old.clone(), new.clone())),
                            );

                            let body = rewrite(case.body.clone(), subst);
                            let let_ = P(IForm {
                                span: form.span,
                                term: ITerm::Let(Let {
                                    style: LetStyle::Let,
                                    variables: boxed_args,
                                    initializers: box_init,
                                    body,
                                }),
                            });

                            ProcCase {
                                loc: case.loc,
                                args,
                                variadic,
                                body: let_,
                            }
                        })
                        .collect::<Vec<ProcCase>>();

                    P(Proc {
                        loc: proc.loc,
                        name: proc.name.clone(),
                        cases,
                    })
                })
                .collect::<Vec<P<Proc>>>();

            let body = rewrite(fix.body.clone(), subst);

            P(IForm {
                span: form.span,
                term: ITerm::Fix(Fix {
                    variables: fix.variables.clone(),
                    procedures: funcs,
                    body,
                }),
            })
        }

        ITerm::Proc(proc) => {
            let cases = proc
                .cases
                .iter()
                .map(|case| {
                    let mut mutated_args = case
                        .args
                        .iter()
                        .cloned()
                        .filter(|arg| arg.is_mutated())
                        .collect::<Vec<_>>();
                    if let Some(variadic) = case.variadic.as_ref().filter(|v| v.is_mutated()) {
                        mutated_args.push(variadic.clone());
                    }

                    let args = case.args.clone();
                    let variadic = case.variadic.clone();

                    if mutated_args.is_empty() {
                        return ProcCase {
                            loc: case.loc,
                            args,
                            variadic,
                            body: rewrite(case.body.clone(), subst),
                        };
                    }

                    let boxed_args = mutated_args
                        .iter()
                        .map(|arg| {
                            LVar::new(Datum::make_symbol(&format!("&{}", arg.name), None), None)
                        })
                        .collect::<Vec<_>>();
                    let box_init = mutated_args
                        .iter()
                        .map(|arg| {
                            let prim = INTERNER.get_or_intern("box");
                            P(IForm {
                                span: form.span,
                                term: ITerm::PrimApp(
                                    prim,
                                    vec![P(IForm {
                                        span: form.span,
                                        term: ITerm::LRef(arg.clone()),
                                    })],
                                ),
                            })
                        })
                        .collect::<Vec<_>>();

                    subst.extend(
                        mutated_args
                            .iter()
                            .zip(boxed_args.iter())
                            .map(|(old, new)| (old.clone(), new.clone())),
                    );

                    let body = rewrite(case.body.clone(), subst);
                    let let_ = P(IForm {
                        span: form.span,
                        term: ITerm::Let(Let {
                            style: LetStyle::Let,
                            variables: boxed_args,
                            initializers: box_init,
                            body,
                        }),
                    });

                    ProcCase {
                        loc: case.loc,
                        args,
                        variadic,
                        body: let_,
                    }
                })
                .collect::<Vec<ProcCase>>();

            let proc = P(Proc {
                loc: proc.loc,
                name: proc.name.clone(),
                cases,
            });

            P(IForm {
                span: form.span,
                term: ITerm::Proc(proc),
            })
        }

        ITerm::Let(let_) => {
            let mutated_vars = let_
                .variables
                .iter()
                .cloned()
                .filter(|var| var.is_mutated())
                .collect::<Vec<_>>();

            if mutated_vars.is_empty() {
                let new_inits = let_
                    .initializers
                    .iter()
                    .map(|init| rewrite(init.clone(), subst))
                    .collect::<Vec<_>>();

                let new_body = rewrite(let_.body.clone(), subst);

                return P(IForm {
                    span: form.span,
                    term: ITerm::Let(Let {
                        style: let_.style,
                        variables: let_.variables.clone(),
                        initializers: new_inits,
                        body: new_body,
                    }),
                });
            }

            let boxed_vars = mutated_vars
                .iter()
                .map(|var| LVar::new(Datum::make_symbol(&format!("&{}", var.name), None), None))
                .collect::<Vec<_>>();

            let box_inits = mutated_vars
                .iter()
                .map(|var| {
                    let prim = INTERNER.get_or_intern("box");
                    P(IForm {
                        span: form.span,
                        term: ITerm::PrimApp(
                            prim,
                            vec![P(IForm {
                                span: form.span,
                                term: ITerm::LRef(var.clone()),
                            })],
                        ),
                    })
                })
                .collect::<Vec<_>>();

            subst.extend(
                mutated_vars
                    .iter()
                    .zip(boxed_vars.iter())
                    .map(|(old, new)| (old.clone(), new.clone())),
            );

            let new_inits = let_
                .initializers
                .iter()
                .map(|init| rewrite(init.clone(), subst))
                .collect::<Vec<_>>();

            let new_body = rewrite(let_.body.clone(), subst);
            let nlet_ = P(IForm {
                span: form.span,
                term: ITerm::Let(Let {
                    style: let_.style,
                    variables: boxed_vars,
                    initializers: box_inits,
                    body: new_body,
                }),
            });

            return P(IForm {
                span: form.span,
                term: ITerm::Let(Let {
                    style: let_.style,
                    variables: let_.variables.clone(),
                    initializers: new_inits,
                    body: nlet_,
                }),
            });
        }
        ITerm::PrimApp(prim, args) => {
            let new_args: Vec<P<IForm>> =
                args.iter().map(|arg| rewrite(arg.clone(), subst)).collect();
            P(IForm {
                span: form.span,
                term: ITerm::PrimApp(prim.clone(), new_args),
            })
        }

        ITerm::GSet(var, val) => {
            let new_val = rewrite(val.clone(), subst);
            P(IForm {
                span: form.span,
                term: ITerm::GSet(var.clone(), new_val),
            })
        }

        ITerm::PrimRef(_) | ITerm::GRef(_) | ITerm::Const(_) => form.clone(),
        ITerm::Seq(seq) => {
            let new_seq = seq
                .iter()
                .map(|item| rewrite(item.clone(), subst))
                .collect::<Vec<P<IForm>>>();
            P(IForm {
                span: form.span,
                term: ITerm::Seq(new_seq),
            })
        }

        ITerm::If(test, cons, alt) => {
            let new_test = rewrite(test.clone(), subst);
            let new_cons = rewrite(cons.clone(), subst);
            let new_alt = rewrite(alt.clone(), subst);
            P(IForm {
                span: form.span,
                term: ITerm::If(new_test, new_cons, new_alt),
            })
        }
    }
}
