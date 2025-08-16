use crate::cps::builder::CPSBuilder;
use crate::cps::term::{Atom, BranchHint, Cont, Func, FuncRef, Term, TermRef};
use crate::expander::core::{LVarRef, LetStyle, Proc, TermKind, TermRef as CoreTermRef};
use crate::runtime::Context;
use crate::runtime::value::{Str, Vector};
use crate::runtime::value::{TypeCode8, Value};
use crate::{static_symbols, with_cps};
use rsgc::alloc::array::Array;
use rsgc::{Gc, Global, Rootable, Trace};
use std::collections::HashMap;
use std::mem::offset_of;
use std::sync::OnceLock;

pub fn t_k<'a, 'gc>(
    cps: &'a mut CPSBuilder<'gc>,
    form: CoreTermRef<'gc>,
    fk: Box<dyn FnOnce(&mut CPSBuilder<'gc>, &[Atom<'gc>]) -> TermRef<'gc> + 'a>,
    h: LVarRef<'gc>,
) -> TermRef<'gc> {
    let src = form.source;
    match form.kind {
        TermKind::Values(values) => {
            with_cps!(cps;
                @tk* (h) vals = &values;
                # fk(cps, &vals)
            )
        }
        TermKind::Receive(formals, formals_opt, producer, consumer) => {
            let consumer_k_var = cps.fresh_variable("consumer");

            let consumer_k = Cont::Local {
                name: Value::new(false),
                binding: consumer_k_var,
                args: formals,
                variadic: formals_opt,
                body: t_k(cps, consumer, fk, h),
                reified: false,
                source: src,
            };
            let letk_body = t_c(cps, producer, consumer_k_var, h);

            let consumer_k = Gc::new(&cps.ctx, consumer_k);
            Gc::new(
                &cps.ctx,
                Term::Letk(Array::from_array(&cps.ctx, &[consumer_k]), letk_body),
            )
        }
        TermKind::Const(_) | TermKind::LRef(_) | TermKind::GRef(_) => {
            let atom = m(cps, form);
            fk(cps, &[atom])
        }

        TermKind::Proc(proc) => {
            let tmp = cps.fresh_variable("proc");

            let func = cps_func(cps, &proc, tmp);
            let body = fk(cps, &[Atom::Local(tmp)]);
            Gc::new(
                &cps.ctx,
                Term::Fix(Array::from_array(&cps.ctx, &[func]), body),
            )
        }

        TermKind::Call(proc, args) => {
            with_cps!(cps;
                letk after_call (res) = fk(cps, &[Atom::Local(res)]);
                @tk (h) proc = proc;
                @tk* (h) args = &args;
                # {
                    let args = Array::from_array(&cps.ctx, &args);
                    Gc::new(
                        &cps.ctx,
                        Term::App(proc[0], after_call, h, args, src),
                    )
                }
            )
        }

        TermKind::PrimCall(prim, args) => t_k_many(
            cps,
            &args,
            Box::new(move |cps, args| {
                with_cps!(cps;
                    letk r (rv) = fk(cps, &[Atom::Local(rv)]);
                    # if let Some(term) = get_primitive_table(cps.ctx).try_expand(cps, form.source, prim, &args, r, h) {
                        term
                    } else {
                        with_cps!(cps;
                            let atom = #% prim (h) args... @ src;
                            continue r (Atom::Local(atom))
                        )
                    }
                )
            }),
            h,
        ),

        TermKind::Define(var, val) => with_cps!(cps;
            @tk (h) atom = val;
            let rv = #% "define" (h, Atom::Constant(var), atom[0]) @ form.source;
            # fk (cps, &[Atom::Local(rv)])
        ),

        TermKind::GSet(var, val) => with_cps!(cps;
            @tk (h) atom = val;
            let rv = #% "gset!" (h, Atom::Constant(var), atom[0]) @ form.source;
            # fk(cps, &[Atom::Local(rv)])
        ),

        TermKind::Let(let_) => {
            // let* is compiled differently
            if let LetStyle::LetStar = let_.style {
                let mut cpsed = t_k(cps, let_.body, fk, h);

                for (binding, expr) in let_.lhs.iter().zip(let_.rhs.iter()).rev() {
                    let single = Array::from_array(&cps.ctx, &[*binding]);
                    let expr = *expr;
                    cpsed = with_cps!(cps;
                        letk letstar (single...) @ src = cpsed;
                        @tc (letstar, h) expr
                    );
                }

                cpsed
            } else {
                let args = let_.lhs;
                with_cps!(cps;
                    @tk* (h) aexps = &let_.rhs;
                    letk r#let(args...) @ src = t_k(cps, let_.body, fk, h);
                    continue r#let aexps ...
                )
            }
        }

        TermKind::Seq(seq) => with_cps!(cps;
            @tk* (h) aexps = &seq;
            # fk(cps, &[*aexps.last().expect("Sequence should not be empty")])
        ),

        TermKind::If(test, cons, alt) => with_cps!(cps;
            @tk (h) atest = test;
            letk cont (rv) = fk(cps, &[Atom::Local(rv)]);
            letk kcons () = t_c(cps, cons, cont, h);
            letk kalt () = t_c(cps, alt, cont, h);
            # Gc::new(&cps.ctx, Term::If(atest[0], kcons, kalt, [BranchHint::Normal, BranchHint::Normal]))
        ),

        TermKind::Fix(fix) => {
            let funcs = fix
                .lhs
                .iter()
                .zip(fix.rhs.iter())
                .map(|(binding, func)| {
                    let func = cps_func(cps, func, *binding);
                    func
                })
                .collect::<Vec<_>>();

            let body = t_k(cps, fix.body, fk, h);

            Gc::new(
                &cps.ctx,
                Term::Fix(Array::from_array(&cps.ctx, &funcs), body),
            )
        }

        TermKind::LSet(_, _) => panic!("LSet should not be in CPS form"),
    }
}

pub fn t_c<'a, 'gc>(
    cps: &'a mut CPSBuilder<'gc>,
    form: CoreTermRef<'gc>,
    k: LVarRef<'gc>,
    h: LVarRef<'gc>,
) -> TermRef<'gc> {
    let src = form.source;

    match form.kind {
        TermKind::Fix(fix) => {
            let funcs = fix
                .lhs
                .iter()
                .zip(fix.rhs.iter())
                .map(|(binding, func)| {
                    let func = cps_func(cps, func, *binding);
                    func
                })
                .collect::<Vec<_>>();

            let body = t_c(cps, fix.body, k, h);
            Gc::new(
                &cps.ctx,
                Term::Fix(Array::from_array(&cps.ctx, &funcs), body),
            )
        }

        TermKind::Const(_) | TermKind::LRef(_) | TermKind::GRef(_) => {
            let atom = m(cps, form);
            with_cps!(cps;
                continue k (atom)
            )
        }

        TermKind::Proc(proc) => {
            let tmp = cps.fresh_variable("proc");

            let func = cps_func(cps, &proc, tmp);
            let t = with_cps!(cps; continue k (Atom::Local(tmp)));
            let body = Term::Fix(Array::from_array(&cps.ctx, &[func]), t);
            Gc::new(&cps.ctx, body)
        }

        TermKind::Call(proc, args) => {
            with_cps!(cps;
                @tk* (h) args = &args;
                @tk (h) proc = proc;
                # let proc = proc[0];
                proc (k, h, args ...) @ src
            )
        }

        TermKind::PrimCall(prim, args) => t_k_many(
            cps,
            &args,
            Box::new(move |cps, args| {
                if let Some(term) =
                    get_primitive_table(cps.ctx).try_expand(cps, src, prim, &args, k, h)
                {
                    return term;
                }

                with_cps!(cps;
                    let rv = #% prim (h) args... @ src;
                    continue k (Atom::Local(rv))
                )
            }),
            h,
        ),

        TermKind::Define(var, val) => with_cps!(cps;
            @tk (h) atom = val;
            let rv = #% "define" (h, Atom::Constant(var), atom[0]) @ src;
            continue k (Atom::Local(rv))
        ),

        TermKind::GSet(var, val) => with_cps!(cps;
            @tk (h) atom = val;
            let rv = #% "gset!" (h, Atom::Constant(var), atom[0]) @ src;
            continue k (Atom::Local(rv))
        ),

        TermKind::Let(let_) => {
            if let LetStyle::LetStar = let_.style {
                let mut cpsed = t_c(cps, let_.body, k, h);

                for (binding, expr) in let_.lhs.iter().zip(let_.rhs.iter()).rev() {
                    let single = Array::from_array(&cps.ctx, &[*binding]);
                    let expr = *expr;
                    cpsed = with_cps!(cps;
                        letk letstar (single...) @ src = cpsed;
                        @tc (letstar, h) expr
                    );
                }

                cpsed
            } else {
                let args = let_.lhs;
                with_cps!(cps;
                    @tk* (h) aexps = &let_.rhs;
                    letk r#let(args...) @ src = t_c(cps, let_.body, k, h);

                    continue r#let aexps ...
                )
            }
        }

        TermKind::Seq(seq) => with_cps!(cps;
            @tk* (h) aexps = &seq;
            # {
                let args = Array::from_array(&cps.ctx, [*aexps.last().expect("Sequence should not be empty")]);
                Gc::new(&cps.ctx, Term::Continue(k, args, src))
            }
        ),

        TermKind::If(test, cons, alt) => with_cps!(cps;
            @tk (h) atest = test;
            letk kcons () = t_c(cps, cons, k, h);
            letk kalt () = t_c(cps, alt, k, h);
            # Gc::new(&cps.ctx, Term::If(atest[0], kcons, kalt, [BranchHint::Normal, BranchHint::Normal]))
        ),

        TermKind::Receive(formals, formals_opt, producer, consumer) => {
            let consumer_k_var = cps.fresh_variable("consumer");

            let consumer_k = Cont::Local {
                name: Value::new(false),
                binding: consumer_k_var,
                args: formals,
                variadic: formals_opt,
                body: t_c(cps, consumer, k, h),
                reified: false,
                source: src,
            };
            let letk_body = t_c(cps, producer, consumer_k_var, h);

            let consumer_k = Gc::new(&cps.ctx, consumer_k);
            Gc::new(
                &cps.ctx,
                Term::Letk(Array::from_array(&cps.ctx, &[consumer_k]), letk_body),
            )
        }

        TermKind::Values(values) => {
            with_cps!(cps;
                @tk* (h) vals = &values;
                continue k (Atom::Values(Array::from_array(&cps.ctx, &vals)))
            )
        }

        _ => todo!(),
    }
}

pub fn m<'a, 'gc>(_: &'a mut CPSBuilder<'gc>, form: CoreTermRef<'gc>) -> Atom<'gc> {
    match &form.kind {
        TermKind::Const(c) => Atom::Constant(*c),
        TermKind::LRef(lref) => Atom::Local(*lref),
        TermKind::GRef(gref) => Atom::Global(*gref),
        TermKind::Proc(_) => todo!(),
        _ => unreachable!(),
    }
}

pub fn t_k_many<'a, 'gc>(
    builder: &'a mut CPSBuilder<'gc>,
    forms: &'a [CoreTermRef<'gc>],
    fk: Box<dyn FnOnce(&mut CPSBuilder<'gc>, Vec<Atom<'gc>>) -> TermRef<'gc> + 'a>,
    h: LVarRef<'gc>,
) -> TermRef<'gc> {
    let forms = forms.as_ref();
    if forms.is_empty() {
        return fk(builder, Vec::new());
    }

    let (first, rest) = forms.split_first().unwrap();

    t_k(
        builder,
        *first,
        Box::new(move |builder, first| {
            if rest.is_empty() {
                fk(builder, first.to_vec())
            } else {
                t_k_many(
                    builder,
                    rest,
                    Box::new(move |builder, rest| {
                        let mut atoms = first.to_vec();
                        atoms.extend(rest.to_vec());
                        fk(builder, atoms)
                    }),
                    h,
                )
            }
        }),
        h,
    )
}

pub fn cps_func<'a, 'gc>(
    builder: &'a mut CPSBuilder<'gc>,
    proc: &Proc<'gc>,
    binding: LVarRef<'gc>,
) -> FuncRef<'gc> {
    let return_cont = builder.fresh_variable("return");
    let handler_cont = builder.fresh_variable("handler");
    let body = t_c(builder, proc.body, return_cont, handler_cont);

    Gc::new(
        &builder.ctx,
        Func {
            args: proc.args,
            name: proc.name,
            binding,
            source: proc.source,
            return_cont,
            handler_cont,
            variadic: proc.variadic,
            body,
        },
    )
}

pub fn cps_toplevel<'gc>(ctx: Context<'gc>, forms: &[CoreTermRef<'gc>]) -> FuncRef<'gc> {
    let form = if forms.len() == 1 {
        forms[0]
    } else if forms.is_empty() {
        panic!("Cannot compile an empty program to CPS")
    } else {
        let seq = Gc::new(
            &ctx,
            super::core::Term {
                source: Value::new(false),
                kind: TermKind::Seq(Array::from_array(&ctx, forms)),
            },
        );

        seq
    };

    let proc = Proc {
        args: Array::from_array(&ctx, &[]),
        name: Value::new(false),
        body: form,
        source: Value::new(false),
        variadic: None,
    };

    let mut builder = CPSBuilder::new(ctx);

    let bind = builder.fresh_variable("toplevel");
    let func = cps_func(&mut builder, &proc, bind);

    func
}

pub type PrimitiveTransformer = for<'a, 'gc, 'b> fn(
    cps: &'a mut CPSBuilder<'gc>,
    src: Value<'gc>,
    op: Value<'gc>,
    params: &'b [Atom<'gc>],
    k: LVarRef<'gc>,
    h: LVarRef<'gc>,
) -> Option<TermRef<'gc>>;

pub struct PrimitiveTable<'gc> {
    pub table: HashMap<Value<'gc>, PrimitiveTransformer>,
}
unsafe impl<'gc> Trace for PrimitiveTable<'gc> {
    unsafe fn trace(&mut self, visitor: &mut rsgc::collection::Visitor) {
        for (key, _) in self.table.iter_mut() {
            unsafe {
                let value = key as *const Value<'gc> as *mut Value<'gc>;
                (*value).trace(visitor);
            }
        }
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut rsgc::WeakProcessor) {
        let _ = weak_processor;
    }
}

static PRIMTABLE: OnceLock<Global<Rootable!(PrimitiveTable<'_>)>> = OnceLock::new();

macro_rules! primitive_transformers {
    ($(
        $prim: literal => $name : ident ($cps: ident, $src: ident, $op: ident, $args: ident, $k: ident) $b: block
    )*) => {
        #[allow(dead_code, unused_mut, unused_variables)]
        fn make_primitive_table<'gc>(ctx: Context<'gc>) -> HashMap<Value<'gc>, PrimitiveTransformer> {
            let mut table: HashMap<Value<'gc>, PrimitiveTransformer> = HashMap::new();
            $(
                table.insert(
                    Value::new(Symbol::from_str(ctx, $prim)),
                    { fn $name<'gc>($cps: &mut CPSBuilder<'gc>, $src: Value<'gc>, $op: Value<'gc>, $args: &[Atom<'gc>], $k: LVarRef<'gc>) -> Option<TermRef<'gc>> {
                        $b
                    } $name },
                );
            )*
            table
        }
    };
}

pub fn get_primitive_table<'gc>(ctx: Context<'gc>) -> &'gc PrimitiveTable<'gc> {
    PRIMTABLE
        .get_or_init(|| {
            let table = make_primitive_table(ctx);
            Global::new(PrimitiveTable { table })
        })
        .fetch(&ctx)
}

impl<'gc> PrimitiveTable<'gc> {
    pub fn try_expand(
        &self,
        cps: &mut CPSBuilder<'gc>,
        src: Value<'gc>,
        prim: Value<'gc>,
        args: &[Atom<'gc>],
        k: LVarRef<'gc>,
        h: LVarRef<'gc>,
    ) -> Option<TermRef<'gc>> {
        if let Some(transformer) = self.table.get(&prim) {
            transformer(cps, src, prim, args, k, h)
        } else {
            None
        }
    }
}

static_symbols!(
    RAISE_TYPE_ERROR = "raise-type-error"
    RAISE_RANGE_ERROR = "raise-range-error"
    RAISE_ARITY_ERROR = "raise-arity-error"
);

pub fn ensure_string<'gc, F>(
    cps: &mut CPSBuilder<'gc>,
    src: Value<'gc>,
    op: Value<'gc>,
    x: Atom<'gc>,
    h: LVarRef<'gc>,
    have_length: F,
) -> TermRef<'gc>
where
    F: FnOnce(&mut CPSBuilder<'gc>, Atom<'gc>) -> TermRef<'gc>,
{
    let const_not_string = Vector::from_slice(
        &cps.ctx,
        &[
            Str::new(&cps.ctx, op.to_string(), true).into(),
            Value::new(1i32),
            Str::new(&cps.ctx, "string", true).into(),
        ],
    );
    with_cps!(cps;
        letk not_string () = with_cps!(cps;
            throw raise_type_error(cps.ctx), const_not_string; src
        );
        letk k () = with_cps!(cps;
            let length = #% ".refptr" (h, x, offset_of!(Str, length) as i32) @ src;
            # {
                have_length(cps, Atom::Local(length))
            }
        );
        letk check_str_typecode (x) = with_cps!(cps;
            let tc8 = #% ".typecode8" (h, x) @ src;
            let str_typecode = #% "u8=" (h, tc8, Value::new(TypeCode8::STRING.bits() as i32)) @ src;
            if str_typecode => k | not_string
        );

        let is_immediate = #% ".is-immediate" (h, x) @ src;
        if is_immediate => not_string | check_str_typecode
    )
}
/*
primitive_transformers!(
    "string-length" => string_length(cps, src, op, args, k) {
        let x = args.first().copied()?;
        if let Atom::Constant(val) = x
            && val.is::<Str>() {
            let x = with_cps!(cps;
                continue k (val.downcast::<Str>().len() as i32)
            );
            return Some(x);
        }
        Some(ensure_string(cps, src, op, x, |cps, len| {
            with_cps!(cps;
                let vlen = #% "usize->value" (len) @ src;
                continue k (vlen)
            )
        }))
    }

    "string?" => string_is_string(cps, src, op, args, k) {
        let x = args.first().copied()?;

        if let Atom::Constant(val) = x
            && val.is::<Str>() {
            let x = with_cps!(cps;
                continue k (Value::from_bool(true))
            );
            return Some(x);
        }

        Some(ensure_string(cps, src, op, x, |cps, _| {
            with_cps!(cps;
                continue k (true)
            )
        }))
    }
);
*/
primitive_transformers!();
