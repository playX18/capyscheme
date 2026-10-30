use std::{cell::Cell, hash::Hash};

use pretty::{DocAllocator, DocBuilder};

use crate::{
    heap::{
        Gc, Trace,
        alloc::array::{Array, ArrayRef},
        cell::Lock,
    },
    runtime::{
        MutationContext,
        value::{Symbol, Value},
    },
};

#[derive(Trace, Debug)]
#[collect(no_drop)]
pub struct Term<'gc> {
    pub source: Lock<Value<'gc>>,
    pub kind: TermKind<'gc>,
}

impl<'gc> Term<'gc> {
    pub fn source(&self) -> Value<'gc> {
        self.source.get()
    }
}

impl<'gc> Clone for Term<'gc> {
    fn clone(&self) -> Self {
        Term {
            source: Lock::new(self.source.get()),
            kind: self.kind,
        }
    }
}

pub type TermRef<'gc> = Gc<'gc, Term<'gc>>;

#[derive(Trace, Debug, Clone, Copy)]
#[collect(no_drop)]
pub enum TermKind<'gc> {
    LRef(LVarRef<'gc>),
    LSet(LVarRef<'gc>, TermRef<'gc>),
    /// Reference to a variable of a linklet instance: `(key, symbol, slot)`.
    GlobalRef(Value<'gc>, Value<'gc>, GlobalSlot<'gc>),
    /// Assignment to a variable of a linklet instance: `(key, symbol, slot, value)`.
    GlobalSet(Value<'gc>, Value<'gc>, GlobalSlot<'gc>, TermRef<'gc>),
    Linklet(Gc<'gc, LinkletTerm<'gc>>),

    If(TermRef<'gc>, TermRef<'gc>, TermRef<'gc>),
    Seq(TermRef<'gc>, TermRef<'gc>),

    Let(Let<'gc>),
    Fix(Fix<'gc>),

    Receive(
        ArrayRef<'gc, LVarRef<'gc>>,
        Option<LVarRef<'gc>>,
        TermRef<'gc>,
        TermRef<'gc>,
    ),

    WithContinuationMark(TermRef<'gc>, TermRef<'gc>, TermRef<'gc>),

    Values(ArrayRef<'gc, TermRef<'gc>>),
    Proc(ProcRef<'gc>),

    Call(TermRef<'gc>, ArrayRef<'gc, TermRef<'gc>>),
    PrimCall(Value<'gc>, ArrayRef<'gc, TermRef<'gc>>),
    PrimRef(Value<'gc>),

    Const(Value<'gc>),
}

#[derive(Trace, Debug, Clone, Copy)]
#[collect(no_drop)]
pub enum GlobalSlot<'gc> {
    /// Box passed as a linklet body parameter (imports first, then exports).
    Box(LVarRef<'gc>),
    /// A `Variable` object embedded as a constant (namespace/REPL code only).
    Variable(Value<'gc>),
}

#[derive(Trace, Debug, Clone)]
#[collect(no_drop)]
pub struct LinkletTerm<'gc> {
    pub name: Value<'gc>,
    /// List of `(key sym ...)`.
    pub imports: Value<'gc>,
    /// List of syms.
    pub exports: Value<'gc>,
    /// One LVar per slot: import syms in order, then exports.
    pub slots: ArrayRef<'gc, LVarRef<'gc>>,
    pub body: TermRef<'gc>,
    pub meta: Value<'gc>,
}

#[derive(Trace, Debug)]
#[collect(no_drop)]
pub struct LVar<'gc> {
    pub name: Value<'gc>,
    pub id: Value<'gc>,
    pub set_count: Cell<u32>,
    pub ref_count: Cell<u32>,
}

impl<'gc> LVar<'gc> {
    pub fn copy(&self, ctx: MutationContext<'gc>) -> LVarRef<'gc> {
        let name = Symbol::from_str_uninterned(ctx, &format!("{}'", self.name), None);

        ctx.allocate(LVar {
            id: self.id,
            name: name.into(),
            set_count: Cell::new(self.set_count.get()),
            ref_count: Cell::new(self.ref_count.get()),
        })
    }

    pub fn is_mutated(&self) -> bool {
        self.set_count.get() > 0
    }

    pub fn mutate(&self) {
        self.set_count.set(self.set_count.get() + 1);
    }

    pub fn reference(&self) {
        self.ref_count.set(self.ref_count.get() + 1);
    }

    pub fn is_referenced(&self) -> bool {
        self.ref_count.get() > 0
    }
}

impl<'gc> PartialEq for LVar<'gc> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }
}

impl<'gc> Eq for LVar<'gc> {}

impl<'gc> Hash for LVar<'gc> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // SAFETY: The pointer references a valid GC-managed object of the expected type
        let hashcode = Gc::ptr_hash(unsafe { Gc::from_ptr(self) });
        state.write_u64(hashcode);
    }
}

pub type LVarRef<'gc> = Gc<'gc, LVar<'gc>>;

#[derive(Trace, Debug, Clone)]
#[collect(no_drop)]
pub struct Proc<'gc> {
    pub name: Value<'gc>,
    pub source: Value<'gc>,
    pub args: ArrayRef<'gc, LVarRef<'gc>>,
    pub variadic: Option<LVarRef<'gc>>,
    pub body: TermRef<'gc>,
    pub meta: Value<'gc>,
}

impl<'gc> Proc<'gc> {
    pub fn with_body(&self, mc: MutationContext<'gc>, body: TermRef<'gc>) -> ProcRef<'gc> {
        mc.allocate(Proc {
            name: self.name,
            source: self.source,
            args: self.args,
            variadic: self.variadic,
            body,
            meta: self.meta,
        })
    }
}

pub type ProcRef<'gc> = Gc<'gc, Proc<'gc>>;

#[derive(Trace, Debug, Clone, Copy)]
#[collect(no_drop)]
pub struct Let<'gc> {
    pub style: LetStyle,
    pub lhs: ArrayRef<'gc, LVarRef<'gc>>,
    pub rhs: ArrayRef<'gc, TermRef<'gc>>,
    pub body: TermRef<'gc>,
}

impl<'gc> Let<'gc> {
    pub fn with_body(&self, _mc: MutationContext<'gc>, body: TermRef<'gc>) -> Let<'gc> {
        Let {
            style: self.style,
            lhs: self.lhs,
            rhs: self.rhs,
            body,
        }
    }
}

#[derive(Trace, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[collect(no_drop)]
pub enum LetStyle {
    Let,
    LetStar,
    LetRec,
    LetRecStar,
}

#[derive(Trace, Debug, Clone, Copy)]
pub struct Fix<'gc> {
    pub lhs: ArrayRef<'gc, LVarRef<'gc>>,
    pub rhs: ArrayRef<'gc, ProcRef<'gc>>,
    pub body: TermRef<'gc>,
}

impl<'gc> Fix<'gc> {
    pub fn with_body(&self, _mc: MutationContext<'gc>, body: TermRef<'gc>) -> Fix<'gc> {
        Fix {
            lhs: self.lhs,
            rhs: self.rhs,
            body,
        }
    }
}

pub fn constant<'gc>(ctx: MutationContext<'gc>, value: Value<'gc>) -> TermRef<'gc> {
    ctx.allocate(Term {
        source: Lock::new(false.into()),
        kind: TermKind::Const(value),
    })
}

pub fn lref<'gc>(ctx: MutationContext<'gc>, lvar: LVarRef<'gc>) -> TermRef<'gc> {
    lvar.reference();
    ctx.allocate(Term {
        source: Lock::new(false.into()),
        kind: TermKind::LRef(lvar),
    })
}

pub fn lset<'gc>(
    ctx: MutationContext<'gc>,
    lvar: LVarRef<'gc>,
    value: TermRef<'gc>,
) -> TermRef<'gc> {
    lvar.mutate();
    ctx.allocate(Term {
        source: Lock::new(false.into()),
        kind: TermKind::LSet(lvar, value),
    })
}

// used by the linklet expander (milestone 3)
#[allow(dead_code)]
pub fn global_ref<'gc>(
    ctx: MutationContext<'gc>,
    key: Value<'gc>,
    name: Value<'gc>,
    slot: GlobalSlot<'gc>,
    sourcev: Value<'gc>,
) -> TermRef<'gc> {
    if let GlobalSlot::Box(lvar) = slot {
        lvar.reference();
    }
    ctx.allocate(Term {
        source: Lock::new(sourcev),
        kind: TermKind::GlobalRef(key, name, slot),
    })
}

#[allow(dead_code)]
pub fn global_set<'gc>(
    ctx: MutationContext<'gc>,
    key: Value<'gc>,
    name: Value<'gc>,
    slot: GlobalSlot<'gc>,
    exp: TermRef<'gc>,
    sourcev: Value<'gc>,
) -> TermRef<'gc> {
    if let GlobalSlot::Box(lvar) = slot {
        lvar.reference();
    }
    ctx.allocate(Term {
        source: Lock::new(sourcev),
        kind: TermKind::GlobalSet(key, name, slot, exp),
    })
}

#[allow(dead_code)]
pub fn linklet_term<'gc>(
    ctx: MutationContext<'gc>,
    name: Value<'gc>,
    imports: Value<'gc>,
    exports: Value<'gc>,
    slots: ArrayRef<'gc, LVarRef<'gc>>,
    body: TermRef<'gc>,
    meta: Value<'gc>,
    sourcev: Value<'gc>,
) -> TermRef<'gc> {
    ctx.allocate(Term {
        source: Lock::new(sourcev),
        kind: TermKind::Linklet(ctx.allocate(LinkletTerm {
            name,
            imports,
            exports,
            slots,
            body,
            meta,
        })),
    })
}

pub fn if_term<'gc>(
    ctx: MutationContext<'gc>,
    test: TermRef<'gc>,
    then_branch: TermRef<'gc>,
    else_branch: TermRef<'gc>,
) -> TermRef<'gc> {
    ctx.allocate(Term {
        source: Lock::new(false.into()),
        kind: TermKind::If(test, then_branch, else_branch),
    })
}

pub fn seq<'gc>(ctx: MutationContext<'gc>, head: TermRef<'gc>, tail: TermRef<'gc>) -> TermRef<'gc> {
    ctx.allocate(Term {
        source: Lock::new(false.into()),
        kind: TermKind::Seq(head, tail),
    })
}

pub fn seq_from_slice<'gc>(
    ctx: MutationContext<'gc>,
    terms: impl AsRef<[TermRef<'gc>]>,
) -> TermRef<'gc> {
    let terms = terms.as_ref();
    if terms.is_empty() {
        constant(ctx, Value::undefined())
    } else if terms.len() == 1 {
        terms[0]
    } else if terms.len() == 2 {
        seq(ctx, terms[0], terms[1])
    } else {
        let mut term = terms[terms.len() - 1];
        for t in terms[..terms.len() - 1].iter().rev() {
            term = seq(ctx, *t, term);
        }
        term
    }
}

pub fn let_term<'gc>(
    ctx: MutationContext<'gc>,
    style: LetStyle,
    lhs: ArrayRef<'gc, LVarRef<'gc>>,
    rhs: ArrayRef<'gc, TermRef<'gc>>,
    body: TermRef<'gc>,
    src: Value<'gc>,
) -> TermRef<'gc> {
    ctx.allocate(Term {
        source: Lock::new(src),
        kind: TermKind::Let(Let {
            style,
            lhs,
            rhs,
            body,
        }),
    })
}

pub fn fix_term<'gc>(
    ctx: MutationContext<'gc>,
    lhs: ArrayRef<'gc, LVarRef<'gc>>,
    rhs: ArrayRef<'gc, ProcRef<'gc>>,
    body: TermRef<'gc>,
) -> TermRef<'gc> {
    ctx.allocate(Term {
        source: Lock::new(false.into()),
        kind: TermKind::Fix(Fix { lhs, rhs, body }),
    })
}

pub fn proc_term<'gc>(
    ctx: MutationContext<'gc>,
    name: Value<'gc>,
    args: ArrayRef<'gc, LVarRef<'gc>>,
    variadic: Option<LVarRef<'gc>>,
    body: TermRef<'gc>,
    source: Value<'gc>,
    meta: Value<'gc>,
) -> TermRef<'gc> {
    ctx.allocate(Term {
        source: Lock::new(source),
        kind: TermKind::Proc(ctx.allocate(Proc {
            name,
            args,
            variadic,
            body,
            source,
            meta,
        })),
    })
}

pub fn call_term<'gc>(
    ctx: MutationContext<'gc>,
    proc: TermRef<'gc>,
    args: impl AsRef<[TermRef<'gc>]>,
    sourcev: Value<'gc>,
) -> TermRef<'gc> {
    let args = Array::from_slice(ctx, args);
    ctx.allocate(Term {
        source: Lock::new(sourcev),
        kind: TermKind::Call(proc, args),
    })
}

pub fn prim_call_term<'gc>(
    ctx: MutationContext<'gc>,
    proc: Value<'gc>,
    args: impl AsRef<[TermRef<'gc>]>,
    sourcev: Value<'gc>,
) -> TermRef<'gc> {
    let args = Array::from_slice(ctx, args);
    ctx.allocate(Term {
        source: Lock::new(sourcev),
        kind: TermKind::PrimCall(proc, args),
    })
}

impl<'gc> Term<'gc> {
    pub fn pretty<'a, D, A>(&self, alloc: &'a D) -> DocBuilder<'a, D, A>
    where
        D: DocAllocator<'a, A>,
        D::Doc: Clone,
        A: 'a + Clone,
    {
        match &self.kind {
            TermKind::PrimRef(name) => alloc.text(format!("#%{}", name)),
            TermKind::Values(vals) => {
                let vals_doc =
                    alloc.intersperse(vals.iter().map(|v| v.pretty(alloc)), alloc.space());

                alloc.text("values ").append(vals_doc).parens().group()
            }

            TermKind::WithContinuationMark(key, mark, result) => {
                let key_doc = key.pretty(alloc);
                let mark_doc = mark.pretty(alloc);
                let result_doc = result.pretty(alloc);

                alloc
                    .text("with-continuation-mark ")
                    .append(key_doc)
                    .append(alloc.space())
                    .append(mark_doc)
                    .append(alloc.line())
                    .append(result_doc.nest(1).indent(1))
                    .nest(1)
                    .parens()
                    .group()
            }

            TermKind::Receive(formals, opt_formal, producer, consumer) => {
                let formals_doc = alloc.intersperse(
                    formals.iter().map(|f| alloc.text(f.name.to_string())),
                    alloc.space(),
                );

                let opt_formal_doc = if let Some(opt) = opt_formal {
                    alloc.text(opt.name.to_string())
                } else {
                    alloc.nil()
                };

                let formals_doc = if opt_formal.is_some() {
                    formals_doc + alloc.text(" . ") + opt_formal_doc
                } else {
                    formals_doc
                }
                .parens()
                .group();

                let producer_doc = producer.pretty(alloc);
                let consumer_doc = consumer.pretty(alloc);

                alloc
                    .text("receive ")
                    .append(formals_doc)
                    .append(alloc.line())
                    .append(producer_doc.nest(1).indent(1))
                    .append(alloc.line())
                    .append(consumer_doc.nest(1).indent(1))
                    .nest(1)
                    .parens()
                    .group()
            }

            TermKind::Const(c) => alloc
                .text("const ")
                .append(alloc.text(c.to_string()))
                .parens()
                .group(),

            TermKind::Call(proc, args) => {
                let proc_doc = proc.pretty(alloc);
                let args_doc =
                    alloc.intersperse(args.iter().map(|arg| arg.pretty(alloc)), alloc.line());

                let args = if args.is_empty() {
                    alloc.nil()
                } else {
                    alloc.space().append(args_doc.nest(1)).group()
                };

                alloc
                    .text("call ")
                    .append(proc_doc)
                    .append(args)
                    .parens()
                    .group()
            }

            TermKind::PrimCall(proc, args) => {
                let proc_doc = alloc.text(proc.to_string());
                let args_doc =
                    alloc.intersperse(args.iter().map(|arg| arg.pretty(alloc)), alloc.line());

                let args = if args.is_empty() {
                    alloc.nil()
                } else {
                    alloc.space().append(args_doc.nest(1)).group()
                };

                alloc
                    .text("#%")
                    .append(proc_doc)
                    .append(args)
                    .parens()
                    .group()
            }

            TermKind::If(test, cons, alt) => {
                let test_doc = test.pretty(alloc);
                let cons_doc = cons.pretty(alloc);
                let alt_doc = alt.pretty(alloc);

                alloc
                    .text("if")
                    .append(alloc.line())
                    .append(test_doc.nest(1).indent(1))
                    .append(alloc.line())
                    .append(cons_doc.indent(1).nest(1))
                    .append(alloc.line())
                    .append(alt_doc.nest(1).indent(1))
                    .nest(1)
                    .parens()
                    .group()
            }

            TermKind::LRef(lvar) => {
                let lvar_doc = alloc.text(lvar.name.to_string());
                alloc.text("lref ").append(lvar_doc).parens().group()
            }

            TermKind::LSet(lvar, value) => {
                let lvar_doc = alloc.text(lvar.name.to_string());
                let value_doc = value.pretty(alloc);

                alloc
                    .text("lset ")
                    .append(lvar_doc)
                    .append(alloc.space())
                    .append(value_doc)
                    .parens()
                    .group()
            }

            TermKind::GlobalRef(key, name, slot) => {
                let slot_doc = slot.pretty(alloc);
                alloc
                    .text("global-ref")
                    .append(format!(" {key} {name} "))
                    .append(slot_doc)
                    .parens()
                    .group()
            }

            TermKind::GlobalSet(key, name, slot, exp) => {
                let slot_doc = slot.pretty(alloc);
                let exp_doc = exp.pretty(alloc);
                alloc
                    .text("global-set")
                    .append(format!(" {key} {name} "))
                    .append(slot_doc)
                    .append(alloc.space())
                    .append(exp_doc)
                    .parens()
                    .group()
            }

            TermKind::Linklet(linklet) => linklet.pretty(alloc),

            TermKind::Seq(..) => {
                let mut terms = Vec::new();
                let mut current = self;

                loop {
                    match &current.kind {
                        TermKind::Seq(head, tail) => {
                            terms.push(head.pretty(alloc));
                            current = tail;
                        }
                        _ => {
                            terms.push(current.pretty(alloc));
                            break;
                        }
                    }
                }

                alloc
                    .text("seq")
                    .append(alloc.line())
                    .append(alloc.intersperse(terms, alloc.line()).nest(2))
                    .parens()
                    .group()
            }

            TermKind::Let(l) => l.pretty(alloc),
            TermKind::Fix(f) => f.pretty(alloc),
            TermKind::Proc(proc) => proc.pretty(alloc),
        }
    }

    pub fn is_transparent(&self) -> bool {
        match &self.kind {
            TermKind::Const(_) => true,
            TermKind::LRef(_) => true,
            TermKind::Proc(_) => true,
            TermKind::If(test, cons, alt) => {
                test.is_transparent() && cons.is_transparent() && alt.is_transparent()
            }

            TermKind::Seq(head, tail) => head.is_transparent() && tail.is_transparent(),

            _ => false,
        }
    }

    pub fn is_procedure(&self) -> bool {
        matches!(&self.kind, TermKind::Proc(_))
    }

    pub fn fix(
        mc: MutationContext<'gc>,
        lhs: impl IntoIterator<Item = LVarRef<'gc>>,
        rhs: impl IntoIterator<Item = ProcRef<'gc>>,
        body: TermRef<'gc>,
        source: Value<'gc>,
    ) -> TermRef<'gc> {
        mc.allocate(Term {
            source: Lock::new(source),
            kind: TermKind::Fix(Fix {
                lhs: Array::from_iter(mc, lhs),
                rhs: Array::from_iter(mc, rhs),
                body,
            }),
        })
    }

    pub fn let_(
        mc: MutationContext<'gc>,
        style: LetStyle,
        lhs: impl IntoIterator<Item = LVarRef<'gc>>,
        rhs: impl IntoIterator<Item = TermRef<'gc>>,
        body: TermRef<'gc>,
        source: Value<'gc>,
    ) -> TermRef<'gc> {
        mc.allocate(Term {
            source: Lock::new(source),
            kind: TermKind::Let(Let {
                style,
                lhs: Array::from_iter(mc, lhs),
                rhs: Array::from_iter(mc, rhs),
                body,
            }),
        })
    }

    pub fn seq(
        mc: MutationContext<'gc>,
        head: TermRef<'gc>,
        tail: TermRef<'gc>,
        src: Value<'gc>,
    ) -> TermRef<'gc> {
        mc.allocate(Term {
            source: Lock::new(src),
            kind: TermKind::Seq(head, tail),
        })
    }

    pub fn cond(
        mc: MutationContext<'gc>,
        cond: TermRef<'gc>,
        cons: TermRef<'gc>,
        alt: TermRef<'gc>,
        src: Value<'gc>,
    ) -> TermRef<'gc> {
        mc.allocate(Term {
            source: Lock::new(src),
            kind: TermKind::If(cond, cons, alt),
        })
    }

    pub fn prims(
        ctx: MutationContext<'gc>,
        name: &str,
        args: impl IntoIterator<Item = TermRef<'gc>>,
        src: Value<'gc>,
    ) -> TermRef<'gc> {
        ctx.allocate(Term {
            source: Lock::new(src),
            kind: TermKind::PrimCall(
                Symbol::from_str(ctx, name).into(),
                Array::from_iter(ctx, args),
            ),
        })
    }

    pub fn lset(
        mc: MutationContext<'gc>,
        lvar: LVarRef<'gc>,
        value: TermRef<'gc>,
        src: Value<'gc>,
    ) -> TermRef<'gc> {
        mc.allocate(Term {
            source: Lock::new(src),
            kind: TermKind::LSet(lvar, value),
        })
    }
}

impl<'gc> GlobalSlot<'gc> {
    fn pretty<'a, D, A>(&self, alloc: &'a D) -> DocBuilder<'a, D, A>
    where
        D: DocAllocator<'a, A>,
        D::Doc: Clone,
        A: 'a + Clone,
    {
        match self {
            GlobalSlot::Box(lvar) => alloc.text(format!("box {}", lvar.name)),
            GlobalSlot::Variable(v) => alloc.text(format!("variable {v}")),
        }
    }
}

impl<'gc> LinkletTerm<'gc> {
    pub fn pretty<'a, D, A>(&self, alloc: &'a D) -> DocBuilder<'a, D, A>
    where
        D: DocAllocator<'a, A>,
        D::Doc: Clone,
        A: 'a + Clone,
    {
        let slots_doc = alloc.intersperse(
            self.slots
                .iter()
                .map(|slot| alloc.text(slot.name.to_string())),
            alloc.space(),
        );
        let body_doc = self.body.pretty(alloc);

        alloc
            .text(format!("linklet {}", self.name))
            .append(alloc.line())
            .append(slots_doc.parens())
            .append(alloc.line())
            .append(body_doc)
            .nest(1)
            .parens()
            .group()
    }
}

impl<'gc> Proc<'gc> {
    pub fn pretty<'a, D, A>(&self, alloc: &'a D) -> DocBuilder<'a, D, A>
    where
        D: DocAllocator<'a, A>,
        D::Doc: Clone,
        A: 'a + Clone,
    {
        let args = if let Some(variadic) = self.variadic
            && self.args.is_empty()
        {
            alloc.text(variadic.name.to_string())
        } else if let Some(variadic) = self.variadic {
            alloc
                .intersperse(
                    self.args.iter().map(|arg| alloc.text(arg.name.to_string())),
                    alloc.space(),
                )
                .append(alloc.space())
                .append(alloc.text(variadic.name.to_string()))
                .parens()
        } else if !self.args.is_empty() {
            alloc
                .intersperse(
                    self.args.iter().map(|arg| alloc.text(arg.name.to_string())),
                    alloc.space(),
                )
                .parens()
        } else {
            alloc.text("()")
        };

        let body_doc = self.body.pretty(alloc);

        let case = args.group().append(alloc.line()).append(body_doc).nest(1);
        alloc.text("lambda ").append(case).group().parens()
    }
}

impl<'gc> Let<'gc> {
    pub fn pretty<'a, D, A>(&self, alloc: &'a D) -> DocBuilder<'a, D, A>
    where
        D: DocAllocator<'a, A>,
        D::Doc: Clone,
        A: 'a + Clone,
    {
        let bindings_doc = alloc
            .intersperse(
                self.lhs.iter().zip(self.rhs.iter()).map(|(var, exp)| {
                    alloc
                        .text(var.name.to_string())
                        .append(alloc.hardline())
                        .append(exp.pretty(alloc))
                        .nest(2)
                        .brackets()
                        .group()
                }),
                alloc.line(),
            )
            .group()
            .parens();

        let body_doc = self.body.pretty(alloc);

        let style = match self.style {
            LetStyle::Let => "let",
            LetStyle::LetRec => "letrec",
            LetStyle::LetStar => "let*",
            LetStyle::LetRecStar => "letrec*",
        };

        alloc
            .text(style)
            .append(alloc.line())
            .append(bindings_doc)
            .nest(2)
            .append(alloc.line())
            .append(body_doc)
            .nest(1)
            .parens()
            .group()
    }
}

impl<'gc> Fix<'gc> {
    pub fn pretty<'a, D, A>(&self, alloc: &'a D) -> DocBuilder<'a, D, A>
    where
        D: DocAllocator<'a, A>,
        D::Doc: Clone,
        A: 'a + Clone,
    {
        let bindings_doc = alloc
            .intersperse(
                self.lhs.iter().zip(self.rhs.iter()).map(|(var, exp)| {
                    alloc
                        .text(var.name.to_string())
                        .append(alloc.softline())
                        .append(exp.pretty(alloc).nest(1))
                        .brackets()
                        .group()
                }),
                alloc.line(),
            )
            .group()
            .parens();

        let body_doc = self.body.pretty(alloc);

        alloc
            .text("fix")
            .append(alloc.line())
            .append(bindings_doc)
            .nest(1)
            .append(alloc.line())
            .append(body_doc)
            .nest(1)
            .parens()
            .group()
    }
}
