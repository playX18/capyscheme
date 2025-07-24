
use std::hash::Hash;

use pretty::{DocAllocator, DocBuilder};
use crate::ast::INTERNER;

use super::term::*;

impl IForm {
    pub fn pretty<'b, D, A>(&'b self, allocator: &'b D) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone,
    {
        self.p(allocator)
    }

    fn p<'b, D, A>(&'b self, allocator: &'b D) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone,
    {
        match &self.term {
            ITerm::Const(datum) => allocator
                .text("quote")
                .append(allocator.space())
                .append(allocator.text(format!("{datum}")))
                .group()
                .parens(),

            ITerm::LRef(lref) => allocator.text(lref.name.to_string()),
            ITerm::GRef(gref) => allocator
                .text("gref")
                .append(allocator.space())
                .append(allocator.text(format!("{gref}")))
                .group()
                .parens(),

            ITerm::LSet(lset, val) => allocator
                .text("set!")
                .append(allocator.space())
                .append(allocator.text(format!("{}", lset.name)))
                .append(allocator.space())
                .append(val.p(allocator))
                .group()
                .parens(),

            ITerm::GSet(gset, val) => allocator
                .text("gset!")
                .append(allocator.space())
                .append(allocator.text(format!("{gset}")))
                .append(allocator.space())
                .append(val.p(allocator))
                .group()
                .parens(),

            ITerm::PrimRef(primref) => allocator
                .text("prim")
                .append(allocator.space())
                .append(allocator.text(format!("{}", INTERNER.resolve(primref))))
                .group()
                .parens(),

            ITerm::If(test, cons, alt) => allocator
                .text("if")
                .append(allocator.space())
                .append(test.p(allocator))
                .nest(2)
                .append(allocator.softline())
                .append(cons.p(allocator))
                .append(allocator.line())
                .append(alt.p(allocator)),

            ITerm::Seq(seq) => {
                let builder = allocator.text("seq").nest(2);

                builder
                    .append(allocator.line())
                    .append(
                        allocator.intersperse(seq.iter().map(|x| x.p(allocator)), allocator.line()),
                    )
                    .nest(2)
                    .group()
                    .parens()
            }

            ITerm::App(proc, args) => {
                let mut builder = allocator
                    .text("call")
                    .append(allocator.space())
                    .append(proc.p(allocator))
                    .nest(2);
                if !args.is_empty() {
                    builder = builder.append(allocator.softline());
                }
                builder = builder
                    .append(
                        allocator
                            .intersperse(args.iter().map(|x| x.p(allocator)), allocator.softline()),
                    )
                    .nest(2);
                builder.group().parens()
            }

            ITerm::Define(sym, val) => allocator
                .text("define")
                .append(allocator.space())
                .append(allocator.text(format!("{sym}")))
                .append(allocator.softline())
                .append(val.p(allocator).nest(2))
                .group()
                .parens(),

            ITerm::Proc(proc) => {
                let builder = allocator.text("proc");

                builder
                    .append(allocator.hardline())
                    .nest(2)
                    .append(allocator.intersperse(
                        proc.cases.iter().map(|case| {
                            let mut args = vec![];

                            for arg in case.args.iter() {
                                args.push(allocator.text(format!("{}", arg.name)));
                            }

                            if let Some(v) = &case.variadic {
                                args.push(allocator.text(format!("&{}", v.name)));
                            }

                            allocator
                                .text("case")
                                .append(allocator.space())
                                .append(
                                    allocator
                                        .intersperse(args.into_iter(), allocator.space())
                                        .group()
                                        .parens(),
                                )
                                .append(allocator.hardline())
                                .append(case.body.p(allocator))
                                .nest(2)
                                .group()
                                .parens()
                        }),
                        allocator.hardline(),
                    ))
                    .nest(2)
                    .group()
                    .parens()
            }

            ITerm::Let(l) => {
                let text = match l.style {
                    LetStyle::Let => allocator.text("let"),
                    LetStyle::LetRec => allocator.text("letrec"),
                    LetStyle::LetStar => allocator.text("let*"),
                    LetStyle::LetRecStar => allocator.text("letrec*"),
                };

                let vars = l
                    .variables
                    .iter()
                    .zip(l.initializers.iter())
                    .map(|(var, init)| {
                        allocator
                            .text(format!("{}", var.name))
                            .append(allocator.softline())
                            .nest(2)
                            .append(init.p(allocator))
                            .group()
                            .brackets()
                    });
                text.append(allocator.line())
                    .append(
                        allocator
                            .intersperse(vars, allocator.line())
                            .group()
                            .parens()
                            .indent(2)
                    )
                    .append(allocator.hardline())
                    .append(l.body.p(allocator))
                    .nest(2)
                    .group()
                    .parens()
            }
            _ => todo!(),
        }
    }
}

impl PartialEq for IForm {
    fn eq(&self, other: &Self) -> bool {
        self as *const Self == other as *const Self
    }
}

impl Eq for IForm {}

impl Hash for IForm {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (self as *const Self).hash(state);
    }
}
