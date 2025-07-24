use crate::ast::INTERNER;
use crate::il::term::LVar;
use ::pretty::DocAllocator;
use ::pretty::DocBuilder;

use super::term::*;
impl Atom {
    pub fn pretty<'a, D, A>(&self, alloc: &'a D) -> DocBuilder<'a, D, A>
    where
        D: DocAllocator<'a, A>,
        D::Doc: Clone,
        A: 'a + Clone,
    {
        match self {
            Atom::Constant(datum) => alloc.text(format!("'{datum}")),
            Atom::Global(sym, _) => alloc.text(format!("(global {sym})")),
            Atom::Local(local) => alloc.text(format!("{}", local.name)),
            Atom::Prim(prim, _) => alloc.text(format!("#%{}", INTERNER.resolve(prim))),
            Atom::Func(func, _) => func.pretty(alloc),
        }
    }
}

impl Func {
    pub fn pretty<'a, D, A>(&self, alloc: &'a D) -> DocBuilder<'a, D, A>
    where
        D: DocAllocator<'a, A>,
        D::Doc: Clone,
        A: 'a + Clone,
    {
        let name = match &self.name {
            Some(name) => alloc.space() + alloc.text(format!("{}", INTERNER.resolve(name))),
            None => alloc.nil(),
        };

        let args = if let Some(variadic) = &self.variadic
            && self.args.is_empty()
        {
            alloc.text(format!("({} . {})", self.return_cont.name, variadic.name))
        } else if let Some(variadic) = &self.variadic {
            alloc.space()
                + (alloc.text(self.return_cont.name.to_string())
                    + if !self.args.is_empty() {
                        alloc.space()
                    } else {
                        alloc.nil()
                    }
                    + alloc.space()
                    + alloc.intersperse(
                        self.args
                            .iter()
                            .map(|arg| alloc.text(format!("{}", arg.name))),
                        alloc.space(),
                    )
                    + alloc.space()
                    + alloc.text(format!(". {}", variadic.name)))
                .group()
                .parens()
        } else {
            alloc.space()
                + (alloc.text(self.return_cont.name.to_string())
                    + if !self.args.is_empty() {
                        alloc.space()
                    } else {
                        alloc.nil()
                    }
                    + alloc.intersperse(
                        self.args
                            .iter()
                            .map(|arg| alloc.text(format!("{}", arg.name))),
                        alloc.space(),
                    ))
                .group()
                .parens()
        };

        (alloc.text("lambda")
            + name
            + args.group()
            + alloc.line()
            + self.body.pretty(alloc).nest(2))
        .nest(2)
        .group()
        .parens()
    }
}

impl Cont {
    pub fn pretty<'a, D, A>(&self, alloc: &'a D) -> DocBuilder<'a, D, A>
    where
        D: DocAllocator<'a, A>,
        D::Doc: Clone,
        A: 'a + Clone,
    {
        /* (cont <name> <args> <body>) */

        let name = match &self.name {
            Some(name) => alloc.space() + alloc.text(format!("{}", INTERNER.resolve(name))),
            None => alloc.nil(),
        };

        let args = if let Some(variadic) = &self.variadic
            && self.args.is_empty()
        {
            alloc.text(format!(" {}", variadic.name))
        } else if let Some(variadic) = &self.variadic {
            alloc.space()
                + (alloc.intersperse(
                    self.args
                        .iter()
                        .map(|arg| alloc.text(format!("{}", arg.name))),
                    alloc.space(),
                ) + alloc.space()
                    + alloc.text(format!(". {}", variadic.name)))
                .group()
                .parens()
        } else {
            alloc.space()
                + alloc
                    .intersperse(
                        self.args
                            .iter()
                            .map(|arg| alloc.text(format!("{}", arg.name))),
                        alloc.space(),
                    )
                    .group()
                    .parens()
        };

        (alloc.text("cont")
            + name
            + args.group()
            + alloc.hardline()
            + self.body.pretty(alloc).nest(2))
        .parens()
        .group()
    }
}

impl Term {
    pub fn pretty<'a, D, A>(&self, alloc: &'a D) -> DocBuilder<'a, D, A>
    where
        D: DocAllocator<'a, A>,
        D::Doc: Clone,
        A: 'a + Clone,
    {
        match self {
            Term::App(proc, k, args, _) => {
                let args_doc =
                    alloc.intersperse(args.iter().map(|arg| arg.pretty(alloc)), alloc.space());
                (proc.pretty(alloc)
                    + alloc.space()
                    + k.pretty(alloc)
                    + if args.is_empty() {
                        alloc.nil()
                    } else {
                        alloc.space()
                    }
                    + args_doc)
                    .group()
                    .parens()
            }

            Term::Continue(k, args, _) => {
                let args_doc =
                    alloc.intersperse(args.iter().map(|arg| arg.pretty(alloc)), alloc.space());
                (alloc.text("continue ")
                    + k.pretty(alloc)
                    + if args.is_empty() {
                        alloc.nil()
                    } else {
                        alloc.space()
                    }
                    + args_doc)
                    .group()
                    .parens()
            }

            /*
               (fix (
                   [f1 (lambda ...)]
                   [f2 (lambda ...)]
               ) body)
            */
            Term::Fix(funcs, body) => {
                let funcs_doc = alloc.intersperse(
                    funcs.iter().map(|func| {
                        let binding = func.binding.pretty(alloc);
                        let func_doc = func.pretty(alloc);

                        (binding + alloc.softline() + func_doc).group().brackets()
                    }),
                    alloc.line(),
                );

                (alloc.text("fix")
                    + alloc.space()
                    + funcs_doc.parens()
                    + alloc.line()
                    + body.pretty(alloc).nest(2))
                .parens()
            }

            Term::Let(var, expr, body) => (alloc.text("let")
                + alloc.space()
                + (var.pretty(alloc) + alloc.space() + expr.pretty(alloc)).brackets()
                + alloc.hardline()
                + body.pretty(alloc).nest(2))
            .group()
            .parens(),

            Term::If(test,  cons, alt) => (alloc.text("if")
                + alloc.space()
                + test.pretty(alloc)
                + alloc.space()
                + cons.pretty(alloc)
                + alloc.space()
                + alt.pretty(alloc))
            .group()
            .parens(),

            /*
               (letk (
                   [c1 (cont ...)]
                   [c2 (cont ...)]
               ) body)
            */
            Term::Letk(conts, body) => {
                let conts_doc = alloc.intersperse(
                    conts.iter().map(|cont| {
                        let binding = cont.binding.pretty(alloc);
                        let cont_doc = cont.pretty(alloc);

                        (binding + alloc.softline() + cont_doc).group().brackets()
                    }),
                    alloc.line(),
                );

                (alloc.text("letk")
                    + alloc.space()
                    + conts_doc.parens()
                    + alloc.line()
                    + body.pretty(alloc).nest(2))
                .parens()
            }
        }
    }
}

impl LVar {
    pub fn pretty<'a, D, A>(&self, alloc: &'a D) -> DocBuilder<'a, D, A>
    where
        D: DocAllocator<'a, A>,
        D::Doc: Clone,
        A: 'a + Clone,
    {
        alloc.text(format!("{}", self.name))
    }
}

impl Expression {
    pub fn pretty<'a, D, A>(&self, alloc: &'a D) -> DocBuilder<'a, D, A>
    where
        D: DocAllocator<'a, A>,
        D::Doc: Clone,
        A: 'a + Clone,
    {
        match self {
            Expression::PrimCall(prim, args, _) => {
                let args_doc =
                    alloc.intersperse(args.iter().map(|arg| arg.pretty(alloc)), alloc.space());
                (alloc.text(format!("#%{}", INTERNER.resolve(prim)))
                    + if !args.is_empty() {
                        alloc.space()
                    } else {
                        alloc.nil()
                    }
                    + args_doc)
                    .group()
                    .parens()
            }
        }
    }
}
