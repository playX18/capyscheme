use crate::{
    expander::core::LVarRef,
    runtime::value::{Symbol, Value},
};

use super::term::*;
use ::pretty::{DocAllocator, DocBuilder};

impl<'gc> Atom<'gc> {
    pub fn pretty<'a, D, A>(&self, alloc: &'a D) -> DocBuilder<'a, D, A>
    where
        D: DocAllocator<'a, A>,
        D::Doc: Clone,
        A: 'a + Clone,
    {
        match self {
            Atom::Constant(value) => alloc.text(format!("{}", value)),
            Atom::Local(var) => alloc.text(format!("{}:{}", var.name, var.id)),
        }
    }
}

impl<'gc> Func<'gc> {
    pub fn pretty<'a, D, A>(&self, alloc: &'a D) -> DocBuilder<'a, D, A>
    where
        D: DocAllocator<'a, A>,
        D::Doc: Clone,
        A: 'a + Clone,
    {
        let args = name_and_args(
            alloc,
            self.name,
            Some(self.return_cont),
            Some(self.handler_cont),
            self.args.iter().map(|arg| alloc.text(arg.name.to_string())),
            self.variadic.map(|v| alloc.text(v.name.to_string())),
        );

        (alloc.text("lambda ") + args.group() + alloc.line() + self.body().pretty(alloc).nest(2))
            .nest(2)
            .group()
            .parens()
    }
}

impl<'gc> Term<'gc> {
    pub fn pretty<'a, D, A>(&self, alloc: &'a D) -> DocBuilder<'a, D, A>
    where
        D: DocAllocator<'a, A>,
        D::Doc: Clone,
        A: 'a + Clone,
    {
        match self {
            Term::Continue(k, args, _) => (alloc.text("continue")
                + alloc.space()
                + alloc.text(k.name.to_string())
                + if !args.is_empty() {
                    alloc.space()
                } else {
                    alloc.nil()
                }
                + alloc
                    .intersperse(args.iter().map(|arg| arg.pretty(alloc)), alloc.space())
                    .group())
            .group()
            .parens(),

            Term::App(f, k, h, args, _) => {
                let args_doc =
                    alloc.intersperse(args.iter().map(|arg| arg.pretty(alloc)), alloc.space());
                (f.pretty(alloc)
                    + alloc.space()
                    + alloc.text(k.name.to_string())
                    + alloc.space()
                    + alloc.text(h.name.to_string())
                    + if args.is_empty() {
                        alloc.nil()
                    } else {
                        alloc.space()
                    }
                    + args_doc)
                    .group()
                    .parens()
            }

            // (cond <test> => <kcons> | <kalt>)
            Term::If {
                test,
                consequent,
                consequent_args,
                alternative,
                alternative_args,
                ..
            } => {
                let test = test.pretty(alloc);
                let kcons = alloc.text(consequent.name.to_string());
                let kalt = alloc.text(alternative.name.to_string());

                let kcons_args = if let Some(args) = consequent_args {
                    alloc
                        .intersperse(
                            args.iter().map(|arg| arg.pretty(alloc)).collect::<Vec<_>>(),
                            alloc.space(),
                        )
                        .parens()
                        .group()
                } else {
                    alloc.nil()
                };

                let kalt_args = if let Some(args) = alternative_args {
                    alloc
                        .intersperse(
                            args.iter().map(|arg| arg.pretty(alloc)).collect::<Vec<_>>(),
                            alloc.space(),
                        )
                        .parens()
                        .group()
                } else {
                    alloc.nil()
                };

                (alloc.text("cond")
                    + alloc.space()
                    + test
                    + alloc.text(" => ")
                    + kcons
                    + kcons_args
                    + alloc.space()
                    + alloc.text("|")
                    + alloc.space()
                    + kalt
                    + kalt_args)
                    .group()
                    .parens()
            }

            Term::Let(var, expr, next) => (alloc.text("let")
                + alloc.space()
                + (alloc.text(var.name.to_string()) + alloc.space() + expr.pretty(alloc))
                    .brackets()
                + alloc.hardline()
                + next.pretty(alloc))
            .group()
            .parens(),

            Term::Fix(funcs, next) => {
                let funcs_doc = alloc.intersperse(
                    funcs.iter().map(|func| {
                        let binding = alloc.text(func.binding.name.to_string());
                        let func_doc = func.pretty(alloc);

                        (binding + alloc.softline() + func_doc).group().brackets()
                    }),
                    alloc.line(),
                );

                (alloc.text("fix")
                    + alloc.space()
                    + funcs_doc.parens()
                    + alloc.line()
                    + next.pretty(alloc).nest(2))
                .parens()
            }

            Term::Letk(conts, next) => {
                let conts_doc =
                    alloc.intersperse(conts.iter().map(|cont| cont.pretty(alloc)), alloc.line());

                (alloc.text("letk")
                    + alloc.hardline()
                    + conts_doc.parens().indent(2).group()
                    + alloc.hardline()
                    + next.pretty(alloc))
                .nest(1)
                .group()
                .parens()
            }
        }
    }
}

impl<'gc> Expression<'gc> {
    pub fn pretty<'a, D, A>(&self, alloc: &'a D) -> DocBuilder<'a, D, A>
    where
        D: DocAllocator<'a, A>,
        D::Doc: Clone,
        A: 'a + Clone,
    {
        match self {
            // #%prim(args...)
            Expression::PrimCall(prim, args, h, _) => {
                let args_doc = alloc.intersperse(
                    std::iter::once(h)
                        .map(|h| alloc.text(h.name.to_string()))
                        .chain(args.iter().map(|arg| arg.pretty(alloc))),
                    alloc.space(),
                );
                (alloc.text("#%") + alloc.text(prim.to_string()) + alloc.space() + args_doc)
                    .group()
                    .parens()
            }
        }
    }
}

impl<'gc> Cont<'gc> {
    pub fn pretty<'a, D, A>(&self, alloc: &'a D) -> DocBuilder<'a, D, A>
    where
        D: DocAllocator<'a, A>,
        D::Doc: Clone,
        A: 'a + Clone,
    {
        // (cont <name> (<args...>) . <body>)
        match self {
            Cont {
                name,
                binding,
                args,
                variadic,
                body,
                ..
            } => {
                let binding_doc = alloc.text(binding.name.to_string());

                let args = name_and_args(
                    alloc,
                    *name,
                    None,
                    None,
                    args.iter().map(|arg| alloc.text(arg.name.to_string())),
                    variadic.map(|v| alloc.text(v.name.to_string())),
                );

                (alloc.text("cont")
                    + alloc.space()
                    + binding_doc
                    + alloc.space()
                    + args
                    + alloc.text(format!(" @ {}", self.handler.get().name))
                    + alloc.hardline()
                    + body.get().pretty(alloc).indent(2).nest(2))
                .group()
                .parens()
            }
        }
    }
}

/// format name and args for funcs/conts:
///
/// if `name` is false: (args . variadic)
/// if `name` is a symbol: (name args . variadic)
/// if `args` is empty and name is symbol: (name . variadic)
/// if `args` is empty and name is false: variadic
/// if `variadic` is None and args is not empty and name is symbol: (name args)
/// and so on
fn name_and_args<'a, 'gc, D, A>(
    alloc: &'a D,
    name: Value<'gc>,
    k: Option<LVarRef<'gc>>,
    h: Option<LVarRef<'gc>>,
    args: impl Iterator<Item = DocBuilder<'a, D, A>>,
    variadic: Option<DocBuilder<'a, D, A>>,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone,
    A: 'a + Clone,
{
    let name_doc = if name.is::<Symbol>() {
        Some(alloc.text(name.to_string()))
    } else {
        None
    };
    let args = k
        .iter()
        .map(|k| alloc.text(k.name.to_string()))
        .chain(h.iter().map(|h| alloc.text(h.name.to_string())))
        .chain(args);
    let args: Vec<_> = args.collect();
    let is_empty = args.is_empty();
    let args_doc = if !args.is_empty() {
        alloc.intersperse(args, alloc.space())
    } else {
        alloc.nil()
    };

    match (name_doc, is_empty, variadic) {
        // name symbol, has args, has variadic: (name args . variadic)
        (Some(name), false, Some(variadic)) => (name
            + alloc.space()
            + args_doc
            + alloc.space()
            + alloc.text(".")
            + alloc.space()
            + variadic)
            .parens(),
        // name symbol, has args, no variadic: (name args)
        (Some(name), false, None) => (name + alloc.space() + args_doc).parens(),
        // name symbol, no args, has variadic: (name . variadic)
        (Some(name), true, Some(variadic)) => {
            (name + alloc.space() + alloc.text(".") + alloc.space() + variadic).parens()
        }
        // name symbol, no args, no variadic: name
        (Some(name), true, None) => name,
        // no name, has args, has variadic: (args . variadic)
        (None, false, Some(variadic)) => {
            (args_doc + alloc.space() + alloc.text(".") + alloc.space() + variadic).parens()
        }
        // no name, has args, no variadic: (args)
        (None, false, None) => args_doc.parens(),
        // no name, no args, has variadic: variadic
        (None, true, Some(variadic)) => variadic,
        // no name, no args, no variadic: ()
        (None, true, None) => alloc.text("()"),
    }
}
