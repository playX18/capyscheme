use pretty::{DocAllocator, DocBuilder};

use crate::cps::graph::term::{
    ContId, Expression, FuncId, Graph, OccurrenceId, TermId, TermKind, VarId,
};

impl<'gc> Graph<'gc> {
    pub fn pretty<'a, D, A>(&self, alloc: &'a D) -> DocBuilder<'a, D, A>
    where
        D: DocAllocator<'a, A>,
        D::Doc: Clone,
        A: 'a + Clone,
    {
        match self.root {
            Some(root) => self.pretty_func(alloc, root),
            None => alloc.text("#<empty-cps-graph>"),
        }
    }

    fn pretty_var<'a, D, A>(&self, alloc: &'a D, var: VarId) -> DocBuilder<'a, D, A>
    where
        D: DocAllocator<'a, A>,
        D::Doc: Clone,
        A: 'a + Clone,
    {
        alloc.text(format!("{var}"))
    }

    fn pretty_occurrence<'a, D, A>(&self, alloc: &'a D, occ: OccurrenceId) -> DocBuilder<'a, D, A>
    where
        D: DocAllocator<'a, A>,
        D::Doc: Clone,
        A: 'a + Clone,
    {
        self.pretty_var(alloc, self.var(occ))
    }

    fn pretty_expression<'a, D, A>(
        &self,
        alloc: &'a D,
        expr: Expression<'gc>,
    ) -> DocBuilder<'a, D, A>
    where
        D: DocAllocator<'a, A>,
        D::Doc: Clone,
        A: 'a + Clone,
    {
        match expr {
            Expression::PrimCall(prim, args) => {
                let args = args.as_slice(&self.occ_pool);
                let args_doc = alloc.intersperse(
                    args.iter()
                        .copied()
                        .map(|arg| self.pretty_occurrence(alloc, arg)),
                    alloc.text(",") + alloc.space(),
                );

                alloc.text("#%")
                    + alloc.space()
                    + alloc.text(format!("{prim:?}"))
                    + if args.is_empty() {
                        alloc.text("()")
                    } else {
                        args_doc.parens()
                    }
            }
            Expression::Constant(value) => {
                alloc.text("const") + alloc.space() + alloc.text(value.to_string())
            }
        }
    }

    fn pretty_term<'a, D, A>(&self, alloc: &'a D, term_id: TermId) -> DocBuilder<'a, D, A>
    where
        D: DocAllocator<'a, A>,
        D::Doc: Clone,
        A: 'a + Clone,
    {
        match self[term_id].kind.clone() {
            TermKind::Let(..) => {
                let (bindings, body) = self.collect_lets(term_id);
                let bindings_doc = alloc.intersperse(
                    bindings.into_iter().map(|(var, expr)| {
                        alloc.text("let")
                            + alloc.space()
                            + self.pretty_var(alloc, var)
                            + alloc.space()
                            + alloc.text("=")
                            + alloc.space()
                            + self.pretty_expression(alloc, expr)
                            + alloc.text(";")
                    }),
                    alloc.hardline(),
                );

                bindings_doc + alloc.hardline() + self.pretty_term(alloc, body)
            }

            TermKind::Letk(conts, body) => {
                let conts_doc = alloc.intersperse(
                    conts.as_slice(&self.cont_pool).iter().copied().map(|cont| {
                        alloc.text("letk")
                            + alloc.space()
                            + self.pretty_cont(alloc, cont)
                            + alloc.text(";")
                    }),
                    alloc.hardline(),
                );

                conts_doc + alloc.hardline() + self.pretty_term(alloc, body)
            }

            TermKind::Fix(funcs, body) => {
                let funcs_doc = alloc.intersperse(
                    funcs.as_slice(&self.func_pool).iter().copied().map(|func| {
                        alloc.text("fix")
                            + alloc.space()
                            + self.pretty_func_binding(alloc, func)
                            + alloc.text(";")
                    }),
                    alloc.hardline(),
                );

                funcs_doc + alloc.hardline() + self.pretty_term(alloc, body)
            }

            TermKind::App(callee, retk, args) => {
                let mut arg_parts = vec![self.pretty_occurrence(alloc, retk)];
                arg_parts.extend(
                    args.as_slice(&self.occ_pool)
                        .iter()
                        .copied()
                        .map(|arg| self.pretty_occurrence(alloc, arg)),
                );
                let args_doc = alloc.intersperse(arg_parts, alloc.text(",") + alloc.space());
                self.pretty_occurrence(alloc, callee) + args_doc.parens()
            }

            TermKind::Continue(cont, args) => {
                let args_slice = args.as_slice(&self.occ_pool);
                let args_doc = alloc.intersperse(
                    args_slice
                        .iter()
                        .copied()
                        .map(|arg| self.pretty_occurrence(alloc, arg)),
                    alloc.text(",") + alloc.space(),
                );
                alloc.text("continue")
                    + alloc.space()
                    + self.pretty_occurrence(alloc, cont)
                    + if args_slice.is_empty() {
                        alloc.text("()")
                    } else {
                        args_doc.parens()
                    }
            }

            TermKind::If(cond, then_branch, then_args, else_branch, else_args) => {
                let cond_doc = self.pretty_occurrence(alloc, cond);
                let then_args_slice = then_args.as_slice(&self.occ_pool);
                let then_args_doc = alloc.intersperse(
                    then_args_slice
                        .iter()
                        .copied()
                        .map(|arg| self.pretty_occurrence(alloc, arg)),
                    alloc.text(",") + alloc.space(),
                );
                let else_args_slice = else_args.as_slice(&self.occ_pool);
                let else_args_doc = alloc.intersperse(
                    else_args_slice
                        .iter()
                        .copied()
                        .map(|arg| self.pretty_occurrence(alloc, arg)),
                    alloc.text(",") + alloc.space(),
                );

                alloc.text("if")
                    + alloc.space()
                    + cond_doc
                    + alloc.hardline()
                    + alloc.text("then")
                    + alloc.space()
                    + self.pretty_occurrence(alloc, then_branch)
                    + if then_args_slice.is_empty() {
                        alloc.text("()")
                    } else {
                        then_args_doc.parens()
                    }
                    + alloc.hardline()
                    + alloc.text("else")
                    + alloc.space()
                    + self.pretty_occurrence(alloc, else_branch)
                    + if else_args_slice.is_empty() {
                        alloc.text("()")
                    } else {
                        else_args_doc.parens()
                    }
            }
        }
    }

    fn pretty_func_name<'a, D, A>(
        &self,
        alloc: &'a D,
        func_id: FuncId,
    ) -> Option<DocBuilder<'a, D, A>>
    where
        D: DocAllocator<'a, A>,
        D::Doc: Clone,
        A: 'a + Clone,
    {
        let func = &self[func_id];
        let meta = func.meta;
        if !meta.is_pair() {
            return None;
        }
        let name_sym = self.ctx.intern("name");
        let name_val = meta.assq(name_sym)?;
        Some(alloc.text(name_val.cdr().to_string()))
    }

    fn pretty_func<'a, D, A>(&self, alloc: &'a D, func_id: FuncId) -> DocBuilder<'a, D, A>
    where
        D: DocAllocator<'a, A>,
        D::Doc: Clone,
        A: 'a + Clone,
    {
        let func = &self[func_id];
        let signature = self.pretty_signature(
            alloc,
            Some(func.return_continuation),
            func.parameters.as_slice(&self.var_pool).iter().copied(),
            func.variadic,
        );
        let fvar = self[func_id].var;

        let header = if let Some(name) = self.pretty_func_name(alloc, func_id) {
            alloc.text("fun") + alloc.space() + name
        } else {
            alloc.text(format!("fun {}", fvar))
        };

        header
            + signature
            + alloc.space()
            + alloc.text("=")
            + (alloc.hardline() + self.pretty_term(alloc, func.body())).nest(2)
    }

    fn pretty_func_binding<'a, D, A>(&self, alloc: &'a D, func_id: FuncId) -> DocBuilder<'a, D, A>
    where
        D: DocAllocator<'a, A>,
        D::Doc: Clone,
        A: 'a + Clone,
    {
        self.pretty_func(alloc, func_id)
    }

    fn pretty_cont<'a, D, A>(&self, alloc: &'a D, cont_id: ContId) -> DocBuilder<'a, D, A>
    where
        D: DocAllocator<'a, A>,
        D::Doc: Clone,
        A: 'a + Clone,
    {
        let cont = &self[cont_id];
        let signature = self.pretty_signature(
            alloc,
            None,
            cont.parameters.as_slice(&self.var_pool).iter().copied(),
            cont.variadic,
        );

        self.pretty_var(alloc, cont.var)
            + signature
            + alloc.space()
            + alloc.text("=")
            + (alloc.hardline() + self.pretty_term(alloc, cont.body())).nest(2)
    }

    fn pretty_signature<'a, D, A>(
        &self,
        alloc: &'a D,
        head: Option<VarId>,
        vars: impl Iterator<Item = VarId>,
        variadic: Option<VarId>,
    ) -> DocBuilder<'a, D, A>
    where
        D: DocAllocator<'a, A>,
        D::Doc: Clone,
        A: 'a + Clone,
    {
        let vars = head.into_iter().chain(vars).collect::<Vec<_>>();
        let has_fixed_args = !vars.is_empty();
        let vars_doc = if has_fixed_args {
            alloc.intersperse(
                vars.into_iter().map(|var| self.pretty_var(alloc, var)),
                alloc.text(",") + alloc.space(),
            )
        } else {
            alloc.nil()
        };

        match (has_fixed_args, variadic) {
            (true, Some(variadic)) => (vars_doc
                + alloc.text(",")
                + alloc.space()
                + alloc.text(".")
                + alloc.space()
                + self.pretty_var(alloc, variadic))
            .parens(),
            (true, None) => vars_doc.parens(),
            (false, Some(variadic)) => self.pretty_var(alloc, variadic),
            (false, None) => alloc.text("()"),
        }
    }

    fn collect_lets(&self, mut term_id: TermId) -> (Vec<(VarId, Expression<'gc>)>, TermId) {
        let mut bindings = Vec::new();

        loop {
            match self[term_id].kind {
                TermKind::Let(var, expr, next) => {
                    bindings.push((var, expr));
                    term_id = next;
                }
                _ => return (bindings, term_id),
            }
        }
    }
}
