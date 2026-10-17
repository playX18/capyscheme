//! Subterm cloning for graphical CPS.

use std::collections::HashMap;

use crate::runtime::MutationContext;

use super::graph::{
    BoundVar, ExprKind, FreeVar, FreeVars, Function, FunctionLinks, Graph, Subexpr, Subterm,
    TermId, TermKind,
};

/// Clones graph terms into the same graph with fresh local binders.
///
/// The cloner is intentionally independent of any one optimization pass. Seed
/// it with binder substitutions for call actuals or return continuations, then
/// clone the body that should be duplicated.
pub struct GraphClone<'a, 'gc> {
    ctx: MutationContext<'gc>,
    graph: &'a mut Graph<'gc>,
    binders: HashMap<BoundVar, BoundVar>,
}

impl<'a, 'gc> GraphClone<'a, 'gc> {
    pub fn new(ctx: MutationContext<'gc>, graph: &'a mut Graph<'gc>) -> Self {
        Self {
            ctx,
            graph,
            binders: HashMap::new(),
        }
    }

    pub fn with_substitutions(
        ctx: MutationContext<'gc>,
        graph: &'a mut Graph<'gc>,
        substitutions: impl IntoIterator<Item = (BoundVar, BoundVar)>,
    ) -> Self {
        let mut this = Self::new(ctx, graph);
        this.binders.extend(substitutions);
        this
    }

    pub fn clone_subterm(&mut self, root: Subterm) -> Option<Subterm> {
        let clone = self.graph.new_term_link(None);
        self.clone_subterm_into(root, clone)?;
        Some(clone)
    }

    pub fn clone_subterm_into(&mut self, source: Subterm, target: Subterm) -> Option<TermId> {
        let term = self.graph.read_term_link(source)?;
        self.clone_term_into(term, target)
    }

    fn mapped_binder(&self, binder: BoundVar) -> BoundVar {
        self.binders.get(&binder).copied().unwrap_or(binder)
    }

    fn fresh_binder(&mut self, binder: BoundVar) -> BoundVar {
        let fresh = self
            .graph
            .new_bound_var(self.graph[binder].var.copy(self.ctx));
        self.binders.insert(binder, fresh);
        fresh
    }

    fn clone_free_occurrence(&mut self, occ: FreeVar, owner: Subterm) -> FreeVar {
        let binder = self.mapped_binder(self.graph.free_binder(occ));
        self.graph.new_free_occ_for_binder(binder, owner)
    }

    fn clone_free_vars(&mut self, vars: &FreeVars, owner: Subterm) -> FreeVars {
        let vars = self.graph.free_vars_slice(vars).to_vec();
        let vars = vars
            .into_iter()
            .map(|var| self.clone_free_occurrence(var, owner))
            .collect::<Vec<_>>();
        self.graph.new_free_vars(vars)
    }

    fn clone_subterm_link(&mut self, source: Subterm) -> Option<Subterm> {
        let clone = self.graph.new_term_link(None);
        self.clone_subterm_into(source, clone)?;
        Some(clone)
    }

    fn clone_expr_link(&mut self, source: Subexpr, owner: Subterm) -> Option<Subexpr> {
        let expr = self.graph.read_expr_link(source)?;
        let data = self.graph[expr];
        let kind = match data.kind {
            ExprKind::Literal(value) => ExprKind::Literal(value),
            ExprKind::PrimCall(prim, vars) => {
                ExprKind::PrimCall(prim, self.clone_free_vars(&vars, owner))
            }
        };
        let link = self.graph.new_expr_link(None);
        let uplink = self.graph.new_parent_link(None);
        let clone = self.graph.new_expr(uplink, kind, data.source);
        self.graph.set_expr_link(link, clone);
        Some(link)
    }

    fn clone_term_into(&mut self, source: TermId, target: Subterm) -> Option<TermId> {
        let data = self.graph[source];
        let uplink = self.graph.new_parent_link(None);
        let kind = match data.kind {
            TermKind::LetVal((binder, expr), body) => {
                let binder = self.fresh_binder(binder);
                let expr = self.clone_expr_link(expr, target)?;
                let body = self.clone_subterm_link(body)?;
                TermKind::LetVal((binder, expr), body)
            }
            TermKind::Fix(functions, body) => {
                let functions = self.clone_function_links(functions)?;
                let body = self.clone_subterm_link(body)?;
                TermKind::Fix(functions, body)
            }
            TermKind::Letk(functions, body) => {
                let functions = self.clone_function_links(functions)?;
                let body = self.clone_subterm_link(body)?;
                TermKind::Letk(functions, body)
            }
            TermKind::If(test, consequent, alternative, hints) => {
                let test = self.clone_free_occurrence(test, target);
                let consequent = self.clone_subterm_link(consequent)?;
                let alternative = self.clone_subterm_link(alternative)?;
                TermKind::If(test, consequent, alternative, hints)
            }
            TermKind::Continue(cont, vars) => {
                let cont = self.clone_free_occurrence(cont, target);
                let vars = self.clone_free_vars(&vars, target);
                TermKind::Continue(cont, vars)
            }
            TermKind::App(func, vars, cont) => {
                let func = self.clone_free_occurrence(func, target);
                let vars = self.clone_free_vars(&vars, target);
                let cont = self.clone_free_occurrence(cont, target);
                TermKind::App(func, vars, cont)
            }
            TermKind::Raise(kind, vars) => {
                let vars = self.clone_free_vars(&vars, target);
                TermKind::Raise(kind, vars)
            }
            TermKind::BranchPrim(op, consequent, alternative, vars) => {
                let vars = self.clone_free_vars(&vars, target);
                let consequent = self.clone_free_occurrence(consequent, target);
                let alternative = self.clone_free_occurrence(alternative, target);
                TermKind::BranchPrim(op, consequent, alternative, vars)
            }
        };

        let clone = self.graph.new_term(uplink, kind, data.source);
        self.graph.set_term_link(target, clone);
        self.backpatch_term_children(clone, target);
        Some(clone)
    }

    fn clone_function_links(&mut self, functions: FunctionLinks) -> Option<FunctionLinks> {
        let source_links = self.graph.function_links_slice(&functions).to_vec();
        let mut clones = Vec::new();
        let mut cloned_links = Vec::new();

        for source_link in source_links {
            let Some(source) = self.graph.read_function_link(source_link) else {
                continue;
            };
            let data = self.graph[source];
            let var = self.fresh_binder(data.var);
            let vars = {
                let vars = self.graph.bound_vars_slice(&data.vars).to_vec();
                let vars = vars
                    .into_iter()
                    .map(|var| self.fresh_binder(var))
                    .collect::<Vec<_>>();
                self.graph.new_bound_vars(vars)
            };
            let variadic = data.variadic.map(|var| self.fresh_binder(var));
            let cont = data.cont.map(|cont| self.fresh_binder(cont));
            let body = self.graph.new_term_link(None);
            let clone = self.graph.new_function(Function {
                name: data.name,
                source: data.source,
                var,
                vars,
                variadic,
                cont,
                is_variadic: data.is_variadic,
                body,
                is_rec: data.is_rec,
                is_cold: data.is_cold,
                is_noinline: data.is_noinline,
                is_reified: data.is_reified,
                meta: data.meta,
            });
            let clone_link = self.graph.new_function_link(Some(clone));
            cloned_links.push(clone_link);
            clones.push((source, clone));
        }

        for (source, clone) in clones {
            self.clone_subterm_into(self.graph[source].body, self.graph[clone].body)?;
            self.graph.backpatch_function(clone);
        }

        Some(self.graph.new_function_links(cloned_links))
    }

    fn backpatch_term_children(&mut self, term: TermId, owner: Subterm) {
        match self.graph[term].kind {
            TermKind::LetVal((_, expr), body) => {
                self.graph.backpatch_subexprs(term, owner, &[expr]);
                self.graph.backpatch_subterms(term, &[body]);
            }
            TermKind::Fix(_, body) | TermKind::Letk(_, body) => {
                self.graph.backpatch_subterms(term, &[body]);
            }
            TermKind::If(_, consequent, alternative, _) => {
                self.graph
                    .backpatch_subterms(term, &[consequent, alternative]);
            }
            TermKind::Continue(..)
            | TermKind::App(..)
            | TermKind::Raise(..)
            | TermKind::BranchPrim(..) => {}
        }
    }
}
