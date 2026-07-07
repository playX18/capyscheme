//! Subterm cloning for graphical CPS.

use std::collections::HashMap;

use crate::runtime::Context;

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
    ctx: Context<'gc>,
    graph: &'a mut Graph<'gc>,
    binders: HashMap<BoundVar, BoundVar>,
}

impl<'a, 'gc> GraphClone<'a, 'gc> {
    pub fn new(ctx: Context<'gc>, graph: &'a mut Graph<'gc>) -> Self {
        Self {
            ctx,
            graph,
            binders: HashMap::new(),
        }
    }

    pub fn with_substitutions(
        ctx: Context<'gc>,
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
            TermKind::If(test, consequent, alternative) => {
                let test = self.clone_free_occurrence(test, target);
                let consequent = self.clone_subterm_link(consequent)?;
                let alternative = self.clone_subterm_link(alternative)?;
                TermKind::If(test, consequent, alternative)
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
                unroll_count: data.unroll_count,
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
            TermKind::If(_, consequent, alternative) => {
                self.graph
                    .backpatch_subterms(term, &[consequent, alternative]);
            }
            TermKind::Continue(..) | TermKind::App(..) | TermKind::Raise(..) => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        expander::core::fresh_lvar,
        runtime::{Scheme, value::Value},
    };

    use super::super::graph::Parent;

    fn lvar<'gc>(ctx: Context<'gc>, name: &str) -> crate::expander::core::LVarRef<'gc> {
        fresh_lvar(ctx, ctx.intern(name))
    }

    #[test]
    fn clone_letval_uses_fresh_binding_and_preserves_external_uses() {
        Scheme::new_uninit().enter(|ctx| {
            let mut graph = Graph::new();
            let root = graph.new_term_link(None);
            let external = graph.new_bound_var(lvar(ctx, "external"));
            let value = graph.new_bound_var(lvar(ctx, "value"));
            let cont = graph.new_bound_var(lvar(ctx, "cont"));

            let expr_link = graph.new_expr_link(None);
            let expr_external = graph.new_free_occ_for_binder(external, root);
            let expr_vars = graph.new_free_vars([expr_external]);
            let expr_parent = graph.new_parent_link(None);
            let expr = graph.new_expr(
                expr_parent,
                ExprKind::PrimCall(Value::from_i32(1), expr_vars),
                Value::new(false),
            );
            graph.set_expr_link(expr_link, expr);

            let body_link = graph.new_term_link(None);
            let body_cont = graph.new_free_occ_for_binder(cont, body_link);
            let body_value = graph.new_free_occ_for_binder(value, body_link);
            let body_args = graph.new_free_vars([body_value]);
            let body_parent = graph.new_parent_link(None);
            let body = graph.new_term(
                body_parent,
                TermKind::Continue(body_cont, body_args),
                Value::new(false),
            );
            graph.set_term_link(body_link, body);

            let term_parent = graph.new_parent_link(None);
            let term = graph.new_term(
                term_parent,
                TermKind::LetVal((value, expr_link), body_link),
                Value::new(false),
            );
            graph.set_term_link(root, term);
            graph.backpatch_subexprs(term, root, &[expr_link]);
            graph.backpatch_subterms(term, &[body_link]);

            let clone = GraphClone::new(ctx, &mut graph)
                .clone_subterm(root)
                .expect("clone");
            let clone_term = graph.read_term_link(clone).expect("clone term");
            let TermKind::LetVal((clone_value, clone_expr), clone_body) = graph[clone_term].kind
            else {
                panic!("expected cloned letval");
            };
            assert_ne!(clone_value, value);
            assert_eq!(graph.read_parent_link(graph[clone_term].link), None);

            let clone_expr = graph.read_expr_link(clone_expr).expect("clone expr");
            let ExprKind::PrimCall(_, clone_expr_vars) = graph[clone_expr].kind else {
                panic!("expected cloned primcall");
            };
            assert_eq!(
                graph.free_binder(graph.free_vars_slice(&clone_expr_vars)[0]),
                external
            );
            assert_eq!(graph.collect_occurrences(external).len(), 2);

            let clone_body = graph.read_term_link(clone_body).expect("clone body");
            let TermKind::Continue(clone_cont, clone_args) = graph[clone_body].kind else {
                panic!("expected cloned continue");
            };
            assert_eq!(graph.free_binder(clone_cont), cont);
            assert_eq!(
                graph.free_binder(graph.free_vars_slice(&clone_args)[0]),
                clone_value
            );
            assert_eq!(
                graph.read_parent_link(graph[clone_body].link),
                Some(Parent::Term(clone_term))
            );
        });
    }

    #[test]
    fn clone_subterm_applies_seeded_binder_substitutions() {
        Scheme::new_uninit().enter(|ctx| {
            let mut graph = Graph::new();
            let root = graph.new_term_link(None);
            let cont = graph.new_bound_var(lvar(ctx, "cont"));
            let value = graph.new_bound_var(lvar(ctx, "value"));
            let replacement = graph.new_bound_var(lvar(ctx, "replacement"));
            let cont_occ = graph.new_free_occ_for_binder(cont, root);
            let value_occ = graph.new_free_occ_for_binder(value, root);
            let args = graph.new_free_vars([value_occ]);
            let term_parent = graph.new_parent_link(None);
            let term = graph.new_term(
                term_parent,
                TermKind::Continue(cont_occ, args),
                Value::new(false),
            );
            graph.set_term_link(root, term);

            let clone = GraphClone::with_substitutions(ctx, &mut graph, [(value, replacement)])
                .clone_subterm(root)
                .expect("clone");
            let clone_term = graph.read_term_link(clone).expect("clone term");
            let TermKind::Continue(_, args) = graph[clone_term].kind else {
                panic!("expected cloned continue");
            };
            assert_eq!(
                graph.free_binder(graph.free_vars_slice(&args)[0]),
                replacement
            );
            assert_eq!(graph.collect_occurrences(value).len(), 1);
            assert_eq!(graph.collect_occurrences(replacement).len(), 1);
        });
    }

    #[test]
    fn clone_fix_rewrites_internal_function_uses_to_fresh_binders() {
        Scheme::new_uninit().enter(|ctx| {
            let mut graph = Graph::new();
            let root = graph.new_term_link(None);
            let f = graph.new_bound_var(lvar(ctx, "f"));
            let x = graph.new_bound_var(lvar(ctx, "x"));
            let ret = graph.new_bound_var(lvar(ctx, "ret"));
            let halt = graph.new_bound_var(lvar(ctx, "halt"));
            let value = graph.new_bound_var(lvar(ctx, "value"));

            let function_body = graph.new_term_link(None);
            let body_ret = graph.new_free_occ_for_binder(ret, function_body);
            let body_x = graph.new_free_occ_for_binder(x, function_body);
            let body_args = graph.new_free_vars([body_x]);
            let body_parent = graph.new_parent_link(None);
            let body_term = graph.new_term(
                body_parent,
                TermKind::Continue(body_ret, body_args),
                Value::new(false),
            );
            graph.set_term_link(function_body, body_term);

            let function_vars = graph.new_bound_vars([x]);
            let function = graph.new_function(Function {
                name: Value::new(false),
                source: Value::new(false),
                var: f,
                vars: function_vars,
                variadic: None,
                cont: Some(ret),
                is_variadic: false,
                body: function_body,
                is_rec: false,
                unroll_count: 0,
                is_cold: false,
                is_noinline: false,
                is_reified: false,
                meta: Value::new(false),
            });
            graph.backpatch_function(function);
            let function_link = graph.new_function_link(Some(function));
            let functions = graph.new_function_links([function_link]);

            let app_link = graph.new_term_link(None);
            let app_f = graph.new_free_occ_for_binder(f, app_link);
            let app_halt = graph.new_free_occ_for_binder(halt, app_link);
            let app_value = graph.new_free_occ_for_binder(value, app_link);
            let app_args = graph.new_free_vars([app_value]);
            let app_parent = graph.new_parent_link(None);
            let app = graph.new_term(
                app_parent,
                TermKind::App(app_f, app_args, app_halt),
                Value::new(false),
            );
            graph.set_term_link(app_link, app);

            let fix_parent = graph.new_parent_link(None);
            let fix = graph.new_term(
                fix_parent,
                TermKind::Fix(functions, app_link),
                Value::new(false),
            );
            graph.set_term_link(root, fix);
            graph.backpatch_subterms(fix, &[app_link]);

            let clone = GraphClone::new(ctx, &mut graph)
                .clone_subterm(root)
                .expect("clone");
            let clone_fix = graph.read_term_link(clone).expect("clone fix");
            let TermKind::Fix(clone_functions, clone_app_link) = graph[clone_fix].kind else {
                panic!("expected cloned fix");
            };
            let clone_function_link = graph.function_links_slice(&clone_functions)[0];
            let clone_function = graph
                .read_function_link(clone_function_link)
                .expect("clone function");
            let clone_data = graph[clone_function];
            assert_ne!(clone_data.var, f);
            assert_ne!(graph.bound_vars_slice(&clone_data.vars)[0], x);
            assert_ne!(clone_data.cont, Some(ret));

            let clone_app = graph.read_term_link(clone_app_link).expect("clone app");
            let TermKind::App(clone_callee, clone_args, clone_halt) = graph[clone_app].kind else {
                panic!("expected cloned app");
            };
            assert_eq!(graph.free_binder(clone_callee), clone_data.var);
            assert_eq!(graph.free_binder(clone_halt), halt);
            assert_eq!(
                graph.free_binder(graph.free_vars_slice(&clone_args)[0]),
                value
            );

            let clone_body = graph.read_term_link(clone_data.body).expect("clone body");
            let TermKind::Continue(clone_ret, clone_body_args) = graph[clone_body].kind else {
                panic!("expected cloned function body");
            };
            assert_eq!(graph.free_binder(clone_ret), clone_data.cont.unwrap());
            assert_eq!(
                graph.free_binder(graph.free_vars_slice(&clone_body_args)[0]),
                graph.bound_vars_slice(&clone_data.vars)[0]
            );
            assert_eq!(
                graph.read_parent_link(graph[clone_body].link),
                Some(Parent::Func(clone_function))
            );
        });
    }
}
