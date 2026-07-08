//! Conversion between tree CPS and graphical CPS.
//!
//! Graphical CPS deliberately does not model arbitrary atoms. Constants in tree
//! CPS are first named with [`Expression::Literal`] lets, and graph terms then
//! refer only to bound-variable occurrences.

use std::{cell::Cell, collections::HashMap, fmt};

use crate::{
    cps::term::{Atom, Cont, ContRef, Expression, Func, FuncRef, Term, TermRef},
    expander::core::{LVarRef, fresh_lvar},
    rsgc::{
        Gc, Trace,
        alloc::{Array, array::ArrayRef},
        cell::Lock,
    },
    runtime::{
        Context,
        value::{Symbol, Value},
    },
};

use super::graph::{
    BranchHint, BoundVar, ContVar, ExprId, ExprKind, FreeVar, Function, FunctionId, FunctionLink,
    FunctionLinks, Graph, Parent, Subexpr, Subterm, TermId, TermKind, TermLink,
};

fn convert_branch_hint(hint: crate::cps::term::BranchHint) -> BranchHint {
    match hint {
        crate::cps::term::BranchHint::Normal => BranchHint::Normal,
        crate::cps::term::BranchHint::Hot => BranchHint::Hot,
        crate::cps::term::BranchHint::Cold => BranchHint::Cold,
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConvertError {
    ConstantAtomInNormalizedCps,
    DeadTermLink(Subterm),
    DeadExprLink(Subexpr),
    DeadFunctionLink(FunctionLink),
    FunctionInFixHasNoReturnContinuation(FunctionId),
    FunctionInLetkHasReturnContinuation(FunctionId),
}

impl fmt::Display for ConvertError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ConstantAtomInNormalizedCps => {
                write!(f, "constant atom remained after CPS literal normalization")
            }
            Self::DeadTermLink(link) => write!(f, "dead term link while lowering graph: {link}"),
            Self::DeadExprLink(link) => {
                write!(f, "dead expression link while lowering graph: {link}")
            }
            Self::DeadFunctionLink(link) => {
                write!(f, "dead function link while lowering graph: {link}")
            }
            Self::FunctionInFixHasNoReturnContinuation(function) => {
                write!(f, "function in fix has no return continuation: {function}")
            }
            Self::FunctionInLetkHasReturnContinuation(function) => {
                write!(
                    f,
                    "continuation in letk has a return continuation: {function}"
                )
            }
        }
    }
}

impl std::error::Error for ConvertError {}

pub type ConvertResult<T> = Result<T, ConvertError>;

pub struct GraphProgram<'gc> {
    pub graph: Graph<'gc>,
    pub root: Subterm,
}

pub struct GraphFunctionProgram<'gc> {
    pub graph: Graph<'gc>,
    pub entry: FunctionId,
}

impl<'gc> GraphFunctionProgram<'gc> {
    pub fn root(&self) -> Subterm {
        self.graph[self.entry].body
    }
}

type LiteralBind<'gc> = (LVarRef<'gc>, Value<'gc>, Value<'gc>);
type GraphLiteralBind<'gc> = (BoundVar, Value<'gc>, Value<'gc>);

struct LiteralNormalizer<'gc> {
    ctx: Context<'gc>,
    fresh_count: u32,
}

impl<'gc> LiteralNormalizer<'gc> {
    fn new(ctx: Context<'gc>) -> Self {
        Self {
            ctx,
            fresh_count: 0,
        }
    }

    fn fresh_variable(&mut self, prefix: &str) -> LVarRef<'gc> {
        let index = self.fresh_count;
        self.fresh_count += 1;
        let name = Symbol::from_str_uninterned(*self.ctx, &format!("gcps-{prefix}{index}"), None);
        fresh_lvar(self.ctx, name.into())
    }

    fn array<T: Copy + Trace>(&self, items: &[T]) -> ArrayRef<'gc, T> {
        Array::from_slice(*self.ctx, items)
    }

    fn extract_atom(
        &mut self,
        atom: Atom<'gc>,
        source: Value<'gc>,
        binds: &mut Vec<LiteralBind<'gc>>,
    ) -> Atom<'gc> {
        match atom {
            Atom::Local(_) => atom,
            Atom::Constant(value) => {
                let binding = self.fresh_variable("literal");
                binds.push((binding, value, source));
                Atom::Local(binding)
            }
        }
    }

    fn extract_atoms(
        &mut self,
        atoms: impl IntoIterator<Item = Atom<'gc>>,
        source: Value<'gc>,
        binds: &mut Vec<LiteralBind<'gc>>,
    ) -> Vec<Atom<'gc>> {
        atoms
            .into_iter()
            .map(|atom| self.extract_atom(atom, source, binds))
            .collect()
    }

    fn extract_optional_atoms(
        &mut self,
        atoms: Option<ArrayRef<'gc, Atom<'gc>>>,
        source: Value<'gc>,
        binds: &mut Vec<LiteralBind<'gc>>,
    ) -> Option<ArrayRef<'gc, Atom<'gc>>> {
        atoms.map(|atoms| {
            let normalized = self.extract_atoms(atoms.iter().copied(), source, binds);
            self.array(&normalized)
        })
    }

    fn wrap_literal_binds(
        &self,
        binds: Vec<LiteralBind<'gc>>,
        mut body: TermRef<'gc>,
    ) -> TermRef<'gc> {
        for (binding, value, source) in binds.into_iter().rev() {
            body = Gc::new(
                *self.ctx,
                Term::Let(binding, Expression::Literal(value, source), body),
            );
        }
        body
    }

    fn normalize_expression(
        &mut self,
        binding: LVarRef<'gc>,
        expr: Expression<'gc>,
        body: TermRef<'gc>,
    ) -> TermRef<'gc> {
        match expr {
            Expression::Literal(value, source) => Gc::new(
                *self.ctx,
                Term::Let(binding, Expression::Literal(value, source), body),
            ),
            Expression::PrimCall(prim, args, source) => {
                let mut binds = Vec::new();
                let args = self.extract_atoms(args.iter().copied(), source, &mut binds);
                let expr = Expression::PrimCall(prim, self.array(&args), source);
                let term = Gc::new(*self.ctx, Term::Let(binding, expr, body));
                self.wrap_literal_binds(binds, term)
            }
        }
    }

    fn normalize_func(&mut self, func: FuncRef<'gc>) -> FuncRef<'gc> {
        let body = self.normalize_term(func.body());
        Gc::new(
            *self.ctx,
            Func {
                name: func.name,
                source: func.source,
                binding: func.binding,
                return_cont: func.return_cont,
                args: func.args,
                variadic: func.variadic,
                body: Lock::new(body),
                free_vars: Lock::new(func.free_vars.get()),
                meta: func.meta,
            },
        )
    }

    fn normalize_cont(&mut self, cont: ContRef<'gc>) -> ContRef<'gc> {
        let body = self.normalize_term(cont.body());
        Gc::new(
            *self.ctx,
            Cont {
                name: cont.name,
                binding: cont.binding,
                args: cont.args,
                variadic: cont.variadic,
                body: Lock::new(body),
                source: cont.source,
                free_vars: Lock::new(cont.free_vars.get()),
                reified: Cell::new(cont.reified.get()),
                cold: cont.cold,
                noinline: cont.noinline,
                meta: cont.meta,
            },
        )
    }

    fn normalize_term(&mut self, term: TermRef<'gc>) -> TermRef<'gc> {
        match *term {
            Term::Continue(k, args, source) => {
                let mut binds = Vec::new();
                let args = self.extract_atoms(args.iter().copied(), source, &mut binds);
                let term = Gc::new(*self.ctx, Term::Continue(k, self.array(&args), source));
                self.wrap_literal_binds(binds, term)
            }
            Term::App(func, k, args, source) => {
                let mut binds = Vec::new();
                let func = self.extract_atom(func, source, &mut binds);
                let args = self.extract_atoms(args.iter().copied(), source, &mut binds);
                let term = Gc::new(*self.ctx, Term::App(func, k, self.array(&args), source));
                self.wrap_literal_binds(binds, term)
            }
            Term::Raise { kind, args, source } => {
                let mut binds = Vec::new();
                let args = self.extract_atoms(args.iter().copied(), source, &mut binds);
                let term = Gc::new(
                    *self.ctx,
                    Term::Raise {
                        kind,
                        args: self.array(&args),
                        source,
                    },
                );
                self.wrap_literal_binds(binds, term)
            }
            Term::If {
                test,
                consequent,
                consequent_args,
                alternative,
                alternative_args,
                hints,
            } => {
                let source = Value::new(false);
                let mut binds = Vec::new();
                let test = self.extract_atom(test, source, &mut binds);
                let consequent_args =
                    self.extract_optional_atoms(consequent_args, source, &mut binds);
                let alternative_args =
                    self.extract_optional_atoms(alternative_args, source, &mut binds);
                let term = Gc::new(
                    *self.ctx,
                    Term::If {
                        test,
                        consequent,
                        consequent_args,
                        alternative,
                        alternative_args,
                        hints,
                    },
                );
                self.wrap_literal_binds(binds, term)
            }
            Term::Let(binding, expr, body) => {
                let body = self.normalize_term(body);
                self.normalize_expression(binding, expr, body)
            }
            Term::Fix(funcs, body) => {
                let funcs: Vec<_> = funcs
                    .iter()
                    .copied()
                    .map(|func| self.normalize_func(func))
                    .collect();
                let body = self.normalize_term(body);
                Gc::new(
                    *self.ctx,
                    Term::Fix(Array::from_slice(*self.ctx, &funcs), body),
                )
            }
            Term::Letk(conts, body) => {
                let conts: Vec<_> = conts
                    .iter()
                    .copied()
                    .map(|cont| self.normalize_cont(cont))
                    .collect();
                let body = self.normalize_term(body);
                Gc::new(
                    *self.ctx,
                    Term::Letk(Array::from_slice(*self.ctx, &conts), body),
                )
            }
        }
    }
}

/// Rewrite constant atoms in tree CPS to `Expression::Literal` lets.
pub fn normalize_literals<'gc>(ctx: Context<'gc>, term: TermRef<'gc>) -> TermRef<'gc> {
    LiteralNormalizer::new(ctx).normalize_term(term)
}

pub fn cps_to_graph<'gc>(
    ctx: Context<'gc>,
    term: TermRef<'gc>,
) -> ConvertResult<GraphProgram<'gc>> {
    build_cps_graph(term, Some(ctx))
}

pub fn cps_func_to_graph<'gc>(
    ctx: Context<'gc>,
    func: FuncRef<'gc>,
) -> ConvertResult<GraphFunctionProgram<'gc>> {
    build_cps_func_graph(func, Some(ctx))
}

pub fn normalized_cps_to_graph<'gc>(term: TermRef<'gc>) -> ConvertResult<GraphProgram<'gc>> {
    build_cps_graph(term, None)
}

pub fn normalized_cps_func_to_graph<'gc>(
    func: FuncRef<'gc>,
) -> ConvertResult<GraphFunctionProgram<'gc>> {
    build_cps_func_graph(func, None)
}

fn build_cps_graph<'gc>(
    term: TermRef<'gc>,
    ctx: Option<Context<'gc>>,
) -> ConvertResult<GraphProgram<'gc>> {
    let mut graph = Graph::new();
    let mut builder = ToGraph {
        graph: &mut graph,
        vars: HashMap::new(),
        ctx,
        fresh_count: 0,
    };
    let root = builder.convert_root(term)?;
    Ok(GraphProgram { graph, root })
}

fn build_cps_func_graph<'gc>(
    func: FuncRef<'gc>,
    ctx: Option<Context<'gc>>,
) -> ConvertResult<GraphFunctionProgram<'gc>> {
    let mut graph = Graph::new();
    let mut builder = ToGraph {
        graph: &mut graph,
        vars: HashMap::new(),
        ctx,
        fresh_count: 0,
    };
    let entry = builder.convert_func(func, false)?;
    Ok(GraphFunctionProgram { graph, entry })
}

struct ToGraph<'a, 'gc> {
    graph: &'a mut Graph<'gc>,
    vars: HashMap<LVarRef<'gc>, BoundVar>,
    ctx: Option<Context<'gc>>,
    fresh_count: u32,
}

impl<'gc> ToGraph<'_, 'gc> {
    fn fresh_variable(&mut self, prefix: &str) -> ConvertResult<LVarRef<'gc>> {
        let Some(ctx) = self.ctx else {
            return Err(ConvertError::ConstantAtomInNormalizedCps);
        };
        let index = self.fresh_count;
        self.fresh_count += 1;
        let name = Symbol::from_str_uninterned(*ctx, &format!("gcps-{prefix}{index}"), None);
        Ok(fresh_lvar(ctx, name.into()))
    }

    fn bind_lvar(&mut self, lvar: LVarRef<'gc>) -> BoundVar {
        if let Some(bound) = self.vars.get(&lvar) {
            return *bound;
        }

        let bound = self.graph.new_bound_var(lvar);
        self.vars.insert(lvar, bound);
        bound
    }

    fn use_lvar(&mut self, lvar: LVarRef<'gc>, owner: TermLink) -> FreeVar {
        let bound = self.bind_lvar(lvar);
        self.graph.new_free_occ_for_binder(bound, owner)
    }

    fn use_literal_atom(
        &mut self,
        value: Value<'gc>,
        source: Value<'gc>,
        owner: TermLink,
        binds: &mut Vec<GraphLiteralBind<'gc>>,
    ) -> ConvertResult<FreeVar> {
        let binding = self.fresh_variable("literal")?;
        let bound = self.bind_lvar(binding);
        binds.push((bound, value, source));
        Ok(self.graph.new_free_occ_for_binder(bound, owner))
    }

    fn use_atom(
        &mut self,
        atom: Atom<'gc>,
        source: Value<'gc>,
        owner: TermLink,
        binds: &mut Vec<GraphLiteralBind<'gc>>,
    ) -> ConvertResult<FreeVar> {
        match atom {
            Atom::Local(lvar) => Ok(self.use_lvar(lvar, owner)),
            Atom::Constant(value) => self.use_literal_atom(value, source, owner, binds),
        }
    }

    fn use_atoms(
        &mut self,
        atoms: impl IntoIterator<Item = Atom<'gc>>,
        source: Value<'gc>,
        owner: TermLink,
        binds: &mut Vec<GraphLiteralBind<'gc>>,
    ) -> ConvertResult<super::graph::FreeVars> {
        let vars: Vec<_> = atoms
            .into_iter()
            .map(|atom| self.use_atom(atom, source, owner, binds))
            .collect::<ConvertResult<_>>()?;
        Ok(self.graph.new_free_vars(vars))
    }

    fn convert_root(&mut self, term: TermRef<'gc>) -> ConvertResult<Subterm> {
        let root = self.graph.new_term_link(None);
        self.convert_term_into(term, root)?;
        Ok(root)
    }

    fn convert_child(&mut self, term: TermRef<'gc>) -> ConvertResult<Subterm> {
        let link = self.graph.new_term_link(None);
        self.convert_term_into(term, link)?;
        Ok(link)
    }

    fn retarget_direct_free_owners(&mut self, term: TermId, owner: TermLink) {
        let mut vars = Vec::new();
        self.graph.push_direct_free_vars_of_term(term, &mut vars);
        if let TermKind::LetVal((_, expr), _) = self.graph[term].kind {
            if let Some(expr) = self.graph.read_expr_link(expr) {
                self.graph.push_free_vars_of_expr(expr, &mut vars);
            }
        }

        for var in vars {
            self.graph.set_free_owner(var, owner);
        }
    }

    fn wrap_literal_binds(
        &mut self,
        owner: TermLink,
        actual_term: TermId,
        binds: Vec<GraphLiteralBind<'gc>>,
    ) -> TermId {
        if binds.is_empty() {
            return actual_term;
        }

        let old_parent = self.graph.read_parent_link(self.graph[actual_term].link);
        let mut child_link = self.graph.new_term_link(Some(actual_term));
        self.retarget_direct_free_owners(actual_term, child_link);

        let mut child_term = actual_term;
        let mut outer = actual_term;
        let mut iter = binds.into_iter().rev().peekable();
        while let Some((binding, value, source)) = iter.next() {
            let expr_link = self.graph.new_expr_link(None);
            let expr_uplink = self.graph.new_parent_link(None);
            let expr = self
                .graph
                .new_expr(expr_uplink, ExprKind::Literal(value), source);
            self.graph.set_expr_link(expr_link, expr);

            let wrapper_uplink = self.graph.new_parent_link(None);
            let wrapper = self.graph.new_term(
                wrapper_uplink,
                TermKind::LetVal((binding, expr_link), child_link),
                source,
            );
            self.graph
                .set_parent_link(self.graph[child_term].link, Parent::Term(wrapper));
            self.graph
                .set_parent_link(self.graph[expr].link, Parent::Term(wrapper));

            outer = wrapper;
            child_term = wrapper;
            if iter.peek().is_some() {
                child_link = self.graph.new_term_link(Some(wrapper));
            }
        }

        if let Some(parent) = old_parent {
            self.graph.set_parent_link(self.graph[outer].link, parent);
        }
        self.graph.set_term_link(owner, outer);
        outer
    }

    fn convert_expression(
        &mut self,
        expr: Expression<'gc>,
        owner: TermLink,
        binds: &mut Vec<GraphLiteralBind<'gc>>,
    ) -> ConvertResult<Subexpr> {
        let link = self.graph.new_expr_link(None);
        let uplink = self.graph.new_parent_link(None);
        let (kind, source) = match expr {
            Expression::Literal(value, source) => (ExprKind::Literal(value), source),
            Expression::PrimCall(prim, args, source) => {
                let args = self.use_atoms(args.iter().copied(), source, owner, binds)?;
                (ExprKind::PrimCall(prim, args), source)
            }
        };
        let expr = self.graph.new_expr(uplink, kind, source);
        self.graph.set_expr_link(link, expr);
        Ok(link)
    }

    fn convert_branch_continue(
        &mut self,
        cont: LVarRef<'gc>,
        args: Option<ArrayRef<'gc, Atom<'gc>>>,
        binds: &mut Vec<GraphLiteralBind<'gc>>,
    ) -> ConvertResult<Subterm> {
        let link = self.graph.new_term_link(None);
        let uplink = self.graph.new_parent_link(None);
        let cont = self.use_lvar(cont, link);
        let vars = match args {
            Some(args) => self.use_atoms(args.iter().copied(), Value::new(false), link, binds)?,
            None => self.graph.new_free_vars([]),
        };
        let term = self
            .graph
            .new_term(uplink, TermKind::Continue(cont, vars), Value::new(false));
        self.graph.set_term_link(link, term);
        Ok(link)
    }

    fn convert_functions(
        &mut self,
        funcs: impl IntoIterator<Item = FuncRef<'gc>>,
        is_rec: bool,
    ) -> ConvertResult<FunctionLinks> {
        let funcs: Vec<_> = funcs.into_iter().collect();
        let mut links = Vec::with_capacity(funcs.len());

        for func in &funcs {
            self.bind_lvar(func.binding);
            links.push(self.graph.new_function_link(None));
        }

        for (func, link) in funcs.into_iter().zip(links.iter().copied()) {
            let function = self.convert_func(func, is_rec)?;
            self.graph.set_function_link(link, function);
        }

        Ok(self.graph.new_function_links(links))
    }

    fn convert_conts(
        &mut self,
        conts: impl IntoIterator<Item = ContRef<'gc>>,
    ) -> ConvertResult<FunctionLinks> {
        let conts: Vec<_> = conts.into_iter().collect();
        let mut links = Vec::with_capacity(conts.len());

        for cont in &conts {
            self.bind_lvar(cont.binding);
            links.push(self.graph.new_function_link(None));
        }

        for (cont, link) in conts.into_iter().zip(links.iter().copied()) {
            let function = self.convert_cont(cont)?;
            self.graph.set_function_link(link, function);
        }

        Ok(self.graph.new_function_links(links))
    }

    fn convert_func(&mut self, func: FuncRef<'gc>, is_rec: bool) -> ConvertResult<FunctionId> {
        let var = self.bind_lvar(func.binding);
        let cont = self.bind_lvar(func.return_cont);
        let vars: Vec<_> = func
            .args
            .iter()
            .copied()
            .map(|arg| self.bind_lvar(arg))
            .collect();
        let variadic = func.variadic.map(|arg| self.bind_lvar(arg));
        let vars = self.graph.new_bound_vars(vars);
        let body = self.convert_child(func.body())?;
        let function = self.graph.new_function(Function {
            name: func.name,
            source: func.source,
            var,
            vars,
            variadic,
            cont: Some(cont),
            is_variadic: variadic.is_some(),
            body,
            is_rec,
            unroll_count: 0,
            is_cold: false,
            is_noinline: false,
            is_reified: false,
            meta: func.meta,
        });
        self.graph.backpatch_function(function);
        Ok(function)
    }

    fn convert_cont(&mut self, cont: ContRef<'gc>) -> ConvertResult<FunctionId> {
        let var = self.bind_lvar(cont.binding);
        let vars: Vec<_> = cont
            .args
            .iter()
            .copied()
            .map(|arg| self.bind_lvar(arg))
            .collect();
        let variadic = cont.variadic.map(|arg| self.bind_lvar(arg));
        let vars = self.graph.new_bound_vars(vars);
        let body = self.convert_child(cont.body())?;
        let function = self.graph.new_function(Function {
            name: cont.name,
            source: cont.source,
            var,
            vars,
            variadic,
            cont: None,
            is_variadic: variadic.is_some(),
            body,
            is_rec: false,
            unroll_count: 0,
            is_cold: cont.cold,
            is_noinline: cont.noinline,
            is_reified: cont.reified.get(),
            meta: cont.meta,
        });
        self.graph.backpatch_function(function);
        Ok(function)
    }

    fn convert_term_into(&mut self, term: TermRef<'gc>, owner: TermLink) -> ConvertResult<TermId> {
        let uplink = self.graph.new_parent_link(None);
        let source = term.source();
        let mut literal_binds = Vec::new();
        let kind = match *term {
            Term::Continue(k, args, source) => {
                let cont = self.use_lvar(k, owner);
                let args =
                    self.use_atoms(args.iter().copied(), source, owner, &mut literal_binds)?;
                self.graph
                    .new_term(uplink, TermKind::Continue(cont, args), source)
            }
            Term::App(func, k, args, source) => {
                let func = self.use_atom(func, source, owner, &mut literal_binds)?;
                let args =
                    self.use_atoms(args.iter().copied(), source, owner, &mut literal_binds)?;
                let cont = self.use_lvar(k, owner);
                self.graph
                    .new_term(uplink, TermKind::App(func, args, cont), source)
            }
            Term::Raise { kind, args, source } => {
                let args =
                    self.use_atoms(args.iter().copied(), source, owner, &mut literal_binds)?;
                self.graph
                    .new_term(uplink, TermKind::Raise(kind, args), source)
            }
            Term::If {
                test,
                consequent,
                consequent_args,
                alternative,
                alternative_args,
                hints,
            } => {
                let branch_source = Value::new(false);
                let test = self.use_atom(test, branch_source, owner, &mut literal_binds)?;
                let consequent =
                    self.convert_branch_continue(consequent, consequent_args, &mut literal_binds)?;
                let alternative = self.convert_branch_continue(
                    alternative,
                    alternative_args,
                    &mut literal_binds,
                )?;
                let graph_hints = [
                    convert_branch_hint(hints[0]),
                    convert_branch_hint(hints[1]),
                ];
                self.graph.new_term(
                    uplink,
                    TermKind::If(test, consequent, alternative, graph_hints),
                    source,
                )
            }
            Term::Let(binding, expr, body) => {
                let expr = self.convert_expression(expr, owner, &mut literal_binds)?;
                let binding = self.bind_lvar(binding);
                let body = self.convert_child(body)?;
                self.graph
                    .new_term(uplink, TermKind::LetVal((binding, expr), body), source)
            }
            Term::Fix(funcs, body) => {
                let functions = self.convert_functions(funcs.iter().copied(), true)?;
                let body = self.convert_child(body)?;
                self.graph
                    .new_term(uplink, TermKind::Fix(functions, body), source)
            }
            Term::Letk(conts, body) => {
                let functions = self.convert_conts(conts.iter().copied())?;
                let body = self.convert_child(body)?;
                self.graph
                    .new_term(uplink, TermKind::Letk(functions, body), source)
            }
        };

        self.graph.set_term_link(owner, kind);
        let subterms = self.graph.subterms_of(kind);
        self.graph.backpatch_subterms(kind, &subterms);
        let subexprs = self.graph.subexprs_of(kind);
        self.graph.backpatch_subexprs(kind, owner, &subexprs);
        Ok(self.wrap_literal_binds(owner, kind, literal_binds))
    }
}

pub fn graph_to_cps<'gc>(
    ctx: Context<'gc>,
    graph: &Graph<'gc>,
    root: Subterm,
) -> ConvertResult<TermRef<'gc>> {
    FromGraph {
        ctx,
        graph,
        fresh_count: 0,
        known_literals: HashMap::new(),
    }
    .lower_term_link(root)
}

pub fn graph_func_to_cps<'gc>(
    ctx: Context<'gc>,
    graph: &Graph<'gc>,
    entry: FunctionId,
) -> ConvertResult<FuncRef<'gc>> {
    FromGraph {
        ctx,
        graph,
        fresh_count: 0,
        known_literals: HashMap::new(),
    }
    .lower_func(entry)
}

struct FromGraph<'a, 'gc> {
    ctx: Context<'gc>,
    graph: &'a Graph<'gc>,
    fresh_count: u32,
    known_literals: HashMap<BoundVar, Value<'gc>>,
}

impl<'gc> FromGraph<'_, 'gc> {
    fn fresh_variable(&mut self, prefix: &str) -> LVarRef<'gc> {
        let index = self.fresh_count;
        self.fresh_count += 1;
        let name = Symbol::from_str_uninterned(*self.ctx, &format!("gcps-{prefix}{index}"), None);
        fresh_lvar(self.ctx, name.into())
    }

    fn bound_lvar(&self, var: BoundVar) -> LVarRef<'gc> {
        self.graph[var].var
    }

    fn free_atom(&self, var: FreeVar) -> Atom<'gc> {
        Atom::Local(self.bound_lvar(self.graph.free_binder(var)))
    }

    fn free_lvar(&self, var: ContVar) -> LVarRef<'gc> {
        self.bound_lvar(self.graph.free_binder(var))
    }

    fn free_atoms(&self, vars: &super::graph::FreeVars) -> Vec<Atom<'gc>> {
        self.graph
            .free_vars_slice(vars)
            .iter()
            .copied()
            .map(|var| self.free_atom(var))
            .collect()
    }

    fn literal_atom(&self, var: FreeVar) -> Option<Atom<'gc>> {
        let binder = self.graph.free_binder(var);
        self.known_literals
            .get(&binder)
            .copied()
            .map(Atom::Constant)
    }

    fn free_atoms_for_prim(
        &self,
        prim: Value<'gc>,
        vars: &super::graph::FreeVars,
    ) -> Vec<Atom<'gc>> {
        let mut args = self.free_atoms(vars);
        let name = prim.downcast::<Symbol>().to_string();
        if matches!(name.as_str(), "cache-ref" | "cache-set!") {
            if let Some(first) = self.graph.free_vars_slice(vars).first().copied() {
                if let Some(literal) = self.literal_atom(first) {
                    args[0] = literal;
                }
            }
        }
        args
    }

    fn bound_vars(&self, vars: &super::graph::BoundVars) -> Vec<LVarRef<'gc>> {
        self.graph
            .bound_vars_slice(vars)
            .iter()
            .copied()
            .map(|var| self.bound_lvar(var))
            .collect()
    }

    fn atoms_array(&self, atoms: &[Atom<'gc>]) -> ArrayRef<'gc, Atom<'gc>> {
        Array::from_slice(*self.ctx, atoms)
    }

    fn vars_array(&self, vars: &[LVarRef<'gc>]) -> ArrayRef<'gc, LVarRef<'gc>> {
        Array::from_slice(*self.ctx, vars)
    }

    fn lower_term_link(&mut self, link: Subterm) -> ConvertResult<TermRef<'gc>> {
        let term = self
            .graph
            .read_term_link(link)
            .ok_or(ConvertError::DeadTermLink(link))?;
        self.lower_term(term)
    }

    fn lower_expr_link(&mut self, link: Subexpr) -> ConvertResult<Expression<'gc>> {
        let expr = self
            .graph
            .read_expr_link(link)
            .ok_or(ConvertError::DeadExprLink(link))?;
        self.lower_expr(expr)
    }

    fn lower_expr(&mut self, expr: ExprId) -> ConvertResult<Expression<'gc>> {
        let expr_data = self.graph[expr];
        Ok(match expr_data.kind {
            ExprKind::Literal(value) => Expression::Literal(value, expr_data.source),
            ExprKind::PrimCall(prim, vars) => {
                let args = self.free_atoms_for_prim(prim, &vars);
                Expression::PrimCall(prim, self.atoms_array(&args), expr_data.source)
            }
        })
    }

    fn lower_function_link_as_func(&mut self, link: FunctionLink) -> ConvertResult<FuncRef<'gc>> {
        let function = self
            .graph
            .read_function_link(link)
            .ok_or(ConvertError::DeadFunctionLink(link))?;
        self.lower_func(function)
    }

    fn lower_function_link_as_cont(&mut self, link: FunctionLink) -> ConvertResult<ContRef<'gc>> {
        let function = self
            .graph
            .read_function_link(link)
            .ok_or(ConvertError::DeadFunctionLink(link))?;
        self.lower_cont(function)
    }

    fn lower_func(&mut self, function: FunctionId) -> ConvertResult<FuncRef<'gc>> {
        let data = self.graph[function];
        let Some(return_cont) = data.cont else {
            return Err(ConvertError::FunctionInFixHasNoReturnContinuation(function));
        };
        let args = self.bound_vars(&data.vars);
        let body = self.lower_term_link(data.body)?;
        Ok(Gc::new(
            *self.ctx,
            Func {
                name: data.name,
                source: data.source,
                binding: self.bound_lvar(data.var),
                return_cont: self.bound_lvar(return_cont),
                args: self.vars_array(&args),
                variadic: data.variadic.map(|var| self.bound_lvar(var)),
                body: Lock::new(body),
                free_vars: Lock::new(None),
                meta: data.meta,
            },
        ))
    }

    fn lower_cont(&mut self, function: FunctionId) -> ConvertResult<ContRef<'gc>> {
        let data = self.graph[function];
        if data.cont.is_some() {
            return Err(ConvertError::FunctionInLetkHasReturnContinuation(function));
        }
        let args = self.bound_vars(&data.vars);
        let body = self.lower_term_link(data.body)?;
        Ok(Gc::new(
            *self.ctx,
            Cont {
                name: data.name,
                binding: self.bound_lvar(data.var),
                args: self.vars_array(&args),
                variadic: data.variadic.map(|var| self.bound_lvar(var)),
                body: Lock::new(body),
                source: data.source,
                free_vars: Lock::new(None),
                reified: Cell::new(data.is_reified),
                cold: data.is_cold,
                noinline: data.is_noinline,
                meta: data.meta,
            },
        ))
    }

    fn lower_direct_continue(
        &self,
        link: Subterm,
    ) -> ConvertResult<Option<(LVarRef<'gc>, Option<ArrayRef<'gc, Atom<'gc>>>)>> {
        let Some(term) = self.graph.read_term_link(link) else {
            return Err(ConvertError::DeadTermLink(link));
        };

        let TermKind::Continue(cont, args) = self.graph[term].kind else {
            return Ok(None);
        };

        let args = self.free_atoms(&args);
        let args = if args.is_empty() {
            None
        } else {
            Some(self.atoms_array(&args))
        };
        Ok(Some((self.free_lvar(cont), args)))
    }

    fn lower_if(
        &mut self,
        test: FreeVar,
        consequent: Subterm,
        alternative: Subterm,
        hints: [BranchHint; 2],
    ) -> ConvertResult<TermRef<'gc>> {
        let cps_hints = [
            convert_branch_hint_reverse(hints[0]),
            convert_branch_hint_reverse(hints[1]),
        ];
        if let (Some((consequent, consequent_args)), Some((alternative, alternative_args))) = (
            self.lower_direct_continue(consequent)?,
            self.lower_direct_continue(alternative)?,
        ) {
            return Ok(Gc::new(
                *self.ctx,
                Term::If {
                    test: self.free_atom(test),
                    consequent,
                    consequent_args,
                    alternative,
                    alternative_args,
                    hints: cps_hints,
                },
            ));
        }

        let consequent_name = self.fresh_variable("if-consequent");
        let alternative_name = self.fresh_variable("if-alternative");
        let consequent_body = self.lower_term_link(consequent)?;
        let alternative_body = self.lower_term_link(alternative)?;
        let empty_vars: [LVarRef<'gc>; 0] = [];
        let conts = [
            Gc::new(
                *self.ctx,
                Cont {
                    name: Value::new(false),
                    binding: consequent_name,
                    args: self.vars_array(&empty_vars),
                    variadic: None,
                    body: Lock::new(consequent_body),
                    source: Value::new(false),
                    free_vars: Lock::new(None),
                    reified: Cell::new(false),
                    cold: false,
                    noinline: false,
                    meta: Value::new(false),
                },
            ),
            Gc::new(
                *self.ctx,
                Cont {
                    name: Value::new(false),
                    binding: alternative_name,
                    args: self.vars_array(&empty_vars),
                    variadic: None,
                    body: Lock::new(alternative_body),
                    source: Value::new(false),
                    free_vars: Lock::new(None),
                    reified: Cell::new(false),
                    cold: false,
                    noinline: false,
                    meta: Value::new(false),
                },
            ),
        ];
        let body = Gc::new(
            *self.ctx,
            Term::If {
                test: self.free_atom(test),
                consequent: consequent_name,
                consequent_args: None,
                alternative: alternative_name,
                alternative_args: None,
                hints: cps_hints,
            },
        );
        Ok(Gc::new(
            *self.ctx,
            Term::Letk(Array::from_slice(*self.ctx, &conts), body),
        ))
    }

    fn lower_term(&mut self, term: TermId) -> ConvertResult<TermRef<'gc>> {
        let term_data = self.graph[term];
        Ok(match term_data.kind {
            TermKind::LetVal((binding, expr), body) => {
                if let Some(expr_id) = self.graph.read_expr_link(expr) {
                    if let ExprKind::Literal(value) = self.graph[expr_id].kind {
                        self.known_literals.insert(binding, value);
                    }
                }
                let expr = self.lower_expr_link(expr)?;
                let body = self.lower_term_link(body)?;
                Gc::new(*self.ctx, Term::Let(self.bound_lvar(binding), expr, body))
            }
            TermKind::Fix(functions, body) => {
                let funcs: Vec<_> = self
                    .graph
                    .function_links_slice(&functions)
                    .iter()
                    .copied()
                    .filter_map(|link| {
                        self.graph
                            .read_function_link(link)
                            .map(|_| self.lower_function_link_as_func(link))
                    })
                    .collect::<ConvertResult<_>>()?;
                let body = self.lower_term_link(body)?;
                if funcs.is_empty() {
                    return Ok(body);
                }
                Gc::new(
                    *self.ctx,
                    Term::Fix(Array::from_slice(*self.ctx, &funcs), body),
                )
            }
            TermKind::Letk(functions, body) => {
                let conts: Vec<_> = self
                    .graph
                    .function_links_slice(&functions)
                    .iter()
                    .copied()
                    .filter_map(|link| {
                        self.graph
                            .read_function_link(link)
                            .map(|_| self.lower_function_link_as_cont(link))
                    })
                    .collect::<ConvertResult<_>>()?;
                let body = self.lower_term_link(body)?;
                if conts.is_empty() {
                    return Ok(body);
                }
                Gc::new(
                    *self.ctx,
                    Term::Letk(Array::from_slice(*self.ctx, &conts), body),
                )
            }
            TermKind::If(test, consequent, alternative, hints) => {
                return self.lower_if(test, consequent, alternative, hints);
            }
            TermKind::Continue(cont, args) => {
                let args = self.free_atoms(&args);
                Gc::new(
                    *self.ctx,
                    Term::Continue(
                        self.free_lvar(cont),
                        self.atoms_array(&args),
                        term_data.source,
                    ),
                )
            }
            TermKind::App(func, args, cont) => {
                let args = self.free_atoms(&args);
                Gc::new(
                    *self.ctx,
                    Term::App(
                        self.free_atom(func),
                        self.free_lvar(cont),
                        self.atoms_array(&args),
                        term_data.source,
                    ),
                )
            }
            TermKind::Raise(kind, args) => {
                let args = self.free_atoms(&args);
                Gc::new(
                    *self.ctx,
                    Term::Raise {
                        kind,
                        args: self.atoms_array(&args),
                        source: term_data.source,
                    },
                )
            }
        })
    }
}

fn convert_branch_hint_reverse(hint: BranchHint) -> crate::cps::term::BranchHint {
    match hint {
        BranchHint::Normal => crate::cps::term::BranchHint::Normal,
        BranchHint::Hot => crate::cps::term::BranchHint::Hot,
        BranchHint::Cold => crate::cps::term::BranchHint::Cold,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{runtime::Scheme, runtime::vm::exceptions::RaiseKind};

    fn lvar<'gc>(ctx: Context<'gc>, name: &str) -> LVarRef<'gc> {
        fresh_lvar(ctx, ctx.intern(name))
    }

    #[test]
    fn normalize_literals_names_constant_atoms() {
        Scheme::new_uninit().enter(|ctx| {
            let k = lvar(ctx, "k");
            let one = Value::new(1);
            let source = Value::new(false);
            let term = Gc::new(
                *ctx,
                Term::Continue(k, Array::from_slice(*ctx, &[Atom::Constant(one)]), source),
            );

            let normalized = normalize_literals(ctx, term);
            let Term::Let(binding, Expression::Literal(value, literal_source), body) = *normalized
            else {
                panic!("expected literal let");
            };

            assert_eq!(value, one);
            assert_eq!(literal_source, source);
            let Term::Continue(cont, args, _) = *body else {
                panic!("expected continue body");
            };
            assert_eq!(cont, k);
            assert_eq!(args.as_slice(), &[Atom::Local(binding)]);
        });
    }


}
