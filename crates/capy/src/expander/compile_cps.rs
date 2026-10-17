use std::collections::HashMap;
use std::mem::offset_of;
use std::sync::OnceLock;

use crate::compiler::cps::{
    convert::{ConvertResult, GraphFunctionProgram},
    graph::{
        BoundVar, BranchHint, BranchOp, ExprKind, FreeVar, Function, FunctionId, FunctionLinks,
        Graph, Parent, Subexpr, Subterm, TermId, TermKind as GraphTermKind, TermLink,
    },
};
use crate::expander::core::{
    LVarRef, LetStyle, Proc, TermKind, TermRef as CoreTermRef, fresh_lvar, seq_from_slice,
};
use crate::heap::alloc::array::Array;
use crate::heap::object::builtin_class_ids;
use crate::heap::{Gc, Global, Trace, barrier};
use crate::list;
use crate::runtime::MutationContext;
use crate::runtime::prelude::*;
use crate::runtime::value::Value;
use crate::runtime::value::{Str, Vector};
use crate::runtime::vm::exceptions::RaiseKind;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Atom<'gc> {
    Constant(Value<'gc>),
    Local(LVarRef<'gc>),
}

impl<'gc> From<LVarRef<'gc>> for Atom<'gc> {
    fn from(value: LVarRef<'gc>) -> Self {
        Self::Local(value)
    }
}

impl<'gc> From<Value<'gc>> for Atom<'gc> {
    fn from(value: Value<'gc>) -> Self {
        Self::Constant(value)
    }
}

/// Attachment mode: where a term sits
/// relative to `with-continuation-mark` scopes in its frame. Threaded
/// through [`convert`], procedure bodies start at [`AttachmentMode::Tail`].
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AttachmentMode {
    /// Tail position, attachment state unknown.
    Tail,
    /// Tail position with our attachment on top: exits through the
    /// reified frame's underflow restore, which pops for true calls.
    TailSome,
    /// Non-tail position, no attachment of ours on top.
    NonNone,
    /// Non-tail position with our attachment on top: exits pop first.
    NonSome,
}

impl AttachmentMode {
    fn in_some_scope(self) -> bool {
        matches!(self, AttachmentMode::TailSome | AttachmentMode::NonSome)
    }
}

/// Shape of a continuation variable, recorded at [`GraphCpsBuilder::make_cont`]
/// so attachment pop-wraps can forward values unchanged.
#[derive(Clone, Copy, Debug)]
struct ContShape {
    /// Fixed parameters.
    nargs: usize,
    /// Takes a rest list after the fixed parameters.
    has_rest: bool,
    /// A `receive` consumer: multiple values may flow, and the rest list
    /// may be used, so forwarding cannot drop it.
    is_consumer: bool,
}

pub struct GraphCpsBuilder<'gc> {
    pub ctx: MutationContext<'gc>,
    graph: Graph<'gc>,
    vars: HashMap<LVarRef<'gc>, BoundVar>,
    cont_shapes: HashMap<LVarRef<'gc>, ContShape>,
    varcount: u32,
    scope_id: u32,
    current_topbox_scope: Option<u32>,
    current_meta: Value<'gc>,
}

impl<'gc> GraphCpsBuilder<'gc> {
    pub fn new(ctx: MutationContext<'gc>) -> Self {
        Self {
            ctx,
            graph: Graph::new(),
            vars: HashMap::new(),
            cont_shapes: HashMap::new(),
            varcount: 0,
            scope_id: 0,
            current_topbox_scope: None,
            current_meta: Value::null(),
        }
    }

    pub fn finish(self, entry: FunctionId) -> GraphFunctionProgram<'gc> {
        GraphFunctionProgram {
            graph: self.graph,
            entry,
        }
    }

    pub fn fresh_variable(&mut self, prefix: &str) -> LVarRef<'gc> {
        let ix = self.varcount;
        let var = fresh_lvar(
            self.ctx,
            Symbol::from_str_uninterned(self.ctx, &format!("{prefix}{ix}"), None).into(),
        );
        self.varcount += 1;
        var
    }

    fn enter_scope(&mut self) {
        self.current_topbox_scope = Some(self.scope_id);
        self.scope_id += 1;
    }

    fn bind_lvar(&mut self, lvar: LVarRef<'gc>) -> BoundVar {
        if let Some(bound) = self.vars.get(&lvar) {
            return *bound;
        }

        let bound = self.graph.new_bound_var(lvar);
        self.vars.insert(lvar, bound);
        bound
    }

    fn use_lvar(&mut self, lvar: LVarRef<'gc>, owner: Subterm) -> FreeVar {
        let bound = self.bind_lvar(lvar);
        self.graph.new_free_occ_for_binder(bound, owner)
    }

    fn use_atom(
        &mut self,
        atom: Atom<'gc>,
        source: Value<'gc>,
        owner: Subterm,
        binds: &mut Vec<LiteralBind<'gc>>,
    ) -> FreeVar {
        match atom {
            Atom::Local(lvar) => self.use_lvar(lvar, owner),
            Atom::Constant(value) => {
                let binding = self.fresh_variable("literal");
                let bound = self.bind_lvar(binding);
                binds.push((bound, value, source));
                self.graph.new_free_occ_for_binder(bound, owner)
            }
        }
    }

    fn use_atoms(
        &mut self,
        atoms: impl IntoIterator<Item = Atom<'gc>>,
        source: Value<'gc>,
        owner: Subterm,
        binds: &mut Vec<LiteralBind<'gc>>,
    ) -> crate::compiler::cps::graph::FreeVars {
        let vars = atoms
            .into_iter()
            .map(|atom| self.use_atom(atom, source, owner, binds))
            .collect::<Vec<_>>();
        self.graph.new_free_vars(vars)
    }

    fn new_child_link(&mut self) -> Subterm {
        self.graph.new_term_link(None)
    }

    fn retarget_direct_free_owners(&mut self, term: TermId, owner: TermLink) {
        let mut vars = Vec::new();
        self.graph.push_direct_free_vars_of_term(term, &mut vars);
        let expr = match self.graph[term].kind {
            GraphTermKind::LetVal((_, expr), _) => self.graph.read_expr_link(expr),
            _ => None,
        };
        if let Some(expr) = expr {
            self.graph.push_free_vars_of_expr(expr, &mut vars);
        }

        for var in vars {
            self.graph.set_free_owner(var, owner);
        }
    }

    fn wrap_literal_binds(
        &mut self,
        owner: Subterm,
        actual_term: TermId,
        binds: Vec<LiteralBind<'gc>>,
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
                GraphTermKind::LetVal((binding, expr_link), child_link),
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

    fn emit_term(
        &mut self,
        owner: Subterm,
        kind: GraphTermKind,
        source: Value<'gc>,
        literal_binds: Vec<LiteralBind<'gc>>,
    ) -> TermId {
        let uplink = self.graph.new_parent_link(None);
        let term = self.graph.new_term(uplink, kind, source);
        self.graph.set_term_link(owner, term);
        let subterms = self.graph.subterms_of(term);
        self.graph.backpatch_subterms(term, &subterms);
        let subexprs = self.graph.subexprs_of(term);
        self.graph.backpatch_subexprs(term, owner, &subexprs);
        self.wrap_literal_binds(owner, term, literal_binds)
    }

    fn emit_expr(&mut self, kind: ExprKind<'gc>, source: Value<'gc>) -> Subexpr {
        let link = self.graph.new_expr_link(None);
        let uplink = self.graph.new_parent_link(None);
        let expr = self.graph.new_expr(uplink, kind, source);
        self.graph.set_expr_link(link, expr);
        link
    }

    fn emit_continue(
        &mut self,
        owner: Subterm,
        k: LVarRef<'gc>,
        args: impl IntoIterator<Item = Atom<'gc>>,
        source: Value<'gc>,
    ) -> TermId {
        let mut literal_binds = Vec::new();
        let cont = self.use_lvar(k, owner);
        let args = self.use_atoms(args, source, owner, &mut literal_binds);
        self.emit_term(
            owner,
            GraphTermKind::Continue(cont, args),
            source,
            literal_binds,
        )
    }

    /// `(branch-prim (prim args...) (k-true true-args...) (k-false false-args...))`.
    #[allow(clippy::too_many_arguments)]
    fn emit_branch_prim(
        &mut self,
        owner: Subterm,
        prim: crate::runtime::prims::def::Primitive,
        args: Vec<Atom<'gc>>,
        consequent: LVarRef<'gc>,
        true_args: Vec<Atom<'gc>>,
        alternative: LVarRef<'gc>,
        false_args: Vec<Atom<'gc>>,
        hints: [BranchHint; 2],
        source: Value<'gc>,
    ) -> TermId {
        let op = BranchOp {
            prim,
            nargs: u8::try_from(args.len()).expect("branch primitive takes few operands"),
            ntrue: u8::try_from(true_args.len()).expect("continuation takes few arguments"),
            hints,
        };
        let mut literal_binds = Vec::new();
        let consequent = self.use_lvar(consequent, owner);
        let alternative = self.use_lvar(alternative, owner);
        let operands = args.into_iter().chain(true_args).chain(false_args);
        let vars = self.use_atoms(operands, source, owner, &mut literal_binds);
        self.emit_term(
            owner,
            GraphTermKind::BranchPrim(op, consequent, alternative, vars),
            source,
            literal_binds,
        )
    }

    fn emit_raise(
        &mut self,
        owner: Subterm,
        kind: RaiseKind,
        args: impl IntoIterator<Item = Atom<'gc>>,
        source: Value<'gc>,
    ) -> TermId {
        let mut literal_binds = Vec::new();
        let args = self.use_atoms(args, source, owner, &mut literal_binds);
        self.emit_term(
            owner,
            GraphTermKind::Raise(kind, args),
            source,
            literal_binds,
        )
    }

    fn emit_app(
        &mut self,
        owner: Subterm,
        func: Atom<'gc>,
        k: LVarRef<'gc>,
        args: impl IntoIterator<Item = Atom<'gc>>,
        source: Value<'gc>,
    ) -> TermId {
        let mut literal_binds = Vec::new();
        let func = self.use_atom(func, source, owner, &mut literal_binds);
        let args = self.use_atoms(args, source, owner, &mut literal_binds);
        let cont = self.use_lvar(k, owner);
        self.emit_term(
            owner,
            GraphTermKind::App(func, args, cont),
            source,
            literal_binds,
        )
    }

    fn emit_if(
        &mut self,
        owner: Subterm,
        test: Atom<'gc>,
        consequent: Subterm,
        alternate: Subterm,
        hints: [BranchHint; 2],
        source: Value<'gc>,
    ) -> TermId {
        let mut literal_binds = Vec::new();
        let test = self.use_atom(test, source, owner, &mut literal_binds);
        self.emit_term(
            owner,
            GraphTermKind::If(test, consequent, alternate, hints),
            source,
            literal_binds,
        )
    }

    #[allow(clippy::too_many_arguments)]
    fn emit_if_continue(
        &mut self,
        owner: Subterm,
        test: Atom<'gc>,
        consequent: LVarRef<'gc>,
        consequent_args: &[Atom<'gc>],
        alternate: LVarRef<'gc>,
        alternate_args: &[Atom<'gc>],
        hints: [BranchHint; 2],
        source: Value<'gc>,
    ) -> TermId {
        let consequent_link = self.new_child_link();
        self.emit_continue(
            consequent_link,
            consequent,
            consequent_args.iter().copied(),
            source,
        );
        let alternate_link = self.new_child_link();
        self.emit_continue(
            alternate_link,
            alternate,
            alternate_args.iter().copied(),
            source,
        );
        self.emit_if(owner, test, consequent_link, alternate_link, hints, source)
    }

    fn emit_let_val(
        &mut self,
        owner: Subterm,
        binding: LVarRef<'gc>,
        expr: Subexpr,
        body: Subterm,
        source: Value<'gc>,
        literal_binds: Vec<LiteralBind<'gc>>,
    ) -> TermId {
        let binding = self.bind_lvar(binding);
        self.emit_term(
            owner,
            GraphTermKind::LetVal((binding, expr), body),
            source,
            literal_binds,
        )
    }

    fn emit_prim_let(
        &mut self,
        owner: Subterm,
        binding: LVarRef<'gc>,
        prim: Value<'gc>,
        args: impl IntoIterator<Item = Atom<'gc>>,
        source: Value<'gc>,
        body: Subterm,
    ) -> TermId {
        let mut literal_binds = Vec::new();
        let args = self.use_atoms(args, source, owner, &mut literal_binds);
        let expr = self.emit_expr(ExprKind::PrimCall(prim, args), source);
        self.emit_let_val(owner, binding, expr, body, source, literal_binds)
    }

    fn let_prim(
        &mut self,
        owner: Subterm,
        prefix: &str,
        prim: Value<'gc>,
        args: impl IntoIterator<Item = Atom<'gc>>,
        source: Value<'gc>,
        body: impl FnOnce(&mut Self, Subterm, LVarRef<'gc>) -> TermId,
    ) -> TermId {
        let binding = self.fresh_variable(prefix);
        let body_link = self.new_child_link();
        body(self, body_link, binding);
        self.emit_prim_let(owner, binding, prim, args, source, body_link)
    }

    fn bind_literal(
        &mut self,
        owner: Subterm,
        value: Value<'gc>,
        source: Value<'gc>,
        body: impl FnOnce(&mut Self, Subterm, Atom<'gc>) -> TermId,
    ) -> TermId {
        let binding = self.fresh_variable("literal");
        let body_link = self.new_child_link();
        body(self, body_link, Atom::Local(binding));
        let expr = self.emit_expr(ExprKind::Literal(value), source);
        self.emit_let_val(owner, binding, expr, body_link, source, Vec::new())
    }

    fn function_links(&mut self, functions: &[FunctionId]) -> FunctionLinks {
        let links = functions
            .iter()
            .copied()
            .map(|function| {
                let link = self.graph.new_function_link(None);
                self.graph.set_function_link(link, function);
                link
            })
            .collect::<Vec<_>>();
        self.graph.new_function_links(links)
    }

    fn emit_fix(
        &mut self,
        owner: Subterm,
        functions: &[FunctionId],
        body: Subterm,
        source: Value<'gc>,
    ) -> TermId {
        let links = self.function_links(functions);
        self.emit_term(owner, GraphTermKind::Fix(links, body), source, Vec::new())
    }

    fn emit_letk(
        &mut self,
        owner: Subterm,
        continuations: &[FunctionId],
        body: Subterm,
        source: Value<'gc>,
    ) -> TermId {
        let links = self.function_links(continuations);
        self.emit_term(owner, GraphTermKind::Letk(links, body), source, Vec::new())
    }

    fn new_function(&mut self, function: Function<'gc>) -> FunctionId {
        let function = self.graph.new_function(function);
        self.graph.backpatch_function(function);
        function
    }

    #[allow(clippy::too_many_arguments)]
    fn make_cont(
        &mut self,
        binding: LVarRef<'gc>,
        args: &[LVarRef<'gc>],
        variadic: Option<LVarRef<'gc>>,
        body: Subterm,
        source: Value<'gc>,
        cold: bool,
        noinline: bool,
        meta: Value<'gc>,
    ) -> FunctionId {
        self.cont_shapes.insert(
            binding,
            ContShape {
                nargs: args.len(),
                has_rest: variadic.is_some(),
                is_consumer: false,
            },
        );
        let var = self.bind_lvar(binding);
        let args = args
            .iter()
            .copied()
            .map(|arg| self.bind_lvar(arg))
            .collect::<Vec<_>>();
        let variadic = variadic.map(|arg| self.bind_lvar(arg));
        let vars = self.graph.new_bound_vars(args);
        self.new_function(Function {
            name: Value::new(false),
            source,
            var,
            vars,
            variadic,
            cont: None,
            is_variadic: variadic.is_some(),
            body,
            is_rec: false,
            is_cold: cold,
            is_noinline: noinline,
            is_reified: false,
            meta,
        })
    }

    fn make_proc_func(
        &mut self,
        proc: &Proc<'gc>,
        binding: LVarRef<'gc>,
        is_rec: bool,
    ) -> FunctionId {
        let old_meta = self.current_meta;
        self.current_meta = proc.meta;
        let return_cont = self.fresh_variable("return");

        let var = self.bind_lvar(binding);
        let cont = self.bind_lvar(return_cont);
        let args = proc
            .args
            .iter()
            .copied()
            .map(|arg| self.bind_lvar(arg))
            .collect::<Vec<_>>();
        let variadic = proc.variadic.map(|arg| self.bind_lvar(arg));
        let vars = self.graph.new_bound_vars(args);
        let body = self.new_child_link();
        convert(self, proc.body, return_cont, body, AttachmentMode::Tail);
        self.current_meta = old_meta;

        self.new_function(Function {
            name: proc.name,
            source: proc.source,
            var,
            vars,
            variadic,
            cont: Some(cont),
            is_variadic: variadic.is_some(),
            body,
            is_rec,
            is_cold: false,
            is_noinline: false,
            is_reified: false,
            meta: proc.meta,
        })
    }
}

type LiteralBind<'gc> = (BoundVar, Value<'gc>, Value<'gc>);

pub fn cps_func<'gc>(
    builder: &mut GraphCpsBuilder<'gc>,
    proc: &Proc<'gc>,
    binding: LVarRef<'gc>,
) -> FunctionId {
    builder.make_proc_func(proc, binding, false)
}

pub fn cps_toplevel<'gc>(
    ctx: MutationContext<'gc>,
    forms: &[CoreTermRef<'gc>],
) -> ConvertResult<GraphFunctionProgram<'gc>> {
    let form = if forms.len() == 1 {
        forms[0]
    } else if forms.is_empty() {
        panic!("Cannot compile an empty program to CPS")
    } else {
        let source = forms
            .iter()
            .find(|f| f.source() != Value::new(false))
            .map(|f| f.source())
            .unwrap_or(Value::new(false));

        let seq = seq_from_slice(ctx, forms);
        barrier::field!(Gc::write(ctx, seq), super::core::Term, source)
            .unlock()
            .set(source);
        seq
    };

    let proc = Proc {
        args: Array::from_slice(ctx, []),
        name: Value::new(false),
        body: form,
        source: form.source(),
        variadic: None,
        meta: Value::null(),
    };

    let mut builder = GraphCpsBuilder::new(ctx);
    let bind = builder.fresh_variable("toplevel");
    let entry = cps_func(&mut builder, &proc, bind);
    Ok(builder.finish(entry))
}

pub type PrimitiveTransformer = for<'gc> fn(
    cps: &mut GraphCpsBuilder<'gc>,
    owner: Subterm,
    src: Value<'gc>,
    op: Value<'gc>,
    params: &[Atom<'gc>],
    k: LVarRef<'gc>,
) -> Option<TermId>;

pub struct PrimitiveTable<'gc> {
    pub table: HashMap<Value<'gc>, PrimitiveTransformer>,
}

// SAFETY: `PrimitiveTable` stores GC values as keys and traces them below.
unsafe impl<'gc> Trace for PrimitiveTable<'gc> {
    unsafe fn trace(&mut self, visitor: &mut crate::heap::collection::Visitor) {
        for key in self.table.keys() {
            // SAFETY: The key is a GC-managed value stored in the table.
            unsafe {
                let value = key as *const Value<'gc> as *mut Value<'gc>;
                (*value).trace(visitor);
            }
        }
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::heap::WeakProcessor) {
        let _ = weak_processor;
    }
}

type RootedPrimitiveTable = crate::Rootable!(PrimitiveTable<'_>);
type ArgContinuation<'a, 'gc> =
    Box<dyn FnOnce(&mut GraphCpsBuilder<'gc>, Subterm, Atom<'gc>) -> TermId + 'a>;
type ArgsContinuation<'a, 'gc> =
    Box<dyn FnOnce(&mut GraphCpsBuilder<'gc>, Subterm, Vec<Atom<'gc>>) -> TermId + 'a>;

static PRIMTABLE: OnceLock<Global<RootedPrimitiveTable>> = OnceLock::new();

macro_rules! primitive_transformers {
    ($(
        $prim: literal => $name : ident ($cps: ident, $owner: ident, $src: ident, $op: ident, $args: ident, $k: ident) $b: block
    )*) => {
        #[allow(dead_code, unused_mut, unused_variables)]
        fn make_primitive_table<'gc>(ctx: MutationContext<'gc>) -> HashMap<Value<'gc>, PrimitiveTransformer> {
            let mut table: HashMap<Value<'gc>, PrimitiveTransformer> = HashMap::new();
            $(
                table.insert(
                    Value::new(Symbol::from_str(ctx, $prim)),
                    { fn $name<'gc>(
                        $cps: &mut GraphCpsBuilder<'gc>,
                        $owner: Subterm,
                        $src: Value<'gc>,
                        $op: Value<'gc>,
                        $args: &[Atom<'gc>],
                        $k: LVarRef<'gc>,
                    ) -> Option<TermId> {
                        $b
                    } $name },
                );
            )*
            table
        }
    };
}

pub fn get_primitive_table<'gc>(ctx: MutationContext<'gc>) -> &'gc PrimitiveTable<'gc> {
    PRIMTABLE
        .get_or_init(|| {
            let table = make_primitive_table(ctx);
            Global::new(PrimitiveTable { table })
        })
        .fetch(ctx)
}

impl<'gc> PrimitiveTable<'gc> {
    pub fn try_expand(
        &self,
        cps: &mut GraphCpsBuilder<'gc>,
        owner: Subterm,
        src: Value<'gc>,
        prim: Value<'gc>,
        args: &[Atom<'gc>],
        k: LVarRef<'gc>,
    ) -> Option<TermId> {
        self.table
            .get(&prim)
            .and_then(|transformer| transformer(cps, owner, src, prim, args, k))
    }
}

pub fn assertion_violation<'gc>(
    cps: &mut GraphCpsBuilder<'gc>,
    owner: Subterm,
    src: Value<'gc>,
    opc: Value<'gc>,
    message: &str,
    irritants: &[Atom<'gc>],
) -> TermId {
    let message = Atom::Constant(Value::new(Str::new(cps.ctx, message, true)));
    let opc = Atom::Constant(opc);
    let mut args = vec![opc, message];
    args.extend_from_slice(irritants);
    cps.emit_raise(owner, RaiseKind::AssertionViolation, args, src)
}

pub fn primitive_wrong_number_of_arguments<'gc>(
    cps: &mut GraphCpsBuilder<'gc>,
    owner: Subterm,
    src: Value<'gc>,
    kind: RaiseKind,
    got: usize,
) -> TermId {
    cps.emit_raise(owner, kind, [Atom::Constant(Value::new(got as i32))], src)
}

pub fn ensure_string<'gc>(
    cps: &mut GraphCpsBuilder<'gc>,
    owner: Subterm,
    src: Value<'gc>,
    op: Value<'gc>,
    x: Atom<'gc>,
    have_length: impl FnOnce(&mut GraphCpsBuilder<'gc>, Subterm, Atom<'gc>) -> TermId,
) -> TermId {
    let not_string = cps.fresh_variable("not_string");
    let k = cps.fresh_variable("k");
    let check_string = cps.fresh_variable("check_string");

    let not_string_body = cps.new_child_link();
    assertion_violation(cps, not_string_body, src, op, "not a string", &[x]);
    let not_string_cont = cps.make_cont(
        not_string,
        &[],
        None,
        not_string_body,
        Value::new(false),
        true,
        false,
        Value::new(false),
    );

    let k_body = cps.new_child_link();
    cps.let_prim(
        k_body,
        "length",
        Symbol::from_str(cps.ctx, "%refptr").into(),
        [
            x,
            Atom::Constant(Value::new(offset_of!(Str, length) as i32)),
        ],
        src,
        |cps, owner, length| have_length(cps, owner, Atom::Local(length)),
    );
    let k_cont = cps.make_cont(
        k,
        &[],
        None,
        k_body,
        Value::new(false),
        false,
        false,
        Value::new(false),
    );

    let check_body = cps.new_child_link();
    cps.let_prim(
        check_body,
        "is_string",
        Symbol::from_str(cps.ctx, "%class-id?").into(),
        [
            x,
            Atom::Constant(Value::new(builtin_class_ids::STRING as i32)),
        ],
        src,
        |cps, owner, is_string| {
            cps.emit_if_continue(
                owner,
                Atom::Local(is_string),
                k,
                &[],
                not_string,
                &[],
                [BranchHint::Normal, BranchHint::Normal],
                Value::new(false),
            )
        },
    );
    let check_cont = cps.make_cont(
        check_string,
        &[],
        None,
        check_body,
        Value::new(false),
        false,
        false,
        Value::new(false),
    );

    let body = cps.new_child_link();
    cps.let_prim(
        body,
        "is_immediate",
        Symbol::from_str(cps.ctx, "immediate?").into(),
        [x],
        src,
        |cps, owner, is_immediate| {
            cps.emit_if_continue(
                owner,
                Atom::Local(is_immediate),
                not_string,
                &[],
                check_string,
                &[],
                [BranchHint::Normal, BranchHint::Normal],
                Value::new(false),
            )
        },
    );

    cps.emit_letk(
        owner,
        &[not_string_cont, k_cont, check_cont],
        body,
        Value::new(false),
    )
}

pub fn ensure_pair<'gc>(
    cps: &mut GraphCpsBuilder<'gc>,
    owner: Subterm,
    src: Value<'gc>,
    op: Value<'gc>,
    x: Atom<'gc>,
    have_pair: impl FnOnce(&mut GraphCpsBuilder<'gc>, Subterm, Atom<'gc>) -> TermId,
) -> TermId {
    let not_pair = cps.fresh_variable("not_pair");
    let k = cps.fresh_variable("k");

    let not_pair_body = cps.new_child_link();
    assertion_violation(cps, not_pair_body, src, op, "not a pair", &[x]);
    let not_pair_cont = cps.make_cont(
        not_pair,
        &[],
        None,
        not_pair_body,
        Value::new(false),
        true,
        false,
        Value::new(false),
    );

    let k_body = cps.new_child_link();
    have_pair(cps, k_body, x);
    let k_cont = cps.make_cont(
        k,
        &[],
        None,
        k_body,
        Value::new(false),
        false,
        false,
        Value::new(false),
    );

    let body = cps.new_child_link();
    cps.let_prim(
        body,
        "is_pair",
        Symbol::from_str(cps.ctx, "pair?").into(),
        [x],
        src,
        |cps, owner, is_pair| {
            cps.emit_if_continue(
                owner,
                Atom::Local(is_pair),
                k,
                &[],
                not_pair,
                &[],
                [BranchHint::Normal, BranchHint::Normal],
                Value::new(false),
            )
        },
    );

    cps.emit_letk(owner, &[not_pair_cont, k_cont], body, Value::new(false))
}

pub fn ensure_vector<'gc>(
    cps: &mut GraphCpsBuilder<'gc>,
    owner: Subterm,
    src: Value<'gc>,
    op: Value<'gc>,
    x: Atom<'gc>,
    have_length: impl FnOnce(&mut GraphCpsBuilder<'gc>, Subterm, Atom<'gc>) -> TermId,
) -> TermId {
    let not_vector = cps.fresh_variable("not_vector");
    let k = cps.fresh_variable("k");

    let not_vector_body = cps.new_child_link();
    assertion_violation(cps, not_vector_body, src, op, "not a vector", &[x]);
    let not_vector_cont = cps.make_cont(
        not_vector,
        &[],
        None,
        not_vector_body,
        Value::new(false),
        true,
        false,
        Value::new(false),
    );

    let k_body = cps.new_child_link();
    cps.let_prim(
        k_body,
        "length",
        Symbol::from_str(cps.ctx, "%refptr").into(),
        [
            x,
            Atom::Constant(Value::new(offset_of!(Vector, length) as i32)),
        ],
        src,
        |cps, owner, length| have_length(cps, owner, Atom::Local(length)),
    );
    let k_cont = cps.make_cont(
        k,
        &[],
        None,
        k_body,
        Value::new(false),
        false,
        false,
        Value::new(false),
    );

    let body = cps.new_child_link();
    cps.let_prim(
        body,
        "is_vector",
        Symbol::from_str(cps.ctx, "vector?").into(),
        [x],
        src,
        |cps, owner, is_vector| {
            cps.emit_if_continue(
                owner,
                Atom::Local(is_vector),
                k,
                &[],
                not_vector,
                &[],
                [BranchHint::Normal, BranchHint::Normal],
                Value::new(false),
            )
        },
    );

    cps.emit_letk(owner, &[not_vector_cont, k_cont], body, Value::new(false))
}

fn ensure_variable<'gc>(
    cps: &mut GraphCpsBuilder<'gc>,
    owner: Subterm,
    src: Value<'gc>,
    op: Value<'gc>,
    x: Atom<'gc>,
    have_variable: impl FnOnce(&mut GraphCpsBuilder<'gc>, Subterm, Atom<'gc>) -> TermId,
) -> TermId {
    let not_variable = cps.fresh_variable("not_variable");
    let k = cps.fresh_variable("k");

    let not_variable_body = cps.new_child_link();
    assertion_violation(
        cps,
        not_variable_body,
        src,
        op,
        &format!("not a variable {}", not_variable.name),
        &[x],
    );
    let not_variable_cont = cps.make_cont(
        not_variable,
        &[],
        None,
        not_variable_body,
        Value::new(false),
        true,
        false,
        Value::new(false),
    );

    let k_body = cps.new_child_link();
    have_variable(cps, k_body, x);
    let k_cont = cps.make_cont(
        k,
        &[],
        None,
        k_body,
        Value::new(false),
        false,
        false,
        Value::new(false),
    );

    let body = cps.new_child_link();
    cps.let_prim(
        body,
        "is_variable",
        Symbol::from_str(cps.ctx, "variable?").into(),
        [x],
        src,
        |cps, owner, is_variable| {
            cps.emit_if_continue(
                owner,
                Atom::Local(is_variable),
                k,
                &[],
                not_variable,
                &[],
                [BranchHint::Normal, BranchHint::Normal],
                Value::new(false),
            )
        },
    );

    cps.emit_letk(owner, &[not_variable_cont, k_cont], body, Value::new(false))
}

primitive_transformers!(
    "assertion-violation" => assertion_violation_primitive(cps, owner, src, _op, args, _k) {
        // The direct raise ABI has four fixed payload slots.  Keep larger
        // variadic calls on the ordinary primitive path so all irritants are
        // preserved.
        if args.len() > 4 {
            return None;
        }
        if args.len() < 2 {
            return Some(primitive_wrong_number_of_arguments(
                cps, owner, src, RaiseKind::WrongNumberOfArguments, args.len(),
            ));
        }

        Some(cps.emit_raise(
            owner,
            RaiseKind::AssertionViolation,
            args.iter().copied(),
            src,
        ))
    }

    "string-length" => string_length(cps, owner, src, op, args, k) {
        let x = args.first().copied()?;
        if let Atom::Constant(val) = x
            && val.is::<Str>() {
            return Some(cps.emit_continue(
                owner,
                k,
                [Atom::Constant(Value::new(val.downcast::<Str>().len() as i32))],
                Value::new(false),
            ));
        }
        Some(ensure_string(cps, owner, src, op, x, |cps, owner, len| {
            cps.let_prim(
                owner,
                "vlen",
                Symbol::from_str(cps.ctx, "usize->value").into(),
                [len],
                src,
                |cps, owner, vlen| cps.emit_continue(owner, k, [Atom::Local(vlen)], Value::new(false)),
            )
        }))
    }

    "vector-length" => vector_length(cps, owner, src, op, args, k) {
        let x = args.first().copied()?;
        if let Atom::Constant(val) = x
            && val.is::<Vector>() {
            return Some(cps.emit_continue(
                owner,
                k,
                [Atom::Constant(Value::new(val.downcast::<Vector>().len() as i32))],
                Value::new(false),
            ));
        }
        Some(ensure_vector(cps, owner, src, op, x, |cps, owner, len| {
            cps.let_prim(
                owner,
                "vlen",
                Symbol::from_str(cps.ctx, "usize->value").into(),
                [len],
                src,
                |cps, owner, vlen| cps.emit_continue(owner, k, [Atom::Local(vlen)], Value::new(false)),
            )
        }))
    }

    "car" => car(cps, owner, src, _op, args, k) {
        let Some(_x) = (args.len() == 1).then(|| args[0]) else {
            return Some(primitive_wrong_number_of_arguments(
                cps,
                owner,
                src,
                RaiseKind::WrongNumberOfArgumentsCar,
                args.len(),
            ));
        };

        Some(cps.let_prim(
            owner,
            "car",
            Symbol::from_str(cps.ctx, "car").into(),
            [args[0]],
            src,
            |cps, owner, car| cps.emit_continue(owner, k, [Atom::Local(car)], Value::new(false)),
        ))
    }

    "cdr" => cdr(cps, owner, src, _op, args, k) {
        let Some(_x) = (args.len() == 1).then(|| args[0]) else {
            return Some(primitive_wrong_number_of_arguments(
                cps,
                owner,
                src,
                RaiseKind::WrongNumberOfArgumentsCdr,
                args.len(),
            ));
        };

        Some(cps.let_prim(
            owner,
            "cdr",
            Symbol::from_str(cps.ctx, "cdr").into(),
            [args[0]],
            src,
            |cps, owner, cdr| cps.emit_continue(owner, k, [Atom::Local(cdr)], src),
        ))
    }

    "set-car!" => set_car(cps, owner, src, op, args, k) {
        let Some((pair, value)) = (args.len() == 2).then(|| (args[0], args[1])) else {
            return Some(primitive_wrong_number_of_arguments(
                cps,
                owner,
                src,
                RaiseKind::WrongNumberOfArgumentsSetCar,
                args.len(),
            ));
        };
        let undef = Atom::Constant(Value::undefined());
        Some(ensure_pair(cps, owner, src, op, pair, |cps, owner, pair| {
            cps.let_prim(
                owner,
                "_v",
                Symbol::from_str(cps.ctx, "set-car!").into(),
                [pair, value],
                src,
                |cps, owner, _v| cps.emit_continue(owner, k, [undef], src),
            )
        }))
    }

    "set-cdr!" => set_cdr(cps, owner, src, op, args, k) {
        let Some((pair, value)) = (args.len() == 2).then(|| (args[0], args[1])) else {
            return Some(primitive_wrong_number_of_arguments(
                cps,
                owner,
                src,
                RaiseKind::WrongNumberOfArgumentsSetCdr,
                args.len(),
            ));
        };
        let undef = Atom::Constant(Value::undefined());
        Some(ensure_pair(cps, owner, src, op, pair, |cps, owner, pair| {
            cps.let_prim(
                owner,
                "_v",
                Symbol::from_str(cps.ctx, "set-cdr!").into(),
                [pair, value],
                src,
                |cps, owner, _v| cps.emit_continue(owner, k, [undef], src),
            )
        }))
    }

    "variable-ref" => variable_ref(cps, owner, src, op, args, k) {
        let Some(x) = (args.len() == 1).then(|| args[0]) else {
            return Some(primitive_wrong_number_of_arguments(
                cps,
                owner,
                src,
                RaiseKind::WrongNumberOfArgumentsVariableRef,
                args.len(),
            ));
        };

        Some(ensure_variable(cps, owner, src, op, x, |cps, owner, var| {
            cps.let_prim(
                owner,
                "value",
                Symbol::from_str(cps.ctx, "variable-ref").into(),
                [var],
                src,
                |cps, owner, value| cps.emit_continue(owner, k, [Atom::Local(value)], src),
            )
        }))
    }

    "variable-set!" => variable_set(cps, owner, src, op, args, k) {
        let Some((var, value)) = (args.len() == 2).then(|| (args[0], args[1])) else {
            return Some(primitive_wrong_number_of_arguments(
                cps,
                owner,
                src,
                RaiseKind::WrongNumberOfArgumentsVariableSet,
                args.len(),
            ));
        };
        let undef = Atom::Constant(Value::undefined());
        Some(ensure_variable(cps, owner, src, op, var, |cps, owner, var| {
            cps.let_prim(
                owner,
                "_v",
                Symbol::from_str(cps.ctx, "variable-set!").into(),
                [var, value],
                src,
                |cps, owner, _v| cps.emit_continue(owner, k, [undef], src),
            )
        }))
    }

    "variable-bound?" => variable_bound(cps, owner, src, op, args, k) {
        let Some(x) = (args.len() == 1).then(|| args[0]) else {
            return Some(primitive_wrong_number_of_arguments(
                cps,
                owner,
                src,
                RaiseKind::WrongNumberOfArgumentsVariableBound,
                args.len(),
            ));
        };

        Some(ensure_variable(cps, owner, src, op, x, |cps, owner, var| {
            cps.let_prim(
                owner,
                "bound",
                Symbol::from_str(cps.ctx, "variable-bound?").into(),
                [var],
                src,
                |cps, owner, bound| cps.emit_continue(owner, k, [Atom::Local(bound)], src),
            )
        }))
    }
);

pub fn toplevel_box<'gc>(
    cps: &mut GraphCpsBuilder<'gc>,
    owner: Subterm,
    src: Value<'gc>,
    name: Value<'gc>,
    bound: bool,
    have_var: impl FnOnce(&mut GraphCpsBuilder<'gc>, Subterm, Atom<'gc>) -> TermId,
) -> TermId {
    match cps.current_topbox_scope {
        None => {
            let lookup = if bound { "lookup-bound" } else { "lookup" };
            cps.let_prim(
                owner,
                "module",
                Symbol::from_str(cps.ctx, "current-module").into(),
                [],
                src,
                |cps, owner, module| {
                    cps.let_prim(
                        owner,
                        "variable",
                        Symbol::from_str(cps.ctx, lookup).into(),
                        [Atom::Local(module), Atom::Constant(name)],
                        src,
                        |cps, owner, variable| have_var(cps, owner, Atom::Local(variable)),
                    )
                },
            )
        }

        Some(scope) => {
            let kbox = cps.fresh_variable("kbox");
            let box_ = cps.fresh_variable("box_");
            let cont_body = cps.new_child_link();
            have_var(cps, cont_body, Atom::Local(box_));
            let cont = cps.make_cont(
                kbox,
                &[box_],
                None,
                cont_body,
                Value::new(false),
                false,
                false,
                Value::new(false),
            );
            let body = cps.new_child_link();
            cached_toplevel_box(cps, body, kbox, src, Value::new(scope as i32), name, bound);
            cps.emit_letk(owner, &[cont], body, Value::new(false))
        }
    }
}

#[allow(clippy::too_many_arguments)]
pub fn module_box<'gc>(
    cps: &mut GraphCpsBuilder<'gc>,
    owner: Subterm,
    val_proc: impl FnOnce(&mut GraphCpsBuilder<'gc>, Subterm, Atom<'gc>) -> TermId,
    module: Value<'gc>,
    name: Value<'gc>,
    public: bool,
    bound: bool,
    src: Value<'gc>,
) -> TermId {
    let _ = bound;
    let kbox = cps.fresh_variable("kbox");
    let var = cps.fresh_variable("var");
    let cont_body = cps.new_child_link();
    val_proc(cps, cont_body, Atom::Local(var));
    let cont = cps.make_cont(
        kbox,
        &[var],
        None,
        cont_body,
        Value::new(false),
        false,
        false,
        Value::new(false),
    );
    let body = cps.new_child_link();
    cached_module_box(cps, body, kbox, src, module, name, public);
    cps.emit_letk(owner, &[cont], body, Value::new(false))
}

pub fn capture_toplevel_scope<'gc>(
    cps: &mut GraphCpsBuilder<'gc>,
    owner: Subterm,
    src: Value<'gc>,
    scope_id: u32,
    fk: impl FnOnce(&mut GraphCpsBuilder<'gc>, Subterm) -> TermId,
) -> TermId {
    cps.let_prim(
        owner,
        "module",
        Symbol::from_str(cps.ctx, "current-module").into(),
        [],
        src,
        |cps, owner, module| {
            cache_current_module(
                cps,
                owner,
                src,
                Atom::Constant(Value::new(scope_id as i32)),
                Atom::Local(module),
                fk,
            )
        },
    )
}

pub fn cached_toplevel_box<'gc>(
    cps: &mut GraphCpsBuilder<'gc>,
    owner: Subterm,
    k: LVarRef<'gc>,
    src: Value<'gc>,
    scope: Value<'gc>,
    name: Value<'gc>,
    bound: bool,
) -> TermId {
    let cache_key = Value::cons(cps.ctx, scope, name);
    let merge = cps.fresh_variable("merge");
    let cached_arg = cps.fresh_variable("cached");
    let merge_body = cps.new_child_link();
    cps.emit_continue(merge_body, k, [Atom::Local(cached_arg)], Value::new(false));
    let merge_cont = cps.make_cont(
        merge,
        &[cached_arg],
        None,
        merge_body,
        Value::new(false),
        false,
        true,
        Value::new(false),
    );

    let kinit = cps.fresh_variable("kinit");
    let kinit_body = cps.new_child_link();
    cps.let_prim(
        kinit_body,
        "module",
        Symbol::from_str(cps.ctx, "cache-ref").into(),
        [Atom::Constant(scope)],
        src,
        |cps, owner, module| {
            reify_lookup(cps, owner, src, module, name, bound, |cps, owner, var| {
                cps.let_prim(
                    owner,
                    "_k",
                    Symbol::from_str(cps.ctx, "cache-set!").into(),
                    [Atom::Constant(cache_key), var],
                    src,
                    |cps, owner, _k| cps.emit_continue(owner, merge, [var], Value::new(false)),
                )
            })
        },
    );
    let kinit_cont = cps.make_cont(
        kinit,
        &[],
        None,
        kinit_body,
        Value::new(false),
        true,
        false,
        Value::new(false),
    );

    let kok = cps.fresh_variable("kok");
    let body = cps.new_child_link();
    cps.let_prim(
        body,
        "cached",
        Symbol::from_str(cps.ctx, "cache-ref").into(),
        [Atom::Constant(cache_key)],
        src,
        |cps, owner, cached_value| {
            let kok_body = cps.new_child_link();
            cps.emit_continue(
                kok_body,
                merge,
                [Atom::Local(cached_value)],
                Value::new(false),
            );
            let kok_cont = cps.make_cont(
                kok,
                &[],
                None,
                kok_body,
                Value::new(false),
                false,
                false,
                Value::new(false),
            );
            cps.let_prim(
                owner,
                "is_heap_obj",
                Symbol::from_str(cps.ctx, "heap-object?").into(),
                [Atom::Local(cached_value)],
                src,
                |cps, owner, is_heap_obj| {
                    let if_body = cps.new_child_link();
                    cps.emit_if_continue(
                        if_body,
                        Atom::Local(is_heap_obj),
                        kok,
                        &[],
                        kinit,
                        &[],
                        [BranchHint::Normal, BranchHint::Normal],
                        Value::new(false),
                    );
                    cps.emit_letk(owner, &[kok_cont], if_body, Value::new(false))
                },
            )
        },
    );
    cps.emit_letk(owner, &[merge_cont, kinit_cont], body, Value::new(false))
}

pub fn cached_module_box<'gc>(
    cps: &mut GraphCpsBuilder<'gc>,
    owner: Subterm,
    k: LVarRef<'gc>,
    src: Value<'gc>,
    module: Value<'gc>,
    name: Value<'gc>,
    public: bool,
) -> TermId {
    let cache_key = Value::cons(cps.ctx, module, Value::cons(cps.ctx, name, public.into()));
    let merge = cps.fresh_variable("merge");
    let cached_arg = cps.fresh_variable("cached");
    let merge_body = cps.new_child_link();
    cps.emit_continue(merge_body, k, [Atom::Local(cached_arg)], Value::new(false));
    let merge_cont = cps.make_cont(
        merge,
        &[cached_arg],
        None,
        merge_body,
        Value::new(false),
        false,
        true,
        Value::new(false),
    );

    let kinit = cps.fresh_variable("kinit");
    let kinit_body = cps.new_child_link();
    let lookup = if public {
        "lookup-bound-public"
    } else {
        "lookup-bound-private"
    };
    cps.let_prim(
        kinit_body,
        "var",
        Symbol::from_str(cps.ctx, lookup).into(),
        [Atom::Constant(module), Atom::Constant(name)],
        src,
        |cps, owner, var| {
            cps.let_prim(
                owner,
                "_k",
                Symbol::from_str(cps.ctx, "cache-set!").into(),
                [Atom::Constant(cache_key), Atom::Local(var)],
                src,
                |cps, owner, _k| {
                    cps.emit_continue(owner, merge, [Atom::Local(var)], Value::new(false))
                },
            )
        },
    );
    let kinit_cont = cps.make_cont(
        kinit,
        &[],
        None,
        kinit_body,
        Value::new(false),
        true,
        false,
        Value::new(false),
    );

    let body = cps.new_child_link();
    cps.let_prim(
        body,
        "cache_entry",
        Symbol::from_str(cps.ctx, "cache-ref").into(),
        [Atom::Constant(cache_key)],
        src,
        |cps, owner, cache_entry| {
            let kok = cps.fresh_variable("kok");
            let kok_body = cps.new_child_link();
            cps.emit_continue(
                kok_body,
                merge,
                [Atom::Local(cache_entry)],
                Value::new(false),
            );
            let kok_cont = cps.make_cont(
                kok,
                &[],
                None,
                kok_body,
                Value::new(false),
                false,
                false,
                Value::new(false),
            );
            cps.let_prim(
                owner,
                "is_heap_obj",
                Symbol::from_str(cps.ctx, "heap-object?").into(),
                [Atom::Local(cache_entry)],
                src,
                |cps, owner, is_heap_obj| {
                    let if_body = cps.new_child_link();
                    cps.emit_if_continue(
                        if_body,
                        Atom::Local(is_heap_obj),
                        kok,
                        &[],
                        kinit,
                        &[],
                        [BranchHint::Normal, BranchHint::Normal],
                        Value::new(false),
                    );
                    cps.emit_letk(owner, &[kok_cont], if_body, Value::new(false))
                },
            )
        },
    );

    cps.emit_letk(owner, &[merge_cont, kinit_cont], body, Value::new(false))
}

pub fn cache_current_module<'gc>(
    cps: &mut GraphCpsBuilder<'gc>,
    owner: Subterm,
    src: Value<'gc>,
    scope: Atom<'gc>,
    module: Atom<'gc>,
    fk: impl FnOnce(&mut GraphCpsBuilder<'gc>, Subterm) -> TermId,
) -> TermId {
    cps.let_prim(
        owner,
        "_k",
        Symbol::from_str(cps.ctx, "cache-set!").into(),
        [scope, module],
        src,
        |cps, owner, _| fk(cps, owner),
    )
}

pub fn reify_lookup<'gc>(
    cps: &mut GraphCpsBuilder<'gc>,
    owner: Subterm,
    src: Value<'gc>,
    mod_var: LVarRef<'gc>,
    name: Value<'gc>,
    assert_bound: bool,
    have_var: impl FnOnce(&mut GraphCpsBuilder<'gc>, Subterm, Atom<'gc>) -> TermId,
) -> TermId {
    let lookup = if assert_bound {
        "lookup-bound"
    } else {
        "lookup"
    };
    cps.let_prim(
        owner,
        "variable",
        Symbol::from_str(cps.ctx, lookup).into(),
        [Atom::Local(mod_var), Atom::Constant(name)],
        src,
        |cps, owner, variable| have_var(cps, owner, Atom::Local(variable)),
    )
}

pub fn reify_resolve_module<'gc>(
    cps: &mut GraphCpsBuilder<'gc>,
    owner: Subterm,
    k: LVarRef<'gc>,
    src: Value<'gc>,
    module: Value<'gc>,
    public: bool,
) -> TermId {
    cps.let_prim(
        owner,
        "resolved",
        Symbol::from_str(cps.ctx, "resolve-module").into(),
        [Atom::Constant(public.into()), Atom::Constant(module)],
        src,
        |cps, owner, resolved| {
            cps.emit_continue(owner, k, [Atom::Local(resolved)], Value::new(false))
        },
    )
}

fn is_single_valued<'gc>(exp: CoreTermRef<'gc>) -> bool {
    match &exp.kind {
        TermKind::Values(vals) if vals.len() == 1 => true,
        TermKind::ToplevelRef(..) | TermKind::ModuleRef(..) | TermKind::LRef(..) => true,
        // Every primitive continues exactly once with one value or raises:
        // `values', `apply', and `call-with-values' are library procedures,
        // not primitives, and each table expansion ends in a single
        // continue or a raise.
        TermKind::PrimCall(..) => true,
        TermKind::Seq(_, tail) => is_single_valued(*tail),
        TermKind::Receive(_, _, _, body) => is_single_valued(*body),
        TermKind::Let(let_) => is_single_valued(let_.body),
        // Push/pop and the thunk fallback are transparent to values.
        TermKind::WithContinuationMark(_, _, result) => is_single_valued(*result),
        TermKind::Const(_) => true,
        _ => false,
    }
}

pub fn convert_arg<'gc, 'a>(
    cps: &mut GraphCpsBuilder<'gc>,
    exp: CoreTermRef<'gc>,
    owner: Subterm,
    k: ArgContinuation<'a, 'gc>,
) -> TermId {
    let src = exp.source();
    match exp.kind {
        TermKind::LRef(var) => k(cps, owner, Atom::Local(var)),
        TermKind::Const(value) => cps.bind_literal(owner, value, src, k),

        // A primitive with no expansion binds its result directly, with no
        // continuation in between. For the test of an `if` this keeps the
        // comparison next to the branch, so lowering can fuse the two.
        TermKind::PrimCall(prim, _) if !get_primitive_table(cps.ctx).table.contains_key(&prim) => {
            let TermKind::PrimCall(_, args) = &exp.kind else {
                unreachable!("matched a primitive call above");
            };
            convert_args(
                cps,
                args,
                owner,
                Box::new(move |cps, owner, args| {
                    cps.let_prim(owner, "arg", prim, args, src, |cps, owner, atom| {
                        k(cps, owner, Atom::Local(atom))
                    })
                }),
            )
        }

        _ if is_single_valued(exp) => {
            let karg = cps.fresh_variable("karg");
            let arg = cps.fresh_variable("arg");
            let cont_body = cps.new_child_link();
            k(cps, cont_body, Atom::Local(arg));
            let cont = cps.make_cont(
                karg,
                &[arg],
                None,
                cont_body,
                src,
                false,
                false,
                Value::new(false),
            );
            let body = cps.new_child_link();
            convert(cps, exp, karg, body, AttachmentMode::NonNone);
            cps.emit_letk(owner, &[cont], body, src)
        }

        _ => {
            let karg = cps.fresh_variable("karg");
            let arg = cps.fresh_variable("arg");
            let rest = cps.fresh_variable("rest");
            let cont_body = cps.new_child_link();
            k(cps, cont_body, Atom::Local(arg));
            let cont = cps.make_cont(
                karg,
                &[arg],
                Some(rest),
                cont_body,
                src,
                false,
                false,
                Value::new(false),
            );
            let body = cps.new_child_link();
            convert(cps, exp, karg, body, AttachmentMode::NonNone);
            cps.emit_letk(owner, &[cont], body, src)
        }
    }
}

pub fn convert_args<'gc, 'a>(
    cps: &mut GraphCpsBuilder<'gc>,
    exps: &'a [CoreTermRef<'gc>],
    owner: Subterm,
    fk: ArgsContinuation<'a, 'gc>,
) -> TermId {
    if exps.is_empty() {
        return fk(cps, owner, Vec::new());
    }

    let exp = exps[0];
    let exps = &exps[1..];
    convert_arg(
        cps,
        exp,
        owner,
        Box::new(move |cps, owner, arg| {
            convert_args(
                cps,
                exps,
                owner,
                Box::new(move |cps, owner, mut args| {
                    let mut all_args = Vec::with_capacity(args.len() + 1);
                    all_args.push(arg);
                    all_args.append(&mut args);
                    fk(cps, owner, all_args)
                }),
            )
        }),
    )
}

fn convert_let_bindings<'gc>(
    cps: &mut GraphCpsBuilder<'gc>,
    bindings: &[(LVarRef<'gc>, CoreTermRef<'gc>)],
    index: usize,
    body: CoreTermRef<'gc>,
    k: LVarRef<'gc>,
    owner: Subterm,
    source: Value<'gc>,
    mode: AttachmentMode,
) -> TermId {
    if index == usize::MAX {
        return convert(cps, body, k, owner, mode);
    }

    let (binding, expr) = bindings[index];
    let let_k = cps.fresh_variable("let");
    let rest = (!is_single_valued(expr)).then(|| cps.fresh_variable("rest"));
    let cont_body = cps.new_child_link();
    if index == 0 {
        convert(cps, body, k, cont_body, mode);
    } else {
        convert_let_bindings(cps, bindings, index - 1, body, k, cont_body, source, mode);
    }
    let cont = cps.make_cont(
        let_k,
        &[binding],
        rest,
        cont_body,
        source,
        false,
        false,
        Value::new(false),
    );
    let expr_body = cps.new_child_link();
    convert(cps, expr, let_k, expr_body, AttachmentMode::NonNone);
    cps.emit_letk(owner, &[cont], expr_body, source)
}

/// The primitive an `if` can branch on directly: `test` calls a primitive
/// that tests and jumps in one step and that has no expansion of its own.
fn branch_primitive<'gc>(
    ctx: MutationContext<'gc>,
    test: CoreTermRef<'gc>,
) -> Option<crate::runtime::prims::def::Primitive> {
    let TermKind::PrimCall(name, args) = test.kind else {
        return None;
    };
    if get_primitive_table(ctx).table.contains_key(&name) || args.len() > u8::MAX as usize {
        return None;
    }
    let name = name.try_as::<Symbol>()?.to_string();
    crate::runtime::prims::def::Primitive::from_name(&name).filter(|prim| prim.is_branch_fusable())
}

/// `(if (prim args...) consequent alternate)` with a [`branch_primitive`]:
/// both arms become continuations of no arguments and the test a
/// `branch-prim` term that jumps to one of them, so no boolean is made.
fn convert_branch_prim<'gc>(
    cps: &mut GraphCpsBuilder<'gc>,
    test: CoreTermRef<'gc>,
    consequent: CoreTermRef<'gc>,
    alternate: CoreTermRef<'gc>,
    k: LVarRef<'gc>,
    owner: Subterm,
    mode: AttachmentMode,
) -> TermId {
    let prim = branch_primitive(cps.ctx, test).expect("checked by the caller");
    let src = test.source();
    let TermKind::PrimCall(_, args) = &test.kind else {
        unreachable!("branch primitives are primitive calls");
    };

    let (if_true, true_cont) = branch_arm(cps, "ktrue", consequent, k, mode);
    let (if_false, false_cont) = branch_arm(cps, "kfalse", alternate, k, mode);

    let body = cps.new_child_link();
    convert_args(
        cps,
        args,
        body,
        Box::new(move |cps, owner, args| {
            cps.emit_branch_prim(
                owner,
                prim,
                args,
                if_true,
                vec![],
                if_false,
                vec![],
                [BranchHint::Normal, BranchHint::Normal],
                src,
            )
        }),
    );
    cps.emit_letk(owner, &[true_cont, false_cont], body, src)
}

/// A zero-argument continuation that runs `body` and continues to `k`: one
/// arm of a conditional, shared by every branch that targets it.
fn branch_arm<'gc>(
    cps: &mut GraphCpsBuilder<'gc>,
    name: &str,
    body: CoreTermRef<'gc>,
    k: LVarRef<'gc>,
    mode: AttachmentMode,
) -> (LVarRef<'gc>, FunctionId) {
    let binding = cps.fresh_variable(name);
    let link = cps.new_child_link();
    convert(cps, body, k, link, mode);
    let cont = cps.make_cont(
        binding,
        &[],
        None,
        link,
        body.source(),
        false,
        false,
        Value::new(false),
    );
    (binding, cont)
}

/// `(if (if c t e) consequent alternate)`, as `memv`/`memq`/`or`/`and`
/// produce for `case` and `cond`: branches on `c` jump straight to the
/// shared arms, so no boolean is made and a primitive test stays fused
/// into a `branch-prim` (which switch inference then recognises).
fn convert_nested_if<'gc>(
    cps: &mut GraphCpsBuilder<'gc>,
    test: CoreTermRef<'gc>,
    consequent: CoreTermRef<'gc>,
    alternate: CoreTermRef<'gc>,
    k: LVarRef<'gc>,
    owner: Subterm,
    mode: AttachmentMode,
) -> TermId {
    let src = test.source();
    let (if_true, true_cont) = branch_arm(cps, "ktrue", consequent, k, mode);
    let (if_false, false_cont) = branch_arm(cps, "kfalse", alternate, k, mode);
    let body = cps.new_child_link();
    convert_condition(cps, test, if_true, if_false, body);
    cps.emit_letk(owner, &[true_cont, false_cont], body, src)
}

/// Lowers `test` in condition position: control reaches `if_true` or
/// `if_false`, both zero-argument continuations, by truthiness of the value.
fn convert_condition<'gc>(
    cps: &mut GraphCpsBuilder<'gc>,
    test: CoreTermRef<'gc>,
    if_true: LVarRef<'gc>,
    if_false: LVarRef<'gc>,
    owner: Subterm,
) -> TermId {
    let src = test.source();
    match &test.kind {
        TermKind::Const(value) => {
            let target = if value.as_bool() { if_true } else { if_false };
            cps.emit_continue(owner, target, [], src)
        }
        TermKind::If(cond, then, otherwise) => {
            // A constant side needs no continuation, it is its target.
            let mut sides = Vec::new();
            let mut side = |cps: &mut GraphCpsBuilder<'gc>, term: CoreTermRef<'gc>| match &term.kind
            {
                TermKind::Const(value) => {
                    if value.as_bool() {
                        if_true
                    } else {
                        if_false
                    }
                }
                _ => {
                    let binding = cps.fresh_variable("kcond");
                    let link = cps.new_child_link();
                    convert_condition(cps, term, if_true, if_false, link);
                    sides.push(cps.make_cont(
                        binding,
                        &[],
                        None,
                        link,
                        term.source(),
                        false,
                        false,
                        Value::new(false),
                    ));
                    binding
                }
            };
            let then_target = side(cps, *then);
            let else_target = side(cps, *otherwise);
            if sides.is_empty() {
                return convert_condition(cps, *cond, then_target, else_target, owner);
            }
            let body = cps.new_child_link();
            convert_condition(cps, *cond, then_target, else_target, body);
            cps.emit_letk(owner, &sides, body, src)
        }
        _ if branch_primitive(cps.ctx, test).is_some() => {
            let prim = branch_primitive(cps.ctx, test).expect("checked above");
            let TermKind::PrimCall(_, args) = &test.kind else {
                unreachable!("branch primitives are primitive calls");
            };
            convert_args(
                cps,
                args,
                owner,
                Box::new(move |cps, owner, args| {
                    cps.emit_branch_prim(
                        owner,
                        prim,
                        args,
                        if_true,
                        vec![],
                        if_false,
                        vec![],
                        [BranchHint::Normal, BranchHint::Normal],
                        src,
                    )
                }),
            )
        }
        _ => convert_arg(
            cps,
            test,
            owner,
            Box::new(move |cps, owner, test| {
                cps.emit_if_continue(
                    owner,
                    test,
                    if_true,
                    &[],
                    if_false,
                    &[],
                    [BranchHint::Normal, BranchHint::Normal],
                    src,
                )
            }),
        ),
    }
}

/// Run `body` with a fresh continuation that pops the attachment, then
/// continues K with the same values, popping the attachment on scope exit. The new
/// continuation takes NARGS fixed values plus a rest list when K does, only
/// fixed values forward, which is exact when K drops its rest.
fn with_pop_continuation<'gc>(
    cps: &mut GraphCpsBuilder<'gc>,
    owner: Subterm,
    src: Value<'gc>,
    k: LVarRef<'gc>,
    nargs: usize,
    has_rest: bool,
    body: impl FnOnce(&mut GraphCpsBuilder<'gc>, LVarRef<'gc>, Subterm) -> TermId,
) -> TermId {
    let pop = cps.fresh_variable("kpop");
    let inner = cps.new_child_link();
    body(cps, pop, inner);
    let mut fixed = Vec::with_capacity(nargs);
    for _ in 0..nargs {
        fixed.push(cps.fresh_variable("pop_arg"));
    }
    let rest = has_rest.then(|| cps.fresh_variable("pop_rest"));
    let forward: Vec<Atom<'gc>> = fixed.iter().copied().map(Atom::Local).collect();
    let pop_body = cps.new_child_link();
    cps.let_prim(
        pop_body,
        "pop",
        Symbol::from_str(cps.ctx, "$attachment-pop!").into(),
        [],
        src,
        |cps, owner, _| cps.emit_continue(owner, k, forward, src),
    );
    let cont = cps.make_cont(
        pop,
        &fixed,
        rest,
        pop_body,
        src,
        false,
        false,
        Value::new(false),
    );
    cps.emit_letk(owner, &[cont], inner, src)
}

/// Single-valued scope exit: wrap K with a pop when MODE is in a scope.
/// The producer yields exactly one value, so the one-fixed wrap forwards
/// it unchanged whatever shape K has.
fn with_pop_1<'gc>(
    cps: &mut GraphCpsBuilder<'gc>,
    owner: Subterm,
    src: Value<'gc>,
    k: LVarRef<'gc>,
    mode: AttachmentMode,
    body: impl FnOnce(&mut GraphCpsBuilder<'gc>, LVarRef<'gc>, Subterm) -> TermId,
) -> TermId {
    if !mode.in_some_scope() {
        return body(cps, k, owner);
    }
    with_pop_continuation(cps, owner, src, k, 1, false, body)
}

/// Like [`with_pop_1`] for a producer of exactly N values.
fn with_pop_n<'gc>(
    cps: &mut GraphCpsBuilder<'gc>,
    owner: Subterm,
    src: Value<'gc>,
    k: LVarRef<'gc>,
    mode: AttachmentMode,
    nargs: usize,
    body: impl FnOnce(&mut GraphCpsBuilder<'gc>, LVarRef<'gc>, Subterm) -> TermId,
) -> TermId {
    if !mode.in_some_scope() {
        return body(cps, k, owner);
    }
    with_pop_continuation(cps, owner, src, k, nargs, false, body)
}

/// Scope-exit call in [`AttachmentMode::NonSome`]: wrap K with a pop,
/// copying K's shape. The scope entry already ruled out rest-taking
/// `receive` consumers (those keep the thunk lowering), so dropping the
/// rest on forward is exact.
fn with_pop_like<'gc>(
    cps: &mut GraphCpsBuilder<'gc>,
    owner: Subterm,
    src: Value<'gc>,
    k: LVarRef<'gc>,
    body: impl FnOnce(&mut GraphCpsBuilder<'gc>, LVarRef<'gc>, Subterm) -> TermId,
) -> TermId {
    let shape = cps
        .cont_shapes
        .get(&k)
        .copied()
        .expect("pop-wrap of a continuation with unknown shape");
    debug_assert!(
        !(shape.is_consumer && shape.has_rest),
        "pop-wrap of a rest-taking receive consumer"
    );
    with_pop_continuation(cps, owner, src, k, shape.nargs, shape.has_rest, body)
}

/// True when K takes a rest list as a `receive` consumer, or its shape is
/// unknown: pop-wraps cannot forward its values, so the scope keeps the
/// thunk lowering, whose calls stay arity-transparent.
fn cont_needs_thunk_fallback<'gc>(cps: &GraphCpsBuilder<'gc>, k: LVarRef<'gc>) -> bool {
    match cps.cont_shapes.get(&k) {
        None => true,
        Some(shape) => shape.is_consumer && shape.has_rest,
    }
}

/// Today's `with-continuation-mark` lowering: call the native
/// `call-with-continuation-mark`, which reifies and tail-calls the body
/// thunk. Kept for scopes whose continuation pop-wraps cannot forward.
fn convert_wcm_thunk<'gc>(
    cps: &mut GraphCpsBuilder<'gc>,
    key: CoreTermRef<'gc>,
    mark: CoreTermRef<'gc>,
    result: CoreTermRef<'gc>,
    k: LVarRef<'gc>,
    owner: Subterm,
    src: Value<'gc>,
    mode: AttachmentMode,
) -> TermId {
    let thunk = Gc::new(
        cps.ctx,
        Proc {
            args: Array::from_slice(cps.ctx, []),
            name: Value::new(false),
            source: src,
            variadic: None,
            body: result,
            meta: cps.current_meta,
        },
    );
    let thunk = Gc::new(
        cps.ctx,
        crate::expander::core::Term {
            kind: TermKind::Proc(thunk),
            source: crate::heap::cell::Lock::new(src),
        },
    );

    let mref = Gc::new(
        cps.ctx,
        crate::expander::core::Term {
            kind: TermKind::PrimRef(cps.ctx.intern("call-with-continuation-mark")),
            source: crate::heap::cell::Lock::new(src),
        },
    );

    let call = Gc::new(
        cps.ctx,
        crate::expander::core::Term {
            source: crate::heap::cell::Lock::new(src),
            kind: TermKind::Call(mref, Array::from_slice(cps.ctx, [key, mark, thunk])),
        },
    );

    convert(cps, call, k, owner, mode)
}

/// Lower `with-continuation-mark` per MODE.
/// Key and value evaluate first, then the consume/update/push-or-set
/// sequence, then the body in the scope's mode.
fn convert_wcm<'gc>(
    cps: &mut GraphCpsBuilder<'gc>,
    key: CoreTermRef<'gc>,
    mark: CoreTermRef<'gc>,
    result: CoreTermRef<'gc>,
    k: LVarRef<'gc>,
    mode: AttachmentMode,
    owner: Subterm,
    src: Value<'gc>,
) -> TermId {
    if mode == AttachmentMode::NonNone && cont_needs_thunk_fallback(cps, k) {
        return convert_wcm_thunk(cps, key, mark, result, k, owner, src, mode);
    }
    convert_arg(
        cps,
        key,
        owner,
        Box::new(move |cps, owner, key| {
            convert_arg(
                cps,
                mark,
                owner,
                Box::new(move |cps, owner, mark| {
                    let update_mark = Symbol::from_str(cps.ctx, "$update-mark").into();
                    let null = Atom::Constant(Value::null());
                    match mode {
                        AttachmentMode::NonNone => cps.let_prim(
                            owner,
                            "table",
                            update_mark,
                            [null, key, mark],
                            src,
                            |cps, owner, table| {
                                cps.let_prim(
                                    owner,
                                    "push",
                                    Symbol::from_str(cps.ctx, "$attachment-push!").into(),
                                    [Atom::Local(table)],
                                    src,
                                    |cps, owner, _| {
                                        convert(cps, result, k, owner, AttachmentMode::NonSome)
                                    },
                                )
                            },
                        ),
                        AttachmentMode::Tail => cps.let_prim(
                            owner,
                            "marks",
                            Symbol::from_str(cps.ctx, "$attachment-consume-reify!").into(),
                            [null],
                            src,
                            |cps, owner, marks| {
                                cps.let_prim(
                                    owner,
                                    "table",
                                    update_mark,
                                    [Atom::Local(marks), key, mark],
                                    src,
                                    |cps, owner, table| {
                                        cps.let_prim(
                                            owner,
                                            "push",
                                            Symbol::from_str(cps.ctx, "$attachment-push!").into(),
                                            [Atom::Local(table)],
                                            src,
                                            |cps, owner, _| {
                                                convert(
                                                    cps,
                                                    result,
                                                    k,
                                                    owner,
                                                    AttachmentMode::TailSome,
                                                )
                                            },
                                        )
                                    },
                                )
                            },
                        ),
                        AttachmentMode::NonSome | AttachmentMode::TailSome => cps.let_prim(
                            owner,
                            "marks",
                            Symbol::from_str(cps.ctx, "$attachment-consume!").into(),
                            [],
                            src,
                            |cps, owner, marks| {
                                cps.let_prim(
                                    owner,
                                    "table",
                                    update_mark,
                                    [Atom::Local(marks), key, mark],
                                    src,
                                    |cps, owner, table| {
                                        cps.let_prim(
                                            owner,
                                            "set",
                                            Symbol::from_str(cps.ctx, "$attachment-set!").into(),
                                            [Atom::Local(table)],
                                            src,
                                            |cps, owner, _| convert(cps, result, k, owner, mode),
                                        )
                                    },
                                )
                            },
                        ),
                    }
                }),
            )
        }),
    )
}

pub fn convert<'gc>(
    cps: &mut GraphCpsBuilder<'gc>,
    exp: CoreTermRef<'gc>,
    k: LVarRef<'gc>,
    owner: Subterm,
    mode: AttachmentMode,
) -> TermId {
    let src = exp.source();

    match &exp.kind {
        TermKind::Values(vals) => {
            let arity = vals.len();
            with_pop_n(cps, owner, src, k, mode, arity, |cps, k, owner| {
                convert_args(
                    cps,
                    vals,
                    owner,
                    Box::new(move |cps, owner, args| {
                        cps.emit_continue(owner, k, args, Value::new(false))
                    }),
                )
            })
        }

        TermKind::If(test, consequent, alternate) if branch_primitive(cps.ctx, *test).is_some() => {
            convert_branch_prim(cps, *test, *consequent, *alternate, k, owner, mode)
        }

        TermKind::If(test, consequent, alternate) if matches!(test.kind, TermKind::If(..)) => {
            convert_nested_if(cps, *test, *consequent, *alternate, k, owner, mode)
        }

        TermKind::If(test, consequent, alternate) => convert_arg(
            cps,
            *test,
            owner,
            Box::new(move |cps, owner, test| {
                let consequent_link = cps.new_child_link();
                convert(cps, *consequent, k, consequent_link, mode);
                let alternate_link = cps.new_child_link();
                convert(cps, *alternate, k, alternate_link, mode);
                cps.emit_if(
                    owner,
                    test,
                    consequent_link,
                    alternate_link,
                    [BranchHint::Normal, BranchHint::Normal],
                    Value::new(false),
                )
            }),
        ),

        TermKind::LSet(..) => unreachable!(),
        TermKind::LRef(var) => with_pop_1(cps, owner, src, k, mode, |cps, k, owner| {
            cps.emit_continue(owner, k, [Atom::Local(*var)], src)
        }),

        TermKind::Const(c) => with_pop_1(cps, owner, src, k, mode, |cps, k, owner| {
            cps.bind_literal(owner, *c, src, |cps, owner, atom| {
                cps.emit_continue(owner, k, [atom], src)
            })
        }),

        TermKind::PrimRef(name) => with_pop_1(cps, owner, src, k, mode, |cps, k, owner| {
            let module = list!(cps.ctx, cps.ctx.intern("capy"));
            module_box(
                cps,
                owner,
                |cps, owner, var| {
                    cps.let_prim(
                        owner,
                        "val",
                        Symbol::from_str(cps.ctx, "variable-ref").into(),
                        [var],
                        src,
                        |cps, owner, val| {
                            cps.emit_continue(owner, k, [Atom::Local(val)], Value::new(false))
                        },
                    )
                },
                module,
                *name,
                true,
                true,
                src,
            )
        }),

        TermKind::ToplevelRef(_, name) => with_pop_1(cps, owner, src, k, mode, |cps, k, owner| {
            toplevel_box(cps, owner, src, *name, true, |cps, owner, var| {
                cps.let_prim(
                    owner,
                    "val",
                    Symbol::from_str(cps.ctx, "variable-ref").into(),
                    [var],
                    src,
                    |cps, owner, val| {
                        cps.emit_continue(owner, k, [Atom::Local(val)], Value::new(false))
                    },
                )
            })
        }),

        TermKind::ModuleRef(module, name, public) => {
            with_pop_1(cps, owner, src, k, mode, |cps, k, owner| {
                module_box(
                    cps,
                    owner,
                    |cps, owner, var| {
                        cps.let_prim(
                            owner,
                            "val",
                            Symbol::from_str(cps.ctx, "variable-ref").into(),
                            [var],
                            src,
                            |cps, owner, val| {
                                cps.emit_continue(owner, k, [Atom::Local(val)], Value::new(false))
                            },
                        )
                    },
                    *module,
                    *name,
                    *public,
                    false,
                    src,
                )
            })
        }

        TermKind::ToplevelSet(_, name, exp) => {
            with_pop_1(cps, owner, src, k, mode, |cps, k, owner| {
                convert_arg(
                    cps,
                    *exp,
                    owner,
                    Box::new(move |cps, owner, atom| {
                        toplevel_box(cps, owner, src, *name, false, |cps, owner, var| {
                            cps.let_prim(
                                owner,
                                "_val",
                                Symbol::from_str(cps.ctx, "variable-set!").into(),
                                [var, atom],
                                src,
                                |cps, owner, val| {
                                    cps.emit_continue(owner, k, [Atom::Local(val)], src)
                                },
                            )
                        })
                    }),
                )
            })
        }

        TermKind::ModuleSet(module, name, public, exp) => {
            with_pop_1(cps, owner, src, k, mode, |cps, k, owner| {
                convert_arg(
                    cps,
                    *exp,
                    owner,
                    Box::new(move |cps, owner, exp| {
                        module_box(
                            cps,
                            owner,
                            |cps, owner, var| {
                                cps.let_prim(
                                    owner,
                                    "_val",
                                    Symbol::from_str(cps.ctx, "variable-set!").into(),
                                    [var, exp],
                                    src,
                                    |cps, owner, val| {
                                        cps.emit_continue(owner, k, [Atom::Local(val)], src)
                                    },
                                )
                            },
                            *module,
                            *name,
                            *public,
                            true,
                            src,
                        )
                    }),
                )
            })
        }

        TermKind::Proc(proc) => with_pop_1(cps, owner, src, k, mode, |cps, k, owner| {
            if cps.current_topbox_scope.is_some() {
                let tmp = cps.fresh_variable("proc");
                let func = cps.make_proc_func(proc, tmp, true);
                let body = cps.new_child_link();
                cps.emit_continue(body, k, [Atom::Local(tmp)], src);
                cps.emit_fix(owner, &[func], body, src)
            } else {
                let prev = cps.current_topbox_scope;
                cps.enter_scope();
                let id = cps.current_topbox_scope.expect("invariant holds");
                capture_toplevel_scope(cps, owner, src, id, |cps, owner| {
                    let form = convert(cps, exp, k, owner, mode);
                    cps.current_topbox_scope = prev;
                    form
                })
            }
        }),

        TermKind::Define(_module, name, exp) => {
            with_pop_1(cps, owner, src, k, mode, |cps, k, owner| {
                convert_arg(
                    cps,
                    *exp,
                    owner,
                    Box::new(move |cps, owner, atom| {
                        cps.let_prim(
                            owner,
                            "_rv",
                            Symbol::from_str(cps.ctx, "define").into(),
                            [Atom::Constant(*name), atom],
                            src,
                            |cps, owner, rv| cps.emit_continue(owner, k, [Atom::Local(rv)], src),
                        )
                    }),
                )
            })
        }

        TermKind::Call(proc, args) => {
            let call_with = |cps: &mut GraphCpsBuilder<'gc>, k: LVarRef<'gc>, owner: Subterm| {
                convert_arg(
                    cps,
                    *proc,
                    owner,
                    Box::new(move |cps, owner, proc| {
                        convert_args(
                            cps,
                            args,
                            owner,
                            Box::new(move |cps, owner, args| {
                                cps.emit_app(owner, proc, k, args, src)
                            }),
                        )
                    }),
                )
            };
            // A call in tail scope stays a tail call: the reified frame's
            // underflow restore pops for it.
            if mode == AttachmentMode::NonSome {
                with_pop_like(cps, owner, src, k, call_with)
            } else {
                call_with(cps, k, owner)
            }
        }

        TermKind::PrimCall(prim, args) => {
            let prim = *prim;
            with_pop_1(cps, owner, src, k, mode, |cps, k, owner| {
                convert_args(
                    cps,
                    args,
                    owner,
                    Box::new(move |cps, owner, args| {
                        if let Some(term) =
                            get_primitive_table(cps.ctx).try_expand(cps, owner, src, prim, &args, k)
                        {
                            term
                        } else {
                            cps.let_prim(owner, "atom", prim, args, src, |cps, owner, atom| {
                                cps.emit_continue(owner, k, [Atom::Local(atom)], src)
                            })
                        }
                    }),
                )
            })
        }

        TermKind::Let(let_) => {
            assert!(!matches!(
                let_.style,
                LetStyle::LetRec | LetStyle::LetRecStar | LetStyle::LetStar
            ));
            let bindings = let_
                .lhs
                .iter()
                .copied()
                .zip(let_.rhs.iter().copied())
                .collect::<Vec<_>>();
            if bindings.is_empty() {
                convert(cps, let_.body, k, owner, mode)
            } else {
                convert_let_bindings(
                    cps,
                    &bindings,
                    bindings.len() - 1,
                    let_.body,
                    k,
                    owner,
                    src,
                    mode,
                )
            }
        }

        TermKind::Seq(head, tail) => {
            let ktail = cps.fresh_variable("ktail");
            let cont_body = cps.new_child_link();
            convert(cps, *tail, k, cont_body, mode);
            // A head that produces exactly one value binds it directly, only
            // a head of unknown arity needs the rest list, which the tail
            // ignores either way.
            let cont = if is_single_valued(*head) {
                let arg = cps.fresh_variable("arg");
                cps.make_cont(
                    ktail,
                    &[arg],
                    None,
                    cont_body,
                    Value::new(false),
                    false,
                    false,
                    Value::new(false),
                )
            } else {
                let vals = cps.fresh_variable("vals");
                cps.make_cont(
                    ktail,
                    &[],
                    Some(vals),
                    cont_body,
                    Value::new(false),
                    false,
                    false,
                    Value::new(false),
                )
            };
            let body = cps.new_child_link();
            convert(cps, *head, ktail, body, AttachmentMode::NonNone);
            cps.emit_letk(owner, &[cont], body, Value::new(false))
        }

        TermKind::Fix(fix) => {
            if cps.current_topbox_scope.is_some() {
                for binding in fix.lhs.iter().copied() {
                    cps.bind_lvar(binding);
                }
                let funcs = fix
                    .lhs
                    .iter()
                    .copied()
                    .zip(fix.rhs.iter().copied())
                    .map(|(binding, func)| cps.make_proc_func(&func, binding, true))
                    .collect::<Vec<_>>();

                let body = cps.new_child_link();
                convert(cps, fix.body, k, body, mode);
                return cps.emit_fix(owner, &funcs, body, src);
            }

            let prev = cps.current_topbox_scope;
            cps.enter_scope();
            let id = cps.current_topbox_scope.expect("invariant holds");
            capture_toplevel_scope(cps, owner, src, id, |cps, owner| {
                let form = convert(cps, exp, k, owner, mode);
                cps.current_topbox_scope = prev;
                form
            })
        }

        TermKind::Receive(vars, variadic, producer, consumer) => {
            let consumer_k_var = cps.fresh_variable("consumer");
            let body = cps.new_child_link();
            convert(cps, *consumer, k, body, mode);
            let consumer_k = cps.make_cont(
                consumer_k_var,
                vars,
                *variadic,
                body,
                src,
                false,
                false,
                cps.current_meta,
            );
            if let Some(shape) = cps.cont_shapes.get_mut(&consumer_k_var) {
                shape.is_consumer = true;
            }
            let letk_body = cps.new_child_link();
            convert(
                cps,
                *producer,
                consumer_k_var,
                letk_body,
                AttachmentMode::NonNone,
            );
            cps.emit_letk(owner, &[consumer_k], letk_body, src)
        }

        TermKind::WithContinuationMark(key, mark, result) => {
            convert_wcm(cps, *key, *mark, *result, k, mode, owner, src)
        }
    }
}
