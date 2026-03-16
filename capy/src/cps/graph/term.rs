use std::{
    collections::VecDeque,
    ops::{Index, IndexMut},
};

use crate::{cps::graph::intrinsic::Intrinsic, prelude::Value, runtime::Context};
use cranelift_entity::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TermId(u32);
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct VarId(u32);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct AtomId(u32);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct OccurrenceId(u32);

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct FuncId(u32);
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct ContId(u32);

entity_impl!(TermId);
entity_impl!(VarId, "v");
entity_impl!(AtomId);
entity_impl!(OccurrenceId);
entity_impl!(FuncId, "func");
entity_impl!(ContId, "cont");

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ParentLink {
    Term(TermId),
    Func(FuncId),
    Cont(ContId),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TermKind<'gc> {
    Let(VarId, Expression<'gc>, TermId),
    Letk(EntityList<ContId>, TermId),
    Fix(EntityList<FuncId>, TermId),
    App(OccurrenceId, OccurrenceId, EntityList<OccurrenceId>),
    Continue(OccurrenceId, EntityList<OccurrenceId>),
    If(
        OccurrenceId,
        OccurrenceId,
        EntityList<OccurrenceId>,
        OccurrenceId,
        EntityList<OccurrenceId>,
    ),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Expression<'gc> {
    PrimCall(Intrinsic, EntityList<OccurrenceId>),
    Constant(Value<'gc>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Term<'gc> {
    pub parent: Option<ParentLink>,
    pub kind: TermKind<'gc>,
    pub source: Value<'gc>,
}

pub struct Func<'gc> {
    pub var: VarId,
    pub meta: Value<'gc>,
    pub body: Option<TermId>,

    pub return_continuation: VarId,
    pub parameters: EntityList<VarId>,
    pub variadic: Option<VarId>,

    /// A parent term is a term that defines
    /// this function. This is `TermKind::Fix`.
    pub parent: Option<TermId>,
}

pub struct Cont<'gc> {
    pub var: VarId,
    pub meta: Value<'gc>,
    pub body: Option<TermId>,

    pub parameters: EntityList<VarId>,
    pub variadic: Option<VarId>,

    /// A parent term is a term that defines
    /// this continuation. This is `TermKind::Letk`.
    pub parent: Option<TermId>,
}

impl<'gc> Cont<'gc> {
    pub fn body(&self) -> TermId {
        self.body.unwrap()
    }
}

impl<'gc> Func<'gc> {
    pub fn body(&self) -> TermId {
        self.body.unwrap()
    }
}

pub struct Variable {
    pub var: VarId,
    pub def: VarDef,

    pub occurrence_header: OccurrenceId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum VarDef {
    Let(TermId),

    /// A variable is a part of continuation.
    ContPart(ContId, VarType),
    /// A variable is a part of function.
    FuncPart(FuncId, VarType),
    Func(FuncId),
    Cont(ContId),

    /// A variable is not yet defined. This is only
    /// during CPS graph construction, and should never be the case after construction is complete.
    NotDefined,
}

/// Type of variable when it is defined
/// in function or continution.
///
/// In case of function, `ReturnContinuation` is the variable for the return continuation of the function,
/// `Parameter(usize)` is the variable for the parameter at the given index, and `VariadicParameter` is the variable for the variadic parameter (if any).

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum VarType {
    ReturnContinuation,
    Parameter(usize),
    VariadicParameter,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum OccurenceKind {
    Header {
        var: VarId,
    },
    Occurrence {
        is_recursive: bool,
        term: Option<TermId>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Occurence {
    pub kind: OccurenceKind,
    pub next: Option<OccurrenceId>,
    pub prev: Option<OccurrenceId>,
}

pub struct Graph<'gc> {
    pub ctx: Context<'gc>,
    pub terms: PrimaryMap<TermId, Term<'gc>>,
    pub functions: PrimaryMap<FuncId, Func<'gc>>,
    pub continuations: PrimaryMap<ContId, Cont<'gc>>,
    pub variables: PrimaryMap<VarId, Variable>,
    pub occurences: PrimaryMap<OccurrenceId, Occurence>,

    pub occ_pool: ListPool<OccurrenceId>,
    pub var_pool: ListPool<VarId>,
    pub func_pool: ListPool<FuncId>,
    pub cont_pool: ListPool<ContId>,
    pub root: Option<FuncId>,

    pub effects: SecondaryMap<TermId, i32>,
}

impl<'gc> Index<TermId> for Graph<'gc> {
    type Output = Term<'gc>;

    fn index(&self, index: TermId) -> &Self::Output {
        &self.terms[index]
    }
}

impl<'gc> IndexMut<TermId> for Graph<'gc> {
    fn index_mut(&mut self, index: TermId) -> &mut Self::Output {
        &mut self.terms[index]
    }
}

impl<'gc> Index<FuncId> for Graph<'gc> {
    type Output = Func<'gc>;

    fn index(&self, index: FuncId) -> &Self::Output {
        &self.functions[index]
    }
}

impl<'gc> IndexMut<FuncId> for Graph<'gc> {
    fn index_mut(&mut self, index: FuncId) -> &mut Self::Output {
        &mut self.functions[index]
    }
}

impl<'gc> Index<ContId> for Graph<'gc> {
    type Output = Cont<'gc>;

    fn index(&self, index: ContId) -> &Self::Output {
        &self.continuations[index]
    }
}

impl<'gc> IndexMut<ContId> for Graph<'gc> {
    fn index_mut(&mut self, index: ContId) -> &mut Self::Output {
        &mut self.continuations[index]
    }
}

impl<'gc> Index<VarId> for Graph<'gc> {
    type Output = Variable;

    fn index(&self, index: VarId) -> &Self::Output {
        &self.variables[index]
    }
}

impl<'gc> IndexMut<VarId> for Graph<'gc> {
    fn index_mut(&mut self, index: VarId) -> &mut Self::Output {
        &mut self.variables[index]
    }
}

impl<'gc> Index<OccurrenceId> for Graph<'gc> {
    type Output = Occurence;

    fn index(&self, index: OccurrenceId) -> &Self::Output {
        &self.occurences[index]
    }
}

impl<'gc> IndexMut<OccurrenceId> for Graph<'gc> {
    fn index_mut(&mut self, index: OccurrenceId) -> &mut Self::Output {
        &mut self.occurences[index]
    }
}

impl<'gc> Graph<'gc> {
    pub fn new(ctx: Context<'gc>) -> Self {
        Self {
            ctx,
            terms: PrimaryMap::new(),
            functions: PrimaryMap::new(),
            continuations: PrimaryMap::new(),
            variables: PrimaryMap::new(),
            occurences: PrimaryMap::new(),
            occ_pool: ListPool::new(),
            var_pool: ListPool::new(),
            func_pool: ListPool::new(),
            cont_pool: ListPool::new(),
            root: None,
            effects: SecondaryMap::new(),
        }
    }

    pub fn add_term(&mut self, term: TermKind<'gc>, parent: Option<ParentLink>) -> TermId {
        let id = self.terms.push(Term {
            parent,
            kind: term,
            source: Value::new(false),
        });
        id
    }

    pub fn add_func(
        &mut self,
        var: VarId,
        meta: Value<'gc>,
        body: Option<TermId>,
        return_continuation: VarId,
        parameters: EntityList<VarId>,
        variadic: Option<VarId>,
        parent: Option<TermId>,
    ) -> FuncId {
        let id = self.functions.push(Func {
            var,
            meta,
            body,
            return_continuation,
            parameters,
            variadic,
            parent,
        });

        let nvars = parameters.len(&self.var_pool);
        for i in 0..nvars {
            let def = VarDef::FuncPart(id, VarType::Parameter(i));
            let var = parameters.as_slice(&self.var_pool)[i];
            self.variables[var].def = def;
        }

        let def_variadic = VarDef::FuncPart(id, VarType::VariadicParameter);
        if let Some(variadic) = variadic {
            self.variables[variadic].def = def_variadic;
        }

        let def_retk = VarDef::FuncPart(id, VarType::ReturnContinuation);
        self.variables[return_continuation].def = def_retk;

        if let Some(body) = body {
            self[body].parent = Some(ParentLink::Func(id));
        }

        id
    }

    pub fn add_cont(
        &mut self,
        var: VarId,
        meta: Value<'gc>,
        body: Option<TermId>,
        parameters: EntityList<VarId>,
        variadic: Option<VarId>,
        parent: Option<TermId>,
    ) -> ContId {
        let id = self.continuations.push(Cont {
            var,
            meta,
            body,
            parameters,
            variadic,
            parent,
        });

        let nvars = parameters.len(&self.var_pool);
        for i in 0..nvars {
            let def = VarDef::ContPart(id, VarType::Parameter(i));
            let var = parameters.as_slice(&self.var_pool)[i];
            self.variables[var].def = def;
        }

        let def_variadic = VarDef::ContPart(id, VarType::VariadicParameter);
        if let Some(variadic) = variadic {
            self.variables[variadic].def = def_variadic;
        }

        if let Some(body) = body {
            self[body].parent = Some(ParentLink::Cont(id));
        }

        id
    }

    pub fn enqueue_children(&mut self, t: TermId, queue: &mut VecDeque<TermId>) {
        match self[t].kind {
            TermKind::Let(_, _, body) => queue.push_back(body),
            TermKind::Letk(conts, body) => {
                for cont in conts.as_slice(&self.cont_pool) {
                    queue.push_back(self[*cont].body());
                }
                queue.push_back(body);
            }

            TermKind::Fix(funcs, body) => {
                for func in funcs.as_slice(&self.func_pool) {
                    queue.push_back(self[*func].body());
                }
                queue.push_back(body);
            }

            _ => (),
        }
    }

    pub fn set_term_source(&mut self, term: TermId, source: Value<'gc>) {
        self.terms[term].source = source;
    }

    pub fn set_term_parent(&mut self, term: TermId, parent: Option<ParentLink>) {
        self.terms[term].parent = parent;
    }

    pub fn iter_occ(&self, var: VarId) -> impl Iterator<Item = OccurrenceId> + '_ {
        let header = self.variables[var].occurrence_header;
        let mut current = self.occurences[header].next;
        std::iter::from_fn(move || {
            if let Some(occ) = current {
                if occ == header {
                    return None;
                }
                current = self.occurences[occ].next;
                Some(occ)
            } else {
                None
            }
        })
    }

    pub fn occ(&self, var: VarId) -> Vec<OccurrenceId> {
        let mut occs = Vec::new();
        let header = self.variables[var].occurrence_header;
        let mut current = self.occurences[header].next;
        while let Some(occ) = current {
            if occ == header {
                break;
            }
            occs.push(occ);
            current = self.occurences[occ].next;
        }
        occs
    }

    pub fn occ_len(&self, var: VarId) -> OccurrenceCount {
        let header = self.variables[var].occurrence_header;
        let first = self.occurences[header]
            .next
            .expect("occurrence header must always be linked");
        if first == header {
            OccurrenceCount::Zero
        } else if self[first].next == Some(header) {
            OccurrenceCount::One(first)
        } else {
            OccurrenceCount::Many
        }
    }

    pub fn site(&self, occ: OccurrenceId) -> TermId {
        match self.occurences[occ].kind {
            OccurenceKind::Occurrence {
                term: Some(term), ..
            } => term,
            OccurenceKind::Occurrence { term: None, .. } => unreachable!("must be populated"),
            OccurenceKind::Header { .. } => unreachable!("occurrence header has no site"),
        }
    }

    pub fn set_site(&mut self, occ: OccurrenceId, term: TermId) {
        match &mut self.occurences[occ].kind {
            OccurenceKind::Occurrence { term: occ_term, .. } => *occ_term = Some(term),
            OccurenceKind::Header { .. } => unreachable!("occurrence header has no site"),
        }
    }

    pub fn set_site_many(&mut self, occs: EntityList<OccurrenceId>, term: TermId) {
        let len = occs.len(&self.occ_pool);
        for i in 0..len {
            let occ = occs.as_slice(&self.occ_pool)[i];
            self.set_site(occ, term);
        }
    }

    pub fn rec(&self, occ: OccurrenceId) -> bool {
        match self.occurences[occ].kind {
            OccurenceKind::Occurrence { is_recursive, .. } => is_recursive,
            OccurenceKind::Header { .. } => unreachable!("occurrence header is not recursive"),
        }
    }

    pub fn set_rec(&mut self, occ: OccurrenceId, is_rec: bool) {
        match &mut self.occurences[occ].kind {
            OccurenceKind::Occurrence { is_recursive, .. } => *is_recursive = is_rec,
            OccurenceKind::Header { .. } => unreachable!("occurrence header is not recursive"),
        }
    }

    pub fn set_def(&mut self, var: VarId, def: VarDef) {
        self.variables[var].def = def;
    }

    pub fn var(&self, occ: OccurrenceId) -> VarId {
        let mut current = occ;
        loop {
            match self.occurences[current].kind {
                OccurenceKind::Header { var } => return var,
                OccurenceKind::Occurrence { .. } => {
                    current = self.occurences[current]
                        .prev
                        .expect("occurrence must remain linked to its header");
                }
            }
        }
    }

    pub fn occurrence_list(
        &mut self,
        occs: impl IntoIterator<Item = OccurrenceId>,
    ) -> EntityList<OccurrenceId> {
        EntityList::from_iter(occs, &mut self.occ_pool)
    }
    pub fn funcs_list(&mut self, funcs: impl IntoIterator<Item = FuncId>) -> EntityList<FuncId> {
        EntityList::from_iter(funcs, &mut self.func_pool)
    }

    pub fn conts_list(&mut self, conts: impl IntoIterator<Item = ContId>) -> EntityList<ContId> {
        EntityList::from_iter(conts, &mut self.cont_pool)
    }

    pub fn var_list(&mut self, vars: impl IntoIterator<Item = VarId>) -> EntityList<VarId> {
        EntityList::from_iter(vars, &mut self.var_pool)
    }

    pub fn add_var(&mut self, def: VarDef) -> VarId {
        let var_id = self.variables.push(Variable {
            var: VarId::from_u32(0),
            def,
            occurrence_header: OccurrenceId::from_u32(0),
        });
        let occurrence_header = self.occurences.push(Occurence {
            kind: OccurenceKind::Header { var: var_id },
            next: None,
            prev: None,
        });
        self.variables[var_id].var = var_id;
        self.variables[var_id].occurrence_header = occurrence_header;
        self.occurences[occurrence_header].next = Some(occurrence_header);
        self.occurences[occurrence_header].prev = Some(occurrence_header);
        log::trace!("add_var: created {var_id} with def {def:?}");
        var_id
    }

    pub fn add_future_occurrence(&mut self, var: VarId, is_recursive: bool) -> OccurrenceId {
        let occ_id = self.occurences.push(Occurence {
            kind: OccurenceKind::Occurrence {
                is_recursive,
                term: None,
            },
            next: None,
            prev: None,
        });

        let header = self.variables[var].occurrence_header;
        let last = self.occurences[header]
            .prev
            .expect("occurrence header must have a previous occurrence");

        self.occurences[last].next = Some(occ_id);
        self.occurences[header].prev = Some(occ_id);

        self.occurences[occ_id].next = Some(header);
        self.occurences[occ_id].prev = Some(last);

        if self.occurences[header].next == Some(header) {
            self.occurences[header].next = Some(occ_id);
        }

        occ_id
    }

    pub fn add_occurrence(&mut self, var: VarId, term: TermId, is_recursive: bool) -> OccurrenceId {
        let occ_id = self.occurences.push(Occurence {
            kind: OccurenceKind::Occurrence {
                is_recursive,
                term: Some(term),
            },
            next: None,
            prev: None,
        });

        let header = self.variables[var].occurrence_header;
        let last = self.occurences[header]
            .prev
            .expect("occurrence header must have a previous occurrence");

        self.occurences[last].next = Some(occ_id);
        self.occurences[header].prev = Some(occ_id);

        self.occurences[occ_id].next = Some(header);
        self.occurences[occ_id].prev = Some(last);

        if self.occurences[header].next == Some(header) {
            self.occurences[header].next = Some(occ_id);
        }

        log::trace!("add_occurrence: {occ_id:?} for {var} at {term:?} (recursive={is_recursive})");
        occ_id
    }

    /// Substitute all uses of `from` with `to`.
    ///
    /// NOTE: This function is "destructive" as it removes all occurrences of `from` from
    /// the graph and merges them with `to`.
    pub fn subst(&mut self, from: VarId, to: OccurrenceId) {
        let to_var = self.var(to);
        log::debug!("subst: {from} -> {to_var} (via {to:?})");
        // fast path: merge occurence lists. We don't have to update terms because
        // they point to occurences, not variables themselves.

        // mark all occurrences in `to` as recursive if `from` is recursive, since they will now be in the same variable.
        if self.rec(to) {
            for occ in self.occ(from) {
                self.set_rec(occ, true);
            }
        }

        self.merge_occurrence_headers(
            self.variables[from].occurrence_header,
            self.variables[to_var].occurrence_header,
        );
    }

    pub fn occurrences_in_expr(&self, expr: &Expression<'gc>) -> Vec<OccurrenceId> {
        match expr {
            Expression::PrimCall(_, args) => args.as_slice(&self.occ_pool).to_vec(),
            Expression::Constant(_) => vec![],
        }
    }

    pub fn occurrences_in(&self, t: TermId) -> Vec<OccurrenceId> {
        let mut occs = Vec::new();
        let mut worklist = vec![t];
        while let Some(t) = worklist.pop() {
            let term = &self.terms[t];
            match &term.kind {
                TermKind::Let(_, expr, body) => {
                    match expr {
                        Expression::PrimCall(_, args) => {
                            occs.extend(args.as_slice(&self.occ_pool));
                        }
                        Expression::Constant(_) => {}
                    }
                    worklist.push(*body);
                }
                TermKind::Letk(conts, body) => {
                    for cont in conts.as_slice(&self.cont_pool) {
                        worklist.push(self[*cont].body());
                    }
                    worklist.push(*body);
                }
                TermKind::Fix(funcs, body) => {
                    for func in funcs.as_slice(&self.func_pool) {
                        worklist.push(self[*func].body());
                    }
                    worklist.push(*body);
                }
                TermKind::App(callee, retk, args) => {
                    occs.push(*callee);
                    occs.push(*retk);
                    occs.extend(args.as_slice(&self.occ_pool));
                }
                TermKind::Continue(cont, args) => {
                    occs.push(*cont);
                    occs.extend(args.as_slice(&self.occ_pool));
                }
                TermKind::If(cond, then_occ, then_args, else_occ, else_args) => {
                    occs.push(*cond);
                    occs.push(*then_occ);
                    occs.extend(then_args.as_slice(&self.occ_pool));
                    occs.push(*else_occ);
                    occs.extend(else_args.as_slice(&self.occ_pool));
                }
            }
        }
        occs
    }

    /// Merge occurrences of `from` into `to`. This is used when we want to merge two variables, e.g. when we determine that they are equal.
    /// NOTE: This function is "destructive" as it removes all occurrences of `from` from the graph and merges them with `to`.
    pub fn merge_occurrences(&mut self, from: VarId, to: VarId) {
        log::debug!("merge_occurrences: {from} -> {to}");
        self.merge_occurrence_headers(
            self.variables[from].occurrence_header,
            self.variables[to].occurrence_header,
        );
    }

    /// Determine if a variable can be safely deleted or inlined, and return the corresponding redex if so.
    ///
    /// ## Dead redex
    ///
    /// A variable can be safely deleted if it has no occurrences and it has no side-effects.
    ///
    /// ## Inlining redex
    ///
    /// A variable can be safely inlined if it has exactly one occurrence, that occurrence is not recursive, and the term at that occurrence is an application or continuation of the variable.
    pub fn kill_or_inline(&self, var: VarId) -> Option<Redex> {
        let occ_len = self.occ_len(var);
        log::trace!("kill_or_inline: {var} has occurrence count {occ_len:?}");
        match occ_len {
            OccurrenceCount::Many => {
                log::trace!("kill_or_inline: {var} has many occurrences, skip");
                return None;
            }
            _ => {
                let def = &self.variables[var].def;
                match def {
                    VarDef::Let(l) => match self[*l].kind {
                        TermKind::Let(_, Expression::PrimCall(intrinsic, _), _) => match occ_len {
                            OccurrenceCount::Zero if intrinsic.is_effect_free() => {
                                return Some(Redex::Dead(var));
                            }
                            _ => return None,
                        },

                        TermKind::Let(_, Expression::Constant(_), _) => match occ_len {
                            OccurrenceCount::Zero => return Some(Redex::Dead(var)),
                            _ => return None,
                        },

                        _ => return None,
                    },

                    VarDef::Func(_) | VarDef::Cont(_) => match occ_len {
                        OccurrenceCount::One(occ) => {
                            let site = self.site(occ);
                            match self[site].kind {
                                TermKind::App(callee, ..) if callee == occ => {
                                    if self.rec(occ) {
                                        // recursive self-call, not used: can be deleted.
                                        return Some(Redex::Dead(var));
                                    }
                                    return Some(Redex::Inline(var));
                                }
                                TermKind::Continue(cont, ..) if cont == occ => {
                                    if self.rec(occ) {
                                        // recursive self-call, not used: can be deleted.
                                        return Some(Redex::Dead(var));
                                    }
                                    return Some(Redex::Inline(var));
                                }
                                _ => return None,
                            }
                        }

                        // Unused letk or fix bindings can be deleted. They have no effects.
                        _ => return Some(Redex::Dead(var)),
                    },
                    // Parameters (FuncPart, ContPart) cannot be independently removed;
                    // their liveness is governed by the enclosing function/continuation.
                    _ => return None,
                }
            }
        }
    }

    /// Delete an occurrence from the graph. This is used when an occurrence is removed from a term, e.g. when a variable is substituted.
    pub fn delete(&mut self, occ: OccurrenceId, redex_collector: &mut impl FnMut(Redex)) {
        // Skip header occurrences and already-unlinked (previously deleted) occurrences
        if matches!(self.occurences[occ].kind, OccurenceKind::Header { .. }) {
            log::trace!("delete: skipping header {occ:?}");
            return;
        }

        let occ_entry = &self.occurences[occ];
        let prev = occ_entry.prev;
        let next = occ_entry.next;

        // Already unlinked from the circular list — nothing to do
        if prev.is_none() && next.is_none() {
            log::trace!("delete: skipping already-unlinked {occ:?}");
            return;
        }

        let var = self.var(occ);
        log::debug!("delete: removing {occ:?} (var={var})");

        // In a well-formed circular list both prev and next are always Some
        if let (Some(p), Some(n)) = (prev, next) {
            self.occurences[p].next = Some(n);
            self.occurences[n].prev = Some(p);
        }

        let occ_entry = &mut self.occurences[occ];
        // Remove links from occ itself
        occ_entry.prev = None;
        occ_entry.next = None;

        if let Some(redex) = self.kill_or_inline(var) {
            log::debug!("delete: new redex from {var}: {redex:?}");
            redex_collector(redex);
        }
    }

    fn merge_occurrence_headers(&mut self, from_header: OccurrenceId, to_header: OccurrenceId) {
        log::trace!("merge_occurrence_headers: {from_header:?} -> {to_header:?}");
        if from_header == to_header {
            return;
        }

        let from_first = self.occurences[from_header]
            .next
            .expect("occurrence header must always be linked");
        if from_first == from_header {
            return;
        }

        let from_last = self.occurences[from_header]
            .prev
            .expect("occurrence header must always be linked");
        let to_first = self.occurences[to_header]
            .next
            .expect("occurrence header must always be linked");
        let to_last = self.occurences[to_header]
            .prev
            .expect("occurrence header must always be linked");

        if to_first == to_header {
            self.occurences[to_header].next = Some(from_first);
            self.occurences[to_header].prev = Some(from_last);
            self.occurences[from_first].prev = Some(to_header);
            self.occurences[from_last].next = Some(to_header);
        } else {
            self.occurences[to_last].next = Some(from_first);
            self.occurences[from_first].prev = Some(to_last);
            self.occurences[from_last].next = Some(to_header);
            self.occurences[to_header].prev = Some(from_last);
        }

        self.occurences[from_header].next = Some(from_header);
        self.occurences[from_header].prev = Some(from_header);
    }

    /// Inline all usages of the variable `v`.
    pub fn inline(&mut self, v: VarId, mut redex_collector: impl FnMut(Redex)) {
        log::debug!("inline: inlining {v} (def={:?})", self.variables[v].def);
        match self.variables[v].def {
            VarDef::Cont(cont) => self.inline_cont(cont, &mut redex_collector),
            VarDef::Func(func) => self.inline_func(func, &mut redex_collector),
            _ => unreachable!("only variables defined by Cont or Func can be inlined"),
        }
    }

    fn inline_cont(&mut self, cont: ContId, redex_collector: &mut impl FnMut(Redex)) {
        log::debug!("inline_cont: inlining {cont} (var={})", self[cont].var);
        let parent = self[cont]
            .parent
            .expect("continuation must have a parent term; they cannot be root of the graph");
        let TermKind::Letk(mut conts, body) = self[parent].kind else {
            unreachable!("continuation's parent must be a Letk term");
        };

        let OccurrenceCount::One(occ) = self.occ_len(self[cont].var) else {
            unreachable!("continuation must have exactly one occurrence to be inlined");
        };

        let site = self.site(occ);
        let TermKind::Continue(cont_occ, args) = self[site].kind else {
            unreachable!("continuation occurrence must be a Continue term");
        };

        assert_eq!(
            self.var(cont_occ),
            self[cont].var,
            "continuation occurrence must be a Continue term targeting the continuation being inlined"
        );

        let args_len = args.len(&self.occ_pool);
        let params_len = self[cont].parameters.len(&self.var_pool);
        let variadic = self[cont].variadic;

        assert!(
            args_len == params_len || (args_len >= params_len && variadic.is_some()),
            "continuation application arity does not match continuation signature"
        );

        if conts.len(&self.cont_pool) == 1 {
            self.splice(parent, body);
        } else {
            // multiple continuations, we need to create a new Letk term without the inlined continuation and splice it in place of the original Letk
            let new_conts = conts
                .as_slice(&self.cont_pool)
                .iter()
                .copied()
                .filter(|&c| c != cont)
                .collect::<Vec<_>>();
            let new_conts_list = EntityList::from_slice(&new_conts, &mut self.cont_pool);
            let TermKind::Letk(old_conts, _) = &mut self[parent].kind else {
                unreachable!("parent must be a Letk term");
            };
            *old_conts = new_conts_list;
            conts.clear(&mut self.cont_pool);
        }

        let mut cont_body = self[cont].body();

        let new_variadic = if variadic.is_some() {
            let rest_expr = if args_len == params_len {
                Expression::Constant(Value::null())
            } else {
                let rest_args = args.as_slice(&self.occ_pool)[params_len..].to_vec();
                let rest_args_list = EntityList::from_slice(&rest_args, &mut self.occ_pool);
                Expression::PrimCall(Intrinsic::List, rest_args_list)
            };

            let new_variadic = self.add_var(VarDef::NotDefined);

            let rest_binding =
                self.add_term(TermKind::Let(new_variadic, rest_expr, cont_body), None);
            self.set_def(new_variadic, VarDef::Let(rest_binding));
            self[cont_body].parent = Some(ParentLink::Term(rest_binding));

            if let TermKind::Let(_, Expression::PrimCall(_, rest_args), _) = self[rest_binding].kind
            {
                let rest_args = rest_args.as_slice(&self.occ_pool).to_vec();
                for rest_arg in rest_args {
                    self.set_site(rest_arg, rest_binding);
                }
            }

            cont_body = rest_binding;
            Some(new_variadic)
        } else {
            None
        };

        self.splice(site, cont_body);

        for i in 0..params_len {
            let arg_occ = args
                .get(i, &self.occ_pool)
                .expect("argument occurrence must exist");
            let param_var = self[cont]
                .parameters
                .get(i, &self.var_pool)
                .expect("parameter variable must exist");
            self.subst(param_var, arg_occ);
            self.delete(arg_occ, redex_collector)
        }

        if let Some((variadic_param, variadic_subst)) = variadic.zip(new_variadic) {
            self.merge_occurrences(variadic_param, variadic_subst);
        }
    }

    fn inline_func(&mut self, func: FuncId, redex_collector: &mut impl FnMut(Redex)) {
        log::debug!("inline_func: inlining {func} (var={})", self[func].var);
        let parent = self[func]
            .parent
            .expect("function must have a parent term; they cannot be root of the graph");
        let TermKind::Fix(mut funcs, body) = self[parent].kind else {
            unreachable!("function's parent must be a Fix term");
        };

        let OccurrenceCount::One(occ) = self.occ_len(self[func].var) else {
            unreachable!("function must have exactly one occurrence to be inlined");
        };

        let site = self.site(occ);
        let TermKind::App(func_occ, retk_occ, args) = self[site].kind else {
            unreachable!("function occurrence must be an App term");
        };

        assert_eq!(
            self.var(func_occ),
            self[func].var,
            "function occurrence must be an App term targeting the function being inlined"
        );

        let args_len = args.len(&self.occ_pool);
        let params_len = self[func].parameters.len(&self.var_pool);
        let variadic = self[func].variadic;

        assert!(
            args_len == params_len || (args_len >= params_len && variadic.is_some()),
            "function application arity does not match function signature"
        );

        if funcs.len(&self.func_pool) == 1 {
            self.splice(parent, body);
        } else {
            // multiple functions, we need to create a new Fix term without the inlined function and splice it in place of the original Fix
            let new_funcs = funcs
                .as_slice(&self.func_pool)
                .iter()
                .copied()
                .filter(|&f| f != func)
                .collect::<Vec<_>>();
            let new_funcs_list = EntityList::from_slice(&new_funcs, &mut self.func_pool);
            let TermKind::Fix(old_funcs, _) = &mut self[parent].kind else {
                unreachable!("parent must be a Fix term");
            };
            *old_funcs = new_funcs_list;
            funcs.clear(&mut self.func_pool);
        }

        let mut func_body = self[func].body();

        let new_variadic = if variadic.is_some() {
            let rest_expr = if args_len == params_len {
                Expression::Constant(Value::null())
            } else {
                let rest_args = args.as_slice(&self.occ_pool)[params_len..].to_vec();
                let rest_args_list = EntityList::from_slice(&rest_args, &mut self.occ_pool);
                Expression::PrimCall(Intrinsic::List, rest_args_list)
            };

            let new_variadic = self.add_var(VarDef::NotDefined);

            let rest_binding =
                self.add_term(TermKind::Let(new_variadic, rest_expr, func_body), None);
            self.set_def(new_variadic, VarDef::Let(rest_binding));
            self[func_body].parent = Some(ParentLink::Term(rest_binding));

            if let TermKind::Let(_, Expression::PrimCall(_, rest_args), _) = self[rest_binding].kind
            {
                let rest_args = rest_args.as_slice(&self.occ_pool).to_vec();
                for rest_arg in rest_args {
                    self.set_site(rest_arg, rest_binding);
                }
            }

            func_body = rest_binding;
            Some(new_variadic)
        } else {
            None
        };

        self.splice(site, func_body);

        for i in 0..params_len {
            let arg_occ = args
                .get(i, &self.occ_pool)
                .expect("argument occurrence must exist");
            let param_var = self[func]
                .parameters
                .get(i, &self.var_pool)
                .expect("parameter variable must exist");
            // Substitute all usages of function parameter by the corresponding argument occurrence.
            self.subst(param_var, arg_occ);
            // Delete the argument occurrence from the graph, since it is now inlined and should not be considered a separate variable.
            self.delete(arg_occ, redex_collector)
        }

        self.subst(self[func].return_continuation, retk_occ);
        self.delete(retk_occ, redex_collector);

        if let Some((variadic_param, variadic_subst)) = variadic.zip(new_variadic) {
            self.merge_occurrences(variadic_param, variadic_subst);
        }
    }

    /// Splice `to` inplace of `from`.
    pub fn splice(&mut self, from: TermId, to: TermId) {
        log::trace!("splice: replacing {from:?} with {to:?}");
        let parent = self[from].parent;
        // Update parent link of `to` to point to `from`'s parent
        self[to].parent = parent;
        match parent {
            None => unreachable!("term is not connected to the graph"),
            Some(ParentLink::Term(parent)) => {
                let parent_term = &mut self.terms[parent];
                match &mut parent_term.kind {
                    TermKind::Let(_, _, body) | TermKind::Letk(_, body) => {
                        *body = to;
                    }
                    TermKind::Fix(_, body) => {
                        *body = to;
                    }
                    _ => unreachable!("invalid graph"),
                }
            }
            Some(ParentLink::Func(func)) => {
                let func = &mut self.functions[func];
                assert_eq!(
                    func.body(),
                    from,
                    "invalid graph: function body must be the term being spliced"
                );
                func.body = Some(to);
            }

            Some(ParentLink::Cont(cont)) => {
                let cont = &mut self.continuations[cont];
                assert_eq!(
                    cont.body(),
                    from,
                    "invalid graph: continuation body must be the term being spliced"
                );
                cont.body = Some(to);
            }
        }
    }

    pub fn insert_before(&mut self, target: TermId, new_term: TermKind<'gc>) -> TermId {
        log::trace!("insert_before: inserting before {target:?}");
        let parent = self[target]
            .parent
            .expect("term is not connected to the graph");
        let new_term_id = self.add_term(new_term, Some(parent));
        match parent {
            ParentLink::Term(parent) => {
                let parent_term = &mut self.terms[parent];
                match &mut parent_term.kind {
                    TermKind::Let(_, _, body) | TermKind::Letk(_, body) => {
                        assert_eq!(
                            *body, target,
                            "invalid graph: target term must be the body of its parent"
                        );
                        *body = new_term_id;
                    }
                    TermKind::Fix(_, body) => {
                        assert_eq!(
                            *body, target,
                            "invalid graph: target term must be the body of its parent"
                        );
                        *body = new_term_id;
                    }
                    _ => unreachable!("invalid graph"),
                }
            }
            ParentLink::Func(func) => {
                let func = &mut self.functions[func];
                assert_eq!(
                    func.body(),
                    target,
                    "invalid graph: target term must be the body of its parent"
                );
                func.body = Some(new_term_id);
            }

            ParentLink::Cont(cont) => {
                let cont = &mut self.continuations[cont];
                assert_eq!(
                    cont.body(),
                    target,
                    "invalid graph: target term must be the body of its parent"
                );
                cont.body = Some(new_term_id);
            }
        }
        new_term_id
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum OccurrenceCount {
    Zero,
    One(OccurrenceId),
    Many,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Redex {
    Dead(VarId),
    Inline(VarId),
}
