//! Per-site shared closure environments (`EnvRecord`).
//!
//! A closure-creation site (a merged `Fix`/`Letk` group) either allocates one
//! fresh `EnvRecord` holding the free variables its members share, or reuses
//! the enclosing site's record (nested reuse). Reified continuation closures
//! participate like ordinary closures; non-reified continuations are contified
//! and never allocate.
//!
//! Knobs: `CAPY_SHARE_MIN_OVERLAP`, `CAPY_SHARE_CHAIN_COST`, `CAPY_SHARE_ORDER`.

use std::collections::{HashMap, HashSet};

use super::analysis::recursive_functions;
use super::flow::FlowAnalysis;
use super::graph::{BoundVar, FunctionId, Graph, Subterm, TermId, TermKind};
use super::reify::{BinderSet, GraphReifyInfo};

/// Per-member closure layout at a shared site.
#[derive(Clone, Debug)]
pub struct MemberShare {
    pub function: FunctionId,
    /// The member's free variables not in the record, in free-set order (all
    /// of them for flat members).
    pub own_vars: Vec<BoundVar>,
    /// True: flat closure, no env record.
    pub flat: bool,
}

/// The sharing decision for one closure-creation site.
#[derive(Clone, Debug)]
pub struct SiteShare {
    /// True: allocate a fresh record. False: reuse the enclosing function's
    /// record (no `MakeEnv`); each member's env pointer is the enclosing env.
    pub allocate: bool,
    /// Record contents in slot order. Empty for reuse sites (the record
    /// belongs to the enclosing site).
    pub record_vars: Vec<BoundVar>,
    pub members: Vec<MemberShare>,
}

/// Per-function view of a sharing decision.
#[derive(Clone, Debug)]
pub struct FunctionShare {
    /// Record-resident free variables with their record slot indices.
    pub record: Vec<(BoundVar, usize)>,
    /// Private captures (closure slots 1..).
    pub own: Vec<BoundVar>,
}

/// All sharing decisions for one compilation unit.
#[derive(Clone, Debug, Default)]
pub struct SharePlan {
    sites: HashMap<TermId, SiteShare>,
    by_function: HashMap<FunctionId, FunctionShare>,
}

impl SharePlan {
    pub fn is_empty(&self) -> bool {
        self.sites.is_empty()
    }

    pub fn sites(&self) -> impl Iterator<Item = (&TermId, &SiteShare)> {
        self.sites.iter()
    }

    pub fn site(&self, term: TermId) -> Option<&SiteShare> {
        self.sites.get(&term)
    }

    pub fn function_share(&self, function: FunctionId) -> Option<&FunctionShare> {
        self.by_function.get(&function)
    }
}

/// Whether allocating a record for `members` members and `record_vars` slots
/// is worth it: the per-member duplicated slots saved outweigh the record
/// allocation plus one pointer per member.
fn is_profitable(members: usize, record_vars: usize) -> bool {
    record_vars >= 1 && (members - 1) * record_vars > members + 2
}

/// Min record-resident vars a member must use to join a record.
const DEFAULT_MIN_OVERLAP: usize = 2;
/// Default record materialization cost in words.
const DEFAULT_CHAIN_COST: usize = 3;

fn knob_usize(name: &str, default: usize) -> usize {
    std::env::var(name)
        .ok()
        .and_then(|v| v.parse().ok())
        .unwrap_or(default)
}

fn min_overlap() -> usize {
    knob_usize("CAPY_SHARE_MIN_OVERLAP", DEFAULT_MIN_OVERLAP).max(1)
}

fn chain_cost() -> usize {
    knob_usize("CAPY_SHARE_CHAIN_COST", DEFAULT_CHAIN_COST).max(1)
}

fn order_required() -> bool {
    std::env::var("CAPY_SHARE_ORDER").map_or(false, |v| v == "1")
}

/// Compute shared-env decisions for every closure-creation site.
pub fn analyze_sharing<'gc>(
    graph: &Graph<'gc>,
    reify: &GraphReifyInfo,
    stages: &super::analysis::StageAnalysis,
    flow: &FlowAnalysis,
    root: TermId,
    enabled: bool,
) -> SharePlan {
    if !enabled {
        return SharePlan::default();
    }
    let recursive = recursive_functions(graph, reify);
    let nested = NestedSites::new(graph, reify);
    let mut collector = Collector {
        graph,
        reify,
        stages,
        flow,
        recursive: &recursive,
        nested: &nested,
        records: Vec::new(),
        sites: HashMap::new(),
        by_function: HashMap::new(),
        visited: HashSet::new(),
    };
    collector.walk_term(root, None);
    SharePlan {
        sites: collector.sites,
        by_function: collector.by_function,
    }
}

/// Free-variable set of a function or continuation.
fn free_func_of<'a>(
    graph: &Graph<'_>,
    reify: &'a GraphReifyInfo,
    function: FunctionId,
) -> &'a BinderSet {
    if graph[function].cont.is_some() {
        reify.free_vars.function(function)
    } else {
        reify.free_vars.continuation(function)
    }
}

/// Per-function inventory of closure-creation sites directly nested in a
/// function body (a site inside a member's body belongs to that member).
struct NestedSites {
    /// For each function: the member free-variable sets of the sites
    /// directly nested in its body, in site order.
    by_function: HashMap<FunctionId, Vec<Vec<BoundVar>>>,
    /// Union of all those free-variable sets (the chain-content universe).
    unions: HashMap<FunctionId, BinderSet>,
}

impl NestedSites {
    fn new<'gc>(graph: &Graph<'gc>, reify: &GraphReifyInfo) -> Self {
        let mut sites: HashMap<FunctionId, Vec<Vec<BoundVar>>> = HashMap::new();
        let mut unions: HashMap<FunctionId, BinderSet> = HashMap::new();
        // Each function's body subtree is walked once with its own visited
        // set: a site directly nested in the body (Fix/Letk terms at body
        // level) is collected for the enclosing function only; member bodies
        // are collected when their own function is processed (a shared
        // visited set would let the enclosing walk steal them).
        for function in reify
            .functions
            .iter()
            .chain(reify.continuations.iter())
            .copied()
        {
            let mut visited = HashSet::new();
            let mut collected = Vec::new();
            walk_body_sites(graph, reify, graph[function].body, &mut visited, &mut collected);
            let mut union = BinderSet::new();
            for site in &collected {
                for var in site.iter().copied() {
                    union.insert(var);
                }
            }
            sites.insert(function, collected);
            unions.insert(function, union);
        }
        Self {
            by_function: sites,
            unions,
        }
    }

    fn sites_of(&self, function: FunctionId) -> &[Vec<BoundVar>] {
        self.by_function.get(&function).map_or(&[], |s| s)
    }

    fn union_of(&self, function: FunctionId) -> &BinderSet {
        self.unions
            .get(&function)
            .expect("union present for every function")
    }
}

/// Collect the closure sites directly nested in `function`'s body (not inside
/// any member's body). Each site contributes the free sets of its members.
fn walk_body_sites<'gc>(
    graph: &Graph<'gc>,
    reify: &GraphReifyInfo,
    link: Subterm,
    visited: &mut HashSet<TermId>,
    collected: &mut Vec<Vec<BoundVar>>,
) {
    let Some(term) = graph.read_term_link(link) else {
        return;
    };
    if !visited.insert(term) {
        return;
    }
    match graph[term].kind {
        TermKind::LetVal(_, body) => {
            walk_body_sites(graph, reify, body, visited, collected);
        }
        TermKind::Fix(functions, body) | TermKind::Letk(functions, body) => {
            // The site itself (its members' free sets) belongs to the
            // enclosing function's list; member bodies are collected when
            // their own function is processed.
            let members = live_functions(graph, &functions);
            if !members.is_empty() {
                collected.push(
                    members
                        .iter()
                        .copied()
                        .flat_map(|m| free_func_of(graph, reify, m).iter())
                        .collect(),
                );
            }
            walk_body_sites(graph, reify, body, visited, collected);
        }
        TermKind::If(_, consequent, alternative, _) => {
            walk_body_sites(graph, reify, consequent, visited, collected);
            walk_body_sites(graph, reify, alternative, visited, collected);
        }
        TermKind::Continue(..) | TermKind::App(..) | TermKind::Raise(..) => {}
    }
}

struct Collector<'a, 'gc> {
    graph: &'a Graph<'gc>,
    reify: &'a GraphReifyInfo,
    stages: &'a super::analysis::StageAnalysis,
    flow: &'a FlowAnalysis,
    recursive: &'a HashSet<FunctionId>,
    nested: &'a NestedSites,
    /// Every materialized record's `(var, slot)` content, indexed by
    /// `RecordRef::index`.
    records: Vec<Vec<(BoundVar, usize)>>,
    sites: HashMap<TermId, SiteShare>,
    by_function: HashMap<FunctionId, FunctionShare>,
    visited: HashSet<TermId>,
}

/// A shared record in scope: its owner function (for escape-based decisions)
/// and its content index into `Collector::records`.
#[derive(Clone, Copy)]
struct RecordRef {
    owner: FunctionId,
    index: usize,
}

impl<'a, 'gc> Collector<'a, 'gc> {
    fn record(&self, rec: RecordRef) -> &[(BoundVar, usize)] {
        &self.records[rec.index]
    }

    fn walk_term(&mut self, term: TermId, env: Option<RecordRef>) {
        if !self.visited.insert(term) {
            return;
        }
        let kind = self.graph[term].kind;
        let mut walk_link = |collector: &mut Self, link: Subterm| {
            if let Some(child) = collector.graph.read_term_link(link) {
                collector.walk_term(child, env);
            }
        };
        match kind {
            TermKind::LetVal(_, body) => walk_link(self, body),
            TermKind::Fix(functions, body) => {
                let live = live_functions(self.graph, &functions);
                let decision = self.decide_site(term, &live, env);
                for function in live.iter().copied() {
                    let body = self.graph[function].body;
                    if let Some(child) = self.graph.read_term_link(body) {
                        let member_env = match &decision {
                            Some(record) if self.by_function.contains_key(&function) => {
                                Some(*record)
                            }
                            _ => None,
                        };
                        self.walk_term(child, member_env);
                    }
                }
                walk_link(self, body);
            }
            TermKind::Letk(functions, body) => {
                let live = live_functions(self.graph, &functions);
                let (reified, local): (Vec<_>, Vec<_>) = live
                    .into_iter()
                    .partition(|function| self.graph[*function].is_reified);
                let decision = self.decide_site(term, &reified, env);
                for function in reified.iter().copied() {
                    let body = self.graph[function].body;
                    if let Some(child) = self.graph.read_term_link(body) {
                        let member_env = match &decision {
                            Some(record) if self.by_function.contains_key(&function) => {
                                Some(*record)
                            }
                            _ => None,
                        };
                        self.walk_term(child, member_env);
                    }
                }
                // Non-reified continuations are inlined into the enclosing
                // procedure; the enclosing record (this term's env) stays
                // accessible to their nested sites.
                for function in local.iter().copied() {
                    if let Some(child) = self.graph.read_term_link(self.graph[function].body) {
                        self.walk_term(child, env);
                    }
                }
                walk_link(self, body);
            }
            TermKind::If(_, consequent, alternative, _) => {
                walk_link(self, consequent);
                walk_link(self, alternative);
            }
            TermKind::Continue(..) | TermKind::App(..) | TermKind::Raise(..) => {}
        }
    }

    /// Decide the sharing of one closure site.
    ///
    /// Returns the record the member closures point at (its `RecordRef`),
    /// threaded into shared members' bodies for nested reuse.
    fn decide_site(
        &mut self,
        term: TermId,
        members: &[FunctionId],
        env: Option<RecordRef>,
    ) -> Option<RecordRef> {
        if members.is_empty() {
            return None;
        }

        // Per-member free sets (owned: keeps `&mut self` calls borrow-free).
        let frees: Vec<BinderSet> = members
            .iter()
            .copied()
            .map(|m| free_func_of(self.graph, self.reify, m).clone())
            .collect();

        // ---- 1. Reuse of the enclosing record (cross-site chain). ----
        if let Some(enclosing) = env {
            let slots: Vec<(BoundVar, usize)> = self.record(enclosing).to_vec();
            let reused_vars: Vec<BoundVar> = slots.iter().map(|(v, _)| *v).collect();
            let threshold = self.effective_overlap(enclosing.owner);
            let participating: Vec<usize> = members
                .iter()
                .copied()
                .enumerate()
                .filter(|(i, member)| {
                    overlap_count_list(&frees[*i], &reused_vars) >= threshold
                        && self.member_carry_record(*member, &frees[*i])
                })
                .map(|(i, _)| i)
                .collect();
            if !participating.is_empty()
                && (!order_required()
                    || self.order_ok(&participating, members, &frees, &reused_vars))
            {
                for i in participating.iter().copied() {
                    let member = members[i];
                    self.record_share(member, &frees[i], &slots);
                }
                let members_share = self.site_members(members, &frees, &participating, &slots);
                self.sites.insert(
                    term,
                    SiteShare {
                        allocate: false,
                        record_vars: Vec::new(),
                        members: members_share,
                    },
                );
                return Some(enclosing);
            }
        }

        // ---- 2. Allocate a fresh record. ----
        // Single member: the cross-site chain rule. The record holds the
        // member's free variables that its nested sites reuse.
        if members.len() == 1 {
            let member = members[0];
            let free = &frees[0];
            let record = self.chain_record(member, free);
            if record.is_empty() {
                return None;
            }
            let benefit = self.chain_benefit(member, &record);
            if benefit > chain_cost() {
                return Some(self.allocate_record(term, members, &frees, &record, &[0], member));
            }
            return None;
        }

        // Multi-member: intersection of the members' free sets minus the
        // site bindings (Shao & Appel subset rule).
        let mut record = BinderSet::new();
        for var in frees[0].iter() {
            record.insert(var);
        }
        for free in frees.iter().skip(1) {
            let other = free;
            let mut kept = BinderSet::new();
            for var in record.iter() {
                if other.contains(var) {
                    kept.insert(var);
                }
            }
            record = kept;
        }
        for function in members.iter().copied() {
            record.remove(self.graph.function_binder_rooted(self.graph[function].var));
        }

        // Participation: a member uses enough of the record and (if hot) has
        // a chain of its own worth a record.
        let r = record.iter().count();
        let participating: Vec<usize> = members
            .iter()
            .copied()
            .enumerate()
            .filter(|(i, member)| {
                overlap_count(&frees[*i], &record) >= min_overlap()
                    && self.member_carry_record(*member, &frees[*i])
            })
            .map(|(i, _)| i)
            .collect();
        let k = participating.len();
        if k >= 2 && is_profitable(k, r) {
            return Some(self.allocate_record(
                term,
                members,
                &frees,
                &record,
                &participating,
                members[participating[0]],
            ));
        }

        // A single participant remains: fall back to the chain rule for it.
        if k == 1 {
            let i = participating[0];
            let member = members[i];
            let chain = self.chain_record(member, &frees[i]);
            if !chain.is_empty() && self.chain_benefit(member, &chain) > chain_cost() {
                return Some(
                    self.allocate_record(term, members, &frees, &chain, &participating, member),
                );
            }
        }

        None
    }

    /// The chain record of `member`: its free variables that some nested
    /// site also references, minus the member's own binding.
    fn chain_record(&self, member: FunctionId, free: &BinderSet) -> BinderSet {
        let union = self.nested.union_of(member);
        let mut record = BinderSet::new();
        for var in free.iter() {
            if union.contains(var) {
                record.insert(var);
            }
        }
        record.remove(self.graph.function_binder_rooted(self.graph[member].var));
        record
    }

    /// Allocate a site record, record per-member layouts, and register the
    /// site decision. Returns the new record reference.
    fn allocate_record(
        &mut self,
        term: TermId,
        members: &[FunctionId],
        frees: &[BinderSet],
        record: &BinderSet,
        participating: &[usize],
        owner: FunctionId,
    ) -> RecordRef {
        let slots: Vec<(BoundVar, usize)> =
            record.iter().enumerate().map(|(i, v)| (v, i)).collect();
        let index = self.records.len();
        self.records.push(slots.clone());
        let record_ref = RecordRef { owner, index };
        for i in participating.iter().copied() {
            let member = members[i];
            self.record_share(member, &frees[i], &slots);
        }
        let members_share = self.site_members(members, frees, participating, &slots);
        self.sites.insert(
            term,
            SiteShare {
                allocate: true,
                record_vars: record.iter().collect(),
                members: members_share,
            },
        );
        record_ref
    }

    /// Record the member's layout for a shared site: `record` slots (the
    /// member's own record-resident variables, filtered to what it uses) and
    /// `own` = the rest of its free set.
    fn record_share(
        &mut self,
        member: FunctionId,
        free: &BinderSet,
        slots: &[(BoundVar, usize)],
    ) {
        let mut record = Vec::new();
        let mut record_vars = BinderSet::new();
        for (var, slot) in slots.iter().copied() {
            if free.contains(var) {
                record.push((var, slot));
                record_vars.insert(var);
            }
        }
        let own: Vec<BoundVar> = free
            .iter()
            .filter(|var| !record_vars.contains(*var))
            .collect();
        self.by_function.insert(
            member,
            FunctionShare { record, own },
        );
    }

    /// Member list for a decided site: participating members are shared
    /// (env + private captures), the rest stay flat.
    fn site_members(
        &self,
        members: &[FunctionId],
        frees: &[BinderSet],
        participating: &[usize],
        record_slots: &[(BoundVar, usize)],
    ) -> Vec<MemberShare> {
        let record_vars: Vec<BoundVar> = record_slots.iter().map(|(v, _)| *v).collect();
        members
            .iter()
            .enumerate()
            .map(|(i, function)| {
                if participating.contains(&i) {
                    let own: Vec<BoundVar> = frees[i]
                        .iter()
                        .filter(|var| !record_vars.contains(var))
                        .collect();
                    MemberShare {
                        function: *function,
                        own_vars: own,
                        flat: false,
                    }
                } else {
                    MemberShare {
                        function: *function,
                        own_vars: frees[i].iter().collect(),
                        flat: true,
                    }
                }
            })
            .collect()
    }

    /// Whether a member joins record sharing: hot (recursive) members stay
    /// flat unless their own chain benefits from a record.
    fn member_carry_record(&self, member: FunctionId, free: &BinderSet) -> bool {
        if !self.recursive.contains(&member) {
            return true;
        }
        let record = self.chain_record(member, free);
        !record.is_empty() && self.chain_benefit(member, &record) > chain_cost()
    }
    /// Effective overlap threshold at a reuse site: 1 for non-escaping owners
    /// (known call sites), else `min_overlap()`.
    fn effective_overlap(&self, owner: FunctionId) -> usize {
        let escapes = self
            .flow
            .function(owner)
            .map_or(true, |info| info.escapes);
        if escapes {
            min_overlap()
        } else {
            1
        }
    }

    /// Stage-ordering gate (only when `CAPY_SHARE_ORDER=1`): every use of a
    /// reused variable must precede every use of the member's private
    /// captures.
    fn order_ok(
        &self,
        participating: &[usize],
        members: &[FunctionId],
        frees: &[BinderSet],
        reused: &[BoundVar],
    ) -> bool {
        participating.iter().all(|i| {
            let own: Vec<BoundVar> = frees[*i]
                .iter()
                .filter(|var| !reused.contains(var))
                .collect();
            self.stages
                .function(members[*i])
                .map_or(true, |s| s.reused_before_own(reused, &own))
        })
    }

    /// Benefit of `member` materializing a record `record`: per closure site
    /// directly nested in its body, `max(|site_vars ∩ record| - 1, 0)` — each
    /// nested closure sharing `t` record vars replaces `t` inline slots with
    /// one pointer.
    fn chain_benefit(&self, member: FunctionId, record: &BinderSet) -> usize {
        if record.is_empty() {
            return 0;
        }
        let mut benefit = 0;
        for site in self.nested.sites_of(member) {
            let mut site_vars = BinderSet::new();
            site_vars.extend(site.iter().copied());
            benefit += overlap_count(&site_vars, record).saturating_sub(1);
        }
        benefit
    }
}

/// Number of variables in the intersection of two sets.
fn overlap_count(a: &BinderSet, b: &BinderSet) -> usize {
    a.iter().filter(|v| b.contains(*v)).count()
}

/// Number of variables of `a` present in the slice `b`.
fn overlap_count_list(a: &BinderSet, b: &[BoundVar]) -> usize {
    a.iter().filter(|v| b.contains(v)).count()
}

fn live_functions<'gc>(
    graph: &Graph<'gc>,
    functions: &super::graph::FunctionLinks,
) -> Vec<FunctionId> {
    graph
        .function_links_slice(functions)
        .iter()
        .copied()
        .filter_map(|link| graph.read_function_link(link))
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn profitability_threshold() {
        // (k-1)r > k+2
        // k=2: need r > 4
        assert!(!is_profitable(2, 4));
        assert!(is_profitable(2, 5));
        // k=3: (3-1)r > 5 -> r >= 3
        assert!(!is_profitable(3, 2));
        assert!(is_profitable(3, 3));
        // k=4: 3r > 6 -> r >= 3
        assert!(!is_profitable(4, 2));
        assert!(is_profitable(4, 3));
        // never share a single member or an empty record
        assert!(!is_profitable(1, 10));
        assert!(!is_profitable(2, 0));
    }

    #[test]
    fn overlap_count_works() {
        use cranelift_entity::EntityRef;
        let mut a = BinderSet::new();
        let mut b = BinderSet::new();
        for i in 0..5usize {
            a.insert(BoundVar::new(i));
        }
        for i in 3..9usize {
            b.insert(BoundVar::new(i));
        }
        assert_eq!(overlap_count(&a, &b), 2);
        assert_eq!(overlap_count(&b, &a), 2);
        assert_eq!(overlap_count(&a, &a), 5);
        assert_eq!(overlap_count(&BinderSet::new(), &b), 0);
    }

    #[test]
    fn chain_knobs_defaults() {
        // Defaults stay within the documented model.
        assert_eq!(min_overlap(), DEFAULT_MIN_OVERLAP);
        assert_eq!(chain_cost(), DEFAULT_CHAIN_COST);
    }

    #[test]
    fn fix_and_reified_letk_share_one_record() {
        use crate::{
            compiler::cps::{
                analysis::StageAnalysis,
                flow::FlowAnalysis,
                graph::Function,
                reify::reify_graph,
            },
            expander::core::fresh_lvar,
            runtime::{value::Value, Context, Scheme},
        };

        fn lvar<'gc>(ctx: Context<'gc>, name: &str) -> crate::expander::core::LVarRef<'gc> {
            fresh_lvar(ctx, ctx.intern(name))
        }

        fn make_function<'gc>(
            graph: &mut Graph<'gc>,
            ctx: Context<'gc>,
            name: &str,
            cont: Option<BoundVar>,
            body: Subterm,
        ) -> FunctionId {
            let var = graph.new_bound_var(lvar(ctx, name));
            let vars = graph.new_bound_vars([]);
            graph.new_function(Function {
                name: Value::new(false),
                source: Value::new(false),
                var,
                vars,
                variadic: None,
                cont,
                is_variadic: false,
                body,
                is_rec: false,
                unroll_count: 0,
                is_cold: false,
                is_noinline: false,
                is_reified: false,
                meta: Value::new(false),
            })
        }

        /// A leaf continuation term `Continue(cont, [shared...])`; returns its
        /// term link.
        fn continue_leaf<'gc>(
            graph: &mut Graph<'gc>,
            cont: BoundVar,
            args: &[BoundVar],
        ) -> Subterm {
            let link = graph.new_term_link(None);
            let cont_occ = graph.new_free_occ_for_binder(cont, link);
            let arg_occs: Vec<_> = args
                .iter()
                .copied()
                .map(|arg| graph.new_free_occ_for_binder(arg, link))
                .collect();
            let args = graph.new_free_vars(arg_occs);
            let parent = graph.new_parent_link(None);
            let term = graph.new_term(parent, TermKind::Continue(cont_occ, args), Value::new(false));
            graph.set_term_link(link, term);
            link
        }

        Scheme::new_uninit().enter(|ctx| {
            let mut graph = Graph::new();

            // Five variables shared by the fix member and the continuation
            // (k=2, r=5: (k-1)r > k+2 -> the record is profitable).
            let mut shared = Vec::new();
            for i in 0..5 {
                shared.push(graph.new_bound_var(lvar(ctx, &format!("s{i}"))));
            }
            let fret = graph.new_bound_var(lvar(ctx, "fret"));
            let callee = graph.new_bound_var(lvar(ctx, "callee"));

            // f: Fix member. frees(f) = shared (its return cont is removed).
            let f_body = continue_leaf(&mut graph, fret, &shared);
            let f = make_function(&mut graph, ctx, "f", Some(fret), f_body);

            // k: continuation member. frees(k) = shared ∪ {fret}.
            let k_body = continue_leaf(&mut graph, fret, &shared);
            let k = make_function(&mut graph, ctx, "k", None, k_body);

            // Letk(k, App(callee, [], k)): using k as the return continuation
            // of an App is a value use, so reify marks k reified.
            let app_link = graph.new_term_link(None);
            let callee_occ = graph.new_free_occ_for_binder(callee, app_link);
            let retk = graph.new_free_occ_for_binder(graph[k].var, app_link);
            let no_args = graph.new_free_vars([]);
            let app_parent = graph.new_parent_link(None);
            let app = graph.new_term(
                app_parent,
                TermKind::App(callee_occ, no_args, retk),
                Value::new(false),
            );
            graph.set_term_link(app_link, app);

            let letk_link = graph.new_term_link(None);
            let letk_parent = graph.new_parent_link(None);
            let k_link = graph.new_function_link(Some(k));
            let k_links = graph.new_function_links([k_link]);
            let letk = graph.new_term(
                letk_parent,
                TermKind::Letk(k_links, app_link),
                Value::new(false),
            );
            graph.set_term_link(letk_link, letk);

            let fix_link = graph.new_term_link(None);
            let fix_parent = graph.new_parent_link(None);
            let f_link = graph.new_function_link(Some(f));
            let f_links = graph.new_function_links([f_link]);
            let fix = graph.new_term(
                fix_parent,
                TermKind::Fix(f_links, letk_link),
                Value::new(false),
            );
            graph.set_term_link(fix_link, fix);

            let entry = make_function(&mut graph, ctx, "entry", Some(fret), fix_link);

            // Reify first (the merge needs `is_reified`), then merge the
            // Fix/Letk chain into one site, then share.
            let reify = reify_graph(&mut graph, entry);
            super::super::fixmerge::merge_nested_groups(&mut graph, fix_link);
            let stages = StageAnalysis::new(&graph, &reify);
            let flow = FlowAnalysis::new(&graph, &reify);
            let plan = analyze_sharing(&graph, &reify, &stages, &flow, fix, true);

            let mut sites = plan.sites();
            let (site_term, site) = sites.next().expect("one shared site");
            assert!(sites.next().is_none());
            assert_eq!(*site_term, fix);
            assert!(site.allocate, "the merged group must allocate a record");
            assert_eq!(site.record_vars.len(), 5);
            assert_eq!(site.members.len(), 2);
            assert!(site.members.iter().any(|m| m.function == f));
            assert!(site.members.iter().any(|m| m.function == k));
            assert!(site.members.iter().all(|m| !m.flat));
        });
    }
}