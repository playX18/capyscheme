//! Functions and types for maintaining mutable graph.

use std::{
    fmt,
    hash::Hash,
    marker::PhantomData,
    ops::{Index, IndexMut},
};

use crate::runtime::vm::exceptions::RaiseKind;
use crate::{expander::core::LVarRef, runtime::value::Value};
use cranelift_entity::{
    packed_option::{PackedOption, ReservedValue},
    EntityList, EntityRef, ListPool, PrimaryMap,
};

pub use super::worklist::{GraphWorklist, WorklistQueue};

pub const VERBOSE: bool = false;

macro_rules! verbose_log {
    ($($arg:tt)*) => {
        if VERBOSE {
            eprintln!($($arg)*);
        }
    };
}

macro_rules! concrete_entity {
    ($name:ident, $display_prefix:literal) => {
        #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        #[repr(transparent)]
        pub struct $name(u32);

        impl EntityRef for $name {
            #[inline]
            fn new(index: usize) -> Self {
                debug_assert!(index < u32::MAX as usize);
                Self(index as u32)
            }

            #[inline]
            fn index(self) -> usize {
                self.0 as usize
            }
        }

        impl ReservedValue for $name {
            #[inline]
            fn reserved_value() -> Self {
                Self(u32::MAX)
            }

            #[inline]
            fn is_reserved_value(&self) -> bool {
                self.0 == u32::MAX
            }
        }

        impl $name {
            #[inline]
            pub fn from_u32(index: u32) -> Self {
                debug_assert!(index < u32::MAX);
                Self(index)
            }

            #[inline]
            pub fn as_u32(self) -> u32 {
                self.0
            }
        }

        impl fmt::Debug for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                fmt::Display::fmt(self, f)
            }
        }

        impl fmt::Display for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, concat!($display_prefix, "{}"), self.0)
            }
        }
    };
}

concrete_entity!(TermLink, "term_link");
concrete_entity!(ExprLink, "expr_link");
concrete_entity!(ParentLink, "parent_link");
concrete_entity!(FunctionLink, "function_link");
concrete_entity!(FreeOccLink, "free_occ");
concrete_entity!(PointId, "point");
concrete_entity!(BoundVar, "bound_var");
concrete_entity!(TermId, "term");
concrete_entity!(ExprId, "expr");
concrete_entity!(FunctionId, "function");

/// Mutable edge to a child [`Term`].
///
/// Terms own their children indirectly. This allows beta-reduction and case
/// simplification to replace a child by writing through the active link instead
/// of rebuilding all ancestors.
pub type Subterm = TermLink;

/// Mutable edge to a child [`Expr`].
///
/// Expressions are separated from terms: expressions compute bindable values,
/// while terms represent sequencing and control flow.
pub type Subexpr = ExprLink;

/// Parent link stored inside every term and expression node.
///
/// This is the crucial extra edge that makes worklist optimization safe. A
/// queued link can become stale after another optimization rewrites the same
/// region. The active-link lookup can read the node's `Uplink`, ask the parent
/// for its current child links, and select the one whose child's parent link
/// matches the queued node.
pub type Uplink = ParentLink;

/// One use occurrence of a variable.
///
/// All occurrences for the same live binder are stored in one circular doubly
/// linked list. The occurrence payload contains both binder lookup (`point`) and
/// owner-term lookup (`owner`).
pub type FreeOcc = FreeOccLink;

/// Alias used where an occurrence denotes an ordinary value variable.
pub type FreeVar = FreeOcc;

/// Alias used where an occurrence denotes a continuation variable.
///
/// The underlying graph mechanics are identical to [`FreeVar`]; the separate
/// name documents control-flow intent at call sites.
pub type ContVar = FreeVar;

/// Compact graph-owned list of free variable occurrences.
pub type FreeVars = EntityList<FreeVar>;

/// Compact graph-owned list of bound variables.
pub type BoundVars = EntityList<BoundVar>;

/// Compact graph-owned list of nullable function links.
pub type FunctionLinks = EntityList<FunctionLink>;

/// A binding paired with the mutable graph object it binds.
///
/// For value bindings the payload is usually [`Subexpr`]. For functions it is a
/// link to [`Function`], so dead function bindings can be nulled out without
/// changing the surrounding list of bindings.
pub type Known<V> = (BoundVar, V);

/// Nullable mutable edge in the graph.
///
/// This alias is retained for simple call sites. Concrete links are preferred
/// for stored fields because each link kind has a separate primary map.
pub type Link<T> = PackedOption<T>;

/// Read a compact nullable link value.
pub fn read_link<T: ReservedValue>(link: Link<T>) -> Option<T> {
    link.expand()
}

/// Node in a circular doubly linked occurrence ring.
///
/// A ring is used instead of a `Vec` because substitution, dead-use deletion,
/// and binder merging are all common operations:
///
/// * appending a new occurrence is O(1),
/// * splicing two occurrence lists together is O(1),
/// * removing one dead occurrence is O(1).
pub type RingNode = FreeOccPayload;

/// Occurrence ring handle retained as an alias for older graph helpers.
pub type Ring = FreeOcc;

/// Union-find handle used by free occurrences to find their current binder.
///
/// Each [`FreeOcc`] stores a point whose descriptor is the current
/// representative [`BoundVar`]. When one variable is substituted for another,
/// callers union the old point with the representative point of the replacement
/// variable. A later binder lookup follows the union-find parent chain.
pub type Point = PointId;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct UfNode {
    pub parent: PointId,
    pub descriptor: BoundVar,
}

/// One use occurrence payload.
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct FreeOccPayload {
    /// Union-find point whose descriptor is the current representative binder.
    pub point: PointId,
    /// Back-link to the term link that directly contains this occurrence.
    ///
    /// Replacement updates this field so worklist entries derived from variable
    /// uses can find the redex or binding that still owns the occurrence.
    pub owner: TermLink,
    pub prev: FreeOcc,
    pub next: FreeOcc,
}

impl fmt::Debug for FreeOccPayload {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("FreeOccPayload")
            .field("point", &self.point)
            .field("owner", &self.owner)
            .field("prev", &self.prev)
            .field("next", &self.next)
            .finish()
    }
}

/// A binding variable payload stored in the graph arena.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct BoundVarData<'gc> {
    pub var: LVarRef<'gc>,
    pub occ: PackedOption<FreeOcc>,
}

/// Parent of a term or expression node.
///
/// Functions do not have a self-link in this representation, so a function body
/// uses `Func` instead of pretending the function is a regular term.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Parent {
    Term(TermId),
    Func(FunctionId),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ActiveLinkStatus {
    ActiveSubterm(Subterm),
    TermIsDead,
}

/// Sequencing and control-flow node.
///
/// A `Term` is a chain of bindings ending in control flow (`Call`, `Cont`,
/// `If`, or `Case`). Like [`Expr`], each variant carries its [`Uplink`]. Child
/// terms are stored through [`Subterm`] links, so replacing a child is a local
/// link write.
#[derive(Clone, Copy, Debug)]
pub struct Term<'gc> {
    pub link: Uplink,
    pub kind: TermKind,
    pub source: Value<'gc>,
}

#[derive(Clone, Copy, Debug)]
pub enum TermKind {
    LetVal(Known<Subexpr>, Subterm),
    Fix(FunctionLinks, Subterm),
    Letk(FunctionLinks, Subterm),

    If(FreeVar, Subterm, Subterm),
    Continue(ContVar, FreeVars),
    App(FreeVar, FreeVars, ContVar),
    Raise(RaiseKind, FreeVars),
}

#[derive(Clone, Copy, Debug)]
pub struct Expr<'gc> {
    pub link: Uplink,
    pub kind: ExprKind<'gc>,
    pub source: Value<'gc>,
}

#[derive(Clone, Copy, Debug)]
pub enum ExprKind<'gc> {
    Literal(Value<'gc>),
    PrimCall(Value<'gc>, FreeVars),
}

#[derive(Clone, Copy, Debug)]
pub struct Function<'gc> {
    pub name: Value<'gc>,
    pub source: Value<'gc>,
    pub var: BoundVar,
    pub vars: BoundVars,
    pub variadic: Option<BoundVar>,
    /// Functions bind this to return cont, conts have this set to None.
    pub cont: Option<BoundVar>,
    pub is_variadic: bool,
    pub body: Subterm,
    pub is_rec: bool,
    pub unroll_count: usize,
    pub is_cold: bool,
    pub is_noinline: bool,
    pub is_reified: bool,
    pub meta: Value<'gc>,
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct GraphStats {
    pub term_links: usize,
    pub expr_links: usize,
    pub parent_links: usize,
    pub function_links: usize,
    pub free_occurrences: usize,
    pub points: usize,
    pub bound_vars: usize,
    pub terms: usize,
    pub exprs: usize,
    pub functions: usize,
}

/// Owner of every graph-structural arena.
///
/// Runtime payloads such as [`Value`] and [`LVarRef`] still live in the runtime
/// GC. This type owns only graph identity and mutable graph edges.
pub struct Graph<'gc> {
    term_links: PrimaryMap<TermLink, Option<TermId>>,
    expr_links: PrimaryMap<ExprLink, Option<ExprId>>,
    parent_links: PrimaryMap<ParentLink, Option<Parent>>,
    function_links: PrimaryMap<FunctionLink, Option<FunctionId>>,
    free_occ_links: PrimaryMap<FreeOcc, FreeOccPayload>,
    points: PrimaryMap<PointId, UfNode>,
    bound_vars: PrimaryMap<BoundVar, BoundVarData<'gc>>,
    terms: PrimaryMap<TermId, Term<'gc>>,
    exprs: PrimaryMap<ExprId, Expr<'gc>>,
    functions: PrimaryMap<FunctionId, Function<'gc>>,
    free_vars_pool: ListPool<FreeVar>,
    bound_vars_pool: ListPool<BoundVar>,
    function_links_pool: ListPool<FunctionLink>,
    _marker: PhantomData<&'gc ()>,
}

impl<'gc> Default for Graph<'gc> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'gc> Graph<'gc> {
    pub fn new() -> Self {
        Self {
            term_links: PrimaryMap::new(),
            expr_links: PrimaryMap::new(),
            parent_links: PrimaryMap::new(),
            function_links: PrimaryMap::new(),
            free_occ_links: PrimaryMap::new(),
            points: PrimaryMap::new(),
            bound_vars: PrimaryMap::new(),
            terms: PrimaryMap::new(),
            exprs: PrimaryMap::new(),
            functions: PrimaryMap::new(),
            free_vars_pool: ListPool::new(),
            bound_vars_pool: ListPool::new(),
            function_links_pool: ListPool::new(),
            _marker: PhantomData,
        }
    }

    pub fn stats(&self) -> GraphStats {
        GraphStats {
            term_links: self.term_links.len(),
            expr_links: self.expr_links.len(),
            parent_links: self.parent_links.len(),
            function_links: self.function_links.len(),
            free_occurrences: self.free_occ_links.len(),
            points: self.points.len(),
            bound_vars: self.bound_vars.len(),
            terms: self.terms.len(),
            exprs: self.exprs.len(),
            functions: self.functions.len(),
        }
    }

    pub fn new_term_link(&mut self, target: Option<TermId>) -> TermLink {
        self.term_links.push(target)
    }

    pub fn read_term_link(&self, link: TermLink) -> Option<TermId> {
        self.term_links[link]
    }

    pub fn set_term_link(&mut self, link: TermLink, target: TermId) {
        verbose_log!("gcps graph: set {link} -> {target}");
        self.term_links[link] = Some(target);
    }

    pub fn clear_term_link(&mut self, link: TermLink) {
        verbose_log!("gcps graph: clear {link}");
        self.term_links[link] = None;
    }

    pub fn new_expr_link(&mut self, target: Option<ExprId>) -> ExprLink {
        self.expr_links.push(target)
    }

    pub fn read_expr_link(&self, link: ExprLink) -> Option<ExprId> {
        self.expr_links[link]
    }

    pub fn set_expr_link(&mut self, link: ExprLink, target: ExprId) {
        verbose_log!("gcps graph: set {link} -> {target}");
        self.expr_links[link] = Some(target);
    }

    pub fn clear_expr_link(&mut self, link: ExprLink) {
        verbose_log!("gcps graph: clear {link}");
        self.expr_links[link] = None;
    }

    pub fn new_parent_link(&mut self, target: Option<Parent>) -> ParentLink {
        self.parent_links.push(target)
    }

    pub fn read_parent_link(&self, link: ParentLink) -> Option<Parent> {
        self.parent_links[link]
    }

    pub fn set_parent_link(&mut self, link: ParentLink, target: Parent) {
        verbose_log!("gcps graph: set {link} -> {target:?}");
        self.parent_links[link] = Some(target);
    }

    pub fn clear_parent_link(&mut self, link: ParentLink) {
        verbose_log!("gcps graph: clear {link}");
        self.parent_links[link] = None;
    }

    pub fn new_function_link(&mut self, target: Option<FunctionId>) -> FunctionLink {
        self.function_links.push(target)
    }

    pub fn read_function_link(&self, link: FunctionLink) -> Option<FunctionId> {
        self.function_links[link]
    }

    pub fn set_function_link(&mut self, link: FunctionLink, target: FunctionId) {
        verbose_log!("gcps graph: set {link} -> {target}");
        self.function_links[link] = Some(target);
    }

    pub fn clear_function_link(&mut self, link: FunctionLink) {
        verbose_log!("gcps graph: clear {link}");
        self.function_links[link] = None;
    }

    pub fn new_bound_var(&mut self, var: LVarRef<'gc>) -> BoundVar {
        self.bound_vars.push(BoundVarData {
            var,
            occ: PackedOption::default(),
        })
    }

    pub fn set_bound_var_occ(&mut self, var: BoundVar, occ: Option<FreeOcc>) {
        verbose_log!("gcps graph: set bound occurrence {var} -> {occ:?}");
        self.bound_vars[var].occ = occ.into();
    }

    pub fn bound_var_occ(&self, var: BoundVar) -> Option<FreeOcc> {
        self.bound_vars[var].occ.expand()
    }

    pub fn binder_is_dead(&self, var: BoundVar) -> bool {
        self.bound_var_occ(var).is_none()
    }

    pub fn new_point(&mut self, descriptor: BoundVar) -> PointId {
        let point = self.points.next_key();
        self.points.push(UfNode {
            parent: point,
            descriptor,
        });
        point
    }

    /// Return the representative point, compressing the path on the way back.
    pub fn find_root(&mut self, point: PointId) -> PointId {
        let parent = self.points[point].parent;
        if parent == point {
            point
        } else {
            let root = self.find_root(parent);
            self.points[point].parent = root;
            root
        }
    }

    fn find_root_const(&self, point: PointId) -> PointId {
        let mut cursor = point;
        loop {
            let parent = self.points[cursor].parent;
            if parent == cursor {
                return cursor;
            }
            cursor = parent;
        }
    }

    /// Merge `point`'s set into `representative`'s set.
    ///
    /// This is directional: after the call, [`point_descriptor`](Self::point_descriptor)
    /// reads through `representative`. That direction is used by substitutions
    /// to make all occurrences of the removed binder name the replacement binder.
    pub fn union_to(&mut self, point: PointId, representative: PointId) {
        let mine = self.find_root(point);
        let representative = self.find_root(representative);

        if mine != representative {
            verbose_log!("gcps graph: union {mine} -> {representative}");
            self.points[mine].parent = representative;
        }
    }

    /// Descriptor stored on the current representative root.
    pub fn point_descriptor(&mut self, point: PointId) -> BoundVar {
        let root = self.find_root(point);
        self.points[root].descriptor
    }

    pub fn point_descriptor_const(&self, point: PointId) -> BoundVar {
        let root = self.find_root_const(point);
        self.points[root].descriptor
    }

    pub fn set_point_descriptor(&mut self, point: PointId, descriptor: BoundVar) {
        let root = self.find_root(point);
        verbose_log!("gcps graph: set descriptor {root} -> {descriptor}");
        self.points[root].descriptor = descriptor;
    }

    pub fn points_equal(&mut self, left: PointId, right: PointId) -> bool {
        self.find_root(left) == self.find_root(right)
    }

    pub fn free_binder(&self, occ: FreeOcc) -> BoundVar {
        self.point_descriptor_const(self.free_occ_links[occ].point)
    }

    pub fn free_owner(&self, occ: FreeOcc) -> TermLink {
        self.free_occ_links[occ].owner
    }

    pub fn set_free_owner(&mut self, occ: FreeOcc, owner: TermLink) {
        verbose_log!("gcps graph: set free owner {occ} -> {owner}");
        self.free_occ_links[occ].owner = owner;
    }

    pub fn new_free_occ(&mut self, point: PointId, owner: TermLink) -> FreeOcc {
        let occ = self.free_occ_links.next_key();
        verbose_log!("gcps graph: new occurrence {occ} point={point} owner={owner}");
        self.free_occ_links.push(FreeOccPayload {
            point,
            owner,
            prev: occ,
            next: occ,
        });
        occ
    }

    pub fn free_occ_singleton(&mut self, point: PointId, owner: TermLink) -> FreeOcc {
        self.new_free_occ(point, owner)
    }

    pub fn free_occ_next(&self, occ: FreeOcc) -> FreeOcc {
        self.free_occ_links[occ].next
    }

    pub fn free_occ_prev(&self, occ: FreeOcc) -> FreeOcc {
        self.free_occ_links[occ].prev
    }

    pub fn set_free_occ_next(&mut self, occ: FreeOcc, next: FreeOcc) {
        self.free_occ_links[occ].next = next;
    }

    pub fn set_free_occ_prev(&mut self, occ: FreeOcc, prev: FreeOcc) {
        self.free_occ_links[occ].prev = prev;
    }

    pub fn free_occ_is_singleton(&self, occ: FreeOcc) -> bool {
        self.free_occ_next(occ) == occ && self.free_occ_prev(occ) == occ
    }

    pub fn free_occ_append(&mut self, occ: FreeOcc, point: PointId, owner: TermLink) -> FreeOcc {
        let next = self.free_occ_next(occ);
        let inserted = self.new_free_occ(point, owner);
        verbose_log!("gcps graph: append occurrence {inserted} after {occ}");
        self.set_free_occ_prev(inserted, occ);
        self.set_free_occ_next(inserted, next);
        self.set_free_occ_next(occ, inserted);
        self.set_free_occ_prev(next, inserted);
        inserted
    }

    pub fn free_occ_merge(&mut self, left: FreeOcc, right: FreeOcc) {
        verbose_log!("gcps graph: merge occurrence rings {left} and {right}");
        let left_prev = self.free_occ_prev(left);
        let right_prev = self.free_occ_prev(right);
        self.set_free_occ_next(left_prev, right);
        self.set_free_occ_next(right_prev, left);
        self.set_free_occ_prev(left, right_prev);
        self.set_free_occ_prev(right, left_prev);
    }

    pub fn free_occ_remove(&mut self, occ: FreeOcc) {
        verbose_log!("gcps graph: remove occurrence {occ}");
        let prev = self.free_occ_prev(occ);
        let next = self.free_occ_next(occ);
        self.set_free_occ_next(prev, next);
        self.set_free_occ_prev(next, prev);
        self.set_free_occ_prev(occ, occ);
        self.set_free_occ_next(occ, occ);
    }

    pub fn free_occ_to_vec(&self, occ: FreeOcc) -> Vec<FreeOcc> {
        let mut out = vec![occ];
        let mut cursor = self.free_occ_next(occ);
        while cursor != occ {
            out.push(cursor);
            cursor = self.free_occ_next(cursor);
        }

        out
    }

    pub fn for_each_occurrence(&self, var: BoundVar, mut f: impl FnMut(FreeOcc)) {
        let Some(occ) = self.bound_var_occ(var) else {
            return;
        };

        f(occ);
        let mut cursor = self.free_occ_next(occ);
        while cursor != occ {
            f(cursor);
            cursor = self.free_occ_next(cursor);
        }
    }

    pub fn collect_occurrences(&self, var: BoundVar) -> Vec<FreeOcc> {
        let mut out = Vec::new();
        self.for_each_occurrence(var, |occ| out.push(occ));
        out
    }

    pub fn free_occ_is_singleton_verified(&self, occ: FreeOcc) -> bool {
        let binder = self.free_binder(occ);
        self.bound_var_occ(binder) == Some(occ) && self.free_occ_is_singleton(occ)
    }

    pub fn binder_is_singleton_or_dead(&self, var: BoundVar) -> bool {
        self.bound_var_occ(var)
            .map(|occ| self.free_occ_is_singleton(occ))
            .unwrap_or(true)
    }

    pub fn new_free_occ_for_binder(&mut self, binder: BoundVar, owner: TermLink) -> FreeOcc {
        let point = self.new_point(binder);
        match self.bound_var_occ(binder) {
            None => {
                let occ = self.free_occ_singleton(point, owner);
                self.set_bound_var_occ(binder, Some(occ));
                occ
            }
            Some(existing) => {
                self.union_to(point, self.free_occ_links[existing].point);
                self.free_occ_append(existing, point, owner)
            }
        }
    }

    pub fn merge_free_lists(&mut self, replacement: BoundVar, removed: BoundVar) {
        let replacement_occ = self.bound_var_occ(replacement);
        let removed_occ = self.bound_var_occ(removed);
        match (replacement_occ, removed_occ) {
            (None, None) => {}
            (None, Some(occ)) => self.set_bound_var_occ(replacement, Some(occ)),
            (Some(occ), None) => self.set_bound_var_occ(removed, Some(occ)),
            (Some(replacement_occ), Some(removed_occ)) => {
                self.free_occ_merge(replacement_occ, removed_occ);
            }
        }
    }

    pub fn subst_binders(&mut self, replacement: BoundVar, removed: BoundVar) {
        verbose_log!("gcps graph: substitute binder {replacement} for {removed}");
        self.merge_free_lists(replacement, removed);
        self.set_bound_var_occ(removed, None);
    }

    pub fn subst_var_for_bound(&mut self, replacement: FreeOcc, removed: BoundVar) {
        verbose_log!("gcps graph: substitute occurrence {replacement} for bound {removed}");
        if let Some(removed_occ) = self.bound_var_occ(removed) {
            self.subst_var_for_var(replacement, removed_occ);
        }
    }

    pub fn subst_var_for_var(&mut self, replacement: FreeOcc, removed: FreeOcc) {
        verbose_log!("gcps graph: substitute occurrence {replacement} for occurrence {removed}");
        let replacement_point = self.free_occ_links[replacement].point;
        let removed_point = self.free_occ_links[removed].point;
        self.subst_var_for_var_points(replacement_point, removed_point);
    }

    pub fn subst_var_for_var_points(&mut self, replacement: PointId, removed: PointId) {
        verbose_log!("gcps graph: substitute point {replacement} for point {removed}");
        if self.points_equal(replacement, removed) {
            return;
        }

        let replacement_binder = self.point_descriptor(replacement);
        let removed_binder = self.point_descriptor(removed);
        self.subst_binders(replacement_binder, removed_binder);
        self.union_to(removed, replacement);
    }

    pub fn subst_var_for_binders(&mut self, replacement: BoundVar, removed: BoundVar) {
        verbose_log!("gcps graph: substitute binder {replacement} for binder {removed}");
        match (self.bound_var_occ(replacement), self.bound_var_occ(removed)) {
            (Some(replacement_occ), Some(removed_occ)) => {
                self.subst_var_for_var(replacement_occ, removed_occ);
            }
            (Some(_), None) => self.subst_binders(replacement, removed),
            (None, Some(removed_occ)) => {
                let removed_point = self.free_occ_links[removed_occ].point;
                self.subst_binders(replacement, removed);
                self.set_point_descriptor(removed_point, replacement);
            }
            (None, None) => {}
        }
    }

    pub fn new_free_vars(&mut self, vars: impl IntoIterator<Item = FreeVar>) -> FreeVars {
        EntityList::from_iter(vars, &mut self.free_vars_pool)
    }

    pub fn free_vars_slice(&self, vars: &FreeVars) -> &[FreeVar] {
        vars.as_slice(&self.free_vars_pool)
    }

    pub fn free_vars_push(&mut self, vars: &mut FreeVars, var: FreeVar) -> usize {
        vars.push(var, &mut self.free_vars_pool)
    }

    pub fn clear_free_vars(&mut self, vars: &mut FreeVars) {
        vars.clear(&mut self.free_vars_pool);
    }

    pub fn new_bound_vars(&mut self, vars: impl IntoIterator<Item = BoundVar>) -> BoundVars {
        EntityList::from_iter(vars, &mut self.bound_vars_pool)
    }

    pub fn bound_vars_slice(&self, vars: &BoundVars) -> &[BoundVar] {
        vars.as_slice(&self.bound_vars_pool)
    }

    pub fn bound_vars_push(&mut self, vars: &mut BoundVars, var: BoundVar) -> usize {
        vars.push(var, &mut self.bound_vars_pool)
    }

    pub fn new_function_links(
        &mut self,
        links: impl IntoIterator<Item = FunctionLink>,
    ) -> FunctionLinks {
        EntityList::from_iter(links, &mut self.function_links_pool)
    }

    pub fn function_links_slice(&self, links: &FunctionLinks) -> &[FunctionLink] {
        links.as_slice(&self.function_links_pool)
    }

    pub fn function_links_push(&mut self, links: &mut FunctionLinks, link: FunctionLink) -> usize {
        links.push(link, &mut self.function_links_pool)
    }

    pub fn free_vars_of_expr(&self, expr: ExprId) -> Vec<FreeVar> {
        let mut out = Vec::new();
        self.push_free_vars_of_expr(expr, &mut out);
        out
    }

    pub fn push_free_vars_of_expr(&self, expr: ExprId, out: &mut Vec<FreeVar>) {
        self.for_each_free_var_of_expr(expr, |var| out.push(var));
    }

    pub fn for_each_free_var_of_expr(&self, expr: ExprId, mut f: impl FnMut(FreeVar)) {
        match self.exprs[expr].kind {
            ExprKind::Literal(_) => {}
            ExprKind::PrimCall(_, vars) => {
                for var in self.free_vars_slice(&vars) {
                    f(*var);
                }
            }
        }
    }

    pub fn direct_free_vars_of_term(&self, term: TermId) -> Vec<FreeVar> {
        let mut out = Vec::new();
        self.push_direct_free_vars_of_term(term, &mut out);
        out
    }

    pub fn push_direct_free_vars_of_term(&self, term: TermId, out: &mut Vec<FreeVar>) {
        self.for_each_direct_free_var_of_term(term, |var| out.push(var));
    }

    pub fn for_each_direct_free_var_of_term(&self, term: TermId, mut f: impl FnMut(FreeVar)) {
        match self.terms[term].kind {
            TermKind::LetVal(..) | TermKind::Fix(..) | TermKind::Letk(..) => {}
            TermKind::If(var, ..) => f(var),
            TermKind::Continue(cont, vars) => {
                f(cont);
                for var in self.free_vars_slice(&vars) {
                    f(*var);
                }
            }
            TermKind::App(func, vars, cont) => {
                f(func);
                for var in self.free_vars_slice(&vars) {
                    f(*var);
                }
                f(cont);
            }
            TermKind::Raise(_, vars) => {
                for var in self.free_vars_slice(&vars) {
                    f(*var);
                }
            }
        }
    }

    pub fn subexprs_of(&self, term: TermId) -> Vec<Subexpr> {
        let mut out = Vec::new();
        self.for_each_subexpr(term, |subexpr| out.push(subexpr));
        out
    }

    pub fn for_each_subexpr(&self, term: TermId, mut f: impl FnMut(Subexpr)) {
        match self.terms[term].kind {
            TermKind::LetVal((_, expr), _) => f(expr),
            TermKind::Fix(..)
            | TermKind::Letk(..)
            | TermKind::If(..)
            | TermKind::Continue(..)
            | TermKind::App(..)
            | TermKind::Raise(..) => {}
        }
    }

    pub fn subterms_of(&self, term: TermId) -> Vec<Subterm> {
        let mut out = Vec::new();
        self.for_each_subterm(term, |subterm| out.push(subterm));
        out
    }

    pub fn for_each_subterm(&self, term: TermId, mut f: impl FnMut(Subterm)) {
        match self.terms[term].kind {
            TermKind::LetVal(_, body) => f(body),
            TermKind::Fix(_, body) | TermKind::Letk(_, body) => f(body),
            TermKind::If(_, then_branch, else_branch) => {
                f(then_branch);
                f(else_branch);
            }
            TermKind::Continue(..) | TermKind::App(..) | TermKind::Raise(..) => {}
        }
    }

    fn term_link_points_to(&self, link: Subterm, term: TermId) -> bool {
        self.read_term_link(link) == Some(term)
    }

    pub fn get_active_link_for(&self, term: TermId) -> ActiveLinkStatus {
        let Some(parent) = self.read_parent_link(self.terms[term].link) else {
            verbose_log!("gcps graph: active link for {term}: no parent");
            return ActiveLinkStatus::TermIsDead;
        };

        match parent {
            Parent::Func(function) => {
                let body = self.functions[function].body;
                if self.term_link_points_to(body, term) {
                    verbose_log!("gcps graph: active link for {term}: function body {body}");
                    ActiveLinkStatus::ActiveSubterm(body)
                } else {
                    verbose_log!("gcps graph: active link for {term}: dead function body");
                    ActiveLinkStatus::TermIsDead
                }
            }
            Parent::Term(parent) => {
                let mut active = None;
                self.for_each_subterm(parent, |link| {
                    if self.term_link_points_to(link, term) {
                        assert!(
                            active.replace(link).is_none(),
                            "multiple active subterm links point at {term}"
                        );
                    }
                });

                match active {
                    None => {
                        verbose_log!("gcps graph: active link for {term}: dead under {parent}");
                        ActiveLinkStatus::TermIsDead
                    }
                    Some(link) => {
                        verbose_log!("gcps graph: active link for {term}: {link}");
                        ActiveLinkStatus::ActiveSubterm(link)
                    }
                }
            }
        }
    }

    pub fn replace_active_subterm(
        &mut self,
        stale_term: TermId,
        replacement: TermId,
    ) -> ActiveLinkStatus {
        let status = self.get_active_link_for(stale_term);
        if let ActiveLinkStatus::ActiveSubterm(link) = status {
            self.set_term_link(link, replacement);
        }
        status
    }

    pub fn backpatch_subterms(&mut self, parent: TermId, subterms: &[Subterm]) {
        for subterm in subterms {
            if let Some(term) = self.read_term_link(*subterm) {
                let uplink = self.terms[term].link;
                self.set_parent_link(uplink, Parent::Term(parent));
            }
        }
    }

    pub fn backpatch_subexprs(
        &mut self,
        parent: TermId,
        parent_owner: TermLink,
        subexprs: &[Subexpr],
    ) {
        for subexpr in subexprs {
            if let Some(expr) = self.read_expr_link(*subexpr) {
                for var in self.free_vars_of_expr(expr) {
                    self.set_free_owner(var, parent_owner);
                }

                let uplink = self.exprs[expr].link;
                self.set_parent_link(uplink, Parent::Term(parent));
            }
        }
    }

    pub fn backpatch_function(&mut self, function: FunctionId) {
        if let Some(term) = self.read_term_link(self.functions[function].body) {
            let uplink = self.terms[term].link;
            self.set_parent_link(uplink, Parent::Func(function));
        }
    }

    pub fn new_term(&mut self, link: Uplink, kind: TermKind, source: Value<'gc>) -> TermId {
        self.terms.push(Term { link, kind, source })
    }

    pub fn new_expr(&mut self, link: Uplink, kind: ExprKind<'gc>, source: Value<'gc>) -> ExprId {
        self.exprs.push(Expr { link, kind, source })
    }

    pub fn new_function(&mut self, function: Function<'gc>) -> FunctionId {
        self.functions.push(function)
    }

    pub fn term_links(&self) -> &PrimaryMap<TermLink, Option<TermId>> {
        &self.term_links
    }

    pub fn expr_links(&self) -> &PrimaryMap<ExprLink, Option<ExprId>> {
        &self.expr_links
    }

    pub fn parent_links(&self) -> &PrimaryMap<ParentLink, Option<Parent>> {
        &self.parent_links
    }

    pub fn function_links(&self) -> &PrimaryMap<FunctionLink, Option<FunctionId>> {
        &self.function_links
    }

    pub fn display_term(&self, term: TermId) -> PrettyTerm<'_, 'gc> {
        PrettyTerm { graph: self, term }
    }

    pub fn display_expr(&self, expr: ExprId) -> PrettyExpr<'_, 'gc> {
        PrettyExpr { graph: self, expr }
    }

    pub fn display_function(&self, function: FunctionId) -> PrettyFunction<'_, 'gc> {
        PrettyFunction {
            graph: self,
            function,
        }
    }

    pub fn pretty_term(&self, term: TermId) -> String {
        self.display_term(term).to_string()
    }

    pub fn pretty_expr(&self, expr: ExprId) -> String {
        self.display_expr(expr).to_string()
    }

    pub fn pretty_function(&self, function: FunctionId) -> String {
        self.display_function(function).to_string()
    }

    fn fmt_bound_var(&self, f: &mut fmt::Formatter<'_>, var: BoundVar) -> fmt::Result {
        match self.bound_vars.get(var) {
            Some(data) => write!(f, "{var}:{}", data.var.name),
            None => write!(f, "{var}:<missing>"),
        }
    }

    fn fmt_free_var(&self, f: &mut fmt::Formatter<'_>, var: FreeVar) -> fmt::Result {
        let Some(occ) = self.free_occ_links.get(var) else {
            return write!(f, "{var}/<missing>");
        };

        write!(f, "{var}/")?;
        self.fmt_bound_var(f, self.point_descriptor_const(occ.point))
    }

    fn fmt_free_vars(&self, f: &mut fmt::Formatter<'_>, vars: &FreeVars) -> fmt::Result {
        write!(f, "[")?;
        for (index, var) in self.free_vars_slice(vars).iter().enumerate() {
            if index > 0 {
                write!(f, ", ")?;
            }
            self.fmt_free_var(f, *var)?;
        }
        write!(f, "]")
    }

    fn fmt_bound_vars(&self, f: &mut fmt::Formatter<'_>, vars: &BoundVars) -> fmt::Result {
        write!(f, "[")?;
        for (index, var) in self.bound_vars_slice(vars).iter().enumerate() {
            if index > 0 {
                write!(f, ", ")?;
            }
            self.fmt_bound_var(f, *var)?;
        }
        write!(f, "]")
    }

    fn fmt_term_link(
        &self,
        f: &mut fmt::Formatter<'_>,
        link: Subterm,
        depth: usize,
    ) -> fmt::Result {
        match self.read_term_link(link) {
            Some(term) => self.fmt_term(f, term, depth + 1),
            None => write!(f, "{link}:<dead>"),
        }
    }

    fn fmt_expr_link(
        &self,
        f: &mut fmt::Formatter<'_>,
        link: Subexpr,
        depth: usize,
    ) -> fmt::Result {
        match self.read_expr_link(link) {
            Some(expr) => self.fmt_expr(f, expr, depth + 1),
            None => write!(f, "{link}:<dead>"),
        }
    }

    fn fmt_function_link(
        &self,
        f: &mut fmt::Formatter<'_>,
        link: FunctionLink,
        depth: usize,
    ) -> fmt::Result {
        match self.read_function_link(link) {
            Some(function) => self.fmt_function(f, function, depth + 1),
            None => write!(f, "{link}:<dead>"),
        }
    }

    fn fmt_function_links(
        &self,
        f: &mut fmt::Formatter<'_>,
        links: &FunctionLinks,
        depth: usize,
    ) -> fmt::Result {
        write!(f, "[")?;
        for (index, link) in self.function_links_slice(links).iter().enumerate() {
            if index > 0 {
                write!(f, ", ")?;
            }
            self.fmt_function_link(f, *link, depth + 1)?;
        }
        write!(f, "]")
    }

    fn fmt_term(&self, f: &mut fmt::Formatter<'_>, term: TermId, depth: usize) -> fmt::Result {
        if depth > 32 {
            return write!(f, "{term}:...");
        }

        write!(f, "{term} ")?;
        match self.terms[term].kind {
            TermKind::LetVal((var, expr), body) => {
                write!(f, "(let-val ")?;
                self.fmt_bound_var(f, var)?;
                write!(f, " = ")?;
                self.fmt_expr_link(f, expr, depth + 1)?;
                write!(f, " in ")?;
                self.fmt_term_link(f, body, depth + 1)?;
                write!(f, ")")
            }
            TermKind::Fix(functions, body) => {
                write!(f, "(fix ")?;
                self.fmt_function_links(f, &functions, depth + 1)?;
                write!(f, " in ")?;
                self.fmt_term_link(f, body, depth + 1)?;
                write!(f, ")")
            }
            TermKind::Letk(functions, body) => {
                write!(f, "(letk ")?;
                self.fmt_function_links(f, &functions, depth + 1)?;
                write!(f, " in ")?;
                self.fmt_term_link(f, body, depth + 1)?;
                write!(f, ")")
            }
            TermKind::If(var, then_branch, else_branch) => {
                write!(f, "(if ")?;
                self.fmt_free_var(f, var)?;
                write!(f, " ")?;
                self.fmt_term_link(f, then_branch, depth + 1)?;
                write!(f, " ")?;
                self.fmt_term_link(f, else_branch, depth + 1)?;
                write!(f, ")")
            }
            TermKind::Continue(cont, vars) => {
                write!(f, "(continue ")?;
                self.fmt_free_var(f, cont)?;
                write!(f, " ")?;
                self.fmt_free_vars(f, &vars)?;
                write!(f, ")")
            }
            TermKind::App(func, vars, cont) => {
                write!(f, "(app ")?;
                self.fmt_free_var(f, func)?;
                write!(f, " ")?;
                self.fmt_free_vars(f, &vars)?;
                write!(f, " ")?;
                self.fmt_free_var(f, cont)?;
                write!(f, ")")
            }
            TermKind::Raise(kind, vars) => {
                write!(f, "(raise {kind:?} ")?;
                self.fmt_free_vars(f, &vars)?;
                write!(f, ")")
            }
        }
    }

    fn fmt_expr(&self, f: &mut fmt::Formatter<'_>, expr: ExprId, _depth: usize) -> fmt::Result {
        write!(f, "{expr} ")?;
        match self.exprs[expr].kind {
            ExprKind::Literal(value) => write!(f, "(literal {value})"),
            ExprKind::PrimCall(prim, vars) => {
                write!(f, "(prim-call {prim} ")?;
                self.fmt_free_vars(f, &vars)?;
                write!(f, ")")
            }
        }
    }

    fn fmt_function(
        &self,
        f: &mut fmt::Formatter<'_>,
        function: FunctionId,
        depth: usize,
    ) -> fmt::Result {
        if depth > 32 {
            return write!(f, "{function}:...");
        }

        let function_data = self.functions[function];
        write!(f, "{function} (fn ")?;
        self.fmt_bound_var(f, function_data.var)?;
        write!(f, " ")?;
        self.fmt_bound_vars(f, &function_data.vars)?;
        if let Some(variadic) = function_data.variadic {
            write!(f, " & ")?;
            self.fmt_bound_var(f, variadic)?;
        }
        if let Some(cont) = function_data.cont {
            write!(f, " -> ")?;
            self.fmt_bound_var(f, cont)?;
        }
        write!(f, " body ")?;
        self.fmt_term_link(f, function_data.body, depth + 1)?;
        write!(f, ")")
    }
}

pub struct PrettyTerm<'a, 'gc> {
    graph: &'a Graph<'gc>,
    term: TermId,
}

impl fmt::Display for PrettyTerm<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.graph.fmt_term(f, self.term, 0)
    }
}

pub struct PrettyExpr<'a, 'gc> {
    graph: &'a Graph<'gc>,
    expr: ExprId,
}

impl fmt::Display for PrettyExpr<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.graph.fmt_expr(f, self.expr, 0)
    }
}

pub struct PrettyFunction<'a, 'gc> {
    graph: &'a Graph<'gc>,
    function: FunctionId,
}

impl fmt::Display for PrettyFunction<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.graph.fmt_function(f, self.function, 0)
    }
}

impl<'gc> Index<TermLink> for Graph<'gc> {
    type Output = Option<TermId>;

    fn index(&self, index: TermLink) -> &Self::Output {
        &self.term_links[index]
    }
}

impl<'gc> IndexMut<TermLink> for Graph<'gc> {
    fn index_mut(&mut self, index: TermLink) -> &mut Self::Output {
        &mut self.term_links[index]
    }
}

impl<'gc> Index<ExprLink> for Graph<'gc> {
    type Output = Option<ExprId>;

    fn index(&self, index: ExprLink) -> &Self::Output {
        &self.expr_links[index]
    }
}

impl<'gc> IndexMut<ExprLink> for Graph<'gc> {
    fn index_mut(&mut self, index: ExprLink) -> &mut Self::Output {
        &mut self.expr_links[index]
    }
}

impl<'gc> Index<ParentLink> for Graph<'gc> {
    type Output = Option<Parent>;

    fn index(&self, index: ParentLink) -> &Self::Output {
        &self.parent_links[index]
    }
}

impl<'gc> IndexMut<ParentLink> for Graph<'gc> {
    fn index_mut(&mut self, index: ParentLink) -> &mut Self::Output {
        &mut self.parent_links[index]
    }
}

impl<'gc> Index<FunctionLink> for Graph<'gc> {
    type Output = Option<FunctionId>;

    fn index(&self, index: FunctionLink) -> &Self::Output {
        &self.function_links[index]
    }
}

impl<'gc> IndexMut<FunctionLink> for Graph<'gc> {
    fn index_mut(&mut self, index: FunctionLink) -> &mut Self::Output {
        &mut self.function_links[index]
    }
}

impl<'gc> Index<FreeOcc> for Graph<'gc> {
    type Output = FreeOccPayload;

    fn index(&self, index: FreeOcc) -> &Self::Output {
        &self.free_occ_links[index]
    }
}

impl<'gc> IndexMut<FreeOcc> for Graph<'gc> {
    fn index_mut(&mut self, index: FreeOcc) -> &mut Self::Output {
        &mut self.free_occ_links[index]
    }
}

impl<'gc> Index<PointId> for Graph<'gc> {
    type Output = UfNode;

    fn index(&self, index: PointId) -> &Self::Output {
        &self.points[index]
    }
}

impl<'gc> IndexMut<PointId> for Graph<'gc> {
    fn index_mut(&mut self, index: PointId) -> &mut Self::Output {
        &mut self.points[index]
    }
}

impl<'gc> Index<BoundVar> for Graph<'gc> {
    type Output = BoundVarData<'gc>;

    fn index(&self, index: BoundVar) -> &Self::Output {
        &self.bound_vars[index]
    }
}

impl<'gc> IndexMut<BoundVar> for Graph<'gc> {
    fn index_mut(&mut self, index: BoundVar) -> &mut Self::Output {
        &mut self.bound_vars[index]
    }
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

impl<'gc> Index<ExprId> for Graph<'gc> {
    type Output = Expr<'gc>;

    fn index(&self, index: ExprId) -> &Self::Output {
        &self.exprs[index]
    }
}

impl<'gc> IndexMut<ExprId> for Graph<'gc> {
    fn index_mut(&mut self, index: ExprId) -> &mut Self::Output {
        &mut self.exprs[index]
    }
}

impl<'gc> Index<FunctionId> for Graph<'gc> {
    type Output = Function<'gc>;

    fn index(&self, index: FunctionId) -> &Self::Output {
        &self.functions[index]
    }
}

impl<'gc> IndexMut<FunctionId> for Graph<'gc> {
    fn index_mut(&mut self, index: FunctionId) -> &mut Self::Output {
        &mut self.functions[index]
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{expander::core::fresh_lvar, runtime::Scheme};

    fn with_graph<R>(
        f: impl for<'gc> FnOnce(&mut Graph<'gc>, LVarRef<'gc>, LVarRef<'gc>) -> R,
    ) -> R {
        Scheme::new_uninit().enter(|ctx| {
            let left = fresh_lvar(ctx, ctx.intern("left"));
            let right = fresh_lvar(ctx, ctx.intern("right"));
            let mut graph = Graph::new();
            f(&mut graph, left, right)
        })
    }

    #[test]
    fn concrete_entities_have_reserved_values() {
        let term = TermId::new(7);
        assert_eq!(term.index(), 7);
        assert_eq!(term.as_u32(), 7);
        assert_eq!(format!("{term:?}"), "term7");
        assert!(TermId::reserved_value().is_reserved_value());
    }

    #[test]
    fn term_expr_and_kinds_are_copy() {
        fn assert_copy<T: Copy>() {}

        assert_copy::<Term<'static>>();
        assert_copy::<TermKind>();
        assert_copy::<Expr<'static>>();
        assert_copy::<ExprKind<'static>>();
    }

    #[test]
    fn graph_indexes_links_and_nodes() {
        let mut graph = Graph::new();
        let link = graph.new_term_link(None);

        assert_eq!(graph[link], None);

        let parent = graph.new_parent_link(None);
        let term = graph.new_term(
            parent,
            TermKind::Continue(FreeOcc::new(0), FreeVars::new()),
            Value::from_raw(0),
        );
        graph.set_term_link(link, term);

        assert_eq!(graph[link], Some(term));
        assert_eq!(graph.read_term_link(link), Some(term));

        graph[link] = None;
        assert_eq!(graph.read_term_link(link), None);
    }

    #[test]
    fn occurrence_ring_operations_keep_o1_links() {
        let mut graph = Graph::new();
        let bound = BoundVar::new(0);
        let point = graph.new_point(bound);
        let owner = graph.new_term_link(None);

        let first = graph.free_occ_singleton(point, owner);
        assert!(graph.free_occ_is_singleton(first));

        let second = graph.free_occ_append(first, point, owner);
        let third = graph.free_occ_append(second, point, owner);
        assert_eq!(graph.free_occ_to_vec(first), vec![first, second, third]);

        graph.free_occ_remove(second);
        assert_eq!(graph.free_occ_to_vec(first), vec![first, third]);
        assert!(graph.free_occ_is_singleton(second));

        let other = graph.free_occ_singleton(point, owner);
        graph.free_occ_merge(first, other);
        assert_eq!(graph.free_occ_to_vec(first), vec![first, third, other]);
    }

    #[test]
    fn union_find_is_directional_and_compresses_paths() {
        let mut graph = Graph::new();
        let left_desc = BoundVar::new(1);
        let right_desc = BoundVar::new(2);
        let left = graph.new_point(left_desc);
        let right = graph.new_point(right_desc);

        graph.union_to(left, right);

        assert!(graph.points_equal(left, right));
        assert_eq!(graph.point_descriptor(left), right_desc);
        assert_eq!(graph[left].parent, right);

        graph.set_point_descriptor(left, left_desc);
        assert_eq!(graph.point_descriptor(right), left_desc);
    }

    #[test]
    fn substitution_merges_occurrences_and_kills_removed_binder() {
        with_graph(|graph, left_lvar, right_lvar| {
            let left = graph.new_bound_var(left_lvar);
            let right = graph.new_bound_var(right_lvar);
            let left_owner = graph.new_term_link(None);
            let right_owner = graph.new_term_link(None);
            let left_occ = graph.new_free_occ_for_binder(left, left_owner);
            let left_occ_2 = graph.new_free_occ_for_binder(left, left_owner);
            let right_occ = graph.new_free_occ_for_binder(right, right_owner);

            assert_eq!(graph.collect_occurrences(left).len(), 2);
            assert_eq!(graph.collect_occurrences(right), vec![right_occ]);

            graph.subst_var_for_var(left_occ, right_occ);

            assert_eq!(graph.bound_var_occ(right), None);
            assert_eq!(graph.free_binder(right_occ), left);
            assert_eq!(
                graph.collect_occurrences(left),
                vec![left_occ, left_occ_2, right_occ]
            );
        });
    }

    #[test]
    fn active_link_uses_parent_to_find_live_subterm() {
        let mut graph = Graph::new();
        let source = Value::from_raw(0);
        let child_parent = graph.new_parent_link(None);
        let child = graph.new_term(
            child_parent,
            TermKind::Continue(FreeOcc::new(0), FreeVars::new()),
            source,
        );
        let live_child_link = graph.new_term_link(Some(child));
        let _stale_child_link = graph.new_term_link(Some(child));
        let parent_uplink = graph.new_parent_link(None);
        let expr_link = graph.new_expr_link(None);
        let parent = graph.new_term(
            parent_uplink,
            TermKind::LetVal((BoundVar::new(0), expr_link), live_child_link),
            source,
        );
        graph.set_parent_link(child_parent, Parent::Term(parent));

        assert_eq!(
            graph.get_active_link_for(child),
            ActiveLinkStatus::ActiveSubterm(live_child_link)
        );

        graph.clear_term_link(live_child_link);
        assert_eq!(
            graph.get_active_link_for(child),
            ActiveLinkStatus::TermIsDead
        );
    }

    #[test]
    fn pretty_printer_renders_graph_terms() {
        let mut graph = Graph::new();
        let source = Value::from_raw(0);
        let parent = graph.new_parent_link(None);
        let term = graph.new_term(
            parent,
            TermKind::Continue(FreeOcc::new(0), FreeVars::new()),
            source,
        );

        assert_eq!(
            graph.pretty_term(term),
            "term0 (continue free_occ0/<missing> [])"
        );
    }

    #[test]
    fn worklist_queue_is_fifo() {
        let mut queue = WorklistQueue::new();
        queue.add(TermLink::new(1));
        queue.add_list([TermLink::new(2), TermLink::new(3)]);

        assert_eq!(queue.len(), 3);
        assert_eq!(queue.get(), Some(TermLink::new(1)));
        assert_eq!(queue.get(), Some(TermLink::new(2)));
        assert_eq!(queue.get(), Some(TermLink::new(3)));
        assert_eq!(queue.get(), None);
    }

    #[test]
    fn graph_worklist_enqueues_occurrence_owners_and_skips_dead_links() {
        with_graph(|graph, left_lvar, _right_lvar| {
            let left = graph.new_bound_var(left_lvar);
            let live_owner = graph.new_term_link(None);
            let dead_owner = graph.new_term_link(None);
            let live_occ = graph.new_free_occ_for_binder(left, live_owner);
            let _dead_occ = graph.new_free_occ_for_binder(left, dead_owner);
            let source = Value::from_raw(0);
            let parent = graph.new_parent_link(None);
            let term = graph.new_term(
                parent,
                TermKind::Continue(live_occ, FreeVars::new()),
                source,
            );
            graph.set_term_link(live_owner, term);

            let mut worklist = GraphWorklist::new();
            worklist.add_occurrences(graph, [live_occ]);

            assert_eq!(
                worklist.get_next_live_subterm(graph),
                Some((live_owner, term, None))
            );
            assert_eq!(worklist.get_next_live_subterm(graph), None);
        });
    }

    #[test]
    fn entity_lists_use_graph_pools() {
        let mut graph = Graph::new();
        let vars = graph.new_free_vars([FreeOcc::new(0), FreeOcc::new(1)]);
        assert_eq!(
            graph.free_vars_slice(&vars),
            &[FreeOcc::new(0), FreeOcc::new(1)]
        );

        let mut bound_vars = graph.new_bound_vars([BoundVar::new(3)]);
        graph.bound_vars_push(&mut bound_vars, BoundVar::new(4));
        assert_eq!(
            graph.bound_vars_slice(&bound_vars),
            &[BoundVar::new(3), BoundVar::new(4)]
        );

        let mut function_links = graph.new_function_links([FunctionLink::new(5)]);
        graph.function_links_push(&mut function_links, FunctionLink::new(6));
        assert_eq!(
            graph.function_links_slice(&function_links),
            &[FunctionLink::new(5), FunctionLink::new(6)]
        );
    }
}
