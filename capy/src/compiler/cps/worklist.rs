//! Foster-style worklist queues for graphical CPS optimization.

use std::collections::{BTreeSet, VecDeque};

use super::graph::{BoundVar, FreeVar, Graph, Parent, Subterm, TermId};

macro_rules! verbose_log {
    ($($arg:tt)*) => {
        if super::graph::VERBOSE {
            eprintln!($($arg)*);
        }
    };
}

/// FIFO worklist queue.
///
/// This mirrors Foster's `WorklistQ`: add new items to the right and take
/// pending items from the left.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct WorklistQueue<T> {
    items: VecDeque<T>,
}

impl<T> Default for WorklistQueue<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> WorklistQueue<T> {
    pub fn new() -> Self {
        Self {
            items: VecDeque::new(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    pub fn len(&self) -> usize {
        self.items.len()
    }

    pub fn add(&mut self, item: T) {
        verbose_log!("gcps worklist: add item");
        self.items.push_back(item);
    }

    pub fn add_list(&mut self, items: impl IntoIterator<Item = T>) {
        let len = self.items.len();
        self.items.extend(items);
        verbose_log!("gcps worklist: add_list {} items", self.items.len() - len);
    }

    pub fn get(&mut self) -> Option<T> {
        let item = self.items.pop_front();
        verbose_log!("gcps worklist: get item present={}", item.is_some());
        item
    }

    pub fn clear(&mut self) {
        self.items.clear();
    }
}

impl<T: Clone> WorklistQueue<T> {
    pub fn to_vec(&self) -> Vec<T> {
        self.items.iter().cloned().collect()
    }
}

impl<T> FromIterator<T> for WorklistQueue<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let mut queue = Self::new();
        queue.add_list(iter);
        queue
    }
}

/// Mutable graph optimizer worklist state.
///
/// This intentionally tracks only graph-level identities. Pass-specific maps
/// for known values or aliases should live beside this queue in the pass that
/// owns those facts.
#[derive(Clone, Debug, Default)]
pub struct GraphWorklist {
    pending_subterms: WorklistQueue<Subterm>,
    pending_fn_binders: BTreeSet<BoundVar>,
    dead_bindings: WorklistQueue<BoundVar>,
}

impl GraphWorklist {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn pending_subterm_len(&self) -> usize {
        self.pending_subterms.len()
    }

    pub fn dead_binding_len(&self) -> usize {
        self.dead_bindings.len()
    }

    pub fn pending_fn_binder_len(&self) -> usize {
        self.pending_fn_binders.len()
    }

    pub fn is_empty(&self) -> bool {
        self.pending_subterms.is_empty()
            && self.pending_fn_binders.is_empty()
            && self.dead_bindings.is_empty()
    }

    pub fn add_subterm(&mut self, subterm: Subterm) {
        self.pending_subterms.add(subterm);
    }

    pub fn add_subterms(&mut self, subterms: impl IntoIterator<Item = Subterm>) {
        self.pending_subterms.add_list(subterms);
    }

    pub fn get_subterm(&mut self) -> Option<Subterm> {
        self.pending_subterms.get()
    }

    pub fn reconsider_function(&mut self, binder: BoundVar) {
        verbose_log!("gcps worklist: reconsider function {binder}");
        self.pending_fn_binders.insert(binder);
    }

    pub fn get_function_binder(&mut self) -> Option<BoundVar> {
        self.pending_fn_binders.pop_first()
    }

    pub fn add_dead_binding(&mut self, binder: BoundVar) {
        verbose_log!("gcps worklist: add dead binding {binder}");
        self.dead_bindings.add(binder);
    }

    pub fn get_dead_binding(&mut self) -> Option<BoundVar> {
        self.dead_bindings.get()
    }

    pub fn add_occurrences(&mut self, graph: &Graph<'_>, vars: impl IntoIterator<Item = FreeVar>) {
        for var in vars {
            let binder = graph.free_binder(var);
            graph.for_each_occurrence(binder, |occ| {
                self.pending_subterms.add(graph.free_owner(occ));
            });
        }
    }

    pub fn add_known_occurrences(
        &mut self,
        graph: &Graph<'_>,
        args: impl IntoIterator<Item = FreeVar>,
        formals: impl IntoIterator<Item = BoundVar>,
        is_known_expr: impl Fn(BoundVar) -> bool,
    ) {
        for (arg, formal) in args.into_iter().zip(formals) {
            let binder = graph.free_binder(arg);
            if is_known_expr(binder) {
                graph.for_each_occurrence(formal, |occ| {
                    self.pending_subterms.add(graph.free_owner(occ));
                });
            }
        }
    }

    pub fn get_next_live_subterm(
        &mut self,
        graph: &Graph<'_>,
    ) -> Option<(Subterm, TermId, Option<Parent>)> {
        loop {
            let subterm = self.get_subterm()?;
            let Some(term) = graph.read_term_link(subterm) else {
                continue;
            };
            let parent = graph.read_parent_link(graph[term].link);
            return Some((subterm, term, parent));
        }
    }
}
