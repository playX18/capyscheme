use std::collections::{HashMap, VecDeque};

use cranelift_entity::{EntityList, SecondaryMap};

use crate::cps::{Set, SingleValueSet};

use super::term::*;

impl<'gc> Graph<'gc> {}

pub struct Contification<'a, 'gc> {
    graph: &'a mut Graph<'gc>,

    tailcall_set: SecondaryMap<VarId, Vec<OccurrenceId>>,
}

impl<'a, 'gc> Contification<'a, 'gc> {
    pub fn new(graph: &'a mut Graph<'gc>) -> Self {
        let tailcall_set = SecondaryMap::with_capacity(graph.variables.len());
        Contification {
            graph,
            tailcall_set,
        }
    }

    pub fn run(&mut self) {
        log::debug!("contify: starting contification pass");
        let root_body = self.graph[self.graph.root.unwrap()].body();
        let mut worklist = VecDeque::new();
        worklist.push_back(root_body);

        let mut fixes = Vec::new();

        while let Some(term) = worklist.pop_front() {
            let TermKind::Fix(_, _) = self.graph[term].kind else {
                self.graph.enqueue_children(term, &mut worklist);
                continue;
            };

            fixes.push(term);
            self.graph.enqueue_children(term, &mut worklist);
        }

        log::debug!("contify: found {} Fix terms to process", fixes.len());

        // Process deepest fixes first.
        // If we process outer fixes first, we might contify functions that are inside inner fixes, which would make the inner fixes ineligible for contification.
        for term in fixes.into_iter().rev() {
            let TermKind::Fix(funcs, body) = self.graph[term].kind else {
                unreachable!();
            };
            let contifiables = self.contifiables(funcs, body);
            log::debug!(
                "contify: Fix {term:?} has {} contifiable groups",
                contifiables.len()
            );
            for (ns, rc) in contifiables {
                log::debug!("contify: contifying group {:?} with return cont {rc}", ns);
                self.contify_term(term, &ns, rc);
            }
        }
        log::debug!("contify: contification pass complete");
    }

    /// Finds all tail calls to functions in the given term that return to the given continuation, and populates `tailcall_set` with this information.
    fn tailcalls(&mut self, t: TermId, retc: VarId) {
        log::trace!("tailcalls: scanning {t:?} for tail calls via {retc}");
        for occ in self.graph.iter_occ(retc) {
            let site = self.graph.site(occ);
            if !self.is_descendant_of_term(site, t) {
                continue;
            }

            if let TermKind::App(fun, retk, _) = self.graph[site].kind {
                if retk == occ {
                    log::trace!("tailcalls: found tail call at {site:?}, callee={fun:?}");
                    self.tailcall_set[retc].push(fun);
                }
            }
        }
    }

    /// Checks if all calls to functions in `ns` return to the same continuation, and if so, returns it. Otherwise, returns None.
    fn common_return_cont(
        &self,
        t: TermId,
        ns: &Set<VarId>,
        ignore: Option<VarId>,
    ) -> Option<VarId> {
        log::trace!("common_return_cont: checking group {:?} in {t:?}", ns);
        let mut result = SingleValueSet::Bottom;

        for &var in ns.iter() {
            for occ in self.graph.iter_occ(var) {
                let site = self.graph.site(occ);
                let Some(local_ignore) = self.ignore_for_site(t, site, ns, ignore) else {
                    continue;
                };

                let value = match self.graph[site].kind {
                    TermKind::Let(_, Expression::PrimCall(_, args), _)
                        if args.as_slice(&self.graph.occ_pool).contains(&occ) =>
                    {
                        SingleValueSet::Top
                    }

                    TermKind::Continue(_, args)
                        if args.as_slice(&self.graph.occ_pool).contains(&occ) =>
                    {
                        SingleValueSet::Top
                    }

                    TermKind::App(_, _, args)
                        if args.as_slice(&self.graph.occ_pool).contains(&occ) =>
                    {
                        SingleValueSet::Top
                    }

                    TermKind::App(fun, retc, _)
                        if fun == occ && local_ignore != Some(self.graph.var(retc)) =>
                    {
                        SingleValueSet::Singleton(self.graph.var(retc))
                    }

                    _ => SingleValueSet::Bottom,
                };

                result = result.join(value);
                if matches!(result, SingleValueSet::Top) {
                    return None;
                }
            }
        }

        match result {
            SingleValueSet::Singleton(var) => {
                log::trace!("common_return_cont: common return cont = {var}");
                Some(var)
            }
            _ => {
                log::trace!("common_return_cont: no common return cont found");
                None
            }
        }
    }

    /// Check if `term` is a descendant of `root` in the graph. This is used to check if a call site is inside the body of a function we're trying to contify.
    fn is_descendant_of_term(&self, mut term: TermId, root: TermId) -> bool {
        loop {
            if term == root {
                return true;
            }

            match self.graph[term].parent {
                Some(ParentLink::Term(parent)) => term = parent,
                Some(ParentLink::Func(func)) => match self.graph[func].parent {
                    Some(parent) => term = parent,
                    None => return false,
                },
                Some(ParentLink::Cont(cont)) => match self.graph[cont].parent {
                    Some(parent) => term = parent,
                    None => return false,
                },
                None => return false,
            }
        }
    }

    /// Determines the appropriate continuation to ignore for a given site.
    fn ignore_for_site(
        &self,
        root: TermId,
        mut term: TermId,
        ns: &Set<VarId>,
        default_ignore: Option<VarId>,
    ) -> Option<Option<VarId>> {
        let mut local_ignore = default_ignore;

        loop {
            if term == root {
                return Some(local_ignore);
            }

            match self.graph[term].parent {
                Some(ParentLink::Term(parent)) => term = parent,
                Some(ParentLink::Func(func)) => {
                    let func = &self.graph[func];
                    if ns.contains(&func.var) {
                        local_ignore = Some(func.return_continuation);
                    }
                    term = func.parent?;
                }
                Some(ParentLink::Cont(cont)) => {
                    term = self.graph[cont].parent?;
                }
                None => return None,
            }
        }
    }

    /// Find all contifiable functions inside the given term,
    /// grouped by their return continuations. The returned vector contains pairs of the form
    /// (set of functions, common return continuation).
    fn contifiables(
        &mut self,
        funcs: EntityList<FuncId>,
        body: TermId,
    ) -> Vec<(Set<VarId>, VarId)> {
        let mut graph = petgraph::Graph::<VarId, ()>::new();
        let mut node_map = HashMap::new();

        let names = funcs
            .as_slice(&self.graph.func_pool)
            .iter()
            .map(|&f| {
                let var = self.graph[f].var;
                let node = graph.add_node(var);
                node_map.insert(var, node);
                var
            })
            .collect::<Vec<_>>();
        let lfuns = funcs.len(&self.graph.func_pool);
        for i in 0..lfuns {
            let func = funcs.get(i, &self.graph.func_pool).unwrap();
            let retk = self.graph[func].return_continuation;
            let fbody = self.graph[func].body();
            self.tailcalls(fbody, retk);
            let from = node_map[&self.graph[func].var];

            for to_fun in self.tailcall_set[retk]
                .iter()
                .filter(|&f| names.contains(&self.graph.var(*f)))
            {
                if let Some(&to) = node_map.get(&self.graph.var(*to_fun)) {
                    graph.add_edge(from, to, ());
                }
            }
        }

        let sccs = petgraph::algo::kosaraju_scc(&graph);

        sccs.into_iter()
            .filter_map(|scc| {
                let ns = scc.iter().map(|&node| graph[node]).collect::<Set<_>>();

                match self.common_return_cont(body, &ns, None) {
                    Some(val) => Some((ns, val)),
                    _ => None,
                }
            })
            .collect()
    }

    /// Recursive helper that attempts to "push" the insertion site of the
    /// newly created `letk` as deep as possible while still
    /// dominating all uses. Returns whether any changes were made.
    ///
    /// In real usage, this function must always return true.
    fn push_down(
        &mut self,
        wrap_with_cnts: &mut dyn Fn(&mut Self, TermId) -> Insertion<'gc>,
        t: TermId,
        ns: &Set<VarId>,
    ) -> Option<Insertion<'gc>> {
        match self.graph[t].kind {
            TermKind::Let(_, _, body) => self.push_down(wrap_with_cnts, body, ns),
            TermKind::Letk(ks, body) => {
                let len = ks.len(&self.graph.cont_pool);
                let insertions = (0..len)
                    .map(|i| {
                        let k = ks.get(i, &self.graph.cont_pool).unwrap();
                        let cbody = self.graph[k].body();
                        self.push_down(wrap_with_cnts, cbody, ns)
                    })
                    .collect::<Vec<_>>();

                let did_wrap_k = insertions.iter().any(|x| x.is_some());

                match self.push_down(wrap_with_cnts, body, ns) {
                    None if !did_wrap_k => None,
                    None => {
                        if insertions.len() > 1 {
                            log::trace!(
                                "push_down: wrapping letk {t:?} because multiple branches need it"
                            );
                            Some(wrap_with_cnts(self, t))
                        } else {
                            insertions.into_iter().flatten().next()
                        }
                    }

                    Some(insertion) if !did_wrap_k => Some(insertion),
                    Some(_) => {
                        log::trace!(
                            "push_down: wrapping letk {t:?} because body needs it even though some branches already do"
                        );
                        Some(wrap_with_cnts(self, t))
                    }
                }
            }

            TermKind::Fix(funs, body) => {
                let len = funs.len(&self.graph.func_pool);
                let insertions = (0..len)
                    .map(|i| {
                        let f = funs.get(i, &self.graph.func_pool).unwrap();
                        let fbody = self.graph[f].body();
                        self.push_down(wrap_with_cnts, fbody, ns)
                    })
                    .collect::<Vec<_>>();

                let did_wrap_f = insertions.iter().any(|x| x.is_some());

                match self.push_down(wrap_with_cnts, body, ns) {
                    None if !did_wrap_f => None,
                    None => {
                        if insertions.len() > 1 {
                            log::trace!(
                                "push_down: wrapping fix {t:?} because multiple branches need it"
                            );
                            Some(wrap_with_cnts(self, t))
                        } else {
                            insertions.into_iter().flatten().next()
                        }
                    }

                    Some(insertion) if !did_wrap_f => Some(insertion),
                    Some(_) => {
                        log::trace!(
                            "push_down: wrapping fix {t:?} because body needs it even though some branches already do"
                        );
                        Some(wrap_with_cnts(self, t))
                    }
                }
            }

            TermKind::App(fun, _, _) if ns.contains(&self.graph.var(fun)) => {
                log::trace!("push_down: found tail call site at {t:?} that needs wrapping");
                Some(wrap_with_cnts(self, t))
            }

            _ => None,
        }
    }

    /// Rewrites tail calls (`App`) to contified functions into direct
    /// continuation invocations (`Continue`). Traverses all occurrences
    /// of the contified functions and updates their call sites.
    fn transform_apps(&mut self, ns: &Set<VarId>) {
        log::debug!("transform_apps: rewriting tail calls for {:?}", ns);
        for &var in ns.iter() {
            for occ in self.graph.occ(var) {
                let site = self.graph.site(occ);
                if let TermKind::App(fun, retc, args) = self.graph[site].kind {
                    if ns.contains(&self.graph.var(fun)) {
                        self.graph.delete(retc, &mut |_| {});
                        self.graph[site].kind = TermKind::Continue(fun, args);
                    } else {
                        let fvar = self.graph.var(fun);
                        let fretc = self.graph.var(retc);
                        println!(
                            "transform_apps: skipping non-tail call at {site:?} (fun={fvar:?}, retc={fretc:?})"
                        );
                        unreachable!("invalid graph: expected tail call to contified function");
                    }
                } else {
                    unreachable!("invalid graph: contified function not used in tail-call");
                }
            }
        }
    }

    /// Contifies the group `ns` (a set of mutually recursive function
    /// bindings) assuming they share the return continuation `rc`.
    ///
    /// Steps:
    /// 1. Split functions: (to_contify, untouched).
    /// 2. Convert each target function into a `Cont::Local`, substituting
    ///    its individual return continuation with the shared `rc`.
    /// 3. Rebuild the surrounding `Fix` with any untouched functions.
    /// 4. Insert the new continuations via a `Letk`, pushing them down
    ///    as deep as possible (to narrow scope) using `push_down`.
    /// 5. Rewrite tail `App`s to these functions into `Continue`.
    ///
    /// `ns`: set of function bindings to contify.
    /// `rc`: return_cont chosen by `common_return_cont`.
    fn contify_term(&mut self, t: TermId, ns: &Set<VarId>, rc: VarId) {
        log::debug!(
            "contify_term: contifying {:?} in {t:?} with return cont {rc}",
            ns
        );
        let TermKind::Fix(mut funs, body) = self.graph[t].kind else {
            unreachable!("invalid graph: expected a Fix term");
        };

        let (to_contify, untouched): (Vec<_>, Vec<_>) = funs
            .as_slice(&self.graph.func_pool)
            .iter()
            .copied()
            .partition(|&f| ns.contains(&self.graph[self.graph[f].var].var));

        let contified = to_contify
            .into_iter()
            .map(|f| {
                let from = self.graph[f].return_continuation;
                let to = rc;

                self.graph.merge_occurrences(from, to);
                self.func2cont(f)
            })
            .collect::<Vec<_>>();

        let fix = if untouched.is_empty() {
            self.graph.splice(t, body);
            body
        } else {
            println!(
                "contify_term: rebuilding Fix at {t:?} with untouched functions {:?}",
                untouched
            );
            // free memory in func list pool
            funs.clear(&mut self.graph.func_pool);
            let new_funs = EntityList::from_slice(&untouched, &mut self.graph.func_pool);
            self.graph[t].kind = TermKind::Fix(new_funs, body);
            t
        };

        let Some(insertion) = self.push_down(
            &mut |this, t| {
                let conts = EntityList::from_slice(&contified, &mut this.graph.cont_pool);

                Insertion {
                    before: t,
                    term: TermKind::Letk(conts, t),
                }
            },
            fix,
            ns,
        ) else {
            unreachable!("failed to find insertion point that dominates all tail call sites");
        };

        self.graph.insert_before(insertion.before, insertion.term);

        // ns now contains continuations instead of functions, so update the tail calls accordingly
        self.transform_apps(ns);
    }

    /// Converts a function into a continuation. The new continuation will have the same body as the original function,
    /// and its parameters will be the same as the original function's parameters plus the original return
    fn func2cont(&mut self, func: FuncId) -> ContId {
        let var = self.graph[func].var;
        log::debug!("func2cont: converting {func} (var={var}) to continuation");
        let body = self.graph[func].body();
        let parameters = self.graph[func].parameters;
        let variadic = self.graph[func].variadic;
        let meta = self.graph[func].meta;
        let new_cont = self.graph.continuations.push(Cont {
            var,
            meta,
            body: Some(body),
            parameters,
            variadic,
            parent: None,
        });

        // update the graph to reflect the new variable definition
        self.graph[var].def = VarDef::Cont(new_cont);
        new_cont
    }
}

/// A valid insertion point for a `letk` must dominate all uses of the contified functions. If there are multiple such points, we want to choose the one that is deepest in the graph to minimize the scope of the new continuations. This struct represents a candidate insertion point and the term that would be inserted there if we choose it.
struct Insertion<'gc> {
    before: TermId,
    term: TermKind<'gc>,
}
