//! What the emitter needs to know before it emits: which variables live in boxes, and the free
//! variables of every procedure.

use std::collections::{HashMap, HashSet};

use crate::expander::core::{
    GlobalSlot, LVarRef, LetStyle, LinkletTerm, Proc, ProcRef, TermKind, TermRef,
};
use crate::heap::Gc;
use crate::runtime::{MutationContext, objectmodel::Value};

pub(super) fn key<'gc>(var: LVarRef<'gc>) -> usize {
    Gc::as_ptr(var) as usize
}

pub(super) struct Analysis<'gc> {
    ctx: MutationContext<'gc>,
    /// Variables that live in a box: assigned ones and the ones `letrec` binds.
    boxed: HashSet<usize>,
    free: HashMap<usize, Vec<LVarRef<'gc>>>,
    linklet_procs: HashMap<usize, ProcRef<'gc>>,
}

impl<'gc> Analysis<'gc> {
    pub(super) fn new(ctx: MutationContext<'gc>) -> Self {
        Self {
            ctx,
            boxed: HashSet::new(),
            free: HashMap::new(),
            linklet_procs: HashMap::new(),
        }
    }

    pub(super) fn is_boxed(&self, var: LVarRef<'gc>) -> bool {
        self.boxed.contains(&key(var))
    }

    /// The procedure a linklet body is: a lambda over the slots.
    pub(super) fn linklet_proc(
        &mut self,
        linklet: Gc<'gc, LinkletTerm<'gc>>,
        source: Value<'gc>,
    ) -> ProcRef<'gc> {
        let k = Gc::as_ptr(linklet) as usize;
        if let Some(proc) = self.linklet_procs.get(&k) {
            return *proc;
        }
        let proc = self.ctx.allocate(Proc {
            name: linklet.name,
            source,
            args: linklet.slots,
            variadic: None,
            body: linklet.body,
            meta: linklet.meta,
        });
        self.linklet_procs.insert(k, proc);
        proc
    }

    /// Find the variables that need a box in `term`, which is everything under it.
    pub(super) fn scan(&mut self, mut term: TermRef<'gc>) {
        loop {
            match term.kind {
                TermKind::LRef(_) | TermKind::GlobalRef(..) | TermKind::PrimRef(_) => return,
                TermKind::Const(_) => return,
                TermKind::LSet(var, value) => {
                    self.boxed.insert(key(var));
                    term = value;
                }
                TermKind::GlobalSet(.., value) => term = value,
                TermKind::Linklet(linklet) => term = linklet.body,
                TermKind::If(test, then, otherwise) => {
                    self.scan(test);
                    self.scan(then);
                    term = otherwise;
                }
                TermKind::Seq(head, tail) => {
                    self.scan(head);
                    term = tail;
                }
                TermKind::Let(let_) => {
                    if matches!(let_.style, LetStyle::LetRec | LetStyle::LetRecStar) {
                        for var in let_.lhs.iter() {
                            self.boxed.insert(key(*var));
                        }
                    }
                    for rhs in let_.rhs.iter() {
                        self.scan(*rhs);
                    }
                    term = let_.body;
                }
                TermKind::Fix(fix) => {
                    for proc in fix.rhs.iter() {
                        self.scan(proc.body);
                    }
                    term = fix.body;
                }
                TermKind::Receive(_, _, producer, consumer) => {
                    self.scan(producer);
                    term = consumer;
                }
                TermKind::WithContinuationMark(k, m, result) => {
                    self.scan(k);
                    self.scan(m);
                    term = result;
                }
                TermKind::Values(values) => {
                    for value in values.iter() {
                        self.scan(*value);
                    }
                    return;
                }
                TermKind::Proc(proc) => term = proc.body,
                TermKind::CaseLambda(clauses) => {
                    for clause in clauses.iter() {
                        self.scan(clause.body);
                    }
                    return;
                }
                TermKind::Call(callee, args) => {
                    self.scan(callee);
                    for arg in args.iter() {
                        self.scan(*arg);
                    }
                    return;
                }
                TermKind::PrimCall(_, args) => {
                    for arg in args.iter() {
                        self.scan(*arg);
                    }
                    return;
                }
            }
        }
    }

    /// The free variables of `proc` in first-use order.
    pub(super) fn free_vars(&mut self, proc: ProcRef<'gc>) -> Vec<LVarRef<'gc>> {
        let k = Gc::as_ptr(proc) as usize;
        if let Some(free) = self.free.get(&k) {
            return free.clone();
        }
        let mut walk = Walk {
            refs: Vec::new(),
            seen: HashSet::new(),
            binds: HashSet::new(),
        };
        for arg in proc.args.iter() {
            walk.binds.insert(key(*arg));
        }
        if let Some(rest) = proc.variadic {
            walk.binds.insert(key(rest));
        }
        self.walk(proc.body, &mut walk);
        let free: Vec<LVarRef<'gc>> = walk
            .refs
            .into_iter()
            .filter(|var| !walk.binds.contains(&key(*var)))
            .collect();
        self.free.insert(k, free.clone());
        free
    }

    fn walk(&mut self, mut term: TermRef<'gc>, w: &mut Walk<'gc>) {
        loop {
            match term.kind {
                TermKind::LRef(var) => return w.note(var),
                TermKind::GlobalRef(_, _, slot) => {
                    if let GlobalSlot::Box(var) = slot {
                        w.note(var);
                    }
                    return;
                }
                TermKind::PrimRef(_) | TermKind::Const(_) => return,
                TermKind::LSet(var, value) => {
                    w.note(var);
                    term = value;
                }
                TermKind::GlobalSet(_, _, slot, value) => {
                    if let GlobalSlot::Box(var) = slot {
                        w.note(var);
                    }
                    term = value;
                }
                TermKind::Linklet(linklet) => {
                    let proc = self.linklet_proc(linklet, term.source());
                    for var in self.free_vars(proc) {
                        w.note(var);
                    }
                    return;
                }
                TermKind::If(test, then, otherwise) => {
                    self.walk(test, w);
                    self.walk(then, w);
                    term = otherwise;
                }
                TermKind::Seq(head, tail) => {
                    self.walk(head, w);
                    term = tail;
                }
                TermKind::Let(let_) => {
                    for rhs in let_.rhs.iter() {
                        self.walk(*rhs, w);
                    }
                    for var in let_.lhs.iter() {
                        w.binds.insert(key(*var));
                    }
                    term = let_.body;
                }
                TermKind::Fix(fix) => {
                    for var in fix.lhs.iter() {
                        w.binds.insert(key(*var));
                    }
                    for proc in fix.rhs.iter() {
                        for var in self.free_vars(*proc) {
                            w.note(var);
                        }
                    }
                    term = fix.body;
                }
                TermKind::Receive(formals, rest, producer, consumer) => {
                    self.walk(producer, w);
                    for var in formals.iter() {
                        w.binds.insert(key(*var));
                    }
                    if let Some(rest) = rest {
                        w.binds.insert(key(rest));
                    }
                    term = consumer;
                }
                TermKind::WithContinuationMark(k, m, result) => {
                    self.walk(k, w);
                    self.walk(m, w);
                    term = result;
                }
                TermKind::Values(values) => {
                    for value in values.iter() {
                        self.walk(*value, w);
                    }
                    return;
                }
                TermKind::Proc(proc) => {
                    for var in self.free_vars(proc) {
                        w.note(var);
                    }
                    return;
                }
                TermKind::CaseLambda(clauses) => {
                    for clause in clauses.iter() {
                        for var in self.free_vars(*clause) {
                            w.note(var);
                        }
                    }
                    return;
                }
                TermKind::Call(callee, args) => {
                    self.walk(callee, w);
                    for arg in args.iter() {
                        self.walk(*arg, w);
                    }
                    return;
                }
                TermKind::PrimCall(_, args) => {
                    for arg in args.iter() {
                        self.walk(*arg, w);
                    }
                    return;
                }
            }
        }
    }
}

struct Walk<'gc> {
    refs: Vec<LVarRef<'gc>>,
    seen: HashSet<usize>,
    binds: HashSet<usize>,
}

impl<'gc> Walk<'gc> {
    fn note(&mut self, var: LVarRef<'gc>) {
        if self.seen.insert(key(var)) {
            self.refs.push(var);
        }
    }
}
