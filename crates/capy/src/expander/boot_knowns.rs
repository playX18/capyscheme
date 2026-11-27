//! Inline knowns of boot linklets.
//!
//! A boot linklet's meta is `(#%boot key knowns)`. KNOWNS lists the exported procedures that
//! are small enough for importers to copy their bodies into the call sites, as
//! `(name procedure mask body)`. The Scheme compiler makes these knowns for the boot files
//! that psyntax expands (`boot-unit-knowns` and `inline-known-body` in
//! `pkgs/core/src/boot/units.scm`); this module makes them for the files this expander
//! expands. The datum and the rules for keeping a body are the same on both sides, so a unit
//! sees the same knowns for its imports whichever way they were loaded. If the two sides ever
//! disagreed, stage-1 and stage-2 of the bootstrap would compile the boot files differently,
//! and `bootstrap.sh` would report that they are not byte-identical.

use std::collections::HashMap;

use crate::{
    compiler::tree::{LVarRef, LetStyle, TermKind, TermRef},
    list,
    runtime::{
        MutationContext,
        objectmodel::{Symbol, Value, Vector},
    },
};

/// The largest body, in terms, that is kept (`inline-known-max-terms`).
const MAX_TERMS: i32 = 32;
/// The most formals an inline procedure can have, so its arity mask is a fixnum.
const MAX_FORMALS: usize = 16;

/// The knowns of the linklet KEY whose body is BODY. EXPORTED says whether a name of the
/// linklet is a variable other units can refer to.
pub fn boot_knowns<'gc>(
    ctx: MutationContext<'gc>,
    key: Value<'gc>,
    body: TermRef<'gc>,
    exported: &dyn Fn(Value<'gc>) -> bool,
) -> Value<'gc> {
    let mut sets = HashMap::new();
    count_sets(key, body, &mut sets);

    let mut forms = Vec::new();
    top_level_forms(body, &mut forms);

    let procedure: Value<'gc> = Symbol::from_str(ctx, "procedure").into();
    let mut out = Vec::new();
    for form in forms {
        let TermKind::GlobalSet(k, name, _, value) = form.kind else {
            continue;
        };
        // a procedure defined once and never assigned, the `locals` of `close-linking-unit`
        if !k.r5rs_equal(key) || sets.get(&name).copied() != Some(1) || !exported(name) {
            continue;
        }
        let TermKind::Proc(proc) = value.kind else {
            continue;
        };
        if proc.variadic.is_some() || proc.args.len() >= MAX_FORMALS {
            continue;
        }
        let mut walker = Walker {
            ctx,
            key,
            name,
            exported,
            budget: MAX_TERMS,
            count: 0,
            bound: Vec::new(),
        };
        for arg in proc.args.as_slice() {
            walker.bind(*arg);
        }
        let Some(datum) = walker.walk(proc.body) else {
            continue;
        };
        let arity = proc.args.len() as i32;
        let body: Value<'gc> = Vector::from_slice(ctx, &[Value::new(arity), datum]).into();
        out.push(list!(ctx, name, procedure, Value::new(1i32 << arity), body));
    }
    Value::list_from_slice(ctx, &out)
}

/// The forms of a body, through its sequences, as `top-level-forms` sees them.
fn top_level_forms<'gc>(t: TermRef<'gc>, out: &mut Vec<TermRef<'gc>>) {
    match t.kind {
        TermKind::Seq(head, tail) => {
            top_level_forms(head, out);
            top_level_forms(tail, out);
        }
        TermKind::Const(v) if v == Value::undefined() => {}
        _ => out.push(t),
    }
}

/// Counts the assignments, definitions included, of each variable of the linklet KEY in T.
fn count_sets<'gc>(key: Value<'gc>, t: TermRef<'gc>, counts: &mut HashMap<Value<'gc>, u32>) {
    let mut each = |ts: &[TermRef<'gc>], counts: &mut HashMap<Value<'gc>, u32>| {
        for t in ts {
            count_sets(key, *t, counts);
        }
    };
    match t.kind {
        TermKind::LRef(_)
        | TermKind::GlobalRef(..)
        | TermKind::PrimRef(_)
        | TermKind::Const(_)
        | TermKind::Linklet(_) => {}
        TermKind::LSet(_, v) => count_sets(key, v, counts),
        TermKind::GlobalSet(k, name, _, v) => {
            if k.r5rs_equal(key) {
                *counts.entry(name).or_insert(0) += 1;
            }
            count_sets(key, v, counts);
        }
        TermKind::If(a, b, c) | TermKind::WithContinuationMark(a, b, c) => each(&[a, b, c], counts),
        TermKind::Seq(a, b) | TermKind::Receive(_, _, a, b) => each(&[a, b], counts),
        TermKind::Let(l) => {
            each(l.rhs.as_slice(), counts);
            count_sets(key, l.body, counts);
        }
        TermKind::Fix(f) => {
            for p in f.rhs.as_slice() {
                count_sets(key, p.body, counts);
            }
            count_sets(key, f.body, counts);
        }
        TermKind::Values(ts) => each(ts.as_slice(), counts),
        TermKind::Proc(p) => count_sets(key, p.body, counts),
        TermKind::CaseLambda(ps) => {
            for p in ps.as_slice() {
                count_sets(key, p.body, counts);
            }
        }
        TermKind::Call(f, args) => {
            count_sets(key, f, counts);
            each(args.as_slice(), counts);
        }
        TermKind::PrimCall(_, args) => each(args.as_slice(), counts),
    }
}

/// `known-literal?`: a constant that has no identity, so a copy of it is `eq?` to it.
fn known_literal(v: Value<'_>) -> bool {
    v.is_bool()
        || v.is_null()
        || v.is_char()
        || v.is::<Symbol>()
        || (v.is_int32() && (-(1 << 30)..(1 << 30)).contains(&v.as_int32()))
}

/// Turns the body of one procedure into its datum, `walk` of `inline-known-body`.
struct Walker<'a, 'gc> {
    ctx: MutationContext<'gc>,
    key: Value<'gc>,
    /// The procedure itself: a body that calls it is not kept.
    name: Value<'gc>,
    exported: &'a dyn Fn(Value<'gc>) -> bool,
    budget: i32,
    /// The number of the next variable bound.
    count: i32,
    bound: Vec<(LVarRef<'gc>, i32)>,
}

impl<'a, 'gc> Walker<'a, 'gc> {
    fn bind(&mut self, var: LVarRef<'gc>) {
        self.bound.push((var, self.count));
        self.count += 1;
    }

    fn tag(&self, name: &str) -> Value<'gc> {
        Symbol::from_str(self.ctx, name).into()
    }

    fn vector(&self, items: &[Value<'gc>]) -> Option<Value<'gc>> {
        Some(Vector::from_slice(self.ctx, items).into())
    }

    /// TAG followed by the datums of TS.
    fn node(&mut self, tag: &str, head: &[Value<'gc>], ts: &[TermRef<'gc>]) -> Option<Value<'gc>> {
        let mut items = vec![self.tag(tag)];
        items.extend_from_slice(head);
        for t in ts {
            items.push(self.walk(*t)?);
        }
        self.vector(&items)
    }

    fn walk(&mut self, t: TermRef<'gc>) -> Option<Value<'gc>> {
        self.budget -= 1;
        if self.budget < 0 {
            return None;
        }
        match t.kind {
            TermKind::Const(v) if v == Value::undefined() => self.vector(&[self.tag("v")]),
            TermKind::Const(v) if known_literal(v) => self.vector(&[self.tag("c"), v]),
            TermKind::LRef(var) => {
                let n = self.bound.iter().find(|(b, _)| *b == var)?.1;
                self.vector(&[self.tag("l"), Value::new(n)])
            }
            TermKind::GlobalRef(key, name, _) => {
                if key.r5rs_equal(self.key) && (name == self.name || !(self.exported)(name)) {
                    return None;
                }
                self.vector(&[self.tag("g"), key, name])
            }
            TermKind::PrimRef(prim) => self.vector(&[self.tag("p"), prim]),
            TermKind::PrimCall(prim, args) => self.node("pc", &[prim], args.as_slice()),
            TermKind::Call(f, args) => {
                let mut ts = vec![f];
                ts.extend_from_slice(args.as_slice());
                self.node("a", &[], &ts)
            }
            // this expander makes `values` a term, psyntax a call of the primitive
            TermKind::Values(ts) => {
                let capy: Value<'gc> = Symbol::from_str(self.ctx, "#%capy").into();
                let values: Value<'gc> = Symbol::from_str(self.ctx, "values").into();
                let operator = self.vector(&[self.tag("g"), list!(self.ctx, capy), values])?;
                // the operator is one more term, as the `global-ref` of psyntax is
                self.budget -= 1;
                if self.budget < 0 {
                    return None;
                }
                let mut items = vec![self.tag("a"), operator];
                for t in ts.as_slice() {
                    items.push(self.walk(*t)?);
                }
                self.vector(&items)
            }
            TermKind::If(a, b, c) => self.node("i", &[], &[a, b, c]),
            TermKind::Seq(a, b) => self.node("s", &[], &[a, b]),
            TermKind::Let(l) if l.style == LetStyle::Let => {
                // the right-hand sides first, then the variables are numbered, as in Scheme
                let mut items = vec![self.tag("let")];
                for r in l.rhs.as_slice() {
                    items.push(self.walk(*r)?);
                }
                for var in l.lhs.as_slice() {
                    self.bind(*var);
                }
                items.push(self.walk(l.body)?);
                self.vector(&items)
            }
            _ => None,
        }
    }
}
