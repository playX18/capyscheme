use crate::list;
use crate::runtime::value::ValueEqual;
use crate::{
    expander::core::{
        LVarRef, Let, LetStyle, Term, TermKind, TermRef, constant, fresh_lvar, lref, prim_call_term,
    },
    global,
    runtime::{
        Context,
        modules::{Variable, resolve_module},
        value::{Str, Value},
        vm::syntax::SyntaxTransformer,
    },
    static_symbols,
};
use rsgc::alloc::ArrayRef;
use rsgc::{Gc, alloc::Array};
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
};

struct DeclarativeToplevels<'gc> {
    dynamic: HashSet<Value<'gc>>,
    assigned: HashMap<(ValueEqual<'gc>, Value<'gc>), Option<TermRef<'gc>>>,
    defined: HashMap<(ValueEqual<'gc>, Value<'gc>), Option<TermRef<'gc>>>,
}

fn compute_declarative_toplvels<'gc>(
    ctx: Context<'gc>,
    t: TermRef<'gc>,
) -> HashMap<(ValueEqual<'gc>, Value<'gc>), Option<TermRef<'gc>>> {
    let dt = RefCell::new(DeclarativeToplevels {
        dynamic: HashSet::new(),
        assigned: HashMap::new(),
        defined: HashMap::new(),
    });

    t.for_each(|x| {
        let mut dt = dt.borrow_mut();
        match x.kind {
            TermKind::ToplevelSet(module, name, _) => {
                if module != Value::new(false) {
                    dt.assigned.insert((module.into(), name), None);
                } else {
                    dt.dynamic.insert(name);
                }
            }

            TermKind::Define(module, name, exp) => {
                if module != Value::new(false) {
                    let ht = if dt.defined.contains_key(&(module.into(), name)) {
                        &mut dt.assigned
                    } else {
                        &mut dt.defined
                    };
                    ht.insert((module.into(), name), Some(exp));
                } else {
                    dt.dynamic.insert(name);
                }
            }

            _ => (),
        }
    });

    let mut declarative = HashMap::new();

    let is_declarative_module = |name| {
        let m = resolve_module(ctx, name, false, false);
        // default is all modules are declarative
        m.is_none_or(|m| m.declarative.get())
    };

    let dt = dt.into_inner();

    dt.defined.iter().for_each(|((module, name), exp)| {
        if !dt.assigned.contains_key(&(*module, *name))
            && !dt.dynamic.contains(&name)
            && is_declarative_module(module.0)
        {
            declarative.insert((*module, *name), *exp);
        } else {
            /*println!(
                ";; Non-declarative toplevel: {}@{}, dynamic={}, assigned={}, declarative={}",
                module.0,
                name,
                dt.dynamic.contains(&name),
                dt.assigned.contains_key(&(*module, *name)),
                dt.defined.contains_key(&(*module, *name))
            );*/
        }
    });

    declarative
}

fn compute_private_toplevels<'gc>(
    ctx: Context<'gc>,
    declarative: &HashMap<(ValueEqual<'gc>, Value<'gc>), Option<TermRef<'gc>>>,
) -> HashSet<(ValueEqual<'gc>, Value<'gc>)> {
    let mut exports = HashMap::new();
    let mut exports_macro = HashMap::new();
    declarative.iter().for_each(|(&(module, _), _)| {
        if exports_macro.get(&module).is_none() {
            exports_macro.insert(module, false);
            let i = resolve_module(ctx, module.0, false, false);
            if let Some(m) = i.and_then(|m| m.public_interface.get()) {
                for (k, v) in m.obarray.get().iter() {
                    exports.insert(v, k);
                    if let Some(var) = v.try_as::<Variable>()
                        && var.is_bound()
                        && var.get().is::<SyntaxTransformer>()
                    {
                        exports_macro.insert(module, true);
                    }
                }
            }
        }
    });

    let mut private = HashSet::new();

    declarative.iter().for_each(|(&(module, name), _)| {
        let is_macro_export = exports_macro.get(&module).copied().unwrap_or(false);
        let is_exported = if let Some(m) = resolve_module(ctx, module.0, false, false) {
            if let Some(var) = m.local_variable(ctx, name) {
                let var: Value = var.into();
                exports.contains_key(&var)
            } else {
                false
            }
        } else {
            false
        };

        if !is_macro_export && !is_exported {
            private.insert((module, name));
        }
    });

    private
}

pub fn letrectify<'gc>(ctx: Context<'gc>, t: TermRef<'gc>) -> TermRef<'gc> {
    let declarative = compute_declarative_toplvels(ctx, t);
    let private = compute_private_toplevels(ctx, &declarative);

    let mut pass = Letrectify::new(ctx, declarative, private);

    pass.visit_top_level(t)
}

struct Letrectify<'gc> {
    ctx: Context<'gc>,

    mod_vars: HashMap<ValueEqual<'gc>, LVarRef<'gc>>,
    declarative_box_and_value:
        HashMap<(ValueEqual<'gc>, Value<'gc>), (Option<LVarRef<'gc>>, LVarRef<'gc>)>,
}

impl<'gc> Letrectify<'gc> {
    pub fn new(
        ctx: Context<'gc>,
        declarative: HashMap<(ValueEqual<'gc>, Value<'gc>), Option<TermRef<'gc>>>,
        private: HashSet<(ValueEqual<'gc>, Value<'gc>)>,
    ) -> Self {
        let declarative_box_and_value = {
            let mut tab = HashMap::new();
            declarative.iter().for_each(|(&(module, name), _)| {
                // println!(";; Declarative toplevel: {}@{}", module.0, name);
                let boxed = if private.get(&(module, name)).is_none() {
                    Some(fresh_lvar(
                        ctx,
                        Str::new(&ctx, format!("{}#{}", module.0, name).as_str(), true).into(),
                    ))
                } else {
                    None
                };
                let val = fresh_lvar(
                    ctx,
                    Str::new(&ctx, format!("{}@{}", module.0, name).as_str(), true).into(),
                );
                tab.insert((module.into(), name), (boxed, val));
            });
            tab
        };
        Self {
            ctx,

            declarative_box_and_value,
            mod_vars: HashMap::new(),
        }
    }

    fn add_binding(
        &mut self,
        _name: Value<'gc>,
        var: LVarRef<'gc>,
        val: TermRef<'gc>,
        tail: TermRef<'gc>,
    ) -> TermRef<'gc> {
        match tail.kind {
            TermKind::Let(l) if matches!(l.style, LetStyle::LetRecStar) => {
                let mut vars = Vec::with_capacity(l.lhs.len() + 1);

                vars.push(var);
                vars.extend_from_slice(&l.lhs);
                let lhs = Array::from_slice(&self.ctx, &vars);
                let mut inits = Vec::with_capacity(l.rhs.len() + 1);
                inits.push(val);
                inits.extend_from_slice(&l.rhs);
                let rhs = Array::from_slice(&self.ctx, &inits);

                Gc::new(
                    &self.ctx,
                    Term {
                        source: tail.source.get().into(),
                        kind: TermKind::Let(Let {
                            style: LetStyle::LetRecStar,
                            rhs,
                            lhs,

                            body: l.body,
                        }),
                    },
                )
            }

            _ => Gc::new(
                &self.ctx,
                Term {
                    source: tail.source.get().into(),
                    kind: TermKind::Let(Let {
                        style: LetStyle::LetRecStar,
                        rhs: Array::from_slice(&self.ctx, &[val]),
                        lhs: Array::from_slice(&self.ctx, &[var]),
                        body: tail,
                    }),
                },
            ),
        }
    }

    fn add_statement(
        &mut self,
        _src: Value<'gc>,
        stmt: TermRef<'gc>,
        tail: TermRef<'gc>,
    ) -> TermRef<'gc> {
        if stmt.is_transparent() {
            tail
        } else {
            let name = self.ctx.intern("_");
            let tmp = fresh_lvar(self.ctx, name);
            self.add_binding(name, tmp, stmt, tail)
        }
    }

    fn visit_top_level(&mut self, expr: TermRef<'gc>) -> TermRef<'gc> {
        let ctx = self.ctx;
        let src = expr.source.get();
        match expr.kind {
            TermKind::Define(module, name, exp) => {
                match self
                    .declarative_box_and_value
                    .get(&(module.into(), name))
                    .copied()
                {
                    None => self.visit_expr(expr),
                    Some((None, value)) => {
                        let val = self.visit_expr(exp);
                        self.add_binding(name, value, val, constant(self.ctx, Value::undefined()))
                    }

                    Some((Some(boxed), value)) => match self.mod_vars.get(&ValueEqual(module)) {
                        None => {
                            let mod_var = fresh_lvar(ctx, ctx.intern("mod"));
                            self.mod_vars.insert(ValueEqual(module), mod_var);
                            let tail = self.visit_top_level(expr);
                            self.add_binding(
                                Value::new(false),
                                mod_var,
                                prim_call_term(ctx, ctx.intern("current-module"), &[], src),
                                tail,
                            )
                        }

                        Some(mod_var) => {
                            let loc = prim_call_term(
                                ctx,
                                ctx.intern("module-ensure-local-variable!"),
                                &[lref(ctx, *mod_var), constant(ctx, name)],
                                src,
                            );

                            let exp = self.visit_expr(exp);
                            let val_ref = lref(ctx, value);

                            let init = prim_call_term(
                                ctx,
                                ctx.intern("variable-set!"),
                                &[lref(ctx, boxed), val_ref],
                                src,
                            );

                            let stmt = self.add_statement(
                                src,
                                init,
                                constant(self.ctx, Value::undefined()),
                            );

                            let tail = self.add_binding(name, value, exp, stmt);

                            self.add_binding(name, boxed, loc, tail)
                        }
                    },
                }
            }

            TermKind::Let(let_) if let_.style == LetStyle::Let => {
                self.handle_let(&let_.lhs, &let_.rhs, let_.body)
            }

            TermKind::Seq(head, tail) => {
                /*let tail = self.visit_top_level(*seq.last().unwrap());
                let mut head = Vec::new();

                for stmt in &seq[0..seq.len() - 1] {
                    let stmt = self.visit_top_level(*stmt);
                    if let TermKind::Let(l) = stmt.kind
                        && l.style == LetStyle::LetRecStar
                    {
                        let acc = self.add_statement(src, l.body, tail);
                    }
                }*/

                let head = self.visit_top_level(head);
                let tail = self.visit_top_level(tail);
                if let TermKind::Let(l) = head.kind
                    && l.style == LetStyle::LetRecStar
                {
                    let acc = self.add_statement(src, l.body, tail);
                    l.lhs
                        .iter()
                        .zip(l.rhs.iter())
                        .rfold(acc, |tail, (&var, &val)| {
                            self.add_binding(var.name, var, val, tail)
                        })
                } else {
                    self.add_statement(src, head, tail)
                }
            }
            _ => self.visit_expr(expr),
        }
    }

    fn visit_expr(&mut self, expr: TermRef<'gc>) -> TermRef<'gc> {
        expr.post_order(self.ctx, |ctx, expr| match &expr.kind {
            TermKind::ToplevelRef(module, name) => {
                match self
                    .declarative_box_and_value
                    .get(&(ValueEqual(*module), *name))
                {
                    None => expr,
                    Some((_, value)) => {
                        /*println!(
                            ";; Rewrite toplevel-ref {}@{} -> {}",
                            module, name, value.name
                        );*/
                        lref(ctx, *value)
                    }
                }
            }
            _ => expr,
        })
    }

    fn handle_let(
        &mut self,
        vars: &[LVarRef<'gc>],
        vals: &[TermRef<'gc>],
        body: TermRef<'gc>,
    ) -> TermRef<'gc> {
        if vars.is_empty() {
            return self.visit_expr(body);
        }

        let var = vars[0];
        let val = vals[0];
        let vars = &vars[1..];
        let vals = &vals[1..];

        let val = self.visit_expr(val);

        let exp = self.handle_let(vars, vals, body);
        if let Some((module, _)) = is_define_module_term(self.ctx, exp) {
            self.mod_vars.insert(ValueEqual(module), var);
        }

        self.add_binding(var.name, var, val, exp)
    }
}

static_symbols!(SYM_DEFINE_MODULE = "define-module*");

global!(
    pub CAPY_MODULE<'gc>: Value<'gc> = (ctx) {
        let name = ctx.intern("capy");
        list!(ctx, name)
    };
);

// check if `t` is a `((@ (capy) define-module*) mod)` call.
pub fn is_define_module_term<'gc>(
    ctx: Context<'gc>,
    t: TermRef<'gc>,
) -> Option<(Value<'gc>, ArrayRef<'gc, TermRef<'gc>>)> {
    match t.kind {
        TermKind::Call(rator, args) => {
            if let TermKind::ModuleRef(module, name, _) = rator.kind
                && module.r5rs_equal(*capy_module(ctx))
                && name == sym_define_module(ctx).into()
                && args.len() == 1
                && let TermKind::Const(mod_name) = args[0].kind
            {
                // println!(";; module definition: {}", mod_name);
                return Some((mod_name, args));
            }
        }

        _ => (),
    }

    None
}
