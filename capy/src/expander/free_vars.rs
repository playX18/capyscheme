use std::collections::HashMap;

use rsgc::alloc::ArrayRef;

use super::*;
use crate::{
    expander::{
        core::{LVarRef, TermKind, TermRef, module_ref, module_set},
        letrectify::{capy_module, is_define_module_term},
        primitives::sym_current_module,
    },
    runtime::{Context, modules::resolve_module},
};

struct ComputeFreeVarResolver<'gc> {
    ctx: Context<'gc>,
    module_definitions: Vec<(Value<'gc>, ArrayRef<'gc, TermRef<'gc>>)>,
    module_lexicals: Vec<(Value<'gc>, LVarRef<'gc>)>,
    bindings: Vec<(Value<'gc>, Value<'gc>)>,
}

impl<'gc> ComputeFreeVarResolver<'gc> {
    pub fn new(ctx: Context<'gc>) -> Self {
        Self {
            ctx,
            module_definitions: Vec::new(),
            module_lexicals: Vec::new(),
            bindings: Vec::new(),
        }
    }

    fn add_module_definition(&mut self, module: Value<'gc>, args: ArrayRef<'gc, TermRef<'gc>>) {
        self.module_definitions.push((module, args));
    }

    fn add_module_lexical(&mut self, var: LVarRef<'gc>, module: Value<'gc>) {
        if !var.is_mutated() {
            self.module_lexicals.push((module, var));
        }
    }

    fn add_binding(&mut self, module: Value<'gc>, name: Value<'gc>) {
        self.bindings.push((module, name));
    }

    fn record_bindings(&mut self, module: Value<'gc>, lhs: &[LVarRef<'gc>], rhs: &[TermRef<'gc>]) {
        for (var, val) in lhs.iter().zip(rhs.iter()) {
            if let Some((module, args)) = is_define_module_term(self.ctx, *val) {
                self.add_module_definition(module, args);
                self.add_module_lexical(*var, module);
            } else if let TermKind::PrimCall(prim, args) = val.kind
                && prim == sym_current_module(self.ctx).into()
                && args.len() == 0
            {
                if module != Value::new(false) {
                    self.add_module_lexical(*var, module);
                }
            }
        }
    }

    fn visit_many(&mut self, exps: &[TermRef<'gc>], module: Value<'gc>) -> Value<'gc> {
        if exps.is_empty() {
            module
        } else {
            let exp = exps[0];
            let exps = &exps[1..];
            fn rec<'gc>(
                this: &mut ComputeFreeVarResolver<'gc>,
                module: Value<'gc>,
                exps: &[TermRef<'gc>],
            ) -> Value<'gc> {
                if exps.is_empty() {
                    module
                } else {
                    let nmod = this.visit_mod(exps[0], module);
                    if nmod.r5rs_equal(module) {
                        rec(this, module, &exps[1..])
                    } else {
                        rec(this, Value::new(false), &exps[1..])
                    }
                }
            }
            let module = self.visit_mod(exp, module);
            rec(self, module, exps)
        }
    }

    fn visit_mod(&mut self, exp: TermRef<'gc>, module: Value<'gc>) -> Value<'gc> {
        match exp.kind {
            TermKind::Const(_)
            | TermKind::PrimRef(_)
            | TermKind::LRef(_)
            | TermKind::ModuleRef(..)
            | TermKind::ToplevelRef(..) => module,
            TermKind::PrimCall(prim, args) => {
                if prim == sym_current_module(self.ctx).into()
                    && args.len() == 1
                    && let TermKind::LRef(var) = args[0].kind
                {
                    self.module_lexicals
                        .iter()
                        .find_map(|(modu, v)| if *v == var { Some(*modu) } else { None })
                        .unwrap_or(Value::new(false))
                } else {
                    self.visit_many(&args, module)
                }
            }

            TermKind::Call(proc, args) => {
                if let TermKind::ModuleRef(mod_name, var_name, _) = proc.kind
                    && mod_name.r5rs_equal(*capy_module(self.ctx))
                    && var_name == sym_current_module(self.ctx).into()
                    && args.len() == 1
                    && let TermKind::LRef(var) = args[0].kind
                {
                    return self
                        .module_lexicals
                        .iter()
                        .find_map(|(modu, v)| if *v == var { Some(*modu) } else { None })
                        .unwrap_or(Value::new(false));
                }
                self.visit(proc);
                self.visit_star(&args);
                Value::new(false)
            }

            TermKind::If(test, consequent, alternate) => {
                let mod1 = self.visit_mod(test, module);
                self.visit_many(&[consequent, alternate], mod1)
            }

            TermKind::LSet(_, val) => self.visit_mod(val, module),

            TermKind::ToplevelSet(_, _, val) => self.visit_mod(val, module),

            TermKind::ModuleSet(_, _, _, val) => self.visit_mod(val, module),

            TermKind::Define(module, name, exp) => {
                self.add_binding(module, name);
                self.visit_mod(exp, module)
            }

            TermKind::Proc(proc) => {
                self.visit(proc.body);
                module
            }

            TermKind::Seq(head, tail) => {
                let mod1 = self.visit_mod(head, module);
                self.visit_mod(tail, mod1)
            }

            TermKind::Let(l) => {
                self.record_bindings(module, &l.lhs, &l.rhs);
                let mod1 = self.visit_many(&l.rhs, module);
                self.visit_mod(l.body, mod1)
            }

            TermKind::Receive(_, _, producer, consumer) => {
                let mod1 = self.visit_mod(producer, module);
                self.visit_mod(consumer, mod1)
            }

            TermKind::Fix(_) => unreachable!(),
            TermKind::Values(values) => {
                self.visit_star(&values);
                Value::new(false)
            }
        }
    }

    fn visit(&mut self, exp: TermRef<'gc>) -> Value<'gc> {
        self.visit_mod(exp, Value::new(false))
    }

    fn visit_star(&mut self, exps: &[TermRef<'gc>]) {
        if !exps.is_empty() {
            self.visit(exps[0]);
            self.visit_star(&exps[1..]);
        }
    }

    fn compute(
        &mut self,
        exp: TermRef<'gc>,
    ) -> impl Fn(Context<'gc>, Value<'gc>, Value<'gc>) -> Resolved<'gc> {
        self.visit(exp);

        let mut declarative: Vec<Value> = Vec::new();
        let mut not_declarative: Vec<Value> = Vec::new();

        for (module, _args) in self.module_definitions.iter() {
            // println!(";; Module '{} definition", module);
            if not_declarative.iter().any(|m| m.r5rs_equal(*module)) {
                continue;
            } else if self
                .module_definitions
                .iter()
                .filter(|(m, _)| m.r5rs_equal(*module))
                .count()
                > 1
            {
                not_declarative.push(*module);
                continue;
            } else {
                declarative.push(*module);
            }
        }

        let resolvers = declarative
            .iter()
            .map(|&module| {
                let local_bindings = self
                    .bindings
                    .iter()
                    .filter_map(|(modu, name)| {
                        if modu.r5rs_equal(module) {
                            Some(*name)
                        } else {
                            None
                        }
                    })
                    .collect::<Vec<_>>();
                let module_ = resolve_module(self.ctx, module, false, false);
                let resolver = Resolver::new(module_, local_bindings);
                (module, resolver)
            })
            .collect::<Vec<_>>();
        move |ctx: Context<'gc>, module: Value<'gc>, name: Value<'gc>| {
            resolvers
                .iter()
                .find_map(|(modu, resolver)| {
                    if modu.r5rs_equal(module) {
                        Some(resolver.resolve(ctx, name))
                    } else {
                        None
                    }
                })
                .unwrap_or(Resolved::Unknown)
        }
    }
}

struct Resolver<'gc> {
    the_module: Option<Gc<'gc, Module<'gc>>>,
    resolvers: Vec<ImportedResolver<'gc>>,
    local_definitions: Vec<Value<'gc>>,
}

#[derive(Copy, Clone, PartialEq, Eq)]
enum Resolved<'gc> {
    Local,
    Unknown,
    Duplicate,
    Known(Value<'gc>, Value<'gc>),
}

impl<'gc> Resolver<'gc> {
    fn new(module: Option<Gc<'gc, Module<'gc>>>, local_definitions: Vec<Value<'gc>>) -> Self {
        let resolvers = if let Some(module) = module {
            let uses = module.uses.get();
            let mut ls = uses;
            let mut resolvers = Vec::new();
            while ls.is_pair() {
                let iface = ls.car().downcast::<Module>();
                resolvers.push(ImportedResolver::new(iface));
                ls = ls.cdr();
            }
            resolvers
        } else {
            Vec::new()
        };

        Self {
            the_module: module,
            resolvers,
            local_definitions,
        }
    }

    fn resolve(&self, ctx: Context<'gc>, name: Value<'gc>) -> Resolved<'gc> {
        if let Some(module) = self.the_module
            && module.local_variable(ctx, name).is_some()
        {
            return Resolved::Local;
        }

        if self.local_definitions.iter().any(|n| n.r5rs_equal(name)) {
            return Resolved::Local;
        }

        match self
            .resolvers
            .iter()
            .filter_map(|r| r.resolve(ctx, name))
            .collect::<Vec<_>>()
            .as_slice()
        {
            [] => Resolved::Unknown,
            [single] => Resolved::Known(single.0, single.1),
            _ => Resolved::Duplicate,
        }
    }
}

struct ImportedResolver<'gc> {
    iface: Gc<'gc, Module<'gc>>,
    by_var: HashMap<Value<'gc>, Value<'gc>>,
}

impl<'gc> ImportedResolver<'gc> {
    fn resolve(&self, ctx: Context<'gc>, name: Value<'gc>) -> Option<(Value<'gc>, Value<'gc>)> {
        let var: Value<'gc> = self.iface.variable(ctx, name)?.into();
        let name = self.by_var.get(&var)?;
        Some((self.iface.name.get(), *name))
    }

    fn new(iface: Gc<'gc, Module<'gc>>) -> Self {
        let mut by_var = HashMap::new();
        for (name, var) in iface.obarray.get().iter() {
            by_var.insert(var, name);
        }
        Self { iface, by_var }
    }
}

/// Extract module-leve definitions from `exp`.
pub fn resolve_free_vars<'gc>(ctx: Context<'gc>, exp: TermRef<'gc>) -> TermRef<'gc> {
    let mut computer = ComputeFreeVarResolver::new(ctx);
    let resolve = computer.compute(exp);

    exp.post_order(ctx, |ctx, exp| {
        let sourcev = exp.source();
        match exp.kind {
            TermKind::ToplevelRef(module, name) => match resolve(ctx, module, name) {
                Resolved::Local => {
                    // println!(";; Resolved local toplevel ref '{}'", name);
                    exp
                }
                Resolved::Duplicate => {
                    // println!(";; Duplicate toplevel ref '{}'", name);
                    exp
                }
                Resolved::Unknown => exp,
                Resolved::Known(mod_name, var_name) => {
                    // println!(";; Resolved free var '{}::{}'", mod_name, var_name);
                    module_ref(ctx, mod_name, var_name, true, sourcev)
                }
            },

            TermKind::ToplevelSet(module, name, value) => match resolve(ctx, module, name) {
                Resolved::Local | Resolved::Unknown | Resolved::Duplicate => exp,
                Resolved::Known(mod_name, var_name) => {
                    let value = value;
                    module_set(ctx, mod_name, var_name, true, value, sourcev)
                }
            },

            _ => exp,
        }
    })
}
