//! CPS to S-expression conversion.
//!
//! This is mainly used to serialize the CPS IR for incremental compilation.

use std::{cell::Cell, collections::HashMap, sync::OnceLock};

use rsgc::{Global, Rootable, Trace, alloc::Array, cell::Lock, traits::IterGc};

use crate::{
    cps::term::*,
    expander::core::{LVar, LVarRef},
    runtime::{Context, value::*},
    vector,
};

const FUNC_NAME: usize = 0;
const FUNC_SOURCE: usize = 1;
const FUNC_BINDING: usize = 2;
const FUNC_RETURN_CONT: usize = 3;
const FUNC_ARGS: usize = 4;
const FUNC_VARIADIC: usize = 5;
const FUNC_BODY: usize = 6;
const FUNC_HANDLER_CONT: usize = 7;
const FUNC_FREE_VARS: usize = 8;

const CONT_NAME: usize = 0;
const CONT_BINDING: usize = 1;
const CONT_ARGS: usize = 2;
const CONT_VARIADIC: usize = 3;
const CONT_BODY: usize = 4;
const CONT_SOURCE: usize = 5;
const CONT_FREE_VARS: usize = 6;
const CONT_REIFIED: usize = 7;
const CONT_HANDLER: usize = 8;

#[derive(Trace)]
#[collect(no_drop)]
pub struct CPSSymbols<'gc> {
    pub continue_: Value<'gc>,
    pub raise: Value<'gc>,
    pub throw: Value<'gc>,
    pub throw_value: Value<'gc>,
    pub throw_value_and_data: Value<'gc>,
    pub fix: Value<'gc>,
    pub letk: Value<'gc>,
    pub let_: Value<'gc>,
    pub cont: Value<'gc>,
    pub func: Value<'gc>,
    pub cond: Value<'gc>,
    pub primcall: Value<'gc>,
    pub values_at: Value<'gc>,
    pub values_rest: Value<'gc>,
    pub app: Value<'gc>,
    pub global: Value<'gc>,
    pub lref: Value<'gc>,
    pub constant: Value<'gc>,
}

pub struct CPSSerializer<'gc> {
    pub ctx: Context<'gc>,
    pub symbols: &'gc CPSSymbols<'gc>,
    pub vars: HashMap<LVarRef<'gc>, Value<'gc>>,
}

impl<'gc> CPSSerializer<'gc> {
    pub fn new(ctx: Context<'gc>) -> Self {
        let symbols = cps_symbols(ctx);
        Self {
            ctx,
            symbols,
            vars: HashMap::new(),
        }
    }

    pub fn lvar(&mut self, lvar: LVarRef<'gc>) -> Value<'gc> {
        if let Some(value) = self.vars.get(&lvar) {
            *value
        } else {
            let value = Symbol::from_str_uninterned(&self.ctx, &lvar.name.to_string(), None).into();
            self.vars.insert(lvar, value);
            value
        }
    }

    pub fn atom(&mut self, atom: Atom<'gc>) -> Value<'gc> {
        match atom {
            Atom::Local(local) => {
                let lvar = self.lvar(local);

                Value::cons(self.ctx, self.symbols.lref, lvar)
            }

            Atom::Constant(constant) => Value::cons(self.ctx, self.symbols.constant, constant),

            _ => unreachable!(),
        }
    }

    pub fn expr(&mut self, expr: Expression<'gc>) -> Value<'gc> {
        match expr {
            Expression::PrimCall(prim, args, h, src) => {
                let atoms = args.iter().map(|a| self.atom(*a)).collect::<Vec<_>>();
                let args = Vector::from_slice(&self.ctx, &atoms);
                let h = self.lvar(h);
                vector!(self.ctx, self.symbols.primcall, prim, args, h, src).into()
            }
            _ => todo!(),
        }
    }

    pub fn cont(&mut self, k: ContRef<'gc>) -> Value<'gc> {
        match *k {
            Cont {
                name,
                binding,
                args,
                variadic,
                body,
                source,
                ..
            } => {
                let args = args.iter().map(|a| self.lvar(*a)).collect::<Vec<_>>();
                let args = Vector::from_slice(&self.ctx, &args);
                let body = self.term(body);
                let binding = self.lvar(binding);
                let variadic = variadic.map_or(Value::new(false), |v| self.lvar(v));
                let free_vars = k
                    .free_vars
                    .get()
                    .as_ref()
                    .map_or(Value::new(false), |vars| {
                        let vars = vars.iter().map(|&v| self.lvar(v)).collect::<Vec<_>>();
                        Vector::from_slice(&self.ctx, &vars).into()
                    });
                let reified = Value::new(k.reified.get());
                let handler = self.lvar(k.handler.get());
                vector!(
                    self.ctx,
                    self.symbols.cont,
                    name,
                    binding,
                    args,
                    variadic,
                    body,
                    source,
                    free_vars,
                    reified,
                    handler
                )
                .into()
            }
        }
    }

    pub fn func(&mut self, f: FuncRef<'gc>) -> Value<'gc> {
        let args = f.args.iter().map(|a| self.lvar(*a)).collect::<Vec<_>>();
        let args = Vector::from_slice(&self.ctx, &args);
        let body = self.term(f.body);
        let return_cont = self.lvar(f.return_cont());
        let variadic = f.variadic.map_or(Value::new(false), |v| self.lvar(v));
        let binding = self.lvar(f.binding());
        let handler = self.lvar(f.handler_cont);
        let free_vars = f
            .free_vars
            .get()
            .as_ref()
            .map_or(Value::new(false), |vars| {
                let vars = vars.iter().map(|&v| self.lvar(v)).collect::<Vec<_>>();
                Vector::from_slice(&self.ctx, &vars).into()
            });

        vector!(
            self.ctx,
            self.symbols.func,
            f.name,
            f.source,
            binding,
            return_cont,
            args,
            variadic,
            body,
            handler,
            free_vars
        )
        .into()
    }

    pub fn term(&mut self, term: TermRef<'gc>) -> Value<'gc> {
        match *term {
            Term::App(f, k, h, args, src) => {
                let args = args.iter().map(|a| self.atom(*a)).collect::<Vec<_>>();
                let args = Vector::from_slice(&self.ctx, &args);
                let f = self.atom(f);
                let k = self.lvar(k);
                let h = self.lvar(h);
                vector!(self.ctx, self.symbols.app, f, k, h, args, src).into()
            }

            Term::Continue(k, args, src) => {
                let args = args.iter().map(|a| self.atom(*a)).collect::<Vec<_>>();
                let args = Vector::from_slice(&self.ctx, &args);
                let k = self.lvar(k);
                vector!(self.ctx, self.symbols.continue_, k, args, src).into()
            }

            Term::If(test, kcons, kalt, _) => {
                let test = self.atom(test);
                let kcons = self.lvar(kcons);
                let kalt = self.lvar(kalt);
                vector!(self.ctx, self.symbols.cond, test, kcons, kalt).into()
            }

            Term::Fix(funs, body) => {
                let funs = funs.iter().map(|f| self.func(*f)).collect::<Vec<_>>();
                let funs = Vector::from_slice(&self.ctx, &funs);
                let body = self.term(body);
                vector!(self.ctx, self.symbols.fix, funs, body).into()
            }

            Term::Letk(conts, body) => {
                let conts = conts.iter().map(|c| self.cont(*c)).collect::<Vec<_>>();
                let conts = Vector::from_slice(&self.ctx, &conts);
                let body = self.term(body);
                vector!(self.ctx, self.symbols.letk, conts, body).into()
            }

            Term::Let(bind, expr, body) => {
                let bind = self.lvar(bind);
                let expr = self.expr(expr);
                let body = self.term(body);
                vector!(self.ctx, self.symbols.let_, bind, expr, body).into()
            }

            Term::Throw(throw, src) => match throw {
                Throw::Throw(key, value) => {
                    let key = self.atom(key);
                    let value = self.atom(value);
                    vector!(self.ctx, self.symbols.throw, key, value, src).into()
                }
                Throw::Value(key, value) => {
                    let key = self.atom(key);
                    let value = self.atom(value);
                    vector!(self.ctx, self.symbols.throw_value, key, value, src).into()
                }

                Throw::ValueAndData(value, data) => {
                    let value = self.atom(value);
                    let data = self.atom(data);
                    vector!(
                        self.ctx,
                        self.symbols.throw_value_and_data,
                        value,
                        data,
                        src
                    )
                    .into()
                }
            },
        }
    }
}

static CPS_SYMBOLS: OnceLock<Global<Rootable!(CPSSymbols<'_>)>> = OnceLock::new();

pub fn cps_symbols<'gc>(ctx: Context<'gc>) -> &'gc CPSSymbols<'gc> {
    CPS_SYMBOLS
        .get_or_init(|| {
            let continue_ = Symbol::from_str(ctx, "continue").into();
            let throw = Symbol::from_str(ctx, "throw").into();
            let throw_value = Symbol::from_str(ctx, "throw/value").into();
            let throw_value_and_data = Symbol::from_str(ctx, "throw/value+data").into();
            let fix = Symbol::from_str(ctx, "fix").into();
            let letk = Symbol::from_str(ctx, "letk").into();
            let let_ = Symbol::from_str(ctx, "let").into();
            let cont = Symbol::from_str(ctx, "cont").into();
            let func = Symbol::from_str(ctx, "func").into();
            let cond = Symbol::from_str(ctx, "cond").into();
            let primcall = Symbol::from_str(ctx, "primcall").into();
            let app = Symbol::from_str(ctx, "app").into();
            let global = Symbol::from_str(ctx, "global").into();
            let lref = Symbol::from_str(ctx, "lref").into();
            let constant = Symbol::from_str(ctx, "constant").into();

            Global::new(CPSSymbols {
                continue_,
                raise: Symbol::from_str(ctx, "raise").into(),
                values_at: Symbol::from_str(ctx, "values-ref").into(),
                values_rest: Symbol::from_str(ctx, "values&").into(),
                throw,
                throw_value,
                throw_value_and_data,
                fix,
                letk,
                let_,
                cont,
                func,
                cond,
                primcall,
                app,
                global,
                lref,
                constant,
            })
        })
        .fetch(&ctx)
}

pub struct CPSDeserializer<'gc> {
    pub ctx: Context<'gc>,
    pub symbols: &'gc CPSSymbols<'gc>,
    pub vars: HashMap<Value<'gc>, LVarRef<'gc>>,
}

impl<'gc> CPSDeserializer<'gc> {
    pub fn new(ctx: Context<'gc>) -> Self {
        let symbols = cps_symbols(ctx);
        Self {
            ctx,
            symbols,
            vars: HashMap::new(),
        }
    }

    pub fn lvar(&mut self, value: Value<'gc>) -> LVarRef<'gc> {
        if let Some(lvar) = self.vars.get(&value) {
            *lvar
        } else {
            let lvar = LVarRef::new(
                &self.ctx,
                LVar {
                    id: Value::new(false),
                    name: value,
                    set_count: Cell::new(0),
                    ref_count: Cell::new(0),
                },
            );
            self.vars.insert(value, lvar);
            lvar
        }
    }

    pub fn atom(&mut self, value: Value<'gc>) -> Atom<'gc> {
        let car = value.car();
        let cdr = value.cdr();
        if car == self.symbols.lref {
            let lvar = cdr;
            Atom::Local(self.lvar(lvar))
        } else if car == self.symbols.global {
            todo!()
        } else if car == self.symbols.constant {
            Atom::Constant(cdr)
        } else {
            panic!("Unexpected atom type: {:?}", value);
        }
    }

    pub fn expr(&mut self, value: Value<'gc>) -> Expression<'gc> {
        let vec = value.downcast::<Vector>();
        let vec = vec.as_slice();
        if vec[0] == self.symbols.primcall {
            let prim = vec[1];
            let args = vec[2].downcast::<Vector>();
            let h = self.lvar(vec[3]);
            let src = vec[4];
            let args = args.iter().map(|a| self.atom(a.get())).collect::<Vec<_>>();
            let args = Array::from_slice(&self.ctx, &args);
            Expression::PrimCall(prim, args, h, src)
        } else {
            panic!("Unexpected expression type: {:?}", value);
        }
    }

    pub fn cont(&mut self, value: Value<'gc>) -> ContRef<'gc> {
        let vec = value.downcast::<Vector>();
        let vec = vec.as_slice();
        if vec[0] == self.symbols.cont {
            let cont = &vec[1..];
            let name = cont[CONT_NAME];
            let binding = self.lvar(cont[CONT_BINDING]);
            let args = cont[CONT_ARGS].downcast::<Vector>();
            let args = args.iter().map(|a| self.lvar(a.get())).collect::<Vec<_>>();
            let args = Array::from_slice(&self.ctx, &args);
            let variadic = if cont[CONT_VARIADIC] == Value::new(false) {
                None
            } else {
                Some(self.lvar(cont[CONT_VARIADIC]))
            };
            let body = self.term(cont[CONT_BODY]);
            let source = cont[CONT_SOURCE];
            let free_vars = cont[CONT_FREE_VARS];
            let free_vars = if free_vars == Value::new(false) {
                None
            } else {
                let ctx = self.ctx;
                Some(
                    free_vars
                        .downcast::<Vector>()
                        .iter()
                        .map(|var| self.lvar(var.get()))
                        .collect_gc(&ctx),
                )
            };
            let reified = cont[CONT_REIFIED] != Value::new(false);
            let handler = self.lvar(cont[CONT_HANDLER]);

            ContRef::new(
                &self.ctx,
                Cont {
                    name,
                    binding,
                    args,
                    variadic,
                    body,
                    source,
                    free_vars: Lock::new(free_vars),
                    reified: Cell::new(reified),
                    handler: Lock::new(handler),
                    cold: false,
                },
            )
        } else {
            panic!("Unexpected continuation type: {:?}", value);
        }
    }

    pub fn func(&mut self, value: Value<'gc>) -> FuncRef<'gc> {
        let vec = value.downcast::<Vector>();
        let vec = vec.as_slice();
        if vec[0] == self.symbols.func {
            let func = &vec[1..];
            let name = func[FUNC_NAME];
            let source = func[FUNC_SOURCE];
            let binding = self.lvar(func[FUNC_BINDING]);
            let return_cont = self.lvar(func[FUNC_RETURN_CONT]);
            let args = func[FUNC_ARGS].downcast::<Vector>();
            let args = args.iter().map(|a| self.lvar(a.get())).collect::<Vec<_>>();
            let args = Array::from_slice(&self.ctx, &args);
            let variadic = if func[FUNC_VARIADIC] == Value::new(false) {
                None
            } else {
                Some(self.lvar(func[FUNC_VARIADIC]))
            };
            let body = self.term(func[FUNC_BODY]);
            let handler_cont = self.lvar(func[FUNC_HANDLER_CONT]);
            let free_vars = func[FUNC_FREE_VARS];

            let free_vars = if free_vars == Value::new(false) {
                None
            } else {
                let ctx = self.ctx;
                Some(
                    free_vars
                        .downcast::<Vector>()
                        .iter()
                        .map(|var| self.lvar(var.get()))
                        .collect_gc(&ctx),
                )
            };

            FuncRef::new(
                &self.ctx,
                Func {
                    name,
                    binding,
                    return_cont,
                    handler_cont,
                    args,
                    variadic,
                    body,
                    source,
                    free_vars: Lock::new(free_vars),
                },
            )
        } else {
            panic!("Unexpected function type: {:?}", value);
        }
    }

    pub fn term(&mut self, value: Value<'gc>) -> TermRef<'gc> {
        let vec = value.downcast::<Vector>();
        let vec = vec.as_slice();
        let term = match vec[0] {
            v if v == self.symbols.app => {
                let f = self.atom(vec[1]);
                let k = self.lvar(vec[2]);
                let h = self.lvar(vec[3]);
                let args = vec[4].downcast::<Vector>();
                let args = args.iter().map(|a| self.atom(a.get())).collect::<Vec<_>>();
                let args = Array::from_slice(&self.ctx, &args);
                Term::App(f, k, h, args, vec[5])
            }

            v if v == self.symbols.continue_ => {
                let k = self.lvar(vec[1]);
                let args = vec[2].downcast::<Vector>();
                let args = args.iter().map(|a| self.atom(a.get())).collect::<Vec<_>>();
                let args = Array::from_slice(&self.ctx, &args);
                Term::Continue(k, args, vec[3])
            }

            v if v == self.symbols.cond => {
                let test = self.atom(vec[1]);
                let kcons = self.lvar(vec[2]);
                let kalt = self.lvar(vec[3]);
                Term::If(test, kcons, kalt, [BranchHint::Normal; 2])
            }

            v if v == self.symbols.fix => {
                let funs = vec[1].downcast::<Vector>();
                let funs = funs.iter().map(|f| self.func(f.get())).collect::<Vec<_>>();
                let funs = Array::from_slice(&self.ctx, &funs);
                let body = self.term(vec[2]);
                Term::Fix(funs, body)
            }

            v if v == self.symbols.letk => {
                let conts = vec[1].downcast::<Vector>();
                let conts = conts.iter().map(|c| self.cont(c.get())).collect::<Vec<_>>();
                let conts = Array::from_slice(&self.ctx, &conts);
                let body = self.term(vec[2]);
                Term::Letk(conts, body)
            }

            v if v == self.symbols.let_ => {
                let bind = self.lvar(vec[1]);
                let expr = self.expr(vec[2]);
                let body = self.term(vec[3]);
                Term::Let(bind, expr, body)
            }

            v if v == self.symbols.throw => {
                let key = self.atom(vec[1]);
                let value = self.atom(vec[2]);
                Term::Throw(Throw::Throw(key, value), vec[3])
            }

            v if v == self.symbols.throw_value => {
                let key = self.atom(vec[1]);
                let value = self.atom(vec[2]);
                Term::Throw(Throw::Value(key, value), vec[3])
            }

            v if v == self.symbols.throw_value_and_data => {
                let value = self.atom(vec[1]);
                let data = self.atom(vec[2]);
                Term::Throw(Throw::ValueAndData(value, data), vec[3])
            }

            _ => panic!("Unexpected term type: {:?}", value),
        };

        TermRef::new(&self.ctx, term)
    }
}
