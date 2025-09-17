use std::{cell::Cell, collections::HashMap};

use rsgc::{alloc::Array, cell::Lock};

use crate::{
    expander::core::{self, LVar, LVarRef, Term, TermKind, TermRef},
    global, native_fn,
    runtime::{modules::root_module, prelude::*},
    static_symbols,
};

static_symbols!(
    SYM_TERM = "&term"
    SYM_LREF = "&lref"
    SYM_LSET = "&lset"
    SYM_MODULE_REF = "&module-ref"
    SYM_MODULE_SET = "&module-set"
    SYM_TOPLEVEL_REF = "&toplevel-ref"
    SYM_TOPLEVEL_SET = "&toplevel-set"
    SYM_TOPLEVEL_DEFINE = "&toplevel-define"
    SYM_IF = "&if"
    SYM_LET = "&let"
    SYM_FIX = "&fix"
    SYM_RECEIVE = "&receive"
    SYM_APPLICATION = "&application"
    SYM_PRIMCALL = "&primcall"
    SYM_PRIMREF = "&primref"
    SYM_CONSTANT = "&constant"
    SYM_VOID = "&void"
    SYM_VALUES = "&values"
    SYM_SEQUENCE = "&sequence"
    SYM_PROC = "&proc"

    SYM_NAME = "name"
    SYM_LET_ = "let"
    SYM_LET_STAR = "let*"
    SYM_LETREC = "letrec"
    SYM_LETREC_STAR = "letrec*"
);

global!(
    pub loc_term_type<'gc>: VariableRef<'gc> = (ctx) {
        root_module(ctx).variable(ctx, sym_term(ctx).into()).unwrap()
    };

    pub loc_lref_type<'gc>: VariableRef<'gc> = (ctx) {
        root_module(ctx).variable(ctx, sym_lref(ctx).into()).unwrap()
    };

    pub loc_lset_type<'gc>: VariableRef<'gc> = (ctx) {
        root_module(ctx).variable(ctx, sym_lset(ctx).into()).unwrap()
    };

    pub loc_let_type<'gc>: VariableRef<'gc> = (ctx) {
        root_module(ctx).variable(ctx, sym_let(ctx).into()).unwrap()
    };

    pub loc_module_ref_type<'gc>: VariableRef<'gc> = (ctx) {
        root_module(ctx).variable(ctx, sym_module_ref(ctx).into()).unwrap()
    };

    pub loc_module_set_type<'gc>: VariableRef<'gc> = (ctx) {
        root_module(ctx).variable(ctx, sym_module_set(ctx).into()).unwrap()
    };

    pub loc_toplevel_ref_type<'gc>: VariableRef<'gc> = (ctx) {
        root_module(ctx).variable(ctx, sym_toplevel_ref(ctx).into()).unwrap_or_else(|| {
            println!("Not found {} in root module", sym_toplevel_ref(ctx));
            for (var, _) in root_module(ctx).obarray.get().iter() {
                println!("  var: {var}");
            }
            panic!();
        })
    };

    pub loc_toplevel_set_type<'gc>: VariableRef<'gc> = (ctx) {

        root_module(ctx).variable(ctx, sym_toplevel_set(ctx).into()).unwrap()
    };

    pub loc_toplevel_define_type<'gc>: VariableRef<'gc> = (ctx) {
        root_module(ctx).variable(ctx, sym_toplevel_define(ctx).into()).unwrap()
    };

    pub loc_if_type<'gc>: VariableRef<'gc> = (ctx) {
        root_module(ctx).variable(ctx, sym_if(ctx).into()).unwrap()
    };

    pub loc_fix_type<'gc>: VariableRef<'gc> = (ctx) {
        root_module(ctx).variable(ctx, sym_fix(ctx).into()).unwrap()
    };

    pub loc_receive_type<'gc>: VariableRef<'gc> = (ctx) {
        root_module(ctx).variable(ctx, sym_receive(ctx).into()).unwrap()
    };

    pub loc_application_type<'gc>: VariableRef<'gc> = (ctx) {
        root_module(ctx).variable(ctx, sym_application(ctx).into()).unwrap()
    };

    pub loc_primcall_type<'gc>: VariableRef<'gc> = (ctx) {
        root_module(ctx).variable(ctx, sym_primcall(ctx).into()).unwrap()
    };

    pub loc_primref_type<'gc>: VariableRef<'gc> = (ctx) {
        root_module(ctx).variable(ctx, sym_primref(ctx).into()).unwrap()
    };

    pub loc_constant_type<'gc>: VariableRef<'gc> = (ctx) {
        root_module(ctx).variable(ctx, sym_constant(ctx).into()).unwrap()
    };

    pub loc_void_type<'gc>: VariableRef<'gc> = (ctx) {
        root_module(ctx).variable(ctx, sym_void(ctx).into()).unwrap()
    };

    pub loc_values_type<'gc>: VariableRef<'gc> = (ctx) {
        root_module(ctx).variable(ctx, sym_values(ctx).into()).unwrap()
    };

    pub loc_sequence_type<'gc>: VariableRef<'gc> = (ctx) {
        root_module(ctx).variable(ctx, sym_sequence(ctx).into()).unwrap()
    };

    pub loc_proc_type<'gc>: VariableRef<'gc> = (ctx) {
        root_module(ctx).variable(ctx, sym_proc(ctx).into()).unwrap()
    };
);

pub fn is_term<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> bool {
    v.is_record_of(ctx, loc_term_type(ctx).get().record_type_rtd(ctx))
}

pub fn term_sourcev<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Value<'gc> {
    assert!(is_term(ctx, v));
    v.downcast::<Tuple>()[1].get()
}

const TERM_SOURCEV: usize = 1;
const LREF_VARIABLE: usize = TERM_SOURCEV + 1;
const LREF_SYM: usize = TERM_SOURCEV + 2;

pub fn is_lref<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> bool {
    v.is_record_of(ctx, loc_lref_type(ctx).get().record_type_rtd(ctx))
}

pub fn lref_name<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Value<'gc> {
    assert!(is_lref(ctx, v));
    v.downcast::<Tuple>()[LREF_VARIABLE].get()
}

pub fn lref_sym<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Value<'gc> {
    assert!(is_lref(ctx, v));
    v.downcast::<Tuple>()[LREF_SYM].get()
}

const LSET_VARIABLE_: usize = LREF_SYM + 1;
const LSET_SYM: usize = LREF_SYM + 2;
const LSET_VALUE: usize = LREF_SYM + 3;

pub fn is_lset<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> bool {
    v.is_record_of(ctx, loc_lset_type(ctx).get().record_type_rtd(ctx))
}

pub fn lset_name<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Value<'gc> {
    assert!(is_lset(ctx, v));
    v.downcast::<Tuple>()[LSET_VARIABLE_].get()
}

pub fn lset_sym<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Value<'gc> {
    assert!(is_lset(ctx, v));
    v.downcast::<Tuple>()[LSET_SYM].get()
}

pub fn lset_value<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Value<'gc> {
    assert!(is_lset(ctx, v));
    v.downcast::<Tuple>()[LSET_VALUE].get()
}

pub fn is_module_ref<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> bool {
    v.is_record_of(ctx, loc_module_ref_type(ctx).get().record_type_rtd(ctx))
}

const MODULE_REF_MODULE: usize = TERM_SOURCEV + 1;
const MODULE_REF_NAME: usize = TERM_SOURCEV + 2;
const MODULE_REF_PUBLIC: usize = TERM_SOURCEV + 3;

pub fn module_ref_module<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Value<'gc> {
    assert!(is_module_ref(ctx, v));
    v.downcast::<Tuple>()[MODULE_REF_MODULE].get()
}

pub fn module_ref_name<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Value<'gc> {
    assert!(is_module_ref(ctx, v));
    v.downcast::<Tuple>()[MODULE_REF_NAME].get()
}

pub fn module_ref_public<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> bool {
    assert!(is_module_ref(ctx, v));
    v.downcast::<Tuple>()[MODULE_REF_PUBLIC].get() != Value::new(false)
}

pub fn is_module_set<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> bool {
    v.is_record_of(ctx, loc_module_set_type(ctx).get().record_type_rtd(ctx))
}

const MODULE_SET_MODULE: usize = TERM_SOURCEV + 1;
const MODULE_SET_NAME: usize = TERM_SOURCEV + 2;
const MODULE_SET_PUBLIC: usize = TERM_SOURCEV + 3;
const MODULE_SET_VALUE: usize = TERM_SOURCEV + 4;

pub fn module_set_module<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Value<'gc> {
    assert!(is_module_set(ctx, v));
    v.downcast::<Tuple>()[MODULE_SET_MODULE].get()
}

pub fn module_set_name<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Value<'gc> {
    assert!(is_module_set(ctx, v));
    v.downcast::<Tuple>()[MODULE_SET_NAME].get()
}

pub fn module_set_public<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> bool {
    assert!(is_module_set(ctx, v));
    v.downcast::<Tuple>()[MODULE_SET_PUBLIC].get() != Value::new(false)
}

pub fn module_set_value<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Value<'gc> {
    assert!(is_module_set(ctx, v));
    v.downcast::<Tuple>()[MODULE_SET_VALUE].get()
}

const TOPLEVEL_REF_MOD: usize = TERM_SOURCEV + 1;
const TOPLEVEL_REF_NAME: usize = TERM_SOURCEV + 2;

pub fn is_toplevel_ref<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> bool {
    v.is_record_of(ctx, loc_toplevel_ref_type(ctx).get().record_type_rtd(ctx))
}

pub fn toplevel_ref_mod<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Value<'gc> {
    assert!(is_toplevel_ref(ctx, v));
    v.downcast::<Tuple>()[TOPLEVEL_REF_MOD].get()
}

pub fn toplevel_ref_name<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Value<'gc> {
    assert!(is_toplevel_ref(ctx, v));
    v.downcast::<Tuple>()[TOPLEVEL_REF_NAME].get()
}

const TOPLEVEL_SET_MOD: usize = TERM_SOURCEV + 1;
const TOPLEVEL_SET_NAME: usize = TERM_SOURCEV + 2;
const TOPLEVEL_SET_VALUE: usize = TERM_SOURCEV + 3;

pub fn is_toplevel_set<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> bool {
    v.is_record_of(ctx, loc_toplevel_set_type(ctx).get().record_type_rtd(ctx))
}

pub fn toplevel_set_mod<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Value<'gc> {
    assert!(is_toplevel_set(ctx, v));
    v.downcast::<Tuple>()[TOPLEVEL_SET_MOD].get()
}

pub fn toplevel_set_name<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Value<'gc> {
    assert!(is_toplevel_set(ctx, v));
    v.downcast::<Tuple>()[TOPLEVEL_SET_NAME].get()
}

pub fn toplevel_set_value<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Value<'gc> {
    assert!(is_toplevel_set(ctx, v));
    v.downcast::<Tuple>()[TOPLEVEL_SET_VALUE].get()
}

const TOPLEVEL_DEFINE_MOD: usize = TERM_SOURCEV + 1;
const TOPLEVEL_DEFINE_NAME: usize = TERM_SOURCEV + 2;
const TOPLEVEL_DEFINE_VALUE: usize = TERM_SOURCEV + 3;

pub fn is_toplevel_define<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> bool {
    v.is_record_of(
        ctx,
        loc_toplevel_define_type(ctx).get().record_type_rtd(ctx),
    )
}

pub fn toplevel_define_mod<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Value<'gc> {
    assert!(is_toplevel_define(ctx, v));
    v.downcast::<Tuple>()[TOPLEVEL_DEFINE_MOD].get()
}

pub fn toplevel_define_name<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Value<'gc> {
    assert!(is_toplevel_define(ctx, v));
    v.downcast::<Tuple>()[TOPLEVEL_DEFINE_NAME].get()
}

pub fn toplevel_define_value<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Value<'gc> {
    assert!(is_toplevel_define(ctx, v));
    v.downcast::<Tuple>()[TOPLEVEL_DEFINE_VALUE].get()
}

const IF_COND: usize = TERM_SOURCEV + 1;
const IF_THEN: usize = TERM_SOURCEV + 2;
const IF_ELSE: usize = TERM_SOURCEV + 3;

pub fn is_if<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> bool {
    v.is_record_of(ctx, loc_if_type(ctx).get().record_type_rtd(ctx))
}

pub fn if_cond<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Value<'gc> {
    assert!(is_if(ctx, v));
    v.downcast::<Tuple>()[IF_COND].get()
}

pub fn if_then<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Value<'gc> {
    assert!(is_if(ctx, v));
    v.downcast::<Tuple>()[IF_THEN].get()
}

pub fn if_else<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Value<'gc> {
    assert!(is_if(ctx, v));
    v.downcast::<Tuple>()[IF_ELSE].get()
}

const LET_STYLE: usize = TERM_SOURCEV + 1;
const LET_IDS: usize = TERM_SOURCEV + 2;
const LET_LHS: usize = TERM_SOURCEV + 3;
const LET_RHS: usize = TERM_SOURCEV + 4;
const LET_BODY: usize = TERM_SOURCEV + 5;

pub fn is_let<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> bool {
    v.is_record_of(ctx, loc_let_type(ctx).get().record_type_rtd(ctx))
}

pub fn let_style<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Value<'gc> {
    assert!(is_let(ctx, v));
    v.downcast::<Tuple>()[LET_STYLE].get()
}

pub fn let_ids<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Value<'gc> {
    assert!(is_let(ctx, v));
    v.downcast::<Tuple>()[LET_IDS].get()
}

pub fn let_lhs<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Value<'gc> {
    assert!(is_let(ctx, v));
    v.downcast::<Tuple>()[LET_LHS].get()
}

pub fn let_rhs<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Value<'gc> {
    assert!(is_let(ctx, v));
    v.downcast::<Tuple>()[LET_RHS].get()
}

pub fn let_body<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Value<'gc> {
    assert!(is_let(ctx, v));
    v.downcast::<Tuple>()[LET_BODY].get()
}

const FIX_IDS: usize = TERM_SOURCEV + 1;
const FIX_LHS: usize = TERM_SOURCEV + 2;
const FIX_RHS: usize = TERM_SOURCEV + 3;
const FIX_BODY: usize = TERM_SOURCEV + 4;

pub fn is_fix<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> bool {
    v.is_record_of(ctx, loc_fix_type(ctx).get().record_type_rtd(ctx))
}

pub fn fix_ids<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Value<'gc> {
    assert!(is_fix(ctx, v));
    v.downcast::<Tuple>()[FIX_IDS].get()
}

pub fn fix_lhs<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Value<'gc> {
    assert!(is_fix(ctx, v));
    v.downcast::<Tuple>()[FIX_LHS].get()
}

pub fn fix_rhs<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Value<'gc> {
    assert!(is_fix(ctx, v));
    v.downcast::<Tuple>()[FIX_RHS].get()
}

pub fn fix_body<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Value<'gc> {
    assert!(is_fix(ctx, v));
    v.downcast::<Tuple>()[FIX_BODY].get()
}

pub fn is_receive<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> bool {
    v.is_record_of(ctx, loc_receive_type(ctx).get().record_type_rtd(ctx))
}

const RECEIVE_IDS: usize = TERM_SOURCEV + 1;
const RECEIVE_VARS: usize = TERM_SOURCEV + 2;
const RECEIVE_PRODUCER: usize = TERM_SOURCEV + 3;
const RECEIVE_CONSUMER: usize = TERM_SOURCEV + 4;

pub fn receive_ids<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Value<'gc> {
    assert!(is_receive(ctx, v));
    v.downcast::<Tuple>()[RECEIVE_IDS].get()
}

pub fn receive_vars<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Value<'gc> {
    assert!(is_receive(ctx, v));
    v.downcast::<Tuple>()[RECEIVE_VARS].get()
}

pub fn receive_producer<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Value<'gc> {
    assert!(is_receive(ctx, v));
    v.downcast::<Tuple>()[RECEIVE_PRODUCER].get()
}

pub fn receive_consumer<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Value<'gc> {
    assert!(is_receive(ctx, v));
    v.downcast::<Tuple>()[RECEIVE_CONSUMER].get()
}

pub fn is_application<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> bool {
    v.is_record_of(ctx, loc_application_type(ctx).get().record_type_rtd(ctx))
}

const APP_OPERATOR: usize = TERM_SOURCEV + 1;
const APP_OPERANDS: usize = TERM_SOURCEV + 2;

pub fn application_operator<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Value<'gc> {
    assert!(is_application(ctx, v));
    v.downcast::<Tuple>()[APP_OPERATOR].get()
}

pub fn application_operands<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Value<'gc> {
    assert!(is_application(ctx, v));
    v.downcast::<Tuple>()[APP_OPERANDS].get()
}

pub fn is_primcall<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> bool {
    v.is_record_of(ctx, loc_primcall_type(ctx).get().record_type_rtd(ctx))
}

const PRIMCALL_PRIM: usize = TERM_SOURCEV + 1;
const PRIMCALL_ARGS: usize = TERM_SOURCEV + 2;

pub fn primcall_prim<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Value<'gc> {
    assert!(is_primcall(ctx, v));
    v.downcast::<Tuple>()[PRIMCALL_PRIM].get()
}

pub fn primcall_args<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Value<'gc> {
    assert!(is_primcall(ctx, v));
    v.downcast::<Tuple>()[PRIMCALL_ARGS].get()
}

pub fn is_primref<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> bool {
    v.is_record_of(ctx, loc_primref_type(ctx).get().record_type_rtd(ctx))
}

const PRIMREF_NAME: usize = TERM_SOURCEV + 1;

pub fn primref_name<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Value<'gc> {
    assert!(is_primref(ctx, v));
    v.downcast::<Tuple>()[PRIMREF_NAME].get()
}

pub fn is_constant<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> bool {
    v.is_record_of(ctx, loc_constant_type(ctx).get().record_type_rtd(ctx))
}

const CONSTANT_VALUE: usize = TERM_SOURCEV + 1;

pub fn constant_value<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Value<'gc> {
    assert!(is_constant(ctx, v));
    v.downcast::<Tuple>()[CONSTANT_VALUE].get()
}

pub fn is_void<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> bool {
    v.is_record_of(ctx, loc_void_type(ctx).get().record_type_rtd(ctx))
}

pub fn is_values<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> bool {
    v.is_record_of(ctx, loc_values_type(ctx).get().record_type_rtd(ctx))
}

const VALUES_VALUES: usize = TERM_SOURCEV + 1;
pub fn values_values<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Value<'gc> {
    assert!(is_values(ctx, v));
    v.downcast::<Tuple>()[VALUES_VALUES].get()
}

const PROC_ARGS: usize = TERM_SOURCEV + 1;
const PROC_BODY: usize = TERM_SOURCEV + 2;
const PROC_META: usize = TERM_SOURCEV + 3;
const PROC_IDS: usize = TERM_SOURCEV + 4;

pub fn is_proc<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> bool {
    v.is_record_of(ctx, loc_proc_type(ctx).get().record_type_rtd(ctx))
}

pub fn proc_args<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Value<'gc> {
    assert!(is_proc(ctx, v));
    v.downcast::<Tuple>()[PROC_ARGS].get()
}

pub fn proc_body<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Value<'gc> {
    assert!(is_proc(ctx, v));
    v.downcast::<Tuple>()[PROC_BODY].get()
}

pub fn proc_meta<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Value<'gc> {
    assert!(is_proc(ctx, v));
    v.downcast::<Tuple>()[PROC_META].get()
}

pub fn proc_ids<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Value<'gc> {
    assert!(is_proc(ctx, v));
    v.downcast::<Tuple>()[PROC_IDS].get()
}

const SEQUENCE_HEAD: usize = TERM_SOURCEV + 1;
const SEQUENCE_TAIL: usize = TERM_SOURCEV + 2;

pub fn is_sequence<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> bool {
    v.is_record_of(ctx, loc_sequence_type(ctx).get().record_type_rtd(ctx))
}

pub fn sequence_head<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Value<'gc> {
    assert!(is_sequence(ctx, v));
    v.downcast::<Tuple>()[SEQUENCE_HEAD].get()
}

pub fn sequence_tail<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Value<'gc> {
    assert!(is_sequence(ctx, v));
    v.downcast::<Tuple>()[SEQUENCE_TAIL].get()
}

native_fn!(
    register_expand_fns:
    pub ("pretty-print-ir") fn pretty_print_ir<'gc>(nctx, t: Value<'gc>) -> () {

        let mut converter = ScmTermToRsTerm::new(nctx.ctx);
        let term = converter.convert(t).unwrap_or_else(|t| {
            panic!("not a term: {t}")
        });

        let doc = term.pretty::<_, &pretty::BoxAllocator>(&pretty::BoxAllocator);
        doc.1.render(80, &mut std::io::stdout()).unwrap();
        println!();

        nctx.return_(())
    }
);

pub fn init_expand<'gc>(ctx: Context<'gc>) {
    register_expand_fns(ctx);
}

pub(crate) struct ScmTermToRsTerm<'gc> {
    pub ctx: Context<'gc>,
    pub lvars: HashMap<Value<'gc>, LVarRef<'gc>>,
}

impl<'gc> ScmTermToRsTerm<'gc> {
    pub fn new(ctx: Context<'gc>) -> Self {
        Self {
            ctx,
            lvars: HashMap::new(),
        }
    }

    pub fn convert(&mut self, t: Value<'gc>) -> Result<TermRef<'gc>, Value<'gc>> {
        if !is_term(self.ctx, t) {
            return Err(t);
        }
        let source = term_sourcev(self.ctx, t);

        if is_lref(self.ctx, t) {
            let sym = lref_sym(self.ctx, t);
            let Some(var) = self.lvars.get(&sym) else {
                return Err(t);
            };
            return Ok(Gc::new(
                &self.ctx,
                Term {
                    source: Lock::new(source),
                    kind: TermKind::LRef(*var),
                },
            ));
        } else if is_lset(self.ctx, t) {
            let sym = lset_sym(self.ctx, t);
            let Some(var) = self.lvars.get(&sym).cloned() else {
                return Err(t);
            };
            let value = lset_value(self.ctx, t);
            let value = self.convert(value)?;
            return Ok(Gc::new(
                &self.ctx,
                Term {
                    source: Lock::new(source),
                    kind: TermKind::LSet(var, value),
                },
            ));
        } else if is_constant(self.ctx, t) {
            let value = constant_value(self.ctx, t);
            return Ok(Gc::new(
                &self.ctx,
                Term {
                    source: Lock::new(source),
                    kind: TermKind::Const(value),
                },
            ));
        } else if is_void(self.ctx, t) {
            return Ok(Gc::new(
                &self.ctx,
                Term {
                    source: Lock::new(source),
                    kind: TermKind::Const(Value::undefined()),
                },
            ));
        } else if is_application(self.ctx, t) {
            let op = self.convert(application_operator(self.ctx, t))?;
            let mut ls = application_operands(self.ctx, t);
            let len = ls.list_length();
            let mut args = Vec::with_capacity(len);

            while ls.is_pair() {
                let arg = self.convert(ls.car()).map_err(|_| t)?;
                args.push(arg);
                ls = ls.cdr();
            }

            let args = Array::from_slice(&self.ctx, &args);

            return Ok(Gc::new(
                &self.ctx,
                Term {
                    source: Lock::new(source),
                    kind: TermKind::Call(op, args),
                },
            ));
        } else if is_primcall(self.ctx, t) {
            let prim = primcall_prim(self.ctx, t);
            let mut ls = primcall_args(self.ctx, t);
            let len = ls.list_length();
            let mut args = Vec::with_capacity(len);
            while ls.is_pair() {
                let arg = self.convert(ls.car()).map_err(|_| t)?;
                args.push(arg);
                ls = ls.cdr();
            }
            let args = Array::from_slice(&self.ctx, &args);
            return Ok(Gc::new(
                &self.ctx,
                Term {
                    source: Lock::new(source),
                    kind: TermKind::PrimCall(prim, args),
                },
            ));
        } else if is_primref(self.ctx, t) {
            let name = primref_name(self.ctx, t);
            return Ok(Gc::new(
                &self.ctx,
                Term {
                    source: Lock::new(source),
                    kind: TermKind::PrimRef(name),
                },
            ));
        } else if is_module_ref(self.ctx, t) {
            let module = module_ref_module(self.ctx, t);
            let name = module_ref_name(self.ctx, t);
            let public = module_ref_public(self.ctx, t);
            return Ok(Gc::new(
                &self.ctx,
                Term {
                    source: Lock::new(source),
                    kind: TermKind::ModuleRef(module, name, public),
                },
            ));
        } else if is_module_set(self.ctx, t) {
            let module = module_set_module(self.ctx, t);
            let name = module_set_name(self.ctx, t);
            let public = module_set_public(self.ctx, t);
            let value = self.convert(module_set_value(self.ctx, t)).map_err(|_| t)?;
            return Ok(Gc::new(
                &self.ctx,
                Term {
                    source: Lock::new(source),
                    kind: TermKind::ModuleSet(module, name, public, value),
                },
            ));
        } else if is_toplevel_ref(self.ctx, t) {
            let module = toplevel_ref_mod(self.ctx, t);
            let name = toplevel_ref_name(self.ctx, t);
            return Ok(Gc::new(
                &self.ctx,
                Term {
                    source: Lock::new(source),
                    kind: TermKind::ToplevelRef(module, name),
                },
            ));
        } else if is_toplevel_set(self.ctx, t) {
            let module = toplevel_set_mod(self.ctx, t);
            let name = toplevel_set_name(self.ctx, t);
            let value = self
                .convert(toplevel_set_value(self.ctx, t))
                .map_err(|_| t)?;
            return Ok(Gc::new(
                &self.ctx,
                Term {
                    source: Lock::new(source),
                    kind: TermKind::ToplevelSet(module, name, value),
                },
            ));
        } else if is_toplevel_define(self.ctx, t) {
            let module = toplevel_define_mod(self.ctx, t);
            let name = toplevel_define_name(self.ctx, t);
            let value = self
                .convert(toplevel_define_value(self.ctx, t))
                .map_err(|_| t)?;
            return Ok(Gc::new(
                &self.ctx,
                Term {
                    source: Lock::new(source),
                    kind: TermKind::Define(module, name, value),
                },
            ));
        } else if is_if(self.ctx, t) {
            let cond = self.convert(if_cond(self.ctx, t)).map_err(|_| t)?;
            let then = self.convert(if_then(self.ctx, t)).map_err(|_| t)?;
            let els = self.convert(if_else(self.ctx, t)).map_err(|_| t)?;
            return Ok(Gc::new(
                &self.ctx,
                Term {
                    source: Lock::new(source),
                    kind: TermKind::If(cond, then, els),
                },
            ));
        } else if is_let(self.ctx, t) {
            let style = match let_style(self.ctx, t) {
                v if v == sym_let_(self.ctx).into() => core::LetStyle::Let,
                v if v == sym_let_star(self.ctx).into() => core::LetStyle::LetStar,
                v if v == sym_letrec(self.ctx).into() => core::LetStyle::LetRec,
                v if v == sym_letrec_star(self.ctx).into() => core::LetStyle::LetRecStar,
                _ => return Err(t),
            };
            let ids = let_ids(self.ctx, t);
            let lhs = let_lhs(self.ctx, t);
            let rhs = let_rhs(self.ctx, t);
            let body = let_body(self.ctx, t);

            let mut ls_ids = ids;
            let mut ls_lhs = lhs;

            let mut lvars = Vec::new();
            let mut exprs = Vec::new();
            while ls_ids.is_pair() {
                let id = ls_ids.car();
                let lhs = ls_lhs.car();

                let lvar = Gc::new(
                    &self.ctx,
                    LVar {
                        name: id,
                        id: lhs,
                        set_count: Cell::new(0),
                        ref_count: Cell::new(0),
                    },
                );
                self.lvars.insert(id, lvar);
                lvars.push(lvar);
                exprs.push(rhs);
                ls_ids = ls_ids.cdr();
                ls_lhs = ls_lhs.cdr();
            }

            let mut rhs = Vec::new();
            for expr in exprs {
                let expr = self.convert(expr).map_err(|_| t)?;
                rhs.push(expr);
            }
            let lhs = Array::from_slice(&self.ctx, &lvars);
            let rhs = Array::from_slice(&self.ctx, &rhs);
            let body = self.convert(body).map_err(|_| t)?;

            return Ok(Gc::new(
                &self.ctx,
                Term {
                    source: Lock::new(source),
                    kind: TermKind::Let(core::Let {
                        style,
                        lhs,
                        rhs,
                        body,
                    }),
                },
            ));
        } else if is_fix(self.ctx, t) {
            let ids = fix_ids(self.ctx, t);
            let lhs = fix_lhs(self.ctx, t);
            let rhs = fix_rhs(self.ctx, t);
            let body = fix_body(self.ctx, t);

            let mut ls_ids = ids;
            let mut ls_lhs = lhs;

            let mut lvars = Vec::new();
            let mut exprs = Vec::new();
            while ls_ids.is_pair() {
                let id = ls_ids.car();
                let lhs = ls_lhs.car();

                let lvar = Gc::new(
                    &self.ctx,
                    LVar {
                        name: id,
                        id: lhs,
                        set_count: Cell::new(0),
                        ref_count: Cell::new(0),
                    },
                );
                self.lvars.insert(lhs, lvar);
                lvars.push(lvar);
                exprs.push(rhs);
                ls_ids = ls_ids.cdr();
                ls_lhs = ls_lhs.cdr();
            }

            let mut rhs = Vec::new();
            for expr in exprs {
                let TermKind::Proc(proc) = self.convert(expr).map_err(|_| t)?.kind else {
                    return Err(t);
                };

                rhs.push(proc);
            }
            let lhs = Array::from_slice(&self.ctx, &lvars);
            let rhs = Array::from_slice(&self.ctx, &rhs);
            let body = self.convert(body).map_err(|_| t)?;

            return Ok(Gc::new(
                &self.ctx,
                Term {
                    source: Lock::new(source),
                    kind: TermKind::Fix(core::Fix { lhs, rhs, body }),
                },
            ));
        } else if is_proc(self.ctx, t) {
            let args = proc_args(self.ctx, t);
            let body = proc_body(self.ctx, t);
            let meta = proc_meta(self.ctx, t);
            let ids = proc_ids(self.ctx, t);

            let name = meta
                .assq(sym_name(self.ctx).into())
                .unwrap_or(Value::new(false));

            let mut ls_args = args;
            let mut ls_ids = ids;
            let mut lvars = Vec::new();

            while ls_args.is_pair() {
                let arg = ls_args.car();
                let id = ls_ids.car();

                let lvar = Gc::new(
                    &self.ctx,
                    LVar {
                        name: if name != Value::new(false) { name } else { arg },
                        id,
                        set_count: Cell::new(0),
                        ref_count: Cell::new(0),
                    },
                );
                self.lvars.insert(arg, lvar);
                lvars.push(lvar);
                ls_args = ls_args.cdr();
                ls_ids = ls_ids.cdr();
            }

            let variadic = if !ls_args.is_null() {
                let arg = ls_args;
                let id = ls_ids.car();
                let lvar = Gc::new(
                    &self.ctx,
                    LVar {
                        name: id,
                        id: arg,
                        set_count: Cell::new(0),
                        ref_count: Cell::new(0),
                    },
                );
                self.lvars.insert(arg, lvar);
                Some(lvar)
            } else {
                None
            };

            let body = self.convert(body).map_err(|_| t)?;

            return Ok(Gc::new(
                &self.ctx,
                Term {
                    source: Lock::new(source),
                    kind: TermKind::Proc(Gc::new(
                        &self.ctx,
                        core::Proc {
                            source,
                            args: Array::from_slice(&self.ctx, &lvars),
                            variadic,
                            body,
                            name,
                        },
                    )),
                },
            ));
        } else if is_receive(self.ctx, t) {
            let mut ids = receive_ids(self.ctx, t);
            let mut vars = receive_vars(self.ctx, t);
            let producer = receive_producer(self.ctx, t);
            let consumer = receive_consumer(self.ctx, t);

            let mut lvars = Vec::new();
            while ids.is_pair() {
                let id = ids.car();
                let var = vars.car();
                let lvar = Gc::new(
                    &self.ctx,
                    LVar {
                        name: id,
                        id: var,
                        set_count: Cell::new(0),
                        ref_count: Cell::new(0),
                    },
                );
                self.lvars.insert(var, lvar);
                lvars.push(lvar);
                ids = ids.cdr();
                vars = vars.cdr();
            }

            let variadic = if !ids.is_null() {
                let id = ids.car();
                let var = vars.car();
                let lvar = Gc::new(
                    &self.ctx,
                    LVar {
                        name: id,
                        id: var,
                        set_count: Cell::new(0),
                        ref_count: Cell::new(0),
                    },
                );
                self.lvars.insert(var, lvar);
                Some(lvar)
            } else {
                None
            };

            let producer = self.convert(producer).map_err(|_| t)?;
            let consumer = self.convert(consumer).map_err(|_| t)?;

            return Ok(Gc::new(
                &self.ctx,
                Term {
                    source: Lock::new(source),
                    kind: TermKind::Receive(
                        Array::from_slice(&self.ctx, &lvars),
                        variadic,
                        producer,
                        consumer,
                    ),
                },
            ));
        } else if is_values(self.ctx, t) {
            let mut ls = values_values(self.ctx, t);
            let len = ls.list_length();
            let mut vals = Vec::with_capacity(len);
            while ls.is_pair() {
                let v = self.convert(ls.car()).map_err(|_| t)?;
                vals.push(v);
                ls = ls.cdr();
            }
            return Ok(Gc::new(
                &self.ctx,
                Term {
                    source: Lock::new(source),
                    kind: TermKind::Values(Array::from_slice(&self.ctx, &vals)),
                },
            ));
        } else if is_sequence(self.ctx, t) {
            let mut seq = Vec::new();
            seq.push(self.convert(sequence_head(self.ctx, t)).map_err(|_| t)?);

            let mut tail = sequence_tail(self.ctx, t);
            while is_sequence(self.ctx, tail) {
                seq.push(self.convert(sequence_head(self.ctx, tail)).map_err(|_| t)?);
                tail = sequence_tail(self.ctx, tail);
            }
            seq.push(self.convert(tail).map_err(|_| t)?);
            let seq = Array::from_slice(&self.ctx, &seq);
            return Ok(Gc::new(
                &self.ctx,
                Term {
                    source: Lock::new(source),
                    kind: TermKind::Seq(seq),
                },
            ));
        } else {
            todo!()
        }
    }
}
