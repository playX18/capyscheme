use crate::{global, runtime::{modules::root_module, prelude::*}, static_symbols};

static_symbols!(
    SYM_TERM = "&term"
    SYM_LREF = "&lref"
    SYM_LSET = "&lset"
    SYM_MODULE_REF = "&module-ref"
    SYM_MODULE_SET = "&module-set"
    SYM_TOPLEVEL_REF = "&tolevel-ref"
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
);

global!(
    pub loc_term_rtd<'gc>: VariableRef<'gc> = (ctx) {
        root_module(ctx).variable(ctx, sym_term(ctx).into()).unwrap()
    };

    pub loc_lref_rtd<'gc>: VariableRef<'gc> = (ctx) {
        root_module(ctx).variable(ctx, sym_lref(ctx).into()).unwrap()
    };

    pub loc_lset_rtd<'gc>: VariableRef<'gc> = (ctx) {
        root_module(ctx).variable(ctx, sym_lset(ctx).into()).unwrap()
    };

    pub loc_let_rtd<'gc>: VariableRef<'gc> = (ctx) {
        root_module(ctx).variable(ctx, sym_let(ctx).into()).unwrap()
    };

    pub loc_module_ref_rtd<'gc>: VariableRef<'gc> = (ctx) {
        root_module(ctx).variable(ctx, sym_module_ref(ctx).into()).unwrap()
    };

    pub loc_module_set_rtd<'gc>: VariableRef<'gc> = (ctx) {
        root_module(ctx).variable(ctx, sym_module_set(ctx).into()).unwrap()
    };

    pub loc_toplevel_ref_rtd<'gc>: VariableRef<'gc> = (ctx) {
        root_module(ctx).variable(ctx, sym_toplevel_ref(ctx).into()).unwrap()
    };

    pub loc_toplevel_set_rtd<'gc>: VariableRef<'gc> = (ctx) {
        root_module(ctx).variable(ctx, sym_toplevel_set(ctx).into()).unwrap()
    };

    pub loc_toplevel_define_rtd<'gc>: VariableRef<'gc> = (ctx) {
        root_module(ctx).variable(ctx, sym_toplevel_define(ctx).into()).unwrap()
    };

    pub loc_if_rtd<'gc>: VariableRef<'gc> = (ctx) {
        root_module(ctx).variable(ctx, sym_if(ctx).into()).unwrap()
    };

    pub loc_fix_rtd<'gc>: VariableRef<'gc> = (ctx) {
        root_module(ctx).variable(ctx, sym_fix(ctx).into()).unwrap()
    };

    pub loc_receive_rtd<'gc>: VariableRef<'gc> = (ctx) {
        root_module(ctx).variable(ctx, sym_receive(ctx).into()).unwrap()
    };

    pub loc_application_rtd<'gc>: VariableRef<'gc> = (ctx) {
        root_module(ctx).variable(ctx, sym_application(ctx).into()).unwrap()
    };

    pub loc_primcall_rtd<'gc>: VariableRef<'gc> = (ctx) {
        root_module(ctx).variable(ctx, sym_primcall(ctx).into()).unwrap()
    };

    pub loc_primref_rtd<'gc>: VariableRef<'gc> = (ctx) {
        root_module(ctx).variable(ctx, sym_primref(ctx).into()).unwrap()
    };

    pub loc_constant_rtd<'gc>: VariableRef<'gc> = (ctx) {
        root_module(ctx).variable(ctx, sym_constant(ctx).into()).unwrap()
    };

    pub loc_void_rtd<'gc>: VariableRef<'gc> = (ctx) {
        root_module(ctx).variable(ctx, sym_void(ctx).into()).unwrap()
    };

    pub loc_values_rtd<'gc>: VariableRef<'gc> = (ctx) {
        root_module(ctx).variable(ctx, sym_values(ctx).into()).unwrap()
    };

    pub loc_sequence_rtd<'gc>: VariableRef<'gc> = (ctx) {
        root_module(ctx).variable(ctx, sym_sequence(ctx).into()).unwrap()
    };
);

