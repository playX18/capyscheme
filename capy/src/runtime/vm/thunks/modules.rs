use super::ThunkResult;
use crate::runtime::vm::exceptions::make_undefined_violation as undefined_violation;
use crate::runtime::{
    Context,
    modules::{Module, Variable},
    value::{
        Symbol, Value,
    },
    vm::debug::print_stacktraces_impl,
};


pub fn make_variable<'gc>(ctx: Context<'gc>,
        value: Value<'gc>) -> Value<'gc> {


        Variable::new(ctx, value).into()
    }

pub fn lookup_bound<'gc>(ctx: Context<'gc>,
        module: Value<'gc>,
        name: Value<'gc>) -> ThunkResult<'gc> {
        assert!(name.is::<Symbol>());
        if !module.is::<Module>() {
            unreachable!("lookup-bound: not a module: {}", module);
        }
        let variable = module.downcast::<Module>().variable(ctx, name);

        let Some(variable) = variable else {


            return ThunkResult {
                code: 1,
                value:
                    undefined_violation(ctx, Some(name), "variable not found", &[name, module]),
            };
        };

        if variable.get() == Value::undefined() {

            return ThunkResult {
                code: 1,
                value: undefined_violation(ctx, Some(name), &format!("variable not bound in module '{}'", module.downcast::<Module>().name.get()), &[name, module]),
            };
        }

        return ThunkResult {
            code: 0,
            value: variable.into(),
        };
    }

pub fn lookup<'gc>(ctx: Context<'gc>,
        module: Value<'gc>,
        name: Value<'gc>) -> ThunkResult<'gc> {
        let mut module = module;
        if module == Value::new(false) {
            module = crate::runtime::modules::current_module(ctx).get(ctx);
        }

        let variable = module.downcast::<Module>().variable(ctx, name);

        let Some(var) = variable else {
// SAFETY: Return address slot is valid — set up by the native calling convention
            let ret = unsafe { crate::runtime::vm::thunks::helpers::llvm_return_address() };

            return ThunkResult {
                code: 1,
                value: undefined_violation(ctx, Some(name), "variable not found", &[name, module]),
            };
        };

        return ThunkResult {
            code: 0,
            value: var.into(),
        };
    }

pub fn lookup_bound_public<'gc>(ctx: Context<'gc>,
        module: Value<'gc>,
        name: Value<'gc>) -> ThunkResult<'gc> {
        let module = super::resolve_module(ctx, module, true);
        if module.code != 0 {
            return module;
        }

        let var = lookup(ctx, module.value, name);
        if var.code != 0 {
            return var;
        }

        let var = var.value.downcast::<Variable>();
        if !var.is_bound() {

            return ThunkResult {
                code: 1,
                value: undefined_violation(ctx, Some(name), &format!("variable not bound in module '{}'", module.value.downcast::<Module>().name.get()), &[name, module.value]),
            };
        }

        ThunkResult { code: 0, value: var.into() }
    }

pub fn lookup_bound_private<'gc>(ctx: Context<'gc>,
        module: Value<'gc>,
        name: Value<'gc>) -> ThunkResult<'gc> {
        let module = super::resolve_module(ctx, module, false);
        if module.code != 0 {
            return module;
        }

        let var = lookup(ctx, module.value, name);
        if var.code != 0 {
            return var;
        }

        let var = var.value.downcast::<Variable>();
        if !var.is_bound() {
            return ThunkResult {
                code: 1,
                value: undefined_violation(ctx, Some(name), &format!("variable not bound in module '{}'", module.value.downcast::<Module>().name.get()), &[name, module.value]),
            };
        }

        ThunkResult { code: 0, value: var.into() }
    }

pub fn define<'gc>(ctx: Context<'gc>,
        name: Value<'gc>,
        value: Value<'gc>) -> Value<'gc> {
        let module = crate::runtime::modules::current_module(ctx).get(ctx).downcast::<Module>();

        module.define(ctx, name, value);

        Value::undefined()
    }

pub fn current_module<'gc>(ctx: Context<'gc>) -> Value<'gc> {
        let module = crate::runtime::modules::current_module(ctx).get(ctx);
        if !module.is::<Module>() {

            print_stacktraces_impl(ctx);
            panic!("current-module: not a module: {}", module);
        }
        module
    }

pub fn set_current_module<'gc>(ctx: Context<'gc>, module: Value<'gc>) -> Value<'gc> {
        if !module.is::<Module>() {
// SAFETY: Return address slot is valid — set up by the native calling convention
            let ret = unsafe { crate::runtime::vm::thunks::helpers::llvm_return_address() };
            backtrace::resolve(ret as _, |sym| {
                log::trace!("set-current-module: {module} ");
                log::trace!("{sym:?}");
            });
            log::trace!("set-current-module: not a module: {}", module);
            print_stacktraces_impl(ctx);
            panic!("set-current-module: not a module: {}", module);
        }
        crate::runtime::modules::set_current_module(ctx, module);
        Value::undefined()
    }

pub fn module_ensure_local_variable<'gc>(ctx: Context<'gc>, module: Value<'gc>, name: Value<'gc>) -> Value<'gc> {
        let module = module.downcast::<Module>();
        let variable = module.ensure_local_variable(ctx, name);
        variable.into()
    }
