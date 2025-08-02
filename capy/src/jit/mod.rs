use std::collections::HashMap;

use cranelift_codegen::ir::{self, Function};
use cranelift_module::{DataDescription, DataId, FuncId, Linkage, Module};

use crate::{
    cps::{
        ReifyInfo,
        term::{ContRef, FuncRef},
    },
    runtime::{Context, value::Value},
};

pub mod lower_cps;
pub mod ssa;

pub struct JitContext<'gc> {
    pub ctx: Context<'gc>,
    pub module: cranelift_jit::JITModule,

    pub reified_contmap: HashMap<ContRef<'gc>, FuncId>,
    pub funcmap: HashMap<FuncRef<'gc>, FuncId>,
    pub constmap: HashMap<Value<'gc>, DataId>,
    pub globmap: HashMap<Value<'gc>, DataId>,

    pub reify_info: ReifyInfo<'gc>,

    pub thunks: Thunks,
}

impl<'gc> JitContext<'gc> {
    pub fn constant_data(&mut self, c: Value<'gc>) -> DataId {
        if let Some(id) = self.constmap.get(&c) {
            return *id;
        }
        let size = self.constmap.len();
        let id = self
            .module
            .declare_data(&format!("const_{size}"), Linkage::Local, false, false)
            .expect("failed to declare data");
        let mut desc = DataDescription::new();
        desc.define(Box::new(c.bits().to_le_bytes()));

        self.module
            .define_data(id, &desc)
            .expect("failed to define data");

        id
    }

    pub fn constant_in_func(&mut self, func: &mut Function, c: Value<'gc>) -> ir::GlobalValue {
        let id = self.constant_data(c);
        let data = self.module.declare_data_in_func(id, func);
        data
    }

    pub fn global_in_func(&mut self, func: &mut Function, name: Value<'gc>) -> ir::GlobalValue {
        if let Some(id) = self.globmap.get(&name) {
            return self.module.declare_data_in_func(*id, func);
        }
        let ix = self.globmap.len();
        let id = self
            .module
            .declare_data(&format!("global_{name}_{ix}"), Linkage::Local, true, false)
            .expect("failed to declare data");
        let global = self.ctx.global_location(name);
        let mut desc = DataDescription::new();
        desc.define(Box::new(global.bits().to_le_bytes()));

        let data = self.module.declare_data_in_func(id, func);
        self.globmap.insert(name, id);
        data
    }
}

pub struct Thunks {
    pub call_func: FuncId,
    pub argument_mismatch: FuncId,
    pub cons_rest: FuncId,
    pub alloc_closure: FuncId,
    pub alloc_closure_k: FuncId,
}
