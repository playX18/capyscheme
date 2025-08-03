use cranelift::prelude::{FunctionBuilderContext, Signature};
use cranelift_codegen::{
    ir::{self, Function},
    isa::CallConv,
};
use cranelift_jit::JITModule;
use cranelift_module::{DataDescription, DataId, FuncId, Linkage, Module, default_libcall_names};
use rsgc::{Global, Rootable, Trace};
use std::{
    collections::HashMap,
    marker::PhantomData,
    sync::{Mutex, OnceLock},
};

use crate::{
    cps::{
        ReifyInfo,
        term::{ContRef, FuncRef},
    },
    runtime::{Context, value::Value},
};

pub mod lower_cps;
pub mod ssa;
pub mod thunks;
pub mod trampoline;

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
    pub fn new(ctx: Context<'gc>, reify_info: ReifyInfo<'gc>) -> Self {
        let jit_builder = cranelift_jit::JITBuilder::with_flags(
            &[("opt_level", "speed"), ("preserve_frame_pointers", "true")],
            default_libcall_names(),
        )
        .unwrap();

        let mut module = JITModule::new(jit_builder);

        let mut alloc_closure_sig = Signature::new(CallConv::SystemV);
        alloc_closure_sig
            .params
            .push(ir::AbiParam::new(ir::types::I64)); // mc
        alloc_closure_sig
            .params
            .push(ir::AbiParam::new(ir::types::I64)); // state
        alloc_closure_sig
            .params
            .push(ir::AbiParam::new(ir::types::I64)); // nfree
        alloc_closure_sig
            .params
            .push(ir::AbiParam::new(ir::types::I64)); // meta
        alloc_closure_sig
            .returns
            .push(ir::AbiParam::new(ir::types::I64)); // closure

        let alloc_closure = module
            .declare_function("alloc_closure", Linkage::Import, &alloc_closure_sig)
            .expect("failed to declare alloc_closure function");
        let alloc_closure_k = module
            .declare_function("alloc_closure_k", Linkage::Import, &alloc_closure_sig)
            .expect("failed to declare alloc_closure_k function");

        let mut cons_rest_sig = Signature::new(CallConv::SystemV);
        cons_rest_sig.params.push(ir::AbiParam::new(ir::types::I64)); // mc
        cons_rest_sig.params.push(ir::AbiParam::new(ir::types::I64)); // state
        cons_rest_sig.params.push(ir::AbiParam::new(ir::types::I64)); // from
        cons_rest_sig
            .returns
            .push(ir::AbiParam::new(ir::types::I64)); // list
        let cons_rest = module
            .declare_function("cons_rest", Linkage::Import, &cons_rest_sig)
            .expect("failed to declare cons_rest function");

        Self {
            ctx,

            module,

            reified_contmap: HashMap::new(),
            funcmap: HashMap::new(),
            constmap: HashMap::new(),
            globmap: HashMap::new(),

            reify_info,

            thunks: Thunks {
                argument_mismatch: FuncId::from_u32(0),
                cons_rest,
                alloc_closure,
                alloc_closure_k,
            },
        }
    }

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

    /// Lower all functions and continuations in the context to SSA form.
    ///
    /// TODO: Lazy compilation
    pub fn lower_all(&mut self, entrypoint: FuncRef<'gc>) -> *const u8 {
        let mut fsig = Signature::new(CallConv::Tail);
        fsig.params.push(ir::AbiParam::new(ir::types::I64));
        fsig.params.push(ir::AbiParam::new(ir::types::I64));
        fsig.params.push(ir::AbiParam::new(ir::types::I64)); // continuation

        fsig.returns.push(ir::AbiParam::new(ir::types::I8));
        fsig.returns.push(ir::AbiParam::new(ir::types::I64));

        let mut csig = Signature::new(CallConv::Tail);
        csig.params.push(ir::AbiParam::new(ir::types::I64));
        csig.params.push(ir::AbiParam::new(ir::types::I64));

        csig.returns.push(ir::AbiParam::new(ir::types::I8));
        csig.returns.push(ir::AbiParam::new(ir::types::I64));

        let mut conts_to_lower = Vec::new();
        let mut funcs_to_lower = Vec::new();

        for (_, cont) in self.reify_info.free_vars.conts.iter() {
            let contmap_size = self.reified_contmap.len();
            let name = format!("cont_{contmap_size}'{}", cont.name());
            let id = self
                .module
                .declare_function(&name, Linkage::Local, &csig)
                .expect("failed to declare continuation function");
            self.reified_contmap.insert(*cont, id);

            conts_to_lower.push(*cont);
        }

        for (_, func) in self.reify_info.free_vars.funcs.iter() {
            let funcmap_size = self.funcmap.len();
            let name = format!("func_{funcmap_size}'{}", func.name());
            let id = self
                .module
                .declare_function(&name, Linkage::Local, &fsig)
                .expect("failed to declare function");
            self.funcmap.insert(*func, id);

            funcs_to_lower.push(*func);
        }

        let mut fctx = FunctionBuilderContext::new();
        let mut cctx = self.module.make_context();

        println!(
            "Lowering {} continuations and {} functions",
            conts_to_lower.len(),
            funcs_to_lower.len()
        );
        while let Some(cont) = conts_to_lower.pop() {
            self.lower_cont(cont, &mut fctx, &mut cctx);
        }

        while let Some(func) = funcs_to_lower.pop() {
            self.lower_func(func, &mut fctx, &mut cctx);
        }

        self.module.finalize_definitions().unwrap();

        let entry_id = self.funcmap[&entrypoint];
        let addr = self.module.get_finalized_function(entry_id);
        addr
    }

    pub fn lower_cont(
        &mut self,
        cont: ContRef<'gc>,
        fctx: &mut FunctionBuilderContext,
        cctx: &mut cranelift_codegen::Context,
    ) {
        let id = self.reified_contmap[&cont];

        let sig = self
            .module
            .declarations()
            .get_function_decl(id)
            .signature
            .clone();
        cctx.func.signature = sig;

        let self_ref = cont.binding();
        let ctx = self.ctx;
        cctx.set_disasm(true);
        ssa::translate(
            self,
            self_ref,
            ctx,
            cont.name(),
            None,
            &cont.args(),
            cont.variadic(),
            cont.body().expect("BUG: Continuation without body"),
            id,
            fctx,
            cctx,
        );

        println!(
            "Lowered continuation '{}':\n{}",
            cont.name(),
            cctx.func.display()
        );

        self.module
            .define_function(id, cctx)
            .expect("failed to define function");
        println!(
            "lowered continuation '{}':\n{}",
            cont.name(),
            cctx.func.display()
        );
        cctx.clear();
    }

    pub fn lower_func(
        &mut self,
        func: FuncRef<'gc>,
        fctx: &mut FunctionBuilderContext,
        cctx: &mut cranelift_codegen::Context,
    ) {
        let id = self.funcmap[&func];

        let sig = self
            .module
            .declarations()
            .get_function_decl(id)
            .signature
            .clone();
        cctx.func.signature = sig;
        cctx.set_disasm(true);
        let self_ref = func.binding();
        let ctx = self.ctx;
        ssa::translate(
            self,
            self_ref,
            ctx,
            func.name(),
            Some(func.return_cont()),
            &func.args,
            func.variadic,
            func.body,
            id,
            fctx,
            cctx,
        );

        self.module
            .define_function(id, cctx)
            .expect("failed to define function");

        println!(
            "Lowered function '{}':\n{}",
            func.name(),
            cctx.func.display()
        );

        cctx.clear();
    }

    pub fn finalize(mut self, entrypoint: FuncRef<'gc>) -> *const u8 {
        self.module.finalize_definitions().unwrap();
        let entry_id = self.funcmap[&entrypoint];
        let addr = self.module.get_finalized_function(entry_id);

        let constants = self.constmap.values().map(|&id| id).collect();
        let globals = self.globmap.values().map(|&id| id).collect();
        let ctx = self.ctx;
        let compiled = CompiledModule {
            module: self.module,
            entrypoint: entry_id,
            constants,
            globals,
            pd: PhantomData,
        };

        jit(ctx).modules.lock().unwrap().push(compiled);

        addr
    }
}

pub struct Thunks {
    pub argument_mismatch: FuncId,
    pub cons_rest: FuncId,
    pub alloc_closure: FuncId,
    pub alloc_closure_k: FuncId,
}

pub struct CompiledModule<'gc> {
    pub module: JITModule,
    pub entrypoint: FuncId,

    pub constants: Vec<DataId>,
    pub globals: Vec<DataId>,
    pd: PhantomData<&'gc ()>,
}

unsafe impl<'gc> Trace for CompiledModule<'gc> {
    unsafe fn trace(&mut self, visitor: &mut rsgc::Visitor) {
        for data_id in &self.constants {
            let (data, _) = self.module.get_finalized_data(*data_id);
            let data = data as *mut Value<'gc>;

            unsafe {
                (*data).trace(visitor);
            }
        }

        for data_id in &self.globals {
            let (data, _) = self.module.get_finalized_data(*data_id);
            let data = data as *mut Value<'gc>;

            unsafe {
                (*data).trace(visitor);
            }
        }
    }

    unsafe fn process_weak_refs(&mut self, _weak_processor: &mut rsgc::WeakProcessor) {
        // No weak references to process in this module
    }
}

pub struct JIT<'gc> {
    pub modules: Mutex<Vec<CompiledModule<'gc>>>,
}

unsafe impl<'gc> Trace for JIT<'gc> {
    unsafe fn trace(&mut self, visitor: &mut rsgc::Visitor) {
        let modules = self.modules.get_mut().unwrap();

        for module in modules.iter_mut() {
            unsafe {
                module.trace(visitor);
            }
        }
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut rsgc::WeakProcessor) {
        let _ = weak_processor;
    }
}

pub static SCHEME_JIT_CODE: OnceLock<Global<Rootable!(JIT<'_>)>> = OnceLock::new();

pub fn jit<'gc>(ctx: Context<'gc>) -> &'gc JIT<'gc> {
    SCHEME_JIT_CODE
        .get_or_init(|| {
            let jit = JIT {
                modules: Mutex::new(Vec::new()),
            };
            Global::new(jit)
        })
        .fetch(&ctx)
}
