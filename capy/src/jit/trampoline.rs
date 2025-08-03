use std::{mem::offset_of, sync::LazyLock};

use cranelift::prelude::{FunctionBuilder, FunctionBuilderContext, InstBuilder, Signature, types};
use cranelift_codegen::{Context, ir, isa::CallConv};
use cranelift_jit::JITModule;
use cranelift_module::{FuncId, Linkage, Module, default_libcall_names};
use rsgc::{Mutation, mmtk::util::Address};

use crate::runtime::{
    State,
    value::Value,
    vm::{VMReturn, VMReturnCode, VMState},
};

pub struct Trampolines {
    pub module: JITModule,
    pub trampoline_to_scheme: FuncId,
    pub trampoline_to_scheme_addr: Address,
    pub exit_continuation: FuncId,
    pub exit_continuation_addr: Address,
}

impl Trampolines {
    pub fn new(mut module: JITModule) -> Self {
        let mut sig = Signature::new(CallConv::SystemV);
        sig.params.push(ir::AbiParam::new(ir::types::I64)); // mc
        sig.params.push(ir::AbiParam::new(ir::types::I64)); // state
        sig.params.push(ir::AbiParam::new(ir::types::I64)); // entrypoint
        sig.params.push(ir::AbiParam::new(ir::types::I64)); // continuation

        sig.returns.push(ir::AbiParam::new(ir::types::I8)); // return code
        sig.returns.push(ir::AbiParam::new(ir::types::I64)); // value

        let trampoline_to_scheme = module
            .declare_function("trampoline_to_scheme", Linkage::Export, &sig)
            .unwrap();

        let mut fctx = FunctionBuilderContext::new();
        let mut cctx = module.make_context();

        cctx.func.signature = sig;
        emit_trampoline_to_scheme(&mut fctx, &mut cctx);

        module
            .define_function(trampoline_to_scheme, &mut cctx)
            .unwrap();

        cctx.clear();

        let mut sig = Signature::new(CallConv::Tail);
        sig.params.push(ir::AbiParam::new(ir::types::I64)); // mc
        sig.params.push(ir::AbiParam::new(ir::types::I64)); // state
        sig.returns.push(ir::AbiParam::new(ir::types::I8)); // code
        sig.returns.push(ir::AbiParam::new(ir::types::I64)); // value

        let exit_continuation = module
            .declare_function("exit_continuation", Linkage::Export, &sig)
            .unwrap();

        cctx.func.signature = sig;
        emit_exit_continuation(&mut fctx, &mut cctx);

        module
            .define_function(exit_continuation, &mut cctx)
            .unwrap();

        module.finalize_definitions().unwrap();

        let addr = module.get_finalized_function(trampoline_to_scheme);
        let trampoline_to_scheme_addr = Address::from_ptr(addr);
        let addr = module.get_finalized_function(exit_continuation);
        let exit_continuation_addr = Address::from_ptr(addr);

        Self {
            module,
            trampoline_to_scheme,
            trampoline_to_scheme_addr,
            exit_continuation,
            exit_continuation_addr,
        }
    }

    pub fn get_trampoline(&self) -> FuncId {
        self.trampoline_to_scheme
    }

    pub fn rust_to_scheme(
        &self,
    ) -> for<'gc> extern "C-unwind" fn(
        &'gc Mutation<'gc>,
        &'gc State<'gc>,
        Address,
        Value<'gc>,
    ) -> VMReturn<'gc> {
        unsafe { std::mem::transmute(self.trampoline_to_scheme_addr.to_ptr::<u8>()) }
    }

    pub fn exit_continuation(
        &self,
    ) -> for<'gc> extern "C-unwind" fn(&'gc Mutation<'gc>, &'gc State<'gc>) -> VMReturn<'gc> {
        unsafe { std::mem::transmute(self.exit_continuation_addr.to_ptr::<u8>()) }
    }
}

/// A simple trampoline from SystemV ABI to Scheme code with Tail ABI.
///
/// Just calls the entrypoint with the given mc and state, and returns the result.
fn emit_trampoline_to_scheme(fctx: &mut FunctionBuilderContext, cctx: &mut Context) {
    let mut builder = FunctionBuilder::new(&mut cctx.func, fctx);

    let entry = builder.create_block();
    builder.append_block_params_for_function_params(entry);
    let mc_value = builder.block_params(entry)[0];
    let state_value = builder.block_params(entry)[1];
    let entrypoint = builder.block_params(entry)[2];
    let k = builder.block_params(entry)[3];

    builder.switch_to_block(entry);

    let mut sig = Signature::new(CallConv::Tail);
    sig.params.push(ir::AbiParam::new(ir::types::I64)); // mc
    sig.params.push(ir::AbiParam::new(ir::types::I64)); // state
    sig.params.push(ir::AbiParam::new(ir::types::I64)); // continuation

    sig.returns.push(ir::AbiParam::new(ir::types::I8)); // return code
    sig.returns.push(ir::AbiParam::new(ir::types::I64)); // value
    let sig_ref = builder.func.import_signature(sig);

    let call = builder
        .ins()
        .call_indirect(sig_ref, entrypoint, &[mc_value, state_value, k]);

    let return_code = builder.inst_results(call)[0];
    let return_value = builder.inst_results(call)[1];

    builder.ins().return_(&[return_code, return_value]);

    builder.seal_all_blocks();
    builder.finalize();
}

pub fn emit_exit_continuation(fctx: &mut FunctionBuilderContext, cctx: &mut Context) {
    let mut builder = FunctionBuilder::new(&mut cctx.func, fctx);

    let entry = builder.create_block();
    builder.append_block_params_for_function_params(entry);
    let _mc_value = builder.block_params(entry)[0];
    let state_value = builder.block_params(entry)[1];

    builder.switch_to_block(entry);

    let argc = builder.ins().load(
        types::I64,
        ir::MemFlags::new(),
        state_value,
        offset_of!(State, vm_state) as i32 + offset_of!(VMState, argc) as i32,
    );

    let return_undef = builder.create_block();
    let return_one = builder.create_block();

    let cond = builder
        .ins()
        .icmp_imm(cranelift::prelude::IntCC::Equal, argc, 0);
    builder.ins().brif(cond, return_undef, &[], return_one, &[]);

    builder.switch_to_block(return_undef);
    let return_code = builder
        .ins()
        .iconst(types::I8, VMReturnCode::Return as u8 as i64);
    let return_value = builder
        .ins()
        .iconst(types::I64, Value::undefined().bits() as i64);
    builder.ins().return_(&[return_code, return_value]);

    builder.switch_to_block(return_one);
    let return_code = builder
        .ins()
        .iconst(types::I8, VMReturnCode::Return as u8 as i64);
    let argv = builder.ins().load(
        types::I64,
        ir::MemFlags::new(),
        state_value,
        offset_of!(State, vm_state) as i32 + offset_of!(VMState, argv) as i32,
    );
    let return_value = builder.ins().load(types::I64, ir::MemFlags::new(), argv, 0);

    builder.ins().return_(&[return_code, return_value]);
}

unsafe impl Send for Trampolines {}
unsafe impl Sync for Trampolines {}

pub static TRAMPOLINES: LazyLock<Trampolines> = LazyLock::new(|| {
    let module = JITModule::new(
        cranelift_jit::JITBuilder::with_flags(
            &[("preserve_frame_pointers", "true")],
            default_libcall_names(),
        )
        .unwrap(),
    );

    Trampolines::new(module)
});
