use std::{mem::offset_of, sync::LazyLock};

use cranelift::prelude::{
    FunctionBuilder, FunctionBuilderContext, InstBuilder, IntCC, JumpTableData, Signature,
    TrapCode, types,
};
use cranelift_codegen::{
    Context,
    ir::{self, BlockArg, BlockCall},
    isa::CallConv,
};
use cranelift_jit::JITModule;
use cranelift_module::{FuncId, Linkage, Module, default_libcall_names};
use rsgc::{Mutation, mmtk::util::Address};

use crate::runtime::{
    State,
    value::{Closure, NativeProc, NativeTrampolineCode, Value, Vector},
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

/// Emit a trampoline from Scheme to Rust code.
///
/// The job of this trampoline is to call the Rust function without returning to
/// Rust code that entered Scheme code. Instead, we perform trampoline right inside the thunk.
pub fn emit_native_trampoline(fctx: &mut FunctionBuilderContext, cctx: &mut Context) {
    let mut builder = FunctionBuilder::new(&mut cctx.func, fctx);

    let entry = builder.create_block();
    let exit = builder.create_block();

    builder.func.layout.set_cold(exit);
    builder.append_block_param(exit, types::I8);
    builder.append_block_param(exit, types::I64);

    builder.append_block_params_for_function_params(entry);
    let mc_value = builder.block_params(entry)[0];
    let state_value = builder.block_params(entry)[1];
    let k = builder.block_params(entry)[2];

    let mut native_sig = Signature::new(CallConv::SystemV);
    {
        // fn(mc, state, continuation) -> (return_code, value)
        native_sig.params.push(ir::AbiParam::new(ir::types::I64)); // mc
        native_sig.params.push(ir::AbiParam::new(ir::types::I64)); // state
        native_sig.params.push(ir::AbiParam::new(ir::types::I64)); // closure
        native_sig.params.push(ir::AbiParam::new(ir::types::I64)); // continuation

        native_sig.returns.push(ir::AbiParam::new(ir::types::I8)); // return code
        native_sig.returns.push(ir::AbiParam::new(ir::types::I64)); // value
    }
    let native_sig = builder.func.import_signature(native_sig);

    let proc = builder.ins().load(
        types::I64,
        ir::MemFlags::new(),
        state_value,
        offset_of!(State, vm_state) as i32 + offset_of!(VMState, proc) as i32,
    );
    let free = builder.ins().load(
        types::I64,
        ir::MemFlags::new(),
        proc,
        offset_of!(Closure, free) as i32,
    );

    let proc_addr = builder.ins().load(
        types::I64,
        ir::MemFlags::new(),
        free,
        offset_of!(NativeProc, proc) as i32,
    );
    let proc_free = builder.ins().load(
        types::I64,
        ir::MemFlags::new(),
        free,
        offset_of!(NativeProc, closure) as i32,
    );

    let call = builder.ins().call_indirect(
        native_sig,
        proc_addr,
        &[mc_value, state_value, proc_free, k],
    );

    let return_code = builder.inst_results(call)[0];
    let return_value = builder.inst_results(call)[1];

    let bb_continue = builder.create_block();
    let bb_call = builder.create_block();
    let bb_yield = builder.create_block();
    let bb_error = builder.create_block();
    let bb_default = builder.create_block();

    {
        let continue_call = builder.func.dfg.block_call(bb_continue, &[]);
        let call_call = builder.func.dfg.block_call(bb_call, &[]);
        let yield_call = builder.func.dfg.block_call(bb_yield, &[]);
        let error_call = builder.func.dfg.block_call(bb_error, &[]);
        let default_call = builder.func.dfg.block_call(bb_default, &[]);

        let jt = builder.create_jump_table(JumpTableData::new(
            default_call,
            &[continue_call, call_call, yield_call, error_call],
        ));

        builder.ins().br_table(return_code, jt);
    }

    {
        builder.switch_to_block(bb_default);
        builder.ins().trap(TrapCode::STACK_OVERFLOW);
    }

    let vm_state_off = offset_of!(State, vm_state) as i32;
    let k_off = vm_state_off + offset_of!(VMState, k) as i32;
    let proc_off = vm_state_off + offset_of!(VMState, proc) as i32;
    let argv_off = vm_state_off + offset_of!(VMState, argv) as i32;
    let argc_off = vm_state_off + offset_of!(VMState, argc) as i32;

    {
        builder.switch_to_block(bb_continue);

        let k = builder
            .ins()
            .load(types::I64, ir::MemFlags::new(), state_value, k_off);

        let addr = builder.ins().load(
            types::I64,
            ir::MemFlags::new(),
            k,
            offset_of!(Closure, code) as i32,
        );

        let mut callk_sig = Signature::new(CallConv::Tail);
        callk_sig.params.push(ir::AbiParam::new(types::I64)); // mc
        callk_sig.params.push(ir::AbiParam::new(types::I64)); // state

        callk_sig.returns.push(ir::AbiParam::new(types::I8)); // return code
        callk_sig.returns.push(ir::AbiParam::new(types::I64)); //
        let callk_sig = builder.func.import_signature(callk_sig);

        builder
            .ins()
            .return_call_indirect(callk_sig, addr, &[mc_value, state_value]);
    }

    {
        builder.switch_to_block(bb_call);

        let proc = builder
            .ins()
            .load(types::I64, ir::MemFlags::new(), state_value, proc_off);

        let code = builder.ins().load(
            types::I64,
            ir::MemFlags::new(),
            proc,
            offset_of!(Closure, code) as i32,
        );

        let k = builder
            .ins()
            .load(types::I64, ir::MemFlags::new(), state_value, k_off);

        let mut call_sig = Signature::new(CallConv::Tail);
        call_sig.params.push(ir::AbiParam::new(types::I64)); // mc
        call_sig.params.push(ir::AbiParam::new(types::I64)); // state
        call_sig.params.push(ir::AbiParam::new(types::I64)); // proc

        call_sig.returns.push(ir::AbiParam::new(types::I8)); // return code
        call_sig.returns.push(ir::AbiParam::new(types::I64)); // value

        let call_sig = builder.func.import_signature(call_sig);

        builder
            .ins()
            .call_indirect(call_sig, code, &[mc_value, state_value, k]);
    }

    {
        builder.switch_to_block(bb_yield);
        let return_code = builder
            .ins()
            .iconst(types::I8, VMReturnCode::Yield as u8 as i64);
        let undef_value = builder
            .ins()
            .iconst(types::I64, Value::undefined().bits() as i64);
        builder.ins().return_(&[return_code, undef_value]);
    }

    {
        builder.switch_to_block(bb_error);
        let return_code = builder
            .ins()
            .iconst(types::I8, VMReturnCode::Error as u8 as i64);

        builder.ins().return_(&[return_code, return_value]);
    }

    builder.seal_all_blocks();
    builder.finalize();
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
