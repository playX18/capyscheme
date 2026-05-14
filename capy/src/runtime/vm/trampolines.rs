//! Trampolines from Scheme to Rust code and vice-versa.
//!
//! We use cranelift-jit to generate them due to the fact that "tail" calling convention of Cranelift
//! is not ABI stable and we can't write trampolines using `global_asm!` because of that.

use crate::runtime::{CallData, value::ReturnCode};
use std::{
    mem::{offset_of, size_of},
    sync::{LazyLock, Mutex},
};

use crate::rsgc::mmtk::util::Address;
use cranelift::prelude::{
    AbiParam, FunctionBuilder, FunctionBuilderContext, InstBuilder, MemFlags, types,
};
use cranelift_codegen::{
    Context,
    ir::{self},
    isa::CallConv,
};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{FuncId, Linkage, Module, default_libcall_names};

use crate::{
    call_signature,
    runtime::{
        COMPILED_ENTRY_ARG_COUNT, REGISTER_ARG_COUNT, State,
        value::{Closure, NativeProc, Value},
    },
};

pub struct Trampolines {
    module: Mutex<JITModule>,
    enter_scheme_trampoline: FuncId,
    native_trampoline: FuncId,
    native_continuation_trampoline: FuncId,

    pub enter_scheme_size: usize,
    pub native_trampoline_size: usize,
    pub native_continuation_trampoline_size: usize,
}

fn compiled_tail_signature() -> ir::Signature {
    let mut sig = ir::Signature::new(CallConv::Tail);
    for _ in 0..COMPILED_ENTRY_ARG_COUNT {
        sig.params.push(AbiParam::new(types::I64));
    }
    sig.returns.push(AbiParam::new(types::I64));
    sig.returns.push(AbiParam::new(types::I64));
    sig
}

fn native_enter_signature() -> ir::Signature {
    let mut sig = ir::Signature::new(CallConv::SystemV);
    sig.params.push(AbiParam::new(types::I64)); // ctx
    for _ in 0..COMPILED_ENTRY_ARG_COUNT {
        sig.params.push(AbiParam::new(types::I64));
    }
    sig.returns.push(AbiParam::new(types::I64));
    sig.returns.push(AbiParam::new(types::I64));
    sig
}

fn overflow_base(
    builder: &mut FunctionBuilder<'_>,
    state: ir::Value,
    argc: ir::Value,
) -> ir::Value {
    let overflow_count = builder.ins().iadd_imm(argc, -(REGISTER_ARG_COUNT as i64));
    let zero = builder.ins().iconst(types::I64, 0);
    let has_overflow = builder.ins().icmp_imm(
        ir::condcodes::IntCC::UnsignedGreaterThan,
        argc,
        REGISTER_ARG_COUNT as i64,
    );
    let overflow_count = builder.ins().select(has_overflow, overflow_count, zero);
    let overflow_bytes = builder
        .ins()
        .imul_imm(overflow_count, size_of::<Value>() as i64);
    let runstack = builder.ins().load(
        types::I64,
        MemFlags::new(),
        state,
        offset_of!(State, runstack) as i32,
    );
    builder.ins().isub(runstack, overflow_bytes)
}

fn copy_register_arg_if_present(
    builder: &mut FunctionBuilder<'_>,
    argc: ir::Value,
    arg: ir::Value,
    logical_index: usize,
    native_base: ir::Value,
    native_index: usize,
) {
    let present = builder.ins().icmp_imm(
        ir::condcodes::IntCC::UnsignedGreaterThan,
        argc,
        logical_index as i64,
    );
    let store = builder.create_block();
    let done = builder.create_block();
    builder.ins().brif(present, store, &[], done, &[]);
    builder.switch_to_block(store);
    builder.ins().store(
        MemFlags::new(),
        arg,
        native_base,
        (native_index * size_of::<Value>()) as i32,
    );
    builder.ins().jump(done, &[]);
    builder.switch_to_block(done);
}

fn copy_overflow_args(
    builder: &mut FunctionBuilder<'_>,
    argc: ir::Value,
    overflow_base: ir::Value,
    native_base: ir::Value,
    native_index_delta: i64,
) {
    let has_overflow = builder.ins().icmp_imm(
        ir::condcodes::IntCC::UnsignedGreaterThan,
        argc,
        REGISTER_ARG_COUNT as i64,
    );
    let loop_block = builder.create_block();
    let done = builder.create_block();
    builder.append_block_param(loop_block, types::I64);
    builder.ins().brif(
        has_overflow,
        loop_block,
        &[ir::BlockArg::Value(argc)],
        done,
        &[],
    );
    builder.switch_to_block(loop_block);
    let next_index = builder.block_params(loop_block)[0];
    let index = builder.ins().iadd_imm(next_index, -1);
    let src_offset = builder.ins().iadd_imm(index, -(REGISTER_ARG_COUNT as i64));
    let src_offset = builder
        .ins()
        .imul_imm(src_offset, size_of::<Value>() as i64);
    let src = builder.ins().iadd(overflow_base, src_offset);
    let value = builder.ins().load(types::I64, MemFlags::new(), src, 0);

    let dst_offset = builder.ins().iadd_imm(index, native_index_delta);
    let dst_offset = builder
        .ins()
        .imul_imm(dst_offset, size_of::<Value>() as i64);
    let dst = builder.ins().iadd(native_base, dst_offset);
    builder.ins().store(MemFlags::new(), value, dst, 0);

    let more = builder.ins().icmp_imm(
        ir::condcodes::IntCC::UnsignedGreaterThan,
        index,
        REGISTER_ARG_COUNT as i64,
    );
    builder
        .ins()
        .brif(more, loop_block, &[ir::BlockArg::Value(index)], done, &[]);
    builder.switch_to_block(done);
}

fn enter_scheme_trampoline_code(fctx: &mut FunctionBuilderContext, ctx: &mut Context) {
    let mut builder = FunctionBuilder::new(&mut ctx.func, fctx);

    let entry = builder.create_block();

    builder.append_block_params_for_function_params(entry);

    builder.switch_to_block(entry);

    let ctx = builder.block_params(entry)[0];
    let rator = builder.block_params(entry)[1];
    let argc = builder.block_params(entry)[2];
    let arg0 = builder.block_params(entry)[3];
    let arg1 = builder.block_params(entry)[4];
    let arg2 = builder.block_params(entry)[5];
    let arg3 = builder.block_params(entry)[6];

    let sig = compiled_tail_signature();
    let sigref = builder.import_signature(sig);
    let old_pinned = builder.ins().get_pinned_reg(types::I64);
    builder.ins().set_pinned_reg(ctx);

    let code = builder.ins().load(
        types::I64,
        MemFlags::new(),
        rator,
        offset_of!(Closure, code) as i32,
    );
    let call: ir::Inst =
        builder
            .ins()
            .call_indirect(sigref, code, &[rator, argc, arg0, arg1, arg2, arg3]);

    builder.ins().set_pinned_reg(old_pinned);
    let code = builder.inst_results(call)[0];
    let val = builder.inst_results(call)[1];
    builder.ins().return_(&[code, val]);

    builder.seal_all_blocks();
    builder.finalize();
}

/// Trampoline from Scheme code to native procedure. Generated exactly once and is used for every native function.
fn scheme_native_trampoline_code(fctx: &mut FunctionBuilderContext, ctx: &mut Context) {
    let mut builder = FunctionBuilder::new(&mut ctx.func, fctx);

    let entry = builder.create_block();

    builder.append_block_params_for_function_params(entry);

    let rator = builder.block_params(entry)[0];
    let argc = builder.block_params(entry)[1];
    let arg0 = builder.block_params(entry)[2];
    let arg1 = builder.block_params(entry)[3];
    let arg2 = builder.block_params(entry)[4];
    let arg3 = builder.block_params(entry)[5];

    builder.switch_to_block(entry);
    let ret_addr = builder.ins().get_return_address(types::I64);
    let ctx = builder.ins().get_pinned_reg(types::I64);
    let retk = arg0;

    let sig = call_signature!(SystemV(
        I64, /* ctx */
        I64, /* rator */
        I64, /* rands */
        I64, /* num_rands */
        I64  /* retk */
    ) -> (I64, I64));
    let sigref = builder.import_signature(sig);

    let state = builder
        .ins()
        .iadd_imm(ctx, crate::runtime::thread::Context::OFFSET_OF_STATE as i64);
    let native_data = builder.ins().load(
        types::I64,
        MemFlags::new(),
        rator,
        Closure::DATA_OFFSET as i32,
    );

    let proc = builder.ins().load(
        types::I64,
        MemFlags::new(),
        native_data,
        offset_of!(NativeProc, proc) as i32,
    );

    /*let state = builder.ins().load(
        types::I64,
        MemFlags::new(),
        ctx,
        offset_of!(RtCtx, state) as i32,
    );*/
    builder.ins().store(
        MemFlags::new(),
        ret_addr,
        state,
        offset_of!(State, last_ret_addr) as i32,
    );
    //let c = builder.ins().icmp_imm(IntCC::UnsignedLessThan, state, 100);
    //builder.ins().trapnz(c, TrapCode::HEAP_OUT_OF_BOUNDS);
    let overflow_base = overflow_base(&mut builder, state, argc);
    let native_base = overflow_base;
    copy_overflow_args(&mut builder, argc, overflow_base, native_base, -1);
    copy_register_arg_if_present(&mut builder, argc, arg1, 1, native_base, 0);
    copy_register_arg_if_present(&mut builder, argc, arg2, 2, native_base, 1);
    copy_register_arg_if_present(&mut builder, argc, arg3, 3, native_base, 2);
    let num_rands = builder.ins().iadd_imm(argc, -1);
    builder.ins().store(
        MemFlags::new(),
        native_base,
        state,
        offset_of!(State, runstack) as i32,
    );

    let call =
        builder
            .ins()
            .call_indirect(sigref, proc, &[ctx, rator, native_base, num_rands, retk]);
    let code = builder.inst_results(call)[0];
    let value = builder.inst_results(call)[1];

    let on_ret = builder.create_block();
    let on_cont = builder.create_block();

    let is_cont = builder.ins().icmp_imm(
        ir::condcodes::IntCC::Equal,
        code,
        ReturnCode::Continue as i64,
    );

    builder.ins().brif(is_cont, on_cont, &[], on_ret, &[]);

    builder.switch_to_block(on_ret);
    {
        builder.ins().return_(&[code, value]);
    }
    builder.switch_to_block(on_cont);

    let sig_call = compiled_tail_signature();
    let sig_call = builder.import_signature(sig_call);

    {
        let cdata = offset_of!(State, call_data) as i32;
        let rator = builder.ins().load(
            types::I64,
            MemFlags::new(),
            state,
            cdata + offset_of!(CallData, rator) as i32,
        );
        let argc = builder.ins().load(
            types::I64,
            MemFlags::new(),
            state,
            cdata + offset_of!(CallData, argc) as i32,
        );
        let arg0 = builder.ins().load(
            types::I64,
            MemFlags::new(),
            state,
            cdata + offset_of!(CallData, arg0) as i32,
        );
        let arg1 = builder.ins().load(
            types::I64,
            MemFlags::new(),
            state,
            cdata + offset_of!(CallData, arg1) as i32,
        );
        let arg2 = builder.ins().load(
            types::I64,
            MemFlags::new(),
            state,
            cdata + offset_of!(CallData, arg2) as i32,
        );
        let arg3 = builder.ins().load(
            types::I64,
            MemFlags::new(),
            state,
            cdata + offset_of!(CallData, arg3) as i32,
        );
        let zero = builder.ins().iconst(types::I64, 0);
        let undefined = builder
            .ins()
            .iconst(types::I64, Value::undefined().bits() as i64);
        builder.ins().store(
            MemFlags::new(),
            undefined,
            state,
            cdata + offset_of!(CallData, rator) as i32,
        );
        builder.ins().store(
            MemFlags::new(),
            zero,
            state,
            cdata + offset_of!(CallData, argc) as i32,
        );
        for offset in [
            offset_of!(CallData, arg0),
            offset_of!(CallData, arg1),
            offset_of!(CallData, arg2),
            offset_of!(CallData, arg3),
        ] {
            builder
                .ins()
                .store(MemFlags::new(), undefined, state, cdata + offset as i32);
        }

        let code = builder.ins().load(
            types::I64,
            MemFlags::new(),
            rator,
            offset_of!(Closure, code) as i32,
        );

        builder
            .ins()
            .return_call_indirect(sig_call, code, &[rator, argc, arg0, arg1, arg2, arg3]);
    }
}

fn scheme_native_continuation_code(fctx: &mut FunctionBuilderContext, ctx: &mut Context) {
    let mut builder = FunctionBuilder::new(&mut ctx.func, fctx);

    let entry = builder.create_block();

    builder.append_block_params_for_function_params(entry);

    let rator = builder.block_params(entry)[0];
    let argc = builder.block_params(entry)[1];
    let arg0 = builder.block_params(entry)[2];
    let arg1 = builder.block_params(entry)[3];
    let arg2 = builder.block_params(entry)[4];
    let arg3 = builder.block_params(entry)[5];

    builder.switch_to_block(entry);
    let ctx = builder.ins().get_pinned_reg(types::I64);

    let sig = call_signature!(SystemV(
        I64, /* ctx */
        I64, /* rator */
        I64, /* rands */
        I64  /* num_rands */
    ) -> (I64, I64));
    let sigref = builder.import_signature(sig);

    let state = builder
        .ins()
        .iadd_imm(ctx, crate::runtime::thread::Context::OFFSET_OF_STATE as i64);
    let native_data = builder.ins().load(
        types::I64,
        MemFlags::new(),
        rator,
        Closure::DATA_OFFSET as i32,
    );

    let proc = builder.ins().load(
        types::I64,
        MemFlags::new(),
        native_data,
        offset_of!(NativeProc, proc) as i32,
    );

    let overflow_base = overflow_base(&mut builder, state, argc);
    let native_base = overflow_base;
    copy_overflow_args(&mut builder, argc, overflow_base, native_base, 0);
    copy_register_arg_if_present(&mut builder, argc, arg0, 0, native_base, 0);
    copy_register_arg_if_present(&mut builder, argc, arg1, 1, native_base, 1);
    copy_register_arg_if_present(&mut builder, argc, arg2, 2, native_base, 2);
    copy_register_arg_if_present(&mut builder, argc, arg3, 3, native_base, 3);
    builder.ins().store(
        MemFlags::new(),
        native_base,
        state,
        offset_of!(State, runstack) as i32,
    );

    let call = builder
        .ins()
        .call_indirect(sigref, proc, &[ctx, rator, native_base, argc]);

    let code = builder.inst_results(call)[0];

    let on_ret = builder.create_block();
    let on_cont = builder.create_block();

    let is_cont = builder.ins().icmp_imm(
        ir::condcodes::IntCC::Equal,
        code,
        ReturnCode::Continue as i64,
    );

    builder.ins().brif(is_cont, on_cont, &[], on_ret, &[]);

    builder.switch_to_block(on_ret);
    {
        let val = builder.inst_results(call)[1];
        builder.ins().return_(&[code, val]);
    }
    builder.switch_to_block(on_cont);

    let sig_call = compiled_tail_signature();
    let sig_call = builder.import_signature(sig_call);

    {
        let cdata = offset_of!(State, call_data) as i32;
        let rator = builder.ins().load(
            types::I64,
            MemFlags::new(),
            state,
            cdata + offset_of!(CallData, rator) as i32,
        );
        let argc = builder.ins().load(
            types::I64,
            MemFlags::new(),
            state,
            cdata + offset_of!(CallData, argc) as i32,
        );
        let arg0 = builder.ins().load(
            types::I64,
            MemFlags::new(),
            state,
            cdata + offset_of!(CallData, arg0) as i32,
        );
        let arg1 = builder.ins().load(
            types::I64,
            MemFlags::new(),
            state,
            cdata + offset_of!(CallData, arg1) as i32,
        );
        let arg2 = builder.ins().load(
            types::I64,
            MemFlags::new(),
            state,
            cdata + offset_of!(CallData, arg2) as i32,
        );
        let arg3 = builder.ins().load(
            types::I64,
            MemFlags::new(),
            state,
            cdata + offset_of!(CallData, arg3) as i32,
        );
        let zero = builder.ins().iconst(types::I64, 0);
        let undefined = builder
            .ins()
            .iconst(types::I64, Value::undefined().bits() as i64);
        builder.ins().store(
            MemFlags::new(),
            undefined,
            state,
            cdata + offset_of!(CallData, rator) as i32,
        );
        builder.ins().store(
            MemFlags::new(),
            zero,
            state,
            cdata + offset_of!(CallData, argc) as i32,
        );
        for offset in [
            offset_of!(CallData, arg0),
            offset_of!(CallData, arg1),
            offset_of!(CallData, arg2),
            offset_of!(CallData, arg3),
        ] {
            builder
                .ins()
                .store(MemFlags::new(), undefined, state, cdata + offset as i32);
        }

        let code = builder.ins().load(
            types::I64,
            MemFlags::new(),
            rator,
            offset_of!(Closure, code) as i32,
        );

        builder
            .ins()
            .return_call_indirect(sig_call, code, &[rator, argc, arg0, arg1, arg2, arg3]);
    }
}

impl Trampolines {
    pub fn new() -> Self {
        let builder = JITBuilder::with_flags(
            &[
                ("opt_level", "speed"),
                ("preserve_frame_pointers", "true"),
                ("enable_pinned_reg", "true"),
            ],
            default_libcall_names(),
        )
        .unwrap();

        let mut module = JITModule::new(builder);

        let mut sig = native_enter_signature();

        let enter_scheme_trampoline = module
            .declare_function("enter_scheme", Linkage::Export, &sig)
            .unwrap();

        sig = compiled_tail_signature();

        let native_trampoline = module
            .declare_function("native_trampoline", Linkage::Export, &sig)
            .unwrap();

        sig = compiled_tail_signature();
        let native_continuation_trampoline = module
            .declare_function("native_continuation_trampoline", Linkage::Export, &sig)
            .unwrap();

        let mut ctx = module.make_context();
        let mut fctx = FunctionBuilderContext::new();
        let enter_scheme_size;
        let native_trampoline_size;
        let native_continuation_size;
        {
            ctx.func.signature = native_enter_signature();
            enter_scheme_trampoline_code(&mut fctx, &mut ctx);
            module
                .define_function(enter_scheme_trampoline, &mut ctx)
                .unwrap();
            enter_scheme_size = ctx.compiled_code().unwrap().code_info().total_size as usize;
            module.clear_context(&mut ctx);
            fctx = FunctionBuilderContext::new();
            ctx.func.signature = compiled_tail_signature();
            scheme_native_trampoline_code(&mut fctx, &mut ctx);
            module.define_function(native_trampoline, &mut ctx).unwrap();
            native_trampoline_size = ctx.compiled_code().unwrap().code_info().total_size as usize;
            module.clear_context(&mut ctx);
            fctx = FunctionBuilderContext::new();
            ctx.func.signature = compiled_tail_signature();
            scheme_native_continuation_code(&mut fctx, &mut ctx);
            module
                .define_function(native_continuation_trampoline, &mut ctx)
                .unwrap();
            native_continuation_size = ctx.compiled_code().unwrap().code_info().total_size as usize;
            module.clear_context(&mut ctx);
        }

        module.finalize_definitions().unwrap();

        Self {
            enter_scheme_trampoline,
            enter_scheme_size,
            native_trampoline,
            native_trampoline_size,
            native_continuation_trampoline,
            native_continuation_trampoline_size: native_continuation_size,
            module: Mutex::new(module),
        }
    }
}

unsafe impl Send for Trampolines {}
unsafe impl Sync for Trampolines {}

pub(crate) static TRAMPOLINES: LazyLock<Trampolines> = LazyLock::new(|| Trampolines::new());

static TRAMPOLINE_INTO_SCHEME: LazyLock<Address> = LazyLock::new(|| {
    Address::from_ptr(
        TRAMPOLINES
            .module
            .lock()
            .unwrap()
            .get_finalized_function(TRAMPOLINES.enter_scheme_trampoline),
    )
});

static TRAMPOLINE_FROM_SCHEME: LazyLock<Address> = LazyLock::new(|| {
    Address::from_ptr(
        TRAMPOLINES
            .module
            .lock()
            .unwrap()
            .get_finalized_function(TRAMPOLINES.native_trampoline),
    )
});

pub fn get_trampoline_into_scheme() -> Address {
    *TRAMPOLINE_INTO_SCHEME
}

pub fn get_trampoline_from_scheme() -> Address {
    *TRAMPOLINE_FROM_SCHEME
}

pub fn get_cont_trampoline_from_scheme() -> Address {
    let addr = Address::from_ptr(
        TRAMPOLINES
            .module
            .lock()
            .unwrap()
            .get_finalized_function(TRAMPOLINES.native_continuation_trampoline),
    );

    addr
}
