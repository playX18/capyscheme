//! Trampolines from Scheme to Rust code and vice-versa.
//!
//! We use cranelift-jit to generate them due to the fact that "tail" calling convention of Cranelift
//! is not ABI stable and we can't write trampolines using `global_asm!` because of that.

use crate::runtime::{CallData, value::ReturnCode};
use std::{
    mem::offset_of,
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
        State,
        value::{Closure, NativeProc, Vector},
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

fn enter_scheme_trampoline_code(fctx: &mut FunctionBuilderContext, ctx: &mut Context) {
    let mut builder = FunctionBuilder::new(&mut ctx.func, fctx);

    let entry = builder.create_block();

    builder.append_block_params_for_function_params(entry);

    builder.switch_to_block(entry);

    let ctx = builder.block_params(entry)[0];
    let rator = builder.block_params(entry)[1];
    let rands = builder.block_params(entry)[2];
    let num_rands = builder.block_params(entry)[3];

    let sig = call_signature!(Tail (I64, I64, I64) -> (I64, I64));
    let sigref = builder.import_signature(sig);
    let old_pinned = builder.ins().get_pinned_reg(types::I64);
    builder.ins().set_pinned_reg(ctx);

    let code = builder.ins().load(
        types::I64,
        MemFlags::new(),
        rator,
        offset_of!(Closure, code) as i32,
    );
    let call: ir::Inst = builder
        .ins()
        .call_indirect(sigref, code, &[rator, rands, num_rands]);

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
    let rands = builder.block_params(entry)[1];
    let num_rands = builder.block_params(entry)[2];

    builder.switch_to_block(entry);
    let ret_addr = builder.ins().get_return_address(types::I64);
    let ctx = builder.ins().get_pinned_reg(types::I64);

    let retk = builder
        .ins()
        .load(types::I64, ir::MemFlags::new(), rands, 0);
    let reth = builder
        .ins()
        .load(types::I64, ir::MemFlags::new(), rands, 8);

    let sig = call_signature!(SystemV(
        I64, /* ctx */
        I64, /* rator */
        I64, /* rands */
        I64, /* num_rands */
        I64, /* retk */
        I64  /* reth */
    ) -> (I64, I64));
    let sigref = builder.import_signature(sig);

    let fv = builder.ins().load(
        types::I64,
        MemFlags::new(),
        rator,
        offset_of!(Closure, free) as i32,
    );
    let native_data = builder.ins().load(
        types::I64,
        MemFlags::new(),
        fv,
        Vector::OFFSET_OF_DATA as i32,
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
    let state = builder
        .ins()
        .iadd_imm(ctx, crate::runtime::thread::Context::OFFSET_OF_STATE as i64);
    builder.ins().store(
        MemFlags::new(),
        ret_addr,
        state,
        offset_of!(State, last_ret_addr) as i32,
    );
    //let c = builder.ins().icmp_imm(IntCC::UnsignedLessThan, state, 100);
    //builder.ins().trapnz(c, TrapCode::HEAP_OUT_OF_BOUNDS);
    builder.ins().store(
        MemFlags::new(),
        rands,
        state,
        offset_of!(State, runstack) as i32,
    );

    let rands = builder.ins().iadd_imm(rands, 16);
    let num_rands = builder.ins().iadd_imm(num_rands, -2);

    let call =
        builder
            .ins()
            .call_indirect(sigref, proc, &[ctx, rator, rands, num_rands, retk, reth]);
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

    let sig_call = call_signature!(Tail (I64, I64, I64) -> (I64, I64));
    let sig_call = builder.import_signature(sig_call);

    {
        let cdata = offset_of!(State, call_data) as i32;
        let rator = builder.ins().load(
            types::I64,
            MemFlags::new(),
            state,
            cdata + offset_of!(CallData, rator) as i32,
        );
        let rands = builder.ins().load(
            types::I64,
            MemFlags::new(),
            state,
            cdata + offset_of!(CallData, rands) as i32,
        );
        let num_rands = builder.ins().load(
            types::I64,
            MemFlags::new(),
            state,
            cdata + offset_of!(CallData, num_rands) as i32,
        );

        let zero = builder.ins().iconst(types::I64, 0);
        builder.ins().store(
            MemFlags::new(),
            zero,
            state,
            cdata + offset_of!(CallData, rator) as i32,
        );
        builder.ins().store(
            MemFlags::new(),
            zero,
            state,
            cdata + offset_of!(CallData, rands) as i32,
        );
        builder.ins().store(
            MemFlags::new(),
            zero,
            state,
            cdata + offset_of!(CallData, num_rands) as i32,
        );

        let code = builder.ins().load(
            types::I64,
            MemFlags::new(),
            rator,
            offset_of!(Closure, code) as i32,
        );

        builder
            .ins()
            .return_call_indirect(sig_call, code, &[rator, rands, num_rands]);
    }
}

fn scheme_native_continuation_code(fctx: &mut FunctionBuilderContext, ctx: &mut Context) {
    let mut builder = FunctionBuilder::new(&mut ctx.func, fctx);

    let entry = builder.create_block();

    builder.append_block_params_for_function_params(entry);

    let rator = builder.block_params(entry)[0];
    let rands = builder.block_params(entry)[1];
    let num_rands = builder.block_params(entry)[2];

    builder.switch_to_block(entry);
    let ctx = builder.ins().get_pinned_reg(types::I64);

    let sig = call_signature!(SystemV(
        I64, /* ctx */
        I64, /* rator */
        I64, /* rands */
        I64  /* num_rands */
    ) -> (I64, I64));
    let sigref = builder.import_signature(sig);

    let fv = builder.ins().load(
        types::I64,
        MemFlags::new(),
        rator,
        offset_of!(Closure, free) as i32,
    );

    let native_data = builder.ins().load(
        types::I64,
        MemFlags::new(),
        fv,
        Vector::OFFSET_OF_DATA as i32,
    );

    let proc = builder.ins().load(
        types::I64,
        MemFlags::new(),
        native_data,
        offset_of!(NativeProc, proc) as i32,
    );

    let state = builder
        .ins()
        .iadd_imm(ctx, crate::runtime::thread::Context::OFFSET_OF_STATE as i64);

    builder.ins().store(
        MemFlags::new(),
        rands,
        state,
        offset_of!(State, runstack) as i32,
    );

    let call = builder
        .ins()
        .call_indirect(sigref, proc, &[ctx, rator, rands, num_rands]);

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

    let sig_call = call_signature!(Tail (I64, I64, I64) -> (I64, I64));
    let sig_call = builder.import_signature(sig_call);

    {
        let cdata = offset_of!(State, call_data) as i32;
        let rator = builder.ins().load(
            types::I64,
            MemFlags::new(),
            state,
            cdata + offset_of!(CallData, rator) as i32,
        );
        let rands = builder.ins().load(
            types::I64,
            MemFlags::new(),
            state,
            cdata + offset_of!(CallData, rands) as i32,
        );
        let num_rands = builder.ins().load(
            types::I64,
            MemFlags::new(),
            state,
            cdata + offset_of!(CallData, num_rands) as i32,
        );

        let zero = builder.ins().iconst(types::I64, 0);
        builder.ins().store(
            MemFlags::new(),
            zero,
            state,
            cdata + offset_of!(CallData, rator) as i32,
        );
        builder.ins().store(
            MemFlags::new(),
            zero,
            state,
            cdata + offset_of!(CallData, rands) as i32,
        );
        builder.ins().store(
            MemFlags::new(),
            zero,
            state,
            cdata + offset_of!(CallData, num_rands) as i32,
        );

        let code = builder.ins().load(
            types::I64,
            MemFlags::new(),
            rator,
            offset_of!(Closure, code) as i32,
        );

        builder
            .ins()
            .return_call_indirect(sig_call, code, &[rator, rands, num_rands]);
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

        let mut sig = call_signature!(SystemV (I64, I64, I64, I64) -> (I64, I64));

        let enter_scheme_trampoline = module
            .declare_function("enter_scheme", Linkage::Export, &sig)
            .unwrap();

        sig.clear(CallConv::Tail);
        {
            for _ in 0..3 {
                sig.params.push(AbiParam::new(types::I64));
            }

            for _ in 0..2 {
                sig.returns.push(AbiParam::new(types::I64));
            }
        }

        let native_trampoline = module
            .declare_function("native_trampoline", Linkage::Export, &sig)
            .unwrap();

        sig.clear(CallConv::Tail);
        {
            for _ in 0..3 {
                sig.params.push(AbiParam::new(types::I64));
            }
            for _ in 0..2 {
                sig.returns.push(AbiParam::new(types::I64));
            }
        }
        let native_continuation_trampoline = module
            .declare_function("native_continuation_trampoline", Linkage::Export, &sig)
            .unwrap();

        let mut ctx = module.make_context();
        let mut fctx = FunctionBuilderContext::new();
        let enter_scheme_size;
        let native_trampoline_size;
        let native_continuation_size;
        {
            ctx.func.signature = call_signature!(SystemV (I64, I64, I64, I64) -> (I64, I64));
            enter_scheme_trampoline_code(&mut fctx, &mut ctx);
            module
                .define_function(enter_scheme_trampoline, &mut ctx)
                .unwrap();
            enter_scheme_size = ctx.compiled_code().unwrap().code_info().total_size as usize;
            module.clear_context(&mut ctx);
            fctx = FunctionBuilderContext::new();
            ctx.func.signature = call_signature!(Tail (I64, I64, I64) -> (I64, I64));
            scheme_native_trampoline_code(&mut fctx, &mut ctx);
            module.define_function(native_trampoline, &mut ctx).unwrap();
            native_trampoline_size = ctx.compiled_code().unwrap().code_info().total_size as usize;
            module.clear_context(&mut ctx);
            fctx = FunctionBuilderContext::new();
            ctx.func.signature = call_signature!(Tail (I64, I64, I64) -> (I64, I64));
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
