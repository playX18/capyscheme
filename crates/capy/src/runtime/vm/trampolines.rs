//! Trampolines from Scheme to Rust code and vice-versa.
//!
//! We generate these with Cranelift because the "tail" calling convention is
//! not ABI stable and we can't write trampolines using `global_asm!`.

use crate::runtime::{CallData, value::ReturnCode};
use std::{
    mem::{offset_of, size_of},
    sync::{LazyLock, Mutex},
};

use crate::compiler::{
    codegen::{CompileContext, DataKind, ImportKind, Symbol, host_isa},
    direct::{Relocation, Target, compile_function},
};
use crate::heap::mmtk::util::Address;
use crate::runtime::{
    code_memory::{CodeAllocation, CodeMemory, CodeSpan},
    symbols::{RuntimeData, RuntimeThunk},
};
use cranelift::prelude::{AbiParam, FunctionBuilder, FunctionBuilderContext, InstBuilder, types};
use cranelift_codegen::{
    Context,
    binemit::Reloc,
    ir::{self, Function, UserFuncName},
    isa::CallConv,
};

use crate::{
    call_signature,
    runtime::{
        COMPILED_ENTRY_ARG_COUNT, REGISTER_ARG_COUNT, State,
        value::{Closure, CodeBlock, NativeProc, Value},
    },
};

#[cfg(target_arch = "x86_64")]
const ABSOLUTE_JUMP_STUB_LEN: usize = 13;

#[cfg(target_arch = "aarch64")]
const ABSOLUTE_JUMP_STUB_LEN: usize = 16;

#[cfg(target_arch = "riscv64")]
const ABSOLUTE_JUMP_STUB_LEN: usize = 16;

#[cfg(target_arch = "x86_64")]
const ARCH_CALL_RELOC: Reloc = Reloc::X86CallPCRel4;

#[cfg(target_arch = "aarch64")]
const ARCH_CALL_RELOC: Reloc = Reloc::Arm64Call;

#[cfg(target_arch = "riscv64")]
const ARCH_CALL_RELOC: Reloc = Reloc::RiscvCallPlt;

pub struct Trampolines {
    _memory: Mutex<CodeMemory>,
    enter_scheme_trampoline: CodeAllocation,
    native_trampoline: CodeAllocation,
    native_continuation_trampoline: CodeAllocation,
    debug_scheme_trampoline: CodeAllocation,

    pub enter_scheme_size: usize,
    pub native_trampoline_size: usize,
    pub native_continuation_trampoline_size: usize,
    pub debug_scheme_trampoline_size: usize,
}

fn compiled_tail_signature() -> ir::Signature {
    let mut sig = ir::Signature::new(CallConv::Tail);
    for _ in 0..COMPILED_ENTRY_ARG_COUNT {
        sig.params.push(AbiParam::new(types::I64));
    }
    sig
}

fn native_enter_signature() -> ir::Signature {
    // SysV entry from Rust: ctx + Tail args (rator, argc, arg0..arg3).
    let mut sig = ir::Signature::new(CallConv::SystemV);
    sig.params.push(AbiParam::new(types::I64)); // ctx
    for _ in 0..COMPILED_ENTRY_ARG_COUNT {
        sig.params.push(AbiParam::new(types::I64));
    }
    sig
}

fn overflow_base(
    builder: &mut FunctionBuilder<'_>,
    state: ir::Value,
    argc: ir::Value,
) -> ir::Value {
    let mem = ir::MemFlagsData::new();
    let overflow_count = builder.ins().iadd_imm_s(argc, -(REGISTER_ARG_COUNT as i64));
    let zero = builder.ins().iconst(types::I64, 0);
    let has_overflow = builder.ins().icmp_imm_s(
        ir::condcodes::IntCC::UnsignedGreaterThan,
        argc,
        REGISTER_ARG_COUNT as i64,
    );
    let overflow_count = builder.ins().select(has_overflow, overflow_count, zero);
    let overflow_bytes = builder
        .ins()
        .imul_imm_s(overflow_count, size_of::<Value>() as i64);
    let runstack = builder
        .ins()
        .load(types::I64, mem, state, offset_of!(State, runstack) as i32);
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
    let mem = ir::MemFlagsData::new();
    let present = builder.ins().icmp_imm_s(
        ir::condcodes::IntCC::UnsignedGreaterThan,
        argc,
        logical_index as i64,
    );
    let store = builder.create_block();
    let done = builder.create_block();
    builder.ins().brif(present, store, &[], done, &[]);
    builder.switch_to_block(store);
    builder.ins().store(
        mem,
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
    let mem = ir::MemFlagsData::new();
    let has_overflow = builder.ins().icmp_imm_s(
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
    let index = builder.ins().iadd_imm_s(next_index, -1);
    let src_offset = builder
        .ins()
        .iadd_imm_s(index, -(REGISTER_ARG_COUNT as i64));
    let src_offset = builder
        .ins()
        .imul_imm_s(src_offset, size_of::<Value>() as i64);
    let src = builder.ins().iadd(overflow_base, src_offset);
    let value = builder.ins().load(types::I64, mem, src, 0);

    let dst_offset = builder.ins().iadd_imm_s(index, native_index_delta);
    let dst_offset = builder
        .ins()
        .imul_imm_s(dst_offset, size_of::<Value>() as i64);
    let dst = builder.ins().iadd(native_base, dst_offset);
    builder.ins().store(mem, value, dst, 0);

    let more = builder.ins().icmp_imm_s(
        ir::condcodes::IntCC::UnsignedGreaterThan,
        index,
        REGISTER_ARG_COUNT as i64,
    );
    builder
        .ins()
        .brif(more, loop_block, &[ir::BlockArg::Value(index)], done, &[]);
    builder.switch_to_block(done);
}

/// Enter Scheme from Rust: pin ctx, then call into Tail-convention code.
fn enter_scheme_trampoline_code(fctx: &mut FunctionBuilderContext, ctx: &mut Context) {
    let mut builder = FunctionBuilder::new(&mut ctx.func, fctx);
    let mem = ir::MemFlagsData::new();

    let entry = builder.create_block();
    builder.append_block_params_for_function_params(entry);
    builder.switch_to_block(entry);

    let ctx_v = builder.block_params(entry)[0];
    let rator = builder.block_params(entry)[1];
    let argc = builder.block_params(entry)[2];
    let arg0 = builder.block_params(entry)[3];
    let arg1 = builder.block_params(entry)[4];
    let arg2 = builder.block_params(entry)[5];
    let arg3 = builder.block_params(entry)[6];

    let sig = compiled_tail_signature();
    let sigref = builder.import_signature(sig);
    builder.ins().set_pinned_reg(ctx_v);

    let code = builder
        .ins()
        .load(types::I64, mem, rator, offset_of!(Closure, code) as i32);
    builder
        .ins()
        .call_indirect(sigref, code, &[rator, argc, arg0, arg1, arg2, arg3]);
    // Ok/Err exits via longjmp; falling through is a bug.
    builder.ins().trap(ir::TrapCode::STACK_OVERFLOW);

    builder.seal_all_blocks();
    builder.finalize(host_isa().frontend_config());
}

/// Trampoline from Scheme code to native procedure. Generated exactly once and is used for every native function.
fn scheme_native_trampoline_code(fctx: &mut FunctionBuilderContext, ctx: &mut Context) {
    let mut builder = FunctionBuilder::new(&mut ctx.func, fctx);
    let mem = ir::MemFlagsData::new();

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
        .iadd_imm_s(ctx, crate::runtime::thread::Context::OFFSET_OF_STATE as i64);
    let native_data = builder
        .ins()
        .load(types::I64, mem, rator, Closure::DATA_OFFSET as i32);

    let proc = builder.ins().load(
        types::I64,
        mem,
        native_data,
        offset_of!(NativeProc, proc) as i32,
    );

    builder.ins().store(
        mem,
        ret_addr,
        state,
        offset_of!(State, last_ret_addr) as i32,
    );
    let overflow_base = overflow_base(&mut builder, state, argc);
    let native_base = overflow_base;
    copy_overflow_args(&mut builder, argc, overflow_base, native_base, -1);
    copy_register_arg_if_present(&mut builder, argc, arg1, 1, native_base, 0);
    copy_register_arg_if_present(&mut builder, argc, arg2, 2, native_base, 1);
    copy_register_arg_if_present(&mut builder, argc, arg3, 3, native_base, 2);
    let num_rands = builder.ins().iadd_imm_s(argc, -1);
    builder
        .ins()
        .store(mem, native_base, state, offset_of!(State, runstack) as i32);

    let call =
        builder
            .ins()
            .call_indirect(sigref, proc, &[ctx, rator, native_base, num_rands, retk]);
    let code = builder.inst_results(call)[0];
    let value = builder.inst_results(call)[1];

    let on_ret = builder.create_block();
    let on_cont = builder.create_block();

    let is_cont = builder.ins().icmp_imm_s(
        ir::condcodes::IntCC::Equal,
        code,
        ReturnCode::Continue as i64,
    );

    builder.ins().brif(is_cont, on_cont, &[], on_ret, &[]);

    builder.switch_to_block(on_ret);
    {
        let longjmp_sig = call_signature!(SystemV(
            I64, /* ctx */
            I64, /* code */
            I64  /* value */
        ));
        let longjmp_sig = builder.import_signature(longjmp_sig);
        let longjmp = builder.ins().iconst(
            types::I64,
            RuntimeThunk::Thunk_scheme_longjmp.address().as_usize() as i64,
        );
        builder
            .ins()
            .call_indirect(longjmp_sig, longjmp, &[ctx, code, value]);
        builder.ins().trap(ir::TrapCode::STACK_OVERFLOW);
    }
    builder.switch_to_block(on_cont);

    let sig_call = compiled_tail_signature();
    let sig_call = builder.import_signature(sig_call);

    {
        let cdata = offset_of!(State, call_data) as i32;
        let rator = builder.ins().load(
            types::I64,
            mem,
            state,
            cdata + offset_of!(CallData, rator) as i32,
        );
        let argc = builder.ins().load(
            types::I64,
            mem,
            state,
            cdata + offset_of!(CallData, argc) as i32,
        );
        let arg0 = builder.ins().load(
            types::I64,
            mem,
            state,
            cdata + offset_of!(CallData, arg0) as i32,
        );
        let arg1 = builder.ins().load(
            types::I64,
            mem,
            state,
            cdata + offset_of!(CallData, arg1) as i32,
        );
        let arg2 = builder.ins().load(
            types::I64,
            mem,
            state,
            cdata + offset_of!(CallData, arg2) as i32,
        );
        let arg3 = builder.ins().load(
            types::I64,
            mem,
            state,
            cdata + offset_of!(CallData, arg3) as i32,
        );
        let zero = builder.ins().iconst(types::I64, 0);
        let undefined = builder
            .ins()
            .iconst(types::I64, Value::undefined().bits() as i64);
        builder.ins().store(
            mem,
            undefined,
            state,
            cdata + offset_of!(CallData, rator) as i32,
        );
        builder
            .ins()
            .store(mem, zero, state, cdata + offset_of!(CallData, argc) as i32);
        for offset in [
            offset_of!(CallData, arg0),
            offset_of!(CallData, arg1),
            offset_of!(CallData, arg2),
            offset_of!(CallData, arg3),
        ] {
            builder
                .ins()
                .store(mem, undefined, state, cdata + offset as i32);
        }

        let code = builder
            .ins()
            .load(types::I64, mem, rator, offset_of!(Closure, code) as i32);

        builder
            .ins()
            .return_call_indirect(sig_call, code, &[rator, argc, arg0, arg1, arg2, arg3]);
    }

    builder.seal_all_blocks();
    builder.finalize(host_isa().frontend_config());
}

fn scheme_native_continuation_code(fctx: &mut FunctionBuilderContext, ctx: &mut Context) {
    let mut builder = FunctionBuilder::new(&mut ctx.func, fctx);
    let mem = ir::MemFlagsData::new();

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
        .iadd_imm_s(ctx, crate::runtime::thread::Context::OFFSET_OF_STATE as i64);
    let native_data = builder
        .ins()
        .load(types::I64, mem, rator, Closure::DATA_OFFSET as i32);

    let proc = builder.ins().load(
        types::I64,
        mem,
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
    builder
        .ins()
        .store(mem, native_base, state, offset_of!(State, runstack) as i32);

    let call = builder
        .ins()
        .call_indirect(sigref, proc, &[ctx, rator, native_base, argc]);

    let code = builder.inst_results(call)[0];

    let on_ret = builder.create_block();
    let on_cont = builder.create_block();

    let is_cont = builder.ins().icmp_imm_s(
        ir::condcodes::IntCC::Equal,
        code,
        ReturnCode::Continue as i64,
    );

    builder.ins().brif(is_cont, on_cont, &[], on_ret, &[]);

    builder.switch_to_block(on_ret);
    {
        let val = builder.inst_results(call)[1];
        let longjmp_sig = call_signature!(SystemV(
            I64, /* ctx */
            I64, /* code */
            I64  /* value */
        ));
        let longjmp_sig = builder.import_signature(longjmp_sig);
        let longjmp = builder.ins().iconst(
            types::I64,
            RuntimeThunk::Thunk_scheme_longjmp.address().as_usize() as i64,
        );
        builder
            .ins()
            .call_indirect(longjmp_sig, longjmp, &[ctx, code, val]);
        builder.ins().trap(ir::TrapCode::STACK_OVERFLOW);
    }
    builder.switch_to_block(on_cont);

    let sig_call = compiled_tail_signature();
    let sig_call = builder.import_signature(sig_call);

    {
        let cdata = offset_of!(State, call_data) as i32;
        let rator = builder.ins().load(
            types::I64,
            mem,
            state,
            cdata + offset_of!(CallData, rator) as i32,
        );
        let argc = builder.ins().load(
            types::I64,
            mem,
            state,
            cdata + offset_of!(CallData, argc) as i32,
        );
        let arg0 = builder.ins().load(
            types::I64,
            mem,
            state,
            cdata + offset_of!(CallData, arg0) as i32,
        );
        let arg1 = builder.ins().load(
            types::I64,
            mem,
            state,
            cdata + offset_of!(CallData, arg1) as i32,
        );
        let arg2 = builder.ins().load(
            types::I64,
            mem,
            state,
            cdata + offset_of!(CallData, arg2) as i32,
        );
        let arg3 = builder.ins().load(
            types::I64,
            mem,
            state,
            cdata + offset_of!(CallData, arg3) as i32,
        );
        let zero = builder.ins().iconst(types::I64, 0);
        let undefined = builder
            .ins()
            .iconst(types::I64, Value::undefined().bits() as i64);
        builder.ins().store(
            mem,
            undefined,
            state,
            cdata + offset_of!(CallData, rator) as i32,
        );
        builder
            .ins()
            .store(mem, zero, state, cdata + offset_of!(CallData, argc) as i32);
        for offset in [
            offset_of!(CallData, arg0),
            offset_of!(CallData, arg1),
            offset_of!(CallData, arg2),
            offset_of!(CallData, arg3),
        ] {
            builder
                .ins()
                .store(mem, undefined, state, cdata + offset as i32);
        }

        let code = builder
            .ins()
            .load(types::I64, mem, rator, offset_of!(Closure, code) as i32);

        builder
            .ins()
            .return_call_indirect(sig_call, code, &[rator, argc, arg0, arg1, arg2, arg3]);
    }

    builder.seal_all_blocks();
    builder.finalize(host_isa().frontend_config());
}

fn debug_scheme_trampoline_code(fctx: &mut FunctionBuilderContext, ctx: &mut Context) {
    let mut builder = FunctionBuilder::new(&mut ctx.func, fctx);
    let mem = ir::MemFlagsData::new();

    let entry = builder.create_block();
    builder.append_block_params_for_function_params(entry);
    builder.switch_to_block(entry);

    let rator = builder.block_params(entry)[0];
    let argc = builder.block_params(entry)[1];
    let arg0 = builder.block_params(entry)[2];
    let arg1 = builder.block_params(entry)[3];
    let arg2 = builder.block_params(entry)[4];
    let arg3 = builder.block_params(entry)[5];
    let ctx = builder.ins().get_pinned_reg(types::I64);
    let state = builder
        .ins()
        .iadd_imm_s(ctx, crate::runtime::thread::Context::OFFSET_OF_STATE as i64);
    let overflow = overflow_base(&mut builder, state, argc);

    let code_block = builder.ins().load(
        types::I64,
        mem,
        rator,
        offset_of!(Closure, code_block) as i32,
    );
    let real_code = builder.ins().load(
        types::I64,
        mem,
        code_block,
        offset_of!(CodeBlock, entrypoint) as i32,
    );
    let flags = builder.ins().load(
        types::I32,
        mem,
        code_block,
        offset_of!(CodeBlock, flags) as i32,
    );
    let continuation_flag = builder.ins().band_imm_u(flags, 1);
    let is_continuation =
        builder
            .ins()
            .icmp_imm_s(ir::condcodes::IntCC::NotEqual, continuation_flag, 0);
    let has_retk = builder
        .ins()
        .icmp_imm_s(ir::condcodes::IntCC::UnsignedGreaterThan, argc, 0);

    let check_continuation = builder.create_block();
    let push_frame = builder.create_block();
    let call_real = builder.create_block();
    builder.append_block_param(call_real, types::I64);

    builder.ins().brif(
        has_retk,
        check_continuation,
        &[],
        call_real,
        &[ir::BlockArg::Value(arg0)],
    );

    builder.switch_to_block(check_continuation);
    builder.ins().brif(
        is_continuation,
        call_real,
        &[ir::BlockArg::Value(arg0)],
        push_frame,
        &[],
    );

    builder.switch_to_block(push_frame);
    let sig = call_signature!(SystemV(
        I64, /* ctx */
        I64, /* rator */
        I64, /* argc */
        I64, /* arg0 */
        I64, /* arg1 */
        I64, /* arg2 */
        I64, /* arg3 */
        I64  /* overflow */
    ) -> I64);
    let sigref = builder.import_signature(sig);
    let helper = builder.ins().iconst(
        types::I64,
        RuntimeThunk::Thunk_push_debug_dframe_regs
            .address()
            .as_usize() as i64,
    );
    let call = builder.ins().call_indirect(
        sigref,
        helper,
        &[ctx, rator, argc, arg0, arg1, arg2, arg3, overflow],
    );
    let new_arg0 = builder.inst_results(call)[0];
    builder
        .ins()
        .jump(call_real, &[ir::BlockArg::Value(new_arg0)]);

    builder.switch_to_block(call_real);
    let new_arg0 = builder.block_params(call_real)[0];
    let sig_call = compiled_tail_signature();
    let sig_call = builder.import_signature(sig_call);
    builder.ins().return_call_indirect(
        sig_call,
        real_code,
        &[rator, argc, new_arg0, arg1, arg2, arg3],
    );

    builder.seal_all_blocks();
    builder.finalize(host_isa().frontend_config());
}

impl Trampolines {
    pub fn new() -> Self {
        let isa = host_isa();
        let mut memory = CodeMemory::new();

        let enter_scheme_trampoline = compile_trampoline(
            &mut memory,
            &*isa,
            0,
            native_enter_signature(),
            enter_scheme_trampoline_code,
        );
        let native_trampoline = compile_trampoline(
            &mut memory,
            &*isa,
            1,
            compiled_tail_signature(),
            scheme_native_trampoline_code,
        );
        let native_continuation_trampoline = compile_trampoline(
            &mut memory,
            &*isa,
            2,
            compiled_tail_signature(),
            scheme_native_continuation_code,
        );
        let debug_scheme_trampoline = compile_trampoline(
            &mut memory,
            &*isa,
            3,
            compiled_tail_signature(),
            debug_scheme_trampoline_code,
        );

        Self {
            enter_scheme_size: enter_scheme_trampoline.size,
            native_trampoline_size: native_trampoline.size,
            native_continuation_trampoline_size: native_continuation_trampoline.size,
            debug_scheme_trampoline_size: debug_scheme_trampoline.size,
            enter_scheme_trampoline,
            native_trampoline,
            native_continuation_trampoline,
            debug_scheme_trampoline,
            _memory: Mutex::new(memory),
        }
    }
}

impl Default for Trampolines {
    fn default() -> Self {
        Self::new()
    }
}

fn compile_trampoline(
    memory: &mut CodeMemory,
    isa: &dyn cranelift_codegen::isa::TargetIsa,
    symbol: u32,
    signature: ir::Signature,
    build: fn(&mut FunctionBuilderContext, &mut Context),
) -> CodeAllocation {
    let mut cache = CompileContext::new();
    cache.ctx.func = Function::with_name_signature(UserFuncName::user(0, symbol), signature);
    build(&mut cache.builder_ctx, &mut cache.ctx);
    let compiled = compile_function(isa, &mut cache).expect("failed to compile trampoline");
    let (bytes, call_stub_offsets) = code_bytes_with_call_stubs(&compiled.bytes, &compiled.relocs)
        .expect("failed to reserve trampoline call stubs");
    let mut loaded = memory
        .allocate_copy(&bytes)
        .expect("failed to allocate trampoline code");
    let mut site = TrampolineRelocationSite::from_loaded_code(&mut loaded, call_stub_offsets);
    for reloc in &compiled.relocs {
        apply_runtime_relocation(memory, &mut site, reloc)
            .expect("failed to patch trampoline code");
    }
    loaded
}

struct TrampolineRelocationSite<'a> {
    span: &'a mut CodeSpan,
    base: Address,
    call_stub_offsets: std::collections::HashMap<u32, u32>,
}

impl<'a> TrampolineRelocationSite<'a> {
    fn from_loaded_code(
        loaded: &'a mut CodeAllocation,
        call_stub_offsets: std::collections::HashMap<u32, u32>,
    ) -> Self {
        let base = loaded.entrypoint;
        Self {
            span: loaded.span_mut(),
            base,
            call_stub_offsets,
        }
    }
}

fn code_bytes_with_call_stubs(
    input_bytes: &[u8],
    relocations: &[Relocation],
) -> std::io::Result<(Vec<u8>, std::collections::HashMap<u32, u32>)> {
    let mut bytes = input_bytes.to_vec();
    let mut call_stub_offsets = std::collections::HashMap::new();
    for reloc in relocations
        .iter()
        .filter(|reloc| reloc.kind == ARCH_CALL_RELOC)
    {
        if call_stub_offsets.contains_key(&reloc.offset) {
            return Err(invalid_data("duplicate call relocation offset"));
        }
        let stub_offset =
            u32::try_from(bytes.len()).map_err(|_| invalid_data("trampoline code is too large"))?;
        call_stub_offsets.insert(reloc.offset, stub_offset);
        bytes.extend_from_slice(&empty_absolute_jump_stub()?);
    }
    Ok((bytes, call_stub_offsets))
}

fn apply_runtime_relocation(
    memory: &mut CodeMemory,
    site: &mut TrampolineRelocationSite<'_>,
    reloc: &Relocation,
) -> std::io::Result<()> {
    let target = resolve_runtime_relocation_target(reloc.target)?;
    match reloc.kind {
        Reloc::Abs8 => {
            let target = add_i64_to_usize(target.as_usize(), reloc.addend)?;
            memory.patch(site.span, reloc.offset as usize, &target.to_le_bytes())
        }
        Reloc::X86PCRel4 | Reloc::X86CallPCRel4 => apply_x86_pc_rel4(
            memory,
            site,
            reloc.offset,
            reloc.kind,
            target.as_usize(),
            reloc.addend,
        ),
        Reloc::Arm64Call => {
            apply_arm64_call_reloc(memory, site, reloc.offset, target.as_usize(), reloc.addend)
        }
        Reloc::RiscvCallPlt => apply_riscv64_call_plt_reloc(
            memory,
            site,
            reloc.offset,
            target.as_usize(),
            reloc.addend,
        ),
        _ => Err(invalid_data("unsupported trampoline relocation kind")),
    }
}

fn resolve_runtime_relocation_target(target: Target) -> std::io::Result<Address> {
    match target {
        Target::Symbol(Symbol::Data {
            kind: DataKind::RuntimeData,
            symbol,
        }) => RuntimeData::from_id(symbol.index())
            .map(|data| data.address())
            .ok_or_else(|| invalid_data("unknown runtime data relocation target")),
        Target::Symbol(Symbol::Imported { kind, symbol }) => match kind {
            ImportKind::RuntimeThunk => RuntimeThunk::from_id(symbol.index())
                .map(|thunk| thunk.address())
                .ok_or_else(|| invalid_data("unknown runtime thunk relocation target")),
            ImportKind::Trampoline => Err(invalid_data(
                "trampoline relocation target is not supported here",
            )),
        },
        Target::Symbol(Symbol::Function(_))
        | Target::Symbol(Symbol::Data { .. })
        | Target::FunctionOffset(_) => Err(invalid_data(
            "runtime relocation target is not supported here",
        )),
    }
}

fn empty_absolute_jump_stub() -> std::io::Result<Vec<u8>> {
    Ok(vec![0; ABSOLUTE_JUMP_STUB_LEN])
}

fn apply_x86_pc_rel4(
    memory: &mut CodeMemory,
    site: &mut TrampolineRelocationSite<'_>,
    offset: u32,
    kind: Reloc,
    target: usize,
    addend: i64,
) -> std::io::Result<()> {
    let patch_address = add_i64_to_usize(site.base.as_usize(), offset as i64)?;
    let relocated_target = add_i64_to_usize(target, addend)?;
    if let Some(displacement) = rel32_displacement(patch_address, relocated_target) {
        return memory.patch(site.span, offset as usize, &displacement.to_le_bytes());
    }

    if kind != Reloc::X86CallPCRel4 {
        return Err(invalid_data("pc-relative relocation target out of range"));
    }

    let stub_offset = site
        .call_stub_offsets
        .get(&offset)
        .copied()
        .ok_or_else(|| invalid_data("missing call stub for out-of-range relocation"))?;
    let stub_address = add_i64_to_usize(site.base.as_usize(), stub_offset as i64)?;
    let displacement = rel32_displacement(patch_address, add_i64_to_usize(stub_address, addend)?)
        .ok_or_else(|| invalid_data("call stub relocation target out of range"))?;
    memory.patch(site.span, offset as usize, &displacement.to_le_bytes())?;
    memory.patch(
        site.span,
        stub_offset as usize,
        &absolute_jump_stub(target)?,
    )
}

fn rel32_displacement(patch_address: usize, target: usize) -> Option<i32> {
    let displacement = target as i128 - patch_address as i128;
    i32::try_from(displacement).ok()
}

fn absolute_jump_stub(target: usize) -> std::io::Result<Vec<u8>> {
    #[cfg(target_arch = "x86_64")]
    {
        let mut stub = Vec::with_capacity(ABSOLUTE_JUMP_STUB_LEN);
        // movabs r11, imm64; jmp r11
        stub.extend_from_slice(&[0x49, 0xbb]);
        stub.extend_from_slice(&target.to_le_bytes());
        stub.extend_from_slice(&[0x41, 0xff, 0xe3]);
        Ok(stub)
    }
    #[cfg(target_arch = "aarch64")]
    {
        let addr = target as u64;
        let mut stub = Vec::with_capacity(ABSOLUTE_JUMP_STUB_LEN);
        // movz x16, #imm16
        stub.extend_from_slice(
            &((0xd2800000u32 | (((addr & 0xffff) as u32) << 5)) | 16).to_le_bytes(),
        );
        // movk x16, #imm16, lsl #16
        stub.extend_from_slice(
            &((0xf2a00000u32 | ((((addr >> 16) & 0xffff) as u32) << 5)) | 16).to_le_bytes(),
        );
        // movk x16, #imm16, lsl #32
        stub.extend_from_slice(
            &((0xf2c00000u32 | ((((addr >> 32) & 0xffff) as u32) << 5)) | 16).to_le_bytes(),
        );
        // br x16
        stub.extend_from_slice(&0xd61f0200u32.to_le_bytes());
        Ok(stub)
    }
    #[cfg(target_arch = "riscv64")]
    {
        let addr = target as i64;
        let hi20 = ((addr + 0x800) >> 12) as u32;
        let lo12 = (addr - ((hi20 as i64) << 12)) as u32;
        let mut stub = Vec::with_capacity(ABSOLUTE_JUMP_STUB_LEN);
        // lui x1, %hi20
        stub.extend_from_slice(&((hi20 << 12) | 0x37).to_le_bytes());
        // addi x1, x1, %lo12
        stub.extend_from_slice(
            &((lo12 << 20) | (1 << 15) | (0 << 12) | (1 << 7) | 0x13).to_le_bytes(),
        );
        // jalr x0, x1, 0
        stub.extend_from_slice(
            &((0u32 << 20) | (1 << 15) | (0 << 12) | (0 << 7) | 0x67).to_le_bytes(),
        );
        Ok(stub)
    }
    #[cfg(not(any(
        target_arch = "x86_64",
        target_arch = "aarch64",
        target_arch = "riscv64"
    )))]
    {
        Err(invalid_data(
            "absolute jump stubs are not supported on this architecture",
        ))
    }
}

fn read_u32_at(base: Address, offset: u32) -> std::io::Result<u32> {
    let addr = add_i64_to_usize(base.as_usize(), offset as i64)?;
    // SAFETY: The address is within allocated executable memory owned by the caller
    let bytes = unsafe { std::ptr::read_unaligned(addr as *const u32) };
    Ok(bytes)
}

fn apply_arm64_call_reloc(
    memory: &mut CodeMemory,
    site: &mut TrampolineRelocationSite<'_>,
    offset: u32,
    target: usize,
    addend: i64,
) -> std::io::Result<()> {
    let patch_address = add_i64_to_usize(site.base.as_usize(), offset as i64)?;
    let relocated_target = add_i64_to_usize(target, addend)?;
    let displacement = relocated_target as i128 - patch_address as i128;
    if displacement % 4 != 0 {
        return Err(invalid_data("arm64 call target is not 4-byte aligned"));
    }
    let imm26 = displacement / 4;
    if (-(1i128 << 25)..(1i128 << 25)).contains(&imm26) {
        let original = read_u32_at(site.base, offset)?;
        let patched = (original & !0x03ff_ffff) | ((imm26 as u32) & 0x03ff_ffff);
        memory.patch(site.span, offset as usize, &patched.to_le_bytes())
    } else {
        let stub_offset = site
            .call_stub_offsets
            .get(&offset)
            .copied()
            .ok_or_else(|| invalid_data("missing arm64 call stub for out-of-range relocation"))?;
        let stub_address = add_i64_to_usize(site.base.as_usize(), stub_offset as i64)?;
        let stub_displacement = stub_address as i128 - patch_address as i128;
        if stub_displacement % 4 != 0 {
            return Err(invalid_data("arm64 call stub is not 4-byte aligned"));
        }
        let stub_imm26 = stub_displacement / 4;
        if !(-(1i128 << 25)..(1i128 << 25)).contains(&stub_imm26) {
            return Err(invalid_data("arm64 call stub is out of bl range"));
        }
        let original = read_u32_at(site.base, offset)?;
        let patched = (original & !0x03ff_ffff) | ((stub_imm26 as u32) & 0x03ff_ffff);
        memory.patch(site.span, offset as usize, &patched.to_le_bytes())?;
        memory.patch(
            site.span,
            stub_offset as usize,
            &absolute_jump_stub(relocated_target)?,
        )
    }
}

fn apply_riscv64_call_plt_reloc(
    memory: &mut CodeMemory,
    site: &mut TrampolineRelocationSite<'_>,
    offset: u32,
    target: usize,
    addend: i64,
) -> std::io::Result<()> {
    let patch_address = add_i64_to_usize(site.base.as_usize(), offset as i64)?;
    let relocated_target = add_i64_to_usize(target, addend)?;
    let delta = relocated_target as i128 - patch_address as i128;

    // RiscvCallPlt is an AUIPC+JALR pair (8 bytes)
    if (-(1i128 << 31)..(1i128 << 31)).contains(&delta) {
        let auipc_orig = read_u32_at(site.base, offset)?;
        let jalr_orig = read_u32_at(site.base, offset + 4)?;
        let hi20 = ((delta as i64 + 0x800) >> 12) as u32;
        let lo12 = (delta as i64 - ((hi20 as i64) << 12)) as u32;
        let auipc = (auipc_orig & 0x00000fff) | (hi20 << 12);
        let jalr = (jalr_orig & 0x000fffff) | (lo12 << 20);
        let mut patch_bytes = [0u8; 8];
        patch_bytes[0..4].copy_from_slice(&auipc.to_le_bytes());
        patch_bytes[4..8].copy_from_slice(&jalr.to_le_bytes());
        memory.patch(site.span, offset as usize, &patch_bytes)
    } else {
        let stub_offset = site
            .call_stub_offsets
            .get(&offset)
            .copied()
            .ok_or_else(|| invalid_data("missing riscv call stub for out-of-range relocation"))?;
        let stub_address = add_i64_to_usize(site.base.as_usize(), stub_offset as i64)?;
        let stub_delta = stub_address as i128 - patch_address as i128;
        if !(-(1i128 << 31)..(1i128 << 31)).contains(&stub_delta) {
            return Err(invalid_data("riscv call stub is out of auipc+jalr range"));
        }
        let auipc_orig = read_u32_at(site.base, offset)?;
        let jalr_orig = read_u32_at(site.base, offset + 4)?;
        let hi20 = ((stub_delta as i64 + 0x800) >> 12) as u32;
        let lo12 = (stub_delta as i64 - ((hi20 as i64) << 12)) as u32;
        let auipc = (auipc_orig & 0x00000fff) | (hi20 << 12);
        let jalr = (jalr_orig & 0x000fffff) | (lo12 << 20);
        let mut patch_bytes = [0u8; 8];
        patch_bytes[0..4].copy_from_slice(&auipc.to_le_bytes());
        patch_bytes[4..8].copy_from_slice(&jalr.to_le_bytes());
        memory.patch(site.span, offset as usize, &patch_bytes)?;
        memory.patch(
            site.span,
            stub_offset as usize,
            &absolute_jump_stub(relocated_target)?,
        )
    }
}

fn add_i64_to_usize(base: usize, addend: i64) -> std::io::Result<usize> {
    let value = base as i128 + addend as i128;
    usize::try_from(value).map_err(|_| invalid_data("relocation address overflow"))
}

fn invalid_data(message: &'static str) -> std::io::Error {
    std::io::Error::new(std::io::ErrorKind::InvalidData, message)
}

// SAFETY: `Trampolines` is `Send` because all mutable state is synchronized
unsafe impl Send for Trampolines {}
// SAFETY: `Trampolines` is `Sync` because all mutable access is serialized
unsafe impl Sync for Trampolines {}

pub(crate) static TRAMPOLINES: LazyLock<Trampolines> = LazyLock::new(Trampolines::new);

static TRAMPOLINE_INTO_SCHEME: LazyLock<Address> =
    LazyLock::new(|| TRAMPOLINES.enter_scheme_trampoline.entrypoint);

static TRAMPOLINE_FROM_SCHEME: LazyLock<Address> =
    LazyLock::new(|| TRAMPOLINES.native_trampoline.entrypoint);

pub fn get_trampoline_into_scheme() -> Address {
    *TRAMPOLINE_INTO_SCHEME
}

pub fn get_trampoline_from_scheme() -> Address {
    *TRAMPOLINE_FROM_SCHEME
}

pub fn get_cont_trampoline_from_scheme() -> Address {
    TRAMPOLINES.native_continuation_trampoline.entrypoint
}

pub fn get_debug_trampoline_from_scheme() -> Address {
    TRAMPOLINES.debug_scheme_trampoline.entrypoint
}
