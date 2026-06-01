use std::{collections::HashMap, mem::offset_of};

use crate::rsgc::{
    Gc,
    object::{HeapObjectHeader, HeapTypeInfo, OBJECT_REF_OFFSET, builtin_type_ids},
    sync::thread::Thread,
};

use crate::compiler::codegen::{DataSymbol, FunctionSymbol};
use crate::{
    compiler::ssa::{
        LinearRestSource, MAX_RAISE_ARITY, RegisterCallArgs, SSABuilder, VarDef,
        primitive::PrimValue,
    },
    cps::{
        linear::{
            Block as LinearBlock, BranchTarget, ClosureKind, CodeId, Instruction, LinearAtom,
            Procedure, ProcedureKind, RestPredicate, SwitchCaseValue, SwitchKind, Terminator,
            ValueId,
        },
        term::Atom,
    },
    expander::core::{LVarRef, fresh_lvar},
    runtime::{
        Context, REGISTER_ARG_COUNT, State,
        value::CodeBlock,
        value::{Closure, Symbol, Tagged, TypeCode8, TypeCode16, Value},
        vm::exceptions::RaiseKind,
    },
};
use cranelift::frontend::Switch;
use cranelift::prelude::{InstBuilder, IntCC, types};
use cranelift_codegen::ir::{self, BlockArg};

#[derive(Debug, Clone, Copy)]
pub enum Callee {
    /// Callee is obtained by loading code pointer from a closure
    /// and doing an indirect tail-call.
    Indirect {
        target: ir::Value,
        closure: ir::Value,
    },
    /// Callee is known locally and loaded from a far-safe pointer slot.
    Direct {
        target: ir::Value,
        closure: ir::Value,
    },

    /// Callee is a self-recursive function, and the function body block is returned.
    SelfRec(ir::Block),
}

#[derive(Clone, Copy, Debug)]
pub(crate) enum AllocInfoPreset {
    Pair,
    ClosureProc,
    ClosureK,
    MutableVector,
}

impl AllocInfoPreset {
    pub(crate) const fn runtime_data(self) -> crate::runtime::symbols::RuntimeData {
        match self {
            Self::Pair => crate::runtime::symbols::RuntimeData::PairInfo,
            Self::ClosureProc => crate::runtime::symbols::RuntimeData::ClosureProcInfo,
            Self::ClosureK => crate::runtime::symbols::RuntimeData::ClosureKInfo,
            Self::MutableVector => crate::runtime::symbols::RuntimeData::MutableVectorInfo,
        }
    }

    const fn type_bits(self) -> u16 {
        match self {
            Self::Pair => TypeCode8::PAIR.bits() as u16,
            Self::ClosureProc => TypeCode16::CLOSURE_PROC.bits(),
            Self::ClosureK => TypeCode16::CLOSURE_K.bits(),
            Self::MutableVector => TypeCode16::MUTABLE_VECTOR.bits(),
        }
    }

    const fn info_id(self) -> u16 {
        match self {
            Self::Pair => builtin_type_ids::PAIR,
            Self::ClosureProc => builtin_type_ids::CLOSURE_PROC,
            Self::ClosureK => builtin_type_ids::CLOSURE_K,
            Self::MutableVector => builtin_type_ids::MUTABLE_VECTOR,
        }
    }

    const fn header_word(self) -> u64 {
        self.type_bits() as u64 | ((self.info_id() as u64) << 16)
    }
}

#[cfg(test)]
mod alloc_info_preset_tests {
    use super::*;
    use crate::runtime::symbols::RuntimeData;

    #[test]
    fn allocation_info_presets_have_runtime_data_ids() {
        assert_eq!(AllocInfoPreset::Pair.runtime_data(), RuntimeData::PairInfo);
        assert_eq!(
            AllocInfoPreset::ClosureProc.runtime_data(),
            RuntimeData::ClosureProcInfo
        );
        assert_eq!(
            AllocInfoPreset::ClosureK.runtime_data(),
            RuntimeData::ClosureKInfo
        );
        assert_eq!(
            AllocInfoPreset::MutableVector.runtime_data(),
            RuntimeData::MutableVectorInfo
        );
    }
}

impl<'gc, 'a, 'f> SSABuilder<'gc, 'a, 'f> {
    fn target_binding(&self) -> LVarRef<'gc> {
        self.target.sources[&self.target.binding]
    }

    fn target_name(&self) -> Value<'gc> {
        self.target.name
    }

    fn target_source(&self) -> Value<'gc> {
        self.target.source
    }

    fn target_params(&self) -> (Vec<LVarRef<'gc>>, Option<LVarRef<'gc>>) {
        (
            self.target
                .params
                .iter()
                .map(|param| self.target.sources[param])
                .collect(),
            self.target
                .variadic
                .map(|variadic| self.target.sources[&variadic]),
        )
    }

    fn target_linear_variadic(&self) -> Option<ValueId> {
        self.target.variadic
    }

    fn should_materialize_entry_rest(&self, _rest: LVarRef<'gc>) -> bool {
        false
    }

    fn target_return_cont(&self) -> Option<LVarRef<'gc>> {
        self.target
            .return_cont
            .map(|return_cont| self.target.sources[&return_cont])
    }

    fn target_is_function(&self) -> bool {
        self.target.kind == ProcedureKind::Function
    }

    pub fn is_self_reference(&mut self, var: LVarRef<'gc>) -> bool {
        Gc::ptr_eq(self.target.sources[&self.target.binding], var)
    }

    pub fn closure_from_callee(&mut self, callee: Callee) -> ir::Value {
        match callee {
            Callee::Indirect { closure, .. } => closure,
            Callee::Direct { closure, .. } => closure,
            Callee::SelfRec(_) => self.rator,
        }
    }

    pub fn import_runtime_data(
        &mut self,
        data: crate::runtime::symbols::RuntimeData,
        typ: ir::Type,
    ) -> ir::Value {
        let data = self
            .module_builder
            .declare_runtime_data_in_func(data, &mut self.builder.func);
        self.builder.ins().global_value(typ, data)
    }

    pub fn import_data(&mut self, data: DataSymbol) -> ir::GlobalValue {
        self.data_imports
            .entry(data)
            .or_insert_with(|| {
                self.module_builder
                    .declare_data_in_func(data, &mut self.builder.func)
            })
            .clone()
    }

    pub(crate) fn data_slot_address(&mut self, data: DataSymbol) -> ir::Value {
        let global_value = self.import_data(data);
        self.builder.ins().global_value(types::I64, global_value)
    }

    pub(crate) fn load_data_value(&mut self, data: DataSymbol) -> ir::Value {
        let addr = self.data_slot_address(data);
        self.builder
            .ins()
            .load(types::I64, ir::MemFlags::trusted().with_can_move(), addr, 0)
    }

    pub(crate) fn store_data_value(&mut self, data: DataSymbol, value: ir::Value) {
        let addr = self.data_slot_address(data);
        self.builder
            .ins()
            .store(ir::MemFlags::trusted().with_can_move(), value, addr, 0);
    }

    fn load_function_entrypoint(&mut self, function: FunctionSymbol) -> ir::Value {
        let slot = self.module_builder.declare_function_pointer_slot(function);
        self.load_data_value(slot)
    }

    pub fn var(&mut self, var: LVarRef<'gc>) -> ir::Value {
        if self.is_self_reference(var) {
            return self.rator;
        }
        match *self.variables.get(&var).unwrap_or_else(|| {
            panic!(
                "{}@{:p} var not found when compiling {}({})",
                var.name,
                var,
                self.target_binding().name,
                self.target_name()
            )
        }) {
            VarDef::Comparison(val) => {
                let true_ = self.builder.ins().iconst(types::I64, Value::VALUE_TRUE);
                let false_ = self.builder.ins().iconst(types::I64, Value::VALUE_FALSE);

                let val = self.builder.ins().select(val, true_, false_);
                self.debug_local(var, val);
                val
            }

            VarDef::Value(val) => val,
        }
    }

    fn state_ptr(&mut self) -> ir::Value {
        let ctx = self.builder.ins().get_pinned_reg(types::I64);
        self.builder
            .ins()
            .iadd_imm(ctx, Context::OFFSET_OF_STATE as i64)
    }

    fn overflow_base_from_argc(&mut self, argc: ir::Value) -> ir::Value {
        let state = self.state_ptr();
        super::overflow_base_from_argc(&mut self.builder, state, argc)
    }

    fn raw_arg_at(
        &mut self,
        args: [ir::Value; REGISTER_ARG_COUNT],
        overflow: ir::Value,
        index: usize,
    ) -> ir::Value {
        if index < REGISTER_ARG_COUNT {
            return args[index];
        }

        self.builder.ins().load(
            types::I64,
            ir::MemFlags::trusted().with_can_move(),
            overflow,
            ((index - REGISTER_ARG_COUNT) * std::mem::size_of::<Value>()) as i32,
        )
    }

    fn call_values(
        &mut self,
        rator: ir::Value,
        args: RegisterCallArgs,
    ) -> [ir::Value; REGISTER_ARG_COUNT + 2] {
        [
            rator,
            args.argc,
            args.args[0],
            args.args[1],
            args.args[2],
            args.args[3],
        ]
    }

    fn call_block_args(&mut self, rator: ir::Value, args: RegisterCallArgs) -> Vec<BlockArg> {
        self.call_values(rator, args)
            .into_iter()
            .map(BlockArg::Value)
            .collect()
    }

    fn emit_wrong_arity_trampoline_call(
        &mut self,
        actual_argc: ir::Value,
        retk_or_zero: ir::Value,
        got: ir::Value,
        expected: isize,
    ) {
        let got = self.linear_fixnum_from_usize_value(got);
        let expected = self
            .builder
            .ins()
            .iconst(types::I64, Value::new(expected as i32).bits() as i64);
        let target = self.load_function_entrypoint(self.module_builder.wrong_arity_trampoline);
        let undefined = self
            .builder
            .ins()
            .iconst(types::I64, Value::undefined().bits() as i64);
        self.builder.ins().return_call_indirect(
            self.sig_call,
            target,
            &[
                self.rator,
                actual_argc,
                retk_or_zero,
                got,
                expected,
                undefined,
            ],
        );
    }

    pub fn entrypoint(&mut self, argc: ir::Value, args: [ir::Value; REGISTER_ARG_COUNT]) {
        let source = self.target_source();
        let is_function = self.target_is_function();
        self.set_debug_loc(source);
        if is_function {
            self.check_yield(self.rator, argc, args);
        }

        if self.load_fixed_arity_register_arguments(argc, args) {
            return;
        }

        let overflow = self.overflow_base_from_argc(argc);
        self.load_arguments(argc, args, overflow);
    }

    fn load_fixed_arity_register_arguments(
        &mut self,
        argc: ir::Value,
        args: [ir::Value; REGISTER_ARG_COUNT],
    ) -> bool {
        let (params, rest) = self.target_params();
        if rest.is_some() || self.target_linear_variadic().is_some() {
            return false;
        }

        let return_cont = self.target_return_cont();
        let first_arg = usize::from(return_cont.is_some());
        let expected_argc = first_arg + params.len();
        if expected_argc > REGISTER_ARG_COUNT {
            return false;
        }

        let exact = self
            .builder
            .ins()
            .icmp_imm(IntCC::Equal, argc, expected_argc as i64);
        let succ = self.builder.create_block();
        let err = self.builder.create_block();
        self.builder.func.layout.set_cold(err);
        self.builder.ins().brif(exact, succ, &[], err, &[]);

        self.builder.switch_to_block(err);
        {
            let got = if first_arg == 0 {
                argc
            } else {
                self.builder.ins().iadd_imm(argc, -(first_arg as i64))
            };
            let expected = params.len() as isize;
            let retk_or_zero = return_cont
                .map(|_| args[0])
                .unwrap_or_else(|| self.builder.ins().iconst(types::I64, 0));
            self.emit_wrong_arity_trampoline_call(argc, retk_or_zero, got, expected);
        }

        self.builder.switch_to_block(succ);
        if let Some(return_cont) = return_cont {
            let retk = args[0];
            self.debug_local(return_cont, retk);
            self.variables.insert(return_cont, VarDef::Value(retk));
        }

        for (i, param) in params.iter().enumerate() {
            let value = args[first_arg + i];
            self.variables.insert(*param, VarDef::Value(value));
            self.debug_local(*param, value);
        }

        true
    }

    pub(crate) fn current_retk_value(&mut self) -> ir::Value {
        match self.target_return_cont() {
            Some(return_cont) => self.var(return_cont),
            None => {
                let ctx = self.builder.ins().get_pinned_reg(types::I64);
                let call = self.builder.ins().call(self.thunks.default_retk, &[ctx]);
                self.builder.inst_results(call)[0]
            }
        }
    }

    pub(crate) fn alloc_with_info_preset(
        &mut self,
        preset: AllocInfoPreset,
        con_alloc_size: usize,
        var_alloc_size: Option<ir::Value>,
    ) -> ir::Value {
        let info = self.import_runtime_data(preset.runtime_data(), types::I64);
        self.alloc_with_info_inner(info, Some(preset), con_alloc_size, var_alloc_size)
    }

    fn alloc_with_info_inner(
        &mut self,
        info: ir::Value,
        preset: Option<AllocInfoPreset>,
        con_alloc_size: usize,
        var_alloc_size: Option<ir::Value>,
    ) -> ir::Value {
        const INLINE_ALLOC_LIMIT: usize = 8 * 1024;
        const HEADER_SIZE: usize = size_of::<HeapObjectHeader>();
        let ctx = self.builder.ins().get_pinned_reg(types::I64);
        let info =
            self.builder
                .ins()
                .load(types::I64, ir::MemFlags::trusted().with_can_move(), info, 0);
        let slowpath = self.builder.create_block();
        let merge = self.builder.create_block();
        self.builder.append_block_param(merge, types::I64);
        self.builder.func.layout.set_cold(slowpath);

        let payload_size = match var_alloc_size {
            Some(var_alloc_size) if con_alloc_size != 0 => self
                .builder
                .ins()
                .iadd_imm(var_alloc_size, con_alloc_size as i64),
            Some(var_alloc_size) => var_alloc_size,
            None => self.builder.ins().iconst(types::I64, con_alloc_size as i64),
        };

        let inline_payload_limit = INLINE_ALLOC_LIMIT.saturating_sub(HEADER_SIZE);
        if con_alloc_size >= inline_payload_limit {
            self.builder.ins().jump(slowpath, &[]);
        } else if let Some(var_alloc_size) = var_alloc_size {
            let remaining = inline_payload_limit - con_alloc_size;
            let is_small = self.builder.ins().icmp_imm(
                IntCC::UnsignedLessThan,
                var_alloc_size,
                remaining as i64,
            );
            let fastpath = self.builder.create_block();
            self.builder
                .ins()
                .brif(is_small, fastpath, &[], slowpath, &[]);
            self.builder.switch_to_block(fastpath);
            self.emit_alloc_with_info_fastpath(ctx, info, preset, payload_size, slowpath, merge);
        } else {
            self.emit_alloc_with_info_fastpath(ctx, info, preset, payload_size, slowpath, merge);
        }

        self.builder.switch_to_block(slowpath);
        {
            let call = self
                .builder
                .ins()
                .call(self.thunks.alloc_with_info, &[ctx, info, payload_size]);
            let value = self.builder.inst_results(call)[0];
            self.builder.ins().jump(merge, &[BlockArg::Value(value)]);
        }

        self.builder.switch_to_block(merge);
        self.builder.block_params(merge)[0]
    }

    fn emit_alloc_with_info_fastpath(
        &mut self,
        ctx: ir::Value,
        info: ir::Value,
        preset: Option<AllocInfoPreset>,
        payload_size: ir::Value,
        slowpath: ir::Block,
        merge: ir::Block,
    ) {
        let header_word = preset.map(|preset| preset.header_word());
        let cursor = self.builder.ins().load(
            types::I64,
            ir::MemFlags::trusted(),
            ctx,
            Thread::LAB_OFFSET_CURSOR as i32,
        );
        let limit = self.builder.ins().load(
            types::I64,
            ir::MemFlags::trusted().with_can_move(),
            ctx,
            Thread::LAB_OFFSET_LIMIT as i32,
        );
        let aligned_payload_size = self.builder.ins().iadd_imm(payload_size, 7);
        let aligned_payload_size = self.builder.ins().band_imm(aligned_payload_size, !7);
        let total_size = self
            .builder
            .ins()
            .iadd_imm(aligned_payload_size, size_of::<HeapObjectHeader>() as i64);
        let alloc_end = self.builder.ins().iadd(cursor, total_size);
        let fits = self
            .builder
            .ins()
            .icmp(IntCC::UnsignedLessThanOrEqual, alloc_end, limit);
        let commit = self.builder.create_block();
        self.builder.ins().brif(fits, commit, &[], slowpath, &[]);

        self.builder.switch_to_block(commit);
        self.builder.ins().store(
            ir::MemFlags::trusted(),
            alloc_end,
            ctx,
            Thread::LAB_OFFSET_CURSOR as i32,
        );
        let header_word = match header_word {
            Some(header_word) => self.builder.ins().iconst(types::I64, header_word as i64),
            None => {
                let header_type_bits = self.builder.ins().load(
                    types::I16,
                    ir::MemFlags::trusted().with_can_move(),
                    info,
                    HeapTypeInfo::TYPE_BITS_OFFSET as i32,
                );
                let header_info_id = self.builder.ins().load(
                    types::I16,
                    ir::MemFlags::trusted().with_can_move(),
                    info,
                    HeapTypeInfo::ID_OFFSET as i32,
                );
                let header_type_bits = self.builder.ins().uextend(types::I64, header_type_bits);
                let header_type_bits = self
                    .builder
                    .ins()
                    .band_imm(header_type_bits, u16::MAX as i64);
                let header_info_id = self.builder.ins().uextend(types::I64, header_info_id);
                let header_info_id = self.builder.ins().ishl_imm(header_info_id, 16);
                self.builder.ins().bor(header_type_bits, header_info_id)
            }
        };
        self.builder
            .ins()
            .store(ir::MemFlags::trusted(), header_word, cursor, 0);
        let object = self
            .builder
            .ins()
            .iadd_imm(cursor, OBJECT_REF_OFFSET as i64);
        self.emit_set_vo_bit(object);
        self.builder.ins().jump(merge, &[BlockArg::Value(object)]);
    }

    pub(crate) fn make_closure(
        &mut self,
        code_block: ir::Value,
        free_count: usize,
        is_cont: bool,
    ) -> ir::Value {
        let preset = if is_cont {
            AllocInfoPreset::ClosureK
        } else {
            AllocInfoPreset::ClosureProc
        };
        let size = size_of::<Closure>() + free_count * size_of::<Value>();
        let closure = self.alloc_with_info_preset(preset, size, None);

        let entrypoint = self.builder.ins().load(
            types::I64,
            ir::MemFlags::trusted().with_can_move(),
            code_block,
            offset_of!(CodeBlock, entrypoint) as i32,
        );
        let metadata = self.builder.ins().load(
            types::I64,
            ir::MemFlags::trusted().with_can_move(),
            code_block,
            offset_of!(CodeBlock, metadata) as i32,
        );
        let nfree = self.builder.ins().iconst(types::I64, free_count as i64);
        self.builder.ins().store(
            ir::MemFlags::trusted(),
            entrypoint,
            closure,
            offset_of!(Closure, code) as i32,
        );
        self.builder.ins().store(
            ir::MemFlags::trusted(),
            code_block,
            closure,
            offset_of!(Closure, code_block) as i32,
        );
        self.builder.ins().store(
            ir::MemFlags::trusted(),
            metadata,
            closure,
            offset_of!(Closure, meta) as i32,
        );
        self.builder.ins().store(
            ir::MemFlags::trusted(),
            nfree,
            closure,
            offset_of!(Closure, nfree) as i32,
        );

        let undefined = self
            .builder
            .ins()
            .iconst(types::I64, Value::undefined().bits() as i64);
        for i in 0..free_count {
            self.builder.ins().store(
                ir::MemFlags::trusted(),
                undefined,
                closure,
                Closure::DATA_OFFSET as i32 + (i * size_of::<Value>()) as i32,
            );
        }

        closure
    }

    pub(crate) fn raise_to_exception_handler(&mut self, err: ir::Value) -> ir::Inst {
        let retk = match self.target_return_cont() {
            Some(return_cont) => self.var(return_cont),
            None => self.builder.ins().iconst(types::I64, 0),
        };
        let target = self
            .load_function_entrypoint(self.module_builder.raise_to_exception_handler_trampoline);
        let undefined = self
            .builder
            .ins()
            .iconst(types::I64, Value::undefined().bits() as i64);
        self.builder.ins().return_call_indirect(
            self.sig_call,
            target,
            &[err, undefined, retk, undefined, undefined, undefined],
        )
    }

    fn load_arguments(
        &mut self,
        argc: ir::Value,
        args: [ir::Value; REGISTER_ARG_COUNT],
        overflow: ir::Value,
    ) {
        let (params, rest) = self.target_params();

        let ctx = self.builder.ins().get_pinned_reg(types::I64);

        let state = self.state_ptr();
        /* reset the runstack to the state it was before call */
        self.builder.ins().store(
            ir::MemFlags::trusted().with_can_move(),
            overflow,
            state,
            offset_of!(State, runstack) as i32,
        );

        let mut first_arg = 0usize;
        let mut num_rands = argc;
        if let Some(return_cont) = self.target_return_cont() {
            let retk = self.raw_arg_at(args, overflow, 0);
            first_arg = 1;
            num_rands = self.builder.ins().iadd_imm(num_rands, -1);

            self.debug_local(return_cont, retk);

            self.variables.insert(return_cont, VarDef::Value(retk));
        }

        if let Some(rest) = self.target_linear_variadic() {
            self.linear_rest_sources.insert(
                rest,
                LinearRestSource {
                    argc,
                    args,
                    overflow,
                    first_rest: first_arg + params.len(),
                },
            );
        }

        if params.len() != 0 {
            if let Some(rest) = rest {
                let not_enough = self.builder.ins().icmp_imm(
                    IntCC::UnsignedLessThan,
                    num_rands,
                    params.len() as i64,
                );

                let succ = self.builder.create_block();
                let err = self.builder.create_block();
                self.builder.func.layout.set_cold(err);
                self.builder.ins().brif(not_enough, err, &[], succ, &[]);

                self.builder.switch_to_block(err);
                {
                    let got = num_rands;
                    let expected = -(params.len() as isize);
                    let retk_or_zero = self.current_retk_value();
                    self.emit_wrong_arity_trampoline_call(argc, retk_or_zero, got, expected);
                }
                self.builder.switch_to_block(succ);
                {
                    for (i, param) in params.iter().enumerate() {
                        let value = self.raw_arg_at(args, overflow, first_arg + i);
                        self.variables.insert(*param, VarDef::Value(value));
                        self.debug_local(*param, value);
                    }

                    let need_cons = self.builder.ins().icmp_imm(
                        IntCC::NotEqual,
                        num_rands,
                        params.len() as i64,
                    );

                    let null = self
                        .builder
                        .ins()
                        .iconst(types::I64, Value::null().bits() as i64);

                    let succ = self.builder.create_block();
                    let cons_block = self.builder.create_block();
                    self.builder.append_block_param(succ, types::I64);
                    self.builder.ins().brif(
                        need_cons,
                        cons_block,
                        &[],
                        succ,
                        &[BlockArg::Value(null)],
                    );
                    self.builder.switch_to_block(cons_block);
                    {
                        if self.should_materialize_entry_rest(rest) {
                            let from = self
                                .builder
                                .ins()
                                .iconst(types::I64, (first_arg + params.len()) as i64);
                            let cons = self.builder.ins().call(
                                self.thunks.cons_rest_regs,
                                &[
                                    ctx, argc, args[0], args[1], args[2], args[3], overflow, from,
                                ],
                            );
                            let list = self.builder.inst_results(cons)[0];

                            self.builder.ins().jump(succ, &[BlockArg::Value(list)]);
                        } else {
                            // rest list is not referenced: do not waste time materializing it
                            let null = self
                                .builder
                                .ins()
                                .iconst(types::I64, Value::null().bits() as i64);
                            self.builder.ins().jump(succ, &[BlockArg::Value(null)]);
                        }
                    }
                    self.builder.switch_to_block(succ);
                    {
                        let ls = self.builder.block_params(succ)[0];
                        self.variables.insert(rest, VarDef::Value(ls));
                        self.debug_local(rest, ls);
                    }
                }
            } else {
                let exact =
                    self.builder
                        .ins()
                        .icmp_imm(IntCC::Equal, num_rands, params.len() as i64);

                let succ = self.builder.create_block();
                let err = self.builder.create_block();
                self.builder.func.layout.set_cold(err);
                self.builder.ins().brif(exact, succ, &[], err, &[]);

                self.builder.switch_to_block(err);
                {
                    let got = num_rands;
                    let expected = params.len() as isize;
                    let retk_or_zero = self.current_retk_value();
                    self.emit_wrong_arity_trampoline_call(argc, retk_or_zero, got, expected);
                }
                self.builder.switch_to_block(succ);
                for (i, param) in params.iter().enumerate() {
                    let value = self.raw_arg_at(args, overflow, first_arg + i);
                    self.variables.insert(*param, VarDef::Value(value));
                    self.debug_local(*param, value);
                }
            }
        } else {
            if let Some(rest) = rest {
                let succ = self.builder.create_block();
                let cons_block = self.builder.create_block();

                let null = self
                    .builder
                    .ins()
                    .iconst(types::I64, Value::null().bits() as i64);

                self.builder.append_block_param(succ, types::I64);

                let need_cons = self.builder.ins().icmp_imm(IntCC::NotEqual, num_rands, 0);

                self.builder
                    .ins()
                    .brif(need_cons, cons_block, &[], succ, &[BlockArg::Value(null)]);
                self.builder.switch_to_block(cons_block);
                {
                    if self.should_materialize_entry_rest(rest) {
                        let from = self.builder.ins().iconst(types::I64, first_arg as i64);
                        let cons = self.builder.ins().call(
                            self.thunks.cons_rest_regs,
                            &[
                                ctx, argc, args[0], args[1], args[2], args[3], overflow, from,
                            ],
                        );
                        let list = self.builder.inst_results(cons)[0];

                        self.builder.ins().jump(succ, &[BlockArg::Value(list)]);
                    } else {
                        // rest list is not referenced: do not waste time materializing it
                        let null = self
                            .builder
                            .ins()
                            .iconst(types::I64, Value::null().bits() as i64);
                        self.builder.ins().jump(succ, &[BlockArg::Value(null)]);
                    }
                }
                self.builder.switch_to_block(succ);
                {
                    let ls = self.builder.block_params(succ)[0];
                    self.variables.insert(rest, VarDef::Value(ls));
                    self.debug_local(rest, ls);
                }
            } else {
                let succ = self.builder.create_block();
                let err = self.builder.create_block();

                self.builder.func.layout.set_cold(err);

                let nonzero = self.builder.ins().icmp_imm(IntCC::NotEqual, num_rands, 0);

                self.builder.ins().brif(nonzero, err, &[], succ, &[]);

                self.builder.switch_to_block(err);
                {
                    let got = num_rands;
                    let expected = 0isize;
                    let retk_or_zero = self.current_retk_value();
                    self.emit_wrong_arity_trampoline_call(argc, retk_or_zero, got, expected);
                }

                self.builder.switch_to_block(succ);
            }
        }
    }

    /// Prepare register-call arguments. The first four logical arguments are
    /// returned as SSA values; logical arg4 and later are stored at the current runstack.
    pub fn prepare_call_args(&mut self, args: &[ir::Value]) -> RegisterCallArgs {
        let overflow_count = args.len().saturating_sub(REGISTER_ARG_COUNT);
        let zero = self.builder.ins().iconst(types::I64, 0);
        let overflow = if overflow_count == 0 {
            zero
        } else {
            let state = self.state_ptr();
            let runstack = self.builder.ins().load(
                types::I64,
                ir::MemFlags::trusted().with_can_move(),
                state,
                offset_of!(State, runstack) as i32,
            );
            let new_runstack = self.builder.ins().iadd_imm(
                runstack,
                (overflow_count * std::mem::size_of::<Value>()) as i64,
            );
            self.builder.ins().store(
                ir::MemFlags::trusted().with_can_move(),
                new_runstack,
                state,
                offset_of!(State, runstack) as i32,
            );
            runstack
        };

        let mut regs = [zero; REGISTER_ARG_COUNT];
        for (i, arg) in args.iter().enumerate() {
            if i < REGISTER_ARG_COUNT {
                regs[i] = *arg;
            } else {
                self.builder.ins().store(
                    ir::MemFlags::trusted().with_can_move(),
                    *arg,
                    overflow,
                    ((i - REGISTER_ARG_COUNT) * std::mem::size_of::<Value>()) as i32,
                );
            }
        }

        RegisterCallArgs {
            argc: self.builder.ins().iconst(types::I64, args.len() as i64),
            args: regs,
            overflow,
        }
    }

    pub fn atom(&mut self, atom: Atom<'gc>) -> ir::Value {
        match atom {
            Atom::Local(var) => self.var(var),
            Atom::Constant(obj) => {
                let Some(data_id) = self.module_builder.intern_constant(obj) else {
                    return self.builder.ins().iconst(types::I64, obj.bits() as i64);
                };

                let global_value = self.import_data(data_id);

                let addr = self.builder.ins().global_value(types::I64, global_value);
                self.builder.ins().load(
                    types::I64,
                    ir::MemFlags::trusted().with_can_move(),
                    addr,
                    0,
                )
            }
        }
    }

    pub fn linear_procedure(&mut self, procedure: &Procedure<'gc>) {
        for block in &procedure.blocks {
            if block.id == procedure.entry {
                self.linear_blockmap.insert(block.id, self.entry_block);
                continue;
            }

            let clif_block = self.builder.create_block();
            for _ in &block.params {
                self.builder.append_block_param(clif_block, types::I64);
            }
            self.linear_blockmap.insert(block.id, clif_block);
        }

        let entry = procedure
            .blocks
            .iter()
            .find(|block| block.id == procedure.entry)
            .expect("linear procedure should contain its entry block");
        self.lower_linear_block(procedure, entry, true);

        for block in &procedure.blocks {
            if block.id == procedure.entry {
                continue;
            }

            let clif_block = self.linear_blockmap[&block.id];
            self.builder.switch_to_block(clif_block);
            self.lower_linear_block(procedure, block, false);
        }
    }

    fn lower_linear_block(
        &mut self,
        procedure: &Procedure<'gc>,
        block: &LinearBlock<'gc>,
        is_entry: bool,
    ) {
        self.set_debug_loc(block.source);
        if is_entry {
            self.bind_linear_var(procedure.binding, VarDef::Value(self.rator));
            for (var, source) in procedure.sources.iter() {
                if let Some(def) = self.variables.get(source).copied() {
                    self.bind_linear_var(*var, def);
                }
            }
        } else {
            let clif_block = self.linear_blockmap[&block.id];
            let params = self.builder.block_params(clif_block).to_vec();
            for (var, value) in block.params.iter().copied().zip(params.iter().copied()) {
                self.bind_linear_var(var, VarDef::Value(value));
            }
        }

        for instruction in &block.instructions {
            self.linear_instruction(instruction);
        }

        self.linear_terminator(procedure, &block.terminator);
    }

    fn bind_linear_var(&mut self, var: ValueId, def: VarDef) {
        self.linear_variables.insert(var, def);
        if let Some(source) = self.linear_source(var) {
            self.variables.insert(source, def);
            if let VarDef::Value(value) = def {
                self.debug_local(source, value);
            }
        }
    }

    fn linear_source(&self, var: ValueId) -> Option<LVarRef<'gc>> {
        self.target.sources.get(&var).copied()
    }

    fn comparison_to_value(&mut self, val: ir::Value) -> ir::Value {
        let true_ = self.builder.ins().iconst(types::I64, Value::VALUE_TRUE);
        let false_ = self.builder.ins().iconst(types::I64, Value::VALUE_FALSE);
        self.builder.ins().select(val, true_, false_)
    }

    fn linear_var(&mut self, var: ValueId) -> ir::Value {
        match *self
            .linear_variables
            .get(&var)
            .unwrap_or_else(|| panic!("linear variable {var:?} not found"))
        {
            VarDef::Value(value) => value,
            VarDef::Comparison(value) => self.comparison_to_value(value),
        }
    }

    fn linear_rest_source(&self, rest: ValueId) -> LinearRestSource {
        *self
            .linear_rest_sources
            .get(&rest)
            .unwrap_or_else(|| panic!("linear rest source {rest:?} not found"))
    }

    fn linear_rest_suffix_len(&mut self, rest: ValueId, skip: usize) -> ir::Value {
        let source = self.linear_rest_source(rest);
        self.builder
            .ins()
            .iadd_imm(source.argc, -((source.first_rest + skip) as i64))
    }

    fn linear_fixnum_from_usize_value(&mut self, value: ir::Value) -> ir::Value {
        let value = self.ireduce(types::I32, value);
        let value = self.zextend(types::I64, value);
        self.builder.ins().bor_imm(value, Value::NUMBER_TAG as i64)
    }

    fn linear_atom(&mut self, atom: LinearAtom<'gc>) -> ir::Value {
        match atom {
            LinearAtom::Constant(value) => self.atom(Atom::Constant(value)),
            LinearAtom::Local(var) => self.linear_var(var),
        }
    }

    fn linear_atom_for_cond(&mut self, atom: LinearAtom<'gc>) -> ir::Value {
        match atom {
            LinearAtom::Local(var) => match self.linear_variables.get(&var).copied() {
                Some(VarDef::Comparison(val)) => val,
                _ => {
                    let val = self.linear_var(var);
                    self.builder
                        .ins()
                        .icmp_imm(IntCC::NotEqual, val, Value::VALUE_FALSE)
                }
            },
            LinearAtom::Constant(_) => {
                let val = self.linear_atom(atom);
                self.builder
                    .ins()
                    .icmp_imm(IntCC::NotEqual, val, Value::VALUE_FALSE)
            }
        }
    }

    fn linear_atom_as_term_atom(&mut self, atom: LinearAtom<'gc>) -> Atom<'gc> {
        match atom {
            LinearAtom::Constant(value) => Atom::Constant(value),
            LinearAtom::Local(var) => {
                let alias = self.linear_source(var).unwrap_or_else(|| {
                    if let Some(alias) = self.synthetic_aliases.get(&var).copied() {
                        alias
                    } else {
                        let name =
                            Symbol::from_str(self.module_builder.ctx, "linear-synthetic").into();
                        let alias = fresh_lvar(self.module_builder.ctx, name);
                        self.synthetic_aliases.insert(var, alias);
                        alias
                    }
                });
                let def = *self
                    .linear_variables
                    .get(&var)
                    .unwrap_or_else(|| panic!("linear variable {var:?} not found"));
                self.variables.insert(alias, def);
                Atom::Local(alias)
            }
        }
    }

    fn code_block_data(&self, code: CodeId<'gc>) -> DataSymbol {
        match code {
            CodeId::Function(func) => self.module_builder.code_block_for_func[&func],
            CodeId::Continuation(cont) => self.module_builder.code_block_for_cont[&cont],
        }
    }

    fn linear_instruction(&mut self, instruction: &Instruction<'gc>) {
        match instruction {
            Instruction::Const { dst, value } => {
                let value = self.atom(Atom::Constant(*value));
                self.bind_linear_var(*dst, VarDef::Value(value));
            }
            Instruction::MakeClosure {
                dst,
                code,
                kind,
                free_count,
            } => {
                let code_block = self.load_data_value(self.code_block_data(*code));
                let clos = self.make_closure(
                    code_block,
                    *free_count,
                    matches!(kind, ClosureKind::Continuation),
                );
                self.bind_linear_var(*dst, VarDef::Value(clos));
            }
            Instruction::ClosureRef {
                dst,
                closure,
                index,
            } => {
                let closure = self.linear_atom(*closure);
                let value = self.builder.ins().load(
                    types::I64,
                    ir::MemFlags::trusted().with_can_move(),
                    closure,
                    Closure::DATA_OFFSET as i32 + (*index * 8) as i32,
                );
                self.bind_linear_var(*dst, VarDef::Value(value));
            }
            Instruction::ClosureSet {
                closure,
                index,
                value,
            } => {
                let closure = self.linear_atom(*closure);
                let value = self.linear_atom(*value);
                self.builder.ins().store(
                    ir::MemFlags::trusted().with_can_move(),
                    value,
                    closure,
                    Closure::DATA_OFFSET as i32 + (*index * 8) as i32,
                );
            }
            Instruction::CacheRef {
                dst,
                cache_key,
                source,
            } => {
                self.set_debug_loc(*source);
                let LinearAtom::Constant(cache_key) = cache_key else {
                    panic!("invalid cache-ref: expected constant cache key, got {cache_key:?}");
                };
                let cell = self.module_builder.intern_cache_cell(*cache_key);
                let value = self.load_data_value(cell);
                self.bind_linear_var(*dst, VarDef::Value(value));
            }
            Instruction::CacheSet {
                dst,
                cache_key,
                value,
                source,
            } => {
                self.set_debug_loc(*source);
                let LinearAtom::Constant(cache_key) = cache_key else {
                    panic!("invalid cache-set!: expected constant cache key, got {cache_key:?}");
                };
                let value = self.linear_atom(*value);
                let cell = self.module_builder.intern_cache_cell(*cache_key);
                self.store_data_value(cell, value);
                let undefined = self
                    .builder
                    .ins()
                    .iconst(types::I64, Value::undefined().bits() as i64);
                self.bind_linear_var(*dst, VarDef::Value(undefined));
            }
            Instruction::PrimCall {
                dst,
                prim,
                args,
                source,
            } => {
                self.set_debug_loc(*source);
                let args = args
                    .iter()
                    .copied()
                    .map(|arg| self.linear_atom_as_term_atom(arg))
                    .collect::<Vec<_>>();
                let val = match prim.lower(self, &args) {
                    PrimValue::Value(val) => VarDef::Value(val),
                    PrimValue::Comparison(val) => VarDef::Comparison(val),
                };
                self.bind_linear_var(*dst, val);
            }
            Instruction::RestToList { dst, rest, source } => {
                self.set_debug_loc(*source);
                let rest_source = self.linear_rest_source(*rest);
                let ctx = self.builder.ins().get_pinned_reg(types::I64);
                let from = self
                    .builder
                    .ins()
                    .iconst(types::I64, rest_source.first_rest as i64);
                let call = self.builder.ins().call(
                    self.thunks.cons_rest_regs,
                    &[
                        ctx,
                        rest_source.argc,
                        rest_source.args[0],
                        rest_source.args[1],
                        rest_source.args[2],
                        rest_source.args[3],
                        rest_source.overflow,
                        from,
                    ],
                );
                let list = self.builder.inst_results(call)[0];
                self.bind_linear_var(*dst, VarDef::Value(list));
            }
            Instruction::RestRef {
                dst,
                rest,
                index,
                source,
            } => {
                self.set_debug_loc(*source);
                let rest_source = self.linear_rest_source(*rest);
                let value = self.raw_arg_at(
                    rest_source.args,
                    rest_source.overflow,
                    rest_source.first_rest + *index,
                );
                self.bind_linear_var(*dst, VarDef::Value(value));
            }
            Instruction::RestLength {
                dst,
                rest,
                skip,
                source,
            } => {
                self.set_debug_loc(*source);
                let len = self.linear_rest_suffix_len(*rest, *skip);
                let len = self.linear_fixnum_from_usize_value(len);
                self.bind_linear_var(*dst, VarDef::Value(len));
            }
            Instruction::RestPredicate {
                dst,
                rest,
                predicate,
                skip,
                source,
            } => {
                self.set_debug_loc(*source);
                let rest_source = self.linear_rest_source(*rest);
                let threshold = (rest_source.first_rest + *skip) as i64;
                let val = match predicate {
                    RestPredicate::Null => VarDef::Comparison(self.builder.ins().icmp_imm(
                        IntCC::Equal,
                        rest_source.argc,
                        threshold,
                    )),
                    RestPredicate::Pair => VarDef::Comparison(self.builder.ins().icmp_imm(
                        IntCC::UnsignedGreaterThan,
                        rest_source.argc,
                        threshold,
                    )),
                    RestPredicate::List => {
                        let true_ = self.builder.ins().iconst(types::I64, Value::VALUE_TRUE);
                        VarDef::Value(true_)
                    }
                };
                self.bind_linear_var(*dst, val);
            }
        }
    }

    fn get_callee_linear(&mut self, callee: LinearAtom<'gc>) -> Callee {
        if let LinearAtom::Local(var_id) = callee
            && let Some(var) = self.linear_source(var_id)
        {
            if self.is_self_reference(var) {
                return Callee::SelfRec(self.entry_block);
            }

            if let Some(func) = self.module_builder.reify_info.free_vars.funcs.get(&var)
                && let Some(func_id) = self.module_builder.func_for_func.get(func).copied()
            {
                let closure = self.linear_atom(callee);
                return Callee::Direct {
                    target: self.load_function_entrypoint(func_id),
                    closure,
                };
            }
        }

        let callee = self.linear_atom(callee);
        let get_clos_code = self.builder.create_block();
        let error = self.builder.create_block();
        self.builder.func.layout.set_cold(error);
        self.branch_if_has_typ8(callee, Closure::TC8.bits(), get_clos_code, &[], error, &[]);
        self.builder.switch_to_block(error);
        {
            self.emit_raise(RaiseKind::NonApplicable, &[callee], Value::new(false));
        }

        self.builder.switch_to_block(get_clos_code);
        let code = self.builder.ins().load(
            types::I64,
            ir::MemFlags::trusted().with_can_move(),
            callee,
            offset_of!(Closure, code) as i32,
        );
        Callee::Indirect {
            target: code,
            closure: callee,
        }
    }

    fn get_tail_callee_linear(&mut self, callee: LinearAtom<'gc>) -> Callee {
        if let LinearAtom::Local(var_id) = callee
            && let Some(var) = self.linear_source(var_id)
            && let Some(cont) = self.module_builder.reify_info.free_vars.conts.get(&var)
            && let Some(func_id) = self.module_builder.func_for_cont.get(cont).copied()
        {
            let closure = self.linear_atom(callee);
            return Callee::Direct {
                target: self.load_function_entrypoint(func_id),
                closure,
            };
        }

        let closure = self.linear_atom(callee);
        let code = self.builder.ins().load(
            types::I64,
            ir::MemFlags::trusted().with_can_move(),
            closure,
            offset_of!(Closure, code) as i32,
        );
        Callee::Indirect {
            target: code,
            closure,
        }
    }

    fn linear_call(
        &mut self,
        callee: LinearAtom<'gc>,
        retk: LinearAtom<'gc>,
        args: &[LinearAtom<'gc>],
        source: Value<'gc>,
    ) {
        self.set_debug_loc(source);
        let callee = self.get_callee_linear(callee);
        let retk = self.linear_atom(retk);
        let rands = std::iter::once(retk)
            .chain(args.iter().copied().map(|arg| self.linear_atom(arg)))
            .collect::<Vec<_>>();
        let mut call_args = self.prepare_call_args(&rands);

        if self.module_builder.stacktraces {
            let ctx = self.builder.ins().get_pinned_reg(types::I64);
            let src_info = self.atom(Atom::Constant(source));
            let rator = self.closure_from_callee(callee);

            let call = self.builder.ins().call(
                self.thunks.push_dframe_regs,
                &[
                    ctx,
                    src_info,
                    rator,
                    call_args.argc,
                    call_args.args[0],
                    call_args.args[1],
                    call_args.args[2],
                    call_args.args[3],
                    call_args.overflow,
                ],
            );
            call_args.args[0] = self.builder.inst_results(call)[0];
        }

        self.emit_callee_jump(callee, call_args);
    }

    fn linear_tail_call(
        &mut self,
        callee: LinearAtom<'gc>,
        args: &[LinearAtom<'gc>],
        source: Value<'gc>,
    ) {
        self.set_debug_loc(source);
        let callee = self.get_tail_callee_linear(callee);
        let args = args
            .iter()
            .copied()
            .map(|arg| self.linear_atom(arg))
            .collect::<Vec<_>>();
        let call_args = self.prepare_call_args(&args);
        self.emit_callee_jump(callee, call_args);
    }

    fn linear_raise(&mut self, kind: RaiseKind, args: &[LinearAtom<'gc>], source: Value<'gc>) {
        let args = args
            .iter()
            .copied()
            .map(|arg| self.linear_atom(arg))
            .collect::<Vec<_>>();
        self.emit_raise(kind, &args, source);
    }

    pub(crate) fn emit_raise(&mut self, kind: RaiseKind, args: &[ir::Value], source: Value<'gc>) {
        self.set_debug_loc(source);
        if args.len() > MAX_RAISE_ARITY {
            panic!(
                "%raise arity {} exceeds maximum fixed raise arity {}",
                args.len(),
                MAX_RAISE_ARITY
            );
        }
        let code = self.builder.ins().iconst(types::I64, kind.code() as i64);
        let retk = self.current_retk_value();
        let rands = std::iter::once(retk)
            .chain(args.iter().copied())
            .collect::<Vec<_>>();
        let call_args = self.prepare_call_args(&rands);
        let trampoline_id = self.module_builder.raise_trampolines[args.len()];
        let target = self.load_function_entrypoint(trampoline_id);
        let values = self.call_values(code, call_args);
        self.builder
            .ins()
            .return_call_indirect(self.sig_call, target, &values);
    }

    fn emit_callee_jump(&mut self, callee: Callee, call_args: RegisterCallArgs) {
        match callee {
            Callee::Indirect { target, closure } => {
                let mut block_args = vec![BlockArg::Value(target)];
                block_args.extend(self.call_block_args(closure, call_args));
                self.builder.ins().jump(self.exit_block, &block_args);
            }
            Callee::Direct { target, closure } => {
                let values = self.call_values(closure, call_args);
                self.builder
                    .ins()
                    .return_call_indirect(self.sig_call, target, &values);
            }
            Callee::SelfRec(block) => {
                let block_args = self.call_block_args(self.rator, call_args);
                self.builder.ins().jump(block, &block_args);
            }
        }
    }

    fn jump_to_linear_block(
        &mut self,
        procedure: &Procedure<'gc>,
        target: crate::cps::linear::BlockId,
        args: &[LinearAtom<'gc>],
    ) {
        let block = procedure
            .blocks
            .iter()
            .find(|block| block.id == target)
            .expect("linear jump target should exist");
        let fixed_count = if block.variadic.is_some() {
            block.params.len() - 1
        } else {
            block.params.len()
        };
        if args.len() < fixed_count {
            self.raise_wrong_linear_block_arity(args, -(fixed_count as isize));
            return;
        }

        let mut block_args = args[..fixed_count]
            .iter()
            .copied()
            .map(|arg| BlockArg::Value(self.linear_atom(arg)))
            .collect::<Vec<_>>();

        if let Some(variadic) = block.variadic {
            let variadic_is_referenced = procedure
                .sources
                .get(&variadic)
                .is_some_and(|source| source.is_referenced());
            if variadic_is_referenced {
                let mut ls = self
                    .builder
                    .ins()
                    .iconst(types::I64, Value::null().bits() as i64);
                for arg in args[fixed_count..].iter().rev().copied() {
                    let arg = self.linear_atom(arg);
                    ls = self.cons(arg, ls);
                }
                block_args.push(BlockArg::Value(ls));
            } else {
                let null = self
                    .builder
                    .ins()
                    .iconst(types::I64, Value::null().bits() as i64);
                block_args.push(BlockArg::Value(null));
            }
        } else if args.len() != fixed_count {
            self.raise_wrong_linear_block_arity(args, fixed_count as isize);
            return;
        }

        let clif_block = self.linear_blockmap[&target];
        self.builder.ins().jump(clif_block, &block_args);
    }

    fn raise_wrong_linear_block_arity(&mut self, args: &[LinearAtom<'gc>], expected: isize) {
        let got = self
            .builder
            .ins()
            .iconst(types::I64, Value::new(args.len() as i32).bits() as i64);
        let expected = self
            .builder
            .ins()
            .iconst(types::I64, Value::new(expected as i32).bits() as i64);
        self.emit_raise(
            RaiseKind::WrongNumberOfArguments,
            &[self.rator, got, expected],
            Value::new(false),
        );
    }

    fn linear_branch_target(&mut self, procedure: &Procedure<'gc>, target: &BranchTarget<'gc>) {
        match target {
            BranchTarget::Local { block, args } => {
                self.jump_to_linear_block(procedure, *block, args)
            }
            BranchTarget::Reified { continuation, args } => {
                self.linear_tail_call(*continuation, args, Value::new(false));
            }
        }
    }

    fn linear_terminator(&mut self, procedure: &Procedure<'gc>, terminator: &Terminator<'gc>) {
        match terminator {
            Terminator::Call {
                callee,
                retk,
                args,
                source,
            } => self.linear_call(*callee, *retk, args, *source),
            Terminator::TailCall {
                callee,
                args,
                source,
            } => self.linear_tail_call(*callee, args, *source),
            Terminator::Raise { kind, args, source } => self.linear_raise(*kind, args, *source),
            Terminator::Jump { target, args } => {
                self.jump_to_linear_block(procedure, *target, args);
            }
            Terminator::Branch {
                test,
                consequent,
                alternative,
                hints: _,
            } => {
                let truthy = self.linear_atom_for_cond(*test);
                let kcons = self.builder.create_block();
                let kalt = self.builder.create_block();

                self.builder.ins().brif(truthy, kcons, &[], kalt, &[]);
                self.builder.switch_to_block(kalt);
                self.linear_branch_target(procedure, alternative);

                self.builder.switch_to_block(kcons);
                self.linear_branch_target(procedure, consequent);
            }
            Terminator::Switch {
                kind,
                scrutinee,
                cases,
                default,
            } => self.linear_switch(procedure, *kind, *scrutinee, cases, default),
        }
    }

    fn linear_switch(
        &mut self,
        procedure: &Procedure<'gc>,
        kind: SwitchKind,
        scrutinee: LinearAtom<'gc>,
        cases: &[crate::cps::linear::SwitchCase<'gc>],
        default: &BranchTarget<'gc>,
    ) {
        let value = self.linear_atom(scrutinee);
        let switch_block = self.builder.create_block();
        let switch_default_block = self.builder.create_block();
        let type_miss_block = if matches!(
            kind,
            SwitchKind::Eq | SwitchKind::CharEq | SwitchKind::SymbolEq { .. }
        ) {
            switch_default_block
        } else {
            self.builder.create_block()
        };
        let mut case_blocks: Vec<(ir::Block, Vec<&crate::cps::linear::SwitchCase<'gc>>)> =
            Vec::with_capacity(cases.len());
        let mut case_block_by_key: HashMap<u128, usize> = HashMap::new();
        let mut switch = Switch::new();

        let switch_value = match kind {
            SwitchKind::Char | SwitchKind::CharEq => {
                let mask = self
                    .builder
                    .ins()
                    .iconst(types::I64, Value::CHAR_MASK as i64);
                let tag = self.builder.ins().iconst(types::I64, Value::CHAR_TAG);
                let masked = self.builder.ins().band(value, mask);
                let is_char = self.builder.ins().icmp(IntCC::Equal, masked, tag);
                self.builder
                    .ins()
                    .brif(is_char, switch_block, &[], type_miss_block, &[]);
                self.builder.switch_to_block(switch_block);
                let value = self.builder.ins().ushr_imm(value, 16);
                self.builder.ins().ireduce(types::I32, value)
            }
            SwitchKind::Eq | SwitchKind::Fixnum | SwitchKind::Numeric => {
                let is_fixnum = self.is_int32(value);
                self.builder
                    .ins()
                    .brif(is_fixnum, switch_block, &[], type_miss_block, &[]);
                self.builder.switch_to_block(switch_block);
                self.builder.ins().ireduce(types::I32, value)
            }
            SwitchKind::SymbolEq { mask } => {
                let is_symbol = self.has_typ8(value, TypeCode8::SYMBOL.bits());
                self.builder
                    .ins()
                    .brif(is_symbol, switch_block, &[], type_miss_block, &[]);
                self.builder.switch_to_block(switch_block);
                let hash = self.builder.ins().load(
                    types::I64,
                    ir::MemFlags::trusted().with_can_move(),
                    value,
                    offset_of!(Symbol, hash) as i32,
                );
                if mask == u64::MAX {
                    hash
                } else {
                    self.builder.ins().band_imm(hash, mask as i64)
                }
            }
        };

        for case in cases {
            let key = switch_case_key(case.value);
            if let Some(index) = case_block_by_key.get(&key).copied() {
                case_blocks[index].1.push(case);
            } else {
                let block = self.builder.create_block();
                switch.set_entry(key, block);
                case_block_by_key.insert(key, case_blocks.len());
                case_blocks.push((block, vec![case]));
            }
        }

        switch.emit(&mut self.builder, switch_value, switch_default_block);

        self.builder.switch_to_block(switch_default_block);
        self.linear_branch_target(procedure, default);

        if type_miss_block != switch_default_block {
            self.builder.switch_to_block(type_miss_block);
        }
        match kind {
            SwitchKind::Eq | SwitchKind::CharEq | SwitchKind::SymbolEq { .. } => {}
            SwitchKind::Fixnum => {
                let scrutinee = self.linear_atom(scrutinee);
                self.linear_fixnum_switch_error_or_default(scrutinee, cases, procedure, default)
            }
            SwitchKind::Numeric => {
                let scrutinee = self.linear_atom(scrutinee);
                self.linear_numeric_switch_fallback(procedure, scrutinee, cases, default)
            }
            SwitchKind::Char => {
                let scrutinee = self.linear_atom(scrutinee);
                self.linear_char_switch_error_or_default(procedure, scrutinee, default)
            }
        }

        for (block, cases) in case_blocks {
            self.builder.switch_to_block(block);
            if matches!(kind, SwitchKind::SymbolEq { .. }) {
                for (index, case) in cases.iter().enumerate() {
                    let matched_block = self.builder.create_block();
                    let next_block = if index + 1 == cases.len() {
                        switch_default_block
                    } else {
                        self.builder.create_block()
                    };
                    let SwitchCaseValue::Symbol {
                        value: symbol_value,
                        ..
                    } = case.value
                    else {
                        continue;
                    };
                    let symbol = self.atom(Atom::Constant(symbol_value));
                    let matched = self.builder.ins().icmp(IntCC::Equal, value, symbol);
                    self.builder
                        .ins()
                        .brif(matched, matched_block, &[], next_block, &[]);
                    self.builder.switch_to_block(matched_block);
                    self.linear_branch_target(procedure, &case.target);
                    if index + 1 != cases.len() {
                        self.builder.switch_to_block(next_block);
                    }
                }
            } else {
                let Some(case) = cases.first() else {
                    self.linear_branch_target(procedure, default);
                    continue;
                };
                self.linear_branch_target(procedure, &case.target);
            }
        }
    }

    fn linear_fixnum_switch_error_or_default(
        &mut self,
        scrutinee: ir::Value,
        cases: &[crate::cps::linear::SwitchCase<'gc>],
        procedure: &Procedure<'gc>,
        default: &BranchTarget<'gc>,
    ) {
        let Some(case) = cases.first() else {
            self.linear_branch_target(procedure, default);
            return;
        };
        let SwitchCaseValue::Integer(value) = case.value else {
            self.linear_branch_target(procedure, default);
            return;
        };
        let ctx = self.builder.ins().get_pinned_reg(types::I64);
        let constant = self.atom(Atom::Constant(Value::new(value)));
        let _ = self.handle_thunk_call_result(self.thunks.fxeq, &[ctx, scrutinee, constant]);
        self.linear_branch_target(procedure, default);
    }

    fn linear_char_switch_error_or_default(
        &mut self,
        procedure: &Procedure<'gc>,
        scrutinee: ir::Value,
        default: &BranchTarget<'gc>,
    ) {
        let ctx = self.builder.ins().get_pinned_reg(types::I64);
        let _ = self.handle_thunk_call_result(self.thunks.char_to_integer, &[ctx, scrutinee]);
        self.linear_branch_target(procedure, default);
    }

    fn linear_numeric_switch_fallback(
        &mut self,
        procedure: &Procedure<'gc>,
        scrutinee: ir::Value,
        cases: &[crate::cps::linear::SwitchCase<'gc>],
        default: &BranchTarget<'gc>,
    ) {
        let ctx = self.builder.ins().get_pinned_reg(types::I64);
        for case in cases {
            let SwitchCaseValue::Integer(value) = case.value else {
                continue;
            };
            let next = self.builder.create_block();
            let matched = self.builder.create_block();
            let constant = self.atom(Atom::Constant(Value::new(value)));
            let result =
                self.handle_thunk_call_result(self.thunks.number_eq, &[ctx, scrutinee, constant]);
            let truthy = self.to_boolean(result);
            self.builder.ins().brif(truthy, matched, &[], next, &[]);
            self.builder.switch_to_block(matched);
            self.linear_branch_target(procedure, &case.target);
            self.builder.switch_to_block(next);
        }
        self.linear_branch_target(procedure, default);
    }

    pub fn finalize(&mut self) {
        self.builder.switch_to_block(self.exit_block);

        let code = self.builder.block_params(self.exit_block)[0];
        let rator = self.builder.block_params(self.exit_block)[1];
        let argc = self.builder.block_params(self.exit_block)[2];
        let arg0 = self.builder.block_params(self.exit_block)[3];
        let arg1 = self.builder.block_params(self.exit_block)[4];
        let arg2 = self.builder.block_params(self.exit_block)[5];
        let arg3 = self.builder.block_params(self.exit_block)[6];

        self.builder.ins().return_call_indirect(
            self.sig_call,
            code,
            &[rator, argc, arg0, arg1, arg2, arg3],
        );

        self.builder.seal_all_blocks();
    }

    pub fn set_debug_loc(&mut self, loc: Value<'gc>) {
        if loc != Value::new(false) {
            let (file_id, line, column) = self.module_builder.debug_context.get_span_loc(loc);

            let source_loc = self.func_debug_cx.add_dbg_loc(file_id, line, column);
            self.builder.set_srcloc(source_loc);
            self.srcloc = Some(source_loc);
        }
    }

    pub fn check_yield(
        &mut self,
        rator: ir::Value,
        argc: ir::Value,
        args: [ir::Value; REGISTER_ARG_COUNT],
    ) {
        let ctx = self.builder.ins().get_pinned_reg(types::I64);

        let thread = ctx;

        let yieldpoint = self.builder.ins().load(
            types::I32,
            ir::MemFlags::trusted(),
            thread,
            Thread::TAKE_YIELDPOINT_OFFSET as i32,
        );
        let on_yieldpoint = self.builder.create_block();
        let on_no_yieldpoint = self.builder.create_block();
        self.builder.func.layout.set_cold(on_yieldpoint);

        let take_yieldpoint = self.builder.ins().icmp_imm(IntCC::NotEqual, yieldpoint, 0);

        self.builder
            .ins()
            .brif(take_yieldpoint, on_yieldpoint, &[], on_no_yieldpoint, &[]);
        self.builder.switch_to_block(on_yieldpoint);
        {
            let ctx = self.builder.ins().get_pinned_reg(types::I64);
            self.builder.ins().call(
                self.thunks.yieldpoint_block,
                &[ctx, rator, argc, args[0], args[1], args[2], args[3]],
            );
            self.builder.ins().jump(on_no_yieldpoint, &[]);
        }
        self.builder.switch_to_block(on_no_yieldpoint);
    }
}

fn switch_case_key(value: SwitchCaseValue<'_>) -> u128 {
    match value {
        SwitchCaseValue::Integer(value) => value as u32 as u128,
        SwitchCaseValue::Symbol { hash, .. } => hash as u128,
    }
}
