use crate::rsgc::mmtk::BarrierSelector;
use cranelift::prelude::{InstBuilder, IntCC, MemFlags, types};
use cranelift_codegen::ir::{self, BlockArg};
use std::mem::{offset_of, size_of};

use crate::{
    compiler::ssa::{AllocationHeaderPreset, SSABuilder},
    cps::term::{Atom, ContRef, FuncRef},
    expander::core::LVarRef,
    runtime::value::{Pair, Value, Vector},
};

impl<'gc, 'a, 'f> SSABuilder<'gc, 'a, 'f> {
    pub fn to_boolean(&mut self, v: ir::Value) -> ir::Value {
        self.builder
            .ins()
            .icmp_imm(IntCC::NotEqual, v, Value::new(false).bits() as i64)
    }

    pub fn cons(&mut self, a: ir::Value, b: ir::Value) -> ir::Value {
        let pair = self.alloc_with_header_word_preset(
            AllocationHeaderPreset::Pair,
            size_of::<Pair>(),
            None,
        );
        self.builder.ins().store(
            ir::MemFlags::trusted(),
            a,
            pair,
            offset_of!(Pair, car) as i32,
        );
        self.builder.ins().store(
            ir::MemFlags::trusted(),
            b,
            pair,
            offset_of!(Pair, cdr) as i32,
        );
        pair
    }

    pub fn inline_cmp_op(
        &mut self,
        lhs: ir::Value,
        rhs: ir::Value,
        i32_fastpath: impl FnOnce(&mut Self, ir::Value, ir::Value, ir::Block) -> ir::Value,
        slowpath: impl FnOnce(&mut Self, ir::Value, ir::Value) -> ir::Value,
    ) -> ir::Value {
        /*let mask_lhs_i32 = self.builder.ins().band_imm(lhs, Value::NUMBER_TAG as i64);
        let is_lhs_i32 =
            self.builder
                .ins()
                .icmp_imm(IntCC::Equal, mask_lhs_i32, Value::NUMBER_TAG as i64);
        let mask_rhs_i32 = self.builder.ins().band_imm(rhs, Value::NUMBER_TAG as i64);
        let is_rhs_i32 =
            self.builder
                .ins()
                .icmp_imm(IntCC::Equal, mask_rhs_i32, Value::NUMBER_TAG as i64);
        let is_both_i32 = self.builder.ins().band(is_lhs_i32, is_rhs_i32);

        let fastpath_bb = self.builder.create_block();*/

        let slowpath_bb = self.builder.create_block();
        let check_rhs = self.builder.create_block();
        let fastpath_bb = self.builder.create_block();
        let join = self.builder.create_block();

        self.builder.append_block_param(join, types::I8);
        self.builder.func.layout.set_cold(slowpath_bb);
        self.branch_if_int32(lhs, check_rhs, &[], slowpath_bb, &[]);
        self.builder.switch_to_block(check_rhs);
        {
            self.branch_if_int32(rhs, fastpath_bb, &[], slowpath_bb, &[]);
        }

        self.builder.switch_to_block(fastpath_bb);
        {
            let lhs_i32 = self.builder.ins().ireduce(types::I32, lhs);
            let rhs_i32 = self.builder.ins().ireduce(types::I32, rhs);

            let res = i32_fastpath(self, lhs_i32, rhs_i32, slowpath_bb);

            self.builder.ins().jump(join, &[BlockArg::Value(res)]);
        }
        self.builder.switch_to_block(slowpath_bb);
        {
            let res = slowpath(self, lhs, rhs);
            self.builder.ins().jump(join, &[BlockArg::Value(res)]);
        }

        self.builder.switch_to_block(join);
        self.builder.block_params(join)[0]
    }

    pub fn inline_binary_op(
        &mut self,
        lhs: ir::Value,
        rhs: ir::Value,
        i32_fastpath: impl FnOnce(&mut Self, ir::Value, ir::Value, ir::Block) -> ir::Value,
        slowpath: impl FnOnce(&mut Self, ir::Value, ir::Value) -> ir::Value,
    ) -> ir::Value {
        let check_rhs_i32 = self.builder.create_block();
        let fastpath_bb = self.builder.create_block();
        let slowpath_bb = self.builder.create_block();
        let join = self.builder.create_block();

        self.builder.append_block_param(join, types::I64);
        self.builder.func.layout.set_cold(slowpath_bb);

        let mask_lhs_i32 = self.builder.ins().band_imm(lhs, Value::NUMBER_TAG);
        let is_lhs_i32 = self
            .builder
            .ins()
            .icmp_imm(IntCC::Equal, mask_lhs_i32, Value::NUMBER_TAG);
        self.builder
            .ins()
            .brif(is_lhs_i32, check_rhs_i32, &[], slowpath_bb, &[]);
        self.builder.switch_to_block(check_rhs_i32);

        let mask_rhs_i32 = self.builder.ins().band_imm(rhs, Value::NUMBER_TAG);
        let is_rhs_i32 = self
            .builder
            .ins()
            .icmp_imm(IntCC::Equal, mask_rhs_i32, Value::NUMBER_TAG);

        self.builder
            .ins()
            .brif(is_rhs_i32, fastpath_bb, &[], slowpath_bb, &[]);

        self.builder.switch_to_block(fastpath_bb);
        {
            let lhs_i32 = self.builder.ins().ireduce(types::I32, lhs);
            let rhs_i32 = self.builder.ins().ireduce(types::I32, rhs);

            let res = i32_fastpath(self, lhs_i32, rhs_i32, slowpath_bb);
            let res = self.builder.ins().uextend(types::I64, res);
            let res = self.builder.ins().bor_imm(res, Value::NUMBER_TAG);
            self.builder.ins().jump(join, &[BlockArg::Value(res)]);
        }
        self.builder.switch_to_block(slowpath_bb);
        {
            let res = slowpath(self, lhs, rhs);
            self.builder.ins().jump(join, &[BlockArg::Value(res)]);
        }

        self.builder.switch_to_block(join);
        self.builder.block_params(join)[0]
    }

    pub fn inline_float_unary_op(
        &mut self,
        value: ir::Value,
        fastpath: impl FnOnce(&mut Self, ir::Value) -> ir::Value,
        slowpath: impl FnOnce(&mut Self, ir::Value) -> ir::Value,
    ) -> ir::Value {
        let is_f64 = self.is_flonum(value);
        let fastpath_bb = self.builder.create_block();
        let slowpath_bb = self.builder.create_block();
        let join = self.builder.create_block();

        self.builder.append_block_param(join, types::I64);

        self.builder
            .ins()
            .brif(is_f64, fastpath_bb, &[], slowpath_bb, &[]);

        self.builder.switch_to_block(fastpath_bb);
        {
            let f64_encode_off = self
                .builder
                .ins()
                .iconst(types::I64, Value::DOUBLE_ENCODE_OFFSET as i64);
            let value_bits = self.builder.ins().isub(value, f64_encode_off);
            let value_f64 = self
                .builder
                .ins()
                .bitcast(types::F64, MemFlags::new(), value_bits);
            let res = fastpath(self, value_f64);
            let res = self.builder.ins().bitcast(types::I64, MemFlags::new(), res);
            let res = self
                .builder
                .ins()
                .iadd_imm(res, Value::DOUBLE_ENCODE_OFFSET as i64);
            self.builder.ins().jump(join, &[BlockArg::Value(res)]);
        }
        self.builder.switch_to_block(slowpath_bb);
        {
            let res = slowpath(self, value);
            self.builder.ins().jump(join, &[BlockArg::Value(res)]);
        }

        self.builder.switch_to_block(join);
        self.builder.block_params(join)[0]
    }

    pub fn inline_unary_op(
        &mut self,
        value: ir::Value,
        fastpath: impl FnOnce(&mut Self, ir::Value, ir::Block) -> ir::Value,
        slowpath: impl FnOnce(&mut Self, ir::Value) -> ir::Value,
    ) -> ir::Value {
        let mask = self.builder.ins().band_imm(value, Value::NUMBER_TAG);
        let is_i32 = self
            .builder
            .ins()
            .icmp_imm(IntCC::Equal, mask, Value::NUMBER_TAG);

        let fastpath_bb = self.builder.create_block();
        let slowpath_bb = self.builder.create_block();
        let join = self.builder.create_block();

        self.builder.append_block_param(join, types::I64);

        self.builder
            .ins()
            .brif(is_i32, fastpath_bb, &[], slowpath_bb, &[]);

        self.builder.switch_to_block(fastpath_bb);
        {
            let value_i32 = self.builder.ins().ireduce(types::I32, value);
            let res = fastpath(self, value_i32, slowpath_bb);
            let res = self.builder.ins().uextend(types::I64, res);
            let res = self.builder.ins().bor_imm(res, Value::NUMBER_TAG);
            self.builder.ins().jump(join, &[BlockArg::Value(res)]);
        }
        self.builder.switch_to_block(slowpath_bb);
        {
            let res = slowpath(self, value);
            self.builder.ins().jump(join, &[BlockArg::Value(res)]);
        }

        self.builder.switch_to_block(join);
        self.builder.block_params(join)[0]
    }

    pub fn is_int32(&mut self, v: ir::Value) -> ir::Value {
        let tag = self.builder.ins().band_imm(v, Value::NUMBER_TAG);
        self.builder
            .ins()
            .icmp_imm(IntCC::Equal, tag, Value::NUMBER_TAG)
    }

    pub fn is_flonum(&mut self, v: ir::Value) -> ir::Value {
        let tag = self.builder.ins().band_imm(v, Value::NUMBER_TAG);
        let not_zero = self.builder.ins().icmp_imm(IntCC::NotEqual, v, 0);
        let not_i32 = self
            .builder
            .ins()
            .icmp_imm(IntCC::NotEqual, tag, Value::NUMBER_TAG);
        self.builder.ins().band(not_i32, not_zero)
    }

    pub fn vector_ref_imm(&mut self, vec: ir::Value, ix: usize) -> ir::Value {
        let offset = Vector::OFFSET_OF_DATA as i32 + (ix as i32 * 8);
        self.builder.ins().load(
            types::I64,
            ir::MemFlags::trusted().with_can_move(),
            vec,
            offset,
        )
    }

    pub fn vector_set_imm(&mut self, vec: ir::Value, ix: usize, value: ir::Value) {
        let offset = Vector::OFFSET_OF_DATA as i32 + (ix as i32 * 8);
        self.builder
            .ins()
            .store(ir::MemFlags::trusted().with_can_move(), value, vec, offset);
    }

    pub fn vector_ref(&mut self, vec: ir::Value, ix: ir::Value) -> ir::Value {
        let mut offset = self.builder.ins().imul_imm(ix, 8);
        offset = self
            .builder
            .ins()
            .iadd_imm(offset, Vector::OFFSET_OF_DATA as i64);
        let addr = self.builder.ins().iadd(vec, offset);
        self.builder
            .ins()
            .load(types::I64, ir::MemFlags::trusted().with_can_move(), addr, 0)
    }

    pub fn vector_set(&mut self, vec: ir::Value, ix: ir::Value, value: ir::Value) {
        let mut offset = self.builder.ins().imul_imm(ix, 8);
        offset = self
            .builder
            .ins()
            .iadd_imm(offset, Vector::OFFSET_OF_DATA as i64);
        let addr = self.builder.ins().iadd(vec, offset);
        self.builder
            .ins()
            .store(ir::MemFlags::trusted().with_can_move(), value, addr, 0);
    }

    pub fn branch_if_int32(
        &mut self,
        v: ir::Value,
        then: ir::Block,
        then_args: &[BlockArg],
        else_: ir::Block,
        else_args: &[BlockArg],
    ) {
        let tag = self.builder.ins().band_imm(v, Value::NUMBER_TAG);
        let is_int = self
            .builder
            .ins()
            .icmp_imm(IntCC::Equal, tag, Value::NUMBER_TAG);
        self.builder
            .ins()
            .brif(is_int, then, then_args, else_, else_args);
    }

    pub fn is_immediate(&mut self, v: ir::Value) -> ir::Value {
        let tag = self.builder.ins().band_imm(v, Value::NOT_CELL_MASK);
        self.builder.ins().icmp_imm(IntCC::NotEqual, tag, 0)
    }

    pub fn is_heap_object(&mut self, v: ir::Value) -> ir::Value {
        let tag = self.builder.ins().band_imm(v, Value::NOT_CELL_MASK);
        let non_zero = self.builder.ins().icmp_imm(IntCC::NotEqual, v, 0);
        let is_cell = self.builder.ins().icmp_imm(IntCC::Equal, tag, 0);

        self.builder.ins().band(is_cell, non_zero)
    }

    pub fn as_boolean(&mut self, v: ir::Value) -> ir::Value {
        let tr = self
            .builder
            .ins()
            .iconst(types::I64, Value::new(true).raw_i64());
        let fs = self
            .builder
            .ins()
            .iconst(types::I64, Value::new(false).raw_i64());
        self.builder.ins().select(v, tr, fs)
    }

    pub fn has_class_id(&mut self, v: ir::Value, class_id: u32) -> ir::Value {
        let class_id = self.builder.ins().iconst(types::I32, class_id as i64);
        let call = self
            .builder
            .ins()
            .call(self.thunks.has_class_id, &[v, class_id]);
        self.builder.inst_results(call)[0]
    }

    pub fn has_any_class_id(&mut self, v: ir::Value, class_ids: &[u32]) -> ir::Value {
        assert!(
            !class_ids.is_empty(),
            "has_any_class_id requires at least one class ID"
        );

        let mut result = self.has_class_id(v, class_ids[0]);
        for &class_id in &class_ids[1..] {
            let next = self.has_class_id(v, class_id);
            result = self.builder.ins().bor(result, next);
        }
        result
    }

    pub fn branch_if_immediate(
        &mut self,
        v: ir::Value,
        then: ir::Block,
        then_args: &[BlockArg],
        else_: ir::Block,
        else_args: &[BlockArg],
    ) {
        let is_immediate = self.is_immediate(v);
        self.builder
            .ins()
            .brif(is_immediate, then, then_args, else_, else_args);
    }

    pub fn branch_if_heap_object(
        &mut self,
        v: ir::Value,
        then: ir::Block,
        then_args: &[BlockArg],
        else_: ir::Block,
        else_args: &[BlockArg],
    ) {
        let is_heap_object = self.is_heap_object(v);
        self.builder
            .ins()
            .brif(is_heap_object, then, then_args, else_, else_args);
    }

    pub fn branch_if_has_class_id(
        &mut self,
        v: ir::Value,
        class_id: u32,
        then: ir::Block,
        then_args: &[BlockArg],
        else_: ir::Block,
        else_args: &[BlockArg],
    ) {
        let has_class_id = self.has_class_id(v, class_id);
        self.builder
            .ins()
            .brif(has_class_id, then, then_args, else_, else_args);
    }

    pub fn branch_if_has_any_class_id(
        &mut self,
        v: ir::Value,
        class_ids: &[u32],
        then: ir::Block,
        then_args: &[BlockArg],
        else_: ir::Block,
        else_args: &[BlockArg],
    ) {
        let has_class_id = self.has_any_class_id(v, class_ids);
        self.builder
            .ins()
            .brif(has_class_id, then, then_args, else_, else_args);
    }

    pub fn handle_thunk_call_result(
        &mut self,
        thunk: ir::FuncRef,
        args: &[ir::Value],
    ) -> ir::Value {
        let call = self.builder.ins().call(thunk, args);

        let code = self.builder.inst_results(call)[0];
        let value = self.builder.inst_results(call)[1];

        let is_error = self.builder.ins().icmp_imm(IntCC::NotEqual, code, 0);

        let on_error = self.builder.create_block();
        let on_success = self.builder.create_block();
        self.builder.append_block_param(on_error, types::I64);
        self.builder.append_block_param(on_success, types::I64);
        self.builder.func.layout.set_cold(on_error);

        self.builder.ins().brif(
            is_error,
            on_error,
            &[BlockArg::Value(value)],
            on_success,
            &[BlockArg::Value(value)],
        );
        self.builder.switch_to_block(on_error);
        {
            let value = self.builder.block_params(on_error)[0];
            self.raise_to_exception_handler(value);
        }
        self.builder.switch_to_block(on_success);
        self.builder.block_params(on_success)[0]
    }

    pub fn debug_local(&mut self, lvar: LVarRef<'gc>, val: ir::Value) {
        let srcloc = self.srcloc;
        let label = self.func_debug_cx.add_variable(lvar, srcloc);
        self.builder.set_val_label(val, label);
    }

    pub fn get_srcloc(&mut self, src: Value<'gc>) -> Option<ir::SourceLoc> {
        if src != Value::new(false) {
            let (file_id, line, column) = self.module_builder.debug_context.get_span_loc(src);

            let source_loc = self.func_debug_cx.add_dbg_loc(file_id, line, column);
            Some(source_loc)
        } else {
            None
        }
    }

    pub fn debug_local_with_source(&mut self, lvar: LVarRef<'gc>, val: ir::Value, src: Value<'gc>) {
        let srcloc = self.get_srcloc(src);
        let label = self.func_debug_cx.add_variable(lvar, srcloc);
        self.builder.set_val_label(val, label);
    }

    /// Given CPS function, construct metadata value for it.
    pub fn meta_for_func(&mut self, f: FuncRef<'gc>) -> ir::Value {
        self.atom(Atom::Constant(f.meta))
    }

    pub fn meta_for_cont(&mut self, c: ContRef<'gc>) -> ir::Value {
        self.atom(Atom::Constant(c.meta))
    }

    pub fn ireduce(&mut self, to: types::Type, v: ir::Value) -> ir::Value {
        let vty = self.builder.func.dfg.value_type(v);
        if to.bits() < vty.bits() {
            self.builder.ins().ireduce(to, v)
        } else if to.bits() > vty.bits() {
            self.builder.ins().uextend(to, v)
        } else {
            v
        }
    }

    pub fn sextend(&mut self, to: types::Type, v: ir::Value) -> ir::Value {
        let vty = self.builder.func.dfg.value_type(v);
        if to.bits() < vty.bits() {
            self.builder.ins().ireduce(to, v)
        } else if to.bits() > vty.bits() {
            self.builder.ins().sextend(to, v)
        } else {
            v
        }
    }

    pub fn zextend(&mut self, to: types::Type, v: ir::Value) -> ir::Value {
        let vty = self.builder.func.dfg.value_type(v);
        if to.bits() < vty.bits() {
            self.builder.ins().ireduce(to, v)
        } else if to.bits() > vty.bits() {
            self.builder.ins().uextend(to, v)
        } else {
            v
        }
    }

    pub fn emit_set_vo_bit(&mut self, object_ref: ir::Value) {
        let vo_bit_side_metadata =
            self.import_data(self.module_builder.vo_bit_side_metadata_base_address);
        let meta_base_address = self
            .builder
            .ins()
            .global_value(types::I64, vo_bit_side_metadata);
        let meta_base_address = self.builder.ins().load(
            types::I64,
            ir::MemFlags::trusted().with_can_move(),
            meta_base_address,
            0,
        );
        let shifted_addr = self.builder.ins().ushr_imm(object_ref, 6);
        let meta_addr = self.builder.ins().iadd(meta_base_address, shifted_addr);
        let shift = self.builder.ins().ushr_imm(object_ref, 3);
        let shift = self.builder.ins().band_imm(shift, 0b111);

        let byte_val = self.builder.ins().load(
            types::I8,
            ir::MemFlags::trusted().with_can_move(),
            meta_addr,
            0,
        );
        let byte_i64 = self.builder.ins().uextend(types::I64, byte_val);
        let one = self.builder.ins().iconst(types::I64, 1);
        let mask = self.builder.ins().ishl(one, shift);
        let new_byte_i64 = self.builder.ins().bor(byte_i64, mask);
        let new_byte = self.builder.ins().ireduce(types::I8, new_byte_i64);
        self.builder
            .ins()
            .store(ir::MemFlags::trusted(), new_byte, meta_addr, 0);
    }

    pub fn pre_write_barrier(&mut self, src: ir::Value, offset: i32, target: ir::Value) {
        let _ = (src, offset, target);

        match self.module_builder.ctx.mc.barrier() {
            BarrierSelector::SATBBarrier => {
                let done = self.builder.create_block();
                let check_wb = self.builder.create_block();
                let slowpath = self.builder.create_block();

                self.builder.func.layout.set_cold(slowpath);
                self.branch_if_immediate(src, done, &[], check_wb, &[]);

                self.builder.switch_to_block(check_wb);
                {
                    let global_side_metadata =
                        self.import_data(self.module_builder.global_side_metadata_base_address);
                    let meta_base_address = self
                        .builder
                        .ins()
                        .global_value(types::I64, global_side_metadata);
                    let meta_base_address = self.builder.ins().load(
                        types::I64,
                        ir::MemFlags::trusted().with_can_move(),
                        meta_base_address,
                        0,
                    );
                    let shifted_addr = self.builder.ins().ushr_imm(src, 6);
                    let meta_addr = self.builder.ins().iadd(meta_base_address, shifted_addr);
                    let shift = self.builder.ins().ushr_imm(src, 3);
                    let shift = self.builder.ins().band_imm(shift, 0b111);

                    let byte_val = self.builder.ins().load(
                        types::I8,
                        ir::MemFlags::trusted().with_can_move(),
                        meta_addr,
                        0,
                    );

                    let shifted_val = self.builder.ins().ushr(byte_val, shift);
                    let masked = self.builder.ins().band_imm(shifted_val, 1);
                    let is_set = self.builder.ins().icmp_imm(IntCC::Equal, masked, 1);

                    self.builder.ins().brif(is_set, slowpath, &[], done, &[]);
                }
                self.builder.switch_to_block(slowpath);
                {
                    let ctx = self.builder.ins().get_pinned_reg(types::I64);
                    let offset = self.builder.ins().iconst(types::I32, offset as i64);
                    self.builder.ins().call(
                        self.thunks.post_write_barrier_slow,
                        &[ctx, src, offset, target],
                    );
                    self.builder.ins().jump(done, &[]);
                }
                self.builder.switch_to_block(done);
                /*
                let ctx = self.builder.ins().get_pinned_reg(types::I64);
                let offset = self.builder.ins().iconst(types::I32, offset as i64);
                self.builder
                    .ins()
                    .call(self.thunks.pre_write_barrier, &[ctx, src, offset, target]);*/
            }

            BarrierSelector::NoBarrier | BarrierSelector::ObjectBarrier => { /* no-op */ }
        }
    }

    pub fn post_write_barrier(&mut self, src: ir::Value, offset: i32, target: ir::Value) {
        match self.module_builder.ctx.mc.barrier() {
            BarrierSelector::NoBarrier => {}
            BarrierSelector::SATBBarrier => { /* no-op */ }
            BarrierSelector::ObjectBarrier => {
                let done = self.builder.create_block();
                let check_wb = self.builder.create_block();
                let slowpath = self.builder.create_block();

                self.builder.func.layout.set_cold(slowpath);
                self.branch_if_immediate(src, done, &[], check_wb, &[]);

                self.builder.switch_to_block(check_wb);
                {
                    let global_side_metadata =
                        self.import_data(self.module_builder.global_side_metadata_base_address);
                    let meta_base_address = self
                        .builder
                        .ins()
                        .global_value(types::I64, global_side_metadata);
                    let meta_base_address = self.builder.ins().load(
                        types::I64,
                        ir::MemFlags::trusted().with_can_move(),
                        meta_base_address,
                        0,
                    );
                    let shifted_addr = self.builder.ins().ushr_imm(src, 6);
                    let meta_addr = self.builder.ins().iadd(meta_base_address, shifted_addr);
                    let shift = self.builder.ins().ushr_imm(src, 3);
                    let shift = self.builder.ins().band_imm(shift, 0b111);

                    let byte_val = self.builder.ins().load(
                        types::I8,
                        ir::MemFlags::trusted().with_can_move(),
                        meta_addr,
                        0,
                    );

                    let shifted_val = self.builder.ins().ushr(byte_val, shift);
                    let masked = self.builder.ins().band_imm(shifted_val, 1);
                    let is_set = self.builder.ins().icmp_imm(IntCC::Equal, masked, 1);

                    self.builder.ins().brif(is_set, slowpath, &[], done, &[]);
                }
                self.builder.switch_to_block(slowpath);
                {
                    let ctx = self.builder.ins().get_pinned_reg(types::I64);
                    let offset = self.builder.ins().iconst(types::I32, offset as i64);
                    self.builder.ins().call(
                        self.thunks.post_write_barrier_slow,
                        &[ctx, src, offset, target],
                    );
                    self.builder.ins().jump(done, &[]);
                }
                self.builder.switch_to_block(done);
            }
        }
    }

    pub fn pre_write_barrier_n(&mut self, src: ir::Value, slot: ir::Value, target: ir::Value) {
        let _ = (src, slot, target);
        match self.module_builder.ctx.mc.barrier() {
            BarrierSelector::SATBBarrier => {
                let done = self.builder.create_block();
                let check_wb = self.builder.create_block();
                let slowpath = self.builder.create_block();

                self.builder.func.layout.set_cold(slowpath);
                self.branch_if_immediate(src, done, &[], check_wb, &[]);

                self.builder.switch_to_block(check_wb);
                {
                    let global_side_metadata =
                        self.import_data(self.module_builder.global_side_metadata_base_address);
                    let meta_base_address = self
                        .builder
                        .ins()
                        .global_value(types::I64, global_side_metadata);
                    let meta_base_address = self.builder.ins().load(
                        types::I64,
                        ir::MemFlags::trusted().with_can_move(),
                        meta_base_address,
                        0,
                    );
                    let shifted_addr = self.builder.ins().ushr_imm(src, 6);
                    let meta_addr = self.builder.ins().iadd(meta_base_address, shifted_addr);
                    let shift = self.builder.ins().ushr_imm(src, 3);
                    let shift = self.builder.ins().band_imm(shift, 0b111);
                    let shift = self.builder.ins().ireduce(types::I8, shift);
                    let byte_val = self.builder.ins().load(
                        types::I8,
                        ir::MemFlags::trusted().with_can_move(),
                        meta_addr,
                        0,
                    );

                    let shifted_val = self.builder.ins().ushr(byte_val, shift);
                    let masked = self.builder.ins().band_imm(shifted_val, 1);
                    let is_set = self.builder.ins().icmp_imm(IntCC::Equal, masked, 1);

                    self.builder.ins().brif(is_set, slowpath, &[], done, &[]);
                }
                self.builder.switch_to_block(slowpath);
                {
                    let ctx = self.builder.ins().get_pinned_reg(types::I64);
                    self.builder.ins().call(
                        self.thunks.post_write_barrier_at_slot,
                        &[ctx, src, slot, target],
                    );
                    self.builder.ins().jump(done, &[]);
                }
                self.builder.switch_to_block(done); /*
                let ctx = self.builder.ins().get_pinned_reg(types::I64);
                self.builder.ins().call(
                self.thunks.pre_write_barrier_at_slot,
                &[ctx, src, slot, target],
                );*/
            }

            BarrierSelector::NoBarrier | BarrierSelector::ObjectBarrier => { /* no-op */ }
        }
    }

    pub fn post_write_barrier_n(&mut self, src: ir::Value, slot: ir::Value, target: ir::Value) {
        match self.module_builder.ctx.mc.barrier() {
            BarrierSelector::NoBarrier => {}
            BarrierSelector::SATBBarrier => {}
            BarrierSelector::ObjectBarrier => {
                let done = self.builder.create_block();
                let check_wb = self.builder.create_block();
                let slowpath = self.builder.create_block();

                self.builder.func.layout.set_cold(slowpath);
                self.branch_if_immediate(src, done, &[], check_wb, &[]);

                self.builder.switch_to_block(check_wb);
                {
                    let global_side_metadata =
                        self.import_data(self.module_builder.global_side_metadata_base_address);
                    let meta_base_address = self
                        .builder
                        .ins()
                        .global_value(types::I64, global_side_metadata);
                    let meta_base_address = self.builder.ins().load(
                        types::I64,
                        ir::MemFlags::trusted().with_can_move(),
                        meta_base_address,
                        0,
                    );
                    let shifted_addr = self.builder.ins().ushr_imm(src, 6);
                    let meta_addr = self.builder.ins().iadd(meta_base_address, shifted_addr);
                    let shift = self.builder.ins().ushr_imm(src, 3);
                    let shift = self.builder.ins().band_imm(shift, 0b111);
                    let shift = self.builder.ins().ireduce(types::I8, shift);
                    let byte_val = self.builder.ins().load(
                        types::I8,
                        ir::MemFlags::trusted().with_can_move(),
                        meta_addr,
                        0,
                    );

                    let shifted_val = self.builder.ins().ushr(byte_val, shift);
                    let masked = self.builder.ins().band_imm(shifted_val, 1);
                    let is_set = self.builder.ins().icmp_imm(IntCC::Equal, masked, 1);

                    self.builder.ins().brif(is_set, slowpath, &[], done, &[]);
                }
                self.builder.switch_to_block(slowpath);
                {
                    let ctx = self.builder.ins().get_pinned_reg(types::I64);
                    self.builder.ins().call(
                        self.thunks.post_write_barrier_at_slot,
                        &[ctx, src, slot, target],
                    );
                    self.builder.ins().jump(done, &[]);
                }
                self.builder.switch_to_block(done);
            }
        }
    }
}
