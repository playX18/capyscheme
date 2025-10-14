use cranelift::prelude::{InstBuilder, IntCC, types};
use cranelift_codegen::ir::{self, BlockArg};
use rsgc::mmtk::{
    BarrierSelector, util::metadata::side_metadata::GLOBAL_SIDE_METADATA_VM_BASE_ADDRESS,
};

use crate::{
    compiler::ssa::SSABuilder,
    cps::term::{Atom, ContRef, FuncRef},
    expander::core::LVarRef,
    runtime::value::{Symbol, TypeCode8, TypeCode16, Value, Vector},
};

impl<'gc, 'a, 'f> SSABuilder<'gc, 'a, 'f> {
    pub fn to_boolean(&mut self, v: ir::Value) -> ir::Value {
        let cmp = self
            .builder
            .ins()
            .icmp_imm(IntCC::NotEqual, v, Value::new(false).bits() as i64);
        cmp
    }

    pub fn inline_cmp_op(
        &mut self,
        lhs: ir::Value,
        rhs: ir::Value,
        i32_fastpath: impl FnOnce(&mut Self, ir::Value, ir::Value, ir::Block) -> ir::Value,
        slowpath: impl FnOnce(&mut Self, ir::Value, ir::Value) -> ir::Value,
    ) -> ir::Value {
        let mask_lhs_i32 = self.builder.ins().band_imm(lhs, Value::NUMBER_TAG as i64);
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

        let fastpath_bb = self.builder.create_block();

        let slowpath_bb = self.builder.create_block();
        let join = self.builder.create_block();

        self.builder.append_block_param(join, types::I8);

        self.builder
            .ins()
            .brif(is_both_i32, fastpath_bb, &[], slowpath_bb, &[]);

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
        let mask_lhs_i32 = self.builder.ins().band_imm(lhs, Value::NUMBER_TAG as i64);
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

        let fastpath_bb = self.builder.create_block();

        let slowpath_bb = self.builder.create_block();
        let join = self.builder.create_block();

        self.builder.append_block_param(join, types::I64);

        self.builder
            .ins()
            .brif(is_both_i32, fastpath_bb, &[], slowpath_bb, &[]);

        self.builder.switch_to_block(fastpath_bb);
        {
            let lhs_i32 = self.builder.ins().ireduce(types::I32, lhs);
            let rhs_i32 = self.builder.ins().ireduce(types::I32, rhs);

            let res = i32_fastpath(self, lhs_i32, rhs_i32, slowpath_bb);
            let res = self.builder.ins().uextend(types::I64, res);
            let res = self.builder.ins().bor_imm(res, Value::NUMBER_TAG as i64);
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

    pub fn inline_unary_op(
        &mut self,
        value: ir::Value,
        fastpath: impl FnOnce(&mut Self, ir::Value) -> ir::Value,
        slowpath: impl FnOnce(&mut Self, ir::Value) -> ir::Value,
    ) -> ir::Value {
        let mask = self.builder.ins().band_imm(value, Value::NUMBER_TAG as i64);
        let is_i32 = self
            .builder
            .ins()
            .icmp_imm(IntCC::Equal, mask, Value::NUMBER_TAG as i64);

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
            let res = fastpath(self, value_i32);
            let res = self.builder.ins().uextend(types::I64, res);
            let res = self.builder.ins().bor_imm(res, Value::NUMBER_TAG as i64);
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
        let tag = self.builder.ins().band_imm(v, Value::NUMBER_TAG as i64);
        self.builder
            .ins()
            .icmp_imm(IntCC::Equal, tag, Value::NUMBER_TAG as i64)
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
        let tag = self.builder.ins().band_imm(v, Value::NUMBER_TAG as i64);
        let is_int = self
            .builder
            .ins()
            .icmp_imm(IntCC::Equal, tag, Value::NUMBER_TAG as i64);
        self.builder
            .ins()
            .brif(is_int, then, then_args, else_, else_args);
    }

    pub fn is_immediate(&mut self, v: ir::Value) -> ir::Value {
        let tag = self.builder.ins().band_imm(v, Value::NOT_CELL_MASK as i64);
        self.builder.ins().icmp_imm(IntCC::NotEqual, tag, 0)
    }

    pub fn is_heap_object(&mut self, v: ir::Value) -> ir::Value {
        let tag = self.builder.ins().band_imm(v, Value::NOT_CELL_MASK as i64);
        let non_zero = self.builder.ins().icmp_imm(IntCC::NotEqual, v, 0);
        let is_cell = self.builder.ins().icmp_imm(IntCC::Equal, tag, 0);

        self.builder.ins().band(is_cell, non_zero)
    }

    pub fn is_heap_object_tc8(&mut self, v: ir::Value, tc8: TypeCode8) -> ir::Value {
        let if_heap_obj = self.builder.create_block();
        let succ = self.builder.create_block();
        self.builder.append_block_param(succ, types::I64);
        let zero = self.builder.ins().iconst(types::I64, 0);
        let is_cell = self.is_heap_object(v);
        self.builder
            .ins()
            .brif(is_cell, if_heap_obj, &[], succ, &[BlockArg::Value(zero)]);
        self.builder.switch_to_block(if_heap_obj);
        {
            let t =
                self.builder
                    .ins()
                    .load(types::I8, ir::MemFlags::trusted().with_can_move(), v, 0);

            let cmp = self
                .builder
                .ins()
                .icmp_imm(IntCC::Equal, t, tc8.bits() as i64);
            self.builder.ins().jump(succ, &[BlockArg::Value(cmp)]);
        }
        self.builder.switch_to_block(succ);
        self.builder.block_params(succ)[0]
    }

    pub fn is_heap_object_tc16(&mut self, v: ir::Value, tc16: TypeCode16) -> ir::Value {
        let if_heap_obj = self.builder.create_block();
        let succ = self.builder.create_block();
        self.builder.append_block_param(succ, types::I64);
        let zero = self.builder.ins().iconst(types::I64, 0);
        let is_cell = self.is_heap_object(v);
        self.builder
            .ins()
            .brif(is_cell, if_heap_obj, &[], succ, &[BlockArg::Value(zero)]);
        self.builder.switch_to_block(if_heap_obj);
        {
            let t =
                self.builder
                    .ins()
                    .load(types::I16, ir::MemFlags::trusted().with_can_move(), v, 0);

            let cmp = self
                .builder
                .ins()
                .icmp_imm(IntCC::Equal, t, tc16.bits() as i64);
            self.builder.ins().jump(succ, &[BlockArg::Value(cmp)]);
        }
        self.builder.switch_to_block(succ);
        self.builder.block_params(succ)[0]
    }

    pub fn has_typ8(&mut self, v: ir::Value, typ: u8) -> ir::Value {
        let check_object = self.builder.create_block();
        let done = self.builder.create_block();
        self.builder.append_block_param(done, types::I8);

        let false_ = self.builder.ins().iconst(types::I8, 0);
        self.branch_if_immediate(v, done, &[BlockArg::Value(false_)], check_object, &[]);
        self.builder.switch_to_block(check_object);
        {
            let tc8 =
                self.builder
                    .ins()
                    .load(types::I8, ir::MemFlags::trusted().with_can_move(), v, 0);
            let check = self.builder.ins().icmp_imm(IntCC::Equal, tc8, typ as i64);
            self.builder.ins().jump(done, &[BlockArg::Value(check)]);
        }
        self.builder.switch_to_block(done);
        self.builder.block_params(done)[0]
    }

    pub fn has_typ16(&mut self, v: ir::Value, typ: u16) -> ir::Value {
        let check_object = self.builder.create_block();
        let done = self.builder.create_block();
        self.builder.append_block_param(done, types::I8);

        let false_ = self.builder.ins().iconst(types::I8, 0);
        self.branch_if_immediate(v, done, &[BlockArg::Value(false_)], check_object, &[]);
        self.builder.switch_to_block(check_object);
        {
            let tc16 =
                self.builder
                    .ins()
                    .load(types::I16, ir::MemFlags::trusted().with_can_move(), v, 0);
            let check = self.builder.ins().icmp_imm(IntCC::Equal, tc16, typ as i64);
            self.builder.ins().jump(done, &[BlockArg::Value(check)]);
        }

        self.builder.switch_to_block(done);
        self.builder.block_params(done)[0]
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

    pub fn branch_if_has_typ8(
        &mut self,
        v: ir::Value,
        typ: u8,
        then: ir::Block,
        then_args: &[BlockArg],
        else_: ir::Block,
        else_args: &[BlockArg],
    ) {
        let has_typ = self.has_typ8(v, typ);
        self.builder
            .ins()
            .brif(has_typ, then, then_args, else_, else_args);
    }

    pub fn branch_if_has_typ16(
        &mut self,
        v: ir::Value,
        typ: u16,
        then: ir::Block,
        then_args: &[BlockArg],
        else_: ir::Block,
        else_args: &[BlockArg],
    ) {
        let has_typ = self.has_typ16(v, typ);
        self.builder
            .ins()
            .brif(has_typ, then, then_args, else_, else_args);
    }

    pub fn wrong_num_args(&mut self, proc: &str, h: LVarRef<'gc>, got: usize, expected: isize) {
        let c = Symbol::from_str(self.module_builder.ctx, proc);
        let c = self.module_builder.intern_constant(c.into()).unwrap();
        let c = self.import_data(c);

        let addr = self.builder.ins().global_value(types::I64, c);
        let value =
            self.builder
                .ins()
                .load(types::I64, ir::MemFlags::trusted().with_can_move(), addr, 0);
        let ctx = self.builder.ins().get_pinned_reg(types::I64);
        let got = self.builder.ins().iconst(types::I64, got as i64);
        let expected = self.builder.ins().iconst(types::I64, expected as i64);
        let err = self.builder.ins().call(
            self.thunks.wrong_number_of_args,
            &[ctx, value, got, expected],
        );
        let val = self.builder.inst_results(err)[0];
        self.continue_to(h, &[val]);
    }

    pub fn handle_thunk_call_result(
        &mut self,
        thunk: ir::FuncRef,
        args: &[ir::Value],
        handler: LVarRef<'gc>,
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
            self.continue_to(handler, &[value]);
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
        /*let name = if f.name == Value::new(false) {
            Symbol::from_str(self.module_builder.ctx, "<anonymous>").into()
        } else {
            f.name
        };

        let source_location = f.source();
        let ctx = self.module_builder.ctx;
        let ls = list!(ctx, name, source_location, f.binding.name);

        self.atom(Atom::Constant(ls))*/
        self.atom(Atom::Constant(f.meta))
    }

    pub fn meta_for_cont(&mut self, c: ContRef<'gc>) -> ir::Value {
        /*let name = if c.name == Value::new(false) {
            Symbol::from_str(self.module_builder.ctx, "<anonymous>").into()
        } else {
            c.name
        };

        let source_location = c.source();
        let ctx = self.module_builder.ctx;
        let ls = list!(ctx, Value::new(true), name, source_location, c.binding.name);

        self.atom(Atom::Constant(ls))*/
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

    pub fn pre_write_barrier(&mut self, src: ir::Value, offset: i32, target: ir::Value) {
        let _ = (src, offset, target);
    }

    pub fn post_write_barrier(&mut self, src: ir::Value, offset: i32, target: ir::Value) {
        match self.module_builder.ctx.mc.barrier() {
            BarrierSelector::NoBarrier => {}
            BarrierSelector::ObjectBarrier => {
                let done = self.builder.create_block();
                let check_wb = self.builder.create_block();
                let slowpath = self.builder.create_block();

                self.builder.func.layout.set_cold(slowpath);
                self.branch_if_heap_object(src, check_wb, &[], done, &[]);

                self.builder.switch_to_block(check_wb);
                {
                    let meta_base_address = self.builder.ins().iconst(
                        types::I64,
                        GLOBAL_SIDE_METADATA_VM_BASE_ADDRESS.as_usize() as i64,
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
    }

    pub fn post_write_barrier_n(&mut self, src: ir::Value, slot: ir::Value, target: ir::Value) {
        match self.module_builder.ctx.mc.barrier() {
            BarrierSelector::NoBarrier => {}
            BarrierSelector::ObjectBarrier => {
                let done = self.builder.create_block();
                let check_wb = self.builder.create_block();
                let slowpath = self.builder.create_block();

                self.builder.func.layout.set_cold(slowpath);
                self.branch_if_heap_object(src, check_wb, &[], done, &[]);

                self.builder.switch_to_block(check_wb);
                {
                    let meta_base_address = self.builder.ins().iconst(
                        types::I64,
                        GLOBAL_SIDE_METADATA_VM_BASE_ADDRESS.as_usize() as i64,
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
