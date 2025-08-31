use cranelift::prelude::{InstBuilder, IntCC, types};
use cranelift_codegen::ir::{self, BlockArg};

use crate::{compiler::ssa::SSABuilder, runtime::value::Value};

impl<'gc, 'a, 'f> SSABuilder<'gc, 'a, 'f> {
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
}
