use mmtk::BarrierSelector;
use onlineasm::{Label, VReg, onlineasm};
use smallvec::ToSmallVec;

use super::GcCodegen;
use crate::runtime::codegen::masm::{Masm, RuntimeExtern};
use crate::runtime::codegen::template::{
    OpArg, OpContinue, OpResult, OpTarget, OperationCall, TemplateContext,
};

/// A barrier whose check is emitted, waiting for its slow block.
#[must_use = "emit the slow block with `GcCodegen::emit_store_barrier_slow`"]
pub struct PendingBarrier {
    slow: Label,
    join: Label,
    obj: VReg,
    preserve: smallvec::SmallVec<[VReg; 8]>,
}

impl GcCodegen {
    /// The barrier before a store into `obj`: check the object's unlog
    /// bit, and bind the join the slow block returns to. `obj` and
    /// `preserve` survive the slow path, every other caller-saved register
    /// does not. Clobbers `t` and `t2`. `None` for `NoBarrier`.
    pub fn emit_store_barrier(
        &self,
        masm: &mut Masm,
        obj: VReg,
        t: VReg,
        t2: VReg,
        preserve: &[VReg],
    ) -> Option<PendingBarrier> {
        if self.info.barrier == BarrierSelector::NoBarrier {
            return None;
        }
        let (slow, join) = (masm.label(), masm.label());
        self.emit_unlogged_check(masm, obj, t, t2, slow);
        masm.bind(join);
        let mut preserve = preserve.to_smallvec();
        preserve.push(obj);
        Some(PendingBarrier {
            slow,
            join,
            obj,
            preserve,
        })
    }

    pub fn emit_store_barrier_slow<C: TemplateContext>(
        &self,
        masm: &mut Masm,
        context: &mut C,
        pending: Option<PendingBarrier>,
    ) {
        let Some(pending) = pending else {
            return;
        };
        context.call_operation(
            masm,
            pending.slow,
            OperationCall {
                target: OpTarget::Runtime(RuntimeExtern::LogObject),
                args: smallvec::smallvec![OpArg::Reg(pending.obj)],
                preserve: pending.preserve,
                result: OpResult::None,
                checks_error: false,
                cont: OpContinue::Join(pending.join),
            },
        );
    }

    /// Jump to `slow` when `obj`'s unlog bit is set
    pub fn emit_unlogged_check(&self, masm: &mut Masm, obj: VReg, t: VReg, t2: VReg, slow: Label) {
        match self.info.barrier {
            BarrierSelector::NoBarrier => {}
            BarrierSelector::ObjectBarrier | BarrierSelector::SATBBarrier => {
                let byte = masm.assembler().mem(t, 0);
                onlineasm!(*masm.assembler();
                    move {t}, {obj};
                    urshiftp {t}, 6;
                    move {t2}, {self.info.unlog_side_metadata_base as i64};
                    addp {t}, {t}, {t2};
                    loadb {t2}, {byte};
                    move {t}, {obj};
                    urshiftp {t}, 3;
                    andp {t}, {t}, 7;
                    urshiftp {t2}, {t};
                    btpnz {t2}, 1, {slow};
                );
            }
            BarrierSelector::FieldBarrier => unreachable!("no supported plan uses a field barrier"),
        }
    }
}
