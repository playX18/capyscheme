//! Instance slots (`get_slot`/`put_slot`) in the builder.

use crate::bytecode::def::{OpGetSlot, OpGetSlotMetadata, OpPutSlot, OpPutSlotMetadata};
use crate::runtime::codegen::opt::ir::inst::Op;
use crate::runtime::codegen::opt::ir::types::Type;
use crate::runtime::interp::profile::SlotSite;

use super::{Builder, Flow};

/// Whether a site speculates on its inline entry: the entry is filled and the
/// site has seen one class only. A polymorphic site keeps the helper, which
/// searches the extra entries. A miss would deopt, and the budget would then
/// recompile the unit with the helper.
fn warm(cache: &SlotSite) -> bool {
    cache.mono().is_some() && cache.poly().is_none()
}

impl<'s, 'gc> Builder<'s, 'gc> {
    /// Whether the `get_slot` site has a filled inline entry to check.
    pub(super) fn get_slot_warm(&self, op: &OpGetSlot) -> bool {
        // SAFETY: the metadata area holds this opcode's entries
        unsafe {
            let meta: *const OpGetSlotMetadata =
                OpGetSlot::metadata_ptr(self.buffer.metadata(), op.metadata_id);
            warm(&(*meta).cache)
        }
    }

    /// Whether the `put_slot` site has a filled inline entry to check.
    pub(super) fn put_slot_warm(&self, op: &OpPutSlot) -> bool {
        // SAFETY: as in `get_slot_warm`.
        unsafe {
            let meta: *const OpPutSlotMetadata =
                OpPutSlot::metadata_ptr(self.buffer.metadata(), op.metadata_id);
            warm(&(*meta).cache)
        }
    }

    /// `dst = obj.slot` through the inline entry of its site.
    pub(super) fn lower_get_slot(&mut self, pc: usize, op: &OpGetSlot) -> Result<Flow, String> {
        let frame = self.frame(pc, pc)?;
        let obj = self.reg(op.obj.0)?;
        // SAFETY: as in `get_slot_warm`. The address is in the non-moving buffer.
        let meta =
            unsafe { OpGetSlot::metadata_ptr(self.buffer.metadata(), op.metadata_id) as u64 };
        let value = self.value_op(Op::SlotGet { meta }, &[obj], Type::Any, Some(frame));
        self.set(op.dst.0, value)?;
        Ok(Flow::Next)
    }

    /// `obj.slot = src` through the inline entry of its site.
    pub(super) fn lower_put_slot(&mut self, pc: usize, op: &OpPutSlot) -> Result<Flow, String> {
        let frame = self.frame(pc, pc)?;
        let obj = self.reg(op.obj.0)?;
        let src = self.reg(op.src.0)?;
        // SAFETY: as in `lower_get_slot`.
        let meta =
            unsafe { OpPutSlot::metadata_ptr(self.buffer.metadata(), op.metadata_id) as u64 };
        self.effect_op(Op::SlotPut { meta }, &[obj, src], Some(frame));
        Ok(Flow::Next)
    }
}
