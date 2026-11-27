//! Instance slots (`get_slot`/`put_slot`) in the builder.

use crate::bytecode::def::{OpGetSlot, OpGetSlotMetadata, OpPutSlot, OpPutSlotMetadata};
use crate::heap::object::ClassId;
use crate::runtime::codegen::opt::ir::frame_state::FrameState;
use crate::runtime::codegen::opt::ir::func::{ClassDep, ClassDepKind, Value as IrValue};
use crate::runtime::codegen::opt::ir::inst::{Constant, MemoryRegion, Op, Terminator};
use crate::runtime::codegen::opt::ir::types::{ClassRef, Type};
use crate::runtime::interp::profile::SlotSite;
use crate::runtime::objectmodel::Value as SchemeValue;
use crate::runtime::objectmodel::class::{ClassCategory, OP_REF, OP_SET, class_table, ic};

use super::{Builder, Flow};

/// The layout an inline entry describes, fixed into the code by
/// [`Builder::baked_slot`].
#[derive(Clone, Copy)]
struct BakedSlot {
    class_id: u32,
    /// `(slot count << 32) | layout stamp`: the first word of the storage of
    /// an instance with this layout.
    layout: u64,
    index: u32,
}

/// Whether a site speculates on its inline entry: the entry is filled and the
/// site has seen one class only. A polymorphic site keeps the helper, which
/// searches the extra entries. A miss would deopt, and the budget would then
/// recompile the unit with the helper.
fn warm(cache: &SlotSite) -> bool {
    cache.mono().is_some() && cache.poly().is_none()
}

impl<'s, 'gc> Builder<'s, 'gc> {
    /// The inline entry of a warm site as constants, with the unit depending
    /// on what makes them true: the class stays the current descriptor for
    /// its id (its layout), and its slot protocol stays the stock one for
    /// `op`. The code then checks only the class and the storage's layout
    /// word, both plain IR that GVN and LICM can share or hoist, where the
    /// `SlotGet`/`SlotPut` probe re-reads the entry on every access.
    ///
    /// `None` when the entry no longer describes the class table: the site
    /// then keeps the probe.
    fn baked_slot(&mut self, cache: &SlotSite, op: usize) -> Option<BakedSlot> {
        let entry = cache.mono()?;
        let class = class_table(self.ctx).lookup(ClassId::new(entry.class_id)?)?;
        let slots = class.slot_count();
        if class.category() != ClassCategory::Scheme
            || class.layout_stamp() != entry.stamp
            || entry.index as usize >= slots
            || !ic::protocol_is_plain(self.ctx, class, op)
        {
            return None;
        }
        let len = u32::try_from(slots).ok()?;
        let value = SchemeValue::from(class);
        for kind in [ClassDepKind::Layout, ClassDepKind::Protocol(op)] {
            if !self
                .func
                .class_deps
                .iter()
                .any(|dep| dep.class == value && dep.kind == kind)
            {
                self.func.class_deps.push(ClassDep {
                    class: value,
                    id: entry.class_id,
                    stamp: entry.stamp,
                    kind,
                });
            }
        }
        Some(BakedSlot {
            class_id: entry.class_id,
            layout: (u64::from(len) << 32) | u64::from(entry.stamp),
            index: entry.index,
        })
    }

    /// The storage of `obj`, deopting unless `obj` is an instance of the
    /// baked class whose storage has the baked layout. An obsolete instance
    /// of a redefined class keeps its class id and its old storage, so the
    /// class check alone does not prove the layout.
    fn baked_storage(&mut self, obj: IrValue, baked: BakedSlot, frame: FrameState) -> IrValue {
        let obj = self.check_class(obj, ClassRef::from_class_id(baked.class_id), frame);
        let storage = self.value_op(
            Op::Load {
                field: 1,
                region: MemoryRegion::Heap,
            },
            &[obj],
            Type::Any,
            None,
        );
        let word = self.value_op(
            Op::Load {
                field: 0,
                region: MemoryRegion::Heap,
            },
            &[storage],
            Type::Int64,
            None,
        );
        let expected = self.value_op(
            Op::Const(Constant::Int64(baked.layout as i64)),
            &[],
            Type::Int64,
            None,
        );
        let same = self.value_op(Op::EqBits, &[word, expected], Type::Bool, None);
        self.guard_true(same, frame);
        storage
    }

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
        let meta = unsafe { OpGetSlot::metadata_ptr(self.buffer.metadata(), op.metadata_id) };
        // SAFETY: as above.
        if let Some(baked) = self.baked_slot(unsafe { &(*meta).cache }, OP_REF) {
            let storage = self.baked_storage(obj, baked, frame);
            let value = self.value_op(
                Op::Load {
                    field: 1 + baked.index,
                    region: MemoryRegion::Heap,
                },
                &[storage],
                Type::Any,
                None,
            );
            // An unbound slot runs `slot-unbound` in baseline.
            let empty = self.bits(SchemeValue::empty().bits());
            let unbound = self.value_op(Op::EqBits, &[value, empty], Type::Bool, None);
            let (cont, fail) = (self.func.new_block(), self.func.new_block());
            self.branch_to(unbound, fail, cont);
            self.cur = fail;
            self.terminate(Terminator::Deopt { frame });
            self.cur = cont;
            self.set(op.dst.0, value)?;
            return Ok(Flow::Next);
        }
        let meta = meta as u64;
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
        let meta = unsafe { OpPutSlot::metadata_ptr(self.buffer.metadata(), op.metadata_id) };
        // SAFETY: as above.
        if let Some(baked) = self.baked_slot(unsafe { &(*meta).cache }, OP_SET) {
            let storage = self.baked_storage(obj, baked, frame);
            // Before the store: the SATB barrier must see the old value.
            self.write_barrier(storage, src);
            self.effect_op(
                Op::Store {
                    field: 1 + baked.index,
                    region: MemoryRegion::Heap,
                },
                &[storage, src],
                None,
            );
            return Ok(Flow::Next);
        }
        let meta = meta as u64;
        self.effect_op(Op::SlotPut { meta }, &[obj, src], Some(frame));
        Ok(Flow::Next)
    }
}
