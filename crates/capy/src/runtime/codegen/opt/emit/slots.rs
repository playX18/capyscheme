//! Instance slots in Tier 2 code: the inline entry check of `SlotGet` and
//! `SlotPut` (see `ir::inst::Op::SlotGet`).
//!
//! The check is the baseline template's monomorphic cache check
//! (`template/emit/slots.rs`, `cache_check`), with the same meaning:
//!
//! 1. `obj` is a heap cell whose header class is the entry's class.
//! 2. The instance's `SlotStorage` carries the entry's layout stamp. A stamp
//!    fixes the slot count, so the entry's index is in bounds.
//! 3. The slot is bound (for a read).
//!
//! The entry is read from the site at run time, not baked in. Watchpoints
//! clear a site's cache when a class's slot protocol changes (`clear_slot_site`
//! in `interp/slow.rs`), so a cleared entry makes the guard fail, and the
//! deopt sends the access to baseline. That is the same protection the
//! baseline template has. Tier 2 never fills a cache itself.
//!
//! HOOK(invalidation): once the watchpoint plumbing registers `Dependent::Code`
//! on the class's slot-protocol set, a unit could drop the runtime load and
//! bake the entry in. It must keep the class and stamp guards either way.

use core::mem::offset_of;

use onlineasm::{Label, VReg as MReg, onlineasm};

use crate::bytecode::def::{OpGetSlotMetadata, OpPutSlotMetadata};
use crate::runtime::objectmodel::class::storage::{STORAGE_DATA_OFFSET, STORAGE_STAMP_OFFSET};
use crate::runtime::interp::profile::{SlotIcEntry, SlotSite};
use crate::runtime::sabi;

use super::super::lir::{DeoptExit, VReg as LVReg};
use super::{Deferred, Emitter, T0, T1, T2, T3};

/// Offset of `SchemeInstance::storage`, which holds the `SlotStorage`.
const INSTANCE_STORAGE_OFF: i32 = 8;
const STORAGE_DATA_OFF: i32 = STORAGE_DATA_OFFSET as i32;
const STORAGE_STAMP_OFF: i32 = STORAGE_STAMP_OFFSET as i32;

/// Where the inline entry of a `get_slot` site sits, from the metadata address.
const GET_MONO_OFF: i32 = (offset_of!(OpGetSlotMetadata, cache) + offset_of!(SlotSite, mono)) as i32;
/// The same for a `put_slot` site.
const PUT_MONO_OFF: i32 = (offset_of!(OpPutSlotMetadata, cache) + offset_of!(SlotSite, mono)) as i32;

impl<'a> Emitter<'a> {
    /// `dst <- obj.slot` for the `get_slot` site at `meta`.
    pub(super) fn slot_get(&mut self, dst: LVReg, obj: LVReg, meta: u64, exit: DeoptExit) -> Result<(), String> {
        let fail = self.exit_label(exit);
        if self.stress_guard(fail) {
            return Ok(());
        }
        let o = self.read(obj, T0)?;
        self.slot_probe(o, GET_MONO_OFF, meta, fail);
        // T0 holds the storage and T3 the index.
        let slot = self.masm.assembler().mem_indexed(T0, T3, 8, STORAGE_DATA_OFF);
        onlineasm!(*self.asm();
            loadq {T2}, {slot};
            bqeq {T2}, 0, {fail};
        );
        // Only now is `dst` written. A spilled `dst` uses T2 as its temporary,
        // which is free here.
        let t = self.write(dst)?;
        onlineasm!(*self.asm(); move {t.reg}, {T2};);
        self.commit(t);
        Ok(())
    }

    /// `obj.slot <- src` for the `put_slot` site at `meta`. The store and its
    /// write barrier follow the check, so a deopt happens before any write.
    pub(super) fn slot_put(&mut self, obj: LVReg, src: LVReg, meta: u64, exit: DeoptExit) -> Result<(), String> {
        let fail = self.exit_label(exit);
        if self.stress_guard(fail) {
            return Ok(());
        }
        let o = self.read(obj, T0)?;
        self.slot_probe(o, PUT_MONO_OFF, meta, fail);
        // T0 holds the storage and T3 the index. The barrier names the
        // storage, which is the object written, and runs before the store:
        // the SATB slow path snapshots the storage's current fields, so it
        // must still see the value being overwritten. Its call keeps T0 and
        // T3 for the store after it.
        let (slow, join) = (self.masm.label(), self.masm.label());
        self.gc.emit_unlogged_check(&mut self.masm, T0, T1, T2, slow);
        self.masm.bind(join);
        self.deferred.push(Deferred::Barrier {
            slow,
            join,
            obj: T0,
            preserve: &[T0, T3],
        });
        // T1 is free again.
        let s = self.read(src, T1)?;
        let slot = self.masm.assembler().mem_indexed(T0, T3, 8, STORAGE_DATA_OFF);
        onlineasm!(*self.asm(); storeq {slot}, {s};);
        Ok(())
    }

    /// Checks the inline entry at `cache_off` of the site at `meta` against
    /// `obj` (a register that is not `T0`-`T3`). On success `T0` holds the
    /// instance's storage, `T3` the entry's index, and `T1`/`T2` are clobbered.
    /// Every failure branches to `fail` before anything is written.
    fn slot_probe(&mut self, obj: MReg, cache_off: i32, meta: u64, fail: Label) {
        let meta = meta as i64;
        let class_mask = sabi::CLASS_ID_MASK as i64;
        onlineasm!(*self.asm(); move {T0}, {obj};);
        self.masm.branch_if_not_cell(T0, T2, fail);
        onlineasm!(*self.asm(); move {T1}, {meta};);
        {
            let mono = self.masm.assembler().mem(T1, cache_off);
            onlineasm!(*self.asm(); loadq {T1}, {mono}; bqeq {T1}, 0, {fail};);
        }
        // Class: the header's class id against the entry's.
        {
            let class_at = self.masm.assembler().mem(T1, offset_of!(SlotIcEntry, class_id) as i32);
            onlineasm!(*self.asm(); loadi {T2}, {class_at};);
        }
        {
            let header_at = self.masm.assembler().mem(T0, sabi::HEADER_OFFSET as i32);
            onlineasm!(*self.asm();
                loadq {T3}, {header_at};
                andq {T3}, {T3}, {class_mask};
                bqneq {T2}, {T3}, {fail};
            );
        }
        // Layout: the storage's stamp against the entry's. The storage pointer
        // replaces the object in T0 only after the class is known to match.
        {
            let storage_at = self.masm.assembler().mem(T0, INSTANCE_STORAGE_OFF);
            onlineasm!(*self.asm(); loadq {T0}, {storage_at};);
        }
        {
            let storage_stamp_at = self.masm.assembler().mem(T0, STORAGE_STAMP_OFF);
            onlineasm!(*self.asm(); loadi {T2}, {storage_stamp_at};);
        }
        {
            let stamp_at = self.masm.assembler().mem(T1, offset_of!(SlotIcEntry, stamp) as i32);
            onlineasm!(*self.asm();
                loadi {T3}, {stamp_at};
                bqneq {T2}, {T3}, {fail};
            );
        }
        {
            let index_at = self.masm.assembler().mem(T1, offset_of!(SlotIcEntry, index) as i32);
            onlineasm!(*self.asm(); loadi {T3}, {index_at};);
        }
    }
}
