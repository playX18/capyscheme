//! Instance slots: `get_slot`/`put_slot` monomorphic inline caches.

use core::mem::{offset_of, size_of};

use onlineasm::{Label, onlineasm};

use crate::bytecode::def::{OpGetSlot, OpGetSlotMetadata, OpPutSlot};
use crate::heap::cell::Lock;
use crate::runtime::class::SchemeInstance;
use crate::runtime::class::storage::{STORAGE_DATA_OFFSET, STORAGE_STAMP_OFFSET};
use crate::runtime::codegen::gc::GcCodegen;
use crate::runtime::codegen::masm::Masm;
use crate::runtime::codegen::template::{SlowPath, TemplateContext, tmp};
use crate::runtime::interp::profile::{SlotIcEntry, SlotSite};
use crate::runtime::sabi::{self, regs};
use crate::runtime::value::Value;

/// Offset of `SchemeInstance::storage`, the `SlotStorage` reference.
const INSTANCE_STORAGE_OFF: i32 = 8;
/// Offset of the values of a `SlotStorage`.
const STORAGE_DATA_OFF: i32 = STORAGE_DATA_OFFSET as i32;
/// Offset of its layout stamp.
const STORAGE_STAMP_OFF: i32 = STORAGE_STAMP_OFFSET as i32;

const _: () = assert!(size_of::<SchemeInstance<'static>>() == 16);
const _: () = assert!(size_of::<Lock<Value<'static>>>() == 8);

/// The shared hit check of both slot templates: `obj` (a heap cell in
/// `tmp::T1`) against the `SlotSite` of metadata `cache_field` in `kind`.
///
/// On a hit `tmp::T2` holds the validated slot index, `tmp::T1` the
/// instance's `SlotStorage` (whose stamp matched) and `tmp::T0` the site
/// address, every other outcome jumps to `miss` with `RV` zero. The index is
/// in bounds of that storage: both the entry and the storage carry the
/// layout stamp, and a stamp fixes the slot count.
///
/// # Memory ordering
///
/// No fence here: the loads of the entry's fields depend on the address
/// loaded from the site, and the stamp load depends on the storage pointer,
/// so neither can run early on x86-64 or aarch64. A stale entry is either
/// still correct or cleared by a watchpoint, and the grace period
/// covers readers that loaded it earlier.
fn cache_check<C: TemplateContext>(
    masm: &mut Masm,
    context: &mut C,
    kind: usize,
    cache_field: usize,
    miss: Label,
) {
    let dirty = masm.label();
    let hit = masm.label();
    context.metadata(masm, kind, cache_field, tmp::T0, tmp::T2);
    let mono_at = masm
        .assembler()
        .mem(tmp::T0, offset_of!(SlotSite, mono) as i32);
    let header_at = masm.assembler().mem(tmp::T1, sabi::HEADER_OFFSET as i32);
    let class_mask = sabi::CLASS_ID_MASK as i64;
    let class_at = masm
        .assembler()
        .mem(tmp::T2, offset_of!(SlotIcEntry, class_id) as i32);
    let stamp_at = masm
        .assembler()
        .mem(tmp::T2, offset_of!(SlotIcEntry, stamp) as i32);
    let index_at = masm
        .assembler()
        .mem(tmp::T2, offset_of!(SlotIcEntry, index) as i32);
    let storage_at = masm.assembler().mem(tmp::T1, INSTANCE_STORAGE_OFF);
    let storage_stamp_at = masm.assembler().mem(tmp::T1, STORAGE_STAMP_OFF);

    onlineasm!(*masm.assembler();
        loadq {tmp::T2}, {mono_at};
        bqeq {tmp::T2}, 0, {dirty};
        loadq {tmp::T3}, {header_at};
        andq {tmp::T3}, {tmp::T3}, {class_mask};
        loadi {regs::RV}, {class_at};
        bqneq {tmp::T3}, {regs::RV}, {dirty};
        loadq {tmp::T1}, {storage_at};
        loadi {tmp::T3}, {storage_stamp_at};
        loadi {regs::RV}, {stamp_at};
        bqneq {tmp::T3}, {regs::RV}, {dirty};
        loadi {tmp::T2}, {index_at};
        move {regs::RV}, 0;
        jmp {hit};
    );
    masm.bind(dirty);
    onlineasm!(*masm.assembler();
        move {regs::RV}, 0;
        jmp {miss};
    );
    masm.bind(hit);
}

pub fn op_get_slot<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    let miss = masm.label();
    context.load_register(masm, OpGetSlot::OPERAND_OBJ, tmp::T1);
    masm.branch_if_not_cell(tmp::T1, tmp::T0, miss);
    cache_check(
        masm,
        context,
        OpGetSlot::METADATA_KIND,
        OpGetSlot::METADATA_CACHE,
        miss,
    );
    // `T2` is the validated index and `T1` the storage it indexes: load the
    // slot and reject unbound reads.
    let slot = masm
        .assembler()
        .mem_indexed(tmp::T1, tmp::T2, 8, STORAGE_DATA_OFF);
    onlineasm!(*masm.assembler();
        loadq {tmp::T2}, {slot};
        bqeq {tmp::T2}, 0, {miss};
    );

    let profile_disp =
        (offset_of!(OpGetSlotMetadata, profile) - offset_of!(OpGetSlotMetadata, cache)) as i64;
    if context.profiling() {
        onlineasm!(*masm.assembler();
            addp {tmp::T0}, {tmp::T0}, {profile_disp};
        );
    }
    masm.maybe_observe_value_profile(context.profiling(), tmp::T0, tmp::T2, tmp::T3, tmp::T1);
    context.store_register(masm, OpGetSlot::OPERAND_DST, tmp::T2, tmp::T3);
    context.next(masm);
    context.slow_control(masm, miss, SlowPath::GetSlot);
}

pub fn op_put_slot<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    let miss = masm.label();
    context.load_register(masm, OpPutSlot::OPERAND_OBJ, tmp::T1);
    masm.branch_if_not_cell(tmp::T1, tmp::T0, miss);
    cache_check(
        masm,
        context,
        OpPutSlot::METADATA_KIND,
        OpPutSlot::METADATA_CACHE,
        miss,
    );
    // `T2` is the validated index and `T1` the storage it indexes.
    let gc = GcCodegen::new();
    // The barrier needs two scratches (`T0`, `T3`), `src` loads after it.
    let barrier = gc.emit_store_barrier(masm, tmp::T1, tmp::T0, tmp::T3, &[tmp::T2]);
    context.load_register(masm, OpPutSlot::OPERAND_SRC, tmp::T0);
    let slot = masm
        .assembler()
        .mem_indexed(tmp::T1, tmp::T2, 8, STORAGE_DATA_OFF);
    onlineasm!(*masm.assembler();
        storeq {slot}, {tmp::T0};
    );
    context.next(masm);
    gc.emit_store_barrier_slow(masm, context, barrier);
    context.slow_control(masm, miss, SlowPath::PutSlot);
}
