//! Lazy slot sites: `get_slot`/`put_slot` misses materialize a direct,
//! class- and layout-stamp-guarded access.
//!
//! The inline template only checks the site's single inline (`mono`) entry;
//! every other class, including the polymorphic ones the slow body has
//! already cached, runs the whole instruction through the Rust slow path.
//! This site keeps that miss edge lazy: the first miss runs the existing
//! slow path (`capy_get_slot`/`capy_put_slot`, which fills the cache), then
//! materializes a stub that checks every live cache entry:
//!
//! - the object's class id against the entry's, and its storage's layout
//!   stamp against the entry's, exactly as the template's inline check does;
//! - then the slot load, or the slot store with the same write barrier as
//!   the template.

use crate::runtime::codegen::import::ImportKey;
use std::sync::atomic::{AtomicU64, Ordering::Relaxed};

use onlineasm::onlineasm;

use crate::runtime::codegen::addr::BytecodePc;
use crate::runtime::codegen::emission::Emission;
use crate::runtime::codegen::gc::GcCodegen;
use crate::runtime::codegen::jit::context::{
    DecodedInstruction, FixedSpec, OriginExits, StubContext,
};
use crate::runtime::codegen::masm::Masm;
use crate::runtime::codegen::template::ArithShape;
use crate::runtime::codegen::template::tmp;
use crate::runtime::codegen::template_image::declare_slow_paths;
use crate::runtime::interp::profile::{SLOT_POLY_ENTRIES, SlotIcEntry, SlotSite as SlotCache};
use crate::runtime::interp::stack::Ip;
use crate::runtime::objectmodel::class::storage::{STORAGE_DATA_OFFSET, STORAGE_STAMP_OFFSET};
use crate::runtime::sabi::{CLASS_ID_MASK, HEADER_OFFSET, regs};

use super::{CellState, HitCx, Lazy, Materialize, MaterializeCx};

/// Offset of `SchemeInstance::storage`.
const INSTANCE_STORAGE_OFF: i32 = 8;
/// Offsets of a slot site's inline entry and polymorphic array.
const MONO_OFF: i32 = core::mem::offset_of!(SlotCache, mono) as i32;
const POLY_OFF: i32 = core::mem::offset_of!(SlotCache, poly) as i32;
/// `Array` data offset (the length is a raw word at 0).
const ARRAY_DATA: i32 = 8;
/// `SlotIcEntry` field offsets.
const ENTRY_CLASS: i32 = core::mem::offset_of!(SlotIcEntry, class_id) as i32;
const ENTRY_STAMP: i32 = core::mem::offset_of!(SlotIcEntry, stamp) as i32;
const ENTRY_INDEX: i32 = core::mem::offset_of!(SlotIcEntry, index) as i32;

/// One lazy `get_slot`/`put_slot` site.
pub(crate) struct SlotSite {
    pc: usize,
    ip: Ip,
    /// The instruction after this one, for the stub's exit.
    next_pc: usize,
    is_put: bool,
    /// Address of the site's live `SlotSite` cache in its metadata.
    meta: usize,
    /// Operand fields of the object and of the loaded/stored value.
    obj: i32,
    arg: i32,
    sig: AtomicU64,
}

impl SlotSite {
    pub(crate) fn new(
        pc: usize,
        ip: Ip,
        next_pc: usize,
        is_put: bool,
        meta: usize,
        obj: i32,
        arg: i32,
    ) -> Self {
        Self {
            pc,
            ip,
            next_pc,
            is_put,
            meta,
            obj,
            arg,
            sig: AtomicU64::new(0),
        }
    }

    fn cache(&self) -> &SlotCache {
        // SAFETY: the metadata of a live code buffer, which outlives the
        // unit that owns this cell.
        unsafe { &*(self.meta as *const SlotCache) }
    }

    /// A signature of the live cache: enough to tell whether a hit observed
    /// a new entry.
    fn signature(&self) -> u64 {
        let cache = self.cache();
        let mono = cache.mono();
        let poly = cache.poly();
        let poly_len = poly.map_or(0, |poly| poly.len()).min(SLOT_POLY_ENTRIES);
        let mut sig = (self.is_put as u64) ^ ((poly_len as u64) << 1) ^ (mono.is_some() as u64);
        let mut mix = |entry: &SlotIcEntry<'_>| {
            let packed = ((entry.class_id as u64) << 32) | entry.stamp as u64;
            sig = sig
                .wrapping_mul(0x9E37_79B9_7F4A_7C15)
                .wrapping_add(packed ^ entry.index as u64);
        };
        if let Some(entry) = mono.as_ref() {
            mix(entry);
        }
        if let Some(poly) = poly {
            for entry in poly.iter().take(poly_len) {
                mix(entry);
            }
        }
        sig
    }
}

impl Lazy for SlotSite {
    const NAME: &'static str = "slot_site";

    fn pc(&self) -> usize {
        self.pc
    }

    fn ip(&self) -> Ip {
        self.ip
    }

    fn log_context(&self) -> String {
        let cache = self.cache();
        let poly = cache.poly().map_or(0, |poly| poly.len());
        format!(
            "meta={:#x} mono={} poly={poly}",
            self.meta,
            cache.mono().is_some()
        )
    }

    /// Run the whole instruction through the existing slow path (which
    /// fills the cache), then materialize a stub over the fresh entries.
    fn hit<'gc>(&self, cx: &mut HitCx<'gc>) -> *const u8 {
        let th = cx.mutator().as_ptr();
        // SAFETY: the hit block staged the instruction's frame and `ip`,
        // and the slow path resumes exactly as a boxed slow edge would.
        let next = unsafe {
            if self.is_put {
                crate::runtime::codegen::slow::capy_put_slot(th)
            } else {
                crate::runtime::codegen::slow::capy_get_slot(th)
            }
        };
        cx.materialize(self);
        next
    }

    fn materialize<'gc>(&self, cx: &mut MaterializeCx<'gc>) -> Materialize {
        let cache = self.cache();
        let has_mono = cache.mono().is_some();
        let poly_len = cache
            .poly()
            .map_or(0, |poly| poly.len())
            .min(SLOT_POLY_ENTRIES);
        if !has_mono && poly_len == 0 {
            // No cache entry to guard yet: leave the cell live so a later
            // hit that fills the cache can materialize.
            return Materialize::Later;
        }
        let sig = self.signature();
        if cx.state() == CellState::Specialized && self.sig.load(Relaxed) == sig {
            return Materialize::Later;
        }
        let outcome = self.emit(cx, has_mono, poly_len);
        if matches!(outcome, Materialize::Code { .. }) {
            self.sig.store(sig, Relaxed);
        }
        outcome
    }
}

impl SlotSite {
    fn emit<'gc>(
        &self,
        cx: &mut MaterializeCx<'gc>,
        has_mono: bool,
        poly_len: usize,
    ) -> Materialize {
        let Some(pc) = BytecodePc::new(self.next_pc) else {
            return Materialize::Sticky("next pc");
        };
        let Some(_) = cx.origin_table().lookup(pc) else {
            return Materialize::Sticky("no next instruction");
        };

        let mut masm = cx.masm();
        let paths = declare_slow_paths(&mut masm);
        let start = masm.label();
        masm.bind(start);
        let cell = cx.cell_index();
        let (t0, t1, t2, t3) = (tmp::T0, tmp::T1, tmp::T2, tmp::T3);
        let do_access = masm.label();
        let poly_first = masm.label();
        let fallback = masm.label();
        let obj_at = masm.assembler().mem(regs::SFP, 8 * self.obj);
        let meta_at = masm.assembler().mem(t1, MONO_OFF);
        let poly_at = masm.assembler().mem(t1, POLY_OFF);
        let header_at = masm.assembler().mem(t0, HEADER_OFFSET as i32);
        let storage_at = masm.assembler().mem(t0, INSTANCE_STORAGE_OFF);
        let storage_stamp_at = masm.assembler().mem(t1, STORAGE_STAMP_OFFSET as i32);
        let entry_class_at = masm.assembler().mem(t1, ENTRY_CLASS);
        let entry_stamp_at = masm.assembler().mem(t1, ENTRY_STAMP);
        let entry_index_at = masm.assembler().mem(t1, ENTRY_INDEX);

        // The site's inline entry. Offsets are built as `Mem`s: the
        // assembler's inline `[reg + expr]` form takes an index register, not
        // a displacement.
        if has_mono {
            let miss = if poly_len > 0 { poly_first } else { fallback };
            onlineasm!(*masm.assembler();
                move {t1}, {self.meta as i64};
                loadq {t1}, {meta_at};
                bqeq {t1}, 0, {miss};
                loadq {t0}, {obj_at};
            );
            masm.branch_if_not_cell(t0, t2, miss);
            onlineasm!(*masm.assembler();
                loadq {t3}, {header_at};
                andq {t3}, {t3}, {CLASS_ID_MASK as i64};
                loadi {t2}, {entry_class_at};
                bqneq {t2}, {t3}, {miss};
                loadi {t3}, {entry_index_at};
                loadi {t2}, {entry_stamp_at};
                loadq {t1}, {storage_at};
                loadi {t0}, {storage_stamp_at};
                bqneq {t0}, {t2}, {miss};
                jmp {do_access};
            );
        }

        // The polymorphic entries. The cache is re-read live: the array may
        // have been cleared or rebuilt since the stub was materialized.
        masm.bind(poly_first);
        for index in 0..poly_len {
            let next = masm.label();
            let data_at = masm.assembler().mem(t1, ARRAY_DATA + index as i32 * 8);
            let len_at = masm.assembler().mem(t1, 0);
            onlineasm!(*masm.assembler();
                move {t1}, {self.meta as i64};
                loadq {t1}, {poly_at};
                bqeq {t1}, 0, {next};
                loadi {t2}, {len_at};
                move {t3}, {index as i64};
                bqlteq {t2}, {t3}, {next};
                loadq {t1}, {data_at};
                bqeq {t1}, 0, {next};
                loadq {t0}, {obj_at};
            );
            masm.branch_if_not_cell(t0, t2, next);
            onlineasm!(*masm.assembler();
                loadq {t3}, {header_at};
                andq {t3}, {t3}, {CLASS_ID_MASK as i64};
                loadi {t2}, {entry_class_at};
                bqneq {t2}, {t3}, {next};
                loadi {t3}, {entry_index_at};
                loadi {t2}, {entry_stamp_at};
                loadq {t1}, {storage_at};
                loadi {t0}, {storage_stamp_at};
                bqneq {t0}, {t2}, {next};
                jmp {do_access};
            );
            masm.bind(next);
        }
        onlineasm!(*masm.assembler(); jmp {fallback};);

        // The direct access, shared by every entry. The slot address is
        // `storage + 8 * index`; the index register is consumed here.
        masm.bind(do_access);
        if self.is_put {
            let gc = GcCodegen::new();
            let barrier = gc.emit_store_barrier(&mut masm, t1, t0, t2, &[t3]);
            onlineasm!(*masm.assembler();
                lshiftq {t3}, 3;
                addp {t1}, {t1}, {t3};
            );
            let slot = masm.assembler().mem(t1, STORAGE_DATA_OFFSET as i32);
            let src = masm.assembler().mem(regs::SFP, 8 * self.arg);
            onlineasm!(*masm.assembler();
                loadq {t0}, {src};
                storeq {slot}, {t0};
            );
            emit_next(&mut masm, t0, self.next_pc);
            let mut emission = Emission::default();
            let mut context = StubContext::new(
                self.decoded(),
                OriginExits {
                    origin_table: cx.origin_table(),
                },
                FixedSpec {
                    shape: ArithShape::Full,
                    cell: None,
                },
                &mut emission,
                paths,
            );
            gc.emit_store_barrier_slow(&mut masm, &mut context, barrier);
            drop(context);
        } else {
            onlineasm!(*masm.assembler();
                lshiftq {t3}, 3;
                addp {t1}, {t1}, {t3};
            );
            let slot = masm.assembler().mem(t1, STORAGE_DATA_OFFSET as i32);
            onlineasm!(*masm.assembler();
                loadq {t0}, {slot};
                bqeq {t0}, 0, {fallback};
            );
            let dst = masm.assembler().mem(regs::SFP, 8 * self.arg);
            onlineasm!(*masm.assembler(); storeq {dst}, {t0};);
            emit_next(&mut masm, t0, self.next_pc);
        }

        // Any guard failure re-enters the cell, which runs the slow path
        // again and materializes a stub over the grown cache.
        masm.bind(fallback);
        super::call::emit_cell_hit(&mut masm, self.ip, cell);

        let name = format!("slot_site:{:#x}:{}", self.meta, self.pc);
        match cx.install(masm, &name, start) {
            Ok(_) => Materialize::Code { again: true },
            Err(err) => cx.fail(err),
        }
    }

    /// The instruction record the write barrier's operation call needs (its
    /// arguments are registers, so only `ip` is meaningful).
    fn decoded(&self) -> DecodedInstruction {
        DecodedInstruction {
            op: crate::bytecode::def::OpcodeId::OpNop,
            width: bytecodegen::OperandWidth::Narrow,
            ip: self.ip,
            len: 0,
            entry: self.ip,
            constant_pointer: 0,
            md: 0,
        }
    }
}

fn emit_next(masm: &mut Masm, scratch: onlineasm::VReg, next_pc: usize) {
    masm.load_import(scratch, ImportKey::Origin(next_pc as u32));
    onlineasm!(*masm.assembler(); jmp {scratch};);
}
