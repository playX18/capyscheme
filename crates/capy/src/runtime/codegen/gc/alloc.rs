//! Allocation: the LAB bump, and `capy_op_alloc` when it fails.

use std::sync::atomic::AtomicU64;

use onlineasm::{Label, VReg, onlineasm};

use super::{FAST_ALLOCS, GcCodegen, alloc_stats};
use crate::runtime::codegen::masm::{Masm, RuntimeExtern};
use crate::runtime::codegen::template::{
    OpArg, OpContinue, OpResult, OpTarget, OperationCall, TemplateContext,
};
use crate::runtime::sabi::{self, regs};

/// The size of an allocation, header included.
#[derive(Clone, Copy)]
pub enum AllocSize {
    /// Known when the code is generated.
    Bytes(usize),
    /// `base + 8 * count`, `count` in a register that survives the
    /// allocation on both paths.
    Words { count: VReg, base: usize },
}

/// An allocation whose fast path is emitted, waiting for its slow block.
#[must_use = "emit the slow block with `GcCodegen::emit_alloc_slow`"]
pub struct PendingAlloc {
    slow: Label,
    join: Label,
    size: AllocSize,
    header_word: u64,
    obj: VReg,
}

impl GcCodegen {
    /// Allocate an object of `size` with header word `header_word` into
    /// `obj`, bumping the LAB when the plan allows. Binds the join both
    /// paths continue from, with the object in `obj` and `count` intact.
    /// Clobbers `end` and `limit` on the fast path, every other caller-saved
    /// register on the slow path.
    pub fn emit_alloc(
        &self,
        masm: &mut Masm,
        size: AllocSize,
        header_word: u64,
        obj: VReg,
        end: VReg,
        limit: VReg,
    ) -> PendingAlloc {
        let (slow, join) = (masm.label(), masm.label());
        if self.info.inline_alloc {
            self.emit_bump(masm, size, header_word, obj, end, limit, slow);
        } else {
            onlineasm!(*masm.assembler(); jmp {slow};);
        }
        masm.bind(join);
        PendingAlloc {
            slow,
            join,
            size,
            header_word,
            obj,
        }
    }

    /// The slow block of [`GcCodegen::emit_alloc`]: call `capy_op_alloc`
    /// and rejoin. Emit it after the template's fast path.
    pub fn emit_alloc_slow<C: TemplateContext>(
        &self,
        masm: &mut Masm,
        context: &mut C,
        pending: PendingAlloc,
    ) {
        let (size, preserve) = match pending.size {
            AllocSize::Bytes(bytes) => (OpArg::Imm(bytes as i64), smallvec::smallvec![]),
            AllocSize::Words { count, base } => (
                OpArg::Scaled {
                    reg: count,
                    shift: 3,
                    add: base as i64,
                },
                smallvec::smallvec![count],
            ),
        };
        context.call_operation(
            masm,
            pending.slow,
            OperationCall {
                target: OpTarget::Runtime(RuntimeExtern::Alloc),
                args: smallvec::smallvec![OpArg::Imm(pending.header_word as i64), size],
                preserve,
                result: OpResult::Reg(pending.obj),
                checks_error: false,
                cont: OpContinue::Join(pending.join),
            },
        );
    }

    /// Bump-allocate from the LAB and store the header: `obj` gets the
    /// object reference. Jumps to `slow` with nothing changed when the LAB
    /// is full or the object belongs in the large object space.
    #[allow(clippy::too_many_arguments)]
    fn emit_bump(
        &self,
        masm: &mut Masm,
        size: AllocSize,
        header_word: u64,
        obj: VReg,
        end: VReg,
        limit: VReg,
        slow: Label,
    ) {
        let tc = regs::TC;
        let eap = masm.assembler().mem(tc, sabi::TC_EAP as i32);
        if let AllocSize::Words { count, base } = size {
            onlineasm!(*masm.assembler();
                move {end}, {count};
                lshiftq {end}, 3;
                addp {end}, {end}, {base as i64};
                bpaeq {end}, {self.info.los_threshold as i64}, {slow};
            );
        }
        #[cfg(target_arch = "x86_64")]
        {
            let ap = masm.assembler().mem(tc, sabi::TC_AP as i32);
            onlineasm!(*masm.assembler(); loadp {obj}, {ap};);
            match size {
                AllocSize::Bytes(bytes) => {
                    let at = masm.assembler().mem(obj, bytes as i32);
                    onlineasm!(*masm.assembler(); leap {end}, {at};);
                }
                AllocSize::Words { .. } => onlineasm!(*masm.assembler(); addp {end}, {obj}, {end};),
            }
            onlineasm!(*masm.assembler();
                loadp {limit}, {eap};
                bpa {end}, {limit}, {slow};
                storep {ap}, {end};
            );
        }
        #[cfg(target_arch = "aarch64")]
        {
            let ap = regs::AP;
            onlineasm!(*masm.assembler(); move {obj}, {ap};);
            match size {
                AllocSize::Bytes(bytes) => {
                    onlineasm!(*masm.assembler(); addp {end}, {obj}, {bytes as i64};)
                }
                AllocSize::Words { .. } => onlineasm!(*masm.assembler(); addp {end}, {obj}, {end};),
            }
            onlineasm!(*masm.assembler();
                loadp {limit}, {eap};
                bpa {end}, {limit}, {slow};
                move {ap}, {end};
            );
        }
        let header = masm.assembler().mem(obj, 0);
        onlineasm!(*masm.assembler();
            move {end}, {header_word as i64};
            storeq {header}, {end};
            addp {obj}, {obj}, {sabi::OBJECT_HEADER_BYTES as i64};
        );
        if alloc_stats() {
            let counter = masm.assembler().mem(end, 0);
            onlineasm!(*masm.assembler();
                move {end}, {&FAST_ALLOCS as *const AtomicU64 as i64};
                loadq {limit}, {counter};
                addq {limit}, {limit}, 1;
                storeq {counter}, {limit};
            );
        }
    }

    /// Store `value` into the `count` slots at `at`. Clobbers `at` and
    /// `count`, `value` survives.
    pub fn emit_fill(&self, masm: &mut Masm, at: VReg, count: VReg, value: VReg) {
        let (top, done) = (masm.label(), masm.label());
        let slot = masm.assembler().mem(at, 0);
        masm.bind(top);
        onlineasm!(*masm.assembler();
            btqz {count}, {done};
            storeq {slot}, {value};
            addp {at}, {at}, 8;
            subq {count}, {count}, 1;
            jmp {top};
        );
        masm.bind(done);
    }
}
