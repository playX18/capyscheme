use super::*;

impl Masm {
    /// [`or_profile_immediate`](Self::or_profile_immediate) into the cell at `addr`.
    pub fn maybe_or_profile_immediate(
        &mut self,
        profiling: bool,
        addr: VReg,
        bits: u32,
        tmp: VReg,
    ) {
        if profiling {
            let cell = self.asm.mem(addr, 0);
            self.or_profile_immediate(cell, bits, tmp);
        }
    }

    /// OR `bits` into the `AtomicU32` at `addr`, storing only when something
    /// new was seen.
    pub fn or_profile_immediate(&mut self, addr: onlineasm::Mem, bits: u32, tmp: VReg) {
        let skip = self.label();
        let bits = bits as i64;
        onlineasm!(self.asm;
            loadi {tmp}, {addr};
            andq {tmp}, {tmp}, {bits};
            bqeq {tmp}, {bits}, {skip};
            loadi {tmp}, {addr};
            orq {tmp}, {tmp}, {bits};
            storei {addr}, {tmp};
        );
        self.bind(skip);
    }

    /// [`observe_value_profile`](Self::observe_value_profile)
    pub fn maybe_observe_value_profile(
        &mut self,
        profiling: bool,
        addr: VReg,
        val: VReg,
        kind: VReg,
        tmp: VReg,
    ) {
        if profiling {
            self.observe_value_profile(addr, val, kind, tmp);
        }
    }

    /// Merge `val` into the `ValueProfile` at `addr`.
    /// `val` and `addr` are preserved, `kind` and
    /// `tmp` are clobbered.
    pub fn observe_value_profile(&mut self, addr: VReg, val: VReg, kind: VReg, tmp: VReg) {
        use crate::runtime::interp::profile::*;
        let cell = self.asm.mem(addr, 0);
        let number_tag = regs::NUMBER_TAG;
        let (not_fixnum, flonum, heap, boolean, or_bits, first, done) = (
            self.label(),
            self.label(),
            self.label(),
            self.label(),
            self.label(),
            self.label(),
            self.label(),
        );
        self.branch_if_not_fixnum(val, tmp, not_fixnum);
        onlineasm!(self.asm;
            move {kind}, {VALUE_FIXNUM as i64};
            jmp {or_bits};
        );
        self.bind(not_fixnum);
        // Not a fixnum, so any `NUMBER_TAG` bit means a flonum.
        onlineasm!(self.asm;
            andq {tmp}, {val}, {number_tag};
            btqnz {tmp}, {flonum};
        );
        self.branch_if_cell(val, tmp, heap);
        onlineasm!(self.asm;
            andq {tmp}, {val}, -2;
            bqeq {tmp}, {crate::runtime::objectmodel::Value::VALUE_FALSE}, {boolean};
            move {kind}, {VALUE_OTHER_IMM as i64};
            jmp {or_bits};
        );
        self.bind(flonum);
        onlineasm!(self.asm;
            move {kind}, {VALUE_FLONUM as i64};
            jmp {or_bits};
        );
        self.bind(boolean);
        onlineasm!(self.asm;
            move {kind}, {VALUE_BOOL as i64};
        );
        self.bind(or_bits);
        self.or_profile_register(cell, kind, tmp, done);

        self.bind(heap);
        self.load_class_id(val, kind);
        onlineasm!(self.asm;
            loadi {tmp}, {cell};
            urshiftq {tmp}, {VALUE_CLASS_SHIFT as i64};
            bqeq {tmp}, {kind}, {done};
            bqeq {tmp}, 0, {first};
            // A different class: set the multi flag unless already set.
            loadi {tmp}, {cell};
            andq {kind}, {tmp}, {VALUE_CELL_MULTI as i64};
            bqneq {kind}, 0, {done};
            orq {tmp}, {tmp}, {VALUE_CELL_MULTI as i64};
            storei {cell}, {tmp};
            jmp {done};
        );
        self.bind(first);
        onlineasm!(self.asm;
            lshiftq {kind}, {VALUE_CLASS_SHIFT as i64};
            loadi {tmp}, {cell};
            orq {tmp}, {tmp}, {kind};
            storei {cell}, {tmp};
        );
        self.bind(done);
    }

    /// OR the bits in register `bits` into the `AtomicU32` at `addr`,
    /// storing only when something new was seen, then jump to `done`.
    fn or_profile_register(&mut self, addr: onlineasm::Mem, bits: VReg, tmp: VReg, done: Label) {
        onlineasm!(self.asm;
            loadi {tmp}, {addr};
            andq {tmp}, {tmp}, {bits};
            bqeq {tmp}, {bits}, {done};
            loadi {tmp}, {addr};
            orq {tmp}, {tmp}, {bits};
            storei {addr}, {tmp};
            jmp {done};
        );
    }

    /// `out` = the class id of the heap object `val`.
    pub fn load_class_id(&mut self, val: VReg, out: VReg) {
        let class_mask = sabi::CLASS_ID_MASK as i64;
        onlineasm!(self.asm;
            loadq {out}, [{val} - 8];
            andq {out}, {out}, {class_mask};
        );
    }

    /// Jump to `slow` unless `SFP + 8*total` fits under the overflow limit.
    pub fn check_frame_limit(&mut self, total: VReg, addr: VReg, limit: VReg, slow: Label) {
        self.check_frame_limit_at(total, addr, limit, slow, sabi::TC_ESP);
    }

    /// [`check_frame_limit`](Self::check_frame_limit) against the limit field
    /// at `limit_offset` from `TC`. An OSR prologue uses `TC_REAL_ESP`: the
    /// transfer's room was checked on the owning thread, but `TC_ESP` is
    /// nulled by *other* threads to request a yieldpoint, and the OSR entry
    /// has no fallback (its failure branch traps).
    pub(crate) fn check_frame_limit_at(
        &mut self,
        total: VReg,
        addr: VReg,
        limit: VReg,
        slow: Label,
        limit_offset: usize,
    ) {
        let (tc, sfp) = (regs::TC, regs::SFP);
        let esp = self.asm.mem(tc, limit_offset as i32);
        // AArch64 folds the index/scale into `lsl` + `add` in the address
        // synthesizer; x86-64 encodes it in the LEA. `total` stays intact.
        let end = self.asm.mem_indexed(sfp, total, 8, 0);
        onlineasm!(self.asm;
            leap {addr}, {end};
        );
        onlineasm!(self.asm;
            loadp {limit}, {esp};
            bqa {addr}, {limit}, {slow};
        );
    }

    /// The yieldpoint poll
    pub fn yieldpoint_check(&mut self, tmp: VReg, slow: Label, is_loop: bool) {
        let tc = regs::TC;
        let slot = self.asm.mem(tc, sabi::TC_YIELDPOINT as i32);
        onlineasm!(self.asm;
            loadi {tmp}, {slot};
            sxi2q {tmp}, {tmp};
        );
        if is_loop {
            onlineasm!(self.asm;
                bqgt {tmp}, 0, {slow};
            );
        } else {
            onlineasm!(self.asm;
                bqneq {tmp}, 0, {slow};
            );
        }
    }

    /// The hotness bump: increment the `CodeBlock` heat counter and jump
    /// to `slow` once it reaches `threshold`.
    pub fn hot_check(&mut self, threshold: u32, step: i64, ptr: VReg, count: VReg, slow: Label) {
        let sfp = regs::SFP;
        let code_block = sabi::CLOSURE_CODE_BLOCK as i32;
        let counter = sabi::CODE_BLOCK_HOT_COUNT as i32;
        let threshold = threshold as i64;
        let closure_at = self.asm.mem(sfp, 8);
        onlineasm!(self.asm;
            loadq {ptr}, {closure_at};
        );
        let block_at = self.asm.mem(ptr, code_block);
        onlineasm!(self.asm;
            loadq {ptr}, {block_at};
        );
        let counter_at = self.asm.mem(ptr, counter);
        onlineasm!(self.asm;
            loadi {count}, {counter_at};
            addq {count}, {count}, {step};
            storei {counter_at}, {count};
            bqgteq {count}, {threshold}, {slow};
        );
    }

    /// Bump heat if the JIT is on, on reaching
    /// [threshold](crate::utils::flags::jit_threshold) jump to `slow`.
    pub fn maybe_hot_check(&mut self, ptr: VReg, count: VReg, slow: Label) {
        self.maybe_hot_check_step(ptr, count, slow, 1);
    }

    /// As [`maybe_hot_check`](Self::maybe_hot_check), adding `step` heat.
    /// Procedure entries add more than loop iterations so a few calls tier up
    /// as quickly as a loop.
    pub fn maybe_hot_check_step(&mut self, ptr: VReg, count: VReg, slow: Label, step: i64) {
        if let Some(threshold) = flags::jit().then_some(flags::jit_threshold() as u32) {
            self.hot_check(threshold, step, ptr, count, slow);
        }
    }

    /// Jump to `slow` unless `x` and `y` are both fixnums.
    pub fn check_fixnums(&mut self, x: VReg, y: VReg, tmp: VReg, slow: Label) {
        let number_tag = regs::NUMBER_TAG;
        onlineasm!(self.asm;
            andq {tmp}, {x}, {y};
            bqb {tmp}, {number_tag}, {slow};
        );
    }

    /// Jump to `target` when `x` and `y` are both fixnums
    pub fn branch_if_fixnums(&mut self, x: VReg, y: VReg, tmp: VReg, target: Label) {
        let number_tag = regs::NUMBER_TAG;
        onlineasm!(self.asm;
            andq {tmp}, {x}, {y};
            bqaeq {tmp}, {number_tag}, {target};
        );
    }

    /// Jump to `slow` unless the fixnum divisor `y` divides safely.
    pub fn check_divisor(&mut self, x: VReg, y: VReg, slow: Label) {
        let number_tag = regs::NUMBER_TAG;

        onlineasm!(self.asm;
            bqeq {y}, {number_tag}, {slow};
            bqneq {x}, {i32::MIN as i64}, .no_min_max;
            bqeq {y}, {-1i64}, {slow};
            .no_min_max:
        );
    }
}
