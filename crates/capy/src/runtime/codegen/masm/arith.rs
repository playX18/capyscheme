use super::*;

impl Masm {
    /// Divide guarded fixnums: the quotient (low 32 bits) lands in `quot`,
    /// the remainder (low 32) in `x`, `y` is preserved. Only after
    /// [`Masm::check_divisor`]: the divisor is nonzero and the pair is
    /// not `MIN/-1`, so neither target faults.
    pub fn fixnum_divmod(&mut self, quot: VReg, x: VReg, y: VReg, save: VReg) {
        let (rv, ac) = (regs::RV, regs::AC);
        onlineasm!(self.asm;
            move {save}, {ac};
            move {rv}, {x};
            cdqi;
            idivi {y};
            move {quot}, {rv};
            move {rv}, 0;
            move {x}, {ac};
            move {ac}, {save};
        );
    }

    /// `dst` = boxed `x + y` for boxed fixnums `x` and `y`, jump to `slow` on signed
    /// 32-bit overflow.
    pub fn fixnum_add_checked(&mut self, dst: VReg, x: VReg, y: VReg, tmp: VReg, slow: Label) {
        let _ = tmp;
        onlineasm!(self.asm;
            baddio {dst}, {x}, {y}, {slow};
        );
        self.box_int32(dst, dst);
    }

    /// `dst` = boxed `x + immediate` for boxed fixnum `x` and a raw decoded
    /// immediate.
    pub fn fixnum_add_immediate_checked(
        &mut self,
        dst: VReg,
        x: VReg,
        immediate: i64,
        slow: Label,
    ) {
        onlineasm!(self.asm;
            baddio {dst}, {x}, {immediate}, {slow};
        );
        self.box_int32(dst, dst);
    }

    /// `dst` = boxed `x - y`, like [`Masm::fixnum_add_checked`].
    pub fn fixnum_sub_checked(&mut self, dst: VReg, x: VReg, y: VReg, tmp: VReg, slow: Label) {
        let _ = tmp;
        onlineasm!(self.asm;
            bsubio {dst}, {x}, {y}, {slow};
        );
        self.box_int32(dst, dst);
    }

    /// `dst` = boxed `x * y`, like [`Masm::fixnum_add_checked`].
    pub fn fixnum_mul_checked(&mut self, dst: VReg, x: VReg, y: VReg, tmp: VReg, slow: Label) {
        let _ = tmp;
        onlineasm!(self.asm;
            bmulio {dst}, {x}, {y}, {slow};
        );
        self.box_int32(dst, dst);
    }

    /// `dst` = boxed `-x` for boxed fixnum `x`, jump to `slow` on `MIN`
    /// (which negates to a bignum). There is no checked-negate branch, so
    /// `0 - x` goes through the checked subtract. Clobbers `tmp`.
    pub fn fixnum_neg_checked(&mut self, dst: VReg, x: VReg, tmp: VReg, slow: Label) {
        onlineasm!(self.asm;
            move {tmp}, 0;
            bsubio {dst}, {tmp}, {x}, {slow};
        );
        self.box_int32(dst, dst);
    }

    /// `dst` = boxed `|x|` for boxed fixnum `x`, jump to `slow` on `MIN`.
    /// Non-negative values return themselves, already boxed. Clobbers `tmp`.
    pub fn fixnum_abs_checked(&mut self, dst: VReg, x: VReg, tmp: VReg, slow: Label) {
        let done = self.label();
        onlineasm!(self.asm;
            move {tmp}, 0;
            bilt {x}, {tmp}, .neg;
            move {dst}, {x};
            jmp {done};
            .neg:
            bsubio {dst}, {tmp}, {x}, {slow};
        );
        self.box_int32(dst, dst);
        self.bind(done);
    }

    /// `dst` = boxed `x / y`.
    pub fn fixnum_div_exact(&mut self, dst: VReg, x: VReg, y: VReg, tmp: VReg, slow: Label) {
        self.fixnum_divmod(dst, x, y, tmp);
        // The remainder's upper half is garbage on x86: extend before the
        // full-width test.
        self.unbox_int32(x, x);
        onlineasm!(self.asm;
            bqneq {x}, 0, {slow};
        );
        self.box_int32(dst, dst);
    }

    /// `dst` = boxed truncated `x / y`.
    pub fn fixnum_quotient(&mut self, dst: VReg, x: VReg, y: VReg, tmp: VReg, _slow: Label) {
        self.fixnum_divmod(dst, x, y, tmp);
        self.box_int32(dst, dst);
    }

    /// `dst` = boxed `x % y` for guarded fixnums. Call only after
    /// [`Masm::check_divisor`].
    pub fn fixnum_remainder(&mut self, dst: VReg, x: VReg, y: VReg, tmp: VReg, _slow: Label) {
        self.fixnum_divmod(dst, x, y, tmp);
        // The remainder landed in `x`, the quotient in `dst` is dead.
        self.box_int32(dst, x);
    }

    /// `dst` = boxed `x mod y` for guarded fixnums
    pub fn fixnum_modulo(&mut self, dst: VReg, x: VReg, y: VReg, tmp: VReg, _slow: Label) {
        self.fixnum_divmod(dst, x, y, tmp);
        self.unbox_int32(x, x);
        onlineasm!(self.asm;
            bqeq {x}, 0, .box_it;
            move {tmp}, 0;
            bilt {y}, {tmp}, .rhs_neg;
            bilt {x}, {tmp}, .do_add;
            jmp .box_it;
            .rhs_neg:
            bilt {x}, {tmp}, .box_it;
            .do_add:
            addi {x}, {x}, {y};
            .box_it:
        );

        self.box_int32(dst, x);
    }

    /// `dst = flag ? #t : #f`, for a flag that is exactly 0 or 1. `VALUE_TRUE`
    /// is `VALUE_FALSE | 1`, so the flag is ORed into `#f`. Branch-free.
    pub fn bool_to_value(&mut self, dst: VReg, flag: VReg) {
        let no = Value::VALUE_FALSE;
        onlineasm!(self.asm;
            move {dst}, {flag};
            orq {dst}, {dst}, {no};
        );
    }

    /// A 32-bit signed comparison of boxed fixnums.
    pub fn fixnum_cmp_bool(&mut self, dst: VReg, x: VReg, y: VReg, cond: Cmp) {
        let no = Value::VALUE_FALSE;
        match cond {
            Cmp::Lt => onlineasm!(self.asm; cilt {dst}, {x}, {y};),
            Cmp::Le => onlineasm!(self.asm; cilteq {dst}, {x}, {y};),
            Cmp::Eq => onlineasm!(self.asm; cieq {dst}, {x}, {y};),
            Cmp::Ge => onlineasm!(self.asm; cigteq {dst}, {x}, {y};),
            Cmp::Gt => onlineasm!(self.asm; cigt {dst}, {x}, {y};),
        }
        onlineasm!(self.asm;
            orq {dst}, {dst}, {no};
        );
    }

    /// `dst = box(x + y)` for unboxed doubles `x` and `y`, NaN-canonicalized
    /// like [`Masm::box_double_fpr`].
    pub fn flonum_add(&mut self, dst: VReg, x: VReg, y: VReg, tmp: VReg) {
        onlineasm!(self.asm;
            addd {tmp}, {x}, {y};
        );
        self.box_double_fpr(dst, tmp);
    }

    /// `dst = box(x - y)`, like [`Masm::flonum_add`].
    pub fn flonum_sub(&mut self, dst: VReg, x: VReg, y: VReg, tmp: VReg) {
        onlineasm!(self.asm;
            subd {tmp}, {x}, {y};
        );
        self.box_double_fpr(dst, tmp);
    }

    /// `dst = box(x * y)`, like [`Masm::flonum_add`].
    pub fn flonum_mul(&mut self, dst: VReg, x: VReg, y: VReg, tmp: VReg) {
        onlineasm!(self.asm;
            muld {tmp}, {x}, {y};
        );
        self.box_double_fpr(dst, tmp);
    }

    /// `dst = box(x / y)`, like [`Masm::flonum_add`].
    pub fn flonum_div(&mut self, dst: VReg, x: VReg, y: VReg, tmp: VReg) {
        onlineasm!(self.asm;
            divd {tmp}, {x}, {y};
        );
        self.box_double_fpr(dst, tmp);
    }

    /// `dst = box(-x)` for unboxed double `x`, like [`Masm::flonum_add`].
    pub fn flonum_neg(&mut self, dst: VReg, x: VReg, tmp: VReg) {
        onlineasm!(self.asm;
            negd {tmp}, {x};
        );
        self.box_double_fpr(dst, tmp);
    }

    /// `dst = box(|x|)` for unboxed double `x`.
    pub fn flonum_abs(&mut self, dst: VReg, x: VReg, tmp: VReg) {
        onlineasm!(self.asm;
            absd {tmp}, {x};
        );
        self.box_double_fpr(dst, tmp);
    }

    /// `dst = box(sqrt(x))`
    pub fn flonum_sqrt(&mut self, dst: VReg, x: VReg, tmp: VReg) {
        onlineasm!(self.asm;
            sqrtd {tmp}, {x};
        );
        self.box_double_fpr(dst, tmp);
    }

    /// `dst = box(floor(x))`
    pub fn flonum_floor(&mut self, dst: VReg, x: VReg, tmp: VReg) {
        onlineasm!(self.asm;
            floord {tmp}, {x};
        );
        self.box_double_fpr(dst, tmp);
    }

    /// `dst = box(ceiling(x))`
    pub fn flonum_ceiling(&mut self, dst: VReg, x: VReg, tmp: VReg) {
        onlineasm!(self.asm;
            ceild {tmp}, {x};
        );
        self.box_double_fpr(dst, tmp);
    }

    /// `dst = box(truncate(x))`
    pub fn flonum_truncate(&mut self, dst: VReg, x: VReg, tmp: VReg) {
        onlineasm!(self.asm;
            truncated {tmp}, {x};
        );
        self.box_double_fpr(dst, tmp);
    }

    /// `dst = box(round(x))`
    pub fn flonum_round(&mut self, dst: VReg, x: VReg, tmp: VReg) {
        onlineasm!(self.asm;
            roundd {tmp}, {x};
        );
        self.box_double_fpr(dst, tmp);
    }

    /// `dst = box(trunc(val))`
    pub fn checked_flonum_to_fixnum(
        &mut self,
        dst: VReg,
        val: VReg,
        f0: VReg,
        f1: VReg,
        tmp: VReg,
        slow: Label,
    ) {
        self.unbox_double_fpr(f0, val, tmp);
        onlineasm!(self.asm;
            truncated2is {dst}, {f0};
            sxi2q {dst}, {dst};
            bqlt {dst}, 0, {slow};
            ci2d {f1}, {dst};
            bdneq {f1}, {f0}, {slow};
        );
        self.box_int32(dst, dst);
    }

    /// Jump to `target` when the boxed fixnum `val` is zero (boxed zero is
    /// exactly `NUMBER_TAG`). For the `quotient` leaves, whose zero cases
    /// need the thunk (fixnum zero divides to a fixnum, flonum zeros to
    /// a canonical `+0.0`).
    pub fn branch_if_zero_fixnum(&mut self, val: VReg, target: Label) {
        let number_tag = regs::NUMBER_TAG;
        onlineasm!(self.asm;
            bqeq {val}, {number_tag}, {target};
        );
    }

    /// Jump to `target` when the unboxed double `x` is either zero.
    pub fn branch_if_zero_double(&mut self, x: VReg, ftmp: VReg, gtmp: VReg, target: Label) {
        onlineasm!(self.asm;
            move {gtmp}, 0;
            fq2d {ftmp}, {gtmp};
            bdeq {x}, {ftmp}, {target};
        );
    }

    pub fn flonum_finite_bool(&mut self, dst: VReg, val: VReg, tmp: VReg) {
        let no = Value::VALUE_FALSE;
        self.unbox_double_gpr(tmp, val);
        onlineasm!(self.asm;
            urshiftq {tmp}, 52;
            andq {tmp}, {tmp}, 0x7ff;
            cqlt {dst}, {tmp}, 0x7ff;
            orq {dst}, {dst}, {no};
        );
    }

    pub fn flonum_nan_bool(&mut self, dst: VReg, val: VReg, f0: VReg, tmp: VReg) {
        let (is_nan, done) = (self.label(), self.label());
        self.unbox_double_fpr(f0, val, tmp);
        onlineasm!(self.asm;
            bdnequn {f0}, {f0}, {is_nan};
            move {dst}, {Value::VALUE_FALSE};
            jmp {done};
        );
        self.bind(is_nan);
        onlineasm!(self.asm;
            move {dst}, {Value::VALUE_TRUE};
        );
        self.bind(done);
    }

    pub fn flonum_cmp_bool(&mut self, dst: VReg, x: VReg, y: VReg, cond: Cmp) {
        let no = Value::VALUE_FALSE;
        match cond {
            Cmp::Lt => onlineasm!(self.asm; cdlt {dst}, {x}, {y};),
            Cmp::Le => onlineasm!(self.asm; cdlteq {dst}, {x}, {y};),
            Cmp::Eq => onlineasm!(self.asm; cdeq {dst}, {x}, {y};),
            Cmp::Ge => onlineasm!(self.asm; cdgteq {dst}, {x}, {y};),
            Cmp::Gt => onlineasm!(self.asm; cdgt {dst}, {x}, {y};),
        }
        onlineasm!(self.asm;
            orq {dst}, {dst}, {no};
        );
    }

}
