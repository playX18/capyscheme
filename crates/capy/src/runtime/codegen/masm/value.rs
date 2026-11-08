use super::*;

impl Masm {
    pub fn new() -> Self {
        Self {
            asm: Assembler::native(),
            symbols: Vec::new(),
            cfi: Vec::new(),
            headers: Vec::new(),
            externs: Vec::new(),
            pool_words: std::collections::HashMap::new(),
        }
    }

    pub fn reset(&mut self) {
        self.asm.reset();
        self.symbols.clear();
        self.cfi.clear();
        self.headers.clear();
        self.externs.clear();
        self.pool_words.clear();
    }

    /// Embeds `bits` once as an 8-byte data island and returns its label.
    /// Repeated calls with the same bits share one island.
    pub fn embed_u64(&mut self, bits: u64) -> Label {
        if let Some(&label) = self.pool_words.get(&bits) {
            return label;
        }
        let label = self.asm.embed_u64(bits);
        self.pool_words.insert(bits, label);
        label
    }

    /// `dst = bits` through one PC-relative load from a shared constant
    /// island. One instruction on both targets, instead of a `movabs` /
    /// `movz`+`movk*` sequence per use.
    pub fn load_u64_const(&mut self, dst: VReg, bits: u64) {
        let label = self.embed_u64(bits);
        let at = self.asm.mem_label(label);
        onlineasm!(self.asm;
            loadq {dst}, {at};
        );
    }

    pub fn target(&self) -> Target {
        self.asm.target()
    }

    pub fn assembler(&mut self) -> &mut Assembler {
        &mut self.asm
    }

    pub fn label(&mut self) -> Label {
        self.asm.label()
    }

    pub fn bind(&mut self, label: Label) {
        self.asm.bind(label);
    }

    pub fn align(&mut self, alignment: u32) {
        self.asm.align(alignment);
    }

    /// Pad so that a patchable `jmp` emitted next has its displacement
    /// field 4-byte aligned, which keeps it inside one aligned 8-byte word:
    /// the runtime retargets it with a single atomic 4-byte store
    /// ([`CodeMemory::patch_live_u32`](crate::runtime::code_memory::CodeMemory::patch_live_u32))
    /// while other threads may execute it. AArch64 instructions are already
    /// 4-aligned and the `b` is the field. On x86-64 the rel32 ends the
    /// 5-byte `jmp rel32`, so the jump starts at 3 mod 4: a real `align`
    /// (the assembler keeps it through jump shrinking) and three 1-byte
    /// `nop`s.
    pub fn align_patchable_jump(&mut self) {
        self.asm.align(4);
        if self.asm.target() == Target::X64 {
            for _ in 0..3 {
                onlineasm!(self.asm; nop;);
            }
        }
    }

    pub fn define_symbol(
        &mut self,
        name: impl Into<String>,
        start: Label,
        end: Label,
        kind: SymbolKind,
    ) {
        self.symbols.push(SymbolRecord {
            name: name.into(),
            start,
            end,
            kind,
        });
    }

    pub fn cfi_region(&mut self, start: Label, end: Label, cfa_offset: u32) {
        self.cfi.push(CfiRecord {
            start,
            end,
            cfa_offset,
        });
    }

    /// Declares `name` as a far extern and records it once, so declaring the
    /// same name again returns the same handle.
    fn declare_extern(&mut self, name: ExternalName) -> Extern {
        let ext = self.asm.extern_far(name.clone());
        if !self.externs.contains(&name) {
            self.externs.push(name);
        }
        ext
    }

    /// Declares a Rust helper as a far extern, resolved to its address at
    /// load. Declaring the same helper again returns the same handle.
    pub fn declare_rust_fn(&mut self, helper: RuntimeExtern) -> Extern {
        self.declare_extern(helper.external())
    }

    /// Declares a direct prim wrapper by prim id. The JIT resolves it to
    /// the wrapper's address at load, declaring the same id again returns
    /// the same handle.
    pub fn declare_prim_fn(&mut self, id: u16) -> Extern {
        self.declare_extern(ExternalName::user(EXTERN_NAMESPACE_PRIM, id as u32))
    }

    /// Declares a test-only helper under the test namespace.
    #[cfg(test)]
    pub fn declare_test_fn(&mut self, index: u32) -> Extern {
        self.declare_extern(ExternalName::user(EXTERN_NAMESPACE_TEST, index))
    }

    /// Branch to `target` when `val` is an int32. `NUMBER_TAG` is exactly
    /// the top 15 bits, so `raw >= NUMBER_TAG` (unsigned) iff every tag bit
    /// is set. Reads the reserved tag register, `tmp` is unused, kept so callers need no target-specific scratch.
    pub fn branch_if_fixnum(&mut self, val: VReg, tmp: VReg, target: Label) {
        let _ = tmp;
        let number_tag = regs::NUMBER_TAG;
        onlineasm!(self.asm;
            bqaeq {val}, {number_tag}, {target};
        );
    }

    pub fn branch_if_not_fixnum(&mut self, val: VReg, tmp: VReg, target: Label) {
        let _ = tmp;
        let number_tag = regs::NUMBER_TAG;
        onlineasm!(self.asm;
            bqb {val}, {number_tag}, {target};
        );
    }

    /// Branch to `target` when `val` is a heap object (`raw & NOT_CELL_MASK
    /// == 0`, excluding the empty sentinel). Reads the reserved mask
    /// register. `tmp` is unused; the masked test does not write `val` on
    /// either target, so the test folds into one instruction.
    pub fn branch_if_cell(&mut self, val: VReg, tmp: VReg, target: Label) {
        let _ = tmp;
        let not_cell = regs::NOT_CELL_MASK;
        onlineasm!(self.asm;
            bqeq {val}, 0, .not;
            btqz {val}, {not_cell}, {target};
        .not:
        );
    }

    pub fn branch_if_not_cell(&mut self, val: VReg, tmp: VReg, target: Label) {
        let _ = tmp;
        let not_cell = regs::NOT_CELL_MASK;
        onlineasm!(self.asm;
            bqeq {val}, 0, {target};
            btqnz {val}, {not_cell}, {target};
        );
    }

    /// Branch to `target` when `val` is a boxed double (an inline number that
    /// is not an int32). Reads the reserved tag register. Clobbers `tmp`.
    pub fn branch_if_double(&mut self, val: VReg, tmp: VReg, target: Label) {
        let number_tag = regs::NUMBER_TAG;
        onlineasm!(self.asm;
            andq {tmp}, {val}, {number_tag};
            btqz {tmp}, .not;
            bqeq {tmp}, {number_tag}, .not;
            jmp {target};
        .not:
        );
    }

    pub fn branch_if_not_double(&mut self, val: VReg, tmp: VReg, target: Label) {
        let number_tag = regs::NUMBER_TAG;
        onlineasm!(self.asm;
            andq {tmp}, {val}, {number_tag};
            btqz {tmp}, {target};
            bqeq {tmp}, {number_tag}, {target};
        );
    }

    /// Branch to `target` when `val` is an immediate (`raw &
    /// NOT_CELL_MASK != 0`), for `immediate?`.
    /// `val` is preserved; `tmp` is unused.
    pub fn branch_if_immediate(&mut self, val: VReg, tmp: VReg, target: Label) {
        let _ = tmp;
        let not_cell = regs::NOT_CELL_MASK;
        onlineasm!(self.asm;
            btqnz {val}, {not_cell}, {target};
        );
    }

    /// Branch to `target` when `val` is not an immediate
    pub fn branch_if_not_immediate(&mut self, val: VReg, tmp: VReg, target: Label) {
        let _ = tmp;
        let not_cell = regs::NOT_CELL_MASK;
        onlineasm!(self.asm;
            btqz {val}, {not_cell}, {target};
        );
    }

    /// Branch to `target` unless `val` is a heap object whose header class id
    /// is `class_id`.
    pub fn branch_not_heap_class(&mut self, val: VReg, class_id: u32, tmp: VReg, target: Label) {
        let not_cell = regs::NOT_CELL_MASK;
        let class_mask = sabi::CLASS_ID_MASK as i64;
        let want = class_id as i64;
        onlineasm!(self.asm;
            bqeq {val}, 0, {target};
            btqnz {val}, {not_cell}, {target};
            loadq {tmp}, [{val} - 8];
            andq {tmp}, {tmp}, {class_mask};
            bqneq {tmp}, {want}, {target};
        );
    }

    /// `dst = box(src as i32)`. Only the low 32 bits of `src` are read, the
    /// tag comes from the reserved register.
    pub fn box_int32(&mut self, dst: VReg, src: VReg) {
        let number_tag = regs::NUMBER_TAG;
        onlineasm!(self.asm;
            sxi2q {dst}, {src};
            orq {dst}, {dst}, {number_tag};
        );
    }

    /// `dst = unbox(val) as i32`, sign-extended.
    pub fn unbox_int32(&mut self, dst: VReg, src: VReg) {
        onlineasm!(self.asm;
            sxi2q {dst}, {src};
        );
    }

    /// `dst = box(src)` where `src` holds raw `f64` bits.
    pub fn box_double_gpr(&mut self, dst: VReg, src: VReg) {
        let offset = Value::DOUBLE_ENCODE_OFFSET as i64;
        onlineasm!(self.asm;
            addq {dst}, {src}, {offset};
        );
    }

    /// `dst = unbox(val)` as raw `f64` bits.
    pub fn unbox_double_gpr(&mut self, dst: VReg, src: VReg) {
        let offset = Value::DOUBLE_ENCODE_OFFSET as i64;
        onlineasm!(self.asm;
            subq {dst}, {src}, {offset};
        );
    }

    /// `dst = box(src)` where `src` is an `f64` register.
    pub fn box_double_fpr(&mut self, dst: VReg, src: VReg) {
        let offset = Value::DOUBLE_ENCODE_OFFSET as i64;
        let nan = Value::from_f64(f64::NAN).raw_i64();
        onlineasm!(self.asm;
            bdnequn {src}, {src}, .nan;
            fd2q {dst}, {src};
            addq {dst}, {dst}, {offset};
            jmp .done;
        .nan:
            move {dst}, {nan};
        .done:
        );
    }

    /// `dst = unbox(val)` into an `f64` register. Clobbers `tmp`.
    pub fn unbox_double_fpr(&mut self, dst: VReg, src: VReg, tmp: VReg) {
        let offset = Value::DOUBLE_ENCODE_OFFSET as i64;
        onlineasm!(self.asm;
            subq {tmp}, {src}, {offset};
            fq2d {dst}, {tmp};
        );
    }

    /// `dst = src as f64` for a boxed fixnum `src` into an `f64` register.
    pub fn fixnum_to_double(&mut self, dst: VReg, src: VReg) {
        onlineasm!(self.asm;
            ci2ds {dst}, {src};
        );
    }

    /// `dst = src as f64` for a raw int32 `src`, sign-extended in its word.
    /// The conversion is exact.
    pub fn int32_to_double_fpr(&mut self, dst: VReg, src: VReg) {
        onlineasm!(self.asm;
            ci2d {dst}, {src};
        );
    }

    /// Emits `bytes` of nops (one byte each on x64, four on fixed-width
    /// targets) as placeholder data patched after load.
    // TODO: extend onlineasm's embed_data to reserve data directly.
    fn emit_nop_padding(&mut self, bytes: usize) {
        let nops = match self.asm.target() {
            Target::X64 => bytes,
            _ => bytes / 4,
        };
        for _ in 0..nops {
            onlineasm!(self.asm; nop;);
        }
    }

    /// Reserves `bytes` of 8-aligned header before a new return address
    /// label.
    fn reserve_rp_header(&mut self, bytes: usize, ra: Label, words: HeaderWords) -> Label {
        self.asm.align(8);
        let header = self.asm.label();
        self.asm.bind(header);
        self.emit_nop_padding(bytes);
        self.asm.bind(ra);
        self.headers.push(PendingHeader { header, ra, words });
        ra
    }

    /// Emits an 8-aligned `RpHeader`
    /// (4 words) and returns the label just after it, the machine return
    /// address.
    pub fn emit_rp_header(
        &mut self,
        mv_return_address: HeaderTarget,
        livemask: usize,
        toplink: usize,
        frame_size: isize,
    ) -> Label {
        let ra = self.asm.label();
        self.reserve_rp_header(
            sabi::SIZE_RP_HEADER,
            ra,
            HeaderWords::Full {
                mv_return_address,
                livemask,
                toplink,
                frame_size,
            },
        )
    }

    /// Like [`Masm::emit_rp_header`] with the mv return address pointing at
    /// its own return address: a stub takes any value count itself.
    pub fn emit_rp_header_self(
        &mut self,
        livemask: usize,
        toplink: usize,
        frame_size: isize,
    ) -> Label {
        let ra = self.asm.label();
        self.reserve_rp_header(
            sabi::SIZE_RP_HEADER,
            ra,
            HeaderWords::Full {
                mv_return_address: HeaderTarget::Label(ra),
                livemask,
                toplink,
                frame_size,
            },
        )
    }

    /// Like [`Masm::emit_rp_header`] for the existing, unbound return
    /// address `ra`.
    pub fn emit_rp_header_at(
        &mut self,
        ra: Label,
        mv_return_address: HeaderTarget,
        livemask: usize,
        toplink: usize,
        frame_size: isize,
    ) {
        self.reserve_rp_header(
            sabi::SIZE_RP_HEADER,
            ra,
            HeaderWords::Full {
                mv_return_address,
                livemask,
                toplink,
                frame_size,
            },
        );
    }

    /// Like [`Masm::emit_rp_compact_header`] for the existing, unbound
    /// return address `ra`.
    pub fn emit_rp_compact_header_at(&mut self, ra: Label, header: RpCompactHeader) {
        self.reserve_rp_header(
            sabi::SIZE_RP_COMPACT_HEADER,
            ra,
            HeaderWords::Compact(header),
        );
    }

    /// Reserve `bytes` (a multiple of 8) of code to patch after load.
    pub fn reserve_words(&mut self, bytes: usize) {
        self.asm.align(8);
        self.emit_nop_padding(bytes);
    }

    /// Emits an 8-aligned `RpCompactHeader` (2 words) and returns the
    /// return address after it.
    pub fn emit_rp_compact_header(
        &mut self,
        toplink: usize,
        values_error: bool,
        frame_words: usize,
        livemask: usize,
    ) -> Label {
        let ra = self.asm.label();
        self.reserve_rp_header(
            sabi::SIZE_RP_COMPACT_HEADER,
            ra,
            HeaderWords::Compact(RpCompactHeader::new(
                toplink,
                values_error,
                frame_words,
                livemask,
            )),
        )
    }
}
