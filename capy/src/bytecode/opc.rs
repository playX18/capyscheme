#[macro_export]
macro_rules! for_each_opcode {
    ($m: ident) => {
        $m! {
            (halt, "halt", {})
            (entry, "entry", {})
            (const_nil, "const-nil", {})
            (const_true, "const-true", {})
            (const_false, "const-false", {})
            (const_fixnum, "const-fixnum", { fixnum: i32 })
            (constant, "const", { index: u32 })
            (load, "load", { spoff: u16 })
            (store, "store", { spoff: u16 })
            (pop, "pop", {})
            (push, "push", {})
            (dup, "dup", {})
            (swap, "swap", {})
            (popn, "popn", { n: u16 })
            (j, "j", { offset: i32 })
            (jlt, "jlt", { offset: i32 })
            (jle, "jle", { offset: i32 })
            (je,  "je",  { offset: i32 })
            (jnl, "jnl", { offset: i32 })
            (jne, "jne", { offset: i32 })
            (jge, "jge", { offset: i32 })
            (jgt, "jnge", { offset: i32 })

            (immediatep, "immediate?", {})
            (heap_objectp, "heap-object?", {})
            (tc8eq, "tc8=?", { typ8: u8 })
            (tc16eq, "tc16=?", { typ16: u16})
            (eq,     "eq?", {})
            (eqv,    "eqv?", {})
            (equal,  "equal?", {})
            (neq,    "=?", {})
            (lt,     "<?", {})

            (make_closure, "make-closure", { index: u32, nfree: u16 })

            (return_call, "return-call", { nargs: u16 })


            (call_intrinsic0, "call-intrinsic0", { index: u8 })
            (call_intrinsic1, "call-intrinsic1", { index: u8 })
            (call_intrinsic2, "call-intrinsic2", { index: u8 })
            (call_intrinsic3, "call-intrinsic3", { index: u8 })
            (call_intrinsic4, "call-intrinsic4", { index: u8 })
            (call_intrinsic5, "call-intrinsic5", { index: u8 })


            (mref8, "mref8", {})
            (mref16, "mref16", {})
            (mref32, "mref32", {})
            (mref64, "mref64", {})

            (mrefcell, "mrefcell", {})

            (mset8, "mset8", {})
            (mset16, "mset16", {})
            (mset32, "mset32", {})
            (mset64, "mset64", {})
            (msetcell, "msetcell", {})

            (mref8_imm, "mref8/imm", { offset: i16 })
            (mref16_imm, "mref16/imm", { offset: i16 })
            (mref32_imm, "mref32/imm", { offset: i16 })
            (mref64_imm, "mref64/imm", { offset: i16 })
            (mrefcell_imm, "mrefcell/imm", { offset: i16 })
            (mset8_imm, "mset8/imm", { offset: i16 })
            (mset16_imm, "mset16/imm", { offset: i16 })
            (mset32_imm, "mset32/imm", { offset: i16 })
            (mset64_imm, "mset64/imm", { offset: i16 })
            (msetcell_imm, "msetcell/imm", { offset: i16 })

            (u64_add, "u64+", {})
            (u64_sub, "u64-", {})
            (u64_mul, "u64*", {})
            (u64_div, "u64/", {})
            (u64_mod, "u64%", {})
            (u64_lshift, "u64<<", {})
            (u64_rshift, "u64>>", {})
            (u64_and, "u64&", {})
            (u64_or, "u64|", {})
            (u64_xor, "u64^", {})
            (i32_add, "i32+", {})
            (i32_sub, "i32-", {})
            (i32_mul, "i32*", {})
            (i32_div, "i32/", {})
            (i32_mod, "i32%", {})
            (i32_lshift, "i32<<", {})
            (i32_rshift, "i32>>", {})
            (i32_and, "i32&", {})
            (i32_or, "i32|", {})
            (i32_xor, "i32^", {})
            (i32_negate, "i32-negate", {})
            (i32_to_u64, "i32->u64", {})
            (u64_to_i32, "u64->i32", {})
            (i32_add_ovf, "i32/ovf", {})
            (i32_sub_ovf, "i32/ovf", {})
            (i32_mul_ovf, "i32/ovf", {})
        }
    };
}

macro_rules! define_opcodes {
    ($(($name: ident, $sname: literal, { $($field: ident : $t :ty),* }))*) => {
        paste::paste! {
            #[derive(Debug, Clone, Copy, PartialEq, Eq)]
            #[repr(u8)]
            pub enum Opcode {

                    $(
                        [< $name: camel >],
                    )*
                    Nop,
            }

            pub const NUM_OPCODES: usize = Opcode::Nop as usize;
        }
    };
}

for_each_opcode!(define_opcodes);
