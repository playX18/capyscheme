use capy_bytecode::bytecodegen::BytecodeBuffer;

use crate::{
    Gc, Trace,
    object::{GCObject, VTable},
    prelude::{ScmHeader, Tagged, TypeCode8, Value},
};

#[repr(C)]
pub struct CodeBlock<'gc> {
    pub header: ScmHeader,
    pub constants: Value<'gc>,
    pub nlocals: u32,
    pub code_len: usize,
    pub code: [u8; 0],
}

unsafe impl Trace for CodeBlock<'_> {
    unsafe fn trace(&mut self, visitor: &mut crate::Visitor) {
        visitor.trace(&mut self.constants);
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::WeakProcessor) {
        let _ = weak_processor;
    }
}
extern "C" fn trace(_obj: GCObject, _visitor: &mut crate::Visitor) {
    unsafe {
        let cb = _obj.to_address().as_mut_ref::<CodeBlock>();
        cb.trace(_visitor);
    }
}

extern "C" fn process_weak_refs(_obj: GCObject, _weak_processor: &mut crate::WeakProcessor) {
    unsafe {
        let cb = _obj.to_address().as_mut_ref::<CodeBlock>();
        cb.process_weak_refs(_weak_processor);
    }
}

extern "C" fn compute_size(_obj: GCObject) -> usize {
    unsafe {
        let cb = _obj.to_address().as_ref::<CodeBlock>();
        cb.code_len
    }
}

pub static CODE_BLOCK_VTABLE: &'static VTable = &VTable {
    alignment: std::mem::align_of::<CodeBlock>(),
    compute_size: Some(compute_size),
    compute_alignment: None,
    trace,
    instance_size: std::mem::size_of::<CodeBlock>(),
    type_name: CodeBlock::TYPE_NAME,
    weak_proc: process_weak_refs,
};

unsafe impl<'gc> Tagged for CodeBlock<'gc> {
    const TC8: TypeCode8 = TypeCode8::CODE_BLOCK;
    const TYPE_NAME: &'static str = "#<code block>";
}

impl<'gc> BytecodeBuffer for CodeBlock<'gc> {
    #[inline(always)]
    fn read_u8(&self, pc: usize) -> u8 {
        unsafe { self.code.as_ptr().add(pc).read() }
    }

    #[inline(always)]
    fn read_i32_static<const WIDTH: u8>(&self, pc: usize) -> (i32, usize) {
        match WIDTH {
            0 => {
                let byte = self.read_u8(pc);
                (byte as i8 as i32, pc + 1)
            }
            1 => unsafe {
                (
                    self.code.as_ptr().add(pc).cast::<u16>().read() as i16 as i32,
                    pc + 2,
                )
            },
            2 => unsafe {
                (
                    self.code.as_ptr().add(pc).cast::<i32>().read() as i32,
                    pc + 4,
                )
            },
            _ => panic!("Unsupported width for read_i32_static: {}", WIDTH),
        }
    }

    #[inline(always)]
    fn read_u16(&self, pc: usize) -> u16 {
        unsafe { self.code.as_ptr().add(pc).cast::<u16>().read() }
    }

    #[inline(always)]
    fn read_u32(&self, pc: usize) -> u32 {
        unsafe { self.code.as_ptr().add(pc).cast::<u32>().read() }
    }
}

impl<'gc, T: BytecodeBuffer> BytecodeBuffer for Gc<'gc, T> {
    #[inline(always)]
    fn read_i32_static<const WIDTH: u8>(&self, pc: usize) -> (i32, usize) {
        (**self).read_i32_static::<WIDTH>(pc)
    }

    #[inline(always)]
    fn read_u16(&self, pc: usize) -> u16 {
        (**self).read_u16(pc)
    }

    #[inline(always)]
    fn read_u32(&self, pc: usize) -> u32 {
        (**self).read_u32(pc)
    }

    #[inline(always)]
    fn read_u8(&self, pc: usize) -> u8 {
        (**self).read_u8(pc)
    }
}
