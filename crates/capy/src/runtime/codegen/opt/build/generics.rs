//! Generic-call devirtualization in the builder.
//!
//! A generic procedure is a native closure whose free slot 1 is the
//! `GenericDescriptor` and whose code block metadata carries the same
//! descriptor under the `generic` property. The machine `generic_call` stub
//! serves it, so the closure's `code` says nothing about the method. Like the
//! record path, this reads the **code block metadata** (which Scheme cannot
//! rewrite) rather than the closure's mutable `meta`.
//!
//! Two caches are responsible for specialization:
//!
//! - the descriptor's slot cache (a plain class accessor resolved to a slot):
//!   the helper re-checks the argument's class, the entry's writer bit and the
//!   instance storage's layout stamp, and reads the epoch before and afte
//! - the descriptor's `NO_NEXT` method cache: the helper matches the argument
//!   classes and argument count and re-reads the epoch, so an odd (change in
//!   progress) or changed epoch is a miss

use crate::heap::Gc;
use crate::runtime::codegen::opt::ir::func::Value as IrValue;
use crate::runtime::codegen::opt::ir::inst::{Helper, Op, ReturnValues};
use crate::runtime::codegen::opt::ir::types::{CodeRef, Type};
use crate::runtime::objectmodel::class::GenericDescriptor;
use crate::runtime::objectmodel::proc::Closure;
use crate::runtime::objectmodel::{CodeBlock, Symbol, Value as SchemeValue};
use crate::utils::flags;

use super::profile::{self, CallShape};
use super::{Builder, Flow};

fn metadata_symbol<'gc>(meta: SchemeValue<'gc>, key: &str) -> Option<SchemeValue<'gc>> {
    let mut entry = meta;
    // Metadata alists are short; the bound only guards against a cycle.
    for _ in 0..64 {
        if !entry.is_pair() {
            return None;
        }
        let pair = entry.car();
        if pair.is_pair() {
            let name = pair.car();
            if name.is::<Symbol>() && name.downcast::<Symbol>().as_str().as_ref() == key {
                return Some(pair.cdr());
            }
        }
        entry = entry.cdr();
    }
    None
}

fn generic_of_block<'gc>(
    block: Gc<'gc, CodeBlock<'gc>>,
) -> Option<Gc<'gc, GenericDescriptor<'gc>>> {
    let meta = block.metadata.get();
    let value = metadata_symbol(meta, "generic")?;
    value
        .is::<GenericDescriptor>()
        .then(|| value.downcast::<GenericDescriptor>())
}

impl<'s, 'gc> Builder<'s, 'gc> {
    fn generic_callee(
        &self,
        callee: IrValue,
        pc: usize,
    ) -> Option<(Gc<'gc, CodeBlock<'gc>>, Gc<'gc, GenericDescriptor<'gc>>)> {
        if let Some(closure) = self.constant_closure(callee) {
            let block = Closure::code_block(closure.downcast::<Closure>());
            if let Some(generic) = generic_of_block(block) {
                return Some((block, generic));
            }
        }
        if let Some(CallShape::Mono(Some(target))) = profile::call_shape(self.buffer, pc) {
            let block = target.callee;
            if let Some(generic) = generic_of_block(block) {
                return Some((block, generic));
            }
        }
        None
    }

    pub(super) fn try_generic_call(
        &mut self,
        pc: usize,
        next: usize,
        callee_reg: u32,
        first_arg: u32,
        argc: u32,
        _values: ReturnValues,
        site: super::ResultSite,
    ) -> Result<Option<Flow>, String> {
        if !flags::opt_generics() || !(1..=2).contains(&argc) {
            return Ok(None);
        }
        // A site that deopted past its budget stays generic.
        if (self.blocked)(pc) {
            return Ok(None);
        }
        let callee = self.reg(callee_reg)?;
        let Some((block, generic)) = self.generic_callee(callee, pc) else {
            return Ok(None);
        };

        let slot = generic.has_slot_cache();

        if !slot {
            return Ok(None);
        }
        let frame = self.frame(pc, pc)?;
        // Code identity: only this closure (and its descriptor) is trusted.
        let code: CodeRef = self.code_ref_value(SchemeValue::from(block));
        let _checked = self.value_op(
            Op::CheckCode(code),
            &[callee],
            Type::Closure(Some(code)),
            Some(frame),
        );
        let generic_value = self.value_const(generic.into());
        let args = self.regs(first_arg..first_arg + argc)?;
        let result = if argc == 1 {
            self.value_op(
                Op::CallOp(Helper::GenericSlotGet.helper_ref()),
                &[generic_value, args[0]],
                Type::Any,
                Some(frame),
            )
        } else {
            // The writer returns the stored value.
            self.value_op(
                Op::CallOp(Helper::GenericSlotPut.helper_ref()),
                &[generic_value, args[0], args[1]],
                Type::Any,
                Some(frame),
            )
        };
        self.finish_result(site, next, result)?;
        Ok(Some(Flow::Next))
    }
}
