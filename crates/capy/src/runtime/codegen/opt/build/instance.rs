//! Inline `make-instance` for a statically known class.

use crate::runtime::codegen::opt::ir::frame_state::FrameState;
use crate::runtime::codegen::opt::ir::func::{Value as IrValue, ValueDef};
use crate::runtime::codegen::opt::ir::inst::{Constant, Op, Terminator};
use crate::runtime::codegen::opt::ir::types::Type;
use crate::runtime::objectmodel::Value as SchemeValue;
use crate::runtime::objectmodel::class::{ClassCategory, ClassDescriptor};

use super::Builder;

impl<'s, 'gc> Builder<'s, 'gc> {
    pub(super) fn try_instance_call(
        &mut self,
        pc: usize,
        callee_reg: u32,
        first_arg: u32,
        argc: u32,
    ) -> Result<Option<IrValue>, String> {
        if argc != 1 || (self.blocked)(pc) {
            return Ok(None);
        }
        let callee = self.reg(callee_reg)?;
        let Some(callee_value) = self.runtime_constant(callee) else {
            return Ok(None);
        };
        if !crate::runtime::objectmodel::class::is_make_instance_procedure(self.ctx, callee_value) {
            return Ok(None);
        }
        let class_arg = self.reg(first_arg)?;
        let Some(class_value) = self.runtime_constant(class_arg) else {
            return Ok(None);
        };
        if !class_value.is::<ClassDescriptor>() {
            return Ok(None);
        }
        // SAFETY: checked above.
        let class = class_value.downcast::<ClassDescriptor>();
        if class.category() != ClassCategory::Scheme || !class.has_plain_scheme_allocation() {
            return Ok(None);
        }

        if class
            .slots()
            .iter()
            .any(|slot| !slot.init_thunk().is_empty())
        {
            return Ok(None);
        }

        let current = crate::runtime::objectmodel::class::class_table(self.ctx).lookup(class.id());
        if current.map(SchemeValue::from) != Some(class_value) {
            return Ok(None);
        }

        let Some(storage_header) = crate::runtime::objectmodel::class::slot_storage_header_word()
        else {
            return Ok(None);
        };
        let Ok(slot_count) = u32::try_from(class.slot_count()) else {
            return Ok(None);
        };

        if class.slot_count() > i32::MAX as usize / 8 {
            return Ok(None);
        }
        let stamp = class.layout_stamp();
        debug_assert_ne!(stamp, 0, "a Scheme class has a layout stamp");

        let frame = self.frame(pc, pc)?;

        let callee_const = self.value_const(callee_value);
        let same_callee = self.value_op(Op::EqBits, &[callee, callee_const], Type::Bool, None);
        self.guard_before_call(same_callee, frame);
        let class_const = self.value_const(class_value);
        let same_class = self.value_op(Op::EqBits, &[class_arg, class_const], Type::Bool, None);
        self.guard_before_call(same_class, frame);

        let header = self.value_op(
            Op::Const(Constant::Int64(storage_header as i64)),
            &[],
            Type::Int64,
            None,
        );
        let stamp_len = ((slot_count as u64) << 32) | u64::from(stamp);
        let stamp_len = self.value_op(
            Op::Const(Constant::Int64(stamp_len as i64)),
            &[],
            Type::Int64,
            None,
        );
        let slots = class.slots();
        let mut operands = Vec::with_capacity(slots.len() + 3);
        operands.push(class_arg);
        operands.push(header);
        operands.push(stamp_len);
        for slot in slots {
            operands.push(self.value_const(slot.init_value()));
        }
        let result = self.value_op(
            Op::AllocInstance {
                class: class.id().bits(),
                slots: slot_count,
            },
            &operands,
            Type::Any,
            None,
        );
        // The descriptor and its layout are an assumption of this unit.
        let dep = crate::runtime::codegen::opt::ir::func::ClassDep {
            class: class_value,
            id: class.id().bits(),
            stamp,
        };
        if !self
            .func
            .class_deps
            .iter()
            .any(|mine| mine.class == dep.class)
        {
            self.func.class_deps.push(dep);
        }
        Ok(Some(result))
    }

    fn runtime_constant(&self, value: IrValue) -> Option<SchemeValue<'gc>> {
        self.resolve_constant(
            value,
            &mut std::collections::HashSet::new(),
            &mut std::collections::HashMap::new(),
            &mut false,
        )
    }

    fn resolve_constant(
        &self,
        value: IrValue,
        visiting: &mut std::collections::HashSet<IrValue>,
        memo: &mut std::collections::HashMap<IrValue, Option<SchemeValue<'gc>>>,
        cycle_hit: &mut bool,
    ) -> Option<SchemeValue<'gc>> {
        if let Some(constant) = self.folded_constant(value) {
            return Some(constant);
        }
        if let Some(cached) = memo.get(&value).copied() {
            return cached;
        }
        if !visiting.insert(value) {
            *cycle_hit = true;
            return None;
        }
        let mut child_cycle = false;
        let resolved = match self.func.value_def(value) {
            ValueDef::Param { block, index } if block != self.func.entry => {
                let mut resolved: Option<SchemeValue<'gc>> = None;
                let mut saw_edge = false;
                for (_, data) in self.func.blocks.iter() {
                    let Some(term) = &data.term else {
                        continue;
                    };
                    for edge in term.edges() {
                        if edge.block != block {
                            continue;
                        }
                        let args = self.func.edge_args(&edge);
                        let index = index as usize;
                        if index >= args.len() {
                            visiting.remove(&value);
                            return None;
                        }
                        let arg = args[index];
                        if arg == value {
                            continue;
                        }
                        saw_edge = true;
                        let Some(constant) =
                            self.resolve_constant(arg, visiting, memo, &mut child_cycle)
                        else {
                            visiting.remove(&value);
                            return None;
                        };
                        match resolved {
                            Some(previous) if previous.bits() != constant.bits() => {
                                visiting.remove(&value);
                                return None;
                            }
                            Some(_) => {}
                            None => resolved = Some(constant),
                        }
                    }
                }
                saw_edge.then_some(resolved).flatten()
            }
            // The builder keeps a non-folded `cell-ref` as a load from the
            // pool cell. When that cell is a `Variable`, its current value is
            // the value the load would produce; the identity guard pays for a
            // concurrent write.
            ValueDef::Inst(inst) => match self.func.insts[inst].op {
                // A class check only refines its operand.
                Op::CheckClass(_) | Op::CheckCode(_) => {
                    let src = self.func.args(inst)[0];
                    self.resolve_constant(src, visiting, memo, &mut child_cycle)
                }
                // The requesting closure's free variables are known when the
                // compile was entered from it. The identity guards make the
                // value an assumption: another closure of the same code (a
                // second library instance) fails its guard and deopts. The
                // closure operand itself may be a block parameter in a join,
                // so its identity is not required here.
                Op::LoadFree { index } => {
                    if self.entry_closure.is_some() {
                        self.free_cells.get(index as usize).copied()
                    } else {
                        None
                    }
                }
                Op::LoadCell => {
                    let cell = self.func.args(inst)[0];
                    match self.resolve_constant(cell, visiting, memo, &mut child_cycle) {
                        Some(cell) if cell.is::<crate::runtime::variable::Variable>() => {
                            let variable = cell.downcast::<crate::runtime::variable::Variable>();
                            let current = variable.get();
                            (current != SchemeValue::undefined()).then_some(current)
                        }
                        _ => None,
                    }
                }
                _ => None,
            },
            ValueDef::Param { .. } => None,
        };

        if resolved.is_none() || !child_cycle {
            memo.insert(value, resolved);
        }
        *cycle_hit |= child_cycle;
        visiting.remove(&value);
        resolved
    }

    fn folded_constant(&self, value: IrValue) -> Option<SchemeValue<'gc>> {
        let ValueDef::Inst(inst) = self.func.value_def(value) else {
            return None;
        };
        match self.func.insts[inst].op {
            Op::Const(Constant::Bits(bits)) => Some(SchemeValue::from_raw(bits)),
            Op::Const(Constant::Table(slot)) => {
                self.func.heap_constants.get(slot as usize - 1).copied()
            }
            Op::Const(Constant::Pool { code, index }) if code == super::root() => {
                Some(self.buffer.constant(index as usize))
            }
            _ => None,
        }
    }

    fn guard_before_call(&mut self, cond: IrValue, frame: FrameState) {
        let cont = self.func.new_block();
        let fail = self.func.new_block();
        let then = self.func.edge(cont, &[]);
        let otherwise = self.func.edge(fail, &[]);
        self.terminate(Terminator::Branch {
            cond,
            then,
            otherwise,
        });
        self.cur = fail;
        self.terminate(Terminator::Deopt { frame });
        self.cur = cont;
    }
}
