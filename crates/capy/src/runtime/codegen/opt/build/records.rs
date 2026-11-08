//! Record accessors, mutators and predicates in the builder.
//!
//! The closures `record-accessor`, `record-mutator` and `record-predicate`
//! return are native procedures (`record_ops::make_record_accessor` and
//! friends) whose **code block metadata** carries a private descriptor:
//! `(kind, rtd, flat-offset, depth)`. The code block of each such closure is
//! unique, and its metadata is not reachable from Scheme
//! (`set-procedure-properties!` replaces the closure's `meta`, not the code
//! block's), so the descriptor cannot be forged.
//!
//! A call is specialized when its callee is either a folded constant closure
//! carrying that descriptor, or a monomorphic profile whose callee code block
//! carries it. Both emit `CheckCode` first, so a stale register or a profile
//! that named another closure deopts before the call.
//!
//! Records are `Tuple`s: payload word 0 is the length, word 1 the RTD, and
//! field `i` (a flat `tuple-ref` index) is payload word `i + 1`. An accessor
//! becomes `CheckClass(TUPLE)` plus an RTD identity guard plus `Load`; a
//! mutator the same plus `Store` and a write barrier; a predicate a total
//! `TestClass(TUPLE)`/RTD test materialized as `#t`/`#f`, which also indexes
//! the RTD's ancestry tuple for a parented type.
//!
//! Every failed guard deopts *before* the call, so baseline runs the
//! procedure again and raises the exact error the specialization suppressed.

use crate::heap::Gc;
use crate::runtime::codegen::opt::ir::frame_state::FrameState;
use crate::runtime::codegen::opt::ir::func::Value as IrValue;
use crate::runtime::codegen::opt::ir::func::ValueDef;
use crate::runtime::codegen::opt::ir::inst::{Cmp, Constant, MemoryRegion, Op, Terminator};
use crate::runtime::codegen::opt::ir::types::{ClassRef, Type};
use crate::runtime::objectmodel::proc::Closure;
use crate::runtime::objectmodel::{CodeBlock, Value as SchemeValue};
use crate::runtime::record_ops::{
    self, RECORD_KIND_ACCESSOR, RECORD_KIND_CONSTRUCTOR, RECORD_KIND_MUTATOR,
    RECORD_KIND_PREDICATE,
};

use super::profile::{self, CallShape};
use super::{Builder, Flow};

impl<'s, 'gc> Builder<'s, 'gc> {
    /// The closure value a callee operand is, when the builder folded it into
    /// a constant: either a constant-table reference or a root-buffer pool
    /// constant.
    pub(super) fn constant_closure(&self, value: IrValue) -> Option<SchemeValue<'gc>> {
        let ValueDef::Inst(inst) = self.func.value_def(value) else {
            return None;
        };
        let value = match self.func.insts[inst].op {
            Op::Const(Constant::Table(slot)) => {
                self.func.heap_constants.get(slot as usize - 1).copied()?
            }
            Op::Const(Constant::Pool { code, index }) if code == super::root() => {
                self.buffer.constant(index as usize)
            }
            _ => return None,
        };
        value.is::<Closure>().then_some(value)
    }

    /// The record procedure descriptor and code block behind a callee, from a
    /// folded constant closure or the site's monomorphic profile.
    fn record_op_of_callee(
        &self,
        callee: IrValue,
        pc: usize,
    ) -> Option<(record_ops::RecordOp<'gc>, Gc<'gc, CodeBlock<'gc>>)> {
        if let Some(closure) = self.constant_closure(callee) {
            let closure = closure.downcast::<Closure>();
            let block = Closure::code_block(closure);
            if let Some(op) = record_ops::record_op_from_meta(block.metadata.get()) {
                return Some((op, block));
            }
        }
        if let Some(CallShape::Mono(Some(target))) = profile::call_shape(self.buffer, pc) {
            let block = target.callee;
            if let Some(op) = record_ops::record_op_from_meta(block.metadata.get()) {
                return Some((op, block));
            }
        }
        None
    }

    /// Lowers the call at `pc` to a record accessor, mutator, predicate or
    /// constructor when the callee is recognized. `None` leaves the ordinary
    /// call path in charge.
    pub(super) fn try_record_call(
        &mut self,
        pc: usize,
        next: usize,
        callee_reg: u32,
        first_arg: u32,
        argc: u32,
        site: super::ResultSite,
    ) -> Result<Option<Flow>, String> {
        let Some(value) = self.record_value(pc, callee_reg, first_arg, argc)? else {
            return Ok(None);
        };
        self.finish_result(site, next, value)?;
        Ok(Some(Flow::Next))
    }

    /// The same lowering for a tail call: the specialized value is returned
    /// from the frame. Returns whether the call was lowered.
    pub(super) fn try_record_tail_call(&mut self, pc: usize, argc: u32) -> Result<bool, String> {
        let Some(value) = self.record_value(pc, 1, 2, argc)? else {
            return Ok(false);
        };
        self.terminate(Terminator::Return(value));
        Ok(true)
    }

    /// The specialized value of a call to a recognized record procedure.
    fn record_value(
        &mut self,
        pc: usize,
        callee_reg: u32,
        first_arg: u32,
        argc: u32,
    ) -> Result<Option<IrValue>, String> {
        let callee = self.reg(callee_reg)?;
        let Some((op, block)) = self.record_op_of_callee(callee, pc) else {
            return Ok(None);
        };
        let arity = match op.kind {
            RECORD_KIND_ACCESSOR | RECORD_KIND_PREDICATE => 1,
            RECORD_KIND_MUTATOR => 2,
            // Large records stay on the native call path.
            RECORD_KIND_CONSTRUCTOR if (0..=16).contains(&op.offset) => op.offset as u32,
            _ => return Ok(None),
        };
        if argc != arity {
            return Ok(None);
        }
        let is_constant = self.constant_closure(callee).is_some();
        let frame = self.frame(pc, pc)?;
        // Code identity: Scheme can replace a closure's `meta`, but not the
        // code block's metadata. A different closure that shares this code
        // block is still the same procedure, and a profile that named another
        // code block deopts here.
        let code = self.code_ref_value(SchemeValue::from(block));
        let checked = self.value_op(
            Op::CheckCode(code),
            &[callee],
            Type::Closure(Some(code)),
            Some(frame),
        );
        if op.kind == RECORD_KIND_CONSTRUCTOR {
            let rtd = if is_constant {
                self.value_const(op.rtd)
            } else {
                self.value_op(Op::LoadFree { index: 1 }, &[checked], Type::Any, None)
            };
            let mut elements = Vec::with_capacity(op.offset as usize + 1);
            elements.push(rtd);
            for index in 0..op.offset as u32 {
                elements.push(self.reg(first_arg + index)?);
            }
            return Ok(Some(self.value_op(Op::AllocTuple, &elements, Type::Any, None)));
        }
        // A constant closure's descriptor is static: use the static field
        // offset and the RTD constant. Otherwise the profile names a code
        // block but not the closure instance, so read the captured RTD and
        // offset through `load.free` and guard them like any other value.
        let (rtd, offset) = if is_constant {
            (self.value_const(op.rtd), None)
        } else {
            let rtd = self.value_op(Op::LoadFree { index: 1 }, &[checked], Type::Any, None);
            let offset = self.value_op(Op::LoadFree { index: 2 }, &[checked], Type::Any, None);
            (rtd, Some(offset))
        };
        let exact_only = is_constant && op.depth < 0;
        let depth = if exact_only {
            u32::MAX
        } else {
            op.depth as u32
        };
        // The flat `tuple-ref` index becomes payload word `offset + 1`.
        let field = u32::try_from(op.offset)
            .ok()
            .and_then(|offset| offset.checked_add(1))
            .ok_or_else(|| self.error("record field index overflows".to_string()))?;
        let obj = self.reg(first_arg)?;
        let value = match op.kind {
            RECORD_KIND_ACCESSOR => self.record_access(obj, rtd, field, offset, frame)?,
            RECORD_KIND_MUTATOR => {
                let value = self.reg(first_arg + 1)?;
                self.record_store(obj, value, rtd, field, offset, frame)?;
                self.undef()
            }
            RECORD_KIND_PREDICATE => self.record_test(obj, rtd, depth, frame)?,
            _ => unreachable!("the kind was checked above"),
        };
        Ok(Some(value))
    }

    /// The field `offset` of the record `obj`, guarded by the tuple class and
    /// the captured RTD. `field` is the static payload word for a constant
    /// closure; `dynamic_offset` reads the captured flat index at run time.
    fn record_access(
        &mut self,
        obj: IrValue,
        rtd: IrValue,
        field: u32,
        dynamic_offset: Option<IrValue>,
        frame: FrameState,
    ) -> Result<IrValue, String> {
        let tuple = self.check_class(obj, ClassRef::TUPLE, frame);
        self.guard_rtd_exact(tuple, rtd, frame);
        let value = match dynamic_offset {
            Some(offset) => {
                // The flat index is a fixnum; `check.index` bounds-checks it
                // against the tuple's length word (at word 0, as for vectors)
                // and `load.element` reads `data[index]`.
                let index =
                    self.value_op(Op::CheckIndex, &[tuple, offset], Type::Int32, Some(frame));
                self.value_op(Op::LoadElement, &[tuple, index], Type::Any, None)
            }
            None => self.value_op(
                Op::Load {
                    field,
                    region: MemoryRegion::Heap,
                },
                &[tuple],
                Type::Any,
                None,
            ),
        };
        Ok(value)
    }

    /// Stores `value` in field `offset` of the record `obj`, with the same
    /// guards as [`Self::record_access`].
    fn record_store(
        &mut self,
        obj: IrValue,
        value: IrValue,
        rtd: IrValue,
        field: u32,
        dynamic_offset: Option<IrValue>,
        frame: FrameState,
    ) -> Result<(), String> {
        let tuple = self.check_class(obj, ClassRef::TUPLE, frame);
        self.guard_rtd_exact(tuple, rtd, frame);
        // Before the store: the SATB barrier must see the old field value.
        self.write_barrier(tuple, value);
        match dynamic_offset {
            Some(offset) => {
                let index =
                    self.value_op(Op::CheckIndex, &[tuple, offset], Type::Int32, Some(frame));
                self.effect_op(Op::StoreElement, &[tuple, index, value], None);
            }
            None => {
                self.effect_op(
                    Op::Store {
                        field,
                        region: MemoryRegion::Heap,
                    },
                    &[tuple, value],
                    None,
                );
            }
        }
        Ok(())
    }

    /// `#t`/`#f` for `obj` being a record of `rtd` or one of its subtypes.
    /// `depth` is the RTD's ancestry index; `u32::MAX` checks identity only.
    fn record_test(
        &mut self,
        obj: IrValue,
        rtd: IrValue,
        depth: u32,
        frame: FrameState,
    ) -> Result<IrValue, String> {
        let check = self.func.new_block();
        let hit = self.func.new_block();
        let miss = self.func.new_block();
        let join = self.func.new_block();
        let result = self.func.append_param(join, Type::Any);
        let yes = self.bits(SchemeValue::VALUE_TRUE as u64);
        let no = self.bits(SchemeValue::VALUE_FALSE as u64);
        let is_tuple = self.value_op(Op::TestClass(ClassRef::TUPLE), &[obj], Type::Bool, None);
        let then = self.func.edge(check, &[]);
        let otherwise = self.func.edge(miss, &[]);
        self.terminate(Terminator::Branch {
            cond: is_tuple,
            then,
            otherwise,
        });
        // Not a tuple: #f.
        self.cur = miss;
        let edge = self.func.edge(join, &[no]);
        self.terminate(Terminator::Jump(edge));
        // A tuple: compare its RTD.
        self.cur = check;
        let actual = self.value_op(
            Op::Load {
                field: 1,
                region: MemoryRegion::Heap,
            },
            &[obj],
            Type::Any,
            None,
        );
        let same = self.value_op(Op::EqBits, &[actual, rtd], Type::Bool, None);
        if depth == u32::MAX {
            let then = self.func.edge(hit, &[]);
            let otherwise = self.func.edge(miss, &[]);
            self.terminate(Terminator::Branch {
                cond: same,
                then,
                otherwise,
            });
        } else {
            let ancestry = self.func.new_block();
            let indexed = self.func.new_block();
            let then = self.func.edge(hit, &[]);
            let otherwise = self.func.edge(ancestry, &[]);
            self.terminate(Terminator::Branch {
                cond: same,
                then,
                otherwise,
            });
            // Not the exact type: its RTD may still be a descendant. RTDs
            // carry an ancestry tuple `#f ... parent self`, so index it
            // directly; there is no parent-pointer walk.
            self.cur = ancestry;
            let is_rtd = self.value_op(Op::TestClass(ClassRef::RTD), &[actual], Type::Bool, None);
            let then = self.func.edge(indexed, &[]);
            let otherwise = self.func.edge(miss, &[]);
            self.terminate(Terminator::Branch {
                cond: is_rtd,
                then,
                otherwise,
            });
            self.cur = indexed;
            let ancestors = self.value_op(
                Op::Load {
                    field: 0,
                    region: MemoryRegion::Heap,
                },
                &[actual],
                Type::Any,
                None,
            );
            let len = self.value_op(Op::VectorLength, &[ancestors], Type::Int32, Some(frame));
            let depth_value = self.value_op(
                Op::Const(Constant::Int32(depth as i32)),
                &[],
                Type::Int32,
                None,
            );
            let within = self.value_op(Op::CmpI32(Cmp::Gt), &[len, depth_value], Type::Bool, None);
            let in_bounds = self.func.new_block();
            let then = self.func.edge(in_bounds, &[]);
            let otherwise = self.func.edge(miss, &[]);
            self.terminate(Terminator::Branch {
                cond: within,
                then,
                otherwise,
            });
            // `depth < len` here, so the indexed load is in bounds. The load
            // must live in its own block: doing it in `indexed` would both
            // overwrite that block's terminator and read past a shallower
            // ancestry (a parented predicate applied to a root instance).
            self.cur = in_bounds;
            let value = self.value_op(
                Op::Load {
                    field: depth + 1,
                    region: MemoryRegion::Heap,
                },
                &[ancestors],
                Type::Any,
                None,
            );
            let matches = self.value_op(Op::EqBits, &[value, rtd], Type::Bool, None);
            let then = self.func.edge(hit, &[]);
            let otherwise = self.func.edge(miss, &[]);
            self.terminate(Terminator::Branch {
                cond: matches,
                then,
                otherwise,
            });
        }
        self.cur = hit;
        let edge = self.func.edge(join, &[yes]);
        self.terminate(Terminator::Jump(edge));
        self.cur = join;
        Ok(result)
    }

    /// Deopts before the call unless `obj.data[0]` is exactly `rtd`.
    fn guard_rtd_exact(&mut self, obj: IrValue, rtd: IrValue, frame: FrameState) {
        let actual = self.value_op(
            Op::Load {
                field: 1,
                region: MemoryRegion::Heap,
            },
            &[obj],
            Type::Any,
            None,
        );
        let same = self.value_op(Op::EqBits, &[actual, rtd], Type::Bool, None);
        self.guard_true(same, frame);
    }

    /// Ends the current block with a branch to a deopt on a false guard.
    fn guard_true(&mut self, cond: IrValue, frame: FrameState) {
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
