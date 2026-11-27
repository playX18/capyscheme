//! Generic-call devirtualization.

use crate::heap::Gc;
use crate::heap::object::{ClassId, builtin_class_ids};
use crate::runtime::codegen::opt::ir::func::{ClassDep, ClassDepKind};
use crate::runtime::codegen::opt::ir::frame_state::FrameState;
use crate::runtime::codegen::opt::ir::func::GenericDep;
use crate::runtime::codegen::opt::ir::func::Value as IrValue;
use crate::runtime::codegen::opt::ir::func::ValueDef;
use crate::runtime::codegen::opt::ir::inst::{
    Cmp, Constant, Helper, MemoryRegion, Op, ReturnValues, Terminator,
};
use crate::runtime::codegen::opt::ir::types::{ClassRef, CodeRef, Type};
use crate::runtime::hooks::Hook;
use crate::runtime::objectmodel::class::generic::MethodRole;
use crate::runtime::objectmodel::class::{
    ClassCategory, ClassDescriptor, GenericDescriptor, class_table,
};
use crate::runtime::objectmodel::proc::Closure;
use crate::runtime::objectmodel::{CodeBlock, CodeBlockKind, Symbol, Value as SchemeValue};

use crate::utils::flags;

use super::profile::{self, CallShape};

use super::{Builder, Flow};

/// Max method-cache shapes lowered without helper.
const MAX_CLASS_TESTS: usize = 4;
/// Max method-cache shapes lowered as class-test chain.
const MAX_CHAIN_TESTS: usize = 8;
/// Max slot readers inlined.
const MAX_SLOT_TESTS: usize = 8;

/// One inlined slot reader: class, class id, layout stamp/len, slot index.
type SlotArm<'gc> = (Gc<'gc, ClassDescriptor<'gc>>, u32, u32, u32, u32);

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

    /// Lower a generic call from its caches.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn try_generic_call(
        &mut self,
        pc: usize,
        next: usize,
        callee_reg: u32,
        first_arg: u32,
        argc: u32,
        values: ReturnValues,
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
        // Multi-value call nodes carry values in callee frame.
        let taking = !matches!(
            values,
            ReturnValues::Forward | ReturnValues::Ignore | ReturnValues::Error
        );
        if generic.has_slot_cache() {
            return self.slot_call(
                pc, next, callee, block, generic, first_arg, argc, values, site,
            );
        }
        if !taking
            && let Some(flow) = self.fold_constant_call(
                pc, next, callee, block, generic, first_arg, argc, values, site,
            )?
        {
            return Ok(Some(flow));
        }
        // Only simple generics have method caches.
        if !generic.flags().simple() {
            return self.combined_call(
                pc, next, callee, block, generic, first_arg, argc, values, site,
            );
        }
        if let Some(flow) =
            self.sole_method_call(pc, next, callee, block, generic, first_arg, argc, values, site)?
        {
            return Ok(Some(flow));
        }
        let argc_usize = argc as usize;
        // Fall back to cursor cache.
        let method_entries = generic.method_cache_entries(argc_usize);
        let Some((epoch, entries)) = method_entries.filter(|(_, entries)| !entries.is_empty())
        else {
            if let Some((epoch, cursors)) = generic.cursor_entries(argc_usize)
                && !cursors.is_empty()
            {
                return self.cursor_call(
                    pc, next, callee, block, generic, first_arg, argc, epoch, cursors, values, site,
                );
            }
            self.note_cold_generic(generic);
            return Ok(None);
        };
        // Chains serve one-value taking sites.
        let rest_list = matches!(
            site,
            super::ResultSite::Taken {
                wants_list: true,
                ..
            }
        );
        let chain_takes =
            !rest_list && matches!(values, ReturnValues::Take(1) | ReturnValues::TakeRest(1));
        // Extend arms with subclass descendants.
        let (entries, subtree) =
            self.subclass_extension(generic, argc, entries, taking, chain_takes);
        let shapes = entries.len();
        let helper = shapes > MAX_CHAIN_TESTS || (shapes >= 2 && taking && !chain_takes);
        // Register dependency before emitting.
        if helper && !subtree.is_empty() {
            return Ok(None);
        }
        if !helper && !self.depend_on_generic(generic, epoch) {
            return Ok(None);
        }
        for &(member, stamp) in &subtree {
            if !self.depend_on_subtree(member, stamp) {
                return Ok(None);
            }
            self.depend_on_class(member);
        }

        let frame = self.frame(pc, pc)?;
        // Verify code.
        let code: CodeRef = self.code_ref_value(SchemeValue::from(block));
        self.value_op(
            Op::CheckCode(code),
            &[callee],
            Type::Closure(Some(code)),
            Some(frame),
        );
        let args = self.regs(first_arg..first_arg + argc)?;

        if !helper && shapes > 1 {
            let chain: Vec<_> = entries
                .iter()
                .map(|&(class0, class1, body)| (class0, class1, body, None))
                .collect();
            let parts = {
                let frame = self.site_call_frame(site, next)?;
                self.poly_call(callee, &args, argc, &chain, values, frame)?
            };
            self.finish_parts(site, next, &parts)?;
            return Ok(Some(Flow::Next));
        }
        let result = if helper {
            // Helper resolves body.
            let generic_value = self.value_const(generic.into());
            let argc_raw = self.bits(argc as u64);
            let arg1 = *args.get(1).unwrap_or(&args[0]);
            let body = self.value_op(
                Op::CallOp(Helper::GenericMethod.helper_ref()),
                &[generic_value, args[0], arg1, argc_raw],
                Type::Closure(None),
                Some(frame),
            );
            let frame = self.site_call_frame(site, next)?;
            self.call_body(body, None, &args, argc, values, frame)?
        } else if shapes == 1 {
            let (class0, class1, body) = entries[0];
            let checked0 = self.check_class(args[0], ClassRef::from_class_id(class0), frame);
            let mut checked = vec![checked0];
            if argc_usize == 2 {
                checked.push(self.check_class(args[1], ClassRef::from_class_id(class1), frame));
            }
            let body_block = closure_block(body);
            let body = self.value_const(body);
            let frame = self.site_call_frame(site, next)?;
            self.call_body(body, body_block, &checked, argc, values, frame)?
        } else {
            unreachable!("a single shape is the arm above")
        };
        self.finish_result(site, next, result)?;
        Ok(Some(Flow::Next))
    }

    /// The Scheme value of an operand that is a constant, if it is one.
    pub(super) fn constant_value(&self, value: IrValue) -> Option<SchemeValue<'gc>> {
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

    /// A call of a generic whose only method applies to every argument: one
    /// primary method, without `next-method`, specialized on `<top>` at
    /// every position. Every call runs its body, whatever the classes, so
    /// the site calls the body directly, without the class tests or the
    /// method lookup its cache would need, and depends on the generic's
    /// epoch, which changes with its method set.
    #[allow(clippy::too_many_arguments)]
    fn sole_method_call(
        &mut self,
        pc: usize,
        next: usize,
        callee: IrValue,
        block: Gc<'gc, CodeBlock<'gc>>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        first_arg: u32,
        argc: u32,
        values: ReturnValues,
        site: super::ResultSite,
    ) -> Result<Option<Flow>, String> {
        let flags = generic.flags();
        if flags.custom_dfun() || flags.has_eql() {
            return Ok(None);
        }
        let epoch = generic.epoch();
        if epoch & 1 != 0 {
            return Ok(None);
        }
        let methods = generic.methods();
        let [method] = methods.as_slice() else {
            return Ok(None);
        };
        let method = *method;
        let top = ClassId::new(builtin_class_ids::TOP);
        let method_flags = method.flags();
        if method.role() != MethodRole::Primary
            || !method_flags.no_next()
            || method_flags.slot_reader()
            || method_flags.slot_writer()
            || method_flags.boot_slot_protocol()
            || method.required_arg_count() != argc as usize
            || method.specializers().iter().any(|&s| Some(s) != top)
            || (0..argc as usize).any(|position| method.eql_at(position).is_some())
        {
            return Ok(None);
        }
        let Some(body_block) = closure_block(method.body()) else {
            return Ok(None);
        };
        // The epoch is read before the method list: a method added since
        // makes the install fail.
        if generic.epoch() != epoch || !self.depend_on_generic(generic, epoch) {
            return Ok(None);
        }
        let frame = self.frame(pc, pc)?;
        let code: CodeRef = self.code_ref_value(SchemeValue::from(block));
        self.value_op(
            Op::CheckCode(code),
            &[callee],
            Type::Closure(Some(code)),
            Some(frame),
        );
        let args = self.regs(first_arg..first_arg + argc)?;
        let body = self.value_const(method.body());
        let frame = self.site_call_frame(site, next)?;
        let result = self.call_body(body, Some(body_block), &args, argc, values, frame)?;
        self.finish_result(site, next, result)?;
        Ok(Some(Flow::Next))
    }

    /// Fold constant-argument generic call.
    #[allow(clippy::too_many_arguments)]
    fn fold_constant_call(
        &mut self,
        pc: usize,
        next: usize,
        callee: IrValue,
        block: Gc<'gc, CodeBlock<'gc>>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        first_arg: u32,
        argc: u32,
        values: ReturnValues,
        site: super::ResultSite,
    ) -> Result<Option<Flow>, String> {
        // Custom dfun handles call.
        if generic.flags().custom_dfun() {
            return Ok(None);
        }
        let args = self.regs(first_arg..first_arg + argc)?;
        let constants: Option<Vec<SchemeValue<'gc>>> =
            args.iter().map(|&arg| self.constant_value(arg)).collect();
        let Some(constants) = constants else {
            return Ok(None);
        };
        let epoch = generic.epoch();
        if epoch & 1 != 0 {
            return Ok(None);
        }
        let Ok(methods) =
            GenericDescriptor::applicable_methods_at(self.ctx, generic, &constants, epoch)
        else {
            return Ok(None);
        };
        if methods.len() != 1 {
            return Ok(None);
        }
        let Some(&method) = methods.iter().next() else {
            return Ok(None);
        };
        let flags = method.flags();
        if method.role() != MethodRole::Primary
            || !flags.no_next()
            || flags.slot_reader()
            || flags.slot_writer()
            || flags.boot_slot_protocol()
        {
            return Ok(None);
        }
        let Some(body_block) = closure_block(method.body()) else {
            return Ok(None);
        };
        if !self.depend_on_generic(generic, epoch) {
            return Ok(None);
        }
        let frame = self.frame(pc, pc)?;
        let code: CodeRef = self.code_ref_value(SchemeValue::from(block));
        self.value_op(
            Op::CheckCode(code),
            &[callee],
            Type::Closure(Some(code)),
            Some(frame),
        );
        let body = self.value_const(method.body());
        let frame = self.site_call_frame(site, next)?;
        let result = self.call_body(body, Some(body_block), &args, argc, values, frame)?;
        self.finish_result(site, next, result)?;
        Ok(Some(Flow::Next))
    }

    /// Combined-generic call via cached effective method.
    #[allow(clippy::too_many_arguments)]
    fn combined_call(
        &mut self,
        pc: usize,
        next: usize,
        callee: IrValue,
        block: Gc<'gc, CodeBlock<'gc>>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        first_arg: u32,
        argc: u32,
        values: ReturnValues,
        site: super::ResultSite,
    ) -> Result<Option<Flow>, String> {
        let flags = generic.flags();
        // Eql/resolved-key generics are not class-keyed.
        if flags.has_eql() || flags.resolved_keys() {
            return Ok(None);
        }
        let argc_usize = argc as usize;
        // Use cached EMF or sole bucket.
        let (epoch, key_len, class_ids, emf) = match profile::generic_emf_at(self.buffer, pc) {
            Some(cached)
                if cached.generic.as_ptr() == generic.as_ptr()
                    && cached.epoch == generic.epoch()
                    && cached.epoch & 1 == 0
                    && cached.key_len as usize <= argc_usize
                    && cached.min_argc as usize <= argc_usize
                    && cached.emf.is_some() =>
            {
                let emf = cached.emf.expect("checked above");
                (
                    cached.epoch,
                    cached.key_len as usize,
                    cached.class_ids,
                    SchemeValue::from(emf),
                )
            }
            Some(_) => return Ok(None),
            None => {
                let Some((epoch, key_len, class_ids, emf)) = generic.sole_emf(argc_usize) else {
                    return Ok(None);
                };
                (epoch, key_len, class_ids, emf)
            }
        };
        // Inline before/primary/after when taking one value.
        if matches!(
            site,
            super::ResultSite::Taken {
                wants_list: false,
                ..
            }
        ) && matches!(values, ReturnValues::Take(1) | ReturnValues::TakeRest(1))
            && let Some(flow) = self.combined_inline(
                pc, next, callee, block, generic, first_arg, argc, epoch, key_len, class_ids, site,
                values,
            )?
        {
            return Ok(Some(flow));
        }
        if !self.depend_on_generic(generic, epoch) {
            return Ok(None);
        }
        let frame = self.frame(pc, pc)?;
        let code: CodeRef = self.code_ref_value(SchemeValue::from(block));
        self.value_op(
            Op::CheckCode(code),
            &[callee],
            Type::Closure(Some(code)),
            Some(frame),
        );
        let args = self.regs(first_arg..first_arg + argc)?;
        let mut checked = Vec::with_capacity(argc_usize);
        for (index, arg) in args.iter().enumerate() {
            if index < key_len {
                checked.push(self.check_class(
                    *arg,
                    ClassRef::from_class_id(class_ids[index]),
                    frame,
                ));
            } else {
                checked.push(*arg);
            }
        }
        let body_block = closure_block(emf);
        let body = self.value_const(emf);
        let frame = self.site_call_frame(site, next)?;
        let result = self.call_body(body, body_block, &checked, argc, values, frame)?;
        self.finish_result(site, next, result)?;
        Ok(Some(Flow::Next))
    }

    /// Inline before/primary/after methods.
    #[allow(clippy::too_many_arguments)]
    fn combined_inline(
        &mut self,
        pc: usize,
        next: usize,
        callee: IrValue,
        block: Gc<'gc, CodeBlock<'gc>>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        first_arg: u32,
        argc: u32,
        epoch: u32,
        key_len: usize,
        class_ids: [u32; 2],
        site: super::ResultSite,
        values: ReturnValues,
    ) -> Result<Option<Flow>, String> {
        if generic.flags().custom_dfun() || !generic.combination().is_standard() {
            return Ok(None);
        }
        if key_len != generic.dispatch_width_value() as usize {
            return Ok(None);
        }
        let mut ids = Vec::with_capacity(key_len);
        for &id in &class_ids[..key_len] {
            let Some(id) = ClassId::new(id) else {
                return Ok(None);
            };
            ids.push(id);
        }
        // Pad key to full argc with top class.
        let Some(&first_id) = ids.first() else {
            return Ok(None);
        };
        while ids.len() < argc as usize {
            ids.push(first_id);
        }
        let Some(methods) = generic.applicable_for_class_ids(self.ctx, &ids, false) else {
            return Ok(None);
        };
        if methods.len() > 4 {
            return Ok(None);
        }
        let mut befores = Vec::new();
        let mut afters = Vec::new();
        let mut primary = None;
        for method in methods.iter().copied() {
            let mflags = method.flags();
            if mflags.slot_reader()
                || mflags.slot_writer()
                || mflags.boot_slot_protocol()
                || closure_block(method.body()).is_none()
            {
                return Ok(None);
            }
            match method.role() {
                MethodRole::Before => befores.push(method.body()),
                MethodRole::After => afters.push(method.body()),
                MethodRole::Primary => {
                    if primary.is_some() || !mflags.no_next() {
                        return Ok(None);
                    }
                    primary = Some(method.body());
                }
                _ => return Ok(None),
            }
        }
        let Some(primary) = primary else {
            return Ok(None);
        };
        // Afters run least specific first.
        afters.reverse();
        let Some(spread) = self.ctx.try_hook(Hook::ValuesSpread) else {
            return Ok(None);
        };
        if !self.depend_on_generic(generic, epoch) {
            return Ok(None);
        }
        let frame = self.frame(pc, pc)?;
        let code: CodeRef = self.code_ref_value(SchemeValue::from(block));
        self.value_op(
            Op::CheckCode(code),
            &[callee],
            Type::Closure(Some(code)),
            Some(frame),
        );
        let args = self.regs(first_arg..first_arg + argc)?;
        let mut checked = Vec::with_capacity(argc as usize);
        for (index, arg) in args.iter().enumerate() {
            if index < key_len {
                checked.push(self.check_class(
                    *arg,
                    ClassRef::from_class_id(class_ids[index]),
                    frame,
                ));
            } else {
                checked.push(*arg);
            }
        }

        // The befores, the primary and the afters are not the site's result:
        // baseline cannot resume after one of them, so their frames are the
        // return point's without returned values, and the check after the
        // dispatch stays.
        let inner = self.frame(next, next)?;
        for body in befores {
            let body_block = closure_block(body);
            let body_value = self.value_const(body);
            self.call_body(
                body_value,
                body_block,
                &checked,
                argc,
                ReturnValues::Ignore,
                inner,
            )?;
        }

        // Read primary values.
        let primary_block = closure_block(primary);
        let primary_value = self.value_const(primary);
        let result = self.call_body(
            primary_value,
            primary_block,
            &checked,
            argc,
            ReturnValues::TakeRest(0),
            inner,
        )?;
        let count = self.value_op(Op::ReturnedCount(0), &[result], Type::Int32, None);
        let addr = self.value_op(Op::ReturnedAddr(0), &[result], Type::Int64, None);
        let first = self.value_op(Op::ReturnedValue(0), &[result], Type::Any, None);
        let one = self.value_op(Op::Const(Constant::Int32(1)), &[], Type::Int32, None);
        let is_one = self.value_op(Op::CmpI32(Cmp::Eq), &[count, one], Type::Bool, None);
        let kept = self.func.new_block();
        let one_block = self.func.new_block();
        let many_block = self.func.new_block();
        let kept_value = self.func.append_param(kept, Type::Any);
        let kept_count = self.func.append_param(kept, Type::Int32);
        self.branch_to(is_one, one_block, many_block);
        self.cur = one_block;
        let edge = self.func.edge(kept, &[first, count]);
        self.terminate(Terminator::Jump(edge));
        self.cur = many_block;
        let list_frame = self.frame(next, next)?;
        let list = self.value_op(
            Op::CallOp(Helper::ValuesRest.helper_ref()),
            &[addr, count],
            Type::Any,
            Some(list_frame),
        );
        let edge = self.func.edge(kept, &[list, count]);
        self.terminate(Terminator::Jump(edge));
        self.cur = kept;

        for body in afters {
            let body_block = closure_block(body);
            let body_value = self.value_const(body);
            self.call_body(
                body_value,
                body_block,
                &checked,
                argc,
                ReturnValues::Ignore,
                inner,
            )?;
        }

        // Deliver via spread if not one value.
        let one_after = self.value_op(Op::Const(Constant::Int32(1)), &[], Type::Int32, None);
        let ok = self.value_op(
            Op::CmpI32(Cmp::Eq),
            &[kept_count, one_after],
            Type::Bool,
            None,
        );
        // Extra count for rest.
        let deliver = self.func.new_block();
        let respread = self.func.new_block();
        let done = self.func.new_block();
        let done_value = self.func.append_param(done, Type::Any);
        let done_extra = self.func.append_param(done, Type::Int32);
        self.branch_to(ok, deliver, respread);
        self.cur = deliver;
        let zero = self.value_op(Op::Const(Constant::Int32(0)), &[], Type::Int32, None);
        let edge = self.func.edge(done, &[kept_value, zero]);
        self.terminate(Terminator::Jump(edge));
        self.cur = respread;
        let spread_frame = self.site_call_frame(site, next)?;
        let spread_value = self.value_const(spread);
        let spread_call = self.value_op(
            Op::CallGeneric(values),
            &[spread_value, kept_value],
            Type::Any,
            Some(spread_frame),
        );
        let parts = self.call_parts(spread_call, values)?;
        let spread_extra = match parts.get(1) {
            Some(&extra) => extra,
            None => self.value_op(Op::Const(Constant::Int32(0)), &[], Type::Int32, None),
        };
        let edge = self.func.edge(done, &[parts[0], spread_extra]);
        self.terminate(Terminator::Jump(edge));
        self.cur = done;
        self.finish_taken_value(site, next, done_value, Some(done_extra))?;
        Ok(Some(Flow::Next))
    }

    /// Cursor-cache call.
    #[allow(clippy::too_many_arguments)]
    fn cursor_call(
        &mut self,
        pc: usize,
        next: usize,
        callee: IrValue,
        block: Gc<'gc, CodeBlock<'gc>>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        first_arg: u32,
        argc: u32,
        epoch: u32,
        entries: Vec<(u32, u32, SchemeValue<'gc>, SchemeValue<'gc>)>,
        values: ReturnValues,
        site: super::ResultSite,
    ) -> Result<Option<Flow>, String> {
        let shapes = entries.len();
        let taking = !matches!(
            values,
            ReturnValues::Forward | ReturnValues::Ignore | ReturnValues::Error
        );
        let rest_list = matches!(
            site,
            super::ResultSite::Taken {
                wants_list: true,
                ..
            }
        );
        let chain_takes =
            !rest_list && matches!(values, ReturnValues::Take(1) | ReturnValues::TakeRest(1));
        if shapes > MAX_CHAIN_TESTS || (shapes >= 2 && taking && !chain_takes) {
            return Ok(None);
        }
        if !self.depend_on_generic(generic, epoch) {
            return Ok(None);
        }
        let frame = self.frame(pc, pc)?;
        let code: CodeRef = self.code_ref_value(SchemeValue::from(block));
        self.value_op(
            Op::CheckCode(code),
            &[callee],
            Type::Closure(Some(code)),
            Some(frame),
        );
        let args = self.regs(first_arg..first_arg + argc)?;
        if shapes > 1 {
            let chain: Vec<_> = entries
                .iter()
                .map(|&(class0, class1, body, lead)| (class0, class1, body, Some(lead)))
                .collect();
            let parts = {
                let frame = self.site_call_frame(site, next)?;
                self.poly_call(callee, &args, argc, &chain, values, frame)?
            };
            self.finish_parts(site, next, &parts)?;
            return Ok(Some(Flow::Next));
        }
        let argc_usize = argc as usize;
        let (class0, class1, body, lead) = entries[0];
        let checked0 = self.check_class(args[0], ClassRef::from_class_id(class0), frame);
        let mut checked = vec![checked0];
        if argc_usize == 2 {
            checked.push(self.check_class(args[1], ClassRef::from_class_id(class1), frame));
        }
        let body_block = closure_block(body);
        let body = self.value_const(body);
        let result =
            {
                let frame = self.site_call_frame(site, next)?;
                self.call_body_led(body, body_block, &checked, argc, values, frame, Some(lead))?
            };
        self.finish_result(site, next, result)?;
        Ok(Some(Flow::Next))
    }

    /// Mark generic as cold.
    fn note_cold_generic(&mut self, generic: Gc<'gc, GenericDescriptor<'gc>>) {
        let flags = generic.flags();
        if flags.simple() && !flags.has_eql() && !flags.resolved_keys() {
            self.func.generic_cold = true;
        }
    }

    /// Record generic dependency.
    fn depend_on_generic(&mut self, generic: Gc<'gc, GenericDescriptor<'gc>>, epoch: u32) -> bool {
        let value: SchemeValue<'gc> = generic.into();
        if let Some(dep) = self
            .func
            .generic_deps
            .iter()
            .find(|dep| dep.generic.bits() == value.bits())
        {
            return dep.epoch == epoch;
        }
        self.func.generic_deps.push(GenericDep {
            generic: value,
            epoch,
        });
        true
    }

    /// Call body with `#f` next.
    fn call_body(
        &mut self,
        body: IrValue,
        block: Option<Gc<'gc, CodeBlock<'gc>>>,
        args: &[IrValue],
        argc: u32,
        values: ReturnValues,
        frame: FrameState,
    ) -> Result<IrValue, String> {
        self.call_body_led(body, block, args, argc, values, frame, None)
    }

    /// Call body with lead next.
    #[allow(clippy::too_many_arguments)]
    fn call_body_led(
        &mut self,
        body: IrValue,
        block: Option<Gc<'gc, CodeBlock<'gc>>>,
        args: &[IrValue],
        argc: u32,
        values: ReturnValues,
        frame: FrameState,
        lead: Option<SchemeValue<'gc>>,
    ) -> Result<IrValue, String> {
        let next_arg = match lead {
            Some(lead) => self.value_const(lead),
            None => self.bits(SchemeValue::VALUE_FALSE as u64),
        };
        let mut operands = vec![body, next_arg];
        operands.extend_from_slice(args);
        let known = block.filter(|block| {
            block.kind == CodeBlockKind::Bytecode && super::direct_arity_matches(block, argc + 1)
        });
        let op = match known {
            Some(block) => Op::CallKnown(self.code_ref_value(SchemeValue::from(block)), values),
            None => Op::CallGeneric(values),
        };
        Ok(self.value_op(op, &operands, Type::Any, Some(frame)))
    }

    /// Class-test chain.
    fn poly_call(
        &mut self,
        callee: IrValue,
        args: &[IrValue],
        argc: u32,
        entries: &[(u32, u32, SchemeValue<'gc>, Option<SchemeValue<'gc>>)],
        values: ReturnValues,
        frame: FrameState,
    ) -> Result<Vec<IrValue>, String> {
        let join = self.func.new_block();
        let mut params = vec![self.func.append_param(join, Type::Any)];
        if matches!(values, ReturnValues::TakeRest(1)) {
            params.push(self.func.append_param(join, Type::Int32));
        }
        let mut test = self.cur;
        for &(class0, class1, body, lead) in entries {
            let body_block = self.func.new_block();
            let miss = self.func.new_block();
            self.cur = test;
            let ok0 = self.value_op(
                Op::TestClass(ClassRef::from_class_id(class0)),
                &[args[0]],
                Type::Bool,
                None,
            );
            if argc == 2 {
                let second = self.func.new_block();
                self.branch_to(ok0, second, miss);
                self.cur = second;
                let ok1 = self.value_op(
                    Op::TestClass(ClassRef::from_class_id(class1)),
                    &[args[1]],
                    Type::Bool,
                    None,
                );
                self.branch_to(ok1, body_block, miss);
            } else {
                self.branch_to(ok0, body_block, miss);
            }
            self.cur = body_block;
            let block = closure_block(body);
            let body_value = self.value_const(body);
            let result = self.call_body_led(body_value, block, args, argc, values, frame, lead)?;
            let parts = self.call_parts(result, values)?;
            let edge = self.func.edge(join, &parts);
            self.terminate(Terminator::Jump(edge));
            test = miss;
        }
        self.cur = test;
        let mut operands = vec![callee];
        operands.extend_from_slice(args);
        let result = self.value_op(Op::CallGeneric(values), &operands, Type::Any, Some(frame));
        let parts = self.call_parts(result, values)?;
        let edge = self.func.edge(join, &parts);
        self.terminate(Terminator::Jump(edge));
        self.cur = join;
        Ok(params)
    }

    /// Join values.
    fn call_parts(&mut self, call: IrValue, values: ReturnValues) -> Result<Vec<IrValue>, String> {
        match values {
            ReturnValues::Take(1) => Ok(vec![self.value_op(
                Op::ReturnedValue(0),
                &[call],
                Type::Any,
                None,
            )]),
            ReturnValues::TakeRest(1) => {
                let value = self.value_op(Op::ReturnedValue(0), &[call], Type::Any, None);
                let extra = self.value_op(Op::ReturnedCount(1), &[call], Type::Int32, None);
                Ok(vec![value, extra])
            }
            ReturnValues::Forward | ReturnValues::Ignore | ReturnValues::Error => Ok(vec![call]),
            _ => Err("a chain of generic arms takes one value".to_string()),
        }
    }

    /// Slot-cache call.
    #[allow(clippy::too_many_arguments)]
    fn slot_call(
        &mut self,
        pc: usize,
        next: usize,
        callee: IrValue,
        block: Gc<'gc, CodeBlock<'gc>>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        first_arg: u32,
        argc: u32,
        values: ReturnValues,
        site: super::ResultSite,
    ) -> Result<Option<Flow>, String> {
        let flags = generic.flags();
        if flags.custom_dfun() || flags.resolved_keys() || flags.has_eql() {
            return Ok(None);
        }
        // Slot reads return one value.
        let taking = !matches!(
            values,
            ReturnValues::Forward | ReturnValues::Ignore | ReturnValues::Error
        );
        if taking && !matches!(values, ReturnValues::Take(1) | ReturnValues::TakeRest(1)) {
            return Ok(None);
        }
        if let super::ResultSite::Taken {
            wants_list: true, ..
        } = site
        {
            return Ok(None);
        }

        // Validate before emit.
        let mut arms = Vec::new();
        if argc == 1 {
            let epoch = generic.epoch();
            if epoch & 1 != 0 {
                return Ok(None);
            }
            let Some(readers) = generic.slot_reader_entries(epoch) else {
                return Ok(None);
            };
            if readers.len() <= MAX_SLOT_TESTS {
                for (class_id, stamp, index) in readers {
                    let Some(id) = crate::heap::object::ClassId::new(class_id) else {
                        return Ok(None);
                    };
                    let Some(class) = class_table(self.ctx).lookup(id) else {
                        return Ok(None);
                    };
                    // Only scheme classes.
                    let slot_count = class.slot_count();
                    if class.category() != ClassCategory::Scheme
                        || class.layout_stamp() != stamp
                        || index as usize >= slot_count
                    {
                        return Ok(None);
                    }
                    let Ok(len) = u32::try_from(slot_count) else {
                        return Ok(None);
                    };
                    arms.push((class, class_id, stamp, len, index));
                }
                if !arms.is_empty() && !self.depend_on_generic(generic, epoch) {
                    return Ok(None);
                }
            }
        }

        let frame = self.frame(pc, pc)?;
        // Verify code.
        let code: CodeRef = self.code_ref_value(SchemeValue::from(block));
        self.value_op(
            Op::CheckCode(code),
            &[callee],
            Type::Closure(Some(code)),
            Some(frame),
        );
        let generic_value = self.value_const(generic.into());
        let args = self.regs(first_arg..first_arg + argc)?;
        for &(class, ..) in &arms {
            self.depend_on_class(class);
        }

        let helper = |this: &mut Self, args: &[IrValue]| {
            if argc == 1 {
                this.value_op(
                    Op::CallOp(Helper::GenericSlotGet.helper_ref()),
                    &[generic_value, args[0]],
                    Type::Any,
                    Some(frame),
                )
            } else {
                // Writer.
                this.value_op(
                    Op::CallOp(Helper::GenericSlotPut.helper_ref()),
                    &[generic_value, args[0], args[1]],
                    Type::Any,
                    Some(frame),
                )
            }
        };
        let result = if arms.is_empty() {
            helper(self, &args[..])
        } else {
            self.slot_read_chain(args[0], &arms, |this| helper(this, &args[..]))
        };
        self.finish_taken_value(site, next, result, None)?;
        Ok(Some(Flow::Next))
    }

    /// Record slot read.
    fn depend_on_class(&mut self, class: Gc<'gc, ClassDescriptor<'gc>>) {
        let value = SchemeValue::from(class);
        if self
            .func
            .class_deps
            .iter()
            .any(|dep| dep.class == value && dep.kind == ClassDepKind::Layout)
        {
            return;
        }
        self.func.class_deps.push(ClassDep {
            class: value,
            id: class.id().bits(),
            stamp: class.layout_stamp(),
            kind: ClassDepKind::Layout,
        });
    }

    /// Record subtree dependency.
    fn depend_on_subtree(&mut self, class: Gc<'gc, ClassDescriptor<'gc>>, stamp: u32) -> bool {
        let value = SchemeValue::from(class);
        if let Some(dep) = self
            .func
            .class_deps
            .iter()
            .find(|dep| dep.kind == ClassDepKind::Subtree && dep.class == value)
        {
            return dep.stamp == stamp;
        }
        self.func.class_deps.push(ClassDep {
            class: value,
            id: class.id().bits(),
            stamp,
            kind: ClassDepKind::Subtree,
        });
        true
    }

    /// Subtree members.
    fn subtree_members(
        &self,
        root: Gc<'gc, ClassDescriptor<'gc>>,
    ) -> Option<Vec<(Gc<'gc, ClassDescriptor<'gc>>, u32)>> {
        let table = class_table(self.ctx);
        let mut members: Vec<(Gc<'gc, ClassDescriptor<'gc>>, u32)> = Vec::new();
        let mut pending = vec![root];
        while let Some(class) = pending.pop() {
            if members.iter().any(|(known, _)| known.id() == class.id()) {
                continue;
            }
            if members.len() >= MAX_CHAIN_TESTS {
                return None;
            }
            let stamp = class.subclass_stamp();
            members.push((class, stamp));
            for weak in class.direct_subclasses().iter() {
                let Some(sub) = weak.upgrade(self.ctx) else {
                    continue;
                };
                let current = table.lookup(sub.id())?;
                if current.as_ptr() != sub.as_ptr() {
                    return None;
                }
                pending.push(current);
            }
        }
        Some(members)
    }

    /// Subclass arms.
    fn subclass_arms(
        &self,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        entries: &[(u32, u32, SchemeValue<'gc>)],
    ) -> Option<(
        Vec<(u32, u32, SchemeValue<'gc>)>,
        Vec<(Gc<'gc, ClassDescriptor<'gc>>, u32)>,
    )> {
        let flags = generic.flags();
        if flags.has_eql() || flags.resolved_keys() || flags.custom_dfun() || !flags.simple() {
            return None;
        }
        let methods = generic.methods();
        if methods.iter().any(|method| method.specializers().len() > 1) {
            return None;
        }
        let top = ClassId::new(builtin_class_ids::TOP)?;
        let mut specialized: Vec<ClassId> = methods
            .iter()
            .map(|method| method.specializers().first().copied().unwrap_or(top))
            .collect();
        specialized.push(top);
        let signature = |class: Gc<'gc, ClassDescriptor<'gc>>| -> Vec<ClassId> {
            class
                .cpl()
                .iter()
                .copied()
                .filter(|id| specialized.contains(id))
                .collect()
        };
        let table = class_table(self.ctx);
        let mut extended = entries.to_vec();
        let mut dependencies = Vec::new();
        for &(class0, class1, body) in entries {
            if class1 != 0 {
                continue;
            }
            let Some(root) = ClassId::new(class0).and_then(|id| table.lookup(id)) else {
                continue;
            };
            if root.flags().no_subclasses() {
                continue;
            }
            let Some(members) = self.subtree_members(root) else {
                continue;
            };
            if members.len() < 2 {
                continue;
            }
            let root_signature = signature(root);
            if members
                .iter()
                .any(|(member, _)| signature(*member) != root_signature)
            {
                continue;
            }
            let body_bits = body.bits();
            let mut additions = Vec::new();
            for (member, _) in members.iter().skip(1) {
                let id = member.id().bits();
                match extended.iter().find(|entry| entry.0 == id && entry.1 == 0) {
                    Some(entry) if entry.2.bits() != body_bits => return None,
                    Some(_) => {}
                    None => additions.push((id, 0, body)),
                }
            }
            extended.extend(additions);
            dependencies.extend(members);
        }
        (extended.len() > entries.len()).then_some((extended, dependencies))
    }

    /// Subclass extension.
    fn subclass_extension(
        &self,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        argc: u32,
        entries: Vec<(u32, u32, SchemeValue<'gc>)>,
        taking: bool,
        chain_takes: bool,
    ) -> (
        Vec<(u32, u32, SchemeValue<'gc>)>,
        Vec<(Gc<'gc, ClassDescriptor<'gc>>, u32)>,
    ) {
        if argc != 1 {
            return (entries, Vec::new());
        }
        match self.subclass_arms(generic, &entries) {
            Some((extended, dependencies))
                if extended.len() <= MAX_CHAIN_TESTS
                    && !(extended.len() >= 2 && taking && !chain_takes) =>
            {
                (extended, dependencies)
            }
            _ => (entries, Vec::new()),
        }
    }

    /// Slot read chain.
    fn slot_read_chain(
        &mut self,
        obj: IrValue,
        arms: &[SlotArm<'gc>],
        fallback: impl FnOnce(&mut Self) -> IrValue,
    ) -> IrValue {
        let join = self.func.new_block();
        let joined = self.func.append_param(join, Type::Any);
        let mut test = self.cur;
        for &(_, class_id, stamp, len, index) in arms {
            let class_ok = self.func.new_block();
            let miss = self.func.new_block();
            self.cur = test;
            let is_class = self.value_op(
                Op::TestClass(ClassRef::from_class_id(class_id)),
                &[obj],
                Type::Bool,
                None,
            );
            self.branch_to(is_class, class_ok, miss);

            // Check layout stamp/len.
            self.cur = class_ok;
            let storage = self.value_op(
                Op::Load {
                    field: 1,
                    region: MemoryRegion::Heap,
                },
                &[obj],
                Type::Any,
                None,
            );
            let word = self.value_op(
                Op::Load {
                    field: 0,
                    region: MemoryRegion::Heap,
                },
                &[storage],
                Type::Int64,
                None,
            );
            let layout = ((u64::from(len)) << 32) | u64::from(stamp);
            let expected = self.value_op(
                Op::Const(Constant::Int64(layout as i64)),
                &[],
                Type::Int64,
                None,
            );
            let same_layout = self.value_op(Op::EqBits, &[word, expected], Type::Bool, None);
            let read = self.func.new_block();
            self.branch_to(same_layout, read, miss);

            self.cur = read;
            // Load slot.
            let slot = self.value_op(
                Op::Load {
                    field: 1 + index,
                    region: MemoryRegion::Heap,
                },
                &[storage],
                Type::Any,
                None,
            );
            let empty = self.bits(SchemeValue::empty().bits());
            let unbound = self.value_op(Op::EqBits, &[slot, empty], Type::Bool, None);
            let found = self.func.new_block();
            self.branch_to(unbound, miss, found);

            self.cur = found;
            let edge = self.func.edge(join, &[slot]);
            self.terminate(Terminator::Jump(edge));
            test = miss;
        }
        self.cur = test;
        let result = fallback(self);
        let edge = self.func.edge(join, &[result]);
        self.terminate(Terminator::Jump(edge));
        self.cur = join;
        joined
    }

    /// Finish chain.
    fn finish_parts(
        &mut self,
        site: super::ResultSite,
        next: usize,
        parts: &[IrValue],
    ) -> Result<(), String> {
        self.finish_taken_value(site, next, parts[0], parts.get(1).copied())
    }

    /// Finish taken value.
    pub(super) fn finish_taken_value(
        &mut self,
        site: super::ResultSite,
        next: usize,
        value: IrValue,
        extra: Option<IrValue>,
    ) -> Result<(), String> {
        let super::ResultSite::Taken {
            first_value,
            mv_pc,
            base,
            values,
            wants_list,
        } = site
        else {
            return self.finish_result(site, next, value);
        };
        let (count, rest) = match values {
            ReturnValues::Take(count) => (count, false),
            ReturnValues::TakeRest(count) => (count, true),
            _ => return Err("a taken value has no value count".to_string()),
        };
        if count != 1 || wants_list {
            return Err("a taken value is one value without a list".to_string());
        }
        self.set(first_value, value)?;
        if rest {
            let extra = match extra {
                Some(extra) => extra,
                None => self.value_op(Op::Const(Constant::Int32(0)), &[], Type::Int32, None),
            };
            self.mv_rest = Some((extra, None));
        }
        self.pending_return = Some(next);
        self.pending_mv = Some(super::PendingMv {
            mv_pc,
            base,
            result: value,
            continues: true,
        });
        self.mv_taken = Some((mv_pc, count));
        Ok(())
    }
}

/// Block of closure constant.
fn closure_block<'gc>(value: SchemeValue<'gc>) -> Option<Gc<'gc, CodeBlock<'gc>>> {
    value
        .is::<Closure>()
        .then(|| Closure::code_block(value.downcast::<Closure>()))
}
