//! SSA building from CPS IR
//!
//! This module builds Cranelift SSA IR from the CPS IR defined in [`cps`](crate::cps::term) module.

use std::{collections::HashMap, mem::offset_of};

use cranelift::prelude::{FunctionBuilder, FunctionBuilderContext, InstBuilder, IntCC, types};
use cranelift_codegen::{ir, isa::CallConv};
use cranelift_module::{FuncId, Module};
use rsgc::{Mutation, sync::thread::Thread};

use crate::{
    cps::term::{Atom, ContRef, Expression, FuncRef, Term, TermRef},
    expander::core::LVarRef,
    jit::{JitContext, ssa::primitives::PrimitiveLowererTable},
    runtime::{
        Context, State,
        value::{Closure, Pair, TypeCode8, Value, Vector},
        vm::{VMState, primitive::PrimitiveLocations, *},
    },
};

#[allow(dead_code)]
struct Current<'gc, 'a, 'f> {
    jit: &'a mut JitContext<'gc>,
    ctx: Context<'gc>,

    mc_value: ir::Value,
    state_value: ir::Value,

    exit_block: ir::Block,

    // defined for functions, None for reified continuations
    return_cont: Option<LVarRef<'gc>>,
    return_cont_val: Option<ir::Value>,
    arguments: &'a [LVarRef<'gc>],
    variadic: Option<LVarRef<'gc>>,
    self_ref: Option<ir::Value>,
    self_bind: LVarRef<'gc>,

    blockmap: HashMap<ContRef<'gc>, ir::Block>,

    vars: HashMap<LVarRef<'gc>, ir::Value>,

    sig_call: ir::SigRef,
    sig_callk: ir::SigRef,
    sig_prim: ir::SigRef,

    builder: FunctionBuilder<'f>,

    thunks: ImportedThunks,
}

impl<'gc, 'a, 'f> Current<'gc, 'a, 'f> {
    fn new(
        jit_context: &'a mut JitContext<'gc>,
        mc_value: ir::Value,
        state_value: ir::Value,
        self_ref: LVarRef<'gc>,
        context: Context<'gc>,
        _name: Value<'gc>,
        return_cont: Option<LVarRef<'gc>>,
        return_cont_val: Option<ir::Value>,
        arguments: &'a [LVarRef<'gc>],
        variadic: Option<LVarRef<'gc>>,

        _id: FuncId,

        mut builder: FunctionBuilder<'f>,
    ) -> Self {
        let thunks = ImportedThunks::new(jit_context, &mut builder);

        let exit_block = builder.create_block();

        builder.append_block_param(exit_block, types::I8);
        builder.append_block_param(exit_block, types::I64);
        builder.func.layout.set_cold(exit_block);

        let mut sig_call = ir::Signature::new(CallConv::Tail);
        {
            sig_call.params.push(ir::AbiParam::new(types::I64)); // mc
            sig_call.params.push(ir::AbiParam::new(types::I64)); // state
            sig_call.params.push(ir::AbiParam::new(types::I64)); // continuation

            sig_call.returns.push(ir::AbiParam::new(types::I8)); // code
            sig_call.returns.push(ir::AbiParam::new(types::I64)); // value
        }
        let sig_call = builder.import_signature(sig_call);

        let mut sig_callk = ir::Signature::new(CallConv::Tail);
        {
            sig_callk.params.push(ir::AbiParam::new(types::I64)); // mc
            sig_callk.params.push(ir::AbiParam::new(types::I64)); // state

            sig_callk.returns.push(ir::AbiParam::new(types::I8)); // code
            sig_callk.returns.push(ir::AbiParam::new(types::I64)); // value
        }

        let sig_callk = builder.import_signature(sig_callk);

        let mut sig_prim = ir::Signature::new(CallConv::SystemV);
        {
            sig_prim.params.push(ir::AbiParam::new(types::I64)); // mc
            sig_prim.params.push(ir::AbiParam::new(types::I64)); // state

            sig_prim.returns.push(ir::AbiParam::new(types::I8)); // code
            sig_prim.returns.push(ir::AbiParam::new(types::I64)); // value
        }

        let sig_prim = builder.import_signature(sig_prim);

        let mut vars = HashMap::new();
        if let (Some(return_cont), Some(return_cont_val)) = (return_cont, return_cont_val) {
            vars.insert(return_cont, return_cont_val);
        }

        Self {
            state_value,
            mc_value,
            exit_block,
            sig_call,
            sig_callk,
            sig_prim,

            jit: jit_context,
            return_cont,
            return_cont_val,
            arguments,
            blockmap: HashMap::new(),
            variadic,
            thunks,
            self_bind: self_ref,

            builder,
            ctx: context,
            vars,
            self_ref: None,
        }
    }

    pub fn br_if_cell(
        &mut self,
        val: ir::Value,
        if_cell: ir::Block,
        if_cell_args: &[ir::BlockArg],
        if_not_cell: ir::Block,
        if_not_cell_args: &[ir::BlockArg],
    ) {
        let masked = self
            .builder
            .ins()
            .band_imm(val, Value::NOT_CELL_MASK as i64);
        let check = self.builder.ins().icmp_imm(IntCC::Equal, masked, 0i64);
        let check_not_empty =
            self.builder
                .ins()
                .icmp_imm(IntCC::NotEqual, val, Value::VALUE_EMPTY as i64);

        let x = self.builder.ins().band(check, check_not_empty);

        self.builder
            .ins()
            .brif(x, if_cell, if_cell_args, if_not_cell, if_not_cell_args);
    }
    #[allow(dead_code)]
    pub fn br_if_immediate(
        &mut self,
        val: ir::Value,
        if_immediate: ir::Block,
        if_immediate_args: &[ir::BlockArg],
        if_not_immediate: ir::Block,
        if_not_immediate_args: &[ir::BlockArg],
    ) {
        let masked = self
            .builder
            .ins()
            .band_imm(val, Value::NOT_CELL_MASK as i64);
        let check = self.builder.ins().icmp_imm(IntCC::NotEqual, masked, 0i64);

        self.builder.ins().brif(
            check,
            if_immediate,
            if_immediate_args,
            if_not_immediate,
            if_not_immediate_args,
        );
    }

    pub fn typecode8(&mut self, val: ir::Value) -> ir::Value {
        self.builder
            .ins()
            .load(types::I8, ir::MemFlags::new(), val, 0)
    }

    pub fn typecode16(&mut self, val: ir::Value) -> ir::Value {
        self.builder
            .ins()
            .load(types::I16, ir::MemFlags::new(), val, 0)
    }

    fn atom(&mut self, atom: Atom<'gc>) -> ir::Value {
        match atom {
            Atom::Constant(value) => {
                if !value.is_cell() {
                    // value is not a cell and does not require GC tracing.
                    return self.builder.ins().iconst(types::I64, value.bits() as i64);
                }

                // allocate storage in JIT module so that GC can trace constant if necessary.
                let gv = self.jit.constant_in_func(&mut self.builder.func, value);

                self.builder.ins().global_value(types::I64, gv)
            }

            Atom::Local(local) => self.vars[&local],

            Atom::Global(global) => {
                let gv = self.jit.global_in_func(&mut self.builder.func, global);
                let loaded = self.builder.ins().global_value(types::I64, gv);
                self.builder.ins().load(
                    types::I64,
                    ir::MemFlags::new(),
                    loaded,
                    offset_of!(Pair, car) as i32,
                )
            }

            _ => unreachable!(),
        }
    }

    pub fn block_for_cont(&mut self, cont: ContRef<'gc>) -> ir::Block {
        if let Some(block) = self.blockmap.get(&cont) {
            return *block;
        }

        let block = self.builder.create_block();
        for _ in 0..cont.args().len() {
            self.builder.append_block_param(block, types::I64);
        }
        self.blockmap.insert(cont, block);
        block
    }

    #[allow(dead_code)]
    pub fn cond(&mut self, cond: Atom<'gc>, kcons: LVarRef<'gc>, kalt: LVarRef<'gc>) {
        let cond = self.atom(cond);

        let is_false = self
            .builder
            .ins()
            .icmp_imm(IntCC::Equal, cond, Value::VALUE_FALSE);

        let bb_false = self.builder.create_block();
        let bb_true = self.builder.create_block();

        self.builder
            .ins()
            .brif(is_false, bb_false, &[], bb_true, &[]);

        self.builder.switch_to_block(bb_true);
        self.continue_to(kcons, &[]);
        self.builder.switch_to_block(bb_false);
        self.continue_to(kalt, &[]);
    }

    pub fn continue_to(&mut self, k: LVarRef<'gc>, args: &[Atom<'gc>]) -> ir::Inst {
        let values = args.iter().map(|atom| self.atom(*atom)).collect::<Vec<_>>();

        if let Some(k) = self.jit.reify_info.free_vars.conts.get(&k)
            && !k.is_return()
            && !self.jit.reify_info.reified.contains(&k)
        {
            let block = self.block_for_cont(*k);
            let block_args = values
                .into_iter()
                .map(|v| ir::BlockArg::Value(v))
                .collect::<Vec<_>>();
            return self.builder.ins().jump(block, &block_args);
        }

        // reified or return continuation: perform a call
        let k = self.atom(Atom::Local(k));
        let args = values;
        self.push_args::<true>(k, &args);

        let thread_addr = self.builder.ins().load(
            types::I64,
            ir::MemFlags::new(),
            self.mc_value,
            Mutation::THREAD_OFFSET as i32,
        );
        let take_yieldpoint = self.builder.ins().load(
            types::I32,
            ir::MemFlags::new(),
            thread_addr,
            Thread::TAKE_YIELDPOIN_OFFSET as i32,
        );

        let no_yieldpoint = self
            .builder
            .ins()
            .icmp_imm(IntCC::Equal, take_yieldpoint, 0);

        let bb_continue = self.builder.create_block();
        let bb_yield = self.builder.create_block();

        self.builder
            .ins()
            .brif(no_yieldpoint, bb_continue, &[], bb_yield, &[]);

        self.builder.func.layout.set_cold(bb_yield);
        self.builder.switch_to_block(bb_yield);
        let undef_value: ir::Value = self
            .builder
            .ins()
            .iconst(types::I64, Value::VALUE_UNDEFINED as i64);
        let code = self
            .builder
            .ins()
            .iconst(types::I8, VMReturnCode::Yield as i8 as i64);

        /* save continuation, proc set to UNDEFINED

          args should already be on stack

        */
        self.builder.ins().store(
            ir::MemFlags::new(),
            k,
            self.state_value,
            offset_of!(State, vm_state) as i32 + offset_of!(VMState, k) as i32,
        );
        self.builder.ins().store(
            ir::MemFlags::new(),
            undef_value,
            self.state_value,
            offset_of!(State, vm_state) as i32 + offset_of!(VMState, proc) as i32,
        );
        self.builder.ins().jump(
            self.exit_block,
            &[ir::BlockArg::Value(code), ir::BlockArg::Value(undef_value)],
        );

        self.builder.switch_to_block(bb_continue);

        let addr = self.builder.ins().load(
            types::I64,
            ir::MemFlags::new(),
            k,
            offset_of!(Closure, code) as i32,
        );

        self.builder.ins().return_call_indirect(
            self.sig_callk,
            addr,
            &[self.mc_value, self.state_value],
        )
    }

    pub fn push_args<const K: bool>(&mut self, proc: ir::Value, args: &[ir::Value]) {
        let state = self.state_value;
        let vm_off = offset_of!(State, vm_state) as i32;
        let argc_off = offset_of!(VMState, argc) as i32;
        let argv_off = offset_of!(VMState, argv) as i32;

        let argv =
            self.builder
                .ins()
                .load(types::I64, ir::MemFlags::new(), state, vm_off + argv_off);

        for (i, val) in args.iter().enumerate() {
            self.builder
                .ins()
                .store(ir::MemFlags::new(), *val, argv, (i * 8) as i32);
        }

        self.builder
            .ins()
            .store(ir::MemFlags::new(), argv, state, vm_off + argv_off);

        let argc = self.builder.ins().iconst(types::I64, args.len() as i64);
        self.builder
            .ins()
            .store(ir::MemFlags::new(), argc, state, vm_off + argc_off);

        let offset = if K {
            offset_of!(VMState, k) as i32
        } else {
            offset_of!(VMState, proc) as i32
        };

        self.builder
            .ins()
            .store(ir::MemFlags::new(), proc, state, vm_off + offset);
    }

    pub fn return_call<const CHECK_CLOS: bool>(
        &mut self,
        proc: ir::Value,
        k: ir::Value,
        args: &[ir::Value],
    ) -> ir::Inst {
        self.push_args::<false>(proc, args);
        if CHECK_CLOS {
            let err_code = self
                .builder
                .ins()
                .iconst(types::I8, VMReturnCode::NonProcedureCall as i8 as i64);

            let bb_check_closure = self.builder.create_block();

            self.br_if_cell(
                proc,
                bb_check_closure,
                &[],
                self.exit_block,
                &[ir::BlockArg::Value(err_code), ir::BlockArg::Value(proc)],
            );
            self.builder.switch_to_block(bb_check_closure);
            let proc_type = self.typecode8(proc);
            let is_closure = self.builder.ins().icmp_imm(
                IntCC::Equal,
                proc_type,
                TypeCode8::CLOSURE.bits() as i64,
            );

            let bb_closure = self.builder.create_block();

            self.builder.ins().brif(
                is_closure,
                bb_closure,
                &[],
                self.exit_block,
                &[ir::BlockArg::Value(err_code), ir::BlockArg::Value(proc)],
            );

            self.builder.switch_to_block(bb_closure);
        }

        let thread_addr = self.builder.ins().load(
            types::I64,
            ir::MemFlags::new(),
            self.mc_value,
            Mutation::THREAD_OFFSET as i32,
        );
        let take_yieldpoint = self.builder.ins().load(
            types::I32,
            ir::MemFlags::new(),
            thread_addr,
            Thread::TAKE_YIELDPOIN_OFFSET as i32,
        );

        let no_yieldpoint = self
            .builder
            .ins()
            .icmp_imm(IntCC::Equal, take_yieldpoint, 0);

        let bb_continue = self.builder.create_block();
        let bb_yield = self.builder.create_block();
        self.builder
            .ins()
            .brif(no_yieldpoint, bb_continue, &[], bb_yield, &[]);

        self.builder.func.layout.set_cold(bb_yield);
        self.builder.switch_to_block(bb_yield);

        let undef_value = self
            .builder
            .ins()
            .iconst(types::I64, Value::VALUE_UNDEFINED as i64);
        let code = self
            .builder
            .ins()
            .iconst(types::I8, VMReturnCode::Yield as i8 as i64);

        self.builder.ins().store(
            ir::MemFlags::new(),
            k,
            self.state_value,
            offset_of!(State, vm_state) as i32 + offset_of!(VMState, k) as i32,
        );
        self.builder.ins().store(
            ir::MemFlags::new(),
            proc,
            self.state_value,
            offset_of!(State, vm_state) as i32 + offset_of!(VMState, proc) as i32,
        );
        self.builder.ins().jump(
            self.exit_block,
            &[ir::BlockArg::Value(code), ir::BlockArg::Value(undef_value)],
        );

        self.builder.switch_to_block(bb_continue);

        let addr = self.builder.ins().load(
            types::I64,
            ir::MemFlags::new(),
            proc,
            offset_of!(Closure, code) as i32,
        );

        self.builder.ins().return_call_indirect(
            self.sig_call,
            addr,
            &[self.mc_value, self.state_value, k],
        )
    }

    pub fn letk(&mut self, conts: &[ContRef<'gc>]) {
        for &cont in conts.iter() {
            if !self.jit.reify_info.reified.contains(&cont) {
                // no special handling required: continuation is not reified
                // thus can be handled as a normal basic block.
                self.block_for_cont(cont);
                continue;
            }
            let meta = cont.make_meta(self.ctx);
            let meta = self.atom(Atom::Constant(meta));

            // reified continuation: we need to allocate a closure for it.
            let free = &self.jit.reify_info.free_vars.cvars[&cont];
            let nfree_c = self.builder.ins().iconst(types::I64, free.len() as i64);

            let closure = self.builder.ins().call(
                self.thunks.alloc_closure_k,
                &[self.mc_value, self.state_value, nfree_c, meta],
            );
            let closure = self.builder.inst_results(closure)[0];

            let cont_ref = self
                .jit
                .module
                .declare_func_in_func(self.jit.reified_contmap[&cont], &mut self.builder.func);
            let faddr = self.builder.ins().func_addr(types::I64, cont_ref);
            self.builder.ins().store(
                ir::MemFlags::new(),
                faddr,
                closure,
                offset_of!(Closure, code) as i32,
            );

            if !free.is_empty() {
                let free_vec = self.builder.ins().load(
                    types::I64,
                    ir::MemFlags::new(),
                    closure,
                    offset_of!(Closure, free) as i32,
                );

                let data = self
                    .builder
                    .ins()
                    .iadd_imm(free_vec, offset_of!(Vector, data) as i64);

                for (i, &var) in free.iter().enumerate() {
                    let val = self.vars[&var];
                    self.builder
                        .ins()
                        .store(ir::MemFlags::new(), val, data, (i * 8) as i32);
                }
            }

            self.vars.insert(cont.binding(), closure);
        }
    }

    pub fn fix(&mut self, funcs: &[FuncRef<'gc>]) {
        for &func in funcs.iter() {
            let meta = func.make_meta(self.ctx);
            let meta = self.atom(Atom::Constant(meta));

            let free = &self.jit.reify_info.free_vars.fvars[&func];

            let nfree_c = self.builder.ins().iconst(types::I64, free.len() as i64);

            let closure = self.builder.ins().call(
                self.thunks.alloc_closure,
                &[self.mc_value, self.state_value, nfree_c, meta],
            );

            let closure = self.builder.inst_results(closure)[0];

            let func_ref = self
                .jit
                .module
                .declare_func_in_func(self.jit.funcmap[&func], &mut self.builder.func);

            let faddr = self.builder.ins().func_addr(types::I64, func_ref);
            self.builder.ins().store(
                ir::MemFlags::new(),
                faddr,
                closure,
                offset_of!(Closure, code) as i32,
            );

            if !free.is_empty() {
                let free_vec = self.builder.ins().load(
                    types::I64,
                    ir::MemFlags::new(),
                    closure,
                    offset_of!(Closure, free) as i32,
                );

                let data = self
                    .builder
                    .ins()
                    .iadd_imm(free_vec, offset_of!(Vector, data) as i64);

                for (i, &var) in free.iter().enumerate() {
                    let val = self.vars[&var];
                    self.builder
                        .ins()
                        .store(ir::MemFlags::new(), val, data, (i * 8) as i32);
                }
            }

            self.vars.insert(func.binding, closure);
        }
    }

    pub fn prelude(&mut self) {
        self.load_args();
        self.load_captured();
    }

    pub fn load_captured(&mut self) {
        let captured = if self.return_cont.is_some() {
            let f = self.jit.reify_info.free_vars.funcs[&self.self_bind];
            &self.jit.reify_info.free_vars.fvars[&f]
        } else {
            let k = self.jit.reify_info.free_vars.conts[&self.self_bind];
            &self.jit.reify_info.free_vars.cvars[&k]
        };

        if captured.is_empty() {
            return;
        }

        let self_proc = self.self_ref.unwrap();
        let free_vec = self.builder.ins().load(
            types::I64,
            ir::MemFlags::new(),
            self_proc,
            offset_of!(Closure, free) as i32,
        );

        let data = self
            .builder
            .ins()
            .iadd_imm(free_vec, offset_of!(Vector, data) as i64);

        for (i, &var) in captured.iter().enumerate() {
            let val =
                self.builder
                    .ins()
                    .load(types::I64, ir::MemFlags::new(), data, (i * 8) as i32);
            self.vars.insert(var, val);
        }
    }

    pub fn check_argc(&mut self) -> ir::Value {
        let state = self.state_value;
        let vm_off = offset_of!(State, vm_state) as i32;
        let argc_off = offset_of!(VMState, argc) as i32;

        let expected = self.arguments.len();

        let got =
            self.builder
                .ins()
                .load(types::I64, ir::MemFlags::new(), state, vm_off + argc_off);

        let cc = if self.variadic.is_some() {
            IntCC::UnsignedLessThan
        } else {
            IntCC::NotEqual
        };

        let is_mismatch = self.builder.ins().icmp_imm(cc, got, expected as i64);

        let bb_mismatch = self.builder.create_block();
        let bb_ok = self.builder.create_block();

        self.builder
            .ins()
            .brif(is_mismatch, bb_mismatch, &[], bb_ok, &[]);

        self.builder.switch_to_block(bb_mismatch);

        let expected = if self.variadic.is_some() {
            -(expected as i64)
        } else {
            expected as i64
        };
        let expected = self
            .builder
            .ins()
            .iconst(types::I64, Value::new(expected as i32).bits() as i64);

        let err_code = self
            .builder
            .ins()
            .iconst(types::I8, VMReturnCode::ArgumentMismatch as i8 as i64);

        self.builder.ins().jump(
            self.exit_block,
            &[ir::BlockArg::Value(err_code), ir::BlockArg::Value(expected)],
        );

        self.builder.switch_to_block(bb_ok);

        got
    }

    pub fn load_args(&mut self) {
        let state = self.state_value;
        let vm_off = offset_of!(State, vm_state) as i32;

        let argv_off = offset_of!(VMState, argv) as i32;
        let proc_off = offset_of!(VMState, proc) as i32;

        let got = self.check_argc();

        let proc =
            self.builder
                .ins()
                .load(types::I64, ir::MemFlags::new(), state, vm_off + proc_off);
        self.self_ref = Some(proc);

        let argv =
            self.builder
                .ins()
                .load(types::I64, ir::MemFlags::new(), state, vm_off + argv_off);

        let mut i = 0;

        for arg in self.arguments.iter() {
            let val =
                self.builder
                    .ins()
                    .load(types::I64, ir::MemFlags::new(), argv, (i * 8) as i32);

            self.vars.insert(*arg, val);
            i += 1;
        }

        if let Some(variadic) = self.variadic {
            let need_to_cons = self.builder.ins().icmp_imm(
                IntCC::UnsignedGreaterThan,
                got,
                self.arguments.len() as i64,
            );

            let no_cons = self.builder.create_block();
            let cons = self.builder.create_block();
            let resume = self.builder.create_block();
            self.builder.append_block_param(resume, types::I64);
            let varval = self.builder.block_params(resume)[0];
            self.vars.insert(variadic, varval);

            self.builder
                .ins()
                .brif(need_to_cons, cons, &[], no_cons, &[]);

            self.builder.switch_to_block(no_cons);
            let null = self
                .builder
                .ins()
                .iconst(types::I64, Value::VALUE_NULL as i64);
            self.builder
                .ins()
                .jump(resume, &[ir::BlockArg::Value(null)]);

            self.builder.switch_to_block(cons);

            let from = self.builder.ins().iconst(types::I64, i as i64);

            let call = self.builder.ins().call(
                self.thunks.cons_rest,
                &[self.mc_value, self.state_value, from],
            );
            let result = self.builder.inst_results(call)[0];
            self.builder
                .ins()
                .jump(resume, &[ir::BlockArg::Value(result)]);

            self.builder.switch_to_block(resume);
        }
    }

    pub fn term(&mut self, term: TermRef<'gc>) {
        match *term {
            Term::Let(bind, expr, next) => {
                let value = self.expr(expr);
                let value = if value.is_none() {
                    self.atom(Atom::Constant(Value::undefined()))
                } else {
                    value.unwrap()
                };

                self.vars.insert(bind, value);

                self.term(next);
            }

            Term::Fix(funs, next) => {
                self.fix(&funs);
                self.term(next);
            }

            Term::Letk(conts, next) => {
                self.letk(&conts);
                self.term(next);
            }

            Term::App(proc, k, args, _) => {
                let mut vargs = Vec::new();
                let kval = self.atom(Atom::Local(k));

                for arg in args.iter() {
                    let value = self.atom(*arg);
                    vargs.push(value);
                }

                let proc = self.atom(proc);

                self.return_call::<true>(proc, kval, &vargs);
            }

            Term::Continue(k, args, _) => {
                self.continue_to(k, &args);
            }

            Term::If(test, kcons, kalt, _) => {
                let test = self.atom(test);
                let is_false = self
                    .builder
                    .ins()
                    .icmp_imm(IntCC::Equal, test, Value::VALUE_FALSE);
                let bb_false = self.builder.create_block();
                let bb_true = self.builder.create_block();

                self.builder
                    .ins()
                    .brif(is_false, bb_false, &[], bb_true, &[]);
                self.builder.switch_to_block(bb_true);
                self.continue_to(kcons, &[]);
                self.builder.switch_to_block(bb_false);
                self.continue_to(kalt, &[]);
            }
            _ => todo!(),
        }
    }

    pub fn expr(&mut self, expr: Expression<'gc>) -> Option<ir::Value> {
        match expr {
            Expression::PrimCall(name, args, _) => {
                let prim_lower = PrimitiveLowererTable::get(self.ctx);
                let args = args.iter().map(|atom| self.atom(*atom)).collect::<Vec<_>>();
                if let Some(lower) = prim_lower.map.get(&name) {
                    return lower(self, name, &args);
                }

                let location =
                    PrimitiveLocations::get(self.ctx, name).expect("BUG: Primitive not defined");

                let addr = self
                    .builder
                    .ins()
                    .iconst(types::I64, location as usize as i64);

                let name = self.atom(Atom::Constant(name));
                self.push_args::<false>(name, &args);

                let call = self.builder.ins().call_indirect(
                    self.sig_prim,
                    addr,
                    &[self.mc_value, self.state_value],
                );

                let code = self.builder.inst_results(call)[0];
                let value = self.builder.inst_results(call)[1];

                let should_exit = self.builder.ins().icmp_imm(
                    IntCC::NotEqual,
                    code,
                    VMReturnCode::Return as i8 as i64,
                );

                let resume = self.builder.create_block();

                self.builder.ins().brif(
                    should_exit,
                    resume,
                    &[],
                    self.exit_block,
                    &[ir::BlockArg::Value(code), ir::BlockArg::Value(value)],
                );

                self.builder.switch_to_block(resume);

                Some(value)
            }
        }
    }
}

pub struct ImportedThunks {
    pub argument_mismatch: ir::FuncRef,
    pub cons_rest: ir::FuncRef,
    pub alloc_closure: ir::FuncRef,
    pub alloc_closure_k: ir::FuncRef,
}

impl ImportedThunks {
    pub fn new(jit: &mut JitContext<'_>, builder: &mut FunctionBuilder<'_>) -> Self {
        let argument_mismatch = jit
            .module
            .declare_func_in_func(jit.thunks.argument_mismatch, &mut builder.func);
        let cons_rest = jit
            .module
            .declare_func_in_func(jit.thunks.cons_rest, &mut builder.func);

        let alloc_closure = jit
            .module
            .declare_func_in_func(jit.thunks.alloc_closure, &mut builder.func);
        let alloc_closure_k = jit
            .module
            .declare_func_in_func(jit.thunks.alloc_closure_k, &mut builder.func);

        Self {
            argument_mismatch,
            cons_rest,
            alloc_closure,
            alloc_closure_k,
        }
    }
}

mod primitives;

pub fn translate<'gc>(
    jit_context: &mut JitContext<'gc>,

    self_ref: LVarRef<'gc>,
    context: Context<'gc>,
    name: Value<'gc>,
    return_cont: Option<LVarRef<'gc>>,
    arguments: &[LVarRef<'gc>],
    variadic: Option<LVarRef<'gc>>,
    body: TermRef<'gc>,
    id: FuncId,
    fctx: &mut FunctionBuilderContext,
    cctx: &mut cranelift_codegen::Context,
) {
    let mut builder = FunctionBuilder::new(&mut cctx.func, fctx);
    let entry = builder.create_block();
    builder.append_block_params_for_function_params(entry);
    let mc_value = builder.block_params(entry)[0];
    let state_value = builder.block_params(entry)[1];

    let return_cont_val = if return_cont.is_some() {
        Some(builder.block_params(entry)[2])
    } else {
        None
    };

    builder.switch_to_block(entry);

    let mut ssa = Current::new(
        jit_context,
        mc_value,
        state_value,
        self_ref,
        context,
        name,
        return_cont,
        return_cont_val,
        arguments,
        variadic,
        id,
        builder,
    );

    ssa.prelude();
    ssa.term(body);
    ssa.builder.switch_to_block(ssa.exit_block);
    let exit_code = ssa.builder.block_params(ssa.exit_block)[0];
    let exit_value = ssa.builder.block_params(ssa.exit_block)[1];

    ssa.builder.ins().return_(&[exit_code, exit_value]);

    ssa.builder.seal_all_blocks();
    ssa.builder.finalize();
}
