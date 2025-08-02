//! SSA building from CPS IR
//!
//! This module builds Cranelift SSA IR from the CPS IR defined in [`cps`](crate::cps::term) module.

use std::{collections::HashMap, mem::offset_of};

use cranelift::prelude::{FunctionBuilder, InstBuilder, IntCC, types};
use cranelift_codegen::ir;
use cranelift_module::{FuncId, Module};
use hashlink::LinkedHashSet;

use crate::{
    cps::term::{Atom, ContRef, FuncRef},
    expander::core::{LVar, LVarRef},
    jit::JitContext,
    runtime::{
        Context, State,
        value::{Closure, Pair, Value, Vector},
        vm::VMState,
    },
};

struct Current<'gc, 'a, 'f> {
    jit: &'a mut JitContext<'gc>,
    ctx: Context<'gc>,

    mc_value: ir::Value,
    state_value: ir::Value,

    exit_block: ir::Block,

    // defined for functions, None for reified continuations
    name: Value<'gc>,
    return_cont: Option<LVarRef<'gc>>,
    arguments: &'a [LVarRef<'gc>],
    variadic: Option<LVarRef<'gc>>,
    self_ref: Option<ir::Value>,
    self_bind: LVarRef<'gc>,

    blockmap: HashMap<ContRef<'gc>, ir::Block>,

    id: FuncId,
    imports: HashMap<FuncId, ir::FuncRef>,
    vars: HashMap<LVarRef<'gc>, ir::Value>,

    builder: FunctionBuilder<'f>,

    thunks: ImportedThunks,
}

impl<'gc, 'a, 'f> Current<'gc, 'a, 'f> {
    fn new(
        jit_context: &'a mut JitContext<'gc>,
        mc_value: ir::Value,
        state_value: ir::Value,
        self_rfe: LVarRef<'gc>,
        context: Context<'gc>,
        name: Value<'gc>,
        return_cont: Option<LVarRef<'gc>>,
        arguments: &'a [LVarRef<'gc>],
        variadic: Option<LVarRef<'gc>>,

        id: FuncId,

        mut builder: FunctionBuilder<'f>,
    ) -> Self {
        let thunks = ImportedThunks::new(jit_context, &mut builder);

        let exit_block = builder.create_block();

        builder.append_block_param(exit_block, types::I64);
        builder.func.layout.set_cold(exit_block);

        Self {
            state_value,
            mc_value,
            exit_block,

            jit: jit_context,
            return_cont,
            arguments,
            blockmap: HashMap::new(),
            variadic,
            name,
            thunks,
            self_bind: self_rfe,
            id,
            imports: HashMap::new(),
            builder,
            ctx: context,
            vars: HashMap::new(),
            self_ref: None,
        }
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
        self.return_call(k, &values)
    }

    pub fn return_call(&mut self, proc: ir::Value, args: &[ir::Value]) -> ir::Inst {
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

        self.builder.ins().return_call(
            self.thunks.call_func,
            &[self.mc_value, self.state_value, proc],
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

            // reified continuation: we need to allocate a closure for it.
            let free = &self.jit.reify_info.free_vars.cvars[&cont];

            let nfree_c = self.builder.ins().iconst(types::I64, free.len() as i64);

            let closure = self.builder.ins().call(
                self.thunks.alloc_closure_k,
                &[self.mc_value, self.state_value, nfree_c],
            );
            let closure = self.builder.inst_results(closure)[0];

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
            let free = &self.jit.reify_info.free_vars.fvars[&func];

            let nfree_c = self.builder.ins().iconst(types::I64, free.len() as i64);

            let closure = self.builder.ins().call(
                self.thunks.alloc_closure,
                &[self.mc_value, self.state_value, nfree_c],
            );
            let closure = self.builder.inst_results(closure)[0];

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
        let state = self.state_value;
        let vm_off = offset_of!(State, vm_state) as i32;
        let argc_off = offset_of!(VMState, argc) as i32;
        let argv_off = offset_of!(VMState, argv) as i32;

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

        let expected = self.arguments.len() + self.return_cont.is_none() as usize;

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
        let expected = self.builder.ins().iconst(types::I64, expected as i64);
        let name = self.atom(Atom::Constant(self.name));
        let has_variadic = self
            .builder
            .ins()
            .iconst(types::I8, self.variadic.is_some() as i64);
        let has_return_cont = self
            .builder
            .ins()
            .iconst(types::I8, self.return_cont.is_some() as i64);
        self.builder.ins().return_call(
            self.thunks.argument_mismatch,
            &[
                self.mc_value,
                self.state_value,
                name,
                expected,
                got,
                has_variadic,
                has_return_cont,
            ],
        );
        self.builder.func.layout.set_cold(bb_mismatch);

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

        if let Some(return_cont) = self.return_cont {
            let val =
                self.builder
                    .ins()
                    .load(types::I64, ir::MemFlags::new(), argv, (i * 8) as i32);

            self.vars.insert(return_cont, val);
            i += 1;
        }

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

            let call = self
                .builder
                .ins()
                .call(self.thunks.cons_rest, &[self.mc_value, self.state_value]);
            let result = self.builder.inst_results(call)[0];
            self.builder
                .ins()
                .jump(resume, &[ir::BlockArg::Value(result)]);

            self.builder.switch_to_block(resume);
        }
    }
}

pub struct ImportedThunks {
    pub call_func: ir::FuncRef,
    pub argument_mismatch: ir::FuncRef,
    pub cons_rest: ir::FuncRef,
    pub alloc_closure: ir::FuncRef,
    pub alloc_closure_k: ir::FuncRef,
}

impl ImportedThunks {
    pub fn new(jit: &mut JitContext<'_>, builder: &mut FunctionBuilder<'_>) -> Self {
        let call_func = jit
            .module
            .declare_func_in_func(jit.thunks.call_func, &mut builder.func);
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
            call_func,
            argument_mismatch,
            cons_rest,
            alloc_closure,
            alloc_closure_k,
        }
    }
}
