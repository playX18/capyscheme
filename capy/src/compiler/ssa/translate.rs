use std::{io::Write, mem::offset_of};

use crate::rsgc::{Gc, sync::thread::Thread};
use crate::{
    compiler::ssa::{ContOrFunc, SSABuilder, VarDef, primitive::PrimValue},
    cps::term::{Atom, ContRef, Expression, FuncRef, Term, TermRef},
    expander::core::LVarRef,
    runtime::{
        Context, State,
        value::{Closure, ReturnCode, Tagged, Value, Vector},
    },
};
use cranelift::prelude::{InstBuilder, IntCC, types};
use cranelift_codegen::ir::{self, BlockArg};
use cranelift_module::{DataId, Linkage, Module};
use pretty::BoxAllocator;

pub enum Callee {
    /// Callee is obtained by loading code pointer from a closure
    /// and doing an indirect tail-call.
    Indirect {
        target: ir::Value,
        closure: ir::Value,
    },
    /// Callee is a direct function reference, and a direct tail-call can be performed.
    Direct {
        target: ir::FuncRef,
        closure: ir::Value,
    },

    /// Callee is a self-recursive function, and the function body block is returned.
    SelfRec(ir::Block),
}

impl<'gc, 'a, 'f> SSABuilder<'gc, 'a, 'f> {
    pub fn is_self_reference(&mut self, var: LVarRef<'gc>) -> bool {
        match self.target {
            ContOrFunc::Cont(c) => Gc::ptr_eq(c.binding, var),
            ContOrFunc::Func(f) => Gc::ptr_eq(f.binding, var),
        }
    }

    pub fn import_static(&mut self, name: &'static str, typ: ir::Type) -> ir::Value {
        let data_id = *self
            .module_builder
            .import_data
            .entry(name)
            .or_insert_with(|| {
                self.module_builder
                    .module
                    .declare_data(name, Linkage::Import, false, false)
                    .unwrap()
            });
        let data = self.import_data(data_id);

        self.builder.ins().global_value(typ, data)
    }

    pub fn import_data(&mut self, data: DataId) -> ir::GlobalValue {
        self.data_imports
            .entry(data)
            .or_insert_with(|| {
                let gv = self
                    .module_builder
                    .module
                    .declare_data_in_func(data, &mut self.builder.func);
                gv
            })
            .clone()
    }

    pub fn var(&mut self, var: LVarRef<'gc>) -> ir::Value {
        if self.is_self_reference(var) {
            return self.rator;
        }
        match *self.variables.get(&var).unwrap_or_else(|| {
            let alloc = ::pretty::BoxAllocator;
            let pretty = match self.target {
                ContOrFunc::Func(f) => f.pretty::<_, &BoxAllocator>(&alloc),
                ContOrFunc::Cont(c) => c.pretty::<_, &BoxAllocator>(&alloc),
            };

            pretty.1.render(80, &mut std::io::stdout()).unwrap();
            println!();
            std::io::stdout().flush().unwrap();
            panic!(
                "{}@{:p} var not found when compiling {}({})",
                var.name,
                var,
                match self.target {
                    ContOrFunc::Func(f) => f.binding.name,
                    ContOrFunc::Cont(c) => c.binding.name,
                },
                match self.target {
                    ContOrFunc::Func(f) => f.name,
                    ContOrFunc::Cont(c) => c.name,
                }
            )
        }) {
            VarDef::Comparison(val) => {
                let true_ = self.builder.ins().iconst(types::I64, Value::VALUE_TRUE);
                let false_ = self.builder.ins().iconst(types::I64, Value::VALUE_FALSE);

                let val = self.builder.ins().select(val, true_, false_);
                self.debug_local(var, val);
                val
            }
            VarDef::Free(ix) => {
                let free = self.builder.ins().load(
                    types::I64,
                    ir::MemFlags::trusted().with_can_move(),
                    self.rator,
                    offset_of!(Closure, free) as i32,
                );
                let value = self.builder.ins().load(
                    types::I64,
                    ir::MemFlags::trusted().with_can_move(),
                    free,
                    Vector::OFFSET_OF_DATA as i32 + (ix * 8) as i32,
                );
                value
            }

            VarDef::Value(val) => val,
        }
    }

    pub fn entrypoint(&mut self, rands: ir::Value, num_rands: ir::Value) {
        match self.target {
            ContOrFunc::Func(func) => {
                self.set_debug_loc(func.source);
                self.check_yield(self.rator, rands, num_rands);
            }

            ContOrFunc::Cont(cont) => {
                self.set_debug_loc(cont.source());
            }
        }

        self.load_free_vars();
        self.load_arguments(rands, num_rands);
    }

    pub fn default_error_handler(&mut self) -> LVarRef<'gc> {
        match self.target {
            ContOrFunc::Func(func) => func.handler_cont,
            ContOrFunc::Cont(cont) => cont.handler.get(),
        }
    }

    fn load_arguments(&mut self, mut rands: ir::Value, mut num_rands: ir::Value) {
        let (params, rest) = match self.target {
            ContOrFunc::Func(func) => (func.args, func.variadic),
            ContOrFunc::Cont(cont) => (cont.args(), cont.variadic()),
        };

        let ctx = self.builder.ins().get_pinned_reg(types::I64);

        let state = self
            .builder
            .ins()
            .iadd_imm(ctx, Context::OFFSET_OF_STATE as i64);
        /* reset the runstack to the state it was before call */
        self.builder.ins().store(
            ir::MemFlags::trusted().with_can_move(),
            rands,
            state,
            offset_of!(State, runstack) as i32,
        );
        let runstack_start = self.builder.ins().load(
            types::I64,
            ir::MemFlags::trusted().with_can_move(),
            state,
            offset_of!(State, runstack_start) as i32,
        );

        let is_eq = self
            .builder
            .ins()
            .icmp(IntCC::NotEqual, rands, runstack_start);
        self.builder
            .ins()
            .trapnz(is_eq, ir::TrapCode::HEAP_OUT_OF_BOUNDS);

        if let ContOrFunc::Func(func) = self.target {
            let retk = self.builder.ins().load(
                types::I64,
                ir::MemFlags::trusted().with_can_move(),
                rands,
                0,
            );
            let reth = self.builder.ins().load(
                types::I64,
                ir::MemFlags::trusted().with_can_move(),
                rands,
                8,
            );

            rands = self.builder.ins().iadd_imm(rands, 16);
            num_rands = self.builder.ins().iadd_imm(num_rands, -2);

            self.debug_local(func.return_cont, retk);
            self.debug_local(func.handler_cont, reth);

            self.variables.insert(func.return_cont, VarDef::Value(retk));
            self.variables
                .insert(func.handler_cont, VarDef::Value(reth));
        }

        if params.len() != 0 {
            if let Some(rest) = rest {
                let not_enough = self.builder.ins().icmp_imm(
                    IntCC::UnsignedLessThan,
                    num_rands,
                    params.len() as i64,
                );

                let succ = self.builder.create_block();
                let err = self.builder.create_block();
                self.builder.func.layout.set_cold(err);
                self.builder.ins().brif(not_enough, err, &[], succ, &[]);

                self.builder.switch_to_block(err);
                {
                    let got = num_rands;
                    let expected = -(params.len() as isize);
                    let expected = self.builder.ins().iconst(types::I64, expected as i64);
                    let err = self.builder.ins().call(
                        self.thunks.wrong_number_of_args,
                        &[ctx, self.rator, got, expected],
                    );
                    let err = self.builder.inst_results(err)[0];
                    let error_handler = self.default_error_handler();

                    self.continue_to(error_handler, &[err]);
                }
                self.builder.switch_to_block(succ);
                {
                    for (i, param) in params.iter().enumerate() {
                        let value = self.builder.ins().load(
                            types::I64,
                            ir::MemFlags::trusted().with_can_move(),
                            rands,
                            (i * 8) as i32,
                        );
                        self.variables.insert(*param, VarDef::Value(value));
                        self.debug_local(*param, value);
                    }

                    let need_cons = self.builder.ins().icmp_imm(
                        IntCC::NotEqual,
                        num_rands,
                        params.len() as i64,
                    );

                    let null = self
                        .builder
                        .ins()
                        .iconst(types::I64, Value::null().bits() as i64);

                    let succ = self.builder.create_block();
                    let cons_block = self.builder.create_block();
                    self.builder.append_block_param(succ, types::I64);
                    self.builder.ins().brif(
                        need_cons,
                        cons_block,
                        &[],
                        succ,
                        &[BlockArg::Value(null)],
                    );
                    self.builder.switch_to_block(cons_block);
                    {
                        if rest.is_referenced() {
                            let from = self.builder.ins().iconst(types::I64, params.len() as i64);
                            let cons = self
                                .builder
                                .ins()
                                .call(self.thunks.cons_rest, &[ctx, rands, num_rands, from]);
                            let list = self.builder.inst_results(cons)[0];

                            self.builder.ins().jump(succ, &[BlockArg::Value(list)]);
                        } else {
                            // rest list is not referenced: do not waste time materializing it
                            let null = self
                                .builder
                                .ins()
                                .iconst(types::I64, Value::null().bits() as i64);
                            self.builder.ins().jump(succ, &[BlockArg::Value(null)]);
                        }
                    }
                    self.builder.switch_to_block(succ);
                    {
                        let ls = self.builder.block_params(succ)[0];
                        self.variables.insert(rest, VarDef::Value(ls));
                        self.debug_local(rest, ls);
                    }
                }
            } else {
                let exact =
                    self.builder
                        .ins()
                        .icmp_imm(IntCC::Equal, num_rands, params.len() as i64);

                let succ = self.builder.create_block();
                let err = self.builder.create_block();
                self.builder.func.layout.set_cold(err);
                self.builder.ins().brif(exact, succ, &[], err, &[]);

                self.builder.switch_to_block(err);
                {
                    let got = num_rands;
                    let expected = params.len() as isize;
                    let expected = self.builder.ins().iconst(types::I64, expected as i64);
                    let err = self.builder.ins().call(
                        self.thunks.wrong_number_of_args,
                        &[ctx, self.rator, got, expected],
                    );
                    let err = self.builder.inst_results(err)[0];
                    let error_handler = self.default_error_handler();

                    self.continue_to(error_handler, &[err]);
                }
                self.builder.switch_to_block(succ);
                for (i, param) in params.iter().enumerate() {
                    let value = self.builder.ins().load(
                        types::I64,
                        ir::MemFlags::trusted().with_can_move(),
                        rands,
                        (i * 8) as i32,
                    );
                    self.variables.insert(*param, VarDef::Value(value));
                    self.debug_local(*param, value);
                }
            }
        } else {
            if let Some(rest) = rest {
                let succ = self.builder.create_block();
                let cons_block = self.builder.create_block();

                let null = self
                    .builder
                    .ins()
                    .iconst(types::I64, Value::null().bits() as i64);

                self.builder.append_block_param(succ, types::I64);

                let need_cons = self.builder.ins().icmp_imm(IntCC::NotEqual, num_rands, 0);

                self.builder
                    .ins()
                    .brif(need_cons, cons_block, &[], succ, &[BlockArg::Value(null)]);
                self.builder.switch_to_block(cons_block);
                {
                    if rest.is_referenced() {
                        let from = self.builder.ins().iconst(types::I64, 0);
                        let cons = self
                            .builder
                            .ins()
                            .call(self.thunks.cons_rest, &[ctx, rands, num_rands, from]);
                        let list = self.builder.inst_results(cons)[0];

                        self.builder.ins().jump(succ, &[BlockArg::Value(list)]);
                    } else {
                        // rest list is not referenced: do not waste time materializing it
                        let null = self
                            .builder
                            .ins()
                            .iconst(types::I64, Value::null().bits() as i64);
                        self.builder.ins().jump(succ, &[BlockArg::Value(null)]);
                    }
                }
                self.builder.switch_to_block(succ);
                {
                    let ls = self.builder.block_params(succ)[0];
                    self.variables.insert(rest, VarDef::Value(ls));
                    self.debug_local(rest, ls);
                }
            } else {
                let succ = self.builder.create_block();
                let err = self.builder.create_block();

                self.builder.func.layout.set_cold(err);

                let nonzero = self.builder.ins().icmp_imm(IntCC::NotEqual, num_rands, 0);

                self.builder.ins().brif(nonzero, err, &[], succ, &[]);

                self.builder.switch_to_block(err);
                {
                    let got = num_rands;
                    let expected = 0isize;
                    let expected = self.builder.ins().iconst(types::I64, expected as i64);
                    let handler = self.default_error_handler();
                    let err = self.builder.ins().call(
                        self.thunks.wrong_number_of_args,
                        &[ctx, num_rands, got, expected],
                    );
                    let err = self.builder.inst_results(err)[0];
                    self.continue_to(handler, &[err]);
                }

                self.builder.switch_to_block(succ);
            }
        }
    }

    pub fn block_for_cont(&mut self, k: ContRef<'gc>) -> ir::Block {
        if let Some(block) = self.blockmap.get(&k) {
            return *block;
        }

        let block = self.builder.create_block();
        if k.cold {
            self.builder.func.layout.set_cold(block);
        }
        self.blockmap.insert(k, block);
        for _ in k.args.iter() {
            self.builder.append_block_param(block, types::I64);
        }

        if k.variadic().is_some() {
            self.builder.append_block_param(block, types::I64);
        }

        for i in 0..k.args.len() {
            self.variables.insert(
                k.args[i],
                VarDef::Value(self.builder.block_params(block)[i]),
            );
        }
        if let Some(rest) = k.variadic() {
            self.variables.insert(
                rest,
                VarDef::Value(self.builder.block_params(block)[k.args.len()]),
            );
        }

        block
    }

    pub fn get_callee_k(&mut self, var: LVarRef<'gc>) -> Callee {
        if let Some(cont) = self.module_builder.reify_info.free_vars.conts.get(&var)
            && let Some(func_id) = self.module_builder.func_for_cont.get(cont)
        {
            let func_id = *func_id;
            let closure = self.var(var);
            let func_ref = self
                .module_builder
                .module
                .declare_func_in_func(func_id, &mut self.builder.func);
            return Callee::Direct {
                target: func_ref,
                closure,
            };
        }

        let callee = self.atom(Atom::Local(var));
        let code = self.builder.ins().load(
            types::I64,
            ir::MemFlags::trusted().with_can_move(),
            callee,
            offset_of!(Closure, code) as i32,
        );
        Callee::Indirect {
            target: code,
            closure: callee,
        }
    }

    /// Get callee entrypoint or report error to `handler`.
    pub fn get_callee(&mut self, callee: &Atom<'gc>, handler: LVarRef<'gc>) -> Callee {
        if let Atom::Local(var) = callee {
            if let Some(func) = self.module_builder.reify_info.free_vars.funcs.get(var)
                && let Some(func_id) = self.module_builder.func_for_func.get(func)
            {
                let func_id = *func_id;
                let closure = self.atom(*callee);
                let func_ref = self
                    .module_builder
                    .module
                    .declare_func_in_func(func_id, &mut self.builder.func);
                return Callee::Direct {
                    target: func_ref,
                    closure,
                };
            }
        }
        let callee = self.atom(*callee);

        let ctx = self.builder.ins().get_pinned_reg(types::I64);
        let get_clos_code = self.builder.create_block();
        let error = self.builder.create_block();
        self.builder.func.layout.set_cold(error);
        self.branch_if_has_typ8(callee, Closure::TC8.bits(), get_clos_code, &[], error, &[]);
        self.builder.switch_to_block(error);
        {
            let err = self
                .builder
                .ins()
                .call(self.thunks.non_applicable, &[ctx, callee]);
            let err = self.builder.inst_results(err)[0];
            self.continue_to(handler, &[err]);
        }

        self.builder.switch_to_block(get_clos_code);
        {
            let code = self.builder.ins().load(
                types::I64,
                ir::MemFlags::trusted().with_can_move(),
                callee,
                offset_of!(Closure, code) as i32,
            );
            Callee::Indirect {
                target: code,
                closure: callee,
            }
        }
    }

    /// Push arguments to runstack. Returns the base of the arguments array.
    pub fn push_args(&mut self, args: &[ir::Value]) -> ir::Value {
        let ctx = self.builder.ins().get_pinned_reg(types::I64);
        let state = self
            .builder
            .ins()
            .iadd_imm(ctx, Context::OFFSET_OF_STATE as i64);
        let runstack = self.builder.ins().load(
            types::I64,
            ir::MemFlags::trusted().with_can_move(),
            state,
            offset_of!(State, runstack) as i32,
        );
        let new_runstack = self
            .builder
            .ins()
            .iadd_imm(runstack, (args.len() * 8) as i64);
        self.builder.ins().store(
            ir::MemFlags::trusted().with_can_move(),
            new_runstack,
            state,
            offset_of!(State, runstack) as i32,
        );
        let runstack_start = self.builder.ins().load(
            types::I64,
            ir::MemFlags::trusted().with_can_move(),
            state,
            offset_of!(State, runstack_start) as i32,
        );
        let is_underflow =
            self.builder
                .ins()
                .icmp(IntCC::UnsignedLessThan, new_runstack, runstack_start);
        self.builder
            .ins()
            .trapnz(is_underflow, ir::TrapCode::HEAP_OUT_OF_BOUNDS);
        let runstack_end = self.builder.ins().load(
            types::I64,
            ir::MemFlags::trusted().with_can_move(),
            state,
            offset_of!(State, runstack_end) as i32,
        );
        let is_overflow =
            self.builder
                .ins()
                .icmp(IntCC::UnsignedGreaterThan, new_runstack, runstack_end);
        self.builder
            .ins()
            .trapnz(is_overflow, ir::TrapCode::HEAP_OUT_OF_BOUNDS);
        for (i, arg) in args.iter().enumerate() {
            self.builder.ins().store(
                ir::MemFlags::trusted().with_can_move(),
                *arg,
                runstack,
                (i * 8) as i32,
            );
        }

        runstack
    }

    /// Continue execution at continuation `k` with arguments `args`.
    ///
    /// If `k` is reified this performs return call instead of local jump. Arguments
    /// are pushed onto runstack in that case.
    pub fn continue_to(&mut self, k: LVarRef<'gc>, args: &[ir::Value]) -> ir::Inst {
        if let Some(k) = self.module_builder.reify_info.free_vars.conts.get(&k)
            && !k.reified.get()
        {
            let k = *k;
            // TODO: Support `k` where variadic is used and materialize a list.
            let block = self.block_for_cont(k);
            if k.args.len() > args.len() {
                panic!(
                    "not enough args when jumping to continuation {}@{:p}, expected {}, got {}",
                    k.binding.name,
                    k.binding,
                    k.args.len(),
                    args.len()
                );
            }
            let mut block_args = args[..k.args.len()]
                .into_iter()
                .map(|v| ir::BlockArg::Value(*v))
                .collect::<Vec<_>>();
            if let Some(variadic) = k.variadic() {
                if variadic.is_referenced() {
                    let mut ls = self
                        .builder
                        .ins()
                        .iconst(types::I64, Value::null().bits() as i64);
                    let ctx = self.builder.ins().get_pinned_reg(types::I64);
                    for arg in args[k.args.len()..].iter().rev() {
                        let call = self.builder.ins().call(self.thunks.cons, &[ctx, *arg, ls]);
                        ls = self.builder.inst_results(call)[0];
                    }
                    block_args.push(ir::BlockArg::Value(ls));
                } else {
                    // Do not materialize the list if variadic is not referenced
                    let null = self
                        .builder
                        .ins()
                        .iconst(types::I64, Value::null().bits() as i64);
                    block_args.push(ir::BlockArg::Value(null));
                }
            }

            let bb_args_len = self.builder.block_params(block).len();
            if bb_args_len != block_args.len() {
                panic!(
                    "wrong number of args when jumping to continuation {}@{:p}, expected {}, got {}",
                    k.binding.name,
                    k.binding,
                    bb_args_len,
                    block_args.len()
                );
            }
            return self.builder.ins().jump(block, &block_args);
        }

        let callee = self.get_callee_k(k);
        let rands = self.push_args(args);
        let num_rands = self.builder.ins().iconst(types::I64, args.len() as i64);
        match callee {
            Callee::Indirect { target, closure } => self.builder.ins().jump(
                self.exit_block,
                &[
                    BlockArg::Value(target),
                    BlockArg::Value(closure),
                    BlockArg::Value(rands),
                    BlockArg::Value(num_rands),
                ],
            ),
            Callee::Direct { target, closure } => self
                .builder
                .ins()
                .return_call(target, &[closure, rands, num_rands]),

            Callee::SelfRec(block) => self.builder.ins().jump(
                block,
                &[
                    BlockArg::Value(self.rator),
                    BlockArg::Value(rands),
                    BlockArg::Value(num_rands),
                ],
            ),
        }
    }

    fn load_free_vars(&mut self) {
        let Some(fvs) = (match self.target {
            ContOrFunc::Func(func) => func.free_vars.get(),
            ContOrFunc::Cont(cont) => cont.free_vars.get(),
        }) else {
            return;
        };

        /* load all free-variables of closure.

           We can directly load values without later modifying `free` vector
           as all variables are immutable, mutable variables should've been boxed by optimizing compiler.
        */

        for (i, var) in fvs.iter().copied().enumerate() {
            /*let offset = Vector::OFFSET_OF_DATA as i32 + (i as i32 * 8);
            let fv = self
                .builder
                .ins()
                .load(types::I64, ir::MemFlags::trusted().with_can_move(), free, offset);
            self.variables.insert(var, fv);*/
            self.variables.insert(var, VarDef::Free(i));
        }
    }

    /* helpers */

    pub fn check_argcount(&mut self, proc: &str, h: LVarRef<'gc>, got_: usize, expected_: usize) {
        let got = self.builder.ins().iconst(types::I64, got_ as i64);
        let expected = self.builder.ins().iconst(types::I64, expected_ as i64);

        let on_err = self.builder.create_block();
        let on_succ = self.builder.create_block();

        let cmp = self.builder.ins().icmp(IntCC::Equal, got, expected);

        self.builder.ins().brif(cmp, on_succ, &[], on_err, &[]);
        self.builder.func.layout.set_cold(on_err);
        self.builder.switch_to_block(on_err);
        {
            self.wrong_num_args(proc, h, got_, expected_ as _);
        }
        self.builder.switch_to_block(on_succ);
    }

    pub fn atom_for_cond(&mut self, atom: Atom<'gc>) -> ir::Value {
        match atom {
            Atom::Local(var) => match self.variables[&var] {
                VarDef::Comparison(val) => val,
                _ => {
                    let val = self.var(var);
                    self.builder
                        .ins()
                        .icmp_imm(IntCC::NotEqual, val, Value::VALUE_FALSE)
                }
            },

            _ => {
                let val = self.atom(atom);
                self.builder
                    .ins()
                    .icmp_imm(IntCC::NotEqual, val, Value::VALUE_FALSE)
            }
        }
    }

    pub fn atom(&mut self, atom: Atom<'gc>) -> ir::Value {
        match atom {
            Atom::Local(var) => self.var(var),
            Atom::Constant(obj) => {
                let Some(data_id) = self.module_builder.intern_constant(obj) else {
                    return self.builder.ins().iconst(types::I64, obj.bits() as i64);
                };

                let global_value = self.import_data(data_id);

                let addr = self.builder.ins().global_value(types::I64, global_value);
                self.builder.ins().load(
                    types::I64,
                    ir::MemFlags::trusted().with_can_move(),
                    addr,
                    0,
                )
            }
        }
    }

    pub fn fix(&mut self, funcs: &[FuncRef<'gc>]) {
        for func in funcs.iter() {
            let free = func.free_vars.get();
            let nfree = free.map_or(0, |f| f.len());
            let nfree = self.builder.ins().iconst(types::I64, nfree as i64);
            let is_cont = self.builder.ins().iconst(types::I8, 0);
            let fref = self.module_builder.func_for_func[&func];
            let fref = self
                .module_builder
                .module
                .declare_func_in_func(fref, &mut self.builder.func);
            let addr = self.builder.ins().func_addr(types::I64, fref);
            let meta = self.meta_for_func(*func);
            let ctx = self.builder.ins().get_pinned_reg(types::I64);

            let clos = self
                .builder
                .ins()
                .call(self.thunks.make_closure, &[ctx, addr, nfree, is_cont, meta]);
            let clos = self.builder.inst_results(clos)[0];
            self.debug_local_with_source(func.binding, clos, func.source);
            self.variables.insert(func.binding, VarDef::Value(clos));
        }

        for func in funcs.iter() {
            let clos = self.var(func.binding);
            let free = func.free_vars.get();
            if let Some(free) = free {
                let fv = self.builder.ins().load(
                    types::I64,
                    ir::MemFlags::trusted().with_can_move(),
                    clos,
                    offset_of!(Closure, free) as i32,
                );
                for (i, &var) in free.iter().enumerate() {
                    let var = self.var(var);

                    self.builder.ins().store(
                        ir::MemFlags::trusted().with_can_move(),
                        var,
                        fv,
                        (Vector::OFFSET_OF_DATA as i32 + (i as i32 * 8)) as i32,
                    );
                }
            }
        }
    }

    pub fn letk(&mut self, conts: &[ContRef<'gc>]) {
        for cont in conts.iter().filter(|k| k.reified.get()) {
            let free = cont.free_vars.get();
            let nfree = free.map_or(0, |f| f.len());

            let func_for_k = self.module_builder.func_for_cont[&cont];
            let func_for_k = self
                .module_builder
                .module
                .declare_func_in_func(func_for_k, &mut self.builder.func);

            let nfree = self.builder.ins().iconst(types::I64, nfree as i64);
            let is_cont = self.builder.ins().iconst(types::I8, 1);
            let addr = self.builder.ins().func_addr(types::I64, func_for_k);
            let meta = self.meta_for_cont(*cont);
            let ctx = self.builder.ins().get_pinned_reg(types::I64);
            let clos = self
                .builder
                .ins()
                .call(self.thunks.make_closure, &[ctx, addr, nfree, is_cont, meta]);
            let clos = self.builder.inst_results(clos)[0];
            self.debug_local_with_source(cont.binding, clos, cont.source);
            self.variables.insert(cont.binding, VarDef::Value(clos));
        }

        for cont in conts.iter().filter(|k| k.reified.get()) {
            let clos = self.var(cont.binding);
            let free = cont.free_vars.get();

            if let Some(free) = free {
                let fv = self.builder.ins().load(
                    types::I64,
                    ir::MemFlags::trusted().with_can_move(),
                    clos,
                    offset_of!(Closure, free) as i32,
                );

                for (i, &var) in free.iter().enumerate() {
                    let var = self.var(var);

                    self.builder.ins().store(
                        ir::MemFlags::trusted().with_can_move(),
                        var,
                        fv,
                        (Vector::OFFSET_OF_DATA as i32 + (i as i32 * 8)) as i32,
                    );
                }
            }
        }

        for &cont in conts.iter().filter(|k| !k.reified.get()) {
            let _block = self.block_for_cont(cont);
            self.to_generate.push(cont);
        }
    }

    pub fn finalize(&mut self) {
        while let Some(cont) = self.to_generate.pop() {
            let block = self.block_for_cont(cont);
            self.builder.switch_to_block(block);
            self.set_debug_loc(cont.source());
            self.term(cont.body());
        }

        self.builder.switch_to_block(self.exit_block);

        let code = self.builder.block_params(self.exit_block)[0];
        let rator = self.builder.block_params(self.exit_block)[1];
        let rands = self.builder.block_params(self.exit_block)[2];
        let num_rands = self.builder.block_params(self.exit_block)[3];

        self.builder
            .ins()
            .return_call_indirect(self.sig_call, code, &[rator, rands, num_rands]);

        self.builder.seal_all_blocks();
    }

    pub fn set_debug_loc(&mut self, loc: Value<'gc>) {
        if loc != Value::new(false) {
            let (file_id, line, column) = self.module_builder.debug_context.get_span_loc(loc);

            let source_loc = self.func_debug_cx.add_dbg_loc(file_id, line, column);
            self.builder.set_srcloc(source_loc);
            self.srcloc = Some(source_loc);
        }
    }

    pub fn term(&mut self, term: TermRef<'gc>) {
        let src = term.source();
        self.set_debug_loc(src);
        match &*term {
            Term::Let(var, expr, next) => {
                match expr {
                    Expression::PrimCall(prim, args, handler, _) => {
                        let Some(lowerer) = self.module_builder.prims.map.get(prim).copied() else {
                            panic!("undefined primitive: {prim}")
                        };

                        let val = match lowerer(self, args, *handler) {
                            PrimValue::Value(val) => VarDef::Value(val),
                            PrimValue::Comparison(val) => VarDef::Comparison(val),
                        };
                        self.variables.insert(*var, val);
                    }
                }
                self.term(*next);
            }

            Term::Letk(conts, next) => {
                self.letk(conts);
                self.term(*next);
            }

            Term::Fix(funs, next) => {
                self.fix(funs);
                self.term(*next);
            }

            Term::App(rator, retk, reth, rands, _) => {
                let num_rands = rands.len() + 2;
                let num_rands = self.builder.ins().iconst(types::I64, num_rands as i64);

                let err_handler = self.default_error_handler();
                let callee = self.get_callee(rator, err_handler);

                let retk = self.var(*retk);
                let reth = self.var(*reth);

                let rands = [retk, reth]
                    .into_iter()
                    .chain(rands.iter().map(|a| self.atom(*a)))
                    .collect::<Vec<_>>();

                let rands = self.push_args(&rands);

                /*
                self.builder.ins().jump(
                    self.exit_block,
                    &[
                        BlockArg::Value(code),
                        BlockArg::Value(rator),
                        BlockArg::Value(rands),
                        BlockArg::Value(num_rands),
                    ],
                );*/

                match callee {
                    Callee::Indirect { target, closure } => {
                        if self.module_builder.stacktraces {
                            let ctx = self.builder.ins().get_pinned_reg(types::I64);
                            let src = self.atom(Atom::Constant(src));
                            self.builder.ins().call(
                                self.thunks.debug_trace,
                                &[ctx, closure, rands, num_rands, src],
                            );
                        }
                        self.builder.ins().jump(
                            self.exit_block,
                            &[
                                BlockArg::Value(target),
                                BlockArg::Value(closure),
                                BlockArg::Value(rands),
                                BlockArg::Value(num_rands),
                            ],
                        );
                    }

                    Callee::Direct { target, closure } => {
                        if self.module_builder.stacktraces {
                            let ctx = self.builder.ins().get_pinned_reg(types::I64);
                            let src = self.atom(Atom::Constant(src));
                            self.builder.ins().call(
                                self.thunks.debug_trace,
                                &[ctx, closure, rands, num_rands, src],
                            );
                        }
                        self.builder
                            .ins()
                            .return_call(target, &[closure, rands, num_rands]);
                    }

                    Callee::SelfRec(block) => {
                        if self.module_builder.stacktraces {
                            let ctx = self.builder.ins().get_pinned_reg(types::I64);
                            let src = self.atom(Atom::Constant(src));
                            self.builder.ins().call(
                                self.thunks.debug_trace,
                                &[ctx, self.rator, rands, num_rands, src],
                            );
                        }
                        // just jump back to entrypoint
                        let block_args = [self.rator, rands, num_rands]
                            .into_iter()
                            .map(|v| ir::BlockArg::Value(v))
                            .collect::<Vec<_>>();
                        self.builder.ins().jump(block, &block_args);
                    }
                }
            }

            Term::Continue(k, rands, _) => {
                let rands = rands.iter().map(|a| self.atom(*a)).collect::<Vec<_>>();

                self.continue_to(*k, &rands);
            }

            Term::If {
                test,
                consequent,
                consequent_args,
                alternative,
                alternative_args,
                hints: _,
            } => {
                let truthy = self.atom_for_cond(*test);

                let kcons = self.builder.create_block();
                let kalt = self.builder.create_block();

                self.builder.ins().brif(truthy, kcons, &[], kalt, &[]);
                self.builder.switch_to_block(kalt);
                {
                    let alternative_args = alternative_args.map_or(vec![], |args| {
                        args.iter().map(|a| self.atom(*a)).collect::<Vec<_>>()
                    });
                    self.continue_to(*alternative, &alternative_args);
                }

                self.builder.switch_to_block(kcons);
                {
                    let consequent_args = consequent_args.map_or(vec![], |args| {
                        args.iter().map(|a| self.atom(*a)).collect::<Vec<_>>()
                    });
                    self.continue_to(*consequent, &consequent_args);
                }
            }
        }
    }

    pub fn maybe_known_function(&mut self, rator: Atom<'gc>) -> Option<ir::FuncRef> {
        let Atom::Local(var) = rator else {
            return None;
        };

        let func = *self.module_builder.reify_info.free_vars.funcs.get(&var)?;
        let id = self
            .module_builder
            .func_for_func
            .get(&func)
            .expect("must be defined");

        self.module_builder
            .module
            .declare_func_in_func(*id, &mut self.builder.func)
            .into()
    }

    pub fn check_yield(&mut self, rator: ir::Value, rands: ir::Value, num_rands: ir::Value) {
        let ctx = self.builder.ins().get_pinned_reg(types::I64);

        let thread = ctx;

        let yieldpoint = self.builder.ins().load(
            types::I32,
            ir::MemFlags::trusted().with_can_move(),
            thread,
            Thread::TAKE_YIELDPOINT_OFFSET as i32,
        );
        let on_yieldpoint = self.builder.create_block();
        let on_no_yieldpoint = self.builder.create_block();
        self.builder.func.layout.set_cold(on_yieldpoint);

        let take_yieldpoint = self.builder.ins().icmp_imm(IntCC::NotEqual, yieldpoint, 0);

        self.builder
            .ins()
            .brif(take_yieldpoint, on_yieldpoint, &[], on_no_yieldpoint, &[]);
        self.builder.switch_to_block(on_yieldpoint);
        {
            let ctx = self.builder.ins().get_pinned_reg(types::I64);

            let nest_level = self.builder.ins().load(
                types::I64,
                ir::MemFlags::trusted().with_can_move(),
                ctx,
                (Context::OFFSET_OF_STATE + offset_of!(State, nest_level)) as i32,
            );

            let is_nested = self
                .builder
                .ins()
                .icmp_imm(IntCC::UnsignedGreaterThan, nest_level, 1);
            let on_not_nested = self.builder.create_block();
            self.builder
                .ins()
                .brif(is_nested, on_no_yieldpoint, &[], on_not_nested, &[]);
            self.builder.switch_to_block(on_not_nested);
            self.builder.func.layout.set_cold(on_not_nested);
            let saved_call = self
                .builder
                .ins()
                .call(self.thunks.yieldpoint, &[ctx, rator, rands, num_rands]);
            let code = self
                .builder
                .ins()
                .iconst(types::I64, ReturnCode::Yield as i64);
            let val = self.builder.inst_results(saved_call)[0];
            self.builder.ins().return_(&[code, val]);
        }
        self.builder.switch_to_block(on_no_yieldpoint);
    }
}
