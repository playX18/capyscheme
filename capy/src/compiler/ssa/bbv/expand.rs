//! Pre-specialization expansion pass (ECOOP'24 Section 3.2).
//!
//! Rewrites generic primitives into explicit type-guarded diamonds so that
//! [`super::specialize`] can later fold the guards under known typing
//! contexts. Each expanded call splits its block: type guards branch into a
//! fast path built from unchecked primitives and a slow path that falls back
//! to the generic primitive (or raises for `car`/`cdr` on non-pairs). All
//! paths join at a continuation block that receives the result as a block
//! parameter, preserving SSA form.
//!
//! Every value that is live across the split (per [`super::liveness`]) is
//! threaded through the guard blocks as explicit block parameters. This keeps
//! the "cross-block values are block parameters" invariant the specializer
//! relies on, and gives each guard block a typing context of its own so that
//! versioning can fold the guards.
//!
//! Expanded shapes:
//!
//! - `(+ x y)` (and `-`, `*`): `fixnum? x` -> `fixnum? y` -> `fx+/ovf?` with a
//!   branch on the `#f` overflow result; a `flonum? x` / `flonum? y` path
//!   using `fl+/unchecked`; otherwise the generic slow primitive.
//! - `(< x y)` (and `<=`, `>`, `>=`, `=`): fixnum path via `fxlt`-style
//!   unchecked comparisons, flonum path via `fllt`, otherwise slow.
//! - `(car p)` / `(cdr p)`: `pair? p` -> `car/unchecked` else
//!   `Raise CarNotPair`/`CdrNotPair`.
//! - `(set-car! p v)` / `(set-cdr! p v)`: `pair? p` -> unchecked else slow.
//! - `(vector-ref v i)` / `(vector-set! v i x)`: `vector? v`, `fixnum? i`,
//!   `0 <= i < (vector-length/unchecked v)` -> unchecked else slow.

use super::config;
use super::specialize::{max_block_id, max_value_id};
use crate::compiler::cps::graph::BranchHint;
use crate::compiler::cranelift::primitive::Primitive;
use crate::compiler::ssa::{
    Block, BlockId, BranchTarget, Instruction, Operand, Procedure, Terminator, ValueId,
};
use crate::runtime::value::Value;
use crate::runtime::vm::exceptions::RaiseKind;
use std::collections::HashSet;

/// Expands a procedure ahead of SBBV specialization.
pub(super) fn expand_procedure<'gc>(mut procedure: Procedure<'gc>) -> Procedure<'gc> {
    if !config::enabled() {
        return procedure;
    }
    let mut expander = Expander::new(&procedure);
    let blocks = std::mem::take(&mut procedure.blocks);
    for block in blocks {
        expander.expand_block(block);
    }
    procedure.blocks = expander.out;
    procedure
}

fn is_expandable(prim: Primitive, argc: usize) -> bool {
    match prim {
        Primitive::Plus
        | Primitive::Minus
        | Primitive::Times
        | Primitive::Div
        | Primitive::Quotient
        | Primitive::Remainder
        | Primitive::NumericLt
        | Primitive::NumericLte
        | Primitive::NumericGt
        | Primitive::NumericGte
        | Primitive::NumericEqual
        | Primitive::SetCar
        | Primitive::SetCdr
        | Primitive::VectorRef
        | Primitive::StringRef
        | Primitive::BytevectorU8Ref => argc == 2,
        Primitive::Car | Primitive::Cdr | Primitive::Sqrt | Primitive::Atan => argc == 1,
        Primitive::VectorSet => argc == 3,
        _ => false,
    }
}

const GUARD_HINTS: [BranchHint; 2] = [BranchHint::Normal, BranchHint::Cold];
const EVEN_HINTS: [BranchHint; 2] = [BranchHint::Normal, BranchHint::Normal];

/// Splits blocks at expandable primitive calls, allocating fresh value and
/// block ids for the guard/fast/slow scaffolding it introduces.
struct Expander<'gc> {
    next_value: u32,
    next_block: usize,
    proc_values: HashSet<ValueId>,
    out: Vec<Block<'gc>>,
}

/// Per-split state: which values the guard chain must thread along.
struct SplitEnv<'gc> {
    /// Original call operands.
    args: Vec<Operand<'gc>>,
    /// Params of every intermediate guard/op/slow/raise block (original ids).
    threaded: Vec<ValueId>,
    /// Values (beyond the result) the continuation block rebinds.
    cont_extra: Vec<ValueId>,
    cont: BlockId,
    prim_source: Value<'gc>,
    block_source: Value<'gc>,
}

impl<'gc> SplitEnv<'gc> {
    fn threaded_atoms(&self) -> Vec<Operand<'gc>> {
        self.threaded.iter().map(|id| Operand::Local(*id)).collect()
    }

    fn cont_args(&self, result: ValueId) -> Vec<Operand<'gc>> {
        let mut args = Vec::with_capacity(1 + self.cont_extra.len());
        args.push(Operand::Local(result));
        args.extend(self.cont_extra.iter().map(|id| Operand::Local(*id)));
        args
    }
}

impl<'gc> Expander<'gc> {
    fn new(procedure: &Procedure<'gc>) -> Self {
        let mut proc_values = HashSet::new();
        proc_values.insert(procedure.binding);
        proc_values.extend(procedure.return_cont);
        proc_values.extend(procedure.params.iter().copied());
        proc_values.extend(procedure.variadic);
        proc_values.extend(procedure.free_vars.iter().copied());
        Self {
            next_value: max_value_id(procedure) + 1,
            next_block: max_block_id(procedure) + 1,
            proc_values,
            out: Vec::new(),
        }
    }

    fn fresh_value(&mut self) -> ValueId {
        let value = ValueId(self.next_value);
        self.next_value += 1;
        value
    }

    fn fresh_block(&mut self) -> BlockId {
        let block = BlockId(self.next_block);
        self.next_block += 1;
        block
    }

    fn expand_block(&mut self, block: Block<'gc>) {
        let Block {
            id,
            params,
            variadic,
            instructions,
            terminator,
            source,
        } = block;

        // Backward liveness within the block: values live after instruction k.
        // Cross-block flow is covered because terminator uses include all edge
        // arguments (the same convention as `liveness::compute_live_in`).
        let mut live_after: Vec<HashSet<ValueId>> = vec![HashSet::new(); instructions.len()];
        let mut live: HashSet<ValueId> = HashSet::new();
        for atom in terminator.uses() {
            if let Operand::Local(value) = atom {
                live.insert(value);
            }
        }
        for (k, instruction) in instructions.iter().enumerate().rev() {
            live_after[k] = live.clone();
            for def in instruction.defs() {
                live.remove(&def);
            }
            for atom in instruction.uses() {
                if let Operand::Local(value) = atom {
                    live.insert(value);
                }
            }
        }

        let mut cur_id = id;
        let mut cur_params = params;
        let mut cur_variadic = variadic;
        let mut cur_instructions: Vec<Instruction<'gc>> = Vec::new();

        for (k, instruction) in instructions.into_iter().enumerate() {
            match instruction {
                Instruction::PrimCall {
                    dst,
                    prim,
                    args,
                    source: prim_source,
                } if is_expandable(prim, args.len()) => {
                    let mut cont_extra: Vec<ValueId> = live_after[k]
                        .iter()
                        .copied()
                        .filter(|value| *value != dst && !self.proc_values.contains(value))
                        .collect();
                    cont_extra.sort_by_key(|value| value.0);

                    let mut threaded = cont_extra.clone();
                    for arg in &args {
                        if let Operand::Local(value) = arg
                            && !self.proc_values.contains(value)
                            && !threaded.contains(value)
                        {
                            threaded.push(*value);
                        }
                    }
                    threaded.sort_by_key(|value| value.0);

                    let cont = self.fresh_block();
                    let env = SplitEnv {
                        args,
                        threaded,
                        cont_extra: cont_extra.clone(),
                        cont,
                        prim_source,
                        block_source: source,
                    };

                    let head = self.expand_primcall(prim, &env, &mut cur_instructions);
                    self.out.push(Block {
                        id: cur_id,
                        params: cur_params,
                        variadic: cur_variadic,
                        instructions: cur_instructions,
                        terminator: head,
                        source,
                    });

                    // The continuation receives the result (reusing `dst` so
                    // downstream uses stay intact) plus the live values.
                    cur_id = cont;
                    cur_params = {
                        let mut params = Vec::with_capacity(1 + cont_extra.len());
                        params.push(dst);
                        params.extend(cont_extra);
                        params
                    };
                    cur_variadic = None;
                    cur_instructions = Vec::new();
                }
                other => cur_instructions.push(other),
            }
        }

        self.out.push(Block {
            id: cur_id,
            params: cur_params,
            variadic: cur_variadic,
            instructions: cur_instructions,
            terminator,
            source,
        });
    }

    /// Emits the guard chain for one primitive call. Helper blocks are pushed
    /// to `self.out`; the returned terminator ends the block being split, and
    /// the head guard instruction is appended to `head_instructions`.
    fn expand_primcall(
        &mut self,
        prim: Primitive,
        env: &SplitEnv<'gc>,
        head_instructions: &mut Vec<Instruction<'gc>>,
    ) -> Terminator<'gc> {
        match prim {
            Primitive::Plus | Primitive::Minus | Primitive::Times | Primitive::Div => {
                self.expand_arith(prim, env, head_instructions)
            }
            Primitive::Quotient | Primitive::Remainder => {
                self.expand_fixnum_div(prim, env, head_instructions)
            }
            Primitive::Sqrt => {
                self.expand_unary_flonum(Primitive::Sqrt, Primitive::FlSqrt, env, head_instructions)
            }
            Primitive::Atan => {
                self.expand_unary_flonum(Primitive::Atan, Primitive::FlAtan, env, head_instructions)
            }
            Primitive::NumericLt
            | Primitive::NumericLte
            | Primitive::NumericGt
            | Primitive::NumericGte
            | Primitive::NumericEqual => self.expand_compare(prim, env, head_instructions),
            Primitive::Car | Primitive::Cdr => self.expand_car_cdr(prim, env, head_instructions),
            Primitive::SetCar | Primitive::SetCdr => {
                self.expand_set_pair(prim, env, head_instructions)
            }
            Primitive::VectorRef | Primitive::VectorSet => {
                self.expand_vector_access(prim, env, head_instructions)
            }
            Primitive::StringRef => self.expand_string_access(env, head_instructions),
            Primitive::BytevectorU8Ref => self.expand_bytevector_access(env, head_instructions),
            _ => unreachable!("not an expandable primitive: {prim:?}"),
        }
    }

    // --- shared emission helpers -------------------------------------------

    /// New block with the threaded values as params and the given body.
    fn emit_block(
        &mut self,
        env: &SplitEnv<'gc>,
        instructions: Vec<Instruction<'gc>>,
        terminator: Terminator<'gc>,
    ) -> BlockId {
        let id = self.fresh_block();
        self.out.push(Block {
            id,
            params: env.threaded.clone(),
            variadic: None,
            instructions,
            terminator,
            source: env.block_source,
        });
        id
    }

    /// `r = prim(args); jump cont(r, extra...)`
    fn op_block(
        &mut self,
        env: &SplitEnv<'gc>,
        prim: Primitive,
        args: Vec<Operand<'gc>>,
    ) -> BlockId {
        let result = self.fresh_value();
        let jump = Terminator::Jump {
            target: env.cont,
            args: env.cont_args(result),
        };
        self.emit_block(
            env,
            vec![Instruction::PrimCall {
                dst: result,
                prim,
                args,
                source: env.prim_source,
            }],
            jump,
        )
    }

    /// `t = prim(args); branch t ? then_block(threaded) : else_block(threaded)`
    fn guard_block(
        &mut self,
        env: &SplitEnv<'gc>,
        prim: Primitive,
        args: Vec<Operand<'gc>>,
        then_block: BlockId,
        else_block: BlockId,
        hints: [BranchHint; 2],
    ) -> BlockId {
        let test = self.fresh_value();
        let branch = branch_to(
            test,
            then_block,
            env.threaded_atoms(),
            else_block,
            env.threaded_atoms(),
            hints,
        );
        self.emit_block(
            env,
            vec![Instruction::PrimCall {
                dst: test,
                prim,
                args,
                source: env.prim_source,
            }],
            branch,
        )
    }

    /// Appends the head guard to the split block and returns its terminator.
    #[allow(clippy::too_many_arguments)]
    fn head_guard(
        &mut self,
        env: &SplitEnv<'gc>,
        prim: Primitive,
        args: Vec<Operand<'gc>>,
        then_block: BlockId,
        else_block: BlockId,
        hints: [BranchHint; 2],
        head_instructions: &mut Vec<Instruction<'gc>>,
    ) -> Terminator<'gc> {
        let test = self.fresh_value();
        head_instructions.push(Instruction::PrimCall {
            dst: test,
            prim,
            args,
            source: env.prim_source,
        });
        branch_to(
            test,
            then_block,
            env.threaded_atoms(),
            else_block,
            env.threaded_atoms(),
            hints,
        )
    }

    // --- per-primitive expansions ------------------------------------------

    fn expand_arith(
        &mut self,
        prim: Primitive,
        env: &SplitEnv<'gc>,
        head_instructions: &mut Vec<Instruction<'gc>>,
    ) -> Terminator<'gc> {
        let (x, y) = (env.args[0], env.args[1]);
        let (ovf_prim, fl_prim) = match prim {
            Primitive::Plus => (Primitive::FxAddOvf, Primitive::FlAdd),
            Primitive::Minus => (Primitive::FxSubOvf, Primitive::FlSub),
            Primitive::Times => (Primitive::FxMulOvf, Primitive::FlMul),
            Primitive::Div => (Primitive::FxAddOvf, Primitive::FlDiv), // fixnum / uses slow path
            _ => unreachable!("not an arithmetic primitive: {prim:?}"),
        };

        let slow = self.op_block(env, prim, vec![x, y]);

        // Fixnum path: the overflow-checked op yields #f on overflow, in
        // which case the generic primitive handles bignum promotion.
        // Division keeps fixnums on the slow path; only flonums fast-path here.
        let fx_op = if matches!(prim, Primitive::Div) {
            slow
        } else {
            let sum = self.fresh_value();
            let branch = branch_to(
                sum,
                env.cont,
                env.cont_args(sum),
                slow,
                env.threaded_atoms(),
                GUARD_HINTS,
            );
            self.emit_block(
                env,
                vec![Instruction::PrimCall {
                    dst: sum,
                    prim: ovf_prim,
                    args: vec![x, y],
                    source: env.prim_source,
                }],
                branch,
            )
        };

        let fl_op = self.op_block(env, fl_prim, vec![x, y]);
        let fl_y = self.guard_block(env, Primitive::IsFlonum, vec![y], fl_op, slow, GUARD_HINTS);
        let fl_x = self.guard_block(env, Primitive::IsFlonum, vec![x], fl_y, slow, GUARD_HINTS);
        let fx_y = self.guard_block(env, Primitive::IsFixnum, vec![y], fx_op, slow, GUARD_HINTS);

        self.head_guard(
            env,
            Primitive::IsFixnum,
            vec![x],
            fx_y,
            fl_x,
            EVEN_HINTS,
            head_instructions,
        )
    }

    fn expand_compare(
        &mut self,
        prim: Primitive,
        env: &SplitEnv<'gc>,
        head_instructions: &mut Vec<Instruction<'gc>>,
    ) -> Terminator<'gc> {
        let (x, y) = (env.args[0], env.args[1]);
        let (fx_prim, fl_prim) = match prim {
            Primitive::NumericLt => (Primitive::FxLt, Primitive::FlLt),
            Primitive::NumericLte => (Primitive::FxLe, Primitive::FlLe),
            Primitive::NumericGt => (Primitive::FxGt, Primitive::FlGt),
            Primitive::NumericGte => (Primitive::FxGe, Primitive::FlGe),
            _ => (Primitive::FxEqU, Primitive::FlEq),
        };

        let slow = self.op_block(env, prim, vec![x, y]);
        let fx_op = self.op_block(env, fx_prim, vec![x, y]);
        let fl_op = self.op_block(env, fl_prim, vec![x, y]);
        let fl_y = self.guard_block(env, Primitive::IsFlonum, vec![y], fl_op, slow, GUARD_HINTS);
        let fl_x = self.guard_block(env, Primitive::IsFlonum, vec![x], fl_y, slow, GUARD_HINTS);
        let fx_y = self.guard_block(env, Primitive::IsFixnum, vec![y], fx_op, slow, GUARD_HINTS);

        self.head_guard(
            env,
            Primitive::IsFixnum,
            vec![x],
            fx_y,
            fl_x,
            EVEN_HINTS,
            head_instructions,
        )
    }

    fn expand_car_cdr(
        &mut self,
        prim: Primitive,
        env: &SplitEnv<'gc>,
        head_instructions: &mut Vec<Instruction<'gc>>,
    ) -> Terminator<'gc> {
        let pair = env.args[0];
        let (unchecked, raise_kind) = match prim {
            Primitive::Car => (Primitive::CarUnchecked, RaiseKind::CarNotPair),
            _ => (Primitive::CdrUnchecked, RaiseKind::CdrNotPair),
        };

        let raise = self.emit_block(
            env,
            vec![],
            Terminator::Raise {
                kind: raise_kind,
                args: vec![pair],
                source: env.prim_source,
            },
        );
        let ok = self.op_block(env, unchecked, vec![pair]);

        self.head_guard(
            env,
            Primitive::IsPair,
            vec![pair],
            ok,
            raise,
            GUARD_HINTS,
            head_instructions,
        )
    }

    fn expand_set_pair(
        &mut self,
        prim: Primitive,
        env: &SplitEnv<'gc>,
        head_instructions: &mut Vec<Instruction<'gc>>,
    ) -> Terminator<'gc> {
        let pair = env.args[0];
        let unchecked = match prim {
            Primitive::SetCar => Primitive::SetCarUnchecked,
            _ => Primitive::SetCdrUnchecked,
        };

        let slow = self.op_block(env, prim, env.args.clone());
        let ok = self.op_block(env, unchecked, env.args.clone());

        self.head_guard(
            env,
            Primitive::IsPair,
            vec![pair],
            ok,
            slow,
            GUARD_HINTS,
            head_instructions,
        )
    }

    fn expand_vector_access(
        &mut self,
        prim: Primitive,
        env: &SplitEnv<'gc>,
        head_instructions: &mut Vec<Instruction<'gc>>,
    ) -> Terminator<'gc> {
        let (vector, index) = (env.args[0], env.args[1]);
        let unchecked = match prim {
            Primitive::VectorRef => Primitive::VectorRefUnchecked,
            _ => Primitive::VectorSetUnchecked,
        };

        let slow = self.op_block(env, prim, env.args.clone());
        let ok = self.op_block(env, unchecked, env.args.clone());

        // Upper bound: i < (vector-length/unchecked v).
        let upper = {
            let length = self.fresh_value();
            let test = self.fresh_value();
            let branch = branch_to(
                test,
                ok,
                env.threaded_atoms(),
                slow,
                env.threaded_atoms(),
                GUARD_HINTS,
            );
            self.emit_block(
                env,
                vec![
                    Instruction::PrimCall {
                        dst: length,
                        prim: Primitive::VectorLengthUnchecked,
                        args: vec![vector],
                        source: env.prim_source,
                    },
                    Instruction::PrimCall {
                        dst: test,
                        prim: Primitive::FxLt,
                        args: vec![index, Operand::Local(length)],
                        source: env.prim_source,
                    },
                ],
                branch,
            )
        };

        // Lower bound: 0 <= i, with the zero materialized as a constant so
        // interval narrowing sees both comparison operands.
        let lower = {
            let zero = self.fresh_value();
            let test = self.fresh_value();
            let branch = branch_to(
                test,
                upper,
                env.threaded_atoms(),
                slow,
                env.threaded_atoms(),
                GUARD_HINTS,
            );
            self.emit_block(
                env,
                vec![
                    Instruction::Const {
                        dst: zero,
                        value: Value::from_i32(0),
                    },
                    Instruction::PrimCall {
                        dst: test,
                        prim: Primitive::FxGe,
                        args: vec![index, Operand::Local(zero)],
                        source: env.prim_source,
                    },
                ],
                branch,
            )
        };

        let fx_index = self.guard_block(
            env,
            Primitive::IsFixnum,
            vec![index],
            lower,
            slow,
            GUARD_HINTS,
        );

        self.head_guard(
            env,
            Primitive::IsVector,
            vec![vector],
            fx_index,
            slow,
            GUARD_HINTS,
            head_instructions,
        )
    }

    fn expand_string_access(
        &mut self,
        env: &SplitEnv<'gc>,
        head_instructions: &mut Vec<Instruction<'gc>>,
    ) -> Terminator<'gc> {
        self.expand_indexed_ref(
            env,
            head_instructions,
            Primitive::IsString,
            Primitive::StringLengthUnchecked,
            Primitive::StringRefUnchecked,
            Primitive::StringRef,
        )
    }

    fn expand_bytevector_access(
        &mut self,
        env: &SplitEnv<'gc>,
        head_instructions: &mut Vec<Instruction<'gc>>,
    ) -> Terminator<'gc> {
        self.expand_indexed_ref(
            env,
            head_instructions,
            Primitive::IsBytevector,
            Primitive::BytevectorLengthUnchecked,
            Primitive::BytevectorU8RefUnchecked,
            Primitive::BytevectorU8Ref,
        )
    }

    fn expand_indexed_ref(
        &mut self,
        env: &SplitEnv<'gc>,
        head_instructions: &mut Vec<Instruction<'gc>>,
        container_test: Primitive,
        length_prim: Primitive,
        unchecked_prim: Primitive,
        slow_prim: Primitive,
    ) -> Terminator<'gc> {
        let (container, index) = (env.args[0], env.args[1]);
        let slow = self.op_block(env, slow_prim, env.args.clone());
        let ok = self.op_block(env, unchecked_prim, env.args.clone());

        let upper = {
            let length = self.fresh_value();
            let test = self.fresh_value();
            let branch = branch_to(
                test,
                ok,
                env.threaded_atoms(),
                slow,
                env.threaded_atoms(),
                GUARD_HINTS,
            );
            self.emit_block(
                env,
                vec![
                    Instruction::PrimCall {
                        dst: length,
                        prim: length_prim,
                        args: vec![container],
                        source: env.prim_source,
                    },
                    Instruction::PrimCall {
                        dst: test,
                        prim: Primitive::FxLt,
                        args: vec![index, Operand::Local(length)],
                        source: env.prim_source,
                    },
                ],
                branch,
            )
        };

        let lower = {
            let zero = self.fresh_value();
            let test = self.fresh_value();
            let branch = branch_to(
                test,
                upper,
                env.threaded_atoms(),
                slow,
                env.threaded_atoms(),
                GUARD_HINTS,
            );
            self.emit_block(
                env,
                vec![
                    Instruction::Const {
                        dst: zero,
                        value: Value::from_i32(0),
                    },
                    Instruction::PrimCall {
                        dst: test,
                        prim: Primitive::FxGe,
                        args: vec![index, Operand::Local(zero)],
                        source: env.prim_source,
                    },
                ],
                branch,
            )
        };

        let fx_index = self.guard_block(
            env,
            Primitive::IsFixnum,
            vec![index],
            lower,
            slow,
            GUARD_HINTS,
        );

        self.head_guard(
            env,
            container_test,
            vec![container],
            fx_index,
            slow,
            GUARD_HINTS,
            head_instructions,
        )
    }

    fn expand_unary_flonum(
        &mut self,
        slow_prim: Primitive,
        fl_prim: Primitive,
        env: &SplitEnv<'gc>,
        head_instructions: &mut Vec<Instruction<'gc>>,
    ) -> Terminator<'gc> {
        let x = env.args[0];
        let slow = self.op_block(env, slow_prim, vec![x]);
        let fl = self.op_block(env, fl_prim, vec![x]);

        self.head_guard(
            env,
            Primitive::IsFlonum,
            vec![x],
            fl,
            slow,
            GUARD_HINTS,
            head_instructions,
        )
    }

    fn expand_fixnum_div(
        &mut self,
        prim: Primitive,
        env: &SplitEnv<'gc>,
        head_instructions: &mut Vec<Instruction<'gc>>,
    ) -> Terminator<'gc> {
        let (x, y) = (env.args[0], env.args[1]);
        let unchecked = match prim {
            Primitive::Quotient => Primitive::FxQuotient,
            Primitive::Remainder => Primitive::FxRemainder,
            _ => unreachable!("not a fixnum division primitive: {prim:?}"),
        };

        let slow = self.op_block(env, prim, vec![x, y]);
        let ok = self.op_block(env, unchecked, vec![x, y]);

        let minimum_dividend = {
            let minimum = self.fresh_value();
            let test = self.fresh_value();
            self.emit_block(
                env,
                vec![
                    Instruction::Const {
                        dst: minimum,
                        value: Value::from_i32(i32::MIN),
                    },
                    Instruction::PrimCall {
                        dst: test,
                        prim: Primitive::FxEqU,
                        args: vec![x, Operand::Local(minimum)],
                        source: env.prim_source,
                    },
                ],
                branch_to(
                    test,
                    slow,
                    env.threaded_atoms(),
                    ok,
                    env.threaded_atoms(),
                    GUARD_HINTS,
                ),
            )
        };

        let exceptional_divisor = {
            let negative_one = self.fresh_value();
            let test = self.fresh_value();
            self.emit_block(
                env,
                vec![
                    Instruction::Const {
                        dst: negative_one,
                        value: Value::from_i32(-1),
                    },
                    Instruction::PrimCall {
                        dst: test,
                        prim: Primitive::FxEqU,
                        args: vec![y, Operand::Local(negative_one)],
                        source: env.prim_source,
                    },
                ],
                branch_to(
                    test,
                    minimum_dividend,
                    env.threaded_atoms(),
                    ok,
                    env.threaded_atoms(),
                    GUARD_HINTS,
                ),
            )
        };

        let nonzero = {
            let zero = self.fresh_value();
            let test = self.fresh_value();
            self.emit_block(
                env,
                vec![
                    Instruction::Const {
                        dst: zero,
                        value: Value::from_i32(0),
                    },
                    Instruction::PrimCall {
                        dst: test,
                        prim: Primitive::FxEqU,
                        args: vec![y, Operand::Local(zero)],
                        source: env.prim_source,
                    },
                ],
                branch_to(
                    test,
                    slow,
                    env.threaded_atoms(),
                    exceptional_divisor,
                    env.threaded_atoms(),
                    GUARD_HINTS,
                ),
            )
        };

        let fx_y = self.guard_block(
            env,
            Primitive::IsFixnum,
            vec![y],
            nonzero,
            slow,
            GUARD_HINTS,
        );

        self.head_guard(
            env,
            Primitive::IsFixnum,
            vec![x],
            fx_y,
            slow,
            GUARD_HINTS,
            head_instructions,
        )
    }
}

fn branch_to<'gc>(
    test: ValueId,
    then_block: BlockId,
    then_args: Vec<Operand<'gc>>,
    else_block: BlockId,
    else_args: Vec<Operand<'gc>>,
    hints: [BranchHint; 2],
) -> Terminator<'gc> {
    Terminator::Branch {
        test: Operand::Local(test),
        consequent: BranchTarget::Local {
            block: then_block,
            args: then_args,
        },
        alternative: BranchTarget::Local {
            block: else_block,
            args: else_args,
        },
        hints,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::ssa::{CodeId, GraphCodeId, ProcedureKind};
    use std::collections::HashMap;

    fn test_procedure(blocks: Vec<Block<'static>>) -> Procedure<'static> {
        Procedure {
            code: CodeId::GraphFunction(GraphCodeId(0)),
            kind: ProcedureKind::Function,
            binding: ValueId(0),
            name: Value::new(false),
            source: Value::new(false),
            meta: Value::new(false),
            return_cont: None,
            params: vec![],
            variadic: None,
            free_vars: vec![],
            sources: HashMap::new(),
            entry: BlockId(0),
            blocks,
        }
    }

    fn collect_prims(procedure: &Procedure<'static>) -> Vec<Primitive> {
        procedure
            .blocks
            .iter()
            .flat_map(|block| &block.instructions)
            .filter_map(|instruction| match instruction {
                Instruction::PrimCall { prim, .. } => Some(*prim),
                _ => None,
            })
            .collect()
    }

    #[test]
    fn expand_plus_builds_guarded_diamond() {
        let x = ValueId(1);
        let y = ValueId(2);
        let dst = ValueId(3);
        let procedure = test_procedure(vec![Block {
            id: BlockId(0),
            params: vec![x, y],
            variadic: None,
            instructions: vec![Instruction::PrimCall {
                dst,
                prim: Primitive::Plus,
                args: vec![Operand::Local(x), Operand::Local(y)],
                source: Value::new(false),
            }],
            terminator: Terminator::TailCall {
                callee: Operand::Local(dst),
                args: vec![],
                source: Value::new(false),
            },
            source: Value::new(false),
        }]);

        let expanded = expand_procedure(procedure);
        // head + slow + fx_op + fl_op + fl_y + fl_x + fx_y + cont
        assert_eq!(expanded.blocks.len(), 8);

        let head = expanded
            .blocks
            .iter()
            .find(|block| block.id == BlockId(0))
            .expect("head block");
        assert!(matches!(
            head.instructions.as_slice(),
            [Instruction::PrimCall {
                prim: Primitive::IsFixnum,
                ..
            }]
        ));
        assert!(matches!(head.terminator, Terminator::Branch { .. }));

        // The continuation rebinds the original dst as its first parameter
        // and keeps the original terminator.
        let cont = expanded
            .blocks
            .iter()
            .find(|block| block.params.first() == Some(&dst))
            .expect("continuation block");
        assert!(matches!(cont.terminator, Terminator::TailCall { .. }));

        let prims = collect_prims(&expanded);
        let count = |prim: Primitive| prims.iter().filter(|p| **p == prim).count();
        assert_eq!(count(Primitive::FxAddOvf), 1);
        assert_eq!(count(Primitive::FlAdd), 1);
        assert_eq!(count(Primitive::Plus), 1);
        assert_eq!(count(Primitive::IsFixnum), 2);
        assert_eq!(count(Primitive::IsFlonum), 2);
    }

    #[test]
    fn expand_fixnum_division_guards_minimum_divided_by_negative_one() {
        for (prim, unchecked) in [
            (Primitive::Quotient, Primitive::FxQuotient),
            (Primitive::Remainder, Primitive::FxRemainder),
        ] {
            let x = ValueId(1);
            let y = ValueId(2);
            let dst = ValueId(3);
            let procedure = test_procedure(vec![Block {
                id: BlockId(0),
                params: vec![x, y],
                variadic: None,
                instructions: vec![Instruction::PrimCall {
                    dst,
                    prim,
                    args: vec![Operand::Local(x), Operand::Local(y)],
                    source: Value::new(false),
                }],
                terminator: Terminator::TailCall {
                    callee: Operand::Local(dst),
                    args: vec![],
                    source: Value::new(false),
                },
                source: Value::new(false),
            }]);

            let prims = collect_prims(&expand_procedure(procedure));
            assert!(prims.contains(&prim));
            assert!(prims.contains(&unchecked));
            assert_eq!(
                prims
                    .iter()
                    .filter(|candidate| **candidate == Primitive::FxEqU)
                    .count(),
                3
            );
        }
    }

    #[test]
    fn expand_threads_live_values_through_guards() {
        let x = ValueId(1);
        let y = ValueId(2);
        let z = ValueId(3);
        let dst = ValueId(4);
        // z is live across the split (used by the terminator).
        let procedure = test_procedure(vec![Block {
            id: BlockId(0),
            params: vec![x, y, z],
            variadic: None,
            instructions: vec![Instruction::PrimCall {
                dst,
                prim: Primitive::Plus,
                args: vec![Operand::Local(x), Operand::Local(y)],
                source: Value::new(false),
            }],
            terminator: Terminator::TailCall {
                callee: Operand::Local(z),
                args: vec![Operand::Local(dst)],
                source: Value::new(false),
            },
            source: Value::new(false),
        }]);

        let expanded = expand_procedure(procedure);
        let cont = expanded
            .blocks
            .iter()
            .find(|block| block.params.first() == Some(&dst))
            .expect("continuation block");
        assert!(cont.params.contains(&z));

        // Guard blocks thread x, y (operands) and z (live-across).
        for block in &expanded.blocks {
            if block.id == BlockId(0) || block.params.first() == Some(&dst) {
                continue;
            }
            assert_eq!(block.params, vec![x, y, z], "block {:?}", block.id);
        }
    }

    #[test]
    fn expand_car_raises_on_non_pair() {
        let p = ValueId(1);
        let dst = ValueId(2);
        let procedure = test_procedure(vec![Block {
            id: BlockId(0),
            params: vec![p],
            variadic: None,
            instructions: vec![Instruction::PrimCall {
                dst,
                prim: Primitive::Car,
                args: vec![Operand::Local(p)],
                source: Value::new(false),
            }],
            terminator: Terminator::TailCall {
                callee: Operand::Local(dst),
                args: vec![],
                source: Value::new(false),
            },
            source: Value::new(false),
        }]);

        let expanded = expand_procedure(procedure);
        assert!(expanded.blocks.iter().any(|block| matches!(
            block.terminator,
            Terminator::Raise {
                kind: RaiseKind::CarNotPair,
                ..
            }
        )));
        assert!(collect_prims(&expanded).contains(&Primitive::CarUnchecked));
        // No slow-path Car remains: non-pairs raise.
        assert!(!collect_prims(&expanded).contains(&Primitive::Car));
    }

    #[test]
    fn expand_vector_ref_checks_bounds_with_unchecked_length() {
        let v = ValueId(1);
        let i = ValueId(2);
        let dst = ValueId(3);
        let procedure = test_procedure(vec![Block {
            id: BlockId(0),
            params: vec![v, i],
            variadic: None,
            instructions: vec![Instruction::PrimCall {
                dst,
                prim: Primitive::VectorRef,
                args: vec![Operand::Local(v), Operand::Local(i)],
                source: Value::new(false),
            }],
            terminator: Terminator::TailCall {
                callee: Operand::Local(dst),
                args: vec![],
                source: Value::new(false),
            },
            source: Value::new(false),
        }]);

        let expanded = expand_procedure(procedure);
        let prims = collect_prims(&expanded);
        for expected in [
            Primitive::IsVector,
            Primitive::IsFixnum,
            Primitive::FxGe,
            Primitive::VectorLengthUnchecked,
            Primitive::FxLt,
            Primitive::VectorRefUnchecked,
            Primitive::VectorRef,
        ] {
            assert!(prims.contains(&expected), "missing {expected:?}");
        }
    }

    #[test]
    fn non_expandable_instructions_pass_through() {
        let x = ValueId(1);
        let dst = ValueId(2);
        let procedure = test_procedure(vec![Block {
            id: BlockId(0),
            params: vec![x],
            variadic: None,
            instructions: vec![Instruction::PrimCall {
                dst,
                prim: Primitive::Cons,
                args: vec![Operand::Local(x), Operand::Local(x)],
                source: Value::new(false),
            }],
            terminator: Terminator::TailCall {
                callee: Operand::Local(dst),
                args: vec![],
                source: Value::new(false),
            },
            source: Value::new(false),
        }]);

        let expanded = expand_procedure(procedure);
        assert_eq!(expanded.blocks.len(), 1);
        assert_eq!(expanded.blocks[0].instructions.len(), 1);
    }
}
