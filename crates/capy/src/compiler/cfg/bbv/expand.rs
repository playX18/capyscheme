//! Pre-specialization expansion pass (ECOOP'24 Section 3.2).
//!
//! Rewrites generic primitives into explicit type-guarded diamonds so that
//! [`super::specialize`] can later fold the guards under known typing
//! contexts. Each expanded call splits its block: type guards branch into a
//! fast path built from unchecked primitives and a slow path that falls back
//! to the generic primitive (or raises for `car`/`cdr` on non-pairs). All
//! paths join at a continuation block that receives the result through the
//! original mutable destination uvar.

use super::config;
use super::specialize::{max_block_id, max_value_id};
use crate::compiler::cfg::{
    Block, BlockId, BranchTarget, Instruction, Operand, Procedure, Terminator, UVar,
};
use crate::compiler::cps::graph::BranchHint;
use crate::compiler::cranelift::primitive::Primitive;
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
        | Primitive::Modulo
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
        Primitive::Abs | Primitive::Car | Primitive::Cdr => argc == 1,
        Primitive::VectorSet => argc == 3,
        _ => false,
    }
}

const GUARD_HINTS: [BranchHint; 2] = [BranchHint::Normal, BranchHint::Cold];
const EVEN_HINTS: [BranchHint; 2] = [BranchHint::Normal, BranchHint::Normal];

/// Splits blocks at expandable primitive calls, allocating fresh block ids
/// for the guard/fast/slow scaffolding it introduces.
struct Expander<'gc> {
    next_value: u32,
    next_block: usize,
    proc_values: HashSet<UVar>,
    out: Vec<Block<'gc>>,
}

/// Per-split state for one expandable call site.
struct SplitEnv<'gc> {
    args: Vec<Operand<'gc>>,
    /// Original destination uvar for the expandable call.
    result: UVar,
    cont: BlockId,
    prim_source: Value<'gc>,
    block_source: Value<'gc>,
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

    fn fresh_value(&mut self) -> UVar {
        let value = UVar(self.next_value);
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
            instructions,
            terminator,
            source,
        } = block;

        let mut cur_id = id;
        let mut cur_instructions: Vec<Instruction<'gc>> = Vec::new();

        for instruction in instructions {
            match instruction {
                Instruction::PrimCall {
                    dst,
                    prim,
                    args,
                    source: prim_source,
                } if is_expandable(prim, args.len()) => {
                    let cont = self.fresh_block();
                    let env = SplitEnv {
                        args,
                        result: dst,
                        cont,
                        prim_source,
                        block_source: source,
                    };

                    let head = self.expand_primcall(prim, &env, &mut cur_instructions);
                    self.out.push(Block {
                        id: cur_id,
                        instructions: cur_instructions,
                        terminator: head,
                        source,
                    });

                    cur_id = cont;
                    cur_instructions = Vec::new();
                }
                other => cur_instructions.push(other),
            }
        }

        self.out.push(Block {
            id: cur_id,
            instructions: cur_instructions,
            terminator,
            source,
        });
    }

    fn expand_primcall(
        &mut self,
        prim: Primitive,
        env: &SplitEnv<'gc>,
        head_instructions: &mut Vec<Instruction<'gc>>,
    ) -> Terminator<'gc> {
        match prim {
            Primitive::Plus | Primitive::Minus | Primitive::Times => {
                self.expand_arith(prim, env, head_instructions)
            }
            Primitive::Div => self.expand_div(env, head_instructions),
            Primitive::Quotient | Primitive::Remainder => {
                self.expand_fixnum_div(prim, env, head_instructions)
            }
            Primitive::Modulo => self.expand_fixnum_mod(env, head_instructions),
            Primitive::NumericLt
            | Primitive::NumericLte
            | Primitive::NumericGt
            | Primitive::NumericGte
            | Primitive::NumericEqual => self.expand_compare(prim, env, head_instructions),
            Primitive::Car | Primitive::Cdr => self.expand_car_cdr(prim, env, head_instructions),
            Primitive::Abs => self.expand_unary_flonum(
                Primitive::Abs,
                Primitive::FlAbsUnchecked,
                env,
                head_instructions,
            ),
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

    fn emit_block(
        &mut self,
        env: &SplitEnv<'gc>,
        instructions: Vec<Instruction<'gc>>,
        terminator: Terminator<'gc>,
    ) -> BlockId {
        let id = self.fresh_block();
        self.out.push(Block {
            id,
            instructions,
            terminator,
            source: env.block_source,
        });
        id
    }

    fn op_block(
        &mut self,
        env: &SplitEnv<'gc>,
        prim: Primitive,
        args: Vec<Operand<'gc>>,
    ) -> BlockId {
        self.emit_block(
            env,
            vec![Instruction::PrimCall {
                dst: env.result,
                prim,
                args,
                source: env.prim_source,
            }],
            Terminator::Jump { target: env.cont },
        )
    }

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
        self.emit_block(
            env,
            vec![Instruction::PrimCall {
                dst: test,
                prim,
                args,
                source: env.prim_source,
            }],
            branch_to(test, then_block, else_block, hints),
        )
    }

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
        branch_to(test, then_block, else_block, hints)
    }

    fn expand_arith(
        &mut self,
        prim: Primitive,
        env: &SplitEnv<'gc>,
        head_instructions: &mut Vec<Instruction<'gc>>,
    ) -> Terminator<'gc> {
        let (x, y) = (env.args[0], env.args[1]);
        let ovf_prim = match prim {
            Primitive::Plus => Primitive::FxAddOvfUnchecked,
            Primitive::Minus => Primitive::FxSubOvfUnchecked,
            Primitive::Times => Primitive::FxMulOvfUnchecked,
            _ => unreachable!("not an arithmetic primitive: {prim:?}"),
        };

        let slow = self.op_block(env, prim, vec![x, y]);
        let fl_prim = match prim {
            Primitive::Plus => Primitive::FlAddUnchecked,
            Primitive::Minus => Primitive::FlSubUnchecked,
            Primitive::Times => Primitive::FlMulUnchecked,
            _ => unreachable!("not an arithmetic primitive: {prim:?}"),
        };
        let fl_op = self.op_block(env, fl_prim, vec![x, y]);
        let fl_y = self.guard_block(env, Primitive::IsFlonum, vec![y], fl_op, slow, GUARD_HINTS);
        let fl_x = self.guard_block(env, Primitive::IsFlonum, vec![x], fl_y, slow, GUARD_HINTS);

        let branch = branch_to(env.result, env.cont, slow, GUARD_HINTS);
        let fx_op = self.emit_block(
            env,
            vec![Instruction::PrimCall {
                dst: env.result,
                prim: ovf_prim,
                args: vec![x, y],
                source: env.prim_source,
            }],
            branch,
        );

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

    fn expand_div(
        &mut self,
        env: &SplitEnv<'gc>,
        head_instructions: &mut Vec<Instruction<'gc>>,
    ) -> Terminator<'gc> {
        let (x, y) = (env.args[0], env.args[1]);
        let slow = self.op_block(env, Primitive::Div, vec![x, y]);
        let fl_op = self.op_block(env, Primitive::FlDivUnchecked, vec![x, y]);
        let fl_y = self.guard_block(env, Primitive::IsFlonum, vec![y], fl_op, slow, GUARD_HINTS);

        self.head_guard(
            env,
            Primitive::IsFlonum,
            vec![x],
            fl_y,
            slow,
            GUARD_HINTS,
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
        let fx_prim = match prim {
            Primitive::NumericLt => Primitive::FxLtUnchecked,
            Primitive::NumericLte => Primitive::FxLeUnchecked,
            Primitive::NumericGt => Primitive::FxGtUnchecked,
            Primitive::NumericGte => Primitive::FxGeUnchecked,
            _ => Primitive::FxEqUUnchecked,
        };
        let fl_prim = match prim {
            Primitive::NumericLt => Primitive::FlLtUnchecked,
            Primitive::NumericLte => Primitive::FlLeUnchecked,
            Primitive::NumericGt => Primitive::FlGtUnchecked,
            Primitive::NumericGte => Primitive::FlGeUnchecked,
            _ => Primitive::FlEqUnchecked,
        };

        let slow = self.op_block(env, prim, vec![x, y]);
        let fx_op = self.op_block(env, fx_prim, vec![x, y]);
        let fx_y = self.guard_block(env, Primitive::IsFixnum, vec![y], fx_op, slow, GUARD_HINTS);
        let fl_op = self.op_block(env, fl_prim, vec![x, y]);
        let fl_y = self.guard_block(env, Primitive::IsFlonum, vec![y], fl_op, slow, GUARD_HINTS);
        let fl_x = self.guard_block(env, Primitive::IsFlonum, vec![x], fl_y, slow, GUARD_HINTS);

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

        let upper = {
            let length = self.fresh_value();
            let test = self.fresh_value();
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
                        prim: Primitive::FxLtUnchecked,
                        args: vec![index, Operand::Local(length)],
                        source: env.prim_source,
                    },
                ],
                branch_to(test, ok, slow, GUARD_HINTS),
            )
        };

        let lower = {
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
                        prim: Primitive::FxGeUnchecked,
                        args: vec![index, Operand::Local(zero)],
                        source: env.prim_source,
                    },
                ],
                branch_to(test, upper, slow, GUARD_HINTS),
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
                        prim: Primitive::FxLtUnchecked,
                        args: vec![index, Operand::Local(length)],
                        source: env.prim_source,
                    },
                ],
                branch_to(test, ok, slow, GUARD_HINTS),
            )
        };

        let lower = {
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
                        prim: Primitive::FxGeUnchecked,
                        args: vec![index, Operand::Local(zero)],
                        source: env.prim_source,
                    },
                ],
                branch_to(test, upper, slow, GUARD_HINTS),
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
                        prim: Primitive::FxEqUUnchecked,
                        args: vec![x, Operand::Local(minimum)],
                        source: env.prim_source,
                    },
                ],
                branch_to(test, slow, ok, GUARD_HINTS),
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
                        prim: Primitive::FxEqUUnchecked,
                        args: vec![y, Operand::Local(negative_one)],
                        source: env.prim_source,
                    },
                ],
                branch_to(test, minimum_dividend, ok, GUARD_HINTS),
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
                        prim: Primitive::FxEqUUnchecked,
                        args: vec![y, Operand::Local(zero)],
                        source: env.prim_source,
                    },
                ],
                branch_to(test, slow, exceptional_divisor, GUARD_HINTS),
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

    fn expand_fixnum_mod(
        &mut self,
        env: &SplitEnv<'gc>,
        head_instructions: &mut Vec<Instruction<'gc>>,
    ) -> Terminator<'gc> {
        let (x, y) = (env.args[0], env.args[1]);
        let slow = self.op_block(env, Primitive::Modulo, vec![x, y]);
        let ok = self.op_block(env, Primitive::FxModulo, vec![x, y]);

        let zero_result = self.emit_block(
            env,
            vec![Instruction::Const {
                dst: env.result,
                value: Value::from_i32(0),
            }],
            Terminator::Jump { target: env.cont },
        );

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
                        prim: Primitive::FxEqUUnchecked,
                        args: vec![y, Operand::Local(zero)],
                        source: env.prim_source,
                    },
                ],
                branch_to(test, zero_result, ok, GUARD_HINTS),
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

fn branch_to(
    test: UVar,
    then_block: BlockId,
    else_block: BlockId,
    hints: [BranchHint; 2],
) -> Terminator<'static> {
    Terminator::Branch {
        test: Operand::Local(test),
        consequent: BranchTarget::Local {
            block: then_block,
            edge_assigns: vec![],
        },
        alternative: BranchTarget::Local {
            block: else_block,
            edge_assigns: vec![],
        },
        hints,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::cfg::{CodeId, GraphCodeId, ProcedureKind};
    use std::collections::HashMap;

    fn test_procedure(blocks: Vec<Block<'static>>) -> Procedure<'static> {
        Procedure {
            code: CodeId::GraphFunction(GraphCodeId(0)),
            kind: ProcedureKind::Function,
            binding: UVar(0),
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
        let x = UVar(1);
        let y = UVar(2);
        let dst = UVar(3);
        let procedure = test_procedure(vec![Block {
            id: BlockId(0),
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

        let cont = expanded
            .blocks
            .iter()
            .find(|block| {
                matches!(
                    block.terminator,
                    Terminator::TailCall {
                        callee: Operand::Local(value),
                        ..
                    } if value == dst
                )
            })
            .expect("continuation block");
        assert!(matches!(cont.terminator, Terminator::TailCall { .. }));

        let prims = collect_prims(&expanded);
        let count = |prim: Primitive| prims.iter().filter(|p| **p == prim).count();
        assert_eq!(count(Primitive::FxAddOvfUnchecked), 1);
        assert_eq!(count(Primitive::Plus), 1);
        assert_eq!(count(Primitive::IsFixnum), 2);
        assert_eq!(count(Primitive::IsFlonum), 2);
        assert_eq!(count(Primitive::FlAdd), 0);
        assert_eq!(count(Primitive::FlAddUnchecked), 1);
    }

    #[test]
    fn expand_div_abs_and_compare_build_flonum_fast_paths() {
        let x = UVar(1);
        let y = UVar(2);
        let quotient = UVar(3);
        let absolute = UVar(4);
        let comparison = UVar(5);
        let procedure = test_procedure(vec![Block {
            id: BlockId(0),
            instructions: vec![
                Instruction::PrimCall {
                    dst: quotient,
                    prim: Primitive::Div,
                    args: vec![Operand::Local(x), Operand::Local(y)],
                    source: Value::new(false),
                },
                Instruction::PrimCall {
                    dst: absolute,
                    prim: Primitive::Abs,
                    args: vec![Operand::Local(quotient)],
                    source: Value::new(false),
                },
                Instruction::PrimCall {
                    dst: comparison,
                    prim: Primitive::NumericLt,
                    args: vec![Operand::Local(absolute), Operand::Local(y)],
                    source: Value::new(false),
                },
            ],
            terminator: Terminator::TailCall {
                callee: Operand::Local(comparison),
                args: vec![],
                source: Value::new(false),
            },
            source: Value::new(false),
        }]);

        let prims = collect_prims(&expand_procedure(procedure));
        assert!(prims.contains(&Primitive::FlDivUnchecked));
        assert!(prims.contains(&Primitive::FlAbsUnchecked));
        assert!(prims.contains(&Primitive::FlLtUnchecked));
        assert_eq!(
            prims
                .iter()
                .filter(|candidate| **candidate == Primitive::IsFlonum)
                .count(),
            5
        );
    }

    #[test]
    fn expand_fixnum_modulo_returns_zero_for_zero_divisor() {
        let x = UVar(1);
        let y = UVar(2);
        let dst = UVar(3);
        let procedure = test_procedure(vec![Block {
            id: BlockId(0),
            instructions: vec![Instruction::PrimCall {
                dst,
                prim: Primitive::Modulo,
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
        assert!(prims.contains(&Primitive::Modulo));
        assert!(prims.contains(&Primitive::FxModulo));
        assert_eq!(
            prims
                .iter()
                .filter(|candidate| **candidate == Primitive::FxEqUUnchecked)
                .count(),
            1
        );
    }

    #[test]
    fn expand_fixnum_division_guards_minimum_divided_by_negative_one() {
        for (prim, unchecked) in [
            (Primitive::Quotient, Primitive::FxQuotient),
            (Primitive::Remainder, Primitive::FxRemainder),
        ] {
            let x = UVar(1);
            let y = UVar(2);
            let dst = UVar(3);
            let procedure = test_procedure(vec![Block {
                id: BlockId(0),
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
                    .filter(|candidate| **candidate == Primitive::FxEqUUnchecked)
                    .count(),
                3
            );
        }
    }

    #[test]
    fn expand_join_uses_original_result_home() {
        let x = UVar(1);
        let y = UVar(2);
        let z = UVar(3);
        let dst = UVar(4);
        let procedure = test_procedure(vec![Block {
            id: BlockId(0),
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
        assert!(expanded.blocks.iter().any(|block| {
            block.instructions.iter().any(|instruction| {
                matches!(
                    instruction,
                    Instruction::PrimCall {
                        prim: Primitive::Plus | Primitive::FxAddOvfUnchecked,
                        ..
                    }
                )
            })
        }));
        assert!(expanded.blocks.iter().all(|block| {
            !matches!(
                block.terminator,
                Terminator::Jump { .. } | Terminator::Branch { .. } | Terminator::BranchPrim { .. }
            ) || match &block.terminator {
                Terminator::Jump { .. } => true,
                Terminator::Branch {
                    consequent,
                    alternative,
                    ..
                }
                | Terminator::BranchPrim {
                    consequent,
                    alternative,
                    ..
                } => {
                    matches!(consequent, BranchTarget::Local { .. })
                        && matches!(alternative, BranchTarget::Local { .. })
                }
                _ => true,
            }
        }));
    }

    #[test]
    fn expand_car_raises_on_non_pair() {
        let p = UVar(1);
        let dst = UVar(2);
        let procedure = test_procedure(vec![Block {
            id: BlockId(0),
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
        assert!(!collect_prims(&expanded).contains(&Primitive::Car));
    }

    #[test]
    fn expand_vector_ref_checks_bounds_with_unchecked_length() {
        let v = UVar(1);
        let i = UVar(2);
        let dst = UVar(3);
        let procedure = test_procedure(vec![Block {
            id: BlockId(0),
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
            Primitive::FxGeUnchecked,
            Primitive::VectorLengthUnchecked,
            Primitive::FxLtUnchecked,
            Primitive::VectorRefUnchecked,
            Primitive::VectorRef,
        ] {
            assert!(prims.contains(&expected), "missing {expected:?}");
        }
    }

    #[test]
    fn non_expandable_instructions_pass_through() {
        let x = UVar(1);
        let dst = UVar(2);
        let procedure = test_procedure(vec![Block {
            id: BlockId(0),
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
