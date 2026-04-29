use crate::{
    cps::linear::{
        Block, BlockId, BranchTarget, ClosureKind, CodeId, Instruction, LinearAtom, LinearProgram,
        Procedure, ProcedureKind, RestPredicate, SwitchKind, Terminator, ValueId,
    },
    expander::core::LVarRef,
    runtime::value::{Symbol, Value},
};
use std::fmt::Write;

pub fn render_program<'gc>(program: &LinearProgram<'gc>) -> String {
    let mut out = String::new();
    writeln!(out, "(linear-program").unwrap();
    writeln!(
        out,
        "  (entry {})",
        render_code_id(&CodeId::Function(program.entry))
    )
    .unwrap();
    for procedure in &program.procedures {
        render_procedure(&mut out, procedure, 2);
    }
    writeln!(out, ")").unwrap();
    out
}

fn render_procedure<'gc>(out: &mut String, procedure: &Procedure<'gc>, indent: usize) {
    let pad = " ".repeat(indent);
    writeln!(
        out,
        "{pad}(procedure {} {}",
        render_procedure_kind(procedure.kind),
        render_code_id(&procedure.code)
    )
    .unwrap();
    writeln!(
        out,
        "{pad}  (binding {})",
        render_value_id(procedure.binding)
    )
    .unwrap();
    writeln!(out, "{pad}  (name {})", render_value(procedure.name)).unwrap();
    writeln!(out, "{pad}  (entry block{})", procedure.entry.0).unwrap();
    writeln!(
        out,
        "{pad}  (return-cont {})",
        render_optional_value_id(procedure.return_cont)
    )
    .unwrap();
    writeln!(
        out,
        "{pad}  (params{})",
        render_value_ids(&procedure.params)
    )
    .unwrap();
    writeln!(
        out,
        "{pad}  (variadic {})",
        render_optional_value_id(procedure.variadic)
    )
    .unwrap();
    writeln!(
        out,
        "{pad}  (free-vars{})",
        render_value_ids(&procedure.free_vars)
    )
    .unwrap();
    for block in &procedure.blocks {
        render_block(out, block, indent + 2);
    }
    writeln!(out, "{pad})").unwrap();
}

fn render_block<'gc>(out: &mut String, block: &Block<'gc>, indent: usize) {
    let pad = " ".repeat(indent);
    writeln!(
        out,
        "{pad}(block {} (params{}) (variadic {})",
        render_block_id(block.id),
        render_value_ids(&block.params),
        render_optional_value_id(block.variadic)
    )
    .unwrap();
    for instruction in &block.instructions {
        writeln!(out, "{pad}  {}", render_instruction(instruction)).unwrap();
    }
    writeln!(out, "{pad}  {}", render_terminator(&block.terminator)).unwrap();
    writeln!(out, "{pad})").unwrap();
}

fn render_instruction<'gc>(instruction: &Instruction<'gc>) -> String {
    match instruction {
        Instruction::MakeClosure {
            dst,
            code,
            kind,
            free_count,
        } => format!(
            "(make-closure {} {} {} {})",
            render_value_id(*dst),
            render_code_id(code),
            render_closure_kind(*kind),
            free_count
        ),
        Instruction::ClosureRef {
            dst,
            closure,
            index,
        } => format!(
            "(closure-ref {} {} {})",
            render_value_id(*dst),
            render_linear_atom(*closure),
            index
        ),
        Instruction::ClosureSet {
            closure,
            index,
            value,
        } => format!(
            "(closure-set {} {} {})",
            render_linear_atom(*closure),
            index,
            render_linear_atom(*value)
        ),
        Instruction::PrimCall {
            dst, prim, args, ..
        } => format!(
            "(prim-call {} {}{})",
            render_value_id(*dst),
            prim,
            render_linear_atoms(args)
        ),
        Instruction::RestToList { dst, rest, .. } => format!(
            "(rest->list {} {})",
            render_value_id(*dst),
            render_value_id(*rest)
        ),
        Instruction::RestRef {
            dst, rest, index, ..
        } => format!(
            "(rest-ref {} {} {})",
            render_value_id(*dst),
            render_value_id(*rest),
            index
        ),
        Instruction::RestLength {
            dst, rest, skip, ..
        } => format!(
            "(rest-length {} {} {})",
            render_value_id(*dst),
            render_value_id(*rest),
            skip
        ),
        Instruction::RestPredicate {
            dst,
            rest,
            predicate,
            skip,
            ..
        } => format!(
            "(rest-{} {} {} {})",
            render_rest_predicate(*predicate),
            render_value_id(*dst),
            render_value_id(*rest),
            skip
        ),
    }
}

fn render_rest_predicate(predicate: RestPredicate) -> &'static str {
    match predicate {
        RestPredicate::Null => "null?",
        RestPredicate::Pair => "pair?",
        RestPredicate::List => "list?",
    }
}

fn render_terminator<'gc>(terminator: &Terminator<'gc>) -> String {
    match terminator {
        Terminator::Call {
            callee, retk, args, ..
        } => format!(
            "(call {} {}{})",
            render_linear_atom(*callee),
            render_linear_atom(*retk),
            render_linear_atoms(args)
        ),
        Terminator::TailCall { callee, args, .. } => format!(
            "(tail-call {}{})",
            render_linear_atom(*callee),
            render_linear_atoms(args)
        ),
        Terminator::Jump { target, args } => {
            format!(
                "(jump {}{})",
                render_block_id(*target),
                render_linear_atoms(args)
            )
        }
        Terminator::Branch {
            test,
            consequent,
            alternative,
            hints,
        } => format!(
            "(branch {} {} {} (hints {:?} {:?}))",
            render_linear_atom(*test),
            render_branch_target(consequent),
            render_branch_target(alternative),
            hints[0],
            hints[1]
        ),
        Terminator::Switch {
            kind,
            scrutinee,
            cases,
            default,
        } => {
            let rendered_cases = cases
                .iter()
                .map(|case| {
                    format!(
                        " ({} {})",
                        render_switch_case_value(case.value),
                        render_branch_target(&case.target)
                    )
                })
                .collect::<String>();
            format!(
                "(switch {} {}{} (else {}))",
                render_switch_kind(*kind),
                render_linear_atom(*scrutinee),
                rendered_cases,
                render_branch_target(default)
            )
        }
    }
}

fn render_switch_kind(kind: SwitchKind) -> &'static str {
    match kind {
        SwitchKind::Eq => "eq?",
        SwitchKind::Fixnum => "fx=?",
        SwitchKind::Numeric => "=",
        SwitchKind::Char => "char=?",
        SwitchKind::CharEq => "eq?",
        SwitchKind::SymbolEq => "eq?",
    }
}

fn render_switch_case_value<'gc>(value: crate::cps::linear::SwitchCaseValue<'gc>) -> String {
    match value {
        crate::cps::linear::SwitchCaseValue::Integer(value) => value.to_string(),
        crate::cps::linear::SwitchCaseValue::Symbol { value, .. } => render_value(value),
    }
}

fn render_branch_target<'gc>(target: &BranchTarget<'gc>) -> String {
    match target {
        BranchTarget::Local { block, args } => {
            format!(
                "(local {}{})",
                render_block_id(*block),
                render_linear_atoms(args)
            )
        }
        BranchTarget::Reified { continuation, args } => format!(
            "(reified {}{})",
            render_linear_atom(*continuation),
            render_linear_atoms(args)
        ),
    }
}

fn render_linear_atom<'gc>(atom: LinearAtom<'gc>) -> String {
    match atom {
        LinearAtom::Constant(value) => render_value(value),
        LinearAtom::Local(var) => render_value_id(var),
    }
}

fn render_linear_atoms<'gc>(atoms: &[LinearAtom<'gc>]) -> String {
    atoms
        .iter()
        .map(|atom| format!(" {}", render_linear_atom(*atom)))
        .collect()
}

fn render_value_id(id: ValueId) -> String {
    format!("%v{}", id.0)
}

fn render_lvar<'gc>(var: LVarRef<'gc>) -> String {
    format!("%{}", render_value(var.name))
}

fn render_value_ids(vars: &[ValueId]) -> String {
    vars.iter()
        .map(|var| format!(" {}", render_value_id(*var)))
        .collect()
}

fn render_optional_value_id(var: Option<ValueId>) -> String {
    var.map(render_value_id).unwrap_or_else(|| "#f".to_string())
}

fn render_code_id<'gc>(code: &CodeId<'gc>) -> String {
    match code {
        CodeId::Function(func) => format!("(function {})", render_lvar(func.binding)),
        CodeId::Continuation(cont) => format!("(continuation {})", render_lvar(cont.binding)),
    }
}

fn render_block_id(id: BlockId) -> String {
    format!("block{}", id.0)
}

fn render_procedure_kind(kind: ProcedureKind) -> &'static str {
    match kind {
        ProcedureKind::Function => "function",
        ProcedureKind::Continuation => "continuation",
    }
}

fn render_closure_kind(kind: ClosureKind) -> &'static str {
    match kind {
        ClosureKind::Function => "function",
        ClosureKind::Continuation => "continuation",
    }
}

fn render_value<'gc>(value: Value<'gc>) -> String {
    if value == Value::new(false) {
        "#f".to_string()
    } else if value == Value::new(true) {
        "#t".to_string()
    } else if value.is::<Symbol>() {
        value.downcast::<Symbol>().to_string()
    } else if let Some(number) = value.number() {
        number.to_string()
    } else {
        format!("{value:?}")
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        cps::{
            linear::{
                Block, BlockId, ClosureKind, CodeId, Instruction, LinearAtom, LinearProgram,
                Procedure, ProcedureKind, Terminator, ValueId,
            },
            term::{Func, Term},
        },
        expander::core::{LVarRef, fresh_lvar},
        rsgc::{Gc, alloc::Array, cell::Lock},
        runtime::{
            Context, Scheme,
            value::{Symbol, Value},
        },
    };

    static TEST_LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());

    fn with_ctx(f: impl for<'gc> FnOnce(Context<'gc>)) {
        let _guard = TEST_LOCK
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        let scm = Scheme::new_uninit();
        scm.enter(f);
    }

    fn lvar<'gc>(ctx: Context<'gc>, name: &str) -> LVarRef<'gc> {
        fresh_lvar(ctx, Symbol::from_str(ctx, name).into())
    }

    #[test]
    fn pretty_mentions_closure_ops_and_blocks() {
        with_ctx(|ctx| {
            let binding = lvar(ctx, "entry");
            let retk = lvar(ctx, "retk");
            let free = lvar(ctx, "free");
            let closure = ValueId(1);
            let body = Gc::new(
                *ctx,
                Term::Continue(retk, Array::from_slice(*ctx, &[]), Value::new(false)),
            );
            let entry = Gc::new(
                *ctx,
                Func {
                    name: Symbol::from_str(ctx, "entry").into(),
                    source: Value::new(false),
                    binding,
                    return_cont: retk,
                    args: Array::from_slice(*ctx, &[]),
                    variadic: None,
                    body: Lock::new(body),
                    free_vars: Lock::new(Some(Array::from_slice(*ctx, &[]))),
                    meta: Value::new(false),
                },
            );
            let program = LinearProgram {
                entry,
                procedures: vec![Procedure {
                    code: CodeId::Function(entry),
                    kind: ProcedureKind::Function,
                    binding: ValueId(0),
                    name: Symbol::from_str(ctx, "entry").into(),
                    source: Value::new(false),
                    meta: Value::new(false),
                    return_cont: Some(ValueId(2)),
                    params: vec![],
                    variadic: None,
                    free_vars: vec![ValueId(3)],
                    sources: [
                        (ValueId(0), binding),
                        (ValueId(2), retk),
                        (ValueId(3), free),
                    ]
                    .into_iter()
                    .collect(),
                    entry: BlockId(0),
                    blocks: vec![Block {
                        id: BlockId(0),
                        params: vec![],
                        variadic: None,
                        instructions: vec![
                            Instruction::MakeClosure {
                                dst: closure,
                                code: CodeId::Function(entry),
                                kind: ClosureKind::Function,
                                free_count: 1,
                            },
                            Instruction::ClosureSet {
                                closure: LinearAtom::Local(closure),
                                index: 0,
                                value: LinearAtom::Local(ValueId(3)),
                            },
                        ],
                        terminator: Terminator::Jump {
                            target: BlockId(0),
                            args: vec![],
                        },
                        source: Value::new(false),
                    }],
                }],
            };

            let rendered = super::render_program(&program);

            assert!(rendered.contains("linear-program"));
            assert!(rendered.contains("procedure"));
            assert!(rendered.contains("block"));
            assert!(rendered.contains("make-closure"));
            assert!(rendered.contains("closure-set"));
        });
    }
}
