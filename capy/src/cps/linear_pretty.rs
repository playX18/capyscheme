use crate::{
    cps::linear::{
        Block, BlockId, BranchTarget, ClosureKind, CodeId, Instruction, LinearAtom, LinearProgram,
        LowIntPredicate, LowPrim, LowType, Procedure, ProcedureKind, RestPredicate, SwitchKind,
        Terminator, ValueId,
    },
    expander::core::LVarRef,
    runtime::value::{Symbol, TypeCode8, Value},
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
        Instruction::Const { dst, value } => {
            format!("(const {} {})", render_value_id(*dst), render_value(*value))
        }
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
        Instruction::CacheRef { dst, cache_key, .. } => format!(
            "(cache-ref {} {})",
            render_value_id(*dst),
            render_linear_atom(*cache_key)
        ),
        Instruction::CacheSet {
            dst,
            cache_key,
            value,
            ..
        } => format!(
            "(cache-set! {} {} {})",
            render_value_id(*dst),
            render_linear_atom(*cache_key),
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
        Instruction::LowPrimCall { dsts, op, args, .. } => format!(
            "(low-call {} {}{})",
            render_low_call_dsts(dsts),
            render_low_prim(*op),
            render_linear_atoms(args)
        ),
        Instruction::RuntimePrimCall {
            dst, prim, args, ..
        } => format!(
            "(runtime-call {} {}{})",
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

fn render_low_prim(op: LowPrim) -> String {
    match op {
        LowPrim::HasType8(typecode) => format!("has-type8.{}", render_typecode8(typecode)),
        LowPrim::IsFixnum => "is-fixnum".to_string(),
        LowPrim::UntagFixnum => "untag-fixnum".to_string(),
        LowPrim::TagFixnum => "tag-fixnum".to_string(),
        LowPrim::IReduce(ty) => format!("ireduce.{}", render_low_type(ty)),
        LowPrim::SExt(ty) => format!("sext.{}", render_low_type(ty)),
        LowPrim::ZExt(ty) => format!("zext.{}", render_low_type(ty)),
        LowPrim::ICmp(cond, ty) => format!(
            "icmp.{}.{}",
            render_low_int_predicate(cond),
            render_low_type(ty)
        ),
        LowPrim::IAdd(ty) => format!("iadd.{}", render_low_type(ty)),
        LowPrim::ISub(ty) => format!("isub.{}", render_low_type(ty)),
        LowPrim::IMul(ty) => format!("imul.{}", render_low_type(ty)),
        LowPrim::IDiv(ty) => format!("idiv.{}", render_low_type(ty)),
        LowPrim::IRem(ty) => format!("irem.{}", render_low_type(ty)),
        LowPrim::IShl(ty) => format!("ishl.{}", render_low_type(ty)),
        LowPrim::IShr(ty) => format!("ishr.{}", render_low_type(ty)),
        LowPrim::IAnd(ty) => format!("iand.{}", render_low_type(ty)),
        LowPrim::IOr(ty) => format!("ior.{}", render_low_type(ty)),
        LowPrim::IXor(ty) => format!("ixor.{}", render_low_type(ty)),
        LowPrim::IAddImm(ty, imm) => format!("iadd-imm.{}.{}", render_low_type(ty), imm),
        LowPrim::IMulImm(ty, imm) => format!("imul-imm.{}.{}", render_low_type(ty), imm),
        LowPrim::IAddOverflow(ty) => format!("iadd-overflow.{}", render_low_type(ty)),
        LowPrim::ISubOverflow(ty) => format!("isub-overflow.{}", render_low_type(ty)),
        LowPrim::IMulOverflow(ty) => format!("imul-overflow.{}", render_low_type(ty)),
        LowPrim::Load { ty, offset } => format!("load.{}@{}", render_low_type(ty), offset),
        LowPrim::FAdd(ty) => format!("fadd.{}", render_low_type(ty)),
        LowPrim::FSub(ty) => format!("fsub.{}", render_low_type(ty)),
        LowPrim::FMul(ty) => format!("fmul.{}", render_low_type(ty)),
        LowPrim::FDiv(ty) => format!("fdiv.{}", render_low_type(ty)),
    }
}

fn render_low_int_predicate(cond: LowIntPredicate) -> &'static str {
    match cond {
        LowIntPredicate::Equal => "eq",
        LowIntPredicate::NotEqual => "ne",
        LowIntPredicate::SignedLessThan => "slt",
        LowIntPredicate::SignedLessThanOrEqual => "sle",
        LowIntPredicate::SignedGreaterThan => "sgt",
        LowIntPredicate::SignedGreaterThanOrEqual => "sge",
        LowIntPredicate::UnsignedLessThan => "ult",
        LowIntPredicate::UnsignedLessThanOrEqual => "ule",
        LowIntPredicate::UnsignedGreaterThan => "ugt",
        LowIntPredicate::UnsignedGreaterThanOrEqual => "uge",
    }
}

fn render_typecode8(typecode: u8) -> String {
    if typecode == TypeCode8::VECTOR.bits() {
        "vector".to_string()
    } else if typecode == TypeCode8::STRING.bits() {
        "string".to_string()
    } else if typecode == TypeCode8::TUPLE.bits() {
        "tuple".to_string()
    } else if typecode == TypeCode8::BYTEVECTOR.bits() {
        "bytevector".to_string()
    } else if typecode == TypeCode8::PAIR.bits() {
        "pair".to_string()
    } else {
        typecode.to_string()
    }
}

fn render_low_type(ty: LowType) -> &'static str {
    match ty {
        LowType::I8 => "i8",
        LowType::I16 => "i16",
        LowType::I32 => "i32",
        LowType::I64 => "i64",
        LowType::U8 => "u8",
        LowType::U16 => "u16",
        LowType::U32 => "u32",
        LowType::U64 => "u64",
        LowType::F32 => "f32",
        LowType::F64 => "f64",
        LowType::Value => "value",
        LowType::Ptr => "ptr",
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
        Terminator::Raise { kind, args, .. } => {
            format!("(%raise {:?}{})", kind, render_linear_atoms(args))
        }
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
        SwitchKind::SymbolEq { .. } => "eq?",
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

fn render_low_call_dsts(dsts: &[ValueId]) -> String {
    match dsts {
        [] => "()".to_string(),
        [dst] => render_value_id(*dst),
        _ => format!(
            "({})",
            dsts.iter()
                .map(|dst| render_value_id(*dst))
                .collect::<Vec<_>>()
                .join(" ")
        ),
    }
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
