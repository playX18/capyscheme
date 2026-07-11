use crate::{
    compiler::ssa::{
        Block, BlockId, BranchTarget, ClosureKind, CodeId, Instruction, Operand, Procedure,
        ProcedureKind, Program, RestPredicate, SwitchKind, Terminator, ValueId,
        bbv::BlockAnnotation,
    },
    expander::core::LVarRef,
    runtime::value::{Symbol, Value},
};
use std::{collections::HashMap, fmt::Write};

pub fn render_program<'gc>(program: &Program<'gc>) -> String {
    render_program_with_annotations(program, None)
}

pub fn render_program_with_annotations<'gc>(
    program: &Program<'gc>,
    annotations: Option<&HashMap<BlockId, BlockAnnotation>>,
) -> String {
    let mut out = String::new();
    for procedure in &program.procedures {
        render_procedure(&mut out, procedure, annotations);
        writeln!(out).unwrap();
    }
    out
}

fn render_procedure<'gc>(
    out: &mut String,
    procedure: &Procedure<'gc>,
    annotations: Option<&HashMap<BlockId, BlockAnnotation>>,
) {
    let name = render_procedure_name(procedure);
    let params = render_value_ids(&procedure.params);
    let retk = render_optional_value_id(procedure.return_cont);
    writeln!(
        out,
        "procedure {} {} {} ({}) retk {}:",
        render_procedure_kind(procedure.kind),
        render_code_id(&procedure.code),
        name,
        params,
        retk
    )
    .unwrap();

    let mut predecessors: HashMap<BlockId, Vec<BlockId>> = HashMap::new();
    for block in &procedure.blocks {
        for successor in block.terminator.successors() {
            predecessors.entry(successor).or_default().push(block.id);
        }
    }

    let mut blocks = procedure.blocks.clone();
    blocks.sort_by_key(|block| block.id.0);
    for block in &blocks {
        render_block(out, block, &predecessors, procedure, annotations);
        writeln!(out).unwrap();
    }
}

fn render_block<'gc>(
    out: &mut String,
    block: &Block<'gc>,
    predecessors: &HashMap<BlockId, Vec<BlockId>>,
    procedure: &Procedure<'gc>,
    annotations: Option<&HashMap<BlockId, BlockAnnotation>>,
) {
    let mut header = format!("BB{}: ({})", block.id.0, render_block_params(block));
    if let Some(annotations) = annotations
        && let Some(annotation) = annotations.get(&block.id)
    {
        write!(header, " ; orig=BB{} {}", annotation.orig.0, annotation.ctx).unwrap();
    }
    writeln!(out, "{header}").unwrap();

    if let Some(preds) = predecessors.get(&block.id) {
        let mut pred_ids = preds.clone();
        pred_ids.sort_by_key(|block| block.0);
        pred_ids.dedup();
        let rendered = pred_ids
            .iter()
            .map(|block| format!("BB{}", block.0))
            .collect::<Vec<_>>()
            .join(", ");
        writeln!(out, "  Predecessors: {rendered}").unwrap();
    }

    for instruction in &block.instructions {
        writeln!(out, "  {}", render_instruction(instruction)).unwrap();
    }

    writeln!(out, "  {}", render_successors(&block.terminator, procedure)).unwrap();
}

fn render_block_params<'gc>(block: &Block<'gc>) -> String {
    let mut params = render_value_ids(&block.params);
    if let Some(variadic) = block.variadic {
        if params.is_empty() {
            params = format!("...{}", render_value_id(variadic));
        } else {
            params = format!("{params}, ...{}", render_value_id(variadic));
        }
    }
    params
}

fn render_instruction<'gc>(instruction: &Instruction<'gc>) -> String {
    match instruction {
        Instruction::Const { dst, value } => {
            format!("{} = Const {}", render_value_id(*dst), render_value(*value))
        }
        Instruction::MakeClosure {
            dst,
            code,
            kind,
            free_count,
        } => format!(
            "{} = MakeClosure {} {} {}",
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
            "{} = ClosureRef {}[{}]",
            render_value_id(*dst),
            render_operand(*closure),
            index
        ),
        Instruction::ClosureSet {
            closure,
            index,
            value,
        } => format!(
            "ClosureSet {}[{}] = {}",
            render_operand(*closure),
            index,
            render_operand(*value)
        ),
        Instruction::CacheRef { dst, cache_key, .. } => format!(
            "{} = CacheRef {}",
            render_value_id(*dst),
            render_operand(*cache_key)
        ),
        Instruction::CacheSet {
            dst,
            cache_key,
            value,
            ..
        } => format!(
            "{} = CacheSet {} {}",
            render_value_id(*dst),
            render_operand(*cache_key),
            render_operand(*value)
        ),
        Instruction::PrimCall {
            dst, prim, args, ..
        } => format!(
            "{} = {}{}",
            render_value_id(*dst),
            prim,
            render_call_args(args)
        ),
        Instruction::RestToList { dst, rest, .. } => format!(
            "{} = RestToList {}",
            render_value_id(*dst),
            render_value_id(*rest)
        ),
        Instruction::RestRef {
            dst, rest, index, ..
        } => format!(
            "{} = RestRef {} {}",
            render_value_id(*dst),
            render_value_id(*rest),
            index
        ),
        Instruction::RestLength {
            dst, rest, skip, ..
        } => format!(
            "{} = RestLength {} {}",
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
            "{} = Rest{} {} {}",
            render_value_id(*dst),
            render_rest_predicate(*predicate),
            render_value_id(*rest),
            skip
        ),
    }
}

fn render_successors<'gc>(terminator: &Terminator<'gc>, _procedure: &Procedure<'gc>) -> String {
    match terminator {
        Terminator::Call {
            callee, retk, args, ..
        } => format!(
            "Successors: Call {} {}({})",
            render_operand(*callee),
            render_operand(*retk),
            render_call_args_list(args)
        ),
        Terminator::TailCall { callee, args, .. } => format!(
            "Successors: TailCall {}({})",
            render_operand(*callee),
            render_call_args_list(args)
        ),
        Terminator::Raise { kind, args, .. } => format!(
            "Successors: Raise {:?}({})",
            kind,
            render_call_args_list(args)
        ),
        Terminator::Jump { target, args } => format!(
            "Successors: BB{}({})",
            target.0,
            render_call_args_list(args)
        ),
        Terminator::Branch {
            test,
            consequent,
            alternative,
            hints,
        } => format!(
            "Successors: If {} Then: {}, Else: {} [{:?}, {:?}]",
            render_operand(*test),
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
            let mut rendered_cases = Vec::new();
            for case in cases {
                rendered_cases.push(format!(
                    "{}: {}",
                    render_switch_case_value(case.value),
                    render_branch_target(&case.target)
                ));
            }
            format!(
                "Successors: Switch {} {} [{}] Default: {}",
                render_switch_kind(*kind),
                render_operand(*scrutinee),
                rendered_cases.join(", "),
                render_branch_target(default)
            )
        }
    }
}

fn render_branch_target<'gc>(target: &BranchTarget<'gc>) -> String {
    match target {
        BranchTarget::Local { block, args } => {
            format!("BB{}({})", block.0, render_call_args_list(args))
        }
        BranchTarget::Reified { continuation, args } => format!(
            "Reified {}({})",
            render_operand(*continuation),
            render_call_args_list(args)
        ),
    }
}

fn render_rest_predicate(predicate: RestPredicate) -> &'static str {
    match predicate {
        RestPredicate::Null => "Null?",
        RestPredicate::Pair => "Pair?",
        RestPredicate::List => "List?",
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

fn render_switch_case_value<'gc>(value: crate::compiler::ssa::SwitchCaseValue<'gc>) -> String {
    match value {
        crate::compiler::ssa::SwitchCaseValue::Integer(value) => value.to_string(),
        crate::compiler::ssa::SwitchCaseValue::Symbol { value, .. } => render_value(value),
    }
}

fn render_call_args<'gc>(args: &[Operand<'gc>]) -> String {
    format!("({})", render_call_args_list(args))
}

fn render_call_args_list<'gc>(args: &[Operand<'gc>]) -> String {
    args.iter()
        .map(|atom| render_operand(*atom))
        .collect::<Vec<_>>()
        .join(", ")
}

fn render_operand<'gc>(atom: Operand<'gc>) -> String {
    match atom {
        Operand::Constant(value) => render_value(value),
        Operand::Local(var) => render_value_id(var),
    }
}

fn render_value_id(id: ValueId) -> String {
    format!("v@{}", id.0)
}

fn render_value_ids(vars: &[ValueId]) -> String {
    vars.iter()
        .map(|var| render_value_id(*var))
        .collect::<Vec<_>>()
        .join(", ")
}

fn render_optional_value_id(var: Option<ValueId>) -> String {
    var.map(render_value_id).unwrap_or_else(|| "#f".to_string())
}

fn render_code_id(code: &CodeId) -> String {
    match code {
        CodeId::GraphFunction(function) => format!("gf{}", function.0),
        CodeId::GraphContinuation(continuation) => format!("gk{}", continuation.0),
    }
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

fn render_procedure_name<'gc>(procedure: &Procedure<'gc>) -> String {
    if let Some(name) = render_source_name(procedure.binding, &procedure.sources) {
        return name;
    }
    render_value(procedure.name)
}

fn render_source_name<'gc>(
    binding: ValueId,
    sources: &HashMap<ValueId, LVarRef<'gc>>,
) -> Option<String> {
    sources.get(&binding).map(|var| render_lvar(*var))
}

fn render_lvar<'gc>(var: LVarRef<'gc>) -> String {
    render_value(var.name)
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
