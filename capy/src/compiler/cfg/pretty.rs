use crate::{
    compiler::cfg::{
        Block, BlockId, BranchTarget, ClosureKind, CodeId, Instruction, Operand, Procedure,
        ProcedureKind, Program, RestPredicate, SwitchKind, Terminator, UVar,
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
    let params = render_uvars(&procedure.params);
    let retk = render_optional_uvar(procedure.return_cont);
    writeln!(
        out,
        "{} {} {}({}) -> {} {{",
        render_procedure_kind(procedure.kind),
        render_code_id(&procedure.code),
        name.trim(),
        params,
        retk
    )
    .unwrap();

    let mut blocks = procedure.blocks.clone();
    blocks.sort_by_key(|block| block.id.0);
    for block in &blocks {
        render_block(out, block, annotations);
        writeln!(out).unwrap();
    }
    writeln!(out, "}}").unwrap();
}

fn render_block<'gc>(
    out: &mut String,
    block: &Block<'gc>,
    annotations: Option<&HashMap<BlockId, BlockAnnotation>>,
) {
    let mut header = format!("block{}:", block.id.0);
    if let Some(annotations) = annotations
        && let Some(annotation) = annotations.get(&block.id)
    {
        write!(
            header,
            " ; orig=block{} {}",
            annotation.orig.0, annotation.ctx
        )
        .unwrap();
    }
    writeln!(out, "{header}").unwrap();

    for instruction in &block.instructions {
        writeln!(out, "    {}", render_instruction(instruction)).unwrap();
    }

    writeln!(out, "    {}", render_terminator(&block.terminator)).unwrap();
}

fn render_instruction<'gc>(instruction: &Instruction<'gc>) -> String {
    match instruction {
        Instruction::Assign { dst, src } => {
            format!("{} = {}", render_uvar(*dst), render_operand(*src))
        }
        Instruction::Const { dst, value } => {
            format!("{} = const {}", render_uvar(*dst), render_value(*value))
        }
        Instruction::MakeClosure {
            dst,
            code,
            kind,
            free_count,
        } => format!(
            "{} = make_closure {} {} {}",
            render_uvar(*dst),
            render_code_id(code),
            render_closure_kind(*kind),
            free_count
        ),
        Instruction::ClosureRef {
            dst,
            closure,
            index,
        } => format!(
            "{} = closure_ref {}[{}]",
            render_uvar(*dst),
            render_operand(*closure),
            index
        ),
        Instruction::ClosureSet {
            closure,
            index,
            value,
        } => format!(
            "closure_set {}[{}], {}",
            render_operand(*closure),
            index,
            render_operand(*value)
        ),
        Instruction::CacheRef { dst, cache_key, .. } => format!(
            "{} = cache_ref {}",
            render_uvar(*dst),
            render_operand(*cache_key)
        ),
        Instruction::CacheSet {
            dst,
            cache_key,
            value,
            ..
        } => format!(
            "{} = cache_set {}, {}",
            render_uvar(*dst),
            render_operand(*cache_key),
            render_operand(*value)
        ),
        Instruction::PrimCall {
            dst, prim, args, ..
        } => format!("{} = {}{}", render_uvar(*dst), prim, render_call_args(args)),
        Instruction::RestToList { dst, rest, .. } => format!(
            "{} = rest_to_list {}",
            render_uvar(*dst),
            render_uvar(*rest)
        ),
        Instruction::RestRef {
            dst, rest, index, ..
        } => format!(
            "{} = rest_ref {}, {}",
            render_uvar(*dst),
            render_uvar(*rest),
            index
        ),
        Instruction::RestLength {
            dst, rest, skip, ..
        } => format!(
            "{} = rest_length {}, {}",
            render_uvar(*dst),
            render_uvar(*rest),
            skip
        ),
        Instruction::RestPredicate {
            dst,
            rest,
            predicate,
            skip,
            ..
        } => format!(
            "{} = rest_{} {}, {}",
            render_uvar(*dst),
            render_rest_predicate(*predicate),
            render_uvar(*rest),
            skip
        ),
    }
}

fn render_terminator<'gc>(terminator: &Terminator<'gc>) -> String {
    match terminator {
        Terminator::Call {
            callee, retk, args, ..
        } => format!(
            "call {}({}) -> {}",
            render_operand(*callee),
            render_call_args_list(args),
            render_operand(*retk)
        ),
        Terminator::TailCall { callee, args, .. } => format!(
            "tail_call {}({})",
            render_operand(*callee),
            render_call_args_list(args)
        ),
        Terminator::Raise { kind, args, .. } => {
            format!("raise {:?}({})", kind, render_call_args_list(args))
        }
        Terminator::Jump { target } => {
            format!("jump block{}", target.0)
        }
        Terminator::Branch {
            test,
            consequent,
            alternative,
            hints,
        } => format!(
            "brif {}, {}, {} ; {:?}, {:?}",
            render_operand(*test),
            render_branch_target(consequent),
            render_branch_target(alternative),
            hints[0],
            hints[1]
        ),
        Terminator::BranchPrim {
            prim,
            args,
            consequent,
            alternative,
            hints,
        } => format!(
            "brif_prim {}({}), {}, {} ; {:?}, {:?}",
            prim.name(),
            args.iter()
                .map(|arg| render_operand(*arg))
                .collect::<Vec<_>>()
                .join(", "),
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
                    "{} => {}",
                    render_switch_case_value(case.value),
                    render_branch_target(&case.target)
                ));
            }
            format!(
                "switch {} {}, [{}], default {}",
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
        BranchTarget::Local {
            block,
            edge_assigns,
        } => {
            if edge_assigns.is_empty() {
                format!("block{}", block.0)
            } else {
                format!("block{}[+{}]", block.0, edge_assigns.len())
            }
        }
        BranchTarget::Reified { continuation, args } => format!(
            "return {}({})",
            render_operand(*continuation),
            render_call_args_list(args)
        ),
    }
}

fn render_rest_predicate(predicate: RestPredicate) -> &'static str {
    match predicate {
        RestPredicate::Null => "null",
        RestPredicate::Pair => "pair",
        RestPredicate::List => "list",
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

fn render_switch_case_value<'gc>(value: crate::compiler::cfg::SwitchCaseValue<'gc>) -> String {
    match value {
        crate::compiler::cfg::SwitchCaseValue::Integer(value) => value.to_string(),
        crate::compiler::cfg::SwitchCaseValue::Symbol { value, .. } => render_value(value),
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
        Operand::Local(var) => render_uvar(var),
    }
}

fn render_uvar(id: UVar) -> String {
    format!("u{}", id.0)
}

fn render_uvars(vars: &[UVar]) -> String {
    vars.iter()
        .map(|var| render_uvar(*var))
        .collect::<Vec<_>>()
        .join(", ")
}

fn render_optional_uvar(var: Option<UVar>) -> String {
    var.map(render_uvar).unwrap_or_else(|| "#f".to_string())
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
    binding: UVar,
    sources: &HashMap<UVar, LVarRef<'gc>>,
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
