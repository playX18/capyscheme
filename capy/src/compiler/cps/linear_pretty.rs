use crate::{
    compiler::ssa::{
        Block, BlockId, BranchTarget, ClosureKind, CodeId, Instruction, LinearAtom, LinearProgram,
        Procedure, ProcedureKind, RestPredicate, SwitchKind, Terminator, ValueId,
    },
    expander::core::LVarRef,
    runtime::value::{Str, Symbol, Value, Vector},
};
use std::fmt::Write;

pub fn render_program<'gc>(program: &LinearProgram<'gc>) -> String {
    let mut out = String::new();
    writeln!(out, "(linear-program").unwrap();
    writeln!(out, "  (entry {})", render_code_id(&program.entry)).unwrap();
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
        "{pad}(procedure {} {}{}",
        render_procedure_kind(procedure.kind),
        render_code_id(&procedure.code),
        render_source_suffix(procedure.source)
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
        "{pad}(block {} (params{}) (variadic {}){}",
        render_block_id(block.id),
        render_value_ids(&block.params),
        render_optional_value_id(block.variadic),
        render_source_suffix(block.source)
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
        Instruction::CacheRef {
            dst,
            cache_key,
            source,
        } => with_source_suffix(
            format!(
                "(cache-ref {} {})",
                render_value_id(*dst),
                render_linear_atom(*cache_key)
            ),
            *source,
        ),
        Instruction::CacheSet {
            dst,
            cache_key,
            value,
            source,
        } => with_source_suffix(
            format!(
                "(cache-set! {} {} {})",
                render_value_id(*dst),
                render_linear_atom(*cache_key),
                render_linear_atom(*value)
            ),
            *source,
        ),
        Instruction::PrimCall {
            dst,
            prim,
            args,
            source,
        } => with_source_suffix(
            format!(
                "(prim-call {} {}{})",
                render_value_id(*dst),
                prim,
                render_linear_atoms(args)
            ),
            *source,
        ),
        Instruction::RestToList { dst, rest, source } => with_source_suffix(
            format!(
                "(rest->list {} {})",
                render_value_id(*dst),
                render_value_id(*rest)
            ),
            *source,
        ),
        Instruction::RestRef {
            dst,
            rest,
            index,
            source,
        } => with_source_suffix(
            format!(
                "(rest-ref {} {} {})",
                render_value_id(*dst),
                render_value_id(*rest),
                index
            ),
            *source,
        ),
        Instruction::RestLength {
            dst,
            rest,
            skip,
            source,
        } => with_source_suffix(
            format!(
                "(rest-length {} {} {})",
                render_value_id(*dst),
                render_value_id(*rest),
                skip
            ),
            *source,
        ),
        Instruction::RestPredicate {
            dst,
            rest,
            predicate,
            skip,
            source,
        } => with_source_suffix(
            format!(
                "(rest-{} {} {} {})",
                render_rest_predicate(*predicate),
                render_value_id(*dst),
                render_value_id(*rest),
                skip
            ),
            *source,
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
            callee,
            retk,
            args,
            source,
        } => with_source_suffix(
            format!(
                "(call {} {}{})",
                render_linear_atom(*callee),
                render_linear_atom(*retk),
                render_linear_atoms(args)
            ),
            *source,
        ),
        Terminator::TailCall {
            callee,
            args,
            source,
        } => with_source_suffix(
            format!(
                "(tail-call {}{})",
                render_linear_atom(*callee),
                render_linear_atoms(args)
            ),
            *source,
        ),
        Terminator::Raise { kind, args, source } => with_source_suffix(
            format!("(%raise {:?}{})", kind, render_linear_atoms(args)),
            *source,
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
        SwitchKind::SymbolEq { .. } => "eq?",
    }
}

fn render_switch_case_value<'gc>(value: crate::compiler::ssa::SwitchCaseValue<'gc>) -> String {
    match value {
        crate::compiler::ssa::SwitchCaseValue::Integer(value) => value.to_string(),
        crate::compiler::ssa::SwitchCaseValue::Symbol { value, .. } => render_value(value),
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

fn render_code_id(code: &CodeId) -> String {
    match code {
        CodeId::GraphFunction(function) => format!("(graph-function {})", function.0),
        CodeId::GraphContinuation(continuation) => {
            format!("(graph-continuation {})", continuation.0)
        }
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

fn with_source_suffix<'gc>(rendered: String, source: Value<'gc>) -> String {
    format!("{rendered}{}", render_source_suffix(source))
}

fn render_source_suffix<'gc>(source: Value<'gc>) -> String {
    render_source_location(source)
        .map(|location| format!(" ; @ {location}"))
        .unwrap_or_default()
}

fn render_source_location<'gc>(source: Value<'gc>) -> Option<String> {
    if !source.is::<Vector>() {
        return None;
    }

    let source = source.downcast::<Vector>();
    if source.len() < 3 {
        return None;
    }

    let file = render_source_file(source[0].get())?;
    let line = source_i32(source[1].get())?;
    let column = source_i32(source[2].get())?;
    let start = format!("{file}:{line}:{column}");
    let Some(end_line) = source_field_i32(source, 3) else {
        return Some(start);
    };
    let Some(end_column) = source_field_i32(source, 4) else {
        return Some(start);
    };

    Some(format!("{start}-{end_line}:{end_column}"))
}

fn source_field_i32<'gc>(source: crate::rsgc::Gc<'gc, Vector<'gc>>, index: usize) -> Option<i32> {
    if index < source.len() {
        source_i32(source[index].get())
    } else {
        None
    }
}

fn source_i32<'gc>(value: Value<'gc>) -> Option<i32> {
    value.is_int32().then(|| value.as_int32())
}

fn render_source_file<'gc>(value: Value<'gc>) -> Option<String> {
    if value == Value::new(false) {
        None
    } else if value.is::<Str>() {
        Some(value.downcast::<Str>().as_str().into_owned())
    } else if value.is::<Symbol>() {
        Some(value.downcast::<Symbol>().to_string())
    } else {
        Some(render_value(value))
    }
}

