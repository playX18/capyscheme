use crate::{
    compiler::cfg::{
        Block, BranchTarget, CodeId, Instruction, Operand, Procedure, Program, RestPredicate,
        SwitchKind, Terminator, UVar,
    },
    expander::core::LVarRef,
};
use std::{collections::HashMap, fmt::Write};

use crate::compiler::cps::pretty::render_value;

pub fn render_program<'gc>(program: &Program<'gc>) -> String {
    let mut out = String::new();
    for procedure in &program.procedures {
        render_procedure(&mut out, procedure);
        writeln!(out).unwrap();
    }
    out
}

fn render_procedure<'gc>(out: &mut String, procedure: &Procedure<'gc>) {
    let name = render_procedure_name(procedure);
    let params = render_uvars(&procedure.params);
    writeln!(
        out,
        "fn {} {}({}) {{",
        render_code_id(&procedure.code),
        name.trim(),
        params,
    )
    .expect("infallible allocation callback");

    let mut blocks = procedure.blocks.clone();
    blocks.sort_by_key(|block| block.id.0);
    for block in &blocks {
        render_block(out, block);
        writeln!(out).unwrap();
    }
    writeln!(out, "}}").unwrap();
}

fn render_block<'gc>(out: &mut String, block: &Block<'gc>) {
    writeln!(out, "block{}:", block.id.0).unwrap();

    for instruction in &block.instructions {
        writeln!(out, "    {}", render_instruction(instruction))
            .expect("write to string cannot fail");
    }

    writeln!(out, "    {}", render_terminator(&block.terminator))
        .expect("write to string cannot fail");
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
            free_count,
            env,
        } => {
            let env_part = match env {
                Some(env) => format!(" env {}", render_operand(*env)),
                None => String::new(),
            };
            format!(
                "{} = make_closure {} {}{}",
                render_uvar(*dst),
                render_code_id(code),
                free_count,
                env_part
            )
        }
        Instruction::MakeEnv { dst, size } => {
            format!("{} = make_env {}", render_uvar(*dst), size)
        }
        Instruction::EnvRef { dst, env, index } => format!(
            "{} = env_ref {}[{}]",
            render_uvar(*dst),
            render_operand(*env),
            index
        ),
        Instruction::EnvSet { env, index, value } => format!(
            "env_set {}[{}], {}",
            render_operand(*env),
            index,
            render_operand(*value)
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
        Instruction::Receive { dst, index } => {
            format!("{} = receive {}", render_uvar(*dst), index)
        }
        Instruction::ReceiveRest { dst, from } => {
            format!("{} = receive_rest {}", render_uvar(*dst), from)
        }
    }
}

fn render_terminator<'gc>(terminator: &Terminator<'gc>) -> String {
    match terminator {
        Terminator::Call {
            callee, ret, args, ..
        } => format!(
            "call {}({}) -> block{} ({}{} values)",
            render_operand(*callee),
            render_call_args_list(args),
            ret.block.0,
            ret.nvals,
            if ret.variadic { "+" } else { "" }
        ),
        Terminator::Return { args, .. } => format!("return ({})", render_call_args_list(args)),
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
        BranchTarget::Return { args } => format!("return ({})", render_call_args_list(args)),
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

fn render_code_id(code: &CodeId) -> String {
    match code {
        CodeId::GraphFunction(function) => format!("gf{}", function.0),
    }
}

fn render_procedure_name<'gc>(procedure: &Procedure<'gc>) -> String {
    if let Some(name) = render_source_name(procedure.binding, &procedure.sources) {
        return name;
    }
    render_value(procedure.name)
}

fn render_source_name<'gc>(binding: UVar, sources: &HashMap<UVar, LVarRef<'gc>>) -> Option<String> {
    sources.get(&binding).map(|var| render_lvar(*var))
}

fn render_lvar<'gc>(var: LVarRef<'gc>) -> String {
    render_value(var.name)
}
