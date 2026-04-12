use std::fmt;

use crate::cps::term::BranchHint;

use super::{
    Block, BlockId, ConstId, ConstRef, Inst, InstKind, Module, Proc, ProcId, ProcKind, TailTarget,
    Terminator, ValueId,
};

impl<'gc> Module<'gc> {
    pub fn pretty(&self) -> PrettyModule<'_, 'gc> {
        PrettyModule { module: self }
    }

    pub fn pretty_string(&self) -> String {
        self.pretty().to_string()
    }
}

pub struct PrettyModule<'a, 'gc> {
    module: &'a Module<'gc>,
}

impl fmt::Display for PrettyModule<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(
            f,
            "module format={} entry={}",
            self.module.format_version,
            fmt_proc(self.module.entry)
        )?;

        if self.module.const_pool.is_empty() {
            writeln!(f, "consts: []")?;
        } else {
            writeln!(f, "consts:")?;
            for (index, data) in self.module.const_pool.entries.iter().enumerate() {
                writeln!(f, "  {} = {}", fmt_const(ConstId(index as u32)), data.get())?;
            }
        }

        for proc in self.module.procs.iter() {
            writeln!(f)?;
            write_proc(f, self.module, proc)?;
        }

        Ok(())
    }
}

fn write_proc(f: &mut fmt::Formatter<'_>, module: &Module<'_>, proc: &Proc<'_>) -> fmt::Result {
    write!(
        f,
        "proc {} {} {} entry={}",
        fmt_proc(proc.id),
        fmt_proc_kind(proc.kind),
        fmt_name_ref(module, proc.binding_name),
        fmt_block(proc.entry)
    )?;
    if proc.cold || proc.noinline {
        write!(f, " flags=[")?;
        let mut first = true;
        if proc.cold {
            first = false;
            f.write_str("cold")?;
        }
        if proc.noinline {
            if !first {
                f.write_str(", ")?;
            }
            f.write_str("noinline")?;
        }
        f.write_str("]")?;
    }
    writeln!(f)?;

    if let Some(display_name) = proc.display_name {
        writeln!(f, "  display-name: {}", fmt_const_ref(module, display_name))?;
    }
    if let Some(source) = proc.source {
        writeln!(f, "  source: {}", fmt_const_ref(module, source))?;
    }
    if let Some(meta) = proc.meta {
        writeln!(f, "  meta: {}", fmt_const_ref(module, meta))?;
    }

    write!(f, "  params: self={}", fmt_value(proc.self_param))?;
    if let Some(retk) = proc.retk_param {
        write!(f, ", retk={}", fmt_value(retk))?;
    }
    write!(f, ", args=[")?;
    for (index, arg) in proc.arg_params.iter().enumerate() {
        if index > 0 {
            f.write_str(", ")?;
        }
        f.write_str(&fmt_value(*arg))?;
    }
    f.write_str("]")?;
    if let Some(rest) = proc.rest_param {
        write!(f, ", rest={}", fmt_value(rest))?;
    }
    writeln!(f)?;

    write!(f, "  captures: [")?;
    for (index, capture) in proc.captures.iter().enumerate() {
        if index > 0 {
            f.write_str(", ")?;
        }
        write!(
            f,
            "{}:{}",
            capture.index,
            fmt_name_ref(module, capture.binding_name)
        )?;
    }
    writeln!(f, "]")?;

    for block in proc.blocks.iter() {
        writeln!(f)?;
        write_block(f, module, block)?;
    }

    Ok(())
}

fn write_block(f: &mut fmt::Formatter<'_>, module: &Module<'_>, block: &Block<'_>) -> fmt::Result {
    write!(f, "  {}", fmt_block(block.id))?;
    if let Some(name) = &block.name {
        write!(f, " {}", fmt_name_ref(module, *name))?;
    }
    f.write_str("(")?;
    for (index, param) in block.params.iter().enumerate() {
        if index > 0 {
            f.write_str(", ")?;
        }
        match &param.name {
            Some(name) => write!(
                f,
                "{}={}",
                fmt_name_ref(module, *name),
                fmt_value(param.value)
            )?,
            None => f.write_str(&fmt_value(param.value))?,
        }
    }
    f.write_str(")")?;
    if block.cold {
        f.write_str(" [cold]")?;
    }
    writeln!(f, ":")?;

    for inst in block.insts.iter() {
        writeln!(f, "    {}", fmt_inst(module, inst))?;
    }
    writeln!(f, "    {}", fmt_term(module, &block.term))?;
    Ok(())
}

fn fmt_inst(module: &Module<'_>, inst: &Inst<'_>) -> String {
    let mut line = String::new();
    if let Some(result) = inst.result {
        line.push_str(&fmt_value(result));
        line.push_str(" = ");
    }

    match &inst.kind {
        InstKind::Const(id) => {
            line.push_str("const ");
            line.push_str(&fmt_const_ref(module, *id));
        }

        InstKind::RestIsEmpty { rest, skip } => {
            line.push_str("rest-is-empty ");
            line.push_str(&fmt_value(*rest));
            line.push_str(" skip=");
            line.push_str(&skip.to_string());
        }

        InstKind::RestIsNotEmpty { rest, skip } => {
            line.push_str("rest-is-not-empty ");
            line.push_str(&fmt_value(*rest));
            line.push_str(" skip=");
            line.push_str(&skip.to_string());
        }

        InstKind::PackRest { items } => {
            line.push_str("pack-rest [");
            line.push_str(&fmt_values(items));
            line.push(']');
        }
        InstKind::PrimCall { prim, args } => {
            line.push_str("prim ");
            line.push_str(prim.name());
            line.push('(');
            line.push_str(&fmt_values(args));
            line.push(')');
        }
        InstKind::RestRef { rest, index } => {
            line.push_str("rest-ref ");
            line.push_str(&fmt_value(*rest));
            line.push('[');
            line.push_str(&index.to_string());
            line.push(']');
        }
        InstKind::RestLength { rest, skip } => {
            line.push_str("rest-length ");
            line.push_str(&fmt_value(*rest));
            line.push_str(" skip=");
            line.push_str(&skip.to_string());
        }
        InstKind::RestToList { rest, skip } => {
            line.push_str("rest-to-list ");
            line.push_str(&fmt_value(*rest));
            line.push_str(" skip=");
            line.push_str(&skip.to_string());
        }
        InstKind::MakeClosure {
            proc,
            is_cont,
            meta,
            slots,
        } => {
            line.push_str("make-closure ");
            line.push_str(if *is_cont {
                "continuation "
            } else {
                "function "
            });
            line.push_str(&fmt_proc(*proc));
            if let Some(meta) = meta {
                line.push_str(" meta=");
                line.push_str(&fmt_const_ref(module, *meta));
            }
            line.push_str(" slots=");
            line.push_str(&slots.to_string());
        }
        InstKind::InitClosure { closure, captures } => {
            line.push_str("init-closure ");
            line.push_str(&fmt_value(*closure));
            line.push_str(" [");
            line.push_str(&fmt_values(captures));
            line.push(']');
        }
        InstKind::ClosureRef { closure, index } => {
            line.push_str("closure-ref ");
            line.push_str(&fmt_value(*closure));
            line.push('[');
            line.push_str(&index.to_string());
            line.push(']');
        }
    }

    if let Some(source) = inst.source {
        line.push_str(" ; source=");
        line.push_str(&fmt_const_ref(module, source));
    }

    line
}

fn fmt_term(module: &Module<'_>, term: &Terminator<'_>) -> String {
    let mut line = String::new();
    match term {
        Terminator::Jump { block, args } => {
            line.push_str("jump ");
            line.push_str(&fmt_block(*block));
            line.push('(');
            line.push_str(&fmt_values(args));
            line.push(')');
        }
        Terminator::Branch {
            cond,
            then_block,
            then_args,
            else_block,
            else_args,
            hints,
        } => {
            line.push_str("branch ");
            line.push_str(&fmt_value(*cond));
            line.push_str(" then ");
            line.push_str(&fmt_block(*then_block));
            line.push('(');
            line.push_str(&fmt_values(then_args));
            line.push(')');
            line.push_str(" [");
            line.push_str(fmt_branch_hint(hints[0]));
            line.push(']');
            line.push_str(" else ");
            line.push_str(&fmt_block(*else_block));
            line.push('(');
            line.push_str(&fmt_values(else_args));
            line.push(')');
            line.push_str(" [");
            line.push_str(fmt_branch_hint(hints[1]));
            line.push(']');
        }
        Terminator::TailContinue {
            callee,
            args,
            source,
        } => {
            line.push_str("tail-continue target=");
            line.push_str(&fmt_tail_target(callee));
            line.push_str(" args=(");
            line.push_str(&fmt_values(args));
            line.push(')');
            if let Some(source) = source {
                line.push_str(" ; source=");
                line.push_str(&fmt_const_ref(module, *source));
            }
        }
        Terminator::TailApp {
            callee,
            retk,
            args,
            source,
        } => {
            line.push_str("tail-app target=");
            line.push_str(&fmt_tail_target(callee));
            line.push_str(" retk=");
            line.push_str(&fmt_value(*retk));
            line.push_str(" args=(");
            line.push_str(&fmt_values(args));
            line.push(')');
            if let Some(source) = source {
                line.push_str(" ; source=");
                line.push_str(&fmt_const_ref(module, *source));
            }
        }
    }
    line
}

fn fmt_tail_target(target: &TailTarget) -> String {
    match target {
        TailTarget::Direct { proc, closure } => {
            format!("direct {} via {}", fmt_proc(*proc), fmt_value(*closure))
        }
        TailTarget::Indirect { closure } => format!("indirect {}", fmt_value(*closure)),
    }
}

fn fmt_values(values: &[ValueId]) -> String {
    values
        .iter()
        .map(|value| fmt_value(*value))
        .collect::<Vec<_>>()
        .join(", ")
}

fn fmt_proc_kind(kind: ProcKind) -> &'static str {
    match kind {
        ProcKind::Function => "function",
        ProcKind::Continuation => "continuation",
    }
}

fn fmt_branch_hint(hint: BranchHint) -> &'static str {
    match hint {
        BranchHint::Normal => "normal",
        BranchHint::Hot => "hot",
        BranchHint::Cold => "cold",
    }
}

fn fmt_proc(id: ProcId) -> String {
    format!("p{}", id.0)
}

fn fmt_block(id: BlockId) -> String {
    format!("b{}", id.0)
}

fn fmt_value(id: ValueId) -> String {
    format!("v{}", id.0)
}

fn fmt_const(id: ConstId) -> String {
    format!("c{}", id.0)
}

fn fmt_const_ref(module: &Module<'_>, const_ref: ConstRef) -> String {
    match const_ref {
        ConstRef::Inline(value) => value.to_value().to_string(),
        ConstRef::Pool(id) => match module.const_pool.get(id) {
            Some(value) => format!("{}={}", fmt_const(id), value),
            None => fmt_const(id),
        },
    }
}

fn fmt_name_ref(module: &Module<'_>, const_ref: ConstRef) -> String {
    match module.decode_const(const_ref) {
        Ok(value) => value.to_string(),
        Err(_) => fmt_const_ref(module, const_ref),
    }
}
