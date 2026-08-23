//! SBBV stage dumps and primitive guard statistics.

use super::specialize::BlockAnnotation;
use crate::compiler::{
    cfg::{Instruction, Procedure, Program, render_program_with_annotations},
    cranelift::primitive::Primitive,
    dump,
};

const GUARD_PRIMITIVES: &[Primitive] = &[
    Primitive::IsFixnum,
    Primitive::IsFlonum,
    Primitive::IsPair,
    Primitive::IsVector,
    Primitive::IsString,
    Primitive::IsBytevector,
    Primitive::IsNull,
    Primitive::IsChar,
    Primitive::IsBoolean,
    Primitive::IsProcedure,
    Primitive::IsNumber,
    Primitive::IsReal,
    Primitive::IsExactInteger,
];

const UNCHECKED_PRIMITIVES: &[Primitive] = &[
    Primitive::FxAddUnchecked,
    Primitive::FxSubUnchecked,
    Primitive::FxMulUnchecked,
    Primitive::FxAddOvfUnchecked,
    Primitive::FxSubOvfUnchecked,
    Primitive::FxMulOvfUnchecked,
    Primitive::FxLtUnchecked,
    Primitive::FxLeUnchecked,
    Primitive::FxGtUnchecked,
    Primitive::FxGeUnchecked,
    Primitive::FxEqUUnchecked,
    Primitive::FxAndUnchecked,
    Primitive::FxIorUnchecked,
    Primitive::FxXorUnchecked,
    Primitive::FxNotUnchecked,
    Primitive::FxAshlUnchecked,
    Primitive::FxAshrUnchecked,
    Primitive::FxZeroUnchecked,
    Primitive::FxPositiveUnchecked,
    Primitive::FxNegativeUnchecked,
    Primitive::FxOddUnchecked,
    Primitive::FxEvenUnchecked,
    Primitive::FxMinUnchecked,
    Primitive::FxMaxUnchecked,
    Primitive::FxQuotient,
    Primitive::FxRemainder,
    Primitive::FxModulo,
    Primitive::FlAddUnchecked,
    Primitive::FlSubUnchecked,
    Primitive::FlMulUnchecked,
    Primitive::FlDivUnchecked,
    Primitive::FlLtUnchecked,
    Primitive::FlLeUnchecked,
    Primitive::FlGtUnchecked,
    Primitive::FlGeUnchecked,
    Primitive::FlEqUnchecked,
    Primitive::FlSqrtUnchecked,
    Primitive::FlAtanUnchecked,
    Primitive::FlAbsUnchecked,
    Primitive::FlFloorUnchecked,
    Primitive::FlCeilingUnchecked,
    Primitive::FlTruncateUnchecked,
    Primitive::FlRoundUnchecked,
    Primitive::FlSinUnchecked,
    Primitive::FlCosUnchecked,
    Primitive::FlTanUnchecked,
    Primitive::FlExpUnchecked,
    Primitive::FlLogUnchecked,
    Primitive::FlAsinUnchecked,
    Primitive::FlAcosUnchecked,
    Primitive::CarUnchecked,
    Primitive::CdrUnchecked,
    Primitive::SetCarUnchecked,
    Primitive::SetCdrUnchecked,
    Primitive::VectorRefUnchecked,
    Primitive::VectorSetUnchecked,
    Primitive::VectorLengthUnchecked,
    Primitive::StringRefUnchecked,
    Primitive::StringLengthUnchecked,
    Primitive::BytevectorU8RefUnchecked,
    Primitive::BytevectorLengthUnchecked,
];

pub(crate) fn maybe_dump_procedure<'gc>(
    stage: &str,
    procedure: &Procedure<'gc>,
    annotations: Option<&std::collections::HashMap<crate::compiler::cfg::BlockId, BlockAnnotation>>,
) {
    if !dump::sbbv_dump_stage_enabled(stage) {
        return;
    }
    let Some(path) =
        dump::resolve_sbbv_dump_path(&format!("{}-{}", stage, render_code_id(&procedure.code)))
    else {
        // resolve_sbbv_dump_path already warned (missing dir or dump limit).
        return;
    };

    let program = Program {
        entry: procedure.code,
        procedures: vec![procedure.clone()],
    };
    let mut rendered = render_program_with_annotations(&program, annotations);
    rendered.push('\n');
    rendered.push_str(&render_guard_stats(procedure));
    rendered.push('\n');

    if let Err(err) = std::fs::write(&path, rendered) {
        eprintln!(
            ";; WARN  (capy)@sbbv: failed to write {} dump to {}: {err}",
            stage,
            path.display()
        );
        return;
    }
    dump::SBBV.log_path(&format!("SBBV {stage}"), &path);
}

fn render_code_id(code: &crate::compiler::cfg::CodeId) -> String {
    use crate::compiler::cfg::CodeId;
    match code {
        CodeId::GraphFunction(id) => format!("gf{}", id.0),
        CodeId::GraphContinuation(id) => format!("gk{}", id.0),
    }
}

fn count_prim_calls(procedure: &Procedure<'_>, set: &[Primitive]) -> usize {
    let mut count = 0;
    for block in &procedure.blocks {
        for instruction in &block.instructions {
            if let Instruction::PrimCall { prim, .. } = instruction
                && set.contains(prim)
            {
                count += 1;
            }
        }
    }
    count
}

fn render_guard_stats(procedure: &Procedure<'_>) -> String {
    let guards = count_prim_calls(procedure, GUARD_PRIMITIVES);
    let unchecked = count_prim_calls(procedure, UNCHECKED_PRIMITIVES);
    let mut lines = vec![format!(
        ";; sbbv-stats guards={guards} unchecked={unchecked} blocks={}",
        procedure.blocks.len()
    )];

    for prim in GUARD_PRIMITIVES {
        let count = count_prim_calls(procedure, &[*prim]);
        if count > 0 {
            lines.push(format!(";;   guard {}: {count}", prim.name()));
        }
    }
    for prim in UNCHECKED_PRIMITIVES {
        let count = count_prim_calls(procedure, &[*prim]);
        if count > 0 {
            lines.push(format!(";;   unchecked {}: {count}", prim.name()));
        }
    }

    lines.join("\n")
}
