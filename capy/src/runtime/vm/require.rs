use std::path::Path;

use pretty::BoxAllocator;
use rsgc::{Gc, mmtk::util::Address};

use crate::{
    expander::{
        assignment_elimination::eliminate_assignments,
        compile_cps::cps_toplevel,
        core::{Cenv, expand},
        fix_letrec::fix_letrec,
        primitives::resolve_primitives,
    },
    frontend::reader::TreeSitter,
    jit::JitContext,
    runtime::{
        Context,
        value::{Closure, ScmHeader, Str, TypeCode16, Value},
    },
};

pub fn load<'gc>(ctx: Context<'gc>, path: impl AsRef<Path>) -> Result<Value<'gc>, Value<'gc>> {
    let path = path.as_ref();
    if !path.exists() {
        return Err(Value::new(Str::new(
            &ctx,
            format!("File not found: {}", path.display()),
            false,
        )));
    }
    let path = path.canonicalize().map_err(|e| {
        Value::new(Str::new(
            &ctx,
            format!("Error canonicalizing path {}: {}", path.display(), e),
            false,
        ))
    })?;

    let text = std::fs::read_to_string(&path)
        .map_err(|e| Value::new(Str::new(&ctx, e.to_string(), false)))?;

    let filename = Value::new(Str::new(&ctx, path.display().to_string(), false));
    let tree_sitter = TreeSitter::new(ctx, &text, filename);

    let program = tree_sitter.read_program().map_err(|e| {
        Value::new(Str::new(
            &ctx,
            format!("Error parsing file {}: {:?}", path.display(), e),
            false,
        ))
    })?;

    let mut terms = Vec::new();

    for expr in program {
        let mut cenv = Cenv::toplevel(ctx);
        let mut term = expand(&mut cenv, expr).map_err(|e| {
            Value::new(Str::new(
                &ctx,
                format!("Error expanding expression: {:?}", e),
                false,
            ))
        })?;

        term = fix_letrec(ctx, term);
        term = eliminate_assignments(ctx, term);
        term = resolve_primitives(ctx, term);
        terms.push(term);
    }

    let cps = cps_toplevel(ctx, &terms);
    let cps = crate::cps::optimizer::rewrite_func(ctx, cps);
    let reify_info = crate::cps::reify(cps);

    let doc = cps.pretty::<_, &BoxAllocator>(&BoxAllocator);
    let mut stdout = std::io::stdout();
    doc.1
        .render(70, &mut stdout)
        .expect("Failed to render CPS document");
    use std::io::Write;
    writeln!(stdout).expect("Failed to write newline");

    let mut jit = JitContext::new(ctx, reify_info);
    jit.lower_all(cps);
    let addr = jit.finalize(cps);

    let clos = Gc::new(
        &ctx,
        Closure {
            header: ScmHeader::with_type_bits(TypeCode16::CLOSURE_PROC.bits() as _),
            free: Value::null(),
            code: Address::from_ptr(addr),
            meta: Value::new(false),
        },
    );

    ctx.call(clos.into(), &[])
}
