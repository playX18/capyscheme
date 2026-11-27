//! Data the Scheme compiler reads from the Rust runtime: the instruction set
//! (`%bytecode-isa`) and the primitive table (`%primitive-table`).

use crate::bytecode::def::{self, NUM_METADATA_KINDS, OpcodeId};
use crate::runtime::MutationContext;
use crate::runtime::objectmodel::{IntoValue, Str, Symbol, Value};
use crate::runtime::prims::calls::PRIM_ARITY;
use crate::runtime::prims::def::Primitive;
use crate::runtime::prims::effects::{
    EffectFlags, Effects, MemoryRanges, MemoryRegion, SemanticEffects, primitive_effects,
};

fn sym<'gc>(ctx: MutationContext<'gc>, name: &str) -> Value<'gc> {
    Symbol::from_str(ctx, name).into()
}

fn list<'gc>(ctx: MutationContext<'gc>, items: &[Value<'gc>]) -> Value<'gc> {
    Value::list_from_slice(ctx, items)
}

fn pair<'gc>(ctx: MutationContext<'gc>, key: &str, value: Value<'gc>) -> Value<'gc> {
    Value::cons(ctx, sym(ctx, key), value)
}

/// `(a b c)` from a slice of names, as a list of symbols.
fn symbols<'gc>(ctx: MutationContext<'gc>, names: &[&str]) -> Value<'gc> {
    let items: Vec<_> = names.iter().map(|name| sym(ctx, name)).collect();
    list(ctx, &items)
}

/// The kebab-case spelling of a Rust type name: `PoolReference` is `pool-reference`.
fn kebab(name: &str) -> String {
    let mut out = String::new();
    for (index, ch) in name.chars().enumerate() {
        if ch.is_ascii_uppercase() {
            if index != 0 {
                out.push('-');
            }
            out.push(ch.to_ascii_lowercase());
        } else {
            out.push(ch);
        }
    }
    out
}

/// One operand of a `BYTECODE_SIGNATURE` entry: `name[?][@reloc]:Type`.
struct SigOperand<'a> {
    name: &'a str,
    optional: bool,
    reloc: Option<&'a str>,
    ty: &'a str,
}

fn parse_operand(text: &str) -> Result<SigOperand<'_>, String> {
    let (head, ty) = text
        .split_once(':')
        .ok_or_else(|| format!("bad operand in bytecode signature: {text}"))?;
    let (head, reloc) = match head.split_once('@') {
        Some((head, reloc)) => (head, Some(reloc.trim())),
        None => (head, None),
    };
    let (name, optional) = match head.strip_suffix('?') {
        Some(name) => (name, true),
        None => (head, false),
    };
    Ok(SigOperand {
        name: name.trim(),
        optional,
        reloc,
        ty: ty.trim(),
    })
}

/// One opcode of `BYTECODE_SIGNATURE`: `name(operands)[metadata fields]`.
struct SigEntry<'a> {
    operands: Vec<SigOperand<'a>>,
    metadata: Vec<(&'a str, &'a str)>,
}

fn parse_signature(signature: &str) -> Result<Vec<SigEntry<'_>>, String> {
    let mut entries = Vec::new();
    for entry in signature.split(';') {
        let open = entry
            .find('(')
            .ok_or_else(|| format!("bad opcode in bytecode signature: {entry}"))?;
        let close = entry
            .find(')')
            .ok_or_else(|| format!("bad opcode in bytecode signature: {entry}"))?;
        let operands = entry[open + 1..close]
            .split(',')
            .filter(|text| !text.trim().is_empty())
            .map(parse_operand)
            .collect::<Result<Vec<_>, _>>()?;
        let rest = entry[close + 1..].trim();
        let mut metadata = Vec::new();
        if let Some(fields) = rest.strip_prefix('[').and_then(|r| r.strip_suffix(']')) {
            for field in fields.split(',') {
                let (name, ty) = field
                    .split_once(':')
                    .ok_or_else(|| format!("bad metadata field in bytecode signature: {field}"))?;
                metadata.push((name.trim(), ty.trim()));
            }
        }
        entries.push(SigEntry { operands, metadata });
    }
    Ok(entries)
}

/// `(%bytecode-isa)`: the instruction set of `bytecode/def.rs` as data.
///
/// An alist:
///
/// - `(format-hash . n)`: `bytecode::def::format_hash()`, an exact integer.
/// - `(num-metadata-kinds . n)`: `NUM_METADATA_KINDS`.
/// - `(wide16-prefix . 0)`, `(wide32-prefix . 1)`, `(first-opcode . 2)`: the header bytes
///   of `bytecodegen`, see docs/BYTECODE.md "Encoding".
/// - `(operand-kinds . ((kind type ...) ...))`: the operand types, `type` being `u32`
///   (unsigned) or `i32` (signed), e.g. `(register u32)`, `(immediate i32)`.
/// - `(opcodes . (opcode ...))`, in opcode-number order. Each opcode is
///   `(name number operands metadata)`:
///   - `name`: the symbol the disassembler prints, `return-point`.
///   - `number`: the opcode number. Numbers 0 and 1 are the wide prefixes, the first
///     opcode is 2. Numbers above 255 have no narrow encoding.
///   - `operands`: in encoding order, `(name kind)` or `(name kind relative-start)` for a
///     jump offset, whose value is `target - start of the instruction`. `kind` is a key of
///     `operand-kinds`.
///   - `metadata`: `#f`, or `(kind-index (field . type) ...)` when the opcode has a
///     metadata record, in which case the instruction ends with one more operand, its
///     metadata id (the index of the record among the instructions of that opcode).
/// - `(metadata-kinds . ((kind-index opcode-name type-name field ...) ...))`: the metadata
///   arrays, in `kind-index` order (the order of `metadata-counts` in a program image).
pub fn bytecode_isa<'gc>(ctx: MutationContext<'gc>) -> Result<Value<'gc>, String> {
    let entries = parse_signature(def::BYTECODE_SIGNATURE)?;
    if entries.len() != OpcodeId::COUNT {
        return Err("bytecode signature and opcode count disagree".into());
    }

    let mut operand_kinds: Vec<String> = Vec::new();
    let mut opcodes = Vec::with_capacity(entries.len());
    let mut metadata_kinds: Vec<(usize, Value<'gc>)> = Vec::new();

    for (index, entry) in entries.iter().enumerate() {
        let number = bytecodegen::FIRST_OPCODE_ID + index as u16;
        let id = OpcodeId::try_from(number)
            .map_err(|_| format!("no opcode numbered {number} in the schema"))?;
        if id.table_index() != index {
            return Err("opcode numbers do not follow schema order".into());
        }
        let kebab_full = id.kebab_name();
        let name = kebab_full.strip_prefix("op-").unwrap_or(kebab_full);

        let operands = entry
            .operands
            .iter()
            .map(|operand| {
                if operand.optional {
                    return Err(format!("optional operand {} in {name}", operand.name));
                }
                let kind = kebab(operand.ty);
                if !operand_kinds.contains(&kind) {
                    operand_kinds.push(kind.clone());
                }
                let mut items = vec![sym(ctx, operand.name), sym(ctx, &kind)];
                match operand.reloc {
                    None => {}
                    Some("relative_start") => items.push(sym(ctx, "relative-start")),
                    Some(other) => return Err(format!("relocation {other} in {name}")),
                }
                Ok(list(ctx, &items))
            })
            .collect::<Result<Vec<_>, String>>()?;

        let metadata = match id.metadata_kind() {
            None => Value::new(false),
            Some(kind) => {
                let fields: Vec<_> = entry
                    .metadata
                    .iter()
                    .map(|(field, ty)| Value::cons(ctx, sym(ctx, field), sym(ctx, ty)))
                    .collect();
                let mut kind_entry = vec![Value::new(kind as i32), sym(ctx, name)];
                kind_entry.push(
                    Str::new(ctx, format!("Op{}Metadata", rust_variant_name(name)), true).into(),
                );
                kind_entry.extend(fields.iter().copied());
                metadata_kinds.push((kind, list(ctx, &kind_entry)));
                Value::cons(ctx, Value::new(kind as i32), list(ctx, &fields))
            }
        };
        opcodes.push(list(
            ctx,
            &[
                sym(ctx, name),
                Value::new(i32::from(number)),
                list(ctx, &operands),
                metadata,
            ],
        ));
    }
    metadata_kinds.sort_by_key(|(kind, _)| *kind);
    if metadata_kinds.len() != NUM_METADATA_KINDS {
        return Err("metadata kind count disagrees with the schema".into());
    }

    let kinds: Vec<_> = operand_kinds
        .iter()
        .map(|kind| {
            let ty = if kind == "i32" || kind == "immediate" {
                "i32"
            } else {
                "u32"
            };
            list(ctx, &[sym(ctx, kind), sym(ctx, ty)])
        })
        .collect();

    let kind_entries: Vec<_> = metadata_kinds.into_iter().map(|(_, entry)| entry).collect();
    Ok(list(
        ctx,
        &[
            pair(ctx, "format-hash", def::format_hash().into_value(ctx)),
            pair(
                ctx,
                "num-metadata-kinds",
                Value::new(NUM_METADATA_KINDS as i32),
            ),
            pair(
                ctx,
                "wide16-prefix",
                Value::new(i32::from(bytecodegen::WIDE16_PREFIX)),
            ),
            pair(
                ctx,
                "wide32-prefix",
                Value::new(i32::from(bytecodegen::WIDE32_PREFIX)),
            ),
            pair(
                ctx,
                "first-opcode",
                Value::new(i32::from(bytecodegen::FIRST_OPCODE_ID)),
            ),
            pair(ctx, "operand-kinds", list(ctx, &kinds)),
            pair(ctx, "opcodes", list(ctx, &opcodes)),
            pair(ctx, "metadata-kinds", list(ctx, &kind_entries)),
        ],
    ))
}

/// `return-point` as `ReturnPoint`.
fn rust_variant_name(kebab_name: &str) -> String {
    kebab_name
        .split('-')
        .map(|part| {
            let mut chars = part.chars();
            match chars.next() {
                Some(first) => first.to_ascii_uppercase().to_string() + chars.as_str(),
                None => String::new(),
            }
        })
        .collect()
}

fn flag_names(flags: EffectFlags) -> Vec<&'static str> {
    const NAMES: &[(u16, &str)] = &[
        (1 << 0, "terminal"),
        (1 << 1, "exits-sideways"),
        (1 << 2, "control-dependent"),
        (1 << 3, "reads-mutable"),
        (1 << 4, "writes-local-state"),
        (1 << 5, "reads-local-state"),
        (1 << 6, "reads-pinned"),
        (1 << 7, "writes-pinned"),
        (1 << 8, "fence"),
        (1 << 9, "may-call"),
        (1 << 10, "may-raise"),
        (1 << 11, "allocates"),
    ];
    NAMES
        .iter()
        .filter(|(bit, _)| flags.bits() & bit != 0)
        .map(|(_, name)| *name)
        .collect()
}

fn semantic_names(effects: SemanticEffects) -> Vec<&'static str> {
    const NAMES: &[(u32, &str)] = &[
        (1 << 0, "mutable-lexical"),
        (1 << 2, "fluid"),
        (1 << 3, "definite-bailout"),
        (1 << 4, "possible-bailout"),
        (1 << 5, "zero-values"),
        (1 << 6, "allocation"),
        (1 << 7, "type-check"),
        (1 << 8, "car"),
        (1 << 9, "cdr"),
        (1 << 10, "vector"),
        (1 << 11, "variable"),
        (1 << 12, "string"),
        (1 << 13, "bytevector"),
        (1 << 14, "unknown"),
    ];
    NAMES
        .iter()
        .filter(|(bit, _)| effects.bits() & bit != 0)
        .map(|(_, name)| *name)
        .collect()
}

fn region_names(ranges: MemoryRanges) -> Vec<&'static str> {
    const REGIONS: &[(MemoryRegion, &str)] = &[
        (MemoryRegion::Heap, "heap"),
        (MemoryRegion::Variables, "variables"),
        (MemoryRegion::Continuations, "continuations"),
        (MemoryRegion::Runtime, "runtime"),
        (MemoryRegion::Unknown, "unknown"),
    ];
    REGIONS
        .iter()
        .filter(|(region, _)| MemoryRanges::region(*region).bits() & ranges.bits() != 0)
        .map(|(_, name)| *name)
        .collect()
}

fn effects_alist<'gc>(ctx: MutationContext<'gc>, effects: Effects) -> Value<'gc> {
    list(
        ctx,
        &[
            pair(ctx, "flags", symbols(ctx, &flag_names(effects.flags))),
            pair(
                ctx,
                "depends-on",
                symbols(ctx, &semantic_names(effects.depends_on)),
            ),
            pair(ctx, "causes", symbols(ctx, &semantic_names(effects.causes))),
            pair(ctx, "reads", symbols(ctx, &region_names(effects.reads))),
            pair(ctx, "writes", symbols(ctx, &region_names(effects.writes))),
            pair(ctx, "effect-free", Value::new(effects.effect_free())),
            pair(
                ctx,
                "allocation-only",
                Value::new(effects.allocation_only()),
            ),
        ],
    )
}

/// The `(argc yes-opcode no-opcode)` of a primitive that lowering fuses into a conditional
/// jump (`fused_jump_kinds` of `bytecode/lower.rs`): the instructions that jump when the
/// test holds and when it does not.
fn fused_jump(prim: Primitive) -> Option<(usize, &'static str, &'static str)> {
    use Primitive as P;
    Some(match prim {
        P::IsNull => (1, "jump-if-null", "jump-if-not-null"),
        P::IsPair => (1, "jump-if-pair", "jump-if-not-pair"),
        P::IsFixnum => (1, "jump-if-fixnum", "jump-if-not-fixnum"),
        P::IsUnspecified => (1, "jump-if-undefined", "jump-if-not-undefined"),
        P::IsEq => (2, "jump-if-eq", "jump-if-not-eq"),
        P::NumericLt => (2, "jump-if-less", "jump-if-not-less"),
        P::NumericLte => (2, "jump-if-less-equal", "jump-if-not-less-equal"),
        P::NumericEqual => (2, "jump-if-number-equal", "jump-if-not-number-equal"),
        P::NumericGte => (2, "jump-if-greater-equal", "jump-if-not-greater-equal"),
        P::NumericGt => (2, "jump-if-greater", "jump-if-not-greater"),
        _ => return None,
    })
}

/// The dedicated instruction `emit_prim` of `bytecode/lower.rs` selects for `(prim, argc)`:
/// `(argc, opcode)`. Any other call is `call-primitive0..3` (`call-primitive-n` for more
/// than three arguments) with the primitive's id.
fn dedicated_opcodes(prim: Primitive) -> &'static [(usize, &'static str)] {
    use Primitive as P;
    match prim {
        P::Car => &[(1, "car")],
        P::Cdr => &[(1, "cdr")],
        P::Cons => &[(2, "cons")],
        P::SetCar => &[(2, "set-car")],
        P::SetCdr => &[(2, "set-cdr")],
        P::IsEq => &[(2, "eq")],
        P::Not => &[(1, "not")],
        P::Plus => &[(2, "add")],
        P::Minus => &[(2, "sub"), (1, "negate")],
        P::Times => &[(2, "mul")],
        P::Div => &[(2, "div")],
        P::Quotient => &[(2, "quotient")],
        P::Remainder => &[(2, "remainder")],
        P::Modulo => &[(2, "modulo")],
        P::Abs => &[(1, "abs")],
        P::Sqrt => &[(1, "sqrt")],
        P::NumericLt => &[(2, "less")],
        P::NumericLte => &[(2, "less-equal")],
        P::NumericEqual => &[(2, "number-equal")],
        P::NumericGte => &[(2, "greater-equal")],
        P::NumericGt => &[(2, "greater")],
        P::VectorRef => &[(2, "vector-ref")],
        P::VectorLength => &[(1, "vector-length")],
        P::VectorSet => &[(3, "vector-set")],
        P::VariableRef => &[(1, "variable-ref")],
        P::VariableSet => &[(2, "variable-set")],
        P::SlotRef => &[(2, "get-slot")],
        P::SlotSet => &[(3, "put-slot")],
        _ => &[],
    }
}

/// Primitives whose result is `#<unspecified>` after a dedicated instruction that produces
/// no value.
fn result_is_unspecified(prim: Primitive) -> bool {
    use Primitive as P;
    matches!(prim, P::SetCar | P::SetCdr | P::VectorSet | P::VariableSet)
}

/// How the CFG passes treat a primitive outside of instruction selection:
/// `rest` (the `cfg/rest.scm` rewrite of a rest-list use), `switch` (`cfg/switch.scm` turns a chain of
/// tests on one value into a switch), `unlowered` (must not reach the bytecode lowering),
/// `slot`, `case-lambda`.
fn cfg_role(prim: Primitive) -> Vec<(&'static str, &'static str)> {
    use Primitive as P;
    let mut roles = Vec::new();
    match prim {
        P::Cdr => roles.push(("rest", "cdr")),
        P::Car => roles.push(("rest", "ref")),
        P::Length => roles.push(("rest", "length")),
        P::IsNull => roles.push(("rest", "null?")),
        P::IsPair => roles.push(("rest", "pair?")),
        P::IsList => roles.push(("rest", "list?")),
        _ => {}
    }
    match prim {
        P::IsEq | P::IsEqv | P::IsEqual => roles.push(("switch", "eq")),
        P::FxEq => roles.push(("switch", "fixnum")),
        P::NumericEqual => roles.push(("switch", "numeric")),
        P::CharToInteger => roles.push(("switch", "char-scrutinee")),
        _ => {}
    }
    match prim {
        P::CacheRef | P::CacheSet => roles.push(("lowering", "cache-cell")),
        P::SlotRef | P::SlotSet => roles.push(("lowering", "slot")),
        P::CaseLambda => roles.push(("lowering", "case-lambda")),
        _ => {}
    }
    roles
}

/// `(%primitive-table)`: what the compiler asks of each primitive in
/// `runtime/prims/def.rs`, as an alist `(name . fields)` in primitive id order..
///
/// `fields` is an alist:
///
/// - `(id . n)`: `Primitive::id()`, the operand of `call-primitive*`.
/// - `(arity . n)`: the argument count of the direct wrapper, `-1` for a variadic primitive.
/// - `(branch-fusable . bool)`: `Primitive::is_branch_fusable`, an `if` on a call of it
///   becomes a `branch-prim` term.
/// - `(effects . alist)`: `primitive_effects`: `flags`, `depends-on`, `causes` (lists of
///   symbols), `reads`, `writes` (lists of memory regions: `heap` `variables`
///   `continuations` `runtime` `unknown`), `effect-free` and `allocation-only` (booleans).
/// - `(fused-jump argc yes-opcode no-opcode)`: present when the branch lowering fuses the
///   test into one conditional jump, for calls with `argc` arguments.
/// - `(opcodes (argc . opcode) ...)`: the dedicated instruction chosen for calls with that
///   many arguments. `plus` and `minus` with a fixnum constant operand use `add-immediate`
///   instead, see `immediate`. Other argument counts, and primitives without this field, are
///   `call-primitive0`..`call-primitive3` or `call-primitive-n`.
/// - `(immediate . add-immediate)`: `+` (either operand) and `-` (second operand, negated)
///   with a constant fixnum operand lower to `add-immediate`.
/// - `(unspecified-result . #t)`: the instruction produces no value; the result is
///   `#<unspecified>`.
/// - `(not-branch . #t)`: (`not` only) a branch on it tests the operand's falsiness.
/// - `(roles (kind . what) ...)`: other passes that treat the primitive specially:
///   `rest` `cdr`/`ref`/`length`/`null?`/`pair?`/`list?` (rest-list rewrites in `cfg/rest.scm`),
///   `switch` `eq`/`fixnum`/`numeric`/`char-scrutinee` (`cfg/switch.scm`), `lowering`
///   `cache-cell`/`slot`/`case-lambda` (`cfg/lower.scm`).
pub fn primitive_table<'gc>(ctx: MutationContext<'gc>) -> Value<'gc> {
    let mut entries = Vec::new();
    for prim in Primitive::ALL {
        let name = prim.name();
        let mut fields = vec![
            pair(ctx, "id", Value::new(i32::from(prim.id()))),
            pair(
                ctx,
                "arity",
                Value::new(i32::from(
                    PRIM_ARITY.get(prim.id() as usize).copied().unwrap_or(-1),
                )),
            ),
            pair(ctx, "branch-fusable", Value::new(prim.is_branch_fusable())),
            pair(ctx, "effects", effects_alist(ctx, primitive_effects(*prim))),
        ];
        if let Some((argc, yes, no)) = fused_jump(*prim) {
            fields.push(pair(
                ctx,
                "fused-jump",
                list(ctx, &[Value::new(argc as i32), sym(ctx, yes), sym(ctx, no)]),
            ));
        }
        let dedicated = dedicated_opcodes(*prim);
        if !dedicated.is_empty() {
            let rows: Vec<_> = dedicated
                .iter()
                .map(|(argc, opcode)| Value::cons(ctx, Value::new(*argc as i32), sym(ctx, opcode)))
                .collect();
            fields.push(pair(ctx, "opcodes", list(ctx, &rows)));
        }
        if matches!(prim, Primitive::Plus | Primitive::Minus) {
            fields.push(pair(ctx, "immediate", sym(ctx, "add-immediate")));
        }
        if result_is_unspecified(*prim) {
            fields.push(pair(ctx, "unspecified-result", Value::new(true)));
        }
        if *prim == Primitive::Not {
            fields.push(pair(ctx, "not-branch", Value::new(true)));
        }
        let roles = cfg_role(*prim);
        if !roles.is_empty() {
            let rows: Vec<_> = roles
                .iter()
                .map(|(kind, what)| Value::cons(ctx, sym(ctx, kind), sym(ctx, what)))
                .collect();
            fields.push(pair(ctx, "roles", list(ctx, &rows)));
        }
        entries.push(Value::cons(ctx, sym(ctx, name), list(ctx, &fields)));
    }
    list(ctx, &entries)
}
