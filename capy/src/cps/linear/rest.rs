use super::graph::loop_blocks;
use super::*;
use crate::compiler::ssa::primitive::Primitive;
use std::collections::{HashMap, HashSet};
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct RestAlias {
    rest: ValueId,
    skip: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum RestRewrite {
    Cdr,
    Ref(RestAlias),
    Length(RestAlias),
    Predicate(RestAlias, RestPredicate),
}

pub(super) fn lower_rest_arguments<'gc>(mut procedure: Procedure<'gc>) -> Procedure<'gc> {
    let Some(rest) = procedure.variadic else {
        return procedure;
    };

    let aliases = collect_rest_aliases(&procedure, rest);
    let loop_blocks = loop_blocks(&procedure);
    if has_incompatible_rest_use(&procedure, rest, &aliases, &loop_blocks) {
        insert_rest_to_list(&mut procedure, rest);
    } else {
        rewrite_rest_uses(&mut procedure, rest, &aliases);
    }

    procedure
}

fn collect_rest_aliases<'gc>(
    procedure: &Procedure<'gc>,
    rest: ValueId,
) -> HashMap<ValueId, RestAlias> {
    let mut aliases = HashMap::new();
    let mut changed = true;

    while changed {
        changed = false;
        for block in &procedure.blocks {
            for instruction in &block.instructions {
                let Instruction::PrimCall {
                    dst, prim, args, ..
                } = instruction
                else {
                    continue;
                };
                if *prim != Primitive::cdr || args.len() != 1 {
                    continue;
                }
                if let Some(alias) = rest_alias_for_atom(args[0], rest, &aliases) {
                    let next_alias = RestAlias {
                        rest: alias.rest,
                        skip: alias.skip + 1,
                    };
                    if aliases.insert(*dst, next_alias) != Some(next_alias) {
                        changed = true;
                    }
                }
            }
        }
    }

    aliases
}

fn rest_alias_for_atom<'gc>(
    atom: LinearAtom<'gc>,
    rest: ValueId,
    aliases: &HashMap<ValueId, RestAlias>,
) -> Option<RestAlias> {
    let LinearAtom::Local(var) = atom else {
        return None;
    };
    if var == rest {
        return Some(RestAlias { rest, skip: 0 });
    }
    aliases.get(&var).copied()
}

fn rest_rewrite_for_prim<'gc>(
    prim: Primitive,
    args: &[LinearAtom<'gc>],
    rest: ValueId,
    aliases: &HashMap<ValueId, RestAlias>,
) -> Option<RestRewrite> {
    if args.len() != 1 {
        return None;
    }
    let alias = rest_alias_for_atom(args[0], rest, aliases)?;
    match prim {
        Primitive::cdr => Some(RestRewrite::Cdr),
        Primitive::car => Some(RestRewrite::Ref(alias)),
        Primitive::length => Some(RestRewrite::Length(alias)),
        Primitive::is_null => Some(RestRewrite::Predicate(alias, RestPredicate::Null)),
        Primitive::is_pair => Some(RestRewrite::Predicate(alias, RestPredicate::Pair)),
        Primitive::is_list => Some(RestRewrite::Predicate(alias, RestPredicate::List)),
        _ => None,
    }
}

fn instruction_mentions_rest<'gc>(
    instruction: &Instruction<'gc>,
    rest: ValueId,
    aliases: &HashMap<ValueId, RestAlias>,
) -> bool {
    instruction
        .uses()
        .iter()
        .any(|atom| rest_alias_for_atom(*atom, rest, aliases).is_some())
}

fn terminator_mentions_rest<'gc>(
    terminator: &Terminator<'gc>,
    rest: ValueId,
    aliases: &HashMap<ValueId, RestAlias>,
) -> bool {
    terminator
        .uses()
        .iter()
        .any(|atom| rest_alias_for_atom(*atom, rest, aliases).is_some())
}

fn has_incompatible_rest_use<'gc>(
    procedure: &Procedure<'gc>,
    rest: ValueId,
    aliases: &HashMap<ValueId, RestAlias>,
    loop_blocks: &HashSet<BlockId>,
) -> bool {
    for block in &procedure.blocks {
        let is_loop_block = loop_blocks.contains(&block.id);
        for instruction in &block.instructions {
            let mentions_rest = instruction_mentions_rest(instruction, rest, aliases);
            if !mentions_rest {
                continue;
            }
            if is_loop_block {
                return true;
            }
            let compatible = match instruction {
                Instruction::PrimCall { prim, args, .. } => {
                    rest_rewrite_for_prim(*prim, args, rest, aliases).is_some()
                }
                _ => false,
            };
            if !compatible {
                return true;
            }
        }

        if terminator_mentions_rest(&block.terminator, rest, aliases) {
            return true;
        }
    }

    false
}

fn insert_rest_to_list<'gc>(procedure: &mut Procedure<'gc>, rest: ValueId) {
    let entry = procedure
        .blocks
        .iter_mut()
        .find(|block| block.id == procedure.entry)
        .expect("procedure should contain entry block");
    entry.instructions.insert(
        0,
        Instruction::RestToList {
            dst: rest,
            rest,
            source: entry.source,
        },
    );
}

fn rewrite_rest_uses<'gc>(
    procedure: &mut Procedure<'gc>,
    rest: ValueId,
    aliases: &HashMap<ValueId, RestAlias>,
) {
    for block in &mut procedure.blocks {
        let mut lowered = Vec::with_capacity(block.instructions.len());
        for instruction in block.instructions.drain(..) {
            match instruction {
                Instruction::PrimCall {
                    dst,
                    prim,
                    args,
                    source,
                } => match rest_rewrite_for_prim(prim, &args, rest, aliases) {
                    Some(RestRewrite::Cdr) => {}
                    Some(RestRewrite::Ref(alias)) => lowered.push(Instruction::RestRef {
                        dst,
                        rest: alias.rest,
                        index: alias.skip,
                        source,
                    }),
                    Some(RestRewrite::Length(alias)) => lowered.push(Instruction::RestLength {
                        dst,
                        rest: alias.rest,
                        skip: alias.skip,
                        source,
                    }),
                    Some(RestRewrite::Predicate(alias, predicate)) => {
                        lowered.push(Instruction::RestPredicate {
                            dst,
                            rest: alias.rest,
                            predicate,
                            skip: alias.skip,
                            source,
                        })
                    }
                    None => lowered.push(Instruction::PrimCall {
                        dst,
                        prim,
                        args,
                        source,
                    }),
                },
                other => lowered.push(other),
            }
        }
        block.instructions = lowered;
    }
}
