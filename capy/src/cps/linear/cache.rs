use super::*;
use crate::compiler::ssa::primitive::Primitive;
pub(super) fn lower_cache_operations<'gc>(mut procedure: Procedure<'gc>) -> Procedure<'gc> {
    for block in &mut procedure.blocks {
        let mut lowered = Vec::with_capacity(block.instructions.len());
        for instruction in block.instructions.drain(..) {
            match instruction {
                Instruction::PrimCall {
                    dst,
                    prim: Primitive::cache_ref,
                    args,
                    source,
                } => {
                    let [cache_key] = args.as_slice() else {
                        panic!("cache-ref expects 1 argument, got {}", args.len());
                    };
                    lowered.push(Instruction::CacheRef {
                        dst,
                        cache_key: *cache_key,
                        source,
                    });
                }
                Instruction::PrimCall {
                    dst,
                    prim: Primitive::cache_set,
                    args,
                    source,
                } => {
                    let [cache_key, value] = args.as_slice() else {
                        panic!("cache-set! expects 2 arguments, got {}", args.len());
                    };
                    lowered.push(Instruction::CacheSet {
                        dst,
                        cache_key: *cache_key,
                        value: *value,
                        source,
                    });
                }
                other => lowered.push(other),
            }
        }
        block.instructions = lowered;
    }
    procedure
}
