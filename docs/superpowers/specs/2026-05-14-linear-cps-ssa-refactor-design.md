# Linear CPS and SSA Refactor Design

## Goal

Simplify the compiler by making linear CPS the only SSA input, deleting the old tree-CPS-to-SSA lowering path, and splitting the oversized linear CPS and SSA implementation files into focused modules.

## Scope

The refactor keeps current compiler behavior while changing organization and removing unreachable tree SSA code. The tree CPS IR remains as the frontend and optimization IR that feeds linearization. The removed path is only the old direct tree CPS lowering inside `capy/src/compiler/ssa`.

## Linear CPS Layout

`capy/src/cps/linear.rs` becomes `capy/src/cps/linear/` with a small `mod.rs` that owns the public API and pass pipeline. The IR types move to `ir.rs`. Tree-to-linear conversion moves to `builder.rs`. Passes move into focused files: `rest.rs`, `switch.rs`, `cache.rs`, `low_prim.rs`, `constant.rs`, and shared CFG helpers in `graph.rs`.

The public module path stays `crate::cps::linear`, so callers such as `linear_pretty`, `compiler::pipeline`, and `compiler::object` should require minimal import changes.

## SSA Layout

`capy/src/compiler/ssa` keeps only linear CPS lowering. Tree-only concepts are removed: `ContOrFunc::Func`, `ContOrFunc::Cont`, non-reified continuation block generation, `term`, `letk`, `fix`, `continue_to`, `blockmap`, and `to_generate`.

After the deletion, SSA is split by responsibility. `module.rs` owns `ModuleBuilder` and module-level data initialization. `builder.rs` owns `SSABuilder` state and construction/finalization. `linear.rs` owns linear instruction and terminator lowering. `switch.rs` owns Cranelift lowering for `Terminator::Switch`. Existing `helpers.rs`, `primitive.rs`, and `traits.rs` remain, with imports adjusted.

## Testing

This is a compile-preserving refactor. Existing tests for linearization, LCPS dumps, and SSA lowering should continue to pass. Verification should include at least `cargo test -p capy cps::linear`, `cargo test -p capy compiler::ssa`, and `cargo check -p capy`. If package names differ, use the closest existing cargo package command revealed by `cargo metadata` or `cargo test --package` output.
