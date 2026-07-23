//! Global CFG effect analysis.

use super::{BlockId, Instruction, Procedure, Terminator, ValueId};
use crate::compiler::cranelift::primitive::Primitive;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
#[repr(transparent)]
pub struct MemoryRanges(u16);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum MemoryRegion {
    Heap = 0,
    Variables = 1,
    Modules = 2,
    Continuations = 3,
    Runtime = 4,
    Unknown = 5,
}

impl MemoryRanges {
    pub const NONE: Self = Self(0);
    pub const TOP: Self = Self((1 << (MemoryRegion::Unknown as u8 + 1)) - 1);

    pub const fn region(region: MemoryRegion) -> Self {
        Self(1 << region as u8)
    }

    pub const fn is_empty(self) -> bool {
        self.0 == 0
    }

    pub const fn contains(self, effects: Self) -> bool {
        self.0 & effects.0 == effects.0
    }

    pub const fn overlaps(self, other: Self) -> bool {
        self.0 & other.0 != 0
    }

    pub const fn union(self, other: Self) -> Self {
        Self(self.0 | other.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
#[repr(transparent)]
pub struct EffectFlags(u16);

impl EffectFlags {
    pub const NONE: Self = Self(0);
    pub const TERMINAL: Self = Self(1 << 0);
    pub const EXITS_SIDEWAYS: Self = Self(1 << 1);
    pub const CONTROL_DEPENDENT: Self = Self(1 << 2);
    pub const READS_MUTABLE: Self = Self(1 << 3);
    pub const WRITES_LOCAL_STATE: Self = Self(1 << 4);
    pub const READS_LOCAL_STATE: Self = Self(1 << 5);
    pub const READS_PINNED: Self = Self(1 << 6);
    pub const WRITES_PINNED: Self = Self(1 << 7);
    pub const FENCE: Self = Self(1 << 8);
    pub const MAY_CALL: Self = Self(1 << 9);
    pub const MAY_RAISE: Self = Self(1 << 10);
    pub const ALLOCATES: Self = Self(1 << 11);

    pub const fn contains(self, flags: Self) -> bool {
        self.0 & flags.0 == flags.0
    }

    pub const fn union(self, other: Self) -> Self {
        Self(self.0 | other.0)
    }
}

/// Semantic effects: the low-level memory ranges above describe aliasing;
/// these bits describe semantic dependencies and observable causes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
#[repr(transparent)]
pub struct SemanticEffects(u32);

impl SemanticEffects {
    pub const NONE: Self = Self(0);
    pub const MUTABLE_LEXICAL: Self = Self(1 << 0);
    pub const TOPLEVEL: Self = Self(1 << 1);
    pub const FLUID: Self = Self(1 << 2);
    pub const DEFINITE_BAILOUT: Self = Self(1 << 3);
    pub const POSSIBLE_BAILOUT: Self = Self(1 << 4);
    pub const ZERO_VALUES: Self = Self(1 << 5);
    pub const ALLOCATION: Self = Self(1 << 6);
    pub const TYPE_CHECK: Self = Self(1 << 7);
    pub const CAR: Self = Self(1 << 8);
    pub const CDR: Self = Self(1 << 9);
    pub const VECTOR: Self = Self(1 << 10);
    pub const VARIABLE: Self = Self(1 << 11);
    pub const STRING: Self = Self(1 << 12);
    pub const BYTEVECTOR: Self = Self(1 << 13);
    pub const UNKNOWN: Self = Self(1 << 14);
    pub const ALL: Self = Self((1 << 15) - 1);

    pub const fn is_empty(self) -> bool {
        self.0 == 0
    }

    pub const fn contains(self, effects: Self) -> bool {
        self.0 & effects.0 == effects.0
    }

    pub const fn union(self, other: Self) -> Self {
        Self(self.0 | other.0)
    }

    pub const fn without(self, other: Self) -> Self {
        Self(self.0 & !other.0)
    }

    pub const fn depends_on(self, effects: Self) -> bool {
        self.contains(effects)
    }

    pub const fn causes(self, effects: Self) -> bool {
        self.contains(effects)
    }

    pub const fn overlaps(self, other: Self) -> bool {
        self.0 & other.0 != 0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct Effects {
    pub flags: EffectFlags,
    pub depends_on: SemanticEffects,
    pub causes: SemanticEffects,
    pub reads: MemoryRanges,
    pub writes: MemoryRanges,
}

impl Effects {
    pub const fn pure() -> Self {
        Self {
            flags: EffectFlags::NONE,
            depends_on: SemanticEffects::NONE,
            causes: SemanticEffects::NONE,
            reads: MemoryRanges::NONE,
            writes: MemoryRanges::NONE,
        }
    }

    pub const fn for_call() -> Self {
        Self {
            flags: EffectFlags::EXITS_SIDEWAYS
                .union(EffectFlags::CONTROL_DEPENDENT)
                .union(EffectFlags::READS_MUTABLE)
                .union(EffectFlags::READS_PINNED)
                .union(EffectFlags::WRITES_PINNED)
                .union(EffectFlags::MAY_CALL)
                .union(EffectFlags::MAY_RAISE)
                .union(EffectFlags::ALLOCATES),
            depends_on: SemanticEffects::ALL,
            causes: SemanticEffects::ALL.without(SemanticEffects::DEFINITE_BAILOUT),
            reads: MemoryRanges::TOP,
            writes: MemoryRanges::TOP,
        }
    }

    pub const fn raise() -> Self {
        Self {
            flags: EffectFlags::TERMINAL
                .union(EffectFlags::EXITS_SIDEWAYS)
                .union(EffectFlags::MAY_RAISE),
            depends_on: SemanticEffects::NONE,
            causes: SemanticEffects::DEFINITE_BAILOUT.union(SemanticEffects::POSSIBLE_BAILOUT),
            reads: MemoryRanges::region(MemoryRegion::Continuations),
            writes: MemoryRanges::NONE,
        }
    }

    pub const fn read(region: MemoryRegion, semantic: SemanticEffects) -> Self {
        Self {
            flags: EffectFlags::CONTROL_DEPENDENT.union(EffectFlags::READS_MUTABLE),
            depends_on: semantic,
            causes: SemanticEffects::NONE,
            reads: MemoryRanges::region(region),
            writes: MemoryRanges::NONE,
        }
    }

    pub const fn checked(mut self) -> Self {
        self.causes = self.causes.union(SemanticEffects::TYPE_CHECK);
        self.flags = self.flags.union(EffectFlags::MAY_RAISE);
        self
    }

    pub const fn write(region: MemoryRegion, semantic: SemanticEffects) -> Self {
        Self {
            flags: EffectFlags::NONE,
            depends_on: SemanticEffects::NONE,
            causes: semantic,
            reads: MemoryRanges::NONE,
            writes: MemoryRanges::region(region),
        }
    }

    pub const fn checked_write(mut self) -> Self {
        self.causes = self.causes.union(SemanticEffects::TYPE_CHECK);
        self.flags = self.flags.union(EffectFlags::MAY_RAISE);
        self
    }

    pub const fn allocate() -> Self {
        Self {
            flags: EffectFlags::ALLOCATES,
            depends_on: SemanticEffects::NONE,
            causes: SemanticEffects::ALLOCATION,
            reads: MemoryRanges::NONE,
            writes: MemoryRanges::region(MemoryRegion::Heap),
        }
    }

    pub const fn union(self, other: Self) -> Self {
        Self {
            flags: self.flags.union(other.flags),
            depends_on: self.depends_on.union(other.depends_on),
            causes: self.causes.union(other.causes),
            reads: self.reads.union(other.reads),
            writes: self.writes.union(other.writes),
        }
    }

    /// Remove causes that are known to be irrelevant to a consumer.
    pub const fn exclude_causes(mut self, effects: SemanticEffects) -> Self {
        self.causes = self.causes.without(effects);
        self
    }

    /// True when evaluating the operation has no observable cause and can be
    /// discarded when its result is unused.  Dependencies alone do not force
    /// execution; this is the distinction needed by letrectify/DCE.
    pub const fn effect_free(self) -> bool {
        self.causes.is_empty()
            && !self.flags.contains(
                EffectFlags::TERMINAL
                    .union(EffectFlags::EXITS_SIDEWAYS)
                    .union(EffectFlags::MAY_CALL)
                    .union(EffectFlags::MAY_RAISE)
                    .union(EffectFlags::WRITES_LOCAL_STATE)
                    .union(EffectFlags::WRITES_PINNED)
                    .union(EffectFlags::FENCE),
            )
            && self.writes.is_empty()
    }

    pub const fn must_execute(self) -> bool {
        !self.effect_free()
    }

    pub const fn interferes(self, other: Self) -> bool {
        (self.flags.contains(EffectFlags::TERMINAL)
            && (other.flags.contains(EffectFlags::TERMINAL)
                || other.flags.contains(EffectFlags::CONTROL_DEPENDENT)
                || !other.writes.is_empty()))
            || (other.flags.contains(EffectFlags::TERMINAL)
                && (self.flags.contains(EffectFlags::CONTROL_DEPENDENT) || !self.writes.is_empty()))
            || (self.flags.contains(EffectFlags::EXITS_SIDEWAYS)
                && (!other.writes.is_empty()
                    || other.flags.contains(EffectFlags::CONTROL_DEPENDENT)))
            || (other.flags.contains(EffectFlags::EXITS_SIDEWAYS)
                && (!self.writes.is_empty() || self.flags.contains(EffectFlags::CONTROL_DEPENDENT)))
            || (self.flags.contains(EffectFlags::WRITES_LOCAL_STATE)
                && (other.flags.contains(EffectFlags::WRITES_LOCAL_STATE)
                    || other.flags.contains(EffectFlags::READS_LOCAL_STATE)))
            || (other.flags.contains(EffectFlags::WRITES_LOCAL_STATE)
                && (self.flags.contains(EffectFlags::WRITES_LOCAL_STATE)
                    || self.flags.contains(EffectFlags::READS_LOCAL_STATE)))
            || (self.flags.contains(EffectFlags::WRITES_PINNED)
                && (other.flags.contains(EffectFlags::WRITES_PINNED)
                    || other.flags.contains(EffectFlags::READS_PINNED)))
            || (other.flags.contains(EffectFlags::WRITES_PINNED)
                && (self.flags.contains(EffectFlags::WRITES_PINNED)
                    || self.flags.contains(EffectFlags::READS_PINNED)))
            || self.writes.overlaps(other.writes.union(other.reads))
            || other.writes.overlaps(self.writes.union(self.reads))
            || self.causes.overlaps(other.depends_on)
            || other.causes.overlaps(self.depends_on)
            || (self.flags.contains(EffectFlags::FENCE) && other.flags.contains(EffectFlags::FENCE))
    }
}

#[derive(Debug, Clone)]
pub struct BlockEffects {
    pub instructions: Vec<Effects>,
    pub terminator: Effects,
}

#[derive(Debug, Clone, Default)]
pub struct ProcedureEffects {
    pub blocks: HashMap<BlockId, BlockEffects>,
}

/// Runs the on-demand pass and returns one effect fact per CFG instruction and
/// terminator.  No procedure-wide summary is inferred here: callers that need
/// interprocedural information can build it explicitly from these facts.
pub fn analyze_procedure(procedure: &Procedure<'_>) -> ProcedureEffects {
    let blocks = procedure
        .blocks
        .iter()
        .map(|block| {
            (
                block.id,
                BlockEffects {
                    instructions: block
                        .instructions
                        .iter()
                        .map(|instruction| instruction_effects(instruction))
                        .collect(),
                    terminator: terminator_effects(&block.terminator),
                },
            )
        })
        .collect();
    ProcedureEffects { blocks }
}

/// Removes definitions whose values are unused and whose instructions have no
/// observable effects.
pub fn eliminate_dead_effect_free_instructions(mut procedure: Procedure<'_>) -> Procedure<'_> {
    loop {
        let mut uses = HashMap::<ValueId, usize>::new();
        for block in &procedure.blocks {
            for instruction in &block.instructions {
                for operand in instruction.uses() {
                    if let super::Operand::Local(value) = operand {
                        *uses.entry(value).or_default() += 1;
                    }
                }
            }
            for operand in block.terminator.uses() {
                if let super::Operand::Local(value) = operand {
                    *uses.entry(value).or_default() += 1;
                }
            }
        }

        let mut changed = false;
        for block in &mut procedure.blocks {
            block.instructions.retain(|instruction| {
                let removable = instruction
                    .def()
                    .is_some_and(|value| uses.get(&value).copied().unwrap_or(0) == 0)
                    && instruction_effects(instruction).effect_free();
                changed |= removable;
                !removable
            });
        }
        if !changed {
            return procedure;
        }
    }
}

pub fn instruction_effects(instruction: &Instruction<'_>) -> Effects {
    match instruction {
        Instruction::Assign { .. } | Instruction::Const { .. } => Effects::pure(),
        Instruction::MakeClosure { .. } | Instruction::RestToList { .. } => Effects::allocate(),
        Instruction::ClosureRef { .. } | Instruction::CacheRef { .. } => {
            Effects::read(MemoryRegion::Heap, SemanticEffects::UNKNOWN)
        }
        Instruction::ClosureSet { .. } | Instruction::CacheSet { .. } => {
            Effects::write(MemoryRegion::Heap, SemanticEffects::UNKNOWN)
        }
        Instruction::PrimCall { prim, .. } => primitive_effects(*prim),
        Instruction::RestRef { .. }
        | Instruction::RestLength { .. }
        | Instruction::RestPredicate { .. } => {
            Effects::read(MemoryRegion::Heap, SemanticEffects::UNKNOWN).checked()
        }
    }
}

pub fn terminator_effects(terminator: &Terminator<'_>) -> Effects {
    match terminator {
        Terminator::Raise { .. } => Effects::raise(),
        Terminator::Call { .. } | Terminator::TailCall { .. } => Effects::for_call(),
        Terminator::Jump { .. }
        | Terminator::Branch { .. }
        | Terminator::BranchPrim { .. }
        | Terminator::Switch { .. } => Effects::pure(),
    }
}

pub fn primitive_effects(primitive: Primitive) -> Effects {
    match primitive {
        Primitive::IsProcedure
        | Primitive::IsVariable
        | Primitive::IsEofObject
        | Primitive::IsNull
        | Primitive::IsPair
        | Primitive::IsList
        | Primitive::IsVector
        | Primitive::IsBytevector
        | Primitive::IsString
        | Primitive::IsBoolean
        | Primitive::IsSymbol
        | Primitive::IsEq
        | Primitive::IsEqv
        | Primitive::IsEqual
        | Primitive::IsExactInteger
        | Primitive::IsInteger
        | Primitive::IsChar
        | Primitive::IsNumber
        | Primitive::IsComplex
        | Primitive::IsNan
        | Primitive::IsReal
        | Primitive::IsRational
        | Primitive::IsInexact
        | Primitive::IsExact
        | Primitive::IsFixnum
        | Primitive::IsFlonum
        | Primitive::Not
        | Primitive::FxEqUUnchecked
        | Primitive::FxAddUnchecked
        | Primitive::FxSubUnchecked
        | Primitive::FxMulUnchecked
        | Primitive::FxLtUnchecked
        | Primitive::FxLeUnchecked
        | Primitive::FxGtUnchecked
        | Primitive::FxGeUnchecked
        | Primitive::FxAndUnchecked
        | Primitive::FxIorUnchecked
        | Primitive::FxXorUnchecked
        | Primitive::FxNotUnchecked
        | Primitive::FxAshlUnchecked
        | Primitive::FxAshrUnchecked
        | Primitive::FxZeroUnchecked
        | Primitive::FxPositiveUnchecked
        | Primitive::FxNegativeUnchecked
        | Primitive::FxOddUnchecked
        | Primitive::FxEvenUnchecked
        | Primitive::FxMinUnchecked
        | Primitive::FxMaxUnchecked
        | Primitive::FxQuotient
        | Primitive::FxRemainder
        | Primitive::FxModulo
        | Primitive::FlAddUnchecked
        | Primitive::FlSubUnchecked
        | Primitive::FlMulUnchecked
        | Primitive::FlDivUnchecked
        | Primitive::FlLtUnchecked
        | Primitive::FlLeUnchecked
        | Primitive::FlGtUnchecked
        | Primitive::FlGeUnchecked
        | Primitive::FlEqUnchecked
        | Primitive::FlZeroUnchecked
        | Primitive::FlPositiveUnchecked
        | Primitive::FlNegativeUnchecked
        | Primitive::FlNanUnchecked
        | Primitive::FlInfiniteUnchecked
        | Primitive::FlFiniteUnchecked
        | Primitive::FlMinUnchecked
        | Primitive::FlMaxUnchecked
        | Primitive::FlAbsUnchecked
        | Primitive::FlFloorUnchecked
        | Primitive::FlCeilingUnchecked
        | Primitive::FlTruncateUnchecked
        | Primitive::FlRoundUnchecked
        | Primitive::FlSinUnchecked
        | Primitive::FlCosUnchecked
        | Primitive::FlTanUnchecked
        | Primitive::FlExpUnchecked
        | Primitive::FlLogUnchecked
        | Primitive::FlAsinUnchecked
        | Primitive::FlAcosUnchecked
        | Primitive::FlSqrtUnchecked
        | Primitive::FlAtanUnchecked
        | Primitive::CharToIntUnchecked => Effects::pure(),
        // Checked fixnum/flonum ops: may raise on wrong type.
        Primitive::FxAdd
        | Primitive::FxSub
        | Primitive::FxMul
        | Primitive::FxLt
        | Primitive::FxLe
        | Primitive::FxGt
        | Primitive::FxGe
        | Primitive::FxEq
        | Primitive::FxAnd
        | Primitive::FxIor
        | Primitive::FxXor
        | Primitive::FxNot
        | Primitive::FxAshl
        | Primitive::FxAshr
        | Primitive::FxZero
        | Primitive::FxPositive
        | Primitive::FxNegative
        | Primitive::FxOdd
        | Primitive::FxEven
        | Primitive::FxMin
        | Primitive::FxMax
        | Primitive::FlAdd
        | Primitive::FlSub
        | Primitive::FlMul
        | Primitive::FlDiv
        | Primitive::FlLt
        | Primitive::FlLe
        | Primitive::FlGt
        | Primitive::FlGe
        | Primitive::FlEq
        | Primitive::FlZero
        | Primitive::FlPositive
        | Primitive::FlNegative
        | Primitive::FlNan
        | Primitive::FlInfinite
        | Primitive::FlFinite
        | Primitive::FlMin
        | Primitive::FlMax
        | Primitive::FlAbs
        | Primitive::FlFloor
        | Primitive::FlCeiling
        | Primitive::FlTruncate
        | Primitive::FlRound
        | Primitive::FlSin
        | Primitive::FlCos
        | Primitive::FlTan
        | Primitive::FlExp
        | Primitive::FlLog
        | Primitive::FlAsin
        | Primitive::FlAcos
        | Primitive::FlSqrt
        | Primitive::FlAtan => Effects::pure().checked(),
        // Overflow-checked fixnum ops: `#f` on overflow (and raise if typed).
        Primitive::FxAddOvf | Primitive::FxSubOvf | Primitive::FxMulOvf => Effects {
            causes: SemanticEffects::POSSIBLE_BAILOUT.union(SemanticEffects::TYPE_CHECK),
            flags: EffectFlags::CONTROL_DEPENDENT.union(EffectFlags::MAY_RAISE),
            ..Effects::pure()
        },
        Primitive::FxAddOvfUnchecked
        | Primitive::FxSubOvfUnchecked
        | Primitive::FxMulOvfUnchecked => Effects {
            causes: SemanticEffects::POSSIBLE_BAILOUT,
            flags: EffectFlags::CONTROL_DEPENDENT,
            ..Effects::pure()
        },
        Primitive::Car => Effects::read(MemoryRegion::Heap, SemanticEffects::CAR).checked(),
        Primitive::Cdr => Effects::read(MemoryRegion::Heap, SemanticEffects::CDR).checked(),
        Primitive::CarUnchecked => Effects::read(MemoryRegion::Heap, SemanticEffects::CAR),
        Primitive::CdrUnchecked => Effects::read(MemoryRegion::Heap, SemanticEffects::CDR),
        Primitive::SetCar => {
            Effects::write(MemoryRegion::Heap, SemanticEffects::CAR).checked_write()
        }
        Primitive::SetCarUnchecked => Effects::write(MemoryRegion::Heap, SemanticEffects::CAR),
        Primitive::SetCdr => {
            Effects::write(MemoryRegion::Heap, SemanticEffects::CDR).checked_write()
        }
        Primitive::SetCdrUnchecked => Effects::write(MemoryRegion::Heap, SemanticEffects::CDR),
        Primitive::VectorRef => {
            Effects::read(MemoryRegion::Heap, SemanticEffects::VECTOR).checked()
        }
        Primitive::StringRef => {
            Effects::read(MemoryRegion::Heap, SemanticEffects::STRING).checked()
        }
        Primitive::BytevectorU8Ref => {
            Effects::read(MemoryRegion::Heap, SemanticEffects::BYTEVECTOR).checked()
        }
        Primitive::VectorRefUnchecked => Effects::read(MemoryRegion::Heap, SemanticEffects::VECTOR),
        Primitive::StringRefUnchecked => Effects::read(MemoryRegion::Heap, SemanticEffects::STRING),
        Primitive::BytevectorU8RefUnchecked => {
            Effects::read(MemoryRegion::Heap, SemanticEffects::BYTEVECTOR)
        }
        Primitive::VectorSet => {
            Effects::write(MemoryRegion::Heap, SemanticEffects::VECTOR).checked_write()
        }
        Primitive::VectorSetUnchecked => {
            Effects::write(MemoryRegion::Heap, SemanticEffects::VECTOR)
        }
        Primitive::VariableRef => {
            Effects::read(MemoryRegion::Variables, SemanticEffects::VARIABLE).checked()
        }
        Primitive::VariableSet => {
            Effects::write(MemoryRegion::Variables, SemanticEffects::VARIABLE)
        }
        Primitive::CacheRef => Effects::read(MemoryRegion::Runtime, SemanticEffects::UNKNOWN),
        Primitive::CacheSet => Effects::write(MemoryRegion::Runtime, SemanticEffects::UNKNOWN),
        Primitive::Cons
        | Primitive::List
        | Primitive::Vector
        | Primitive::Tuple
        | Primitive::MakeTuple
        | Primitive::MakeVector => Effects::allocate(),
        Primitive::Unspec | Primitive::IsUnspecified => Effects::pure(),
        Primitive::VariableBound => {
            Effects::read(MemoryRegion::Variables, SemanticEffects::VARIABLE).checked()
        }
        Primitive::Define => Effects::write(MemoryRegion::Modules, SemanticEffects::TOPLEVEL),
        Primitive::CurrentModule => Effects::read(MemoryRegion::Modules, SemanticEffects::TOPLEVEL),
        Primitive::Plus
        | Primitive::Minus
        | Primitive::Times
        | Primitive::Div
        | Primitive::NumericEqual
        | Primitive::NumericLt
        | Primitive::NumericGt
        | Primitive::NumericGte
        | Primitive::NumericLte
        | Primitive::Sqrt
        | Primitive::Atan
        | Primitive::Asin
        | Primitive::Acos
        | Primitive::Sin
        | Primitive::Cos
        | Primitive::Tan
        | Primitive::Floor
        | Primitive::Ceiling
        | Primitive::Truncate
        | Primitive::Quotient
        | Primitive::Remainder
        | Primitive::Modulo
        | Primitive::IsEven
        | Primitive::IsOdd
        | Primitive::IsZero => Effects {
            causes: SemanticEffects::TYPE_CHECK,
            flags: EffectFlags::MAY_RAISE,
            ..Effects::pure()
        },
        Primitive::Length => Effects::read(
            MemoryRegion::Heap,
            SemanticEffects::CAR.union(SemanticEffects::CDR),
        )
        .checked(),
        Primitive::StringLength => {
            Effects::read(MemoryRegion::Heap, SemanticEffects::STRING).checked()
        }
        Primitive::BytevectorLength => {
            Effects::read(MemoryRegion::Heap, SemanticEffects::BYTEVECTOR).checked()
        }
        _ => Effects::for_call(),
    }
}
