use std::sync::LazyLock;

use crate::cps::graph::{
    intrinsic::Intrinsic,
    term::{Expression, Graph, OccurrenceId},
};

bitflags::bitflags! {

    pub struct EffectKinds: i32 {
        /// Indicates that an expression may cause a type check.  A type check,
        /// for the purposes of this analysis, is the possibility of throwing
        /// an exception the first time an expression is evaluated.  If the
        /// expression did not cause an exception to be thrown, users can
        /// assume that evaluating the expression again will not cause an
        /// exception to be thrown.
        ///
        /// For example, (+ x y) might throw if X or Y are not numbers.  But if
        /// it doesn't throw, it should be safe to elide a dominated, common
        /// subexpression (+ x y).
        const TYPE_CHECK = 1 << 0;
        /// Indicates that an expression may return a fresh object.  The kind
        /// of object is indicated in the object kind field.
        const ALLOCATION = 1 << 1;

        /// Indicates that an expression may cause a read from memory.  The
        /// kind of memory is given in the object kind field.  Some object
        /// kinds have finer-grained fields; those are expressed in the "field"
        /// part of the effects value.  -1 indicates "the whole object".
        const READ = 1 << 2;
        /// Indicates that an expression may cause a write to memory.
        const WRITE = 1 << 3;
    }
}
pub const TYPE_CHECK: i32 = EffectKinds::TYPE_CHECK.bits() as i32;
pub const ALLOCATION: i32 = EffectKinds::ALLOCATION.bits() as i32;

pub const EFFECT_KINDS_BITS: u8 = 4;
pub const ALL_EFFECT_KINDS: i32 = (1 << EFFECT_KINDS_BITS) - 1;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MemoryKind {
    Unknown,
    Fluid,
    Pair,
    Vector,
    Box,
    Module,
    Thread,
    Tuple,
    String,
    ByteVector,
    Closure,
    BitMask,
    Cache,
    Symbol,
    Header,
}

impl MemoryKind {
    pub const COUNT: usize = Self::Header as usize + 1;
    pub const MASK: i32 = (1 << 4) - 1;
    pub const SHIFT: i32 = 4;
    pub const BITS: i32 = 4;
}

pub const fn field(kind: MemoryKind, field: i32) -> i32 {
    ((field << MemoryKind::BITS) | (kind as i32)) << EFFECT_KINDS_BITS
}

pub const fn object(kind: MemoryKind) -> i32 {
    field(kind, -1)
}

pub const fn allocation(kind: MemoryKind) -> i32 {
    object(kind) | EffectKinds::ALLOCATION.bits() as i32
}

pub const fn read_field(kind: MemoryKind, fld: i32) -> i32 {
    field(kind, fld) | EffectKinds::READ.bits() as i32
}

pub const fn read_object(kind: MemoryKind) -> i32 {
    object(kind) | EffectKinds::READ.bits() as i32
}

pub const fn write_field(kind: MemoryKind, fld: i32) -> i32 {
    field(kind, fld) | EffectKinds::WRITE.bits() as i32
}

pub const fn write_object(kind: MemoryKind) -> i32 {
    object(kind) | EffectKinds::WRITE.bits() as i32
}

pub const NO_EFFECTS: i32 = 0;

pub const ALL_EFFECTS: i32 = ALL_EFFECT_KINDS | object(MemoryKind::Unknown);

pub const fn causes_effect(x: i32, effects: i32) -> bool {
    (x & effects) != 0
}

pub const fn causes_all_effects(x: i32) -> bool {
    x == ALL_EFFECTS
}

pub const fn effect_clobbers(a: i32, b: i32) -> bool {
    const fn locations_same(a: i32, b: i32) -> bool {
        let a = a >> EFFECT_KINDS_BITS;
        let b = b >> EFFECT_KINDS_BITS;
        (a & MemoryKind::MASK) == MemoryKind::Unknown as i32
            || (b & MemoryKind::MASK) == MemoryKind::Unknown as i32
            || ((a & MemoryKind::MASK) == (b & MemoryKind::MASK) && (a < 0 || b < 0 || a == b))
    }

    causes_effect(a, EffectKinds::WRITE.bits())
        && causes_effect(b, EffectKinds::READ.bits() | EffectKinds::WRITE.bits())
        && locations_same(a, b)
}

static PRIMITIVE_EFFECTS: LazyLock<
    [Option<Box<dyn for<'gc> Fn(&Graph<'gc>, &[OccurrenceId]) -> i32>>; Intrinsic::_Count as usize],
> = {
    let mut arr: [Option<Box<dyn for<'gc> Fn(&Graph<'gc>, &[OccurrenceId]) -> i32>>; _] =
        [None; Intrinsic::_Count as usize];

    macro_rules! define_primitive_effect {
        ($((($name: ident $($arg: ident)*) $($effect: expr),*))*) => {
            $(
                let clos = Box::new(|graph: &Graph<'_>, occs: &[OccurrenceId]| {
                    let args = graph.occ_pool.slice(occs);
                    $($effect|)* 0
                });
                arr[Intrinsic::$name as usize] = Some(clos);
            )*
        };
    }

    define_primitive_effect! {
        ((IsEq x y))
        ((IsEqual x y))
        ((IsBoolean x))
        ((IsBytevector x))
        ((IsChar x))
        ((IsComplex x))
        ((IsEofObject x))
        ((IsEqv x))
        ((IsFixnum x))
        ((IsImmediate x))
        ((IsHeapObject x))
        ((IsList x))
        ((IsNumber x))
        ((IsPair x))
        ((IsVector x))
        ((IsString x))
        ((IsSymbol x))
        ((IsProcedure x))
        ((IsVariable x))
        ((IsTuple x))
        ((IsNull x))
        ((IsUnspecified x))
    }

    define_primitive_effect! {
        ((SymbolToString x) read_object(MemoryKind::Symbol), EffectKinds::TYPE_CHECK.bits())
        ((StringToSymbol x) read_object(MemoryKind::String), EffectKinds::TYPE_CHECK.bits())

        ((VectorRef vec idx) read_object(MemoryKind::Vector))
        ((VectorSet vec idx val) write_object(MemoryKind::Vector))

        ((TupleRef tuple idx) read_object(MemoryKind::Tuple))
        ((TupleSet tuple idx val) write_object(MemoryKind::Tuple))

        ((VariableBound var) read_object(MemoryKind::Box))
        ((VariableRef var) read_object(MemoryKind::Box))
        ((VariableSet var val) write_object(MemoryKind::Box))

        ((Cons car cdr) allocation(MemoryKind::Pair))
        ((Car pair) read_object(MemoryKind::Pair))
        ((Cdr pair) read_object(MemoryKind::Pair))
        ((SetCar pair val) write_object(MemoryKind::Pair))
        ((SetCdr pair val) write_object(MemoryKind::Pair))

        ((Tuple size) allocation(MemoryKind::Tuple))
        ((MakeTuple size init) allocation(MemoryKind::Tuple))
        ((TupleSize tuple) read_object(MemoryKind::Tuple))
        ((StringLength str) read_object(MemoryKind::String))
        ((StringRef str idx) read_object(MemoryKind::String))
        ((StringSet str idx val) write_object(MemoryKind::String))

    }

    arr
};

impl<'gc> Graph<'gc> {
    fn expression_effects(expr: Expression) -> i32 {
        match expr {
            Expression::Constant(_) => NO_EFFECTS,
            Expression::PrimCall(intrinsic, args) => match intrinsic {
                Intrinsic::Abs | Intrinsic::Ash | Intrinsic::Asin | Intrinsic::Acos => 0,
            },
        }
    }
}
