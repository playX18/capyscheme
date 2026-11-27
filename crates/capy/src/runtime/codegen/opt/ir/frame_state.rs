//! Frame states: the bytecode frames a deopt or a call rebuilds.

use cranelift_entity::entity_impl;

use super::func::{Func, Value};
use super::inst::RematKind;
use super::types::CodeRef;

/// A frame state.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FrameState(u32);
entity_impl!(FrameState, "frame");

/// One bytecode frame at a deopt point or a call site.
///
/// An inlined frame has a `parent`, the frame of its caller at the call site.
/// The outermost frame has none.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FrameStateData {
    pub code: CodeRef,
    /// The bytecode pc to resume at.
    pub pc: u32,
    /// The closure whose free variables the frame reads.
    pub closure: Value,
    /// Live bytecode registers and the values that hold them, in register
    /// order. Dead registers are left out.
    pub registers: Vec<(u32, Value)>,
    pub parent: Option<FrameState>,
    /// For a frame with a `parent`: the base register of the call in the
    /// parent's frame, so this frame starts at the parent's `fp + 8 * base`.
    pub call_base: Option<u32>,
    /// Set when the frame resumes at a multiple-value return point (a
    /// `return_point_values*` entry): the rebuilt frame resumes there with
    /// the callee's frame pushed again.
    pub returned: Option<ReturnedValues>,
    /// The rest layout of the frame's variadic procedure, which names the
    /// pseudo registers in `registers`: they rebuild the rest area.
    pub rest: Option<super::func::RestInfo>,
    /// For a frame of an inlined body: the call site in its caller. The
    /// outermost frame has none.
    pub inline_site: Option<CallSite>,
    /// Objects that do not exist at this point (escape analysis removed
    /// their allocation) and the field values they hold *here*. A value in
    /// `closure`, in `registers` or in another entry's `fields` that names an
    /// entry's `alias` is rebuilt from the entry when the frame is. Entries
    /// of a frame are visible to the frames of its parent chain too: the
    /// innermost state of a chain lists every object that the chain names.
    /// See [`VirtualObject`] and `peval-plan/15-escape-analysis.md` §4.
    pub virtuals: Vec<VirtualObject>,
}

/// One object that a frame state describes field by field instead of by
/// a runtime value.
///
/// - F1: an alias named by a state is in `virtuals` of the state's chain, or
///   is defined by an instruction that dominates the state. The only
///   instruction an alias may stand for without existing at run time is an
///   [`Op::Remat`](super::inst::Op::Remat) placeholder.
/// - F2: `fields` are the object's contents at this state.
/// - F3: two registers that held one object name one alias, so a deopt
///   allocates it once and `eq?` holds afterwards.
/// - F4: `fields` count as uses of the state, so the optimizer keeps them
///   live.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct VirtualObject {
    /// The value that stands for the object. It has no runtime value here.
    pub alias: Value,
    pub kind: RematKind,
    /// `kind.field_count()` values: the fields in the order `RematKind`
    /// documents. A field may itself be the alias of another entry.
    pub fields: Vec<Value>,
}

impl FrameStateData {
    /// Applies `f` to every value the state reads: closure, registers, and
    /// the aliases and fields of its virtual objects.
    pub fn map_values(&mut self, mut f: impl FnMut(Value) -> Value) {
        self.closure = f(self.closure);
        for (_, value) in self.registers.iter_mut() {
            *value = f(*value);
        }
        for object in self.virtuals.iter_mut() {
            object.alias = f(object.alias);
            for field in object.fields.iter_mut() {
                *field = f(*field);
            }
        }
    }
}

/// The call that inlined a body into its caller, for deopt diagnostics.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct CallSite {
    /// The bytecode pc of the call instruction in the caller's frame.
    pub call_pc: u32,
    /// The caller's code block, as a [`CodeRef`] in the function that owns
    /// the frame.
    pub caller: CodeRef,
}

/// How a deopt into a multiple-value entry hands over the callee's values.
///
/// Baseline's entry pops the callee frame (`SFP = fp + 8 * base`), compares
/// `AC` with its value count, and reads the values at `r[base + 2..]`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ReturnedValues {
    /// The callee frame's base register in the caller's frame.
    pub base: u32,
    /// The value count: `Some` when it is known at build time (the values
    /// are then named by the frame's registers), `None` when the callee
    /// returned it in `AC` and the values sit in the callee frame.
    pub count: Option<u32>,
}

impl Func<'_> {
    /// Every value that `frame` and its parents read, innermost frame first:
    /// the values that go into the rebuilt frames (see
    /// [`Func::frame_placed_values`]) and the fields of the virtual objects
    /// of the chain. An alias is not listed unless a register or a field
    /// names it, and it has no runtime value either way.
    pub fn frame_values(&self, frame: FrameState) -> Vec<Value> {
        let mut values = self.frame_placed_values(frame);
        let mut cur = Some(frame);
        while let Some(fs) = cur {
            let data = &self.frames[fs];
            for object in &data.virtuals {
                values.extend(object.fields.iter().copied());
            }
            cur = data.parent;
        }
        values
    }

    /// The values that `frame` and its parents write into the rebuilt
    /// frames, innermost frame first, each frame's closure before its
    /// registers. This is the order of `deopt_dests`.
    pub fn frame_placed_values(&self, frame: FrameState) -> Vec<Value> {
        let mut values = Vec::new();
        let mut cur = Some(frame);
        while let Some(fs) = cur {
            let data = &self.frames[fs];
            values.push(data.closure);
            values.extend(data.registers.iter().map(|&(_, v)| v));
            cur = data.parent;
        }
        values
    }

    /// The virtual object that `alias` names in the chain of `frame`.
    pub fn virtual_of(&self, frame: FrameState, alias: Value) -> Option<&VirtualObject> {
        let mut cur = Some(frame);
        while let Some(fs) = cur {
            let data = &self.frames[fs];
            if let Some(object) = data.virtuals.iter().find(|o| o.alias == alias) {
                return Some(object);
            }
            cur = data.parent;
        }
        None
    }
}
