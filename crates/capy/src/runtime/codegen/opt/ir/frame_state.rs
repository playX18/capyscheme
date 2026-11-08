//! Frame states: the bytecode frames a deopt or a call rebuilds.

use cranelift_entity::entity_impl;

use super::func::{Func, Value};
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
    /// Every value that `frame` and its parents read, innermost frame first.
    pub fn frame_values(&self, frame: FrameState) -> Vec<Value> {
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
}
