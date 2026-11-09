//! Split and spill handling.
//!
//! TODO: Unused, if we add graph coloring/PBQP might be used.

use super::RegFile;
use super::assign::{Assignment, Pieces};
use super::intervals::Built;
use super::liveness::Numbering;
use super::weights::Weights;
use crate::runtime::codegen::opt::lir::LirFunc;

pub(crate) fn plan_split(
    func: &LirFunc,
    num: &Numbering,
    built: &Built,
    regs: &RegFile,
    weights: &Weights,
    pieces: Pieces,
    colored: &[usize],
) -> Result<Assignment, String> {
    super::greedy::refine(func, num, built, regs, weights, pieces, colored)
}
