//! Deoptimization: leaving Tier 2 code for baseline at a failed guard.
//!
//! - [`metadata`]: what the compiler records per exit (where each value is
//!   written when the frames are rebuilt) and the layout of the register save
//!   area that the shared `deopt_common` stub builds.
//! - [`runtime`]: [`capy_opt_deopt`](runtime::capy_opt_deopt), which reads the
//!   save area and the optimized frame, rebuilds the baseline frames (inlined
//!   callees and sunk objects included) and returns the resume address.
//! - [`exits`]: the per-block exit profile that decides when a unit is
//!   invalidated, and the `CAPY_OPT_STRESS_DEOPT` knob.

pub(crate) mod exits;
pub(crate) mod metadata;
pub(crate) mod runtime;
