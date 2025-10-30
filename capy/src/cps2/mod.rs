//! CPS v2: an improved continuation-passing style intermediate representation and optimizer.
//!
//! This module contains an improved version of the CPS IR and optimizer,
//! designed to address some of the limitations of the original CPS implementation such as
//! slow optimization times and huge memory consumption on large inputs.
//!
//! This IR is based on [CPS soup](https://wingolog.org/archives/2015/07/27/cps-soup).

pub mod term;
