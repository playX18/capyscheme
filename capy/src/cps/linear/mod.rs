mod builder;
mod cache;
mod constant;
mod graph;
mod ir;
mod rest;
mod switch;

#[cfg(test)]
mod tests;

pub use builder::linearize;
use cache::lower_cache_operations;
use constant::hoist_constants;
pub use ir::*;
use rest::lower_rest_arguments;
use switch::infer_switches;
