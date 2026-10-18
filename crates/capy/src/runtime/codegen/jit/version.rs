//! TODO LBBV API, NOT COMPLETE!!!
use std::collections::HashMap;

use mmtk::util::Address;

use super::super::emission::Emission;
use super::super::lazy::{EdgeKind, Lazy, Materialize, MaterializeCx};
use super::super::masm::Masm;
use super::super::template::{Known, Placement, Target, Targets};
use crate::runtime::interp::stack::Ip;

#[derive(Clone, PartialEq, Eq, Hash, Debug, Default)]
pub(crate) struct VersionContext {
    known: Vec<(u32, Known)>,
}

impl VersionContext {
    pub(crate) fn refine(&mut self, field: u32, known: Known) {
        match self.known.binary_search_by_key(&field, |(f, _)| *f) {
            Ok(at) if known == Known::Any => {
                self.known.remove(at);
            }
            Ok(at) => self.known[at].1 = known,
            Err(_) if known == Known::Any => {}
            Err(at) => self.known.insert(at, (field, known)),
        }
    }

    pub(crate) fn known(&self, field: u32) -> Known {
        self.known
            .binary_search_by_key(&field, |(f, _)| *f)
            .map_or(Known::Any, |at| self.known[at].1)
    }
}

pub(crate) type VersionKey = (usize, VersionContext);

pub(crate) struct BlockVersion {
    pub(crate) pc: usize,
    /// The bytecode address of `pc`.
    pub(crate) ip: Ip,
    pub(crate) context: VersionContext,
}

unsafe impl Send for BlockVersion {}
unsafe impl Sync for BlockVersion {}

impl Lazy for BlockVersion {
    const NAME: &'static str = "version";

    fn pc(&self) -> usize {
        self.pc
    }

    fn ip(&self) -> Ip {
        self.ip
    }

    fn log_context(&self) -> String {
        format!("{:?}", self.context)
    }

    fn materialize<'gc>(&self, _cx: &mut MaterializeCx<'gc>) -> Materialize {
        Materialize::Sticky("lbbv tier not available")
    }
}

#[allow(dead_code)]
pub(crate) struct VersionTargets {
    entry: Ip,
    context: VersionContext,
    versions: HashMap<VersionKey, Address>,
}

#[allow(dead_code)]
impl VersionTargets {
    pub(crate) fn new(
        entry: Ip,
        context: VersionContext,
        versions: HashMap<VersionKey, Address>,
    ) -> Self {
        Self {
            entry,
            context,
            versions,
        }
    }

    pub(crate) fn refine(&mut self, field: u32, known: Known) {
        self.context.refine(field, known);
    }
}

impl Targets for VersionTargets {
    fn target(&mut self, em: &mut Emission, masm: &mut Masm, pc: usize) -> Target {
        use super::super::lazy::Lazies;
        if let Some(&addr) = self.versions.get(&(pc, self.context.clone())) {
            return Target::Absolute(addr);
        }
        let label = em.edge(
            masm,
            EdgeKind::Direct,
            BlockVersion {
                pc,
                // SAFETY: `pc` is a landing pc inside the procedure's code.
                ip: unsafe { self.entry.add(pc) },
                context: self.context.clone(),
            },
        );
        Target::Edge(label)
    }

    fn placement(&self) -> Placement {
        Placement::Deferred
    }
}
