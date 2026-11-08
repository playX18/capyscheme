//! The exit profile of a code block: how often each bytecode pc deoptimized,
//! the speculation bit the next compile reads, and the unit's exit budget.

use std::collections::{BTreeMap, HashMap, HashSet};
use std::sync::{Mutex, OnceLock};

use super::metadata::GuardKind;

/// Deopts at one pc before the next compile stops speculating there.
pub(crate) const SPECULATION_EXIT_LIMIT: u32 = 4;

/// Deopts of one installed unit before it is invalidated.
pub(crate) const INVALIDATE_AFTER: u32 = 64;

/// Exit counts of one code block.
#[derive(Debug, Default)]
pub(crate) struct ExitProfile {
    /// Deopts per bytecode pc.
    per_pc: HashMap<u32, u32>,
    /// PCs that exited often enough that the next compile must not speculate.
    no_speculate: HashSet<u32>,
    /// Deopts of the installed unit, reset when the unit is invalidated.
    unit_exits: u32,
}

impl ExitProfile {
    /// Counts an exit at `pc`. Returns whether the installed unit has now
    /// exited often enough to be invalidated.
    pub(crate) fn record(&mut self, pc: u32) -> bool {
        let count = self.per_pc.entry(pc).or_default();
        *count += 1;
        if *count >= SPECULATION_EXIT_LIMIT {
            self.no_speculate.insert(pc);
        }
        self.unit_exits += 1;
        self.unit_exits >= INVALIDATE_AFTER
    }

    /// Counts an exit of the installed unit that happened in an inlined body
    /// (the body's own profile holds its pc). Returns whether the unit has
    /// now exited often enough to be invalidated.
    pub(crate) fn count_unit_exit(&mut self) -> bool {
        self.unit_exits += 1;
        self.unit_exits >= INVALIDATE_AFTER
    }

    /// Whether the next compile must not speculate at `pc`.
    pub(crate) fn speculation_blocked(&self, pc: u32) -> bool {
        self.no_speculate.contains(&pc)
    }

    /// Every pc where the next compile must not speculate.
    pub(crate) fn blocked_pcs(&self) -> Vec<u32> {
        self.no_speculate.iter().copied().collect()
    }

    /// The exits counted at `pc`.
    pub(crate) fn exits_at(&self, pc: u32) -> u32 {
        self.per_pc.get(&pc).copied().unwrap_or(0)
    }

    /// Start the count for the next unit.
    pub(crate) fn reset_unit(&mut self) {
        self.unit_exits = 0;
    }
}

/// One `(unit, call pc, inlined code block, guard kind)` diagnostic key.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
struct ExitSite {
    unit: usize,
    call_pc: u32,
    /// `0` for an exit in the root frame; otherwise the index of the inlined
    /// code block in the unit's code table.
    code: u32,
    kind: GuardKind,
}

struct SiteCount {
    /// The procedure label of the block the guard is in, filled in on the
    /// first exit so a hot path pays nothing for it.
    label: String,
    count: u64,
}

static EXIT_SITES: OnceLock<Mutex<BTreeMap<ExitSite, SiteCount>>> = OnceLock::new();

fn exit_sites() -> &'static Mutex<BTreeMap<ExitSite, SiteCount>> {
    EXIT_SITES.get_or_init(|| Mutex::new(BTreeMap::new()))
}

/// Counts one exit per diagnostic site. `label` is only called the first
/// time a site is seen, so an exit pays for a procedure label at most once.
pub(crate) fn record_exit_site(
    unit: usize,
    call_pc: u32,
    code: u32,
    kind: GuardKind,
    label: impl FnOnce() -> String,
) {
    let key = ExitSite {
        unit,
        call_pc,
        code,
        kind,
    };
    let mut sites = exit_sites().lock().unwrap_or_else(|err| err.into_inner());
    let entry = sites.entry(key).or_insert_with(|| SiteCount {
        label: label(),
        count: 0,
    });
    entry.count += 1;
}

pub(crate) fn report_exit_sites() -> Vec<String> {
    let sites = exit_sites().lock().unwrap_or_else(|err| err.into_inner());
    sites
        .iter()
        .map(|(site, count)| {
            if site.code == 0 {
                format!(
                    "exit site: unit {:#x} root pc {} guard {}: {} [{}]",
                    site.unit, site.call_pc, site.kind, count.count, count.label
                )
            } else {
                format!(
                    "exit site: unit {:#x} inlined code {} at call pc {} guard {}: {} [{}]",
                    site.unit, site.code, site.call_pc, site.kind, count.count, count.label
                )
            }
        })
        .collect()
}
