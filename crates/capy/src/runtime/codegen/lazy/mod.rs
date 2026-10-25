//! Lazy code: edges that generate their target on first use.
//!
//! A [`Lazy`] is the behaviour behind a lazy edge: an arithmetic site, and
//! later call inline caches, block versions or side exits. Generated code
//! reaches it through an edge ([`edge`]) that jumps through the `target`
//! word of the lazy's *cell*. The word starts at the edge's own hit block,
//! which leaves through [`capy_lazy_hit`] with the cell as argument, and the
//! hit runs [`Lazy::hit`]: by default it materializes code for the lazy's
//! fresh state ([`Lazy::materialize`]) and continues there. A lazy that must
//! also make progress on the hit (arithmetic runs the generic operation
//! first) overrides `hit` and drives the same machinery through
//! [`HitCx::materialize`].
//!
//! Cells are erased: [`ErasedCell`] owns a `Cell<T>` whose [`CellHeader`]
//! comes first, so every kind shares one runtime entry, one state machine
//! and one arena ([`CellArena`], owned by the code unit's resume table).
//! Cells are ordinary heap data, not code, and each is its own allocation so
//! its address, baked into generated code, never moves. A cell stores no
//! heap references: only raw addresses of bytecode and metadata, which never
//! move, and plain data.
//!
//! Edges are declared through [`Lazies`], the same call before load
//! ([`Emission`](super::emission::Emission): the cell is pending and its
//! address a [`Reloc::Cell`](super::reloc::Reloc::Cell)) and after
//! ([`MaterializeCx`]: the cell is allocated now and its address baked), so
//! code materialized from an edge can declare edges of its own.
//!
//! Hits run on the hitting mutator with Scheme state saved, never on a
//! worker: a frame is waiting on them. [`HitCx`] and [`MaterializeCx`] carry
//! that mutator's `'gc`. The only erasure is the FFI entry
//! [`capy_lazy_hit`], whose C signature cannot name `'gc`.

pub(crate) mod builder;
pub(crate) mod edge;

use std::io;
use std::pin::Pin;
use std::ptr::NonNull;
use std::sync::atomic::AtomicU8;
use std::sync::atomic::Ordering::{Acquire, Relaxed, Release};
use std::sync::{Mutex, OnceLock};

use atomic::Atomic;
use mmtk::util::Address;
use onlineasm::asmkit::Span;
use onlineasm::{Label, PatchHandle};

use self::builder::StubBuilder;
pub(crate) use self::edge::EdgeKind;
use self::edge::{DirectSite, DirectSpan};
use super::addr::CodeOffset;
use super::image::LoadedImage;
use super::jit::registry::{LIVE_CODE_BYTES, ResumeTable};
use super::masm::{Masm, SymbolKind};
use super::template::SiteOperand;
use crate::runtime::MutationContext;
use crate::runtime::interp::stack::{Ip, Slot};
use crate::utils::flags;

type MutatorPointer = *const ();

/// Behaviour behind a lazy edge. One impl per kind: arith site, call IC,
/// block version, side exit, deopt exit.
///
/// The impl type is `'static` only so its vtable can live in the cell
/// header: a lazy never stores heap data with a lifetime, just raw stable
/// addresses and plain data.
pub(crate) trait Lazy: Sized + 'static {
    /// Short name for logs and symbols (`math` keeps the historic lines).
    const NAME: &'static str;

    /// The bytecode offset this lazy stands for, for logs and listings.
    fn pc(&self) -> usize;

    /// The bytecode address the hit block stages in `PC`: where `Tc::ip`
    /// points when the hit runs.
    fn ip(&self) -> Ip;

    /// Diagnostic detail for the log lines, read fresh (logging only: it
    /// may differ from the state `materialize` saw).
    fn log_context(&self) -> String {
        String::new()
    }

    /// A hit on the unresolved edge, on the mutator with Scheme state
    /// saved. Returns the machine address to continue at. Default:
    /// materialize, then continue at the new code, or in the interpreter at
    /// the staged instruction when no code was generated. A lazy that has
    /// work of its own to do on a hit overrides this.
    fn hit<'gc>(&self, cx: &mut HitCx<'gc>) -> *const u8 {
        cx.materialize_and_enter(self)
    }

    /// Emit code for the fresh state, or decline.
    fn materialize<'gc>(&self, cx: &mut MaterializeCx<'gc>) -> Materialize;
}

/// What [`Lazy::materialize`] decided.
pub(crate) enum Materialize {
    /// Code was installed through [`MaterializeCx::install`]. `again`: a
    /// miss in the new code comes back to the same cell (a specialization
    /// that can still grow); otherwise the code is terminal.
    Code { again: bool },
    /// Never generate: the edge stays on its slow path for good.
    Sticky(&'static str),
    /// Budget or contention: try again on the next hit.
    Later,
}

/// What a cell's [`CellHeader::target`] points at.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
#[repr(u8)]
pub(crate) enum CellState {
    /// The edge's hit block: nothing generated yet.
    Slow = 0,
    /// Specialized code: a miss in it still hits the cell.
    Specialized = 2,
    /// Terminal code: a miss in it is a plain call into Rust.
    Full = 3,
    /// The hit block, permanently: nothing to generate.
    Sticky = 4,
}

impl CellState {
    fn from_u8(value: u8) -> Self {
        match value {
            v if v == CellState::Specialized as u8 => CellState::Specialized,
            v if v == CellState::Full as u8 => CellState::Full,
            v if v == CellState::Sticky as u8 => CellState::Sticky,
            _ => CellState::Slow,
        }
    }
}

/// The type-erased operations of one [`Lazy`] impl.
pub(crate) struct CellVTable {
    pub(crate) name: &'static str,
    hit: for<'gc> unsafe fn(*const CellHeader, &mut HitCx<'gc>) -> *const u8,
    pc: unsafe fn(*const CellHeader) -> usize,
}

/// The shared prefix of every cell. `repr(C)`, at the cell's offset zero,
/// so an indirect edge is one zero-displacement jump through
/// [`target`](Self::target).
#[repr(C)]
pub(crate) struct CellHeader {
    /// Where the edge jumps: the edge's hit block, then generated code.
    /// First, at offset zero, on both targets.
    pub(crate) target: Atomic<Address>,
    state: AtomicU8,
    kind: EdgeKind,
    /// Serializes materialization, the slow path outside it stays
    /// lock-free.
    lock: Mutex<()>,
    vtable: &'static CellVTable,
    /// The span of the image holding the patchable direct jump of a
    /// [`EdgeKind::Direct`] edge, and the image offset of the jump. Unset
    /// for any other edge. Set once, when the code holding the jump is
    /// loaded, before the cell can be hit.
    direct: OnceLock<(DirectSpan, CodeOffset)>,
    /// The unit's resume table, resolving baked exits and holding the
    /// arena new cells join. Set when the unit reaches its owner, before
    /// it is published.
    origin: OnceLock<OriginTable>,
}

/// The resume table of the unit a cell belongs to.
struct OriginTable(NonNull<ResumeTable>);

const _: () = assert!(core::mem::offset_of!(CellHeader, target) == 0);

impl CellHeader {
    pub(crate) fn state(&self) -> CellState {
        CellState::from_u8(self.state.load(Acquire))
    }

    pub(crate) fn kind(&self) -> EdgeKind {
        self.kind
    }

    /// The lazy's bytecode offset.
    pub(crate) fn pc(&self) -> usize {
        // SAFETY: a header is the prefix of the cell its vtable was made
        // for (`ErasedCell::new`).
        unsafe { (self.vtable.pc)(self) }
    }

    /// Where this edge's direct jump lives, once its code is loaded.
    pub(crate) fn direct_site(&self) -> Option<DirectSite> {
        let (span, at) = self.direct.get()?;
        Some(DirectSite {
            span: span.0,
            at: *at,
        })
    }

    /// Record where the direct jump of this [`EdgeKind::Direct`] cell lives:
    /// the word at `at` of the image in `span`. Once, when the image is
    /// loaded, before anything can hit the cell.
    pub(crate) fn bind_direct(&self, span: Span, at: CodeOffset) {
        assert!(
            self.kind == EdgeKind::Direct,
            "only a direct edge has a jump"
        );
        assert!(
            self.direct.set((DirectSpan(span), at)).is_ok(),
            "bound once"
        );
    }

    /// Record the resume table of the unit this cell belongs to.
    fn bind_origin(&self, table: &ResumeTable) {
        assert!(
            self.origin.set(OriginTable(NonNull::from(table))).is_ok(),
            "bound once"
        );
    }

    /// The impl's name.
    pub(crate) fn name(&self) -> &'static str {
        self.vtable.name
    }
}

/// `repr(C)` so the header is at offset zero of the allocation.
#[repr(C)]
struct Cell<T: Lazy> {
    header: CellHeader,
    value: T,
}

// SAFETY: a cell is pinned data. Its payload (a `Lazy`) holds only raw stable
// addresses and plain data, its header's atomics and `OnceLock`s synchronize
// the rest, and the code-memory writes behind `DirectSpan` and the resume
// table behind `OriginTable` go through `CodeMemory` under its lock. Both
// live in the unit's heap owner, which outlives its cells and never moves.
unsafe impl Send for CellHeader {}
unsafe impl Sync for CellHeader {}
unsafe impl<T: Lazy> Send for Cell<T> {}
unsafe impl<T: Lazy> Sync for Cell<T> {}

impl<T: Lazy> Cell<T> {
    const VTABLE: CellVTable = CellVTable {
        name: T::NAME,
        hit: hit_thunk::<T>,
        pc: pc_thunk::<T>,
    };
}

/// # Safety
///
/// `header` is the prefix of a live `Cell<T>`.
unsafe fn value_of<'a, T: Lazy>(header: *const CellHeader) -> &'a T {
    // SAFETY: by the contract, the cast recovers the whole cell.
    unsafe { &(*header.cast::<Cell<T>>()).value }
}

unsafe fn hit_thunk<'gc, T: Lazy>(header: *const CellHeader, cx: &mut HitCx<'gc>) -> *const u8 {
    // SAFETY: the vtable was made for `T`, and the cell is alive for the hit.
    unsafe { value_of::<T>(header) }.hit(cx)
}

unsafe fn pc_thunk<T: Lazy>(header: *const CellHeader) -> usize {
    // SAFETY: as in `hit_thunk`.
    unsafe { value_of::<T>(header) }.pc()
}

/// What an [`ErasedCell`] needs of its cell: the header, at the cell's
/// start.
trait CellObject: Send + Sync {
    fn header(&self) -> &CellHeader;
}

impl<T: Lazy> CellObject for Cell<T> {
    fn header(&self) -> &CellHeader {
        &self.header
    }
}

/// An owned, type-erased cell. Plain `Send` data until it is bound into a
/// unit: the allocation never moves (it is pinned), so the address the code
/// bakes is the address it keeps.
pub(crate) struct ErasedCell {
    cell: Pin<Box<dyn CellObject>>,
}

impl ErasedCell {
    /// A cell for `value`, its target not yet set.
    pub(crate) fn new<T: Lazy>(kind: EdgeKind, value: T) -> Self {
        Self {
            cell: Box::pin(Cell {
                header: CellHeader {
                    target: Atomic::new(Address::ZERO),
                    state: AtomicU8::new(CellState::Slow as u8),
                    kind,
                    lock: Mutex::new(()),
                    vtable: &Cell::<T>::VTABLE,
                    direct: OnceLock::new(),
                    origin: OnceLock::new(),
                },
                value,
            }),
        }
    }

    pub(crate) fn header(&self) -> &CellHeader {
        self.cell.header()
    }

    /// The address code bakes for this cell: the header's, which starts
    /// the `repr(C)` cell.
    pub(crate) fn addr(&self) -> Address {
        Address::from_ref(self.header())
    }
}

/// Machine code loaded for a materialized piece: its image and its bytes,
/// counted in [`LIVE_CODE_BYTES`]. Dies with the unit, which every cell that
/// can reach it shares its lifetime with.
pub(crate) struct ExtraImage {
    pub(crate) image: LoadedImage,
    pub(crate) bytes: usize,
}

/// The cells of one code unit and the code materialized for them.
///
/// Appends happen under the lock, on the hitting mutator. Cells are boxed
/// one by one, so appending never moves one.
pub(crate) struct CellArena {
    cells: Mutex<Vec<ErasedCell>>,
    extra: Mutex<Vec<ExtraImage>>,
}

impl CellArena {
    /// An arena holding `initial`, the cells the unit was linked with.
    pub(crate) fn new(initial: Box<[ErasedCell]>) -> Self {
        Self {
            cells: Mutex::new(initial.into_vec()),
            extra: Mutex::new(Vec::new()),
        }
    }

    /// Append `cell`, returning its stable address.
    pub(crate) fn push(&self, cell: ErasedCell) -> Address {
        let addr = cell.addr();
        self.cells.lock().unwrap().push(cell);
        addr
    }

    /// Number of cells.
    pub(crate) fn len(&self) -> usize {
        self.cells.lock().unwrap().len()
    }

    /// Record materialized code to release with the unit.
    fn add_extra(&self, extra: ExtraImage) {
        self.extra.lock().unwrap().push(extra);
    }

    /// Number of materialized pieces.
    pub(crate) fn extra_count(&self) -> usize {
        self.extra.lock().unwrap().len()
    }

    /// Take the materialized code, for the finalizer to release.
    pub(crate) fn take_extra(&self) -> Vec<ExtraImage> {
        std::mem::take(&mut *self.extra.lock().unwrap())
    }

    /// Point every cell at the unit's resume table `origin`.
    /// Pre-publication: the cells have no readers yet.
    pub(crate) fn bind(&self, origin: &ResumeTable) {
        for cell in self.cells.lock().unwrap().iter() {
            cell.header().bind_origin(origin);
        }
    }
}

/// Declares lazy edges. One call, before load or after.
pub(crate) trait Lazies {
    /// Declare an edge for `value`, its entry block bound at `label`.
    fn edge_at<T: Lazy>(&mut self, masm: &mut Masm, label: Label, kind: EdgeKind, value: T);

    /// Declare an edge for `value`, returning the label guard failures
    /// jump to.
    fn edge<T: Lazy>(&mut self, masm: &mut Masm, kind: EdgeKind, value: T) -> Label {
        let label = masm.label();
        self.edge_at(masm, label, kind, value);
        label
    }
}

/// A hit on a lazy edge: the hitting mutator and the cell. Passed to
/// [`Lazy::hit`].
pub(crate) struct HitCx<'gc> {
    mutator: MutationContext<'gc>,
    cell: NonNull<CellHeader>,
}

impl<'gc> HitCx<'gc> {
    fn new(mutator: MutationContext<'gc>, cell: NonNull<CellHeader>) -> Self {
        Self { mutator, cell }
    }

    pub(crate) fn mutator(&self) -> MutationContext<'gc> {
        self.mutator
    }

    /// The frame pointer and bytecode `ip` the edge staged.
    pub(crate) fn frame(&self) -> (Slot<'gc>, Ip) {
        let tc = self.mutator.tc();
        (tc.stack.fp.get(), tc.ip.get())
    }

    /// Resume the frame at `fp` at bytecode `ip`: compiled code when it has
    /// the instruction, else the interpreter.
    ///
    /// # Safety
    ///
    /// `fp` is the live frame of the hit, and `ip` an instruction of its
    /// code.
    pub(crate) unsafe fn resume_at(&self, fp: Slot<'gc>, ip: Ip) -> *const u8 {
        if flags::jit_stats() {
            super::jit::SLOW_HELPER.fetch_add(1, Relaxed);
        }
        let tc = self.mutator.tc();
        // SAFETY: slot 1 of a live frame holds its closure.
        let cp = unsafe { *fp.add(1) };
        tc.cp.set(cp);
        tc.ip.set(ip);
        super::slow::resume_addr(cp, ip)
    }

    /// Run the state machine for `value`: if its state calls for it and no
    /// other thread is generating, materialize and publish the new code.
    /// Returns the new entry, or `None` when nothing was generated (already
    /// terminal, another thread is generating, over budget, declined, or
    /// failed): the caller carries on with the generic path.
    pub(crate) fn materialize<T: Lazy>(&mut self, value: &T) -> Option<Address> {
        // SAFETY: the cell is alive for the hit.
        let header = unsafe { self.cell.as_ref() };
        if !matches!(header.state(), CellState::Slow | CellState::Specialized) {
            return None;
        }
        let Ok(_guard) = header.lock.try_lock() else {
            return None;
        };
        let from = header.state();
        if !matches!(from, CellState::Slow | CellState::Specialized) {
            return None;
        }
        if LIVE_CODE_BYTES.load(Relaxed) >= flags::jit_max_code() {
            return None;
        }
        let mut cx = MaterializeCx::new(self.mutator, self.cell, from);
        let outcome = value.materialize(&mut cx);
        let pc = value.pc();
        let sticky = |reason: &dyn std::fmt::Display| {
            log::info!(
                target: crate::heap::logging::JIT_LOG_TARGET,
                "{}: pc={pc} {from:?} -> Sticky ({reason}, {})",
                T::NAME,
                value.log_context()
            );

            header.state.store(CellState::Sticky as u8, Release);
        };
        match outcome {
            Materialize::Code { again } => {
                let (entry, error) = (cx.entry, cx.error.take());
                let Some(entry) = entry.filter(|_| error.is_none()) else {
                    let err = error.map_or_else(
                        || "materialize installed no code".to_string(),
                        |err| err.to_string(),
                    );
                    log::warn!(target: crate::heap::logging::JIT_LOG_TARGET, "jit {}: {err}", T::NAME);
                    sticky(&err);
                    return None;
                };
                let to = if again {
                    CellState::Specialized
                } else {
                    CellState::Full
                };

                log::info!(
                    target: crate::heap::logging::JIT_LOG_TARGET,
                    "{}: pc={pc} {from:?} -> {to:?} stub={entry} ({} bytes, {})",
                    T::NAME,
                    cx.bytes,
                    value.log_context()
                );

                header.target.store(entry, Release);
                header.state.store(to as u8, Release);
                // The target is written, so the jump of a direct edge can
                // move past its thunk.
                edge::resolve_direct(header, to, entry);
                Some(entry)
            }
            Materialize::Sticky(reason) => {
                if let Some(err) = cx.error.take() {
                    log::warn!(target: crate::heap::logging::JIT_LOG_TARGET, "jit {}: {err}", T::NAME);
                    sticky(&err);
                } else {
                    sticky(&reason);
                }
                None
            }
            Materialize::Later => None,
        }
    }

    /// The default hit: materialize and continue at the new code. When
    /// nothing was generated (another thread is generating, over budget,
    /// declined), resume the staged instruction in the interpreter, which
    /// is always correct for an edge that has done no work of its own. A
    /// lazy that runs the generic operation itself overrides
    /// [`Lazy::hit`].
    pub(crate) fn materialize_and_enter<T: Lazy>(&mut self, value: &T) -> *const u8 {
        if let Some(entry) = self.materialize(value) {
            return entry.to_ptr();
        }
        // SAFETY: the cell is alive for the hit.
        let header = unsafe { self.cell.as_ref() };
        if matches!(header.state(), CellState::Specialized | CellState::Full) {
            // Another thread published while this one raced it.
            return header.target.load(Acquire).to_ptr();
        }
        let (fp, ip) = self.frame();
        // SAFETY: the edge staged the frame and instruction of this hit.
        unsafe { self.resume_at(fp, ip) }
    }
}

/// An edge declared while materializing, emitted at install.
struct NewEdge {
    /// Where guard failures and branches enter. For a direct edge, the
    /// patchable jump to `thunk`.
    label: Label,
    /// The indirect edge of a direct one: `label` falls into it until the
    /// jump is patched.
    thunk: Option<Label>,
    hit: Label,
    cell: Address,
    ip: Ip,
}

/// Materializing a lazy: the mutator, the cell, and the means to load code
/// and declare edges of its own. Passed to [`Lazy::materialize`].
pub(crate) struct MaterializeCx<'gc> {
    mutator: MutationContext<'gc>,
    cell: NonNull<CellHeader>,
    from: CellState,
    /// The entry [`install`](Self::install) loaded, and its size.
    entry: Option<Address>,
    bytes: usize,
    error: Option<io::Error>,
    /// Cells declared so far and the hit blocks their targets start at.
    edges: Vec<NewEdge>,
}

impl<'gc> MaterializeCx<'gc> {
    fn new(mutator: MutationContext<'gc>, cell: NonNull<CellHeader>, from: CellState) -> Self {
        Self {
            mutator,
            cell,
            from,
            entry: None,
            bytes: 0,
            error: None,
            edges: Vec::new(),
        }
    }

    pub(crate) fn mutator(&self) -> MutationContext<'gc> {
        self.mutator
    }

    /// The state the cell is materializing from.
    pub(crate) fn state(&self) -> CellState {
        self.from
    }

    /// The address of the cell being materialized, for code that misses
    /// back into it.
    pub(crate) fn cell_addr(&self) -> Address {
        Address::from_ptr(self.cell.as_ptr())
    }

    /// The resume table of the unit the cell belongs to.
    pub(crate) fn origin_table(&self) -> &'gc ResumeTable {
        // SAFETY: the cell is alive while it materializes.
        let origin = unsafe { self.cell.as_ref() }
            .origin
            .get()
            .expect("a cell is bound to its unit before it can be hit");
        // SAFETY: the table lives in the unit's owner, which the frame that
        // hit the cell keeps alive, and never moves.
        unsafe { origin.0.as_ref() }
    }

    /// Record why materializing failed, and decline for good.
    pub(crate) fn fail(&mut self, error: io::Error) -> Materialize {
        self.error = Some(error);
        Materialize::Sticky("error")
    }

    /// Finish `masm` (whose code starts at `start`), load it, and make its
    /// entry the cell's next target. The unit owns the image. Cells
    /// declared through [`Lazies`] start at their hit blocks, and the direct
    /// jumps of direct edges are located in the loaded image.
    pub(crate) fn install(
        &mut self,
        mut masm: Masm,
        name: &str,
        start: Label,
    ) -> io::Result<Address> {
        let mut edges = std::mem::take(&mut self.edges);
        let mut jumps: Vec<Option<PatchHandle>> = Vec::with_capacity(edges.len());
        for edge in &mut edges {
            masm.bind(edge.label);
            jumps.push(match edge.thunk {
                Some(thunk) => {
                    let jump = edge::emit_direct_jump(&mut masm, thunk);
                    masm.bind(thunk);
                    Some(jump)
                }
                None => None,
            });
            edge::emit_indirect_edge_to(&mut masm, edge.cell);
            masm.bind(edge.hit);
            edge::emit_hit_block(&mut masm, edge.ip, SiteOperand::Register(edge::SITE_TMP));
        }
        // No CFI region: the code is entered by a jump, with no new frame.
        let stub = StubBuilder::new(masm, name, start, SymbolKind::Stub).finish()?;
        let start_off = stub.offset(start);
        let hits: Vec<usize> = edges.iter().map(|edge| stub.offset(edge.hit)).collect();
        let direct_ats: Vec<Option<usize>> = jumps
            .iter()
            .map(|jump| {
                jump.map(|jump| {
                    stub.jump_offset(jump)
                        .expect("a direct jump is a jump site")
                })
            })
            .collect();
        let loaded = stub.load()?;
        let base = loaded.base();
        for ((edge, hit), direct_at) in edges.iter().zip(hits).zip(direct_ats) {
            // SAFETY: the cell is in the arena, alive with the unit.
            let header = unsafe { &*edge.cell.to_ptr::<CellHeader>() };
            if let Some(at) = direct_at {
                header.bind_direct(loaded.span(), CodeOffset(at as u32));
            }
            header.target.store(base + hit, Release);
        }
        let bytes = loaded.code_size();
        self.origin_table().cells().add_extra(ExtraImage {
            image: loaded,
            bytes,
        });
        LIVE_CODE_BYTES.fetch_add(bytes, Relaxed);
        let entry = base + start_off;
        self.entry = Some(entry);
        self.bytes = bytes;
        Ok(entry)
    }
}

impl Lazies for MaterializeCx<'_> {
    fn edge_at<T: Lazy>(&mut self, masm: &mut Masm, label: Label, kind: EdgeKind, value: T) {
        let ip = value.ip();
        let table = self.origin_table();
        let cell = ErasedCell::new(kind, value);
        // The new cell belongs to the same unit as the one materializing:
        // its own materialize resolves exits and declares cells there.
        cell.header().bind_origin(table);
        let cell = table.cells().push(cell);
        self.edges.push(NewEdge {
            label,
            thunk: (kind == EdgeKind::Direct).then(|| masm.label()),
            hit: masm.label(),
            cell,
            ip,
        });
    }
}

/// The single runtime entry of every lazy edge: the hit block's
/// `call`. Dispatches through the cell's vtable on the mutator `th`, and
/// returns the machine address to continue at.
///
/// This is the FFI boundary: the C signature cannot name `'gc`, so the
/// mutator is rebuilt here from `th`, valid for the duration of the call.
/// Nothing derived from it is returned or stored.
///
/// # Safety
///
/// `th` is the calling mutator, and `cell` a live cell of a loaded unit.
pub(crate) unsafe extern "C" fn capy_lazy_hit(
    th: MutatorPointer,
    cell: *const CellHeader,
) -> *const u8 {
    // SAFETY: by the contract, `th` is this thread's mutator.
    let mutator = unsafe { MutationContext::from_ptr(th) };
    // SAFETY: by the contract, `cell` is live.
    unsafe { hit(mutator, cell) }
}

/// # Safety
///
/// `cell` is a live cell, and `mutator` the thread's.
unsafe fn hit<'gc>(mutator: MutationContext<'gc>, cell: *const CellHeader) -> *const u8 {
    // SAFETY: by the contract.
    let (vtable, header) = unsafe {
        (
            (*cell).vtable,
            NonNull::new_unchecked(cell as *mut CellHeader),
        )
    };
    let mut cx = HitCx::new(mutator, header);
    // SAFETY: the vtable belongs to this cell.
    unsafe { (vtable.hit)(cell, &mut cx) }
}
