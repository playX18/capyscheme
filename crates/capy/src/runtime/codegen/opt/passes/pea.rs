//! Partial escape analysis and scalar replacement.
//!
//! This is Stadler, Würthinger and Mössenböck, "Partial Escape Analysis and
//! Scalar Replacement for Java" (CGO 2014), on block parameters.
//!
//! The pass is written as analysis first and effects second. The analysis
//! walks the blocks in reverse postorder with a state per program point and
//! records what to do in tables. Nothing in the function changes until the
//! analysis of the whole function has settled, and a failure of any kind
//! ([`Giveup`]) leaves the function as it was.
//!
//! # Objects
//!
//! A candidate is the allocation of one of these, with at most
//! [`MAX_FIELDS`] fields:
//!
//! - a pair;
//! - a closure;
//! - an environment;
//! - a vector;
//! - a tuple;
//! - an instance;
//! - a variable cell (`make-variable`).
//!
//! Its id is its index in the table of candidates. The state at a program
//! point maps every object to one of three things:
//!
//! - nothing: it is not allocated on this path;
//! - [`Slot::Virt`]: virtual, holding its current field values;
//! - [`Slot::Mat`]: materialized, holding the value of the allocation on this
//!   path.
//!
//! A value denotes an object (it is an alias) when it is one of these:
//!
//! - the allocation's result;
//! - the result of a `check.class` or `check.code` of an alias;
//! - a block parameter that carries the same virtual object on every edge;
//! - a block parameter that merges different virtual objects of one kind
//!   (see below);
//! - a load that yields a virtual object stored as a field.
//!
//! # Merges
//!
//! A field that differs between the incoming edges gets a new block
//! parameter. An object that is virtual on some edges and not on others is
//! materialized at the end of the edges where it is virtual. An edge that
//! leaves a branch is split, so the allocation runs on that path only.
//!
//! A parameter that receives different virtual objects of one kind (two
//! pairs, two vectors of one length, ...) becomes a new virtual object whose
//! fields are merged the same way. Each input object is then known only through
//! the parameter: a later use of an input by another name, or an input that comes
//! in two parameters, sends the merge back to the plain rule, where the inputs are
//! materialized on their edges.
//!
//! # Loops
//!
//! The analysis assumes that an object that is virtual on every entering
//! edge of a header stays virtual, and makes the fields the loop writes into
//! header parameters. It checks the assumption on every back edge:
//!
//! - a field that changed becomes a parameter;
//! - an object that did not stay virtual is materialized on the entering
//!   edges.
//!
//! The whole analysis then runs again with that knowledge. Constraints only
//! grow, so the runs end, within [`MAX_RUNS`].
//!
//! A loop parameter whose back edge brings a fresh object of the same kind
//! (`(loop (cons x y))`) is a merge too. The object of each iteration is the
//! one virtual object of the header, and no iteration allocates.
//!
//! # Frame states
//!
//! Every kept instruction that has a frame state gets a copy of it in which
//! every alias is replaced: a virtual object by its canonical alias, a
//! materialized one by its allocation. The head of the chain holds a
//! [`VirtualObject`] for each virtual object that the state reaches, with
//! the current field values.
//!
//! The allocation of an object that some state still names becomes a
//! `remat <kind>` placeholder, since the alias must have a definition. The
//! other allocations disappear.
//!
//! # Continuations
//!
//! A call that can capture a continuation may return twice into a frame that
//! was rebuilt from its frame state, with a fresh copy of every virtual object
//! the frame names. If an object is stored to and read after such a call, the
//! read would see the copy, so the object is materialized before the call.

use std::collections::{HashMap, HashSet};

use cranelift_entity::{EntityList, EntityRef, SecondaryMap};

use super::PassOutcome;
use crate::runtime::codegen::opt::ir::frame_state::{FrameState, FrameStateData, VirtualObject};
use crate::runtime::codegen::opt::ir::func::{Block, Func, Value, ValueData, ValueDef};
use crate::runtime::codegen::opt::ir::inst::{
    Cmp, Constant, EffectFlags, Helper, Inst, InstData, MemoryRegion, Op, RematKind, Terminator,
};
use crate::runtime::codegen::opt::ir::types::{ClassRef, CodeRef, Type};
use crate::runtime::objectmodel::Value as SchemeValue;
use crate::runtime::prims::def::Primitive;

/// The most fields an object may have to be a candidate.
const MAX_FIELDS: usize = 128;
/// The most analysis runs before the pass gives up.
const MAX_RUNS: usize = 24;
/// The most fields an object may have to be imported at an OSR entry.
const MAX_IMPORT_FIELDS: usize = 16;
/// The most elements a virtual vector or tuple may have to stay virtual
/// under an index that is not a constant. A read is a chain of selects on
/// the index, and a write a select per element.
const MAX_SELECT: usize = 8;

/// The analysis cannot handle the function, or a constraint was added and the
/// analysis runs again (`Constraints::restart`).
struct Giveup;

type Res<T> = Result<T, Giveup>;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Shape {
    Pair,
    Closure(CodeRef),
    Env,
    Vector,
    Tuple,
    Instance(u32),
    Variable,
}

impl Shape {
    fn class(self) -> ClassRef {
        match self {
            Shape::Pair => ClassRef::PAIR,
            Shape::Closure(_) => ClassRef::CLOSURE,
            Shape::Env => ClassRef::ENV_RECORD,
            Shape::Vector => ClassRef::VECTOR,
            Shape::Tuple => ClassRef::TUPLE,
            Shape::Instance(class) => ClassRef::from_class_id(class),
            Shape::Variable => ClassRef::VARIABLE,
        }
    }

    fn indexed(self) -> bool {
        matches!(self, Shape::Vector | Shape::Tuple)
    }
}

/// One object: an allocation of the function, or a merge of virtual objects
/// at a block parameter (`inst` is `None`).
#[derive(Clone)]
struct Obj {
    inst: Option<Inst>,
    /// The value that stands for the object: the allocation's result, or the
    /// merging parameter.
    result: Value,
    kind: RematKind,
    shape: Shape,
    nf: usize,
    /// The values the allocation stores in the fields.
    init: Vec<Value>,
    /// The operands of the allocation before the fields (an instance's).
    header: Vec<Value>,
    ty: Type,
    op: Op,
}

/// A merge of virtual objects: the block and the parameter.
type MergeSite = (Block, usize);

/// What a value is in a state: a plain value, or virtual object `k`.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
enum Key {
    Val(Value),
    Obj(u32),
}

#[derive(Clone, PartialEq, Debug)]
enum Slot {
    Virt(Vec<Value>),
    Mat(Value),
}

type State = Vec<Option<Slot>>;

fn slot(st: &State, k: u32) -> Option<&Slot> {
    st.get(k as usize).and_then(Option::as_ref)
}

fn set_slot(st: &mut State, k: u32, s: Option<Slot>) {
    let k = k as usize;
    if st.len() <= k {
        st.resize(k + 1, None);
    }
    st[k] = s;
}

fn is_virt(s: Option<&Slot>) -> bool {
    matches!(s, Some(Slot::Virt(_)))
}

/// An edge: the block it leaves and its index among the block's edges.
type EdgeId = (Block, usize);

/// Where emitted code goes.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Place {
    Before(Inst),
    End(Block),
    Edge(EdgeId),
}

/// The frame state of an emitted instruction.
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
enum FrameRef {
    Existing(FrameState),
    /// A frame with the values of the mapping replaced, without parents or
    /// virtuals.
    OsrCopy(FrameState, Vec<(Value, Value)>),
}

/// An instruction to emit. `result` is a temporary.
#[derive(Clone, Debug)]
struct Spec {
    op: Op,
    args: Vec<Value>,
    result: Option<Value>,
    frame: Option<FrameRef>,
}

/// The values a frame-state chain replaces and the virtual objects its head
/// describes.
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
struct Desc {
    frame: FrameState,
    mapping: Vec<(Value, Value)>,
    entries: Vec<VirtualObject>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
enum FrameKey {
    Inst(Inst),
    Term(Block),
}

/// A block parameter the pass adds: a temporary, its type, and its argument
/// on each incoming edge (in the order of `incoming`).
#[derive(Clone, Debug)]
struct ParamRec {
    value: Value,
    ty: Type,
    args: Vec<Option<Value>>,
}

/// The new terminator operands of a block.
#[derive(Clone, Debug)]
enum TermRewrite {
    Branch(Value),
    Switch(Value),
    Return(Value),
    ReturnMany(Vec<Value>),
    TailCall(Value, Vec<Value>),
}

/// Constraints that persist across the runs of the analysis. They only grow.
#[derive(Default)]
struct Constraints {
    restart: bool,
    /// Objects to materialize on an edge, sorted.
    edge_mats: HashMap<EdgeId, Vec<u32>>,
    /// A field of an object that is a parameter of a header.
    phis: HashSet<(Block, u32, usize)>,
    /// An object to materialize before a call.
    hazards: HashSet<(u32, Inst)>,
    /// Candidates treated as ordinary allocations.
    excluded: HashSet<u32>,
    /// An object used after a merge although some entering edge has it
    /// virtual and another materialized: it is materialized on the virtual
    /// edges. Without the entry, the merge forgets the object.
    live: HashSet<(Block, u32)>,
    /// Objects that are never imported at an OSR entry.
    osr_banned: HashSet<u32>,
    /// Loop parameters that merge the entering object with the ones of the
    /// back edges.
    loop_merges: HashSet<MergeSite>,
    /// Parameters that are never merged into a new virtual object.
    excluded_merges: HashSet<MergeSite>,
    /// The object of each merge, stable across the runs.
    merged: HashMap<MergeSite, u32>,
}

impl Constraints {
    fn restart(&mut self) -> Giveup {
        self.restart = true;
        Giveup
    }
}

pub(super) fn run(func: &mut Func) -> PassOutcome {
    let work = func.insts.len() + func.blocks.len();
    let outcome = PassOutcome {
        changed: false,
        work,
    };
    if !crate::runtime::codegen::jit::config::opt().pea {
        return outcome;
    }
    let dom = func.dom_tree();
    let rpo: Vec<Block> = dom.rpo().to_vec();
    let mut objs = Vec::new();
    let mut inst_obj: HashMap<Inst, u32> = HashMap::new();
    for &block in &rpo {
        for &inst in &func.blocks[block].insts {
            if let Some(obj) = candidate_of(func, inst) {
                inst_obj.insert(inst, objs.len() as u32);
                objs.push(obj);
            }
        }
    }
    if objs.is_empty() {
        return outcome;
    }
    let mut cs = Constraints::default();
    for _ in 0..MAX_RUNS {
        let result = Analysis::new(func, &rpo, &inst_obj, &mut objs, &mut cs).run();
        if cs.restart {
            cs.restart = false;
            continue;
        }
        let Ok(effects) = result else {
            return outcome;
        };
        if !effects.progress {
            return outcome;
        }
        let stats = (effects.removed_objects, effects.virtual_frames);
        if apply(func, effects, &objs, &rpo).is_err() {
            return outcome;
        }
        log::debug!(
            target: crate::heap::logging::JIT_LOG_TARGET,
            " pea: {} allocations removed, {} frames with virtual objects",
            stats.0,
            stats.1
        );
        return PassOutcome {
            changed: true,
            work,
        };
    }
    outcome
}

fn candidate_of(func: &Func, inst: Inst) -> Option<Obj> {
    let data = &func.insts[inst];
    let result = data.result?;
    let args = func.args(inst);
    let n = args.len();
    let (kind, shape, init, header) = match data.op {
        Op::AllocPair if n == 2 => (RematKind::Pair, Shape::Pair, args.to_vec(), Vec::new()),
        Op::AllocClosure(code) => (
            RematKind::Closure {
                code,
                free: n as u32,
            },
            Shape::Closure(code),
            args.to_vec(),
            Vec::new(),
        ),
        Op::AllocEnv => (
            RematKind::Env { slots: n as u32 },
            Shape::Env,
            args.to_vec(),
            Vec::new(),
        ),
        Op::AllocVector => (
            RematKind::Vector { len: n as u32 },
            Shape::Vector,
            args.to_vec(),
            Vec::new(),
        ),
        Op::AllocTuple => (
            RematKind::Tuple { len: n as u32 },
            Shape::Tuple,
            args.to_vec(),
            Vec::new(),
        ),
        Op::AllocInstance { class, slots } if n == slots as usize + 3 => (
            RematKind::Instance { class, slots },
            Shape::Instance(class),
            args[3..].to_vec(),
            args[..3].to_vec(),
        ),
        Op::CallPrim { prim, .. }
            if n == 1 && Primitive::from_id(prim.index() as u16) == Some(Primitive::MakeBox) =>
        {
            (
                RematKind::Variable,
                Shape::Variable,
                args.to_vec(),
                Vec::new(),
            )
        }
        _ => return None,
    };
    if init.len() > MAX_FIELDS || !init.iter().all(|&a| func.value_type(a).is_tagged()) {
        return None;
    }
    Some(Obj {
        inst: Some(inst),
        result,
        kind,
        shape,
        nf: init.len(),
        init,
        header,
        ty: func.value_type(result),
        op: data.op,
    })
}

/// Where data field `field` of a `load`/`store` lives: a field index, or
/// the length word of a vector or tuple.
#[derive(Clone, Copy, PartialEq, Eq)]
enum FieldPos {
    Data(usize),
    Length,
}

fn field_pos(shape: Shape, nf: usize, field: u32) -> Option<FieldPos> {
    let field = field as usize;
    match shape {
        Shape::Pair => (field < 2).then_some(FieldPos::Data(field)),
        Shape::Env => (field >= 1 && field <= nf).then(|| FieldPos::Data(field - 1)),
        Shape::Tuple if field == 0 => Some(FieldPos::Length),
        Shape::Tuple => (field <= nf).then(|| FieldPos::Data(field - 1)),
        Shape::Vector if field == 0 => Some(FieldPos::Length),
        _ => None,
    }
}

/// Whether two virtual objects can be merged into one: the same kind and the
/// same header operands.
fn mergeable(a: &Obj, b: &Obj) -> bool {
    a.kind == b.kind
        && a.shape != Shape::Variable
        && a.header == b.header
        && matches!(
            a.op,
            Op::AllocPair
                | Op::AllocClosure(_)
                | Op::AllocEnv
                | Op::AllocVector
                | Op::AllocTuple
                | Op::AllocInstance { .. }
        )
        && a.op == b.op
}

/// Whether the call-flagged `op` can capture a continuation.
fn captures(op: Op) -> bool {
    if !op.is_call() || op == Op::Safepoint {
        return false;
    }
    match op {
        // The helpers are leaves of the runtime (numeric operations, slot
        // caches, list helpers): none calls Scheme code, and the ones that
        // fail deopt.
        Op::CallOp(_) => false,
        Op::CallPrim { effects, .. } => effects.flags.contains(EffectFlags::MAY_CALL),
        _ => true,
    }
}

/// The results of one run that `apply` performs.
struct Effects {
    progress: bool,
    removed_objects: usize,
    virtual_frames: usize,
    deleted: HashSet<Inst>,
    new_args: HashMap<Inst, Vec<Value>>,
    rewrites: HashMap<Inst, Constant>,
    /// Instructions that keep their result and frame but change their
    /// operation (a bounds check against a virtual object's length).
    retarget: HashMap<Inst, Op>,
    ins_before: HashMap<Inst, Vec<Spec>>,
    ins_end: HashMap<Block, Vec<Spec>>,
    ins_edge: HashMap<EdgeId, Vec<Spec>>,
    new_term: HashMap<Block, TermRewrite>,
    frame_desc: HashMap<FrameKey, Desc>,
    named: HashSet<u32>,
    temps: Vec<Type>,
    new_params: SecondaryMap<Block, Vec<ParamRec>>,
    removed: SecondaryMap<Block, Vec<usize>>,
    eargs: HashMap<EdgeId, Vec<Value>>,
    in_pos: HashMap<EdgeId, usize>,
    block_edges: SecondaryMap<Block, Vec<Block>>,
    live: SecondaryMap<Block, bool>,
    fwd: HashMap<Value, Value>,
    /// The merged objects whose alias needs a placeholder, by block.
    merge_placeholders: Vec<(Block, u32)>,
}

struct Analysis<'a, 'gc> {
    func: &'a Func<'gc>,
    rpo: &'a [Block],
    inst_obj: &'a HashMap<Inst, u32>,
    objs: &'a mut Vec<Obj>,
    cs: &'a mut Constraints,
    nv: usize,
    rpo_num: SecondaryMap<Block, i64>,
    reserved: HashSet<Block>,
    osr_blocks: HashSet<Block>,
    inst_block: HashMap<Inst, (Block, usize)>,

    alias: HashMap<Value, u32>,
    fwd: HashMap<Value, Value>,
    deleted: HashSet<Inst>,
    new_args: HashMap<Inst, Vec<Value>>,
    rewrites: HashMap<Inst, Constant>,
    /// Instructions that keep their result and frame but change their
    /// operation (a bounds check against a virtual object's length).
    retarget: HashMap<Inst, Op>,
    ins_before: HashMap<Inst, Vec<Spec>>,
    ins_end: HashMap<Block, Vec<Spec>>,
    ins_edge: HashMap<EdgeId, Vec<Spec>>,
    new_term: HashMap<Block, TermRewrite>,
    frame_desc: HashMap<FrameKey, Desc>,
    named: HashSet<u32>,
    temps: Vec<Type>,
    idx_table: HashMap<Value, i32>,
    incoming: SecondaryMap<Block, Vec<EdgeId>>,
    in_pos: HashMap<EdgeId, usize>,
    block_edges: SecondaryMap<Block, Vec<Block>>,
    has_back: SecondaryMap<Block, bool>,
    edge_out: HashMap<EdgeId, State>,
    eargs: HashMap<EdgeId, Vec<Value>>,
    block_in: HashMap<Block, State>,
    new_params: SecondaryMap<Block, Vec<ParamRec>>,
    param_rec: HashMap<Value, (Block, usize)>,
    removed: SecondaryMap<Block, Vec<usize>>,
    store_sites: Vec<Vec<Inst>>,
    load_sites: Vec<Vec<Inst>>,
    mat_count: Vec<usize>,
    allocated: Vec<bool>,
    dropped: Vec<Vec<Block>>,
    imported: Vec<bool>,
    /// The merge each object went into this run.
    merge_input: HashMap<u32, MergeSite>,
    /// Whether a merged object stands for an imported one.
    merge_imports: HashMap<u32, bool>,
    /// The merges made at a loop header, by object.
    loop_merge_of: HashMap<u32, Block>,
    call_obs: Vec<(Inst, Vec<u32>)>,
    progress: bool,
    /// Stores and write barriers folded into virtual objects, and the ones
    /// that materializing a cycle emits again. Folding is progress only when
    /// it removes more than it emits: otherwise a cycle of closures is
    /// folded and patched again on every round.
    stores_folded: usize,
    stores_emitted: usize,
    used: SecondaryMap<Value, bool>,
    reach_cache: HashMap<Block, SecondaryMap<Block, bool>>,
    ext: SecondaryMap<Block, bool>,
    live: SecondaryMap<Block, bool>,
    virtual_frames: usize,
}

impl<'a, 'gc> Analysis<'a, 'gc> {
    fn new(
        func: &'a Func<'gc>,
        rpo: &'a [Block],
        inst_obj: &'a HashMap<Inst, u32>,
        objs: &'a mut Vec<Obj>,
        cs: &'a mut Constraints,
    ) -> Self {
        let mut rpo_num: SecondaryMap<Block, i64> = SecondaryMap::with_default(-1);
        for (i, &b) in rpo.iter().enumerate() {
            rpo_num[b] = i as i64;
        }
        let mut reserved = HashSet::from([func.entry]);
        let mut osr_blocks = HashSet::new();
        for entry in &func.osr {
            reserved.insert(entry.block);
            osr_blocks.insert(entry.block);
        }
        for clause in &func.case_clauses {
            reserved.insert(clause.block);
        }
        let mut inst_block = HashMap::new();
        for &b in rpo {
            for (i, &inst) in func.blocks[b].insts.iter().enumerate() {
                inst_block.insert(inst, (b, i));
            }
        }
        let k = objs.len();
        Analysis {
            func,
            rpo,
            inst_obj,
            objs,
            cs,
            nv: func.values.len(),
            rpo_num,
            reserved,
            osr_blocks,
            inst_block,
            alias: HashMap::new(),
            fwd: HashMap::new(),
            deleted: HashSet::new(),
            new_args: HashMap::new(),
            rewrites: HashMap::new(),
            retarget: HashMap::new(),
            ins_before: HashMap::new(),
            ins_end: HashMap::new(),
            ins_edge: HashMap::new(),
            new_term: HashMap::new(),
            frame_desc: HashMap::new(),
            named: HashSet::new(),
            temps: Vec::new(),
            idx_table: HashMap::new(),
            incoming: SecondaryMap::new(),
            in_pos: HashMap::new(),
            block_edges: SecondaryMap::new(),
            has_back: SecondaryMap::new(),
            edge_out: HashMap::new(),
            eargs: HashMap::new(),
            block_in: HashMap::new(),
            new_params: SecondaryMap::new(),
            param_rec: HashMap::new(),
            removed: SecondaryMap::new(),
            store_sites: vec![Vec::new(); k],
            load_sites: vec![Vec::new(); k],
            mat_count: vec![0; k],
            allocated: vec![false; k],
            dropped: vec![Vec::new(); k],
            imported: vec![false; k],
            merge_input: HashMap::new(),
            merge_imports: HashMap::new(),
            loop_merge_of: HashMap::new(),
            call_obs: Vec::new(),
            progress: false,
            stores_folded: 0,
            stores_emitted: 0,
            used: SecondaryMap::new(),
            reach_cache: HashMap::new(),
            ext: SecondaryMap::new(),
            live: SecondaryMap::new(),
            virtual_frames: 0,
        }
    }

    /// Makes the per-object tables cover every object, after a merge added
    /// one.
    fn grow(&mut self) {
        let k = self.objs.len();
        self.store_sites.resize(k, Vec::new());
        self.load_sites.resize(k, Vec::new());
        self.mat_count.resize(k, 0);
        self.allocated.resize(k, false);
        self.dropped.resize(k, Vec::new());
        self.imported.resize(k, false);
    }

    fn obj(&self, k: u32) -> &Obj {
        &self.objs[k as usize]
    }

    fn rpo_num(&self, b: Block) -> i64 {
        self.rpo_num[b]
    }

    fn new_temp(&mut self, ty: Type) -> Value {
        let v = Value::new(self.nv + self.temps.len());
        self.temps.push(ty);
        v
    }

    fn vtype(&self, v: Value) -> Type {
        if v.index() < self.nv {
            self.func.value_type(v)
        } else {
            self.temps[v.index() - self.nv]
        }
    }

    fn res(&self, mut v: Value) -> Value {
        while let Some(&next) = self.fwd.get(&v) {
            v = next;
        }
        v
    }

    fn alias_of(&self, v: Value) -> Option<u32> {
        self.alias.get(&v).copied()
    }

    /// What `v` is in `st`.
    fn key(&mut self, st: &State, v: Value) -> Res<Key> {
        let v = self.res(v);
        let Some(k) = self.alias_of(v) else {
            return Ok(Key::Val(v));
        };
        match slot(st, k) {
            None => Err(self.dead_use(k)),
            Some(Slot::Virt(_)) => Ok(Key::Obj(k)),
            Some(Slot::Mat(m)) => Ok(Key::Val(*m)),
        }
    }

    /// A use of object `k` where a merge forgot it: those merges must keep
    /// it, and the run starts again.
    fn dead_use(&mut self, k: u32) -> Giveup {
        if let Some(&site) = self.merge_input.get(&k) {
            self.cs.excluded_merges.insert(site);
            return self.cs.restart();
        }
        let blocks = std::mem::take(&mut self.dropped[k as usize]);
        if !blocks.is_empty() {
            for c in blocks {
                self.cs.live.insert((c, k));
            }
            return self.cs.restart();
        }
        Giveup
    }

    fn key_value(&self, key: Key) -> Value {
        match key {
            Key::Val(v) => v,
            Key::Obj(k) => self.obj(k).result,
        }
    }

    fn virtual_obj(&self, st: &State, v: Value) -> Option<u32> {
        let k = self.alias_of(self.res(v))?;
        is_virt(slot(st, k)).then_some(k)
    }

    fn add_constraint(&mut self, e: EdgeId, k: u32) {
        let list = self.cs.edge_mats.entry(e).or_default();
        if let Err(at) = list.binary_search(&k) {
            list.insert(at, k);
            self.cs.restart = true;
        }
    }

    fn add_constraints_fwd(&mut self, c: Block, k: u32) {
        let edges = self.incoming[c].clone();
        for e in edges {
            if self.rpo_num(e.0) < self.rpo_num(c) {
                self.add_constraint(e, k);
            }
        }
    }

    fn add_phi(&mut self, c: Block, k: u32, i: usize) {
        if self.cs.phis.insert((c, k, i)) {
            self.cs.restart = true;
        }
    }

    fn emit(
        &mut self,
        place: Place,
        op: Op,
        args: Vec<Value>,
        ty: Option<Type>,
        frame: Option<FrameRef>,
    ) -> Option<Value> {
        let result = ty.map(|ty| self.new_temp(ty));
        let spec = Spec {
            op,
            args,
            result,
            frame,
        };
        match place {
            Place::Before(inst) => self.ins_before.entry(inst).or_default().push(spec),
            Place::End(b) => self.ins_end.entry(b).or_default().push(spec),
            Place::Edge(e) => self.ins_edge.entry(e).or_default().push(spec),
        }
        result
    }

    /// Allocates virtual object `k` of `st`, and the virtual objects it
    /// reaches, at `place`.
    fn materialize(&mut self, st: &mut State, k: u32, place: Place) -> Res<()> {
        let mut patches: Vec<(u32, usize, u32)> = Vec::new();
        let mut placeholder: Option<Value> = None;
        self.materialize_one(
            st,
            k,
            place,
            &mut Vec::new(),
            &mut patches,
            &mut placeholder,
        )?;
        for (k, i, j) in patches {
            let o = self.obj(k).clone();
            let Some(Slot::Mat(m)) = slot(st, k).cloned() else {
                return Err(Giveup);
            };
            let Some(Slot::Mat(mj)) = slot(st, j).cloned() else {
                return Err(Giveup);
            };
            self.emit(place, Op::WriteBarrier, vec![m, mj], None, None);
            self.stores_emitted += 2;
            match o.shape {
                Shape::Pair => {
                    let store = Op::Store {
                        field: i as u32,
                        region: MemoryRegion::Heap,
                    };
                    self.emit(place, store, vec![m, mj], None, None);
                }
                Shape::Env => {
                    let store = Op::Store {
                        field: i as u32 + 1,
                        region: MemoryRegion::Heap,
                    };
                    self.emit(place, store, vec![m, mj], None, None);
                }
                Shape::Closure(_) => {
                    let store = Op::StoreFree { index: i as u32 };
                    self.emit(place, store, vec![m, mj], None, None);
                }
                Shape::Variable => {
                    self.emit(place, Op::StoreCell, vec![m, mj], None, None);
                }
                Shape::Vector | Shape::Tuple => {
                    let index = self
                        .emit(
                            place,
                            Op::Const(Constant::Int32(i as i32)),
                            Vec::new(),
                            Some(Type::Int32),
                            None,
                        )
                        .expect("a constant has a result");
                    self.emit(place, Op::StoreElement, vec![m, index, mj], None, None);
                }
                Shape::Instance(_) => return Err(Giveup),
            }
        }
        Ok(())
    }

    fn materialize_one(
        &mut self,
        st: &mut State,
        k: u32,
        place: Place,
        stack: &mut Vec<u32>,
        patches: &mut Vec<(u32, usize, u32)>,
        placeholder: &mut Option<Value>,
    ) -> Res<()> {
        let o = self.obj(k).clone();
        if o.shape == Shape::Variable {
            self.cs.excluded.insert(k);
            return Err(self.cs.restart());
        }
        let Some(Slot::Virt(fields)) = slot(st, k).cloned() else {
            return Err(Giveup);
        };
        let mut vals = Vec::with_capacity(fields.len());
        for (i, &fv) in fields.iter().enumerate() {
            match self.key(st, fv)? {
                Key::Val(v) => vals.push(v),
                Key::Obj(j) => {
                    if j == k || stack.contains(&j) {
                        patches.push((k, i, j));
                        let p = match *placeholder {
                            Some(p) => p,
                            None => {
                                let p = self
                                    .emit(
                                        place,
                                        Op::Const(Constant::Bits(SchemeValue::VALUE_FALSE as u64)),
                                        Vec::new(),
                                        Some(Type::Any),
                                        None,
                                    )
                                    .expect("a constant has a result");
                                *placeholder = Some(p);
                                p
                            }
                        };
                        vals.push(p);
                    } else {
                        stack.push(k);
                        self.materialize_one(st, j, place, stack, patches, placeholder)?;
                        stack.pop();
                        let x = self.key(st, fv)?;
                        vals.push(self.key_value(x));
                    }
                }
            }
        }
        let mut args = o.header.clone();
        args.extend(vals);
        let m = self
            .emit(place, o.op, args, Some(o.ty), None)
            .expect("an allocation has a result");
        self.mat_count[k as usize] += 1;
        set_slot(st, k, Some(Slot::Mat(m)));
        Ok(())
    }

    /// A plain value for `v` at `place`: a virtual object is materialized
    /// first.
    fn use_value(&mut self, st: &mut State, v: Value, place: Place) -> Res<Value> {
        match self.key(st, v)? {
            Key::Val(x) => Ok(x),
            Key::Obj(k) => {
                self.materialize(st, k, place)?;
                let x = self.key(st, v)?;
                Ok(self.key_value(x))
            }
        }
    }

    /// The values of the chain of `fid` that the state replaces, the virtual
    /// entries it needs, and the virtual objects it reaches.
    fn describe_once(
        &mut self,
        st: &State,
        fid: FrameState,
    ) -> Res<(Vec<(Value, Value)>, Vec<VirtualObject>, Vec<u32>)> {
        let mut mapping: Vec<(Value, Value)> = Vec::new();
        let mut seen: Vec<u32> = Vec::new();
        let mut queue: Vec<u32> = Vec::new();
        let mut values = Vec::new();
        let mut cur = Some(fid);
        while let Some(fs) = cur {
            let data = &self.func.frames[fs];
            values.push(data.closure);
            values.extend(data.registers.iter().map(|&(_, v)| v));
            for object in &data.virtuals {
                values.push(object.alias);
                values.extend(object.fields.iter().copied());
            }
            cur = data.parent;
        }
        for v in values {
            self.describe_value(st, v, &mut mapping, &mut seen, &mut queue)?;
        }
        let mut entries = Vec::new();
        let mut at = 0;
        while at < queue.len() {
            let k = queue[at];
            at += 1;
            let Some(Slot::Virt(fields)) = slot(st, k).cloned() else {
                return Err(Giveup);
            };
            let mut mapped = Vec::with_capacity(fields.len());
            for f in fields {
                mapped.push(self.describe_value(st, f, &mut mapping, &mut seen, &mut queue)?);
            }
            let o = self.obj(k);
            entries.push(VirtualObject {
                alias: o.result,
                kind: o.kind,
                fields: mapped,
            });
        }
        Ok((mapping, entries, seen))
    }

    fn describe_value(
        &mut self,
        st: &State,
        v: Value,
        mapping: &mut Vec<(Value, Value)>,
        seen: &mut Vec<u32>,
        queue: &mut Vec<u32>,
    ) -> Res<Value> {
        let r = self.res(v);
        let x = match self.alias_of(r) {
            Some(k) => match slot(st, k) {
                None => return Err(self.dead_use(k)),
                Some(Slot::Virt(_)) => {
                    if !seen.contains(&k) {
                        seen.push(k);
                        queue.push(k);
                    }
                    self.obj(k).result
                }
                Some(Slot::Mat(m)) => *m,
            },
            None => r,
        };
        if x != v && !mapping.iter().any(|&(from, _)| from == v) {
            mapping.push((v, x));
        }
        Ok(x)
    }

    /// An object of `seen` that is part of a cycle of virtual objects.
    fn find_cycle(&self, st: &State, seen: &[u32]) -> Option<u32> {
        let succs = |k: u32| -> Vec<u32> {
            let Some(Slot::Virt(fields)) = slot(st, k) else {
                return Vec::new();
            };
            fields
                .iter()
                .filter_map(|&f| {
                    let j = self.alias_of(self.res(f))?;
                    is_virt(slot(st, j)).then_some(j)
                })
                .collect()
        };
        for &k in seen {
            let mut work = succs(k);
            let mut visited = HashSet::new();
            while let Some(j) = work.pop() {
                if j == k {
                    return Some(k);
                }
                if visited.insert(j) {
                    work.extend(succs(j));
                }
            }
        }
        None
    }

    fn describe(&mut self, st: &mut State, fid: FrameState, key: FrameKey) -> Res<()> {
        let place = match key {
            FrameKey::Inst(inst) => Place::Before(inst),
            FrameKey::Term(b) => Place::End(b),
        };
        loop {
            let (mapping, entries, seen) = self.describe_once(st, fid)?;
            if let Some(k) = self.find_cycle(st, &seen) {
                self.materialize(st, k, place)?;
                continue;
            }
            if !mapping.is_empty() || !entries.is_empty() {
                self.named.extend(seen.iter().copied());
                if !entries.is_empty() {
                    self.progress = true;
                    self.virtual_frames += 1;
                }
                self.frame_desc.insert(
                    key,
                    Desc {
                        frame: fid,
                        mapping,
                        entries,
                    },
                );
            }
            return Ok(());
        }
    }

    fn make_param(&mut self, c: Block, ty: Type, fwd: &[EdgeId], vals: &[Value]) -> Res<Value> {
        if self.reserved.contains(&c) || self.ext[c] {
            return Err(Giveup);
        }
        let t = self.new_temp(ty);
        let mut args = vec![None; self.incoming[c].len()];
        for (e, &v) in fwd.iter().zip(vals) {
            args[self.in_pos[e]] = Some(v);
        }
        let index = self.new_params[c].len();
        self.new_params[c].push(ParamRec { value: t, ty, args });
        self.param_rec.insert(t, (c, index));
        Ok(t)
    }

    fn rec_of(&self, c: Block, v: Value) -> Option<usize> {
        match self.param_rec.get(&v) {
            Some(&(b, i)) if b == c => Some(i),
            _ => None,
        }
    }

    /// The fields of an object that is virtual on every entering edge.
    /// `fields[e]` are its fields on edge `e`, and `sts[e]` the state there.
    fn merge_fields(
        &mut self,
        c: Block,
        k: u32,
        fwd: &[EdgeId],
        sts: &[State],
        fields: &[Vec<Value>],
    ) -> Res<Vec<Value>> {
        let n = fields[0].len();
        let mut out = Vec::with_capacity(n);
        for i in 0..n {
            let mut keys = Vec::with_capacity(fwd.len());
            for (st, fs) in sts.iter().zip(fields) {
                keys.push(self.key(st, fs[i])?);
            }
            let forced = self.cs.phis.contains(&(c, k, i));
            if !forced && keys.iter().all(|&x| x == keys[0]) {
                out.push(self.key_value(keys[0]));
                continue;
            }
            for (&e, &x) in fwd.iter().zip(&keys) {
                if let Key::Obj(j) = x {
                    self.add_constraint(e, j);
                }
            }
            if keys.iter().all(|x| matches!(x, Key::Val(_))) {
                let vals: Vec<Value> = keys.iter().map(|&x| self.key_value(x)).collect();
                let ty = vals
                    .iter()
                    .map(|&v| self.vtype(v))
                    .reduce(Type::join)
                    .unwrap_or(Type::Any);
                out.push(self.make_param(c, ty, fwd, &vals)?);
            } else {
                out.push(self.key_value(keys[0]));
            }
        }
        Ok(out)
    }

    fn merge_mat(&mut self, c: Block, k: u32, fwd: &[EdgeId], vals: &[Value]) -> Res<Slot> {
        if vals.iter().all(|&v| v == vals[0]) {
            return Ok(Slot::Mat(vals[0]));
        }
        let ty = self.obj(k).ty;
        Ok(Slot::Mat(self.make_param(c, ty, fwd, vals)?))
    }

    /// The object that merges the virtual objects `inputs` at parameter
    /// `index` of `c`, or `None` when they cannot be merged.
    fn merged_object(&mut self, c: Block, index: usize, inputs: &[u32]) -> Res<Option<u32>> {
        let site = (c, index);
        if self.cs.excluded_merges.contains(&site) {
            return Ok(None);
        }
        let first = self.obj(inputs[0]).clone();
        if !inputs.iter().all(|&k| mergeable(&first, self.obj(k))) {
            return Ok(None);
        }
        let param = self.func.params(c)[index];
        if !self.func.value_type(param).is_tagged() {
            return Ok(None);
        }
        let m = match self.cs.merged.get(&site) {
            Some(&m) => {
                if self.objs[m as usize].kind != first.kind {
                    self.cs.excluded_merges.insert(site);
                    return Err(self.cs.restart());
                }
                m
            }
            None => {
                let m = self.objs.len() as u32;
                self.objs.push(Obj {
                    inst: None,
                    result: param,
                    kind: first.kind,
                    shape: first.shape,
                    nf: first.nf,
                    init: Vec::new(),
                    header: first.header.clone(),
                    ty: first.ty,
                    op: first.op,
                });
                self.cs.merged.insert(site, m);
                m
            }
        };
        self.grow();
        Ok(Some(m))
    }

    /// The frame state that the guards of an import deopt to.
    fn osr_frame(&self, p: Block, c: Block, av: &[Value]) -> Option<FrameRef> {
        let func = self.func;
        if let Some(fid) = func.blocks[p]
            .insts
            .iter()
            .find_map(|&inst| func.insts[inst].frame)
        {
            return Some(FrameRef::Existing(fid));
        }
        let entry = func.osr.iter().find(|entry| entry.block == p)?;
        let fid = func.blocks[c]
            .insts
            .iter()
            .find_map(|&inst| func.insts[inst].frame)?;
        let data = &func.frames[fid];
        if data.pc != entry.pc || data.parent.is_some() || !data.virtuals.is_empty() {
            return None;
        }
        let mapping = func
            .params(c)
            .iter()
            .copied()
            .zip(av.iter().copied())
            .collect();
        Some(FrameRef::OsrCopy(fid, mapping))
    }

    fn importable(&self, k: u32) -> bool {
        let o = self.obj(k);
        !self.cs.osr_banned.contains(&k)
            && o.inst
                .is_some_and(|inst| self.func.osr_unaliased.contains(&inst))
            && !matches!(o.shape, Shape::Instance(_))
            && o.nf <= MAX_IMPORT_FIELDS
    }

    /// Whether `w` is the result of a `check.class` of `class`.
    fn checked_class(&self, w: Value, class: ClassRef) -> bool {
        if w.index() >= self.nv {
            return false;
        }
        match self.func.value_def(w) {
            ValueDef::Inst(inst) => self.func.insts[inst].op == Op::CheckClass(class),
            ValueDef::Param { .. } => false,
        }
    }

    /// Makes object `k` virtual in `st`, the state on an OSR edge, with the
    /// fields loaded from the real value `w`.
    fn import_object(
        &mut self,
        st: &mut State,
        reference: &State,
        k: u32,
        w: Value,
        place: Place,
        frame: &FrameRef,
        stack: &mut Vec<u32>,
    ) -> Res<bool> {
        let o = self.obj(k).clone();
        let class = o.shape.class();
        let Some(Slot::Virt(ref_fields)) = slot(reference, k).cloned() else {
            return Ok(false);
        };
        let w1 = if self.checked_class(w, class) {
            w
        } else {
            self.emit(
                place,
                Op::CheckClass(class),
                vec![w],
                Some(Type::Heap(class)),
                Some(frame.clone()),
            )
            .expect("a check has a result")
        };
        let w2 = match o.shape {
            Shape::Closure(code) => self
                .emit(
                    place,
                    Op::CheckCode(code),
                    vec![w1],
                    Some(Type::Closure(Some(code))),
                    Some(frame.clone()),
                )
                .expect("a check has a result"),
            _ => w1,
        };
        let mut fields = vec![o.result; o.nf];
        set_slot(st, k, Some(Slot::Virt(fields.clone())));
        self.imported[k as usize] = true;
        for i in 0..o.nf {
            let loaded = match o.shape {
                Shape::Pair => self.emit(
                    place,
                    Op::Load {
                        field: i as u32,
                        region: MemoryRegion::Heap,
                    },
                    vec![w1],
                    Some(Type::Any),
                    None,
                ),
                Shape::Env => self.emit(
                    place,
                    Op::Load {
                        field: i as u32 + 1,
                        region: MemoryRegion::Heap,
                    },
                    vec![w1],
                    Some(Type::Any),
                    None,
                ),
                Shape::Closure(_) => self.emit(
                    place,
                    Op::LoadFree { index: i as u32 },
                    vec![w2],
                    Some(Type::Any),
                    None,
                ),
                Shape::Variable => self.emit(place, Op::LoadCell, vec![w1], Some(Type::Any), None),
                Shape::Vector | Shape::Tuple => {
                    let c = self
                        .emit(
                            place,
                            Op::Const(Constant::Bits(SchemeValue::from_i32(i as i32).bits())),
                            Vec::new(),
                            Some(Type::Any),
                            None,
                        )
                        .expect("a constant has a result");
                    let ix = self
                        .emit(
                            place,
                            Op::CheckIndex,
                            vec![w1, c],
                            Some(Type::Int32),
                            Some(frame.clone()),
                        )
                        .expect("a check has a result");
                    self.emit(place, Op::LoadElement, vec![w1, ix], Some(Type::Any), None)
                }
                Shape::Instance(_) => return Ok(false),
            }
            .expect("a load has a result");
            match self.key(reference, ref_fields[i])? {
                Key::Val(_) => fields[i] = loaded,
                Key::Obj(j) => {
                    if stack.contains(&j) {
                        return Ok(false);
                    }
                    if slot(st, j).is_some() {
                        fields[i] = self.obj(j).result;
                    } else if self.importable(j) {
                        stack.push(k);
                        let ok =
                            self.import_object(st, reference, j, loaded, place, frame, stack)?;
                        stack.pop();
                        if !ok {
                            return Ok(false);
                        }
                        fields[i] = self.obj(j).result;
                    } else {
                        return Ok(false);
                    }
                }
            }
            set_slot(st, k, Some(Slot::Virt(fields.clone())));
        }
        Ok(true)
    }

    /// The argument lists of the entering edges `fwd` of the header `c`, with
    /// the objects of the OSR edges imported. `sts` are the states on the
    /// edges, changed in place.
    fn import_osr_edges(
        &mut self,
        c: Block,
        fwd: &[EdgeId],
        sts: &mut [State],
        avs: Vec<Vec<Value>>,
    ) -> Res<Vec<Vec<Value>>> {
        let is_osr: Vec<bool> = fwd.iter().map(|e| self.osr_blocks.contains(&e.0)).collect();
        if !is_osr.iter().any(|&o| o) || is_osr.iter().all(|&o| o) {
            return Ok(avs);
        }
        let others: Vec<usize> = (0..fwd.len()).filter(|&i| !is_osr[i]).collect();
        let params = self.func.params(c).to_vec();
        let mut out = avs.clone();
        for (ei, &e) in fwd.iter().enumerate() {
            if !is_osr[ei] {
                continue;
            }
            let av = avs[ei].clone();
            let Some(frame) = self.osr_frame(e.0, c, &av) else {
                continue;
            };
            let mut done: Vec<u32> = Vec::new();
            for (i, &param) in params.iter().enumerate() {
                let Some(&w) = av.get(i) else {
                    continue;
                };
                if !self.func.value_type(param).is_tagged() || self.alias_of(self.res(w)).is_some()
                {
                    continue;
                }
                let mut keys = Vec::new();
                for &o in &others {
                    let st = sts[o].clone();
                    keys.push(self.key(&st, avs[o][i])?);
                }
                let Key::Obj(k) = keys[0] else {
                    continue;
                };
                if !keys.iter().all(|&x| x == keys[0]) {
                    continue;
                }
                if done.contains(&k) {
                    out[ei][i] = self.obj(k).result;
                } else if slot(&sts[ei], k).is_none() && self.importable(k) {
                    let saved = sts[ei].clone();
                    let reference = sts[others[0]].clone();
                    let mut st = std::mem::take(&mut sts[ei]);
                    let ok = self.import_object(
                        &mut st,
                        &reference,
                        k,
                        w,
                        Place::Edge(e),
                        &frame,
                        &mut Vec::new(),
                    )?;
                    if ok {
                        for j in 0..self.objs.len() as u32 {
                            if slot(&st, j).is_some() && slot(&saved, j).is_none() {
                                done.push(j);
                            }
                        }
                        out[ei][i] = self.obj(k).result;
                        sts[ei] = st;
                    } else {
                        sts[ei] = saved;
                    }
                }
            }
        }
        Ok(out)
    }

    fn merge_block(&mut self, c: Block) -> Res<State> {
        let inc = self.incoming[c].clone();
        let fwd: Vec<EdgeId> = inc
            .iter()
            .copied()
            .filter(|e| self.rpo_num(e.0) < self.rpo_num(c))
            .collect();
        let params = self.func.params(c).to_vec();
        let np = params.len();
        if fwd.is_empty() {
            return Ok(State::new());
        }
        let mut sts: Vec<State> = fwd.iter().map(|e| self.edge_out[e].clone()).collect();
        let avs: Vec<Vec<Value>> = fwd.iter().map(|e| self.edge_args(*e)).collect();
        let avs = self.import_osr_edges(c, &fwd, &mut sts, avs)?;
        if self.ext[c] {
            for (&e, st) in fwd.iter().zip(&sts) {
                for k in 0..st.len() as u32 {
                    if is_virt(slot(st, k)) {
                        self.add_constraint(e, k);
                    }
                }
            }
        }
        let mut out = State::new();
        // The objects that went into a new merged object: they are known
        // only through the parameter after this block.
        let mut merged_inputs: HashSet<u32> = HashSet::new();
        if !self.reserved.contains(&c) && !self.ext[c] {
            // The parameters each virtual object comes in.
            let mut positions: HashMap<u32, HashSet<usize>> = HashMap::new();
            let mut keys_of: Vec<Vec<Key>> = Vec::with_capacity(np);
            for i in 0..np {
                let mut keys = Vec::with_capacity(fwd.len());
                for (st, av) in sts.iter().zip(&avs) {
                    let k = self.key(st, av[i])?;
                    if let Key::Obj(k) = k {
                        positions.entry(k).or_default().insert(i);
                    }
                    keys.push(k);
                }
                keys_of.push(keys);
            }
            for (i, keys) in keys_of.iter().enumerate() {
                let param = params[i];
                let site = (c, i);
                let all_virtual = keys.iter().all(|k| matches!(k, Key::Obj(_)));
                let same = keys.iter().all(|&x| x == keys[0]);
                let loop_merge =
                    self.cs.loop_merges.contains(&site) && !self.cs.excluded_merges.contains(&site);
                if all_virtual && same && !loop_merge {
                    if self.func.value_type(param).is_tagged() {
                        let Key::Obj(k) = keys[0] else { unreachable!() };
                        self.alias.insert(param, k);
                        self.removed[c].push(i);
                    }
                    continue;
                }
                if !all_virtual || !(loop_merge || !same) {
                    continue;
                }
                let inputs: Vec<u32> = keys
                    .iter()
                    .map(|k| match k {
                        Key::Obj(k) => *k,
                        Key::Val(_) => unreachable!(),
                    })
                    .collect();
                // An input that comes in another parameter too would have two
                // names after the merge.
                if inputs.iter().any(|k| positions[k].len() > 1)
                    || inputs.iter().any(|k| merged_inputs.contains(k))
                {
                    continue;
                }
                let Some(m) = self.merged_object(c, i, &inputs)? else {
                    continue;
                };
                let fields: Vec<Vec<Value>> = inputs
                    .iter()
                    .zip(&sts)
                    .map(|(&k, st)| match slot(st, k) {
                        Some(Slot::Virt(fs)) => fs.clone(),
                        _ => unreachable!("the input is virtual"),
                    })
                    .collect();
                let merged = self.merge_fields(c, m, &fwd, &sts, &fields)?;
                set_slot(&mut out, m, Some(Slot::Virt(merged)));
                self.alias.insert(param, m);
                self.removed[c].push(i);
                self.allocated[m as usize] = true;
                let imported = inputs.iter().any(|&k| {
                    self.imported.get(k as usize).copied().unwrap_or(false)
                        || self.merge_imports.get(&k).copied().unwrap_or(false)
                });
                self.merge_imports.insert(m, imported);
                if self.has_back[c] {
                    self.loop_merge_of.insert(m, c);
                }
                for &k in &inputs {
                    merged_inputs.insert(k);
                    self.merge_input.insert(k, site);
                }
            }
        }
        // Every other argument is an escape of the virtual objects it names.
        for ((&e, mut st), av) in fwd.iter().zip(sts.clone()).zip(&avs) {
            let mut fin = Vec::with_capacity(np);
            for (i, &a) in av.iter().enumerate().take(np) {
                if self.removed[c].contains(&i) {
                    continue;
                }
                fin.push(self.use_value(&mut st, a, Place::Edge(e))?);
            }
            self.eargs.insert(e, fin);
            self.edge_out.insert(e, st);
        }
        let sts: Vec<State> = fwd.iter().map(|e| self.edge_out[e].clone()).collect();
        let k_total = self.objs.len() as u32;
        for k in 0..k_total {
            if slot(&out, k).is_some() {
                // A merged object, made above.
                continue;
            }
            if merged_inputs.contains(&k) {
                continue;
            }
            let ss: Vec<Option<&Slot>> = sts.iter().map(|st| slot(st, k)).collect();
            if ss.iter().any(Option::is_none) {
                continue;
            }
            if ss.iter().all(|s| is_virt(*s)) {
                let fields: Vec<Vec<Value>> = ss
                    .iter()
                    .map(|s| match s {
                        Some(Slot::Virt(fs)) => fs.clone(),
                        _ => unreachable!(),
                    })
                    .collect();
                let merged = self.merge_fields(c, k, &fwd, &sts, &fields)?;
                set_slot(&mut out, k, Some(Slot::Virt(merged)));
            } else if ss.iter().all(|s| matches!(s, Some(Slot::Mat(_)))) {
                let vals: Vec<Value> = ss
                    .iter()
                    .map(|s| match s {
                        Some(Slot::Mat(v)) => *v,
                        _ => unreachable!(),
                    })
                    .collect();
                let s = self.merge_mat(c, k, &fwd, &vals)?;
                set_slot(&mut out, k, Some(s));
            } else if self.cs.live.contains(&(c, k)) {
                for (&e, s) in fwd.iter().zip(&ss) {
                    if is_virt(*s) {
                        self.add_constraint(e, k);
                    }
                }
            } else {
                self.dropped[k as usize].push(c);
            }
        }
        Ok(out)
    }

    fn edge_args(&self, (b, k): EdgeId) -> Vec<Value> {
        let term = self.func.blocks[b]
            .term
            .as_ref()
            .expect("a block with edges");
        let edge = term.edges()[k];
        self.func.edge_args(&edge).to_vec()
    }

    /// The back edge `e` into the header `c`: checks what the merge assumed.
    fn finish_back_edge(&mut self, e: EdgeId, c: Block) -> Res<()> {
        let mut st = self.edge_out[&e].clone();
        let av = self.edge_args(e);
        let params = self.func.params(c).to_vec();
        let hin = self.block_in[&c].clone();
        let rem = self.removed[c].clone();
        let pos = self.in_pos[&e];
        let mut fin = Vec::new();
        // The merged objects of this header whose back-edge object was
        // checked through their parameter.
        let mut through_param: HashSet<u32> = HashSet::new();
        // How many parameters each virtual object of the edge comes in.
        let mut positions: HashMap<u32, usize> = HashMap::new();
        for &a in &av {
            if let Key::Obj(k) = self.key(&st, a)? {
                *positions.entry(k).or_default() += 1;
            }
        }
        for (i, _) in params.iter().enumerate() {
            if rem.contains(&i) {
                let kk = self
                    .alias_of(params[i])
                    .expect("a removed parameter is an alias");
                let x = self.key(&st, av[i])?;
                let site = (c, i);
                let merged_here = self.cs.merged.get(&site) == Some(&kk)
                    && self.loop_merge_of.get(&kk) == Some(&c);
                if x == Key::Obj(kk) {
                    if merged_here {
                        through_param.insert(kk);
                        self.check_back_fields(&st, &hin, c, e, pos, kk, kk)?;
                    }
                    continue;
                }
                // A fresh object of the same kind on the back edge: one
                // virtual object for every iteration.
                let fresh = match x {
                    Key::Obj(kb) => {
                        slot(&hin, kb).is_none()
                            && positions.get(&kb) == Some(&1)
                            && mergeable(self.obj(kk), self.obj(kb))
                            && !self.cs.excluded_merges.contains(&site)
                    }
                    Key::Val(_) => false,
                };
                if fresh && merged_here {
                    let Key::Obj(kb) = x else { unreachable!() };
                    through_param.insert(kk);
                    self.check_back_fields(&st, &hin, c, e, pos, kk, kb)?;
                    continue;
                }
                if fresh {
                    // Merge on the next run.
                    if self.cs.loop_merges.insert(site) {
                        self.cs.restart = true;
                    }
                    continue;
                }
                if merged_here {
                    self.cs.excluded_merges.insert(site);
                    self.cs.restart = true;
                    continue;
                }
                self.add_constraints_fwd(c, kk);
                continue;
            }
            fin.push(self.use_value(&mut st, av[i], Place::Edge(e))?);
        }
        self.eargs.insert(e, fin);
        for kk in 0..hin.len() as u32 {
            let Some(sh) = slot(&hin, kk).cloned() else {
                continue;
            };
            if through_param.contains(&kk) || self.loop_merge_of.get(&kk) == Some(&c) {
                continue;
            }
            let s = slot(&st, kk).cloned();
            match sh {
                Slot::Virt(_) => {
                    if !is_virt(s.as_ref()) {
                        self.add_constraints_fwd(c, kk);
                    } else {
                        self.check_back_fields(&st, &hin, c, e, pos, kk, kk)?;
                    }
                }
                Slot::Mat(hv) => {
                    let Some(Slot::Mat(mv)) = s else {
                        return Err(Giveup);
                    };
                    match self.rec_of(c, hv) {
                        Some(r) => self.new_params[c][r].args[pos] = Some(mv),
                        None if hv == mv => {}
                        None => return Err(Giveup),
                    }
                }
            }
        }
        self.edge_out.insert(e, st);
        Ok(())
    }

    /// Checks the fields of object `src` on the back edge `e` against the
    /// fields header object `kk` has at the head of `c`.
    #[allow(clippy::too_many_arguments)]
    fn check_back_fields(
        &mut self,
        st: &State,
        hin: &State,
        c: Block,
        e: EdgeId,
        pos: usize,
        kk: u32,
        src: u32,
    ) -> Res<()> {
        let Some(Slot::Virt(header)) = slot(hin, kk).cloned() else {
            return Err(Giveup);
        };
        let Some(Slot::Virt(fields)) = slot(st, src).cloned() else {
            // Not virtual on the back edge: the merge cannot hold.
            if self.loop_merge_of.get(&kk) == Some(&c) {
                for (&site, &m) in &self.cs.merged {
                    if m == kk {
                        self.cs.excluded_merges.insert(site);
                    }
                }
                return Err(self.cs.restart());
            }
            self.add_constraints_fwd(c, kk);
            return Ok(());
        };
        for i in 0..header.len() {
            let hv = header[i];
            let ke = self.key(st, fields[i])?;
            match self.rec_of(c, hv) {
                Some(r) => match ke {
                    Key::Obj(j) => self.add_constraint(e, j),
                    Key::Val(v) => self.new_params[c][r].args[pos] = Some(v),
                },
                None => {
                    if ke != self.key(hin, hv)? {
                        self.add_phi(c, kk, i);
                    }
                }
            }
        }
        Ok(())
    }

    /// The state on the edge `(p, k)`: the block's final state plus the
    /// materializations that a constraint asks for.
    fn edge_state(&mut self, st: &State, p: Block, k: usize) -> Res<State> {
        let Some(list) = self.cs.edge_mats.get(&(p, k)).cloned() else {
            return Ok(st.clone());
        };
        let mut c = st.clone();
        for o in list {
            if is_virt(slot(&c, o)) {
                self.materialize(&mut c, o, Place::Edge((p, k)))?;
            }
        }
        Ok(c)
    }

    fn reach_set(&mut self, b: Block) -> &SecondaryMap<Block, bool> {
        if !self.reach_cache.contains_key(&b) {
            let mut seen: SecondaryMap<Block, bool> = SecondaryMap::new();
            let mut work: Vec<Block> = self.block_edges[b].clone();
            while let Some(x) = work.pop() {
                if seen[x] {
                    continue;
                }
                seen[x] = true;
                work.extend(self.block_edges[x].iter().copied());
            }
            self.reach_cache.insert(b, seen);
        }
        &self.reach_cache[&b]
    }

    /// Whether instruction `s` can run after the call `c`.
    fn after(&mut self, c: Inst, s: Inst) -> bool {
        let (bc, ic) = self.inst_block[&c];
        let (bs, is) = self.inst_block[&s];
        (bc == bs && is > ic) || self.reach_set(bc)[bs]
    }

    fn index_of(&self, v: Value) -> Option<i32> {
        let v = self.res(v);
        if let Some(&i) = self.idx_table.get(&v) {
            return Some(i);
        }
        if v.index() >= self.nv {
            return None;
        }
        let ValueDef::Inst(def) = self.func.value_def(v) else {
            return None;
        };
        match self.func.insts[def].op {
            Op::Const(Constant::Int32(n)) => Some(n),
            Op::Const(Constant::Bits(bits)) => SchemeValue::from_raw(bits).int32(),
            Op::Unbox(crate::runtime::codegen::opt::ir::inst::Repr::Int32) => {
                let arg = self.func.args(def)[0];
                self.func
                    .const_bits_of(arg)
                    .and_then(|bits| SchemeValue::from_raw(bits).int32())
            }
            _ => None,
        }
    }

    /// The plain, tagged values of `fields` in `st`, or `None` when one of
    /// them is a virtual object (a select cannot choose an object that does
    /// not exist).
    fn plain_fields(&mut self, st: &State, fields: &[Value]) -> Res<Option<Vec<Value>>> {
        let mut out = Vec::with_capacity(fields.len());
        for &f in fields {
            match self.key(st, f)? {
                Key::Val(v) if self.vtype(v).is_tagged() => out.push(v),
                _ => return Ok(None),
            }
        }
        Ok(Some(out))
    }

    /// `index == j`, as a Bool before `inst`.
    fn index_is(&mut self, inst: Inst, index: Value, j: usize) -> Value {
        let place = Place::Before(inst);
        let c = self
            .emit(
                place,
                Op::Const(Constant::Int32(j as i32)),
                Vec::new(),
                Some(Type::Int32),
                None,
            )
            .expect("a constant has a result");
        self.emit(
            place,
            Op::CmpI32(Cmp::Eq),
            vec![index, c],
            Some(Type::Bool),
            None,
        )
        .expect("a compare has a result")
    }

    /// Element `index` of a virtual vector or tuple whose index is not a
    /// constant: `select(index == n-1, f[n-1], ... select(index == 1, f[1],
    /// f[0]))`. A bounds check before the read keeps the index in range.
    fn select_element(
        &mut self,
        st: &State,
        inst: Inst,
        fields: &[Value],
        index: Value,
    ) -> Res<Option<Value>> {
        let index = self.res(index);
        if fields.is_empty() || self.vtype(index) != Type::Int32 {
            return Ok(None);
        }
        let Some(vals) = self.plain_fields(st, fields)? else {
            return Ok(None);
        };
        let mut r = vals[0];
        for (j, &v) in vals.iter().enumerate().skip(1) {
            if v == r {
                continue;
            }
            let cond = self.index_is(inst, index, j);
            let ty = Type::join(self.vtype(v), self.vtype(r));
            r = self
                .emit(
                    Place::Before(inst),
                    Op::Select,
                    vec![cond, v, r],
                    Some(ty),
                    None,
                )
                .expect("a select has a result");
        }
        Ok(Some(r))
    }

    /// The fields of a virtual vector or tuple after a write of `value` at an
    /// index that is not a constant: `f[j] = select(index == j, value, f[j])`.
    fn select_store(
        &mut self,
        st: &State,
        inst: Inst,
        fields: &[Value],
        index: Value,
        value: Value,
    ) -> Res<Option<Vec<Value>>> {
        let index = self.res(index);
        let Key::Val(value) = self.key(st, value)? else {
            return Ok(None);
        };
        if self.vtype(index) != Type::Int32 || !self.vtype(value).is_tagged() {
            return Ok(None);
        }
        let Some(vals) = self.plain_fields(st, fields)? else {
            return Ok(None);
        };
        let mut out = Vec::with_capacity(vals.len());
        for (j, &f) in vals.iter().enumerate() {
            if f == value {
                out.push(f);
                continue;
            }
            let cond = self.index_is(inst, index, j);
            let ty = Type::join(self.vtype(value), self.vtype(f));
            let v = self
                .emit(
                    Place::Before(inst),
                    Op::Select,
                    vec![cond, value, f],
                    Some(ty),
                    None,
                )
                .expect("a select has a result");
            out.push(v);
        }
        Ok(Some(out))
    }

    /// An operation whose first operand is the virtual object `k`. Returns
    /// whether the operation was handled; an unhandled one is an escape.
    fn virtual_op(&mut self, inst: Inst, st: &mut State, k: u32) -> Res<bool> {
        let func = self.func;
        let data = &func.insts[inst];
        let op = data.op;
        let args = func.args(inst).to_vec();
        let result = data.result;
        let o = self.obj(k).clone();
        let Some(Slot::Virt(fields)) = slot(st, k).cloned() else {
            return Ok(false);
        };
        let nf = o.nf;
        let shape = o.shape;
        macro_rules! delete {
            () => {{
                self.deleted.insert(inst);
                self.progress = true;
                return Ok(true);
            }};
        }
        macro_rules! forward {
            ($v:expr) => {{
                let v = $v;
                let r = result.expect("checked");
                self.fwd.insert(r, v);
                self.load_sites[k as usize].push(inst);
                delete!()
            }};
        }
        macro_rules! set_field {
            ($i:expr, $v:expr) => {{
                let stored = self.key(st, $v)?;
                let stored = self.key_value(stored);
                let mut fs = fields.clone();
                fs[$i] = stored;
                set_slot(st, k, Some(Slot::Virt(fs)));
                self.store_sites[k as usize].push(inst);
                self.stores_folded += 1;
                self.deleted.insert(inst);
                return Ok(true);
            }};
        }
        macro_rules! constant {
            ($c:expr) => {{
                self.rewrites.insert(inst, $c);
                self.progress = true;
                return Ok(true);
            }};
        }
        macro_rules! boolean {
            ($b:expr) => {
                constant!(Constant::Bits(u64::from($b)))
            };
        }
        match op {
            Op::Load {
                field,
                region: MemoryRegion::Heap,
            } => match field_pos(shape, nf, field) {
                Some(FieldPos::Length) => {
                    if result.is_some_and(|r| func.value_type(r) == Type::Int32) {
                        constant!(Constant::Int32(nf as i32))
                    }
                }
                Some(FieldPos::Data(i)) if result.is_some() => forward!(fields[i]),
                _ => {}
            },
            Op::Store {
                field,
                region: MemoryRegion::Heap,
            } => {
                if let Some(FieldPos::Data(i)) = field_pos(shape, nf, field) {
                    set_field!(i, args[1])
                }
            }
            Op::WriteBarrier => {
                self.stores_folded += 1;
                self.deleted.insert(inst);
                return Ok(true);
            }
            Op::LoadFree { index } => {
                if matches!(shape, Shape::Closure(_)) && result.is_some() && (index as usize) < nf {
                    forward!(fields[index as usize])
                }
            }
            Op::StoreFree { index } => {
                if matches!(shape, Shape::Closure(_)) && (index as usize) < nf {
                    set_field!(index as usize, args[1])
                }
            }
            Op::LoadCell => {
                if shape == Shape::Variable && result.is_some() {
                    forward!(fields[0])
                }
            }
            Op::StoreCell => {
                if shape == Shape::Variable {
                    set_field!(0, args[1])
                }
            }
            Op::CallOp(helper) => {
                if shape == Shape::Variable
                    && helper == Helper::VariableSet.helper_ref()
                    && args.len() == 2
                    && result.is_none_or(|r| !self.used[r])
                {
                    set_field!(0, args[1])
                }
            }
            Op::VectorLength => {
                if shape.indexed() {
                    constant!(Constant::Int32(nf as i32))
                }
            }
            Op::CheckIndex | Op::CheckIndexRaw => {
                let constant_index = if op == Op::CheckIndex {
                    self.index_of(args[1])
                } else {
                    None
                };
                if !shape.indexed() {
                } else if let Some(i) = constant_index {
                    if i >= 0 && (i as usize) < nf {
                        if let Some(r) = result {
                            self.idx_table.insert(r, i);
                        }
                        constant!(Constant::Int32(i))
                    }
                } else if nf <= MAX_SELECT && result.is_some() {
                    // The same check against the length the object has.
                    let index = self.use_value(st, args[1], Place::Before(inst))?;
                    let check = Op::CheckBound {
                        len: nf as u32,
                        raw: op == Op::CheckIndexRaw,
                    };
                    self.retarget.insert(inst, check);
                    self.new_args.insert(inst, vec![index]);
                    self.progress = true;
                    if let Some(fid) = data.frame {
                        self.describe(st, fid, FrameKey::Inst(inst))?;
                    }
                    return Ok(true);
                }
            }
            Op::LoadElement => {
                if shape.indexed() && result.is_some() {
                    match self.index_of(args[1]) {
                        Some(i) => {
                            if i >= 0 && (i as usize) < nf {
                                forward!(fields[i as usize])
                            }
                        }
                        None if nf <= MAX_SELECT => {
                            if let Some(v) = self.select_element(st, inst, &fields, args[1])? {
                                forward!(v)
                            }
                        }
                        None => {}
                    }
                }
            }
            Op::StoreElement => {
                if shape.indexed() {
                    match self.index_of(args[1]) {
                        Some(i) => {
                            if i >= 0 && (i as usize) < nf {
                                set_field!(i as usize, args[2])
                            }
                        }
                        None if nf <= MAX_SELECT => {
                            if let Some(fs) =
                                self.select_store(st, inst, &fields, args[1], args[2])?
                            {
                                set_slot(st, k, Some(Slot::Virt(fs)));
                                self.store_sites[k as usize].push(inst);
                                self.stores_folded += 1;
                                self.deleted.insert(inst);
                                return Ok(true);
                            }
                        }
                        None => {}
                    }
                }
            }
            Op::CheckClass(class) => {
                if class.class_id() == shape.class().class_id() {
                    if let Some(r) = result {
                        self.alias.insert(r, k);
                    }
                    delete!()
                }
            }
            Op::CheckCode(code) => {
                if shape == Shape::Closure(code) {
                    if let Some(r) = result {
                        self.alias.insert(r, k);
                    }
                    delete!()
                }
            }
            Op::TestClass(class) => boolean!(class.class_id() == shape.class().class_id()),
            Op::TestPair => boolean!(shape == Shape::Pair),
            Op::TestHeap => boolean!(true),
            Op::TestCode(code) => boolean!(shape == Shape::Closure(code)),
            _ => {}
        }
        Ok(false)
    }

    fn generic(&mut self, inst: Inst, st: &mut State) -> Res<()> {
        let func = self.func;
        let args = func.args(inst).to_vec();
        let mut new: Option<Vec<Value>> = None;
        for (i, &a) in args.iter().enumerate() {
            let x = self.use_value(st, a, Place::Before(inst))?;
            if x != a {
                new.get_or_insert_with(|| args.clone())[i] = x;
            }
        }
        if let Some(new) = new {
            self.new_args.insert(inst, new);
        }
        if let Some(fid) = func.insts[inst].frame {
            if captures(func.insts[inst].op) {
                let (_, _, needed) = self.describe_once(st, fid)?;
                for &k in &needed {
                    if self.cs.hazards.contains(&(k, inst)) && is_virt(slot(st, k)) {
                        self.materialize(st, k, Place::Before(inst))?;
                    }
                }
                self.call_obs.push((inst, needed));
            }
            self.describe(st, fid, FrameKey::Inst(inst))?;
        }
        Ok(())
    }

    fn process_inst(&mut self, inst: Inst, st: &mut State) -> Res<()> {
        let func = self.func;
        let data = &func.insts[inst];
        let args = func.args(inst);
        if let Some(&k) = self.inst_obj.get(&inst)
            && !self.cs.excluded.contains(&k) {
                // A candidate: its fields are the values it was allocated with.
                let init = self.obj(k).init.clone();
                let mut fields = Vec::with_capacity(init.len());
                for a in init {
                    let x = self.key(st, a)?;
                    fields.push(self.key_value(x));
                }
                self.alias
                    .insert(data.result.expect("a candidate has a result"), k);
                set_slot(st, k, Some(Slot::Virt(fields)));
                self.allocated[k as usize] = true;
                self.deleted.insert(inst);
                return Ok(());
            }
        let is_eq = match data.op {
            Op::EqBits => Some(false),
            Op::CallPrim { prim, .. }
                if Primitive::from_id(prim.index() as u16) == Some(Primitive::IsEq) =>
            {
                Some(true)
            }
            _ => None,
        };
        if let (Some(tagged), [a, b]) = (is_eq, args) {
            let ka = self.virtual_obj(st, *a);
            let kb = self.virtual_obj(st, *b);
            if ka.is_some() || kb.is_some() {
                let truth = ka == kb;
                let c = if tagged {
                    Constant::Bits(if truth {
                        SchemeValue::VALUE_TRUE as u64
                    } else {
                        SchemeValue::VALUE_FALSE as u64
                    })
                } else {
                    Constant::Bits(u64::from(truth))
                };
                self.rewrites.insert(inst, c);
                self.progress = true;
                return Ok(());
            }
        }
        if let Some(&first) = args.first()
            && let Some(k) = self.virtual_obj(st, first)
                && self.virtual_op(inst, st, k)? {
                    return Ok(());
                }
        self.generic(inst, st)
    }

    fn process_term(&mut self, b: Block, st: &mut State) -> Res<()> {
        let func = self.func;
        let term = func.blocks[b]
            .term
            .clone()
            .expect("a reachable block has a terminator");
        let place = Place::End(b);
        match &term {
            Terminator::Branch { cond, .. } => {
                let v = self.use_value(st, *cond, place)?;
                self.new_term.insert(b, TermRewrite::Branch(v));
            }
            Terminator::Switch { value, .. } => {
                let v = self.use_value(st, *value, place)?;
                self.new_term.insert(b, TermRewrite::Switch(v));
            }
            Terminator::Return(value) => {
                let v = self.use_value(st, *value, place)?;
                self.new_term.insert(b, TermRewrite::Return(v));
            }
            Terminator::ReturnMany { values } => {
                let mut out = Vec::with_capacity(values.len());
                for &v in values {
                    out.push(self.use_value(st, v, place)?);
                }
                self.new_term.insert(b, TermRewrite::ReturnMany(out));
            }
            Terminator::TailCall {
                callee,
                args,
                frame,
                ..
            } => {
                let callee = self.use_value(st, *callee, place)?;
                let mut out = Vec::new();
                for &a in args.as_slice(&func.pool) {
                    out.push(self.use_value(st, a, place)?);
                }
                self.new_term.insert(b, TermRewrite::TailCall(callee, out));
                self.describe(st, *frame, FrameKey::Term(b))?;
            }
            Terminator::Deopt { frame } => {
                self.describe(st, *frame, FrameKey::Term(b))?;
            }
            Terminator::Jump(_) | Terminator::Unreachable => {}
        }
        let targets = self.block_edges[b].clone();
        for (k, &c) in targets.iter().enumerate() {
            let out = self.edge_state(st, b, k)?;
            self.edge_out.insert((b, k), out);
            if self.rpo_num(c) <= self.rpo_num(b) {
                self.finish_back_edge((b, k), c)?;
            }
        }
        Ok(())
    }

    fn tables(&mut self) {
        let func = self.func;
        for &b in self.rpo {
            let Some(term) = &func.blocks[b].term else {
                continue;
            };
            let edges = term.edges();
            self.block_edges[b] = edges.iter().map(|e| e.block).collect();
            for (k, e) in edges.iter().enumerate() {
                self.incoming[e.block].push((b, k));
                if self.rpo_num(e.block) <= self.rpo_num(b) {
                    self.has_back[e.block] = true;
                }
            }
        }
        // The blocks that something can enter. The entry, the OSR blocks
        // and the case-clause blocks are roots.
        let mut work: Vec<Block> = vec![func.entry];
        work.extend(self.reserved.iter().copied());
        while let Some(b) = work.pop() {
            if self.live[b] {
                continue;
            }
            self.live[b] = true;
            if let Some(term) = &func.blocks[b].term {
                work.extend(term.edges().iter().map(|e| e.block));
            }
        }
        // A live block that the entry does not reach, with an edge into a
        // reachable block: that edge carries plain values and is not in
        // `incoming`, so the target keeps its parameters.
        for b in func.blocks.keys() {
            if self.rpo_num(b) < 0 && self.live[b]
                && let Some(term) = &func.blocks[b].term {
                    for e in term.edges() {
                        self.ext[e.block] = true;
                    }
                }
        }
        for c in func.blocks.keys() {
            for (i, &e) in self.incoming[c].iter().enumerate() {
                self.in_pos.insert(e, i);
            }
        }
        // The values something reads.
        for (_, data) in func.blocks.iter() {
            for &inst in &data.insts {
                for &a in func.args(inst) {
                    self.used[a] = true;
                }
            }
            if let Some(term) = &data.term {
                for v in func.terminator_uses(term) {
                    self.used[v] = true;
                }
            }
        }
        for (_, fs) in func.frames.iter() {
            self.used[fs.closure] = true;
            for &(_, v) in &fs.registers {
                self.used[v] = true;
            }
            for o in &fs.virtuals {
                self.used[o.alias] = true;
                for &f in &o.fields {
                    self.used[f] = true;
                }
            }
        }
    }

    fn run(mut self) -> Res<Effects> {
        self.tables();
        let func = self.func;
        for &b in self.rpo {
            let mut st = if b == func.entry {
                State::new()
            } else {
                self.merge_block(b)?
            };
            if self.has_back[b] {
                self.block_in.insert(b, st.clone());
            }
            for &inst in &func.blocks[b].insts {
                self.process_inst(inst, &mut st)?;
            }
            self.process_term(b, &mut st)?;
        }
        if self.cs.restart {
            return Err(Giveup);
        }

        // The continuation hazard: an object that a call's frame names, and
        // that is stored to and read after the call.
        let obs = std::mem::take(&mut self.call_obs);
        for (call, needed) in &obs {
            for &k in needed {
                if self.cs.hazards.contains(&(k, *call)) {
                    continue;
                }
                let stores = self.store_sites[k as usize].clone();
                let loads = self.load_sites[k as usize].clone();
                if stores.iter().any(|&s| self.after(*call, s))
                    && loads.iter().any(|&s| self.after(*call, s))
                {
                    self.cs.hazards.insert((k, *call));
                    self.cs.restart = true;
                }
            }
        }

        let k_total = self.objs.len() as u32;
        for k in 0..k_total {
            let count = self.mat_count[k as usize];
            let merged_site = self
                .cs
                .merged
                .iter()
                .find(|&(_, &m)| m == k)
                .map(|(&site, _)| site);
            match merged_site {
                // A merged object that would be allocated at more than one
                // place, or that stands for an imported object and is
                // allocated, goes back to the plain rule.
                Some(site) => {
                    let imported = self.merge_imports.get(&k).copied().unwrap_or(false);
                    if (count > 1 || (imported && count > 0))
                        && self.cs.excluded_merges.insert(site)
                    {
                        self.cs.restart = true;
                    }
                }
                None => {
                    // An object that would be allocated at more than one
                    // place stays as the program has it: the function must
                    // never hold more allocations than before.
                    if count > 1 && self.cs.excluded.insert(k) {
                        self.cs.restart = true;
                    }
                    // An object imported at an OSR entry must not be
                    // materialized on any path.
                    if self.imported[k as usize] && count > 0 && self.cs.osr_banned.insert(k) {
                        self.cs.restart = true;
                    }
                }
            }
        }
        if self.cs.restart {
            return Err(Giveup);
        }

        // An allocation that no path materializes disappears: that is
        // progress too.
        if self.stores_folded > self.stores_emitted {
            self.progress = true;
        }
        let mut removed_objects = 0;
        for k in 0..k_total {
            if self.allocated[k as usize]
                && self.mat_count[k as usize] == 0
                && self.objs[k as usize].inst.is_some()
            {
                self.progress = true;
                removed_objects += 1;
            }
        }
        let mut merge_placeholders = Vec::new();
        for (&(c, _), &m) in &self.cs.merged {
            if self.named.contains(&m) && self.alias_of(self.objs[m as usize].result) == Some(m) {
                merge_placeholders.push((c, m));
            }
        }
        merge_placeholders.sort_by_key(|&(c, m)| (c.index(), m));
        Ok(Effects {
            progress: self.progress,
            removed_objects,
            virtual_frames: self.virtual_frames,
            deleted: self.deleted,
            new_args: self.new_args,
            rewrites: self.rewrites,
            retarget: self.retarget,
            ins_before: self.ins_before,
            ins_end: self.ins_end,
            ins_edge: self.ins_edge,
            new_term: self.new_term,
            frame_desc: self.frame_desc,
            named: self.named,
            temps: self.temps,
            new_params: self.new_params,
            removed: self.removed,
            eargs: self.eargs,
            in_pos: self.in_pos,
            block_edges: self.block_edges,
            live: self.live,
            fwd: self.fwd,
            merge_placeholders,
        })
    }
}

fn apply(func: &mut Func, fx: Effects, objs: &[Obj], rpo: &[Block]) -> Res<()> {
    let mut inst_cand: HashMap<Inst, u32> = HashMap::new();
    for (k, o) in objs.iter().enumerate() {
        if let Some(inst) = o.inst {
            inst_cand.insert(inst, k as u32);
        }
    }

    // The final arguments of every edge of the reachable blocks, or a giveup
    // before any change.
    let mut finals: HashMap<Block, Vec<Vec<Value>>> = HashMap::new();
    for &b in rpo {
        let mut list = Vec::new();
        for (k, &target) in fx.block_edges[b].iter().enumerate() {
            let base = fx.eargs.get(&(b, k)).ok_or(Giveup)?;
            let pos = fx.in_pos[&(b, k)];
            let mut args = base.clone();
            for rec in &fx.new_params[target] {
                args.push(rec.args[pos].ok_or(Giveup)?);
            }
            list.push(args);
        }
        finals.insert(b, list);
    }

    // A block that no root reaches may name the objects that the pass
    // removes in its frame states, and may pass arguments to blocks whose
    // parameters change. It stays, empty.
    let blocks: Vec<Block> = func.blocks.keys().collect();
    for &b in &blocks {
        if !fx.live[b] {
            let data = &mut func.blocks[b];
            data.insts.clear();
            data.term = Some(Terminator::Unreachable);
        }
    }

    // The temporaries become values, in order.
    let nv = func.values.len();
    for (t, &ty) in fx.temps.iter().enumerate() {
        let v = func.values.push(ValueData {
            ty,
            def: ValueDef::Param {
                block: func.entry,
                index: u32::MAX,
            },
        });
        debug_assert_eq!(v.index(), nv + t);
    }

    let mut frames = FrameMaker {
        cache: HashMap::new(),
        osr: HashMap::new(),
    };

    for &b in rpo {
        // Parameters.
        let params = func.params(b).to_vec();
        let rem = &fx.removed[b];
        let new_params = &fx.new_params[b];
        if !rem.is_empty() || !new_params.is_empty() {
            let mut all: Vec<Value> = params
                .iter()
                .enumerate()
                .filter(|(i, _)| !rem.contains(i))
                .map(|(_, &v)| v)
                .collect();
            for rec in new_params {
                func.values[rec.value].ty = rec.ty;
                all.push(rec.value);
            }
            for (j, &p) in all.iter().enumerate() {
                func.values[p].def = ValueDef::Param {
                    block: b,
                    index: j as u32,
                };
            }
            func.blocks[b].params = EntityList::from_slice(&all, &mut func.pool);
        }

        // Instructions.
        let mut out: Vec<Inst> = Vec::new();
        for &(c, m) in &fx.merge_placeholders {
            if c == b {
                let o = &objs[m as usize];
                let inst = make_inst(
                    func,
                    &mut frames,
                    &Spec {
                        op: Op::Remat(o.kind),
                        args: Vec::new(),
                        result: Some(o.result),
                        frame: None,
                    },
                );
                func.values[o.result].ty = o.ty;
                out.push(inst);
            }
        }
        let insts = func.blocks[b].insts.clone();
        for inst in insts {
            if let Some(specs) = fx.ins_before.get(&inst) {
                for spec in specs {
                    out.push(make_inst(func, &mut frames, spec));
                }
            }
            if fx.deleted.contains(&inst) {
                if let Some(&k) = inst_cand.get(&inst)
                    && fx.named.contains(&k) {
                        let data = &mut func.insts[inst];
                        data.op = Op::Remat(objs[k as usize].kind);
                        data.args = EntityList::new();
                        data.frame = None;
                        out.push(inst);
                    }
                continue;
            }
            if let Some(&c) = fx.rewrites.get(&inst) {
                let data = &mut func.insts[inst];
                data.op = Op::Const(c);
                data.args = EntityList::new();
                data.frame = None;
                out.push(inst);
                continue;
            }
            if let Some(&op) = fx.retarget.get(&inst) {
                func.insts[inst].op = op;
            }
            if let Some(args) = fx.new_args.get(&inst) {
                func.insts[inst].args = EntityList::from_slice(args, &mut func.pool);
            }
            if let Some(desc) = fx.frame_desc.get(&FrameKey::Inst(inst)) {
                let frame = frames.frame_for(func, desc);
                func.insts[inst].frame = Some(frame);
            }
            out.push(inst);
        }
        if let Some(specs) = fx.ins_end.get(&b) {
            for spec in specs {
                out.push(make_inst(func, &mut frames, spec));
            }
        }
        func.blocks[b].insts = out;

        // Terminator.
        let mut term = func.blocks[b]
            .term
            .clone()
            .expect("a reachable block has a terminator");
        if let Some(rw) = fx.new_term.get(&b) {
            match (&mut term, rw) {
                (Terminator::Branch { cond, .. }, TermRewrite::Branch(v)) => *cond = *v,
                (Terminator::Switch { value, .. }, TermRewrite::Switch(v)) => *value = *v,
                (Terminator::Return(value), TermRewrite::Return(v)) => *value = *v,
                (Terminator::ReturnMany { values }, TermRewrite::ReturnMany(vs)) => {
                    *values = vs.clone()
                }
                (Terminator::TailCall { callee, args, .. }, TermRewrite::TailCall(c, vs)) => {
                    *callee = *c;
                    *args = EntityList::from_slice(vs, &mut func.pool);
                }
                _ => return Err(Giveup),
            }
        }
        if let Some(desc) = fx.frame_desc.get(&FrameKey::Term(b)) {
            let frame = frames.frame_for(func, desc);
            match &mut term {
                Terminator::TailCall { frame: f, .. } | Terminator::Deopt { frame: f } => {
                    *f = frame
                }
                _ => {}
            }
        }
        let fin = &finals[&b];
        let mut new_edges: Vec<(Block, Vec<Value>)> = Vec::new();
        for (k, &target) in fx.block_edges[b].iter().enumerate() {
            match fx.ins_edge.get(&(b, k)) {
                None => new_edges.push((target, fin[k].clone())),
                Some(specs) => {
                    let split = func.new_block();
                    let mut list = Vec::with_capacity(specs.len());
                    for spec in specs {
                        list.push(make_inst(func, &mut frames, spec));
                    }
                    func.blocks[split].insts = list;
                    let edge = func.edge(target, &fin[k]);
                    func.blocks[split].term = Some(Terminator::Jump(edge));
                    new_edges.push((split, Vec::new()));
                }
            }
        }
        for (edge, (target, args)) in term.edges_mut().into_iter().zip(new_edges) {
            edge.block = target;
            edge.args = EntityList::from_slice(&args, &mut func.pool);
        }
        func.blocks[b].term = Some(term);
    }

    // Whatever still names a forwarded load reads its field value.
    if !fx.fwd.is_empty() {
        let mut subst = super::Subst::default();
        let mut keys: Vec<&Value> = fx.fwd.keys().collect();
        keys.sort_by_key(|v| v.index());
        for &from in keys {
            let mut to = fx.fwd[&from];
            while let Some(&next) = fx.fwd.get(&to) {
                to = next;
            }
            if subst.resolve(from) == from && from != to {
                subst.add(from, to);
            }
        }
        super::replace_uses(func, &subst);
    }
    Ok(())
}

struct FrameMaker {
    cache: HashMap<Desc, FrameState>,
    osr: HashMap<FrameRef, FrameState>,
}

impl FrameMaker {
    fn copy_chain(
        &mut self,
        func: &mut Func,
        fid: FrameState,
        mapping: &[(Value, Value)],
        entries: &[VirtualObject],
    ) -> FrameState {
        let data = func.frames[fid].clone();
        let g = |v: Value| {
            mapping
                .iter()
                .find(|&&(from, _)| from == v)
                .map_or(v, |&(_, to)| to)
        };
        let parent = data.parent.map(|p| self.copy_chain(func, p, mapping, &[]));
        let mut virtuals: Vec<VirtualObject> = data
            .virtuals
            .iter()
            .map(|o| VirtualObject {
                alias: g(o.alias),
                kind: o.kind,
                fields: o.fields.iter().map(|&f| g(f)).collect(),
            })
            .collect();
        virtuals.extend(entries.iter().cloned());
        func.push_frame(FrameStateData {
            code: data.code,
            pc: data.pc,
            closure: g(data.closure),
            registers: data.registers.iter().map(|&(r, v)| (r, g(v))).collect(),
            parent,
            call_base: data.call_base,
            returned: data.returned,
            rest: data.rest,
            inline_site: data.inline_site,
            virtuals,
        })
    }

    fn frame_for(&mut self, func: &mut Func, desc: &Desc) -> FrameState {
        if let Some(&fs) = self.cache.get(desc) {
            return fs;
        }
        let fs = self.copy_chain(func, desc.frame, &desc.mapping, &desc.entries);
        self.cache.insert(desc.clone(), fs);
        fs
    }

    fn resolve(&mut self, func: &mut Func, frame: &FrameRef) -> FrameState {
        match frame {
            FrameRef::Existing(fs) => *fs,
            FrameRef::OsrCopy(fid, mapping) => {
                if let Some(&fs) = self.osr.get(frame) {
                    return fs;
                }
                let data = func.frames[*fid].clone();
                let g = |v: Value| {
                    mapping
                        .iter()
                        .find(|&&(from, _)| from == v)
                        .map_or(v, |&(_, to)| to)
                };
                let fs = func.push_frame(FrameStateData {
                    code: data.code,
                    pc: data.pc,
                    closure: g(data.closure),
                    registers: data.registers.iter().map(|&(r, v)| (r, g(v))).collect(),
                    parent: None,
                    call_base: data.call_base,
                    returned: data.returned,
                    rest: data.rest,
                    inline_site: data.inline_site,
                    virtuals: Vec::new(),
                });
                self.osr.insert(frame.clone(), fs);
                fs
            }
        }
    }
}

fn make_inst(func: &mut Func, frames: &mut FrameMaker, spec: &Spec) -> Inst {
    let frame = spec.frame.as_ref().map(|f| frames.resolve(func, f));
    let args = EntityList::from_slice(&spec.args, &mut func.pool);
    let inst = func.insts.push(InstData {
        op: spec.op,
        args,
        result: spec.result,
        frame,
    });
    if let Some(r) = spec.result {
        func.values[r].def = ValueDef::Inst(inst);
    }
    inst
}
