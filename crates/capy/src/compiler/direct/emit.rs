//! Code generation of the seed emitter, see the module docs of `direct`.
//!
//! A [`FnCx`] compiles one procedure. It hands out frame slots like a stack (`top`), and tracks
//! which of them hold a value (`defined`): only those are in the liveness mask of a return
//! point, so the collector never reads a slot that was allocated but not yet written.

use std::cell::Cell;
use std::collections::{HashMap, HashSet};

use bytecodegen::{Label, LinkedBytecodeView};

use super::analysis::{Analysis, key};
use super::{DirectCode, DirectConst, DirectProgram};
use crate::bytecode::def::{self as bc, *};
use crate::expander::core::{
    GlobalSlot, LVarRef, LetStyle, Proc, ProcRef, Term, TermKind, TermRef, call_term, constant,
    fresh_lvar, if_term, let_term, lref, prim_call_term, proc_term,
};
use crate::heap::{Gc, Trace, alloc::Array, cell::Lock};
use crate::runtime::MutationContext;
use crate::runtime::fasl::SrcRun;
use crate::runtime::objectmodel::proc::SRC_FLAG_SYNTHETIC;
use crate::runtime::objectmodel::{Str, Symbol, Value};
use crate::runtime::prims::calls::PRIM_ARITY;
use crate::runtime::prims::def::Primitive;
use crate::runtime::vm::exceptions::RaiseKind;

/// Where the value of a term goes.
#[derive(Clone)]
enum Dest {
    /// Into this slot, which the caller defines afterwards.
    Reg(u32),
    /// Returned from the procedure.
    Tail,
    /// Dropped.
    Effect,
    /// Several values into these slots, and the rest as a list into the last.
    Multi(Vec<u32>, Option<u32>),
}

impl Dest {
    fn multi(targets: Vec<u32>, rest: Option<u32>) -> Dest {
        if targets.len() == 1 && rest.is_none() {
            Dest::Reg(targets[0])
        } else {
            Dest::Multi(targets, rest)
        }
    }
}

#[derive(Clone, Copy)]
struct Home {
    reg: u32,
    boxed: bool,
}

enum Atom {
    Ins(bc::LinkableInstruction),
    Label(Label),
}

/// An object the compiler made and holds only through Rust collections, which the collector
/// does not scan. The chain of these hangs off the compiler and the program.
#[derive(Trace)]
#[collect(no_drop)]
pub(crate) struct Keep<'gc> {
    term: Option<TermRef<'gc>>,
    value: Value<'gc>,
    next: Option<Gc<'gc, Keep<'gc>>>,
}

pub(super) struct Compiler<'gc> {
    keep: Cell<Option<Gc<'gc, Keep<'gc>>>>,
    ctx: MutationContext<'gc>,
    an: Analysis<'gc>,
    blocks: Vec<Option<DirectCode<'gc>>>,
    /// Linklet slots that may be unbound when read (`GlobalRef` checks them).
    checked: HashSet<usize>,
    /// `(#%capy . name)` cache keys, one pair per name so procedures share the cell.
    error: Option<String>,
}

impl<'gc> Compiler<'gc> {
    fn keep_term(&self, term: TermRef<'gc>) -> TermRef<'gc> {
        let node = self.ctx.allocate(Keep {
            term: Some(term),
            value: Value::new(false),
            next: self.keep.get(),
        });
        self.keep.set(Some(node));
        term
    }

    fn keep_value(&self, value: Value<'gc>) -> Value<'gc> {
        let node = self.ctx.allocate(Keep {
            term: None,
            value,
            next: self.keep.get(),
        });
        self.keep.set(Some(node));
        value
    }
}

pub(super) fn compile_program<'gc>(
    ctx: MutationContext<'gc>,
    il: TermRef<'gc>,
) -> Result<DirectProgram<'gc>, String> {
    let mut c = Compiler {
        keep: Cell::new(None),
        ctx,
        an: Analysis::new(ctx),
        blocks: Vec::new(),
        checked: HashSet::new(),
        error: None,
    };
    c.an.scan(il);
    let top = ctx.allocate(Proc {
        name: Value::new(false),
        source: il.source(),
        args: Array::from_slice(ctx, []),
        variadic: None,
        body: il,
        meta: Value::null(),
    });
    let top_term = ctx.allocate(Term {
        source: Lock::new(il.source()),
        kind: TermKind::Proc(top),
    });
    c.keep_term(top_term);
    let entry = FnCx::compile(&mut c, top);
    if let Some(err) = c.error {
        return Err(err);
    }
    Ok(DirectProgram {
        blocks: c
            .blocks
            .into_iter()
            .map(|block| block.expect("every code block is finished"))
            .collect(),
        entry,
        keep: c.keep.get(),
    })
}

struct FnCx<'a, 'gc> {
    c: &'a mut Compiler<'gc>,
    atoms: Vec<Atom>,
    sources: Vec<(usize, Value<'gc>)>,
    constants: Vec<DirectConst<'gc>>,
    value_index: HashMap<u64, u32>,
    code_index: HashMap<u32, u32>,
    cell_index: HashMap<u64, u32>,
    pool: Vec<u32>,
    mask_index: HashMap<Vec<u32>, u32>,
    homes: HashMap<usize, Home>,
    top: u32,
    max_reg: u32,
    defined: Vec<bool>,
    /// Atoms of `rest_list` instructions whose `base` is patched at the end.
    rest_fixups: Vec<usize>,
}

fn prim_id(prim: Primitive) -> PrimitiveId {
    PrimitiveId(prim.id() as u32)
}

/// Primitives that are not called through `call_primitive`: they work on the frame or have a
/// dedicated lowering, so they are called as procedures.
fn is_frame_primitive(prim: Primitive) -> bool {
    use Primitive as P;
    matches!(prim, P::CaseLambda | P::PushCframe | P::DefaultRetk)
}

/// Whether `prim` called with `argc` arguments is one `call_primitive` instruction.
fn has_direct_form(prim: Primitive, argc: usize) -> bool {
    !is_frame_primitive(prim)
        && !matches!(prim, Primitive::CacheRef | Primitive::CacheSet)
        && match PRIM_ARITY.get(prim.id() as usize) {
            Some(-1) => true,
            Some(arity) => *arity as usize == argc,
            None => false,
        }
}

impl<'a, 'gc> FnCx<'a, 'gc> {
    /// Compile `proc` to a code block of the program and return its index.
    fn compile(c: &'a mut Compiler<'gc>, proc: ProcRef<'gc>) -> u32 {
        let free = c.an.free_vars(proc);
        let index = c.blocks.len() as u32;
        c.blocks.push(None);
        let mut f = FnCx {
            c,
            atoms: Vec::new(),
            sources: Vec::new(),
            constants: Vec::new(),
            value_index: HashMap::new(),
            code_index: HashMap::new(),
            cell_index: HashMap::new(),
            pool: Vec::new(),
            mask_index: HashMap::new(),
            homes: HashMap::new(),
            top: 2,
            max_reg: 1,
            defined: Vec::new(),
            rest_fixups: Vec::new(),
        };
        let block = f.body(proc, &free);
        f.c.blocks[index as usize] = Some(block);
        index
    }

    fn body(&mut self, proc: ProcRef<'gc>, free: &[LVarRef<'gc>]) -> DirectCode<'gc> {
        self.note_source(Value::new(false));
        // The entry instruction needs the final frame size, it replaces this at the end.
        self.ins(OpNop {}.into());

        let nparams = proc.args.len() as u32;
        let mut to_box = Vec::new();
        for param in proc.args.iter() {
            let reg = self.alloc();
            self.define(reg);
            self.bind(*param, reg);
            to_box.push(*param);
        }
        let mut cnt = None;
        if let Some(rest) = proc.variadic {
            let count = self.alloc();
            let reg = self.alloc();
            cnt = Some(count);
            self.rest_fixups.push(self.atoms.len());
            self.ins(
                OpRestList {
                    dst: Register(reg),
                    cnt: Register(count),
                    base: Register(0),
                    skip: Count(0),
                }
                .into(),
            );
            self.define(reg);
            self.bind(rest, reg);
            to_box.push(rest);
        }
        for var in to_box {
            let home = self.home(var);
            if home.boxed {
                self.make_box(home.reg, home.reg);
            }
        }
        for (index, var) in free.iter().enumerate() {
            let reg = self.alloc();
            self.ins(
                OpFreeRef {
                    dst: Register(reg),
                    index: Count(index as u32),
                }
                .into(),
            );
            self.define(reg);
            self.bind(*var, reg);
        }

        self.comp(proc.body, Dest::Tail);

        let rest_base = self.max_reg + 1;
        let size = self.max_reg + 2;
        if size >= 65536 {
            self.c
                .error
                .get_or_insert_with(|| format!("frame of {} is too large", proc.name));
        }
        for atom in std::mem::take(&mut self.rest_fixups) {
            let Atom::Ins(bc::LinkableInstruction::OpRestList(op)) = &mut self.atoms[atom] else {
                unreachable!("rest fixup names a rest_list");
            };
            op.base = Register(rest_base);
        }
        self.atoms[0] = Atom::Ins(match cnt {
            Some(count) => OpEnterRest {
                required: Count(nparams),
                cnt: Register(count),
                base: Register(rest_base),
                frame: Count(size),
            }
            .into(),
            None => OpEnter {
                argc: Count(nparams),
                frame: Count(size),
            }
            .into(),
        });
        let arity = if proc.variadic.is_some() {
            -(nparams as i32 + 1)
        } else {
            nparams as i32
        };
        self.link(proc, arity, size)
    }

    // ----- frame slots

    fn touch(&mut self, reg: u32) {
        self.max_reg = self.max_reg.max(reg);
    }

    fn alloc(&mut self) -> u32 {
        let reg = self.top;
        self.top += 1;
        self.touch(reg);
        reg
    }

    fn define(&mut self, reg: u32) {
        let index = reg as usize;
        if self.defined.len() <= index {
            self.defined.resize(index + 1, false);
        }
        self.defined[index] = true;
    }

    fn mark(&self) -> u32 {
        self.top
    }

    /// Free every slot from `mark` up.
    fn release(&mut self, mark: u32) {
        for reg in mark..self.top {
            if let Some(slot) = self.defined.get_mut(reg as usize) {
                *slot = false;
            }
        }
        self.top = mark;
    }

    fn bind(&mut self, var: LVarRef<'gc>, reg: u32) {
        let boxed = self.c.an.is_boxed(var);
        self.homes.insert(key(var), Home { reg, boxed });
    }

    fn home(&self, var: LVarRef<'gc>) -> Home {
        *self
            .homes
            .get(&key(var))
            .unwrap_or_else(|| panic!("variable {} has no slot", var.name))
    }

    /// The pool reference of the mask of the defined slots below `below`.
    fn mask_ref(&mut self, below: u32) -> PoolReference {
        let mut bits: Vec<u32> = Vec::new();
        for reg in 2..below {
            if self.defined.get(reg as usize).copied().unwrap_or(false) {
                let word = (reg / 32) as usize;
                if bits.len() <= word {
                    bits.resize(word + 1, 0);
                }
                bits[word] |= 1 << (reg % 32);
            }
        }
        if let Some(index) = self.mask_index.get(&bits) {
            return PoolReference(*index);
        }
        let index = self.pool.len() as u32;
        self.pool.push(bits.len() as u32);
        self.pool.extend_from_slice(&bits);
        self.mask_index.insert(bits, index);
        PoolReference(index)
    }

    // ----- instructions

    fn ins(&mut self, ins: bc::LinkableInstruction) {
        self.atoms.push(Atom::Ins(ins));
    }

    fn label(&mut self, label: Label) {
        self.atoms.push(Atom::Label(label));
    }

    fn note_source(&mut self, source: Value<'gc>) {
        self.sources.push((self.atoms.len(), source));
    }

    fn mov(&mut self, dst: u32, src: u32) {
        if dst != src {
            self.touch(dst.max(src));
            self.ins(
                OpMove {
                    dst: Register(dst),
                    src: Register(src),
                }
                .into(),
            );
        }
    }

    fn value_constant(&mut self, value: Value<'gc>) -> u32 {
        if let Some(index) = self.value_index.get(&value.bits()) {
            return *index;
        }
        let index = self.constants.len() as u32;
        self.constants.push(DirectConst::Value(value));
        self.value_index.insert(value.bits(), index);
        index
    }

    fn code_constant(&mut self, code: u32) -> u32 {
        if let Some(index) = self.code_index.get(&code) {
            return *index;
        }
        let index = self.constants.len() as u32;
        self.constants.push(DirectConst::Code(code));
        self.code_index.insert(code, index);
        index
    }

    fn cell_constant(&mut self, key: Value<'gc>) -> u32 {
        if let Some(index) = self.cell_index.get(&key.bits()) {
            return *index;
        }
        let index = self.constants.len() as u32;
        self.constants.push(DirectConst::Cell(key));
        self.cell_index.insert(key.bits(), index);
        index
    }

    fn load_const(&mut self, dst: u32, value: Value<'gc>) {
        self.touch(dst);
        let dst = Register(dst);
        if value.is_int32() {
            self.ins(
                OpLoadImmediate {
                    dst,
                    value: Immediate(value.as_int32()),
                }
                .into(),
            );
        } else if !value.is_cell() && value.bits() <= u32::MAX as u64 {
            self.ins(
                OpLoadRaw {
                    dst,
                    bits: Count(value.bits() as u32),
                }
                .into(),
            );
        } else {
            let index = self.value_constant(value);
            self.ins(
                OpLoadConstant {
                    dst,
                    constant: Constant(index),
                }
                .into(),
            );
        }
    }

    fn prim1(&mut self, dst: u32, prim: Primitive, a: u32) {
        self.touch(dst);
        self.ins(
            OpCallPrimitive1 {
                dst: Register(dst),
                primitive: prim_id(prim),
                a: Register(a),
            }
            .into(),
        );
    }

    fn make_box(&mut self, dst: u32, value: u32) {
        self.prim1(dst, Primitive::MakeBox, value);
    }

    fn variable_ref(&mut self, dst: u32, var: u32) {
        self.touch(dst);
        let ins =
            OpVariableRef::new(Register(dst), Register(var)).with_metadata(Default::default());
        self.ins(ins);
    }

    fn variable_set(&mut self, var: u32, src: u32) {
        self.ins(
            OpVariableSet {
                var: Register(var),
                src: Register(src),
            }
            .into(),
        );
    }

    fn raise(&mut self, kind: RaiseKind, args: &[Value<'gc>]) {
        for (index, arg) in args.iter().enumerate() {
            self.load_const(2 + index as u32, *arg);
        }
        self.touch(2 + args.len() as u32);
        self.ins(
            OpRaise {
                kind: Count(kind.code() as u32),
                count: Count(args.len() as u32),
            }
            .into(),
        );
    }

    // ----- terms

    fn to_reg(&mut self, term: TermRef<'gc>, dst: u32) {
        self.comp(term, Dest::Reg(dst));
        self.define(dst);
    }

    /// A slot holding the value of `term`: the variable's own slot, or a fresh temporary that
    /// the caller releases.
    fn operand(&mut self, term: TermRef<'gc>) -> u32 {
        if let TermKind::LRef(var) = term.kind {
            let home = self.home(var);
            if !home.boxed {
                return home.reg;
            }
        }
        let reg = self.alloc();
        self.to_reg(term, reg);
        reg
    }

    fn comp(&mut self, term: TermRef<'gc>, dest: Dest) {
        match term.kind {
            TermKind::Seq(..) => {
                let mut current = term;
                while let TermKind::Seq(head, tail) = current.kind {
                    self.comp(head, Dest::Effect);
                    current = tail;
                }
                self.comp(current, dest);
            }
            TermKind::If(test, then, otherwise) => self.comp_if(test, then, otherwise, dest),
            TermKind::Let(let_) => {
                let mark = self.mark();
                match let_.style {
                    // `let` evaluates its inits right to left, as the Scheme compiler does
                    // (`convert-let-bindings`). The compiler's own boot code has side effects in
                    // `let` inits (psyntax allocating names and labels), so a seed-compiled
                    // compiler must run them in the same order for its output to match a
                    // compiler built by itself.
                    LetStyle::Let => {
                        let regs: Vec<_> = let_.lhs.iter().map(|_| self.alloc()).collect();
                        for (rhs, reg) in let_.rhs.iter().zip(regs.iter()).rev() {
                            self.to_reg(*rhs, *reg);
                        }
                        for (var, reg) in let_.lhs.iter().zip(regs) {
                            self.bind(*var, reg);
                            if self.home(*var).boxed {
                                self.make_box(reg, reg);
                            }
                        }
                    }
                    LetStyle::LetStar => {
                        for (var, rhs) in let_.lhs.iter().zip(let_.rhs.iter()) {
                            let reg = self.alloc();
                            self.to_reg(*rhs, reg);
                            self.bind(*var, reg);
                            if self.home(*var).boxed {
                                self.make_box(reg, reg);
                            }
                        }
                    }
                    LetStyle::LetRec | LetStyle::LetRecStar => {
                        for var in let_.lhs.iter() {
                            let reg = self.alloc();
                            self.load_const(reg, Value::undefined());
                            self.make_box(reg, reg);
                            self.define(reg);
                            self.bind(*var, reg);
                        }
                        for (var, rhs) in let_.lhs.iter().zip(let_.rhs.iter()) {
                            let inner = self.mark();
                            let tmp = self.alloc();
                            self.to_reg(*rhs, tmp);
                            let var = self.home(*var).reg;
                            self.variable_set(var, tmp);
                            self.release(inner);
                        }
                    }
                }
                self.comp(let_.body, dest);
                self.release(mark);
            }
            TermKind::Fix(fix) => {
                let mark = self.mark();
                let mut boxed = Vec::new();
                for var in fix.lhs.iter() {
                    let reg = self.alloc();
                    self.bind(*var, reg);
                    if self.home(*var).boxed {
                        self.load_const(reg, Value::undefined());
                        self.make_box(reg, reg);
                        self.define(reg);
                        boxed.push(true);
                    } else {
                        boxed.push(false);
                    }
                }
                // Every closure exists before any of them is filled in, so they can name each
                // other.
                let mut closures = Vec::new();
                for ((var, proc), is_boxed) in fix.lhs.iter().zip(fix.rhs.iter()).zip(boxed) {
                    let reg = self.home(*var).reg;
                    let closure = if is_boxed { self.alloc() } else { reg };
                    let free = self.closure_instruction(*proc, closure);
                    self.define(closure);
                    if is_boxed {
                        self.variable_set(reg, closure);
                    }
                    closures.push((closure, free));
                }
                for (closure, free) in closures {
                    self.fill_closure(closure, &free);
                }
                self.comp(fix.body, dest);
                self.release(mark);
            }
            TermKind::Receive(formals, rest, producer, consumer) => {
                let mark = self.mark();
                let targets: Vec<u32> = formals.iter().map(|_| self.alloc()).collect();
                let rest_reg = rest.map(|_| self.alloc());
                self.comp(producer, Dest::multi(targets.clone(), rest_reg));
                for (var, reg) in formals.iter().zip(targets.iter()) {
                    self.define(*reg);
                    self.bind(*var, *reg);
                }
                if let (Some(var), Some(reg)) = (rest, rest_reg) {
                    self.define(reg);
                    self.bind(var, reg);
                }
                let vars = formals.iter().copied().chain(rest);
                for var in vars.collect::<Vec<_>>() {
                    let home = self.home(var);
                    if home.boxed {
                        self.make_box(home.reg, home.reg);
                    }
                }
                self.comp(consumer, dest);
                self.release(mark);
            }
            TermKind::WithContinuationMark(k, m, result) => {
                let src = term.source();
                let thunk = self.c.ctx.allocate(Proc {
                    name: Value::new(false),
                    source: src,
                    args: Array::from_slice(self.c.ctx, []),
                    variadic: None,
                    body: result,
                    meta: Value::null(),
                });
                let thunk = self.term(src, TermKind::Proc(thunk));
                let callee = self.primref("call-with-continuation-mark", src);
                self.comp_call(callee, &[k, m, thunk], src, dest);
            }
            TermKind::Values(values) => self.comp_values(term, values.as_slice(), dest),
            TermKind::Call(callee, args) => {
                self.comp_call(callee, args.as_slice(), term.source(), dest)
            }
            _ => self.comp_atom(term, dest),
        }
    }

    fn term(&self, source: Value<'gc>, kind: TermKind<'gc>) -> TermRef<'gc> {
        let term = self.c.ctx.allocate(Term {
            source: Lock::new(source),
            kind,
        });
        self.c.keep_term(term)
    }

    fn primref(&self, name: &str, source: Value<'gc>) -> TermRef<'gc> {
        let name = Symbol::from_str(self.c.ctx, name).into();
        self.term(source, TermKind::PrimRef(name))
    }

    fn comp_if(
        &mut self,
        test: TermRef<'gc>,
        then: TermRef<'gc>,
        otherwise: TermRef<'gc>,
        dest: Dest,
    ) {
        if let TermKind::Const(value) = test.kind {
            let branch = if value != Value::new(false) {
                then
            } else {
                otherwise
            };
            return self.comp(branch, dest);
        }
        let mark = self.mark();
        let cond = self.operand(test);
        let else_label = Label::new();
        self.ins(OpJumpIfFalse::linkable(Register(cond), else_label));
        self.release(mark);
        let tail = matches!(dest, Dest::Tail);
        self.comp(then, dest.clone());
        let end_label = Label::new();
        if !tail {
            self.ins(OpJump::linkable(end_label));
        }
        self.label(else_label);
        self.comp(otherwise, dest);
        if !tail {
            self.label(end_label);
        }
    }

    // ----- values and calls

    fn comp_values(&mut self, term: TermRef<'gc>, values: &[TermRef<'gc>], dest: Dest) {
        match dest {
            Dest::Effect => {
                for value in values {
                    self.comp(*value, Dest::Effect);
                }
            }
            _ if values.len() == 1 => self.comp(values[0], dest),
            Dest::Tail => {
                let mark = self.mark();
                let first = self.top;
                for value in values {
                    let reg = self.alloc();
                    self.to_reg(*value, reg);
                }
                for index in 0..values.len() as u32 {
                    self.mov(2 + index, first + index);
                }
                self.touch(2 + values.len() as u32);
                self.ins(
                    OpReturnValues {
                        count: Count(values.len() as u32),
                    }
                    .into(),
                );
                self.release(mark);
            }
            Dest::Reg(_) => {
                // Not one value for a single-value context: `values` raises there.
                let callee = self.primref("values", term.source());
                self.comp_call(callee, values, term.source(), dest);
            }
            Dest::Multi(targets, rest) => {
                let fixed = targets.len();
                let fits = if rest.is_some() {
                    values.len() >= fixed
                } else {
                    values.len() == fixed
                };
                if !fits {
                    for value in values {
                        self.comp(*value, Dest::Effect);
                    }
                    return self.raise(RaiseKind::AssertionViolation, &[]);
                }
                for (value, target) in values.iter().zip(targets.iter()) {
                    self.to_reg(*value, *target);
                }
                if let Some(rest) = rest {
                    let extra = &values[fixed..];
                    if extra.is_empty() {
                        self.load_const(rest, Value::null());
                    } else {
                        let mark = self.mark();
                        let first = self.top;
                        for value in extra {
                            let reg = self.alloc();
                            self.to_reg(*value, reg);
                        }
                        self.call_prim_n(rest, Primitive::List, first, extra.len() as u32);
                        self.release(mark);
                    }
                }
            }
        }
    }

    fn call_prim_n(&mut self, dst: u32, prim: Primitive, first: u32, argc: u32) {
        self.touch(dst);
        self.ins(
            OpCallPrimitiveN {
                dst: Register(dst),
                primitive: prim_id(prim),
                first: Register(first),
                argc: Count(argc),
            }
            .into(),
        );
    }

    fn comp_call(
        &mut self,
        callee: TermRef<'gc>,
        args: &[TermRef<'gc>],
        src: Value<'gc>,
        dest: Dest,
    ) {
        // Raw expander output calls primitives as `(call (primref name) args)`, or through a
        // global of the `(#%capy)` instance.
        let primitive = match callee.kind {
            TermKind::PrimRef(name) => Some(name),
            TermKind::GlobalRef(key, name, GlobalSlot::Box(_))
                if key.is_pair()
                    && key.cdr().is_null()
                    && key.car() == Symbol::from_str(self.c.ctx, "#%capy").into() =>
            {
                Some(name)
            }
            _ => None,
        };
        if let Some(name) = primitive
            && name.is::<Symbol>()
            && Primitive::from_name(&name.downcast::<Symbol>().to_string())
                .is_some_and(|prim| has_direct_form(prim, args.len()))
        {
            let call = prim_call_term(self.c.ctx, name, args, src);
            return self.comp(call, dest);
        }
        let argc = args.len() as u32;
        let mark = self.mark();
        if matches!(dest, Dest::Tail) {
            let callee_reg = self.alloc();
            self.to_reg(callee, callee_reg);
            for arg in args {
                let reg = self.alloc();
                self.to_reg(*arg, reg);
            }
            self.note_source(src);
            self.mov(1, callee_reg);
            for index in 0..argc {
                self.mov(2 + index, callee_reg + 1 + index);
            }
            self.touch(2 + argc);
            let ins = OpTailCall::new(Count(argc)).with_metadata(Default::default());
            self.ins(ins);
            self.release(mark);
            return;
        }

        // The callee's frame starts at `base`. `base - 1` is reserved and holds no value.
        let reserved = self.alloc();
        let base = self.alloc();
        let callee_reg = self.alloc();
        self.to_reg(callee, callee_reg);
        for arg in args {
            let reg = self.alloc();
            self.to_reg(*arg, reg);
        }
        let mask = self.mask_ref(reserved);
        self.touch(base + 2 + argc);
        self.note_source(src);
        let ins = OpCall::new(Count(argc), Register(base)).with_metadata(Default::default());
        self.ins(ins);
        match dest {
            Dest::Reg(dst) => {
                let ins = OpReturnPoint::new(Count(base), mask, Count(MV_ERROR), Register(dst))
                    .with_metadata(Default::default());
                self.ins(ins);
            }
            Dest::Effect => {
                let ins =
                    OpReturnPoint::new(Count(base), mask, Count(MV_IGNORE), Register(base + 2))
                        .with_metadata(Default::default());
                self.ins(ins);
            }
            Dest::Multi(targets, rest) => {
                let fixed = targets.len() as u32;
                self.ins(
                    OpReturnPointForward {
                        frame: Count(base),
                        mask,
                    }
                    .into(),
                );
                if rest.is_some() {
                    self.ins(
                        OpReturnPointValuesRest {
                            nvals: Count(fixed),
                            frame: Count(base),
                        }
                        .into(),
                    );
                } else {
                    self.ins(
                        OpReturnPointValues {
                            nvals: Count(fixed),
                            frame: Count(base),
                        }
                        .into(),
                    );
                }
                self.touch(base + 2 + fixed);
                if let Some(rest) = rest {
                    self.ins(
                        OpValuesRest {
                            base: Register(base),
                            from: Count(fixed),
                        }
                        .into(),
                    );
                    self.mov(rest, base);
                }
                for (index, target) in targets.iter().enumerate() {
                    self.mov(*target, base + 2 + index as u32);
                }
            }
            Dest::Tail => unreachable!("tail calls return above"),
        }
        self.release(mark);
    }

    // ----- everything that has exactly one value and no control flow of its own

    fn comp_atom(&mut self, term: TermRef<'gc>, dest: Dest) {
        match dest {
            Dest::Reg(dst) => self.atom(term, dst),
            Dest::Effect => {
                if matches!(
                    term.kind,
                    TermKind::LRef(_)
                        | TermKind::Const(_)
                        | TermKind::Proc(_)
                        | TermKind::PrimRef(_)
                ) {
                    return;
                }
                let mark = self.mark();
                let reg = self.alloc();
                self.atom(term, reg);
                self.release(mark);
            }
            Dest::Tail => {
                if let TermKind::LRef(var) = term.kind {
                    let home = self.home(var);
                    if !home.boxed {
                        self.ins(
                            OpReturn {
                                src: Register(home.reg),
                            }
                            .into(),
                        );
                        return;
                    }
                }
                let mark = self.mark();
                let reg = self.alloc();
                self.atom(term, reg);
                self.ins(OpReturn { src: Register(reg) }.into());
                self.release(mark);
            }
            Dest::Multi(targets, rest) => {
                let mark = self.mark();
                let reg = self.alloc();
                self.atom(term, reg);
                self.define(reg);
                match (targets.len(), rest) {
                    (1, None) => self.mov(targets[0], reg),
                    (0, Some(rest)) => {
                        let nil = self.alloc();
                        self.load_const(nil, Value::null());
                        self.touch(rest);
                        self.ins(
                            OpCallPrimitive2 {
                                dst: Register(rest),
                                primitive: prim_id(Primitive::Cons),
                                a: Register(reg),
                                b: Register(nil),
                            }
                            .into(),
                        );
                    }
                    _ => self.raise(RaiseKind::AssertionViolation, &[]),
                }
                self.release(mark);
            }
        }
    }

    /// The value of `term` into `dst`, which is not yet defined.
    fn atom(&mut self, term: TermRef<'gc>, dst: u32) {
        self.touch(dst);
        match term.kind {
            TermKind::Const(value) => self.load_const(dst, value),
            TermKind::LRef(var) => {
                let home = self.home(var);
                if home.boxed {
                    self.variable_ref(dst, home.reg);
                } else {
                    self.mov(dst, home.reg);
                }
            }
            TermKind::LSet(var, value) => {
                let mark = self.mark();
                let reg = self.operand(value);
                let home = self.home(var);
                assert!(home.boxed, "assigned variable {} has no box", var.name);
                self.variable_set(home.reg, reg);
                self.release(mark);
                self.load_const(dst, Value::undefined());
            }
            TermKind::GlobalRef(_, name, slot) => {
                self.note_source(term.source());
                let mark = self.mark();
                let (var, checked) = self.global_slot(slot);
                if checked {
                    let bound = self.alloc();
                    self.prim1(bound, Primitive::VariableBound, var);
                    let ok = Label::new();
                    self.ins(OpJumpIfTrue::linkable(Register(bound), ok));
                    let message: Value<'gc> =
                        Str::new(self.c.ctx, format!("undefined variable: {name}"), true).into();
                    self.c.keep_value(message);
                    self.raise(RaiseKind::Undefined, &[Value::new(false), message]);
                    self.label(ok);
                }
                self.variable_ref(dst, var);
                self.release(mark);
            }
            TermKind::GlobalSet(_, _, slot, value) => {
                self.note_source(term.source());
                let mark = self.mark();
                let reg = self.operand(value);
                let (var, _) = self.global_slot(slot);
                self.variable_set(var, reg);
                self.release(mark);
                self.load_const(dst, Value::undefined());
            }
            TermKind::Linklet(linklet) => {
                let src = term.source();
                self.mark_checked_slots(&linklet);
                let proc = self.c.an.linklet_proc(linklet, src);
                let proc = self.term(src, TermKind::Proc(proc));
                let ctx = self.c.ctx;
                let make = self.primref("make-linklet", src);
                let call = call_term(
                    ctx,
                    make,
                    [
                        constant(ctx, linklet.name),
                        constant(ctx, linklet.imports),
                        constant(ctx, linklet.exports),
                        proc,
                        constant(ctx, linklet.meta),
                    ],
                    src,
                );
                self.comp(call, Dest::Reg(dst));
            }
            TermKind::Proc(proc) => {
                let free = self.closure_instruction(proc, dst);
                self.fill_closure(dst, &free);
            }
            TermKind::CaseLambda(clauses) => {
                let desugared = self.desugar_case_lambda(term, clauses.as_slice());
                let desugared = self.c.keep_term(desugared);
                self.comp(desugared, Dest::Reg(dst));
            }
            TermKind::PrimRef(name) => self.prim_ref(name, dst),
            TermKind::PrimCall(name, args) => self.prim_call(term, name, args.as_slice(), dst),
            TermKind::Seq(..)
            | TermKind::If(..)
            | TermKind::Let(_)
            | TermKind::Fix(_)
            | TermKind::Receive(..)
            | TermKind::WithContinuationMark(..)
            | TermKind::Values(_)
            | TermKind::Call(..) => self.comp(term, Dest::Reg(dst)),
        }
    }

    /// The slot with the `Variable` of a global slot, and whether reading it needs a bound check.
    fn global_slot(&mut self, slot: GlobalSlot<'gc>) -> (u32, bool) {
        match slot {
            GlobalSlot::Box(var) => {
                let checked = self.c.checked.contains(&key(var));
                (self.home(var).reg, checked)
            }
            GlobalSlot::Variable(variable) => {
                let reg = self.alloc();
                self.load_const(reg, variable);
                self.define(reg);
                (reg, true)
            }
        }
    }

    /// The slots of a namespace linklet or of a program may be read before they are defined.
    fn mark_checked_slots(&mut self, linklet: &crate::expander::core::LinkletTerm<'gc>) {
        let ctx = self.c.ctx;
        let meta = linklet.meta;
        if meta.is_pair() && meta.car() == Symbol::from_str(ctx, "#%namespace").into() {
            let imports = linklet.imports;
            if imports.is_pair() {
                let count = imports.car().list_length().saturating_sub(1);
                for index in 0..count {
                    self.c.checked.insert(key(linklet.slots[index]));
                }
            }
        }
        if is_program_meta(ctx, meta) {
            let mut count = 0;
            let mut groups = linklet.imports;
            while groups.is_pair() {
                count += groups.car().list_length().saturating_sub(1);
                groups = groups.cdr();
            }
            for index in count..linklet.slots.len() {
                self.c.checked.insert(key(linklet.slots[index]));
            }
        }
    }

    // ----- closures

    /// Compile `proc` and make its closure in `dst`, without its free variables. Returns them.
    fn closure_instruction(&mut self, proc: ProcRef<'gc>, dst: u32) -> Vec<LVarRef<'gc>> {
        let free = self.c.an.free_vars(proc);
        let code = FnCx::compile(&mut *self.c, proc);
        let constant = self.code_constant(code);
        self.touch(dst);
        self.ins(
            OpClosure {
                dst: Register(dst),
                constant: Constant(constant),
                nfree: Count(free.len() as u32),
            }
            .into(),
        );
        free
    }

    fn fill_closure(&mut self, closure: u32, free: &[LVarRef<'gc>]) {
        for (index, var) in free.iter().enumerate() {
            let src = self.home(*var).reg;
            self.ins(
                OpClosureSet {
                    closure: Register(closure),
                    index: Count(index as u32),
                    src: Register(src),
                }
                .into(),
            );
        }
    }

    /// `(case-lambda (formals body) ...)` as a procedure that takes any arguments and applies the
    /// first clause that accepts their count.
    fn desugar_case_lambda(
        &mut self,
        term: TermRef<'gc>,
        clauses: &[ProcRef<'gc>],
    ) -> TermRef<'gc> {
        let ctx = self.c.ctx;
        let src = term.source();
        let args = fresh_lvar(
            ctx,
            Symbol::from_str_uninterned(ctx, "case-lambda-args", None).into(),
        );
        let count = fresh_lvar(
            ctx,
            Symbol::from_str_uninterned(ctx, "case-lambda-n", None).into(),
        );
        let vars: Vec<LVarRef<'gc>> = clauses
            .iter()
            .map(|_| {
                fresh_lvar(
                    ctx,
                    Symbol::from_str_uninterned(ctx, "case-lambda-clause", None).into(),
                )
            })
            .collect();
        let apply = |var: LVarRef<'gc>| {
            let callee = self.primref("apply", src);
            call_term(ctx, callee, [lref(ctx, var), lref(ctx, args)], src)
        };
        let no_match = {
            let callee = self.primref("assertion-violation", src);
            let who: Value<'gc> = Symbol::from_str(ctx, "case-lambda").into();
            let message: Value<'gc> = Str::new(ctx, "no clause matches the arguments", true).into();
            call_term(
                ctx,
                callee,
                [constant(ctx, who), constant(ctx, message), lref(ctx, args)],
                src,
            )
        };
        let mut body = no_match;
        for (clause, var) in clauses.iter().zip(vars.iter()).rev() {
            let (name, bound) = if clause.variadic.is_some() {
                (">=", clause.args.len())
            } else {
                ("=", clause.args.len())
            };
            let test = prim_call_term(
                ctx,
                Symbol::from_str(ctx, name).into(),
                [lref(ctx, count), constant(ctx, Value::new(bound as i32))],
                src,
            );
            body = if_term(ctx, test, apply(*var), body);
        }
        let length = prim_call_term(
            ctx,
            Symbol::from_str(ctx, "length").into(),
            [lref(ctx, args)],
            src,
        );
        let body = let_term(
            ctx,
            LetStyle::Let,
            Array::from_slice(ctx, [count]),
            Array::from_slice(ctx, [length]),
            body,
            src,
        );
        let first = clauses[0];
        let dispatcher = proc_term(
            ctx,
            first.name,
            Array::from_slice(ctx, []),
            Some(args),
            body,
            first.source,
            first.meta,
        );
        let rhs: Vec<TermRef<'gc>> = clauses
            .iter()
            .map(|clause| self.term(clause.source, TermKind::Proc(*clause)))
            .collect();
        let_term(
            ctx,
            LetStyle::Let,
            Array::from_slice(ctx, vars),
            Array::from_slice(ctx, rhs),
            dispatcher,
            src,
        )
    }

    // ----- primitives

    /// The variable of the primitive `name`, from the cache cell of the program: the first use
    /// of a name makes the cell, the others read it.
    fn prim_ref(&mut self, name: Value<'gc>, dst: u32) {
        // The cell is identified by the name, a symbol the term keeps alive. A key allocated here
        // would not survive a collection before the program is written.
        let cache_key = name;
        let cell = self.cell_constant(cache_key);
        let mark = self.mark();
        let var = self.alloc();
        let ins = OpCellRef::new(Register(var), Constant(cell)).with_metadata(Default::default());
        self.ins(ins);
        let ready = Label::new();
        let is_heap = self.alloc();
        self.prim1(is_heap, Primitive::IsHeapObject, var);
        self.ins(OpJumpIfTrue::linkable(Register(is_heap), ready));
        let name_reg = self.alloc();
        self.load_const(name_reg, name);
        self.prim1(var, Primitive::CapyVariable, name_reg);
        self.ins(
            OpCellSet {
                constant: Constant(cell),
                src: Register(var),
            }
            .into(),
        );
        self.label(ready);
        self.variable_ref(dst, var);
        self.release(mark);
    }

    fn prim_call(&mut self, term: TermRef<'gc>, name: Value<'gc>, args: &[TermRef<'gc>], dst: u32) {
        let src = term.source();
        let prim = if name.is::<Symbol>() {
            Primitive::from_name(&name.downcast::<Symbol>().to_string())
        } else {
            None
        };
        let direct = prim.filter(|prim| has_direct_form(*prim, args.len()));
        let Some(prim) = direct else {
            match (prim, args) {
                (Some(Primitive::CacheRef), [cache_key]) => {
                    if let TermKind::Const(cache_key) = cache_key.kind {
                        let cell = self.cell_constant(cache_key);
                        let ins = OpCellRef::new(Register(dst), Constant(cell))
                            .with_metadata(Default::default());
                        return self.ins(ins);
                    }
                }
                (Some(Primitive::CacheSet), [cache_key, value]) => {
                    if let TermKind::Const(cache_key) = cache_key.kind {
                        let cell = self.cell_constant(cache_key);
                        let mark = self.mark();
                        let reg = self.operand(*value);
                        self.ins(
                            OpCellSet {
                                constant: Constant(cell),
                                src: Register(reg),
                            }
                            .into(),
                        );
                        self.release(mark);
                        return self.load_const(dst, Value::undefined());
                    }
                }
                _ => {}
            }
            // No direct form: call the procedure of that name.
            let callee = self.term(src, TermKind::PrimRef(name));
            return self.comp(call_term(self.c.ctx, callee, args, src), Dest::Reg(dst));
        };

        self.note_source(src);
        let mark = self.mark();
        let primitive = prim_id(prim);
        let dst_reg = Register(dst);
        if args.len() <= 3 {
            let regs: Vec<u32> = args.iter().map(|arg| self.operand(*arg)).collect();
            let ins = match regs.as_slice() {
                [] => OpCallPrimitive0 {
                    dst: dst_reg,
                    primitive,
                }
                .into(),
                [a] => OpCallPrimitive1 {
                    dst: dst_reg,
                    primitive,
                    a: Register(*a),
                }
                .into(),
                [a, b] => OpCallPrimitive2 {
                    dst: dst_reg,
                    primitive,
                    a: Register(*a),
                    b: Register(*b),
                }
                .into(),
                [a, b, c] => OpCallPrimitive3 {
                    dst: dst_reg,
                    primitive,
                    a: Register(*a),
                    b: Register(*b),
                    c: Register(*c),
                }
                .into(),
                _ => unreachable!("at most three arguments"),
            };
            self.ins(ins);
        } else {
            let first = self.top;
            for arg in args {
                let reg = self.alloc();
                self.to_reg(*arg, reg);
            }
            self.call_prim_n(dst, prim, first, args.len() as u32);
        }
        self.release(mark);
    }

    // ----- linking

    fn link(&mut self, proc: ProcRef<'gc>, arity: i32, size: u32) -> DirectCode<'gc> {
        let atoms = std::mem::take(&mut self.atoms);
        let mut label_at: Vec<Option<Label>> = Vec::with_capacity(atoms.len());
        let linkables = atoms.into_iter().map(|atom| match atom {
            Atom::Ins(ins) => {
                label_at.push(None);
                ins
            }
            Atom::Label(label) => {
                label_at.push(Some(label));
                bc::bind(label)
            }
        });
        let linked = match bc::link(linkables) {
            Ok(linked) => linked,
            Err(err) => {
                self.c
                    .error
                    .get_or_insert_with(|| format!("cannot link {}: {err:?}", proc.name));
                return DirectCode {
                    code: Vec::new(),
                    pool: Vec::new(),
                    metadata_counts: [0; bc::NUM_METADATA_KINDS],
                    constants: Vec::new(),
                    arity,
                    frame_size: size,
                    meta: proc.meta,
                    source: proc.source,
                    name: proc.name,
                    srcmap: Vec::new(),
                    spans: Vec::new(),
                };
            }
        };
        let code = linked.bytes().to_vec();
        let mut atom_offsets = vec![0u32; label_at.len() + 1];
        let mut ordinal = 0;
        for (index, slot) in label_at.iter().enumerate() {
            if slot.is_none() {
                atom_offsets[index] = linked.instructions()[ordinal].offset;
                ordinal += 1;
            }
        }
        atom_offsets[label_at.len()] = code.len() as u32;
        let mut next = code.len() as u32;
        for index in (0..label_at.len()).rev() {
            if label_at[index].is_some() {
                atom_offsets[index] = next;
            } else {
                next = atom_offsets[index];
            }
        }

        let code_len = code.len() as u32;
        let atom_count = label_at.len();
        let mut spans: Vec<Value<'gc>> = Vec::new();
        let mut span_ids: HashMap<u64, u32> = HashMap::new();
        let mut srcmap: Vec<SrcRun> = Vec::with_capacity(self.sources.len());
        let mut notes = std::mem::take(&mut self.sources).into_iter().peekable();
        while let Some((atom, source)) = notes.next() {
            let end_atom = notes.peek().map_or(atom_count, |(next, _)| *next);
            if end_atom <= atom {
                continue;
            }
            let start = atom_offsets[atom];
            let end = if end_atom >= atom_count {
                code_len
            } else {
                atom_offsets[end_atom]
            };
            if end <= start {
                continue;
            }
            let (flags, span) = if source == Value::new(false) || source.is_null() {
                (SRC_FLAG_SYNTHETIC, 0)
            } else {
                let id = *span_ids.entry(source.bits()).or_insert_with(|| {
                    spans.push(source);
                    spans.len() as u32
                });
                (0, id)
            };
            match srcmap.last_mut() {
                Some(last)
                    if last.span == span
                        && last.flags == flags
                        && last.start + last.len == start =>
                {
                    last.len = end - last.start;
                }
                _ => srcmap.push(SrcRun {
                    start,
                    len: end - start,
                    flags,
                    span,
                }),
            }
        }

        DirectCode {
            code,
            pool: std::mem::take(&mut self.pool),
            metadata_counts: metadata_kind_counts(linked.metadata_counts()),
            constants: std::mem::take(&mut self.constants),
            arity,
            frame_size: size,
            meta: proc.meta,
            source: proc.source,
            name: proc.name,
            srcmap,
            spans,
        }
    }
}

fn metadata_kind_counts(link_counts: &[u32]) -> [u32; bc::NUM_METADATA_KINDS] {
    macro_rules! collect {
        ($(($Op:ident, $Meta:ident, $kind:expr, { $($fname:ident : $fty:ty),* }),)*) => {{
            let mut counts = [0u32; bc::NUM_METADATA_KINDS];
            $(counts[$kind] = link_counts[bc::OpcodeId::$Op.table_index()];)*
            counts
        }};
    }
    bc::for_each_metadata!(collect)
}

fn is_program_meta<'gc>(ctx: MutationContext<'gc>, meta: Value<'gc>) -> bool {
    use crate::runtime::objectmodel::Vector;
    if !meta.is::<Vector>() {
        return false;
    }
    let decl = meta.downcast::<Vector>();
    if decl.len() < 2 || decl[0].get() != Symbol::from_str(ctx, "#%library").into() {
        return false;
    }
    let key = decl[1].get();
    key.is_pair() && key.car() == Symbol::from_str(ctx, "%program").into()
}
