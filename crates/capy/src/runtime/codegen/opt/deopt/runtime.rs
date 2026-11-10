//! The runtime half of a deopt exit. The emitter's `deopt_common` stub saves
//! every register and calls [`capy_opt_deopt`], which rebuilds the baseline
//! frames and returns the address to resume at.
//!
//! An exit is described by a [`DeoptPoint`] in the unit's Tier 2 metadata, indexed
//! by the exit id the stub passes: where each value lives ([`DeoptLoc`]), where
//! it goes ([`DeoptDest`]), the frames to build, and the descriptors of the
//! objects that optimized code never allocated. Frame index 0 is the outermost
//! frame, the one the optimized code was entered as; the last index is the
//! innermost, where baseline resumes.
//!
//! The steps, in order:
//!
//! 1. Read every value location into a buffer. Spill slots overlap the
//!    bytecode frame that is written, so nothing is written before this.
//!    Registers come from the save area ([`SaveArea`]); a constant comes from
//!    the code buffer or the owner image's reference table.
//! 2. Box the values. Doubles and int32s are immediates in this value
//!    representation, so boxing allocates nothing. A raw int64 that fits i32 is
//!    the same, but a wider one allocates a bignum. Sunk objects
//!    ([`DeoptLoc::Materialize`]) are rebuilt here too: pairs, closures, env
//!    records, vectors, tuples, class instances and local variable cells, each
//!    from its field values, fields first, so a nested object exists before its
//!    parent. Every allocation obeys the Tier 2 non-collecting slow-path
//!    contract, so no collection runs while the rebuild holds its values in
//!    plain Rust locals, and nothing needs rooting.
//! 2b. A return into a multiple-value entry: the callee's values sit above the
//!    optimized frame's spill slots and baseline reads them at `r[base + 2..]`.
//!    They are read before any frame is written, since an inlined frame may
//!    cover them.
//! 3. Write the frames in place. Frame 0 is the optimized frame itself: the
//!    closure goes to slot 1, the live registers to their bytecode slots, and
//!    every other register to `undefined`, so no raw spill bits survive. Slot 0,
//!    the return address, is untouched. Each inlined frame `n` starts at
//!    `fps[n - 1] + 8 * base`, over its caller's registers from the call's base
//!    register up, as a call from the interpreter would leave it: the
//!    interpreter-return marker in slot 0, the caller's resume point in the
//!    word below, the closure in slot 1, then its registers. The prologue
//!    reserved room for these (`max_deopt_extent`). The extras of a variadic
//!    frame (rest vector, or scalar-mode extras and tail) are written where no
//!    register value already names the slot.
//! 4. Stage the thread state the way `slow::resume_addr` callers do: the frame
//!    pointer, the innermost closure, `AC`, and the bytecode ip. For a return
//!    into a multiple-value entry the frame pointer is the callee's frame and
//!    `AC` the value count.
//! 5. Count the exit and invalidate the unit when its budget is spent. An exit
//!    inside an inlined body is charged to the body's own block, so its next
//!    compile stops speculating there, and to the root's unit budget. Frames
//!    still running an invalidated unit exit too and must not count the same
//!    invalidation twice.
//! 6. Return the baseline ResumeTable address for the pc, else the interpreter.

use std::process;
use std::sync::atomic::Ordering::{Acquire, Relaxed};

use cranelift_entity::EntityRef;

use super::metadata::{DeoptDest, DeoptRegClass, SaveArea, raw_at};
use crate::heap::Gc;
use crate::runtime::MutationContext;
use crate::runtime::codegen::jit::compile::invalidate_optimized;
use crate::runtime::codegen::opt::build::MAX_INLINED_EXTRAS;
use crate::runtime::codegen::opt::ir::inst::{Constant, RematKind};
use crate::runtime::codegen::opt::regalloc::{DeoptLoc, Loc, RematDesc};
use crate::runtime::codegen::slow::resume_addr;
use crate::runtime::codegen::unit::{CodeUnit, DeoptPoint};
use crate::runtime::interp::markers;
use crate::runtime::interp::stack::{self, Ip, Slot};
use crate::runtime::objectmodel::class::SchemeInstance;
use crate::runtime::objectmodel::{
    BigInt, Closure, CodeBlock, CodeImage, EnvRecord, Pair, Tuple, Value, Vector,
};
use crate::runtime::variable::Variable;

/// Rebuilds the baseline frames for `exit` from the saved registers at
/// `saved`, and returns the address to resume at.
///
/// # Safety
///
/// Called only from `deopt_common`, with `th` the thread context and `saved`
/// the save area that stub built on the native stack.
pub(crate) unsafe extern "C" fn capy_opt_deopt(
    th: *const (),
    exit: u64,
    saved: *const u64,
    owner: *const u8,
) -> *const u8 {
    unsafe { deoptimize(th, exit as usize, saved, owner) }
}

/// # Safety
///
/// Same as [`capy_opt_deopt`].
unsafe fn deoptimize<'gc>(
    th: *const (),
    exit: usize,
    saved: *const u64,
    owner: *const u8,
) -> *const u8 {
    if crate::utils::flags::jit_stats() {
        crate::runtime::codegen::jit::config::OPT_DEOPTS.fetch_add(1, Relaxed);
    }
    // SAFETY: `th` is the thread context that `deopt_common` passed.
    let mutator: MutationContext<'gc> = unsafe { MutationContext::from_ptr(th) };
    let tc = mutator.tc();
    // SAFETY: the stub built the save area for this exit.
    let save = unsafe { SaveArea::new(saved) };
    let fp = save.sfp() as Slot<'gc>;

    // The unit is named by the image's owner word, which the emitter passes:
    // after a recompile `optimized` names the new unit while this frame still
    // runs the old one. The owner slot holds the image.
    // SAFETY: the owner word of the image that `exit` belongs to.
    let owner_value = unsafe { *(owner as Slot<'gc>) };
    if !owner_value.is::<CodeImage>() {
        fatal(exit, "its owner word holds no image");
    }
    let owner = owner_value.downcast::<CodeImage>();
    // The outermost frame's closure is in slot 1, which the optimized frame
    // never overwrites.
    // SAFETY: slot 1 of a live optimized frame holds its closure.
    let outer_value = unsafe { *fp.add(1) };
    if !outer_value.is::<Closure>() {
        fatal(exit, "its frame has no closure in slot 1");
    }
    let outer = Closure::code_block(Closure::from_value(outer_value));
    let unit = CodeUnit::of_image(owner).unwrap_or_else(|| fatal(exit, "its owner is not a unit"));
    let opt = unit
        .opt
        .as_ref()
        .unwrap_or_else(|| fatal(exit, "its unit has no Tier 2 metadata"));
    let point: &DeoptPoint = opt
        .deopt
        .get(exit)
        .unwrap_or_else(|| fatal(exit, "the unit has no such exit"));
    if point.locs.len() != point.targets.len() {
        fatal(
            exit,
            "its deopt point names locations and destinations of different counts",
        );
    }
    let frame_size = outer.frame_size as usize;

    // 1. Read every value before any frame word is written.

    let mut reader = Reader {
        save,
        fp,
        frame_size,
        root: outer,
        owner,
        exit,
        ctx: mutator,
        descs: &point.remat,
        cache: vec![None; point.remat.len()],
        state: vec![0; point.remat.len()],
    };
    let values: Vec<(DeoptDest, Value<'gc>)> = point
        .locs
        .iter()
        .zip(&point.targets)
        .map(|(loc, dest)| {
            // SAFETY: `fp` is the optimized frame of this exit.
            let value = unsafe { reader.read(loc) };

            (*dest, value)
        })
        .collect::<Vec<(_, _)>>();

    // Every frame names its closure, and the code block of frame 0 must be
    // slot 1's.
    let nframes = point.frames.len().max(1);
    let closure_of = |frame: usize| -> Value<'gc> {
        values
            .iter()
            .find_map(|&(dest, value)| {
                (dest
                    == DeoptDest::Closure {
                        frame: frame as u32,
                    })
                .then_some(value)
            })
            .unwrap_or_else(|| fatal(exit, "no value rebuilds the closure"))
    };
    let mut closures: Vec<Value<'gc>> = Vec::with_capacity(nframes);
    for frame in 0..nframes {
        let closure = closure_of(frame);
        if !closure.is::<Closure>() {
            fatal(exit, "the rebuilt closure is not a closure");
        }
        closures.push(closure);
    }
    let closure = closures[0];
    let inner = Closure::code_block(Closure::from_value(closure));
    if inner.as_ptr() != outer.as_ptr() {
        fatal(
            exit,
            "the rebuilt closure runs another code block than slot 1",
        );
    }
    let innermost = closures[nframes - 1];
    let innermost_block = Closure::code_block(Closure::from_value(innermost));
    // Where each frame starts: an inlined frame sits at its call's base
    // register in its caller's frame.
    let mut fps: Vec<Slot<'gc>> = Vec::with_capacity(nframes);
    fps.push(fp);
    for frame in 1..nframes {
        let base = point.frames[frame].base as usize;
        // SAFETY: the prologue reserved the room for every inlined frame
        // (`max_deopt_extent`).
        fps.push(unsafe { fps[frame - 1].add(base) });
    }
    let innermost_fp = fps[nframes - 1];

    // 2. A return into a multiple-value entry: the callee's values are in
    //    its frame, at the words above the optimized frame's spill slots,
    //    and baseline reads them at `r[base + 2..]`. The count is `AC`,
    //    which the exit stub left in the thread context. They are read
    //    before any frame is written, since an inlined frame may cover them.
    let mut resume_fp = innermost_fp;
    let mut resume_ac = 0;
    let mut returned_values: Vec<Value<'gc>> = Vec::new();
    let mut returned_to = 0;
    if let Some(returned) = point.returned {
        let count = match returned.count {
            Some(count) => count as usize,
            None => tc.ac.get() as usize,
        };
        if returned.count.is_none() {
            let from = returned.callee_words as usize + 2;
            returned_to = returned.base as usize + 2;
            for i in 0..count {
                // SAFETY: the callee wrote `count` values at `fp[callee + 2..]`.
                returned_values.push(unsafe { *fp.add(from + i) });
            }
        }
        // SAFETY: the callee frame starts inside the stack the frame owns.
        resume_fp = unsafe { innermost_fp.add(returned.base as usize) };
        resume_ac = count as u64;
    }

    // 3. Write the outermost frame in place.
    let mut named = vec![false; frame_size];
    let mut rest_area: Option<(u32, Value<'gc>)> = None;

    let mut rest_scalars: Vec<(u32, u32, u32, Value<'gc>)> = Vec::new();
    let mut rest_tail: Option<(u32, u32, u32, Value<'gc>)> = None;
    for &(dest, value) in &values {
        match dest {
            DeoptDest::RestArea { frame: 0, base } => rest_area = Some((base, value)),
            DeoptDest::RestScalar {
                frame: 0,
                base,
                index,
                cnt,
            } => {
                rest_scalars.push((base, cnt, index, value));
            }
            DeoptDest::RestTail {
                frame: 0,
                base,
                from,
                cnt,
            } => {
                rest_tail = Some((base, cnt, from, value));
            }
            DeoptDest::Closure { frame: 0 } => unsafe { *fp.add(1) = value },
            // r1 is the closure register in the frame state. Its slot is
            // the closure destination's, so this one only has to match.
            DeoptDest::Register { frame: 0, reg: 1 } => {
                if value.bits() != closure.bits() {
                    fatal(exit, "the closure register disagrees with slot 1");
                }
            }
            DeoptDest::Register { frame: 0, reg } => {
                let reg = reg as usize;
                if reg < 2 || reg >= frame_size {
                    fatal(exit, "a value names a bytecode register outside the frame");
                }
                // SAFETY: `reg` is inside the frame, and the reads are done.
                unsafe { *fp.add(reg) = value };
                named[reg] = true;
            }
            // Inlined frames are written below.
            DeoptDest::Closure { .. } | DeoptDest::Register { .. } => {}
            DeoptDest::RestScalar { .. } | DeoptDest::RestTail { .. } => {}
            DeoptDest::RestArea { .. } => {}
        }
    }
    // Registers that no value names are dead at this pc. Clear them so
    // the frame holds no raw bits from spill slots.
    for (reg, &is_named) in named.iter().enumerate().skip(2) {
        if !is_named {
            // SAFETY: `reg` is inside the frame.
            unsafe { *fp.add(reg) = Value::undefined() };
        }
    }

    // The rest area of a variadic procedure: the elements of the rest vector,
    // where no value names the register
    if let Some((base, vector)) = rest_area {
        // The empty-rest fast path binds the canonical empty list instead of
        // an empty vector: there is no rest area to restore, and the count
        // register already says zero.
        if !vector.is_null() {
            if !vector.is::<Vector>() {
                fatal(exit, "the rest vector is not a vector");
            }
            let vector = vector.downcast::<Vector>();
            for (i, &element) in vector.as_slice().iter().enumerate() {
                let reg = base as usize + i;
                if reg < frame_size && named[reg] {
                    continue;
                }
                // SAFETY: the entry checked room for the frame plus the extras,
                // and the rest area starts inside the frame.
                unsafe { *fp.add(reg) = element };
            }
        }
    }
    // The extras of a scalar-mode variadic procedure: the scalars below the
    // count, then the tail vector. Where no value names the register, as for
    // the vector above. The count register says how many extras exist; a dead
    // one (cleared above) means no instruction can read the area.
    let write_extra = |index: usize, base: u32, element: Value<'gc>| {
        let reg = base as usize + index;
        if reg < frame_size && named[reg] {
            return;
        }
        // SAFETY: the entry checked room for the frame plus the extras, and
        // the rest area starts inside the frame.
        unsafe { *fp.add(reg) = element };
    };
    for &(base, cnt, index, value) in &rest_scalars {
        // SAFETY: `cnt` is a register of the frame.
        let count = unsafe { *fp.add(cnt as usize) };
        if count.is_int32() && (index as i32) < count.as_int32() {
            write_extra(index as usize, base, value);
        }
    }
    if let Some((base, cnt, from, vector)) = rest_tail {
        // SAFETY: `cnt` is a register of the frame.
        let count = unsafe { *fp.add(cnt as usize) };
        // The empty-list marker is no vector: there are no further extras.
        if count.is_int32() && !vector.is_null() {
            if !vector.is::<Vector>() {
                fatal(exit, "the rest tail is not a vector");
            }
            let vector = vector.downcast::<Vector>();
            for (i, &element) in vector.as_slice().iter().enumerate() {
                write_extra(from as usize + i, base, element);
            }
        }
    }
    // 3. The inlined frames, each over its caller's registers from `base` up
    //    as a call from the interpreter would leave it: the return marker in
    //    slot 0, the caller's return point in the slot below, the closure
    //    in slot 1 and the live registers where the callee reads them.
    for frame in 1..nframes {
        let fp_f = fps[frame];
        let closure_f = closures[frame];
        let block = Closure::code_block(Closure::from_value(closure_f));
        let size = block.frame_size as usize;
        // The caller resumes at its return point.
        let caller_block = Closure::code_block(Closure::from_value(closures[frame - 1]));
        let resume =
            (caller_block.entrypoint.as_usize() + point.frames[frame - 1].pc as usize) as Ip;
        // SAFETY: `fp_f` starts a frame inside the reserved room, which
        // holds `size` words, and its slots below are the caller's.
        unsafe {
            stack::set_return_address(fp_f, markers::interpreter_return());
            stack::set_resume_ip(fp_f, resume);
            *fp_f.add(1) = closure_f;
        }
        let mut named_f = vec![false; size];
        let mut scalars_f: Vec<(u32, u32, u32, Value<'gc>)> = Vec::new();
        let mut tail_f: Option<(u32, u32, u32, Value<'gc>)> = None;
        let mut area_f: Option<(u32, Value<'gc>)> = None;
        for &(dest, value) in &values {
            match dest {
                DeoptDest::RestArea { frame: f, base } if f as usize == frame => {
                    area_f = Some((base, value));
                }
                DeoptDest::RestScalar {
                    frame: f,
                    base,
                    index,
                    cnt,
                } if f as usize == frame => {
                    scalars_f.push((base, cnt, index, value));
                }
                DeoptDest::RestTail {
                    frame: f,
                    base,
                    from,
                    cnt,
                } if f as usize == frame => {
                    tail_f = Some((base, cnt, from, value));
                }
                DeoptDest::Register { frame: f, reg } if f as usize == frame => {
                    let reg = reg as usize;
                    if reg == 1 {
                        continue;
                    }
                    if reg < 2 || reg >= size {
                        fatal(
                            exit,
                            "a value names a bytecode register outside the inlined frame",
                        );
                    }
                    // SAFETY: `reg` is inside the frame.
                    unsafe { *fp_f.add(reg) = value };
                    named_f[reg] = true;
                }
                _ => {}
            }
        }
        for (reg, &is_named) in named_f.iter().enumerate().skip(2) {
            if !is_named {
                // SAFETY: `reg` is inside the frame.
                unsafe { *fp_f.add(reg) = Value::undefined() };
            }
        }
        // The extras of an inlined scalar-mode variadic frame, as for frame 0.
        // SAFETY (all writes): `inlined_extent` reserved `base +
        // MAX_INLINED_EXTRAS` words for the frame, and the inliner passes no
        // more extras than that.
        let write_extra = |index: usize, base: u32, element: Value<'gc>| {
            let reg = base as usize + index;
            if reg < size && named_f[reg] {
                return;
            }
            unsafe { *fp_f.add(reg) = element };
        };
        for &(base, cnt, index, value) in &scalars_f {
            let count = unsafe { *fp_f.add(cnt as usize) };
            if count.is_int32() && (index as i32) < count.as_int32() {
                write_extra(index as usize, base, value);
            }
        }
        if let Some((base, vector)) = area_f {
            if !vector.is_null() {
                if !vector.is::<Vector>() {
                    fatal(exit, "the rest vector is not a vector");
                }
                let vector = vector.downcast::<Vector>();
                if vector.as_slice().len() > MAX_INLINED_EXTRAS as usize {
                    fatal(exit, "an inlined frame has too many extras");
                }
                for (i, &element) in vector.as_slice().iter().enumerate() {
                    write_extra(i, base, element);
                }
            }
        }
        if let Some((base, cnt, from, vector)) = tail_f {
            let count = unsafe { *fp_f.add(cnt as usize) };
            if count.is_int32() && !vector.is_null() {
                if !vector.is::<Vector>() {
                    fatal(exit, "the rest tail is not a vector");
                }
                let vector = vector.downcast::<Vector>();
                if vector.as_slice().len() > MAX_INLINED_EXTRAS as usize {
                    fatal(exit, "an inlined frame has too many extras");
                }
                for (i, &element) in vector.as_slice().iter().enumerate() {
                    write_extra(from as usize + i, base, element);
                }
            }
        }
    }

    for (i, &value) in returned_values.iter().enumerate() {
        // SAFETY: baseline's frame for the call has room for the values,
        // which it reads before it pops the callee frame.
        unsafe { *innermost_fp.add(returned_to + i) = value };
    }

    // 4. Stage the thread state, as `slow::stage_control_with` does for a
    //    resume.
    let ip = (innermost_block.entrypoint.as_usize() + point.pc as usize) as Ip;
    // The resumed code runs at `resume_fp`: the frame itself, or the callee
    // frame that a multiple-value entry pops.
    tc.stack.fp.set(resume_fp);
    tc.cp.set(innermost);
    // AC carries an argument or value count, which only an entry or a return
    // point reads. Only a return into a multiple-value entry has one.
    tc.ac.set(resume_ac);
    tc.ip.set(ip);

    // 5. Count the exit and invalidate the unit once its budget is spent.
    // Frames that still run an invalidated unit exit too, and must not
    // count the same invalidation twice.
    let already_invalid = opt.invalid.load(Acquire) != 0;
    let spent = {
        // An exit inside an inlined body counts against the body's own
        // block, so its next compile stops speculating there. The unit's
        // budget is the root's.
        if nframes > 1 {
            let mut body = innermost_block
                .exits
                .lock()
                .unwrap_or_else(|err| err.into_inner());
            body.record(point.pc);
        }
        let mut profile = outer.exits.lock().unwrap_or_else(|err| err.into_inner());
        let spent = if nframes > 1 {
            profile.count_unit_exit()
        } else {
            profile.record(point.pc)
        };
        if spent && !already_invalid {
            profile.reset_unit();
        }
        spent
    };
    if spent && !already_invalid {
        // Only the unit that is still the block's current one charges its
        // recompile budget. A deopt from a superseded unit patches its own
        // entry and returns `None`.
        if let Some(generation) = invalidate_optimized(outer, owner) {
            outer.note_recompile(generation);
        }
    }

    if crate::utils::flags::jit_stats() {
        let (call_pc, code) = match point.inlined {
            Some(site) => (site.call_pc, site.code),
            None => (point.pc, 0),
        };
        super::exits::record_exit_site(owner.as_ptr() as usize, call_pc, code, point.guard, || {
            crate::runtime::objectmodel::proc::procedure_label(innermost_block)
        });
    }

    // 6. The baseline entry for the pc, else the interpreter.
    resume_addr(innermost, ip)
}

/// Reads a deopt location and rebuilds rematerialized objects.
struct Reader<'a, 'gc> {
    save: SaveArea,
    fp: Slot<'gc>,
    frame_size: usize,
    root: Gc<'gc, CodeBlock<'gc>>,
    owner: Gc<'gc, CodeImage<'gc>>,
    exit: usize,
    ctx: MutationContext<'gc>,
    /// The descriptor table of this exit; `Materialize(i)` names entry `i`.
    descs: &'a [RematDesc],
    /// The rebuilt object of each descriptor, by descriptor id.
    cache: Vec<Option<Value<'gc>>>,
    /// 0 = not built, 1 = being built (cycle), 2 = built.
    state: Vec<u8>,
}

impl<'a, 'gc> Reader<'a, 'gc> {
    /// The value of `loc` out of the optimized frame or the save area.
    ///
    /// # Safety
    ///
    /// `self.fp` is the optimized frame of `self.save`'s exit, with
    /// `self.frame_size` words of bytecode frame.
    unsafe fn read(&mut self, loc: &DeoptLoc) -> Value<'gc> {
        let raw = |at: Loc, class: DeoptRegClass| {
            unsafe { raw_at(at, class, self.save, self.fp as *const u64, self.frame_size) }
                .unwrap_or_else(|| fatal(self.exit, "a value lives in a register outside the pool"))
        };
        match loc {
            DeoptLoc::Reg(reg) => {
                let word = self.save.gpr(*reg).unwrap_or_else(|| {
                    fatal(self.exit, "a value lives in a register outside the pool")
                });
                Value::from_raw_i64(word as i64)
            }
            DeoptLoc::Slot(slot) => {
                Value::from_raw_i64(raw(Loc::Slot(*slot), DeoptRegClass::Gpr) as i64)
            }
            DeoptLoc::Const(constant) => {
                constant_value(constant, self.root, self.owner, self.exit, self.ctx)
            }
            DeoptLoc::UnboxedDouble(at) => {
                Value::from_f64(f64::from_bits(raw(*at, DeoptRegClass::Fpr)))
            }
            DeoptLoc::UnboxedI32(at) => Value::from_i32(raw(*at, DeoptRegClass::Gpr) as i32),
            DeoptLoc::UnboxedI64(at) => rebox_i64(self.ctx, raw(*at, DeoptRegClass::Gpr) as i64),
            DeoptLoc::Materialize(id) => self.materialize(*id),
        }
    }

    /// Rebuilds the object named by descriptor `id`, fields first, so nested
    /// sunk objects exist before their parents. The table is a DAG; a cycle
    /// is a build bug and aborts.
    fn materialize(&mut self, id: u32) -> Value<'gc> {
        let index = id as usize;
        if index >= self.descs.len() {
            fatal(self.exit, "a remat location names a missing descriptor");
        }
        match self.state[index] {
            2 => return self.reload(index),
            1 => fatal(self.exit, "remat descriptors form a cycle"),
            _ => {}
        }
        self.state[index] = 1;
        if crate::utils::flags::jit_stats() {
            crate::runtime::codegen::jit::config::OPT_REMAT_OBJECTS.fetch_add(1, Relaxed);
        }
        let desc = self.descs[index].clone();
        let value = self.rebuild(&desc);

        self.cache[index] = Some(value);
        self.state[index] = 2;
        self.reload(index)
    }

    fn reload(&self, id: usize) -> Value<'gc> {
        self.cache[id].unwrap_or_else(|| fatal(self.exit, "a rebuilt object is missing"))
    }

    /// Allocates the object itself from the field values, with the class
    /// header and layout the runtime constructors use. These allocations are
    /// LAB bumps on the mutator's non-collecting slow path.
    fn rebuild(&mut self, desc: &RematDesc) -> Value<'gc> {
        match desc.kind {
            RematKind::Pair => {
                let car = unsafe { self.read(&desc.fields[0]) };
                let cdr = unsafe { self.read(&desc.fields[1]) };
                Value::from(Pair::new(self.ctx, car, cdr))
            }
            RematKind::Closure { code, .. } => {
                let value = self
                    .owner
                    .ref_value(self.code_slot(code))
                    .unwrap_or_else(|| {
                        fatal(self.exit, "a closure descriptor names a missing code slot")
                    });

                if !value.is::<CodeBlock>() {
                    fatal(self.exit, "a remat closure code slot holds no code block");
                }
                let block = value.downcast::<CodeBlock>();
                let closure = Closure::new(self.ctx, block, desc.fields.len(), false);
                let wclosure = Gc::write(self.ctx, closure);
                for (i, field) in desc.fields.iter().enumerate() {
                    let free = unsafe { self.read(field) };
                    wclosure[i].unlock().set(free);
                }

                Value::new(closure)
            }
            RematKind::Env { .. } => {
                let env_record = EnvRecord::new_undefined(self.ctx, desc.fields.len());

                let wrecord = Gc::write(self.ctx, env_record);

                for (i, field) in desc.fields.iter().enumerate() {
                    wrecord[i].unlock().set(unsafe { self.read(field) });
                }

                Value::new(env_record)
            }
            RematKind::Vector { len } => {
                let vector = Vector::new::<false>(self.ctx, len as _, Value::undefined());
                let wvector = Gc::write(self.ctx, vector);

                for (i, field) in desc.fields.iter().enumerate() {
                    wvector[i].unlock().set(unsafe { self.read(field) });
                }

                Value::new(vector)
            }
            RematKind::Tuple { len } => {
                let tuple = Tuple::new(self.ctx, len as _, Value::undefined());
                let wtuple = Gc::write(self.ctx, tuple);

                for (i, field) in desc.fields.iter().enumerate() {
                    wtuple[i].unlock().set(unsafe { self.read(field) });
                }

                Value::new(tuple)
            }
            RematKind::Instance { class, .. } => {
                let id = crate::heap::object::ClassId::new(class)
                    .unwrap_or_else(|| fatal(self.exit, "an instance descriptor names no class"));
                let descriptor = crate::runtime::objectmodel::class::class_table(self.ctx)
                    .lookup(id)
                    .unwrap_or_else(|| fatal(self.exit, "an instance class is not in the table"));
                let instance = SchemeInstance::allocate(self.ctx, descriptor);
                if instance.slot_count() != desc.fields.len() {
                    fatal(self.exit, "an instance class has another slot count");
                }
                for (i, field) in desc.fields.iter().enumerate() {
                    let value = unsafe { self.read(field) };
                    SchemeInstance::set_slot(self.ctx, instance, i, value);
                }

                Value::new(instance)
            }
            RematKind::Variable => {
                let value = unsafe { self.read(&desc.fields[0]) };
                Value::from(Variable::new(self.ctx, value))
            }
        }
    }

    /// The reference slot of `code` in the owner's constant table.
    fn code_slot(&self, code: crate::runtime::codegen::opt::ir::types::CodeRef) -> usize {
        let opt = CodeUnit::of_image(self.owner)
            .and_then(|unit| unit.opt.as_ref())
            .unwrap_or_else(|| fatal(self.exit, "the remat owner has no Tier 2 metadata"));
        opt.code_slots
            .get(code.index())
            .copied()
            .unwrap_or_else(|| fatal(self.exit, "a remat code ref is outside the table"))
            as usize
    }
}

/// The Scheme value of a raw int64 at a deopt: a fixnum when it fits i32,
/// otherwise a bignum.
fn rebox_i64<'gc>(ctx: MutationContext<'gc>, raw: i64) -> Value<'gc> {
    match i32::try_from(raw) {
        Ok(value) => Value::from_i32(value),
        Err(_) => {
            let big = BigInt::from_i64(ctx, raw);
            Value::from_raw(Value::from(big).bits())
        }
    }
}

/// The value of a constant that a deopt location names.
fn constant_value<'gc>(
    constant: &Constant,
    root: Gc<'gc, CodeBlock<'gc>>,
    owner: Gc<'gc, CodeImage<'gc>>,
    exit: usize,
    ctx: MutationContext<'gc>,
) -> Value<'gc> {
    match *constant {
        Constant::Int32(value) => Value::from_i32(value),
        Constant::Int64(value) => rebox_i64(ctx, value),
        Constant::Double(bits) => Value::from_f64(f64::from_bits(bits)),
        Constant::Bits(bits) => Value::from_raw_i64(bits as i64),
        Constant::Table(index) => {
            owner
                .ref_value(index as usize)
                .unwrap_or_else(|| fatal(exit, "a constant names a missing table slot"))
        }
        Constant::Block { .. } => fatal(
            exit,
            "a raw block address is not a Scheme value and has no deopt rematerialization",
        ),
        Constant::Pool { code, index } => {
            if code.index() != 0 {
                fatal(
                    exit,
                    "a pool constant names an inlined code block, which the emitter rejects",
                );
            }
            root.buffer.constant(index as usize)
        }
    }
}

fn fatal(exit: usize, what: &str) -> ! {
    crate::heap::logging::ensure_max_level(log::LevelFilter::Error);
    log::error!(
        target: crate::heap::logging::JIT_LOG_TARGET,
        "optimized code deoptimized at exit {exit}, but {what}"
    );
    process::abort();
}
