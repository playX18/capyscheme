use super::*;

/// Slots of a cached stack
const CACHED_STACK_SIZE: usize = 0;
const CACHED_STACK_LINK: usize = 1;
const CACHED_STACK_SEGMENT: usize = 2;

/// Put the stack `[stack, stack + size)` of `segment`, which nothing uses,
/// on the stack cache.
unsafe fn cache_stack<'gc>(tc: &ThreadContext<'gc>, segment: Value<'gc>, stack: Slot<'gc>, size: usize) {
    unsafe {
        if size <= CACHED_STACK_SEGMENT {
            return;
        }
        *stack.add(CACHED_STACK_SIZE) = Value::from_raw(size as u64);
        *stack.add(CACHED_STACK_LINK) = Value::from_raw(tc.stack.stack_cache.get() as u64);
        *stack.add(CACHED_STACK_SEGMENT) =
            std::mem::transmute::<Value<'gc>, Value<'static>>(segment);
        tc.stack.stack_cache.set(stack);
    }
}

/// Most stacks a [`StackPool`] keeps.
const STACK_POOL_CAP: usize = 64;

/// Stacks of finished threads, kept for the next thread the same worker
/// starts. Starting a thread otherwise allocates a non-moving bytevector.
pub struct StackPool {
    head: Cell<Slot<'static>>,
    len: Cell<usize>,
}

unsafe impl Sync for StackPool {}
unsafe impl Send for StackPool {}

impl StackPool {
    pub const fn new() -> Self {
        Self {
            head: Cell::new(std::ptr::null_mut()),
            len: Cell::new(0),
        }
    }

    /// Forget every stack. The collector calls this with the world stopped.
    pub fn clear(&self) {
        self.head.set(std::ptr::null_mut());
        self.len.set(0);
    }

    /// Keep the stack `[stack, stack + size)` of `segment` for reuse.
    ///
    /// # Safety
    ///
    /// Nothing uses the stack, and no collection runs before it is taken.
    unsafe fn put<'gc>(&self, segment: Value<'gc>, stack: Slot<'gc>, size: usize) {
        unsafe {
            if size <= CACHED_STACK_SEGMENT || self.len.get() >= STACK_POOL_CAP {
                return;
            }
            *stack.add(CACHED_STACK_SIZE) = Value::from_raw(size as u64);
            *stack.add(CACHED_STACK_LINK) = Value::from_raw(self.head.get() as u64);
            *stack.add(CACHED_STACK_SEGMENT) =
                std::mem::transmute::<Value<'gc>, Value<'static>>(segment);
            self.head.set(stack.cast());
            self.len.set(self.len.get() + 1);
        }
    }

    /// The most recently kept stack when it has at least `min` slots.
    unsafe fn take<'gc>(&self, min: usize) -> Option<(Value<'gc>, Slot<'gc>, usize)> {
        unsafe {
            let stack = self.head.get().cast::<Value<'gc>>();
            if stack.is_null() || (*stack.add(CACHED_STACK_SIZE)).bits() as usize <= min - 1 {
                return None;
            }
            let size = (*stack.add(CACHED_STACK_SIZE)).bits() as usize;
            self.head
                .set((*stack.add(CACHED_STACK_LINK)).bits() as usize as Slot<'static>);
            self.len.set(self.len.get() - 1);
            Some((*stack.add(CACHED_STACK_SEGMENT), stack, size))
        }
    }
}

/// Give the stacks of a finished thread to `pool`: the live one and the
/// cached ones. The thread has no stack afterwards.
///
/// # Safety
///
/// The thread's last activation has ended and it is mutating, so no
/// collection has dropped its stacks.
pub unsafe fn retire_stacks<'gc>(tc: &ThreadContext<'gc>, pool: &StackPool) {
    unsafe {
        let stack = &tc.stack;
        let mut cached = stack.stack_cache.get();
        stack.stack_cache.set(std::ptr::null_mut());
        while !cached.is_null() {
            let size = (*cached.add(CACHED_STACK_SIZE)).bits() as usize;
            let next = (*cached.add(CACHED_STACK_LINK)).bits() as usize as Slot;
            pool.put(*cached.add(CACHED_STACK_SEGMENT), cached, size);
            cached = next;
        }
        let live = stack.scheme_stack.get();
        if !live.is_null() {
            pool.put(stack.segment.get(), live, stack.scheme_stack_size.get());
        }
        stack.scheme_stack.set(std::ptr::null_mut());
        stack.scheme_stack_size.set(0);
        stack.segment.set(Value::new(false));
        stack.fp.set(std::ptr::null_mut());
        stack.sp.set(std::ptr::null_mut());
        stack.stack_link.set(Value::new(false));
        stack.cached_frame.set(Value::new(false));
    }
}

/// Make the live stack one of at least
/// `n` slots, from the stack cache or freshly allocated. The old live stack
/// is dropped.
pub(super) unsafe fn reset_scheme_stack<'gc>(ctx: MutationContext<'gc>, n: usize, floor: usize) {
    unsafe {
        let tc = ctx.tc();
        let stack = &tc.stack;

        let mut n = n + (ONE_SHOT_HEADROOM >> 1);

        let mut x = stack.stack_cache.get();
        loop {
            if x.is_null() {
                if n < floor {
                    n = floor;
                }
                if let Some(pool) = crate::runtime::sched::scheduler::stack_pool() {
                    if let Some((segment, base, size)) = pool.take(n) {
                        stack.segment.set(segment);
                        stack.scheme_stack.set(base);
                        stack.scheme_stack_size.set(size);
                        set_esp(tc);
                        return;
                    }
                }
                // stacks are non-moving bytevectors
                let bytes = ByteVector::new::<false>(ctx, n * size_of::<Value>(), false);
                stack.segment.set(bytes.into());
                stack
                    .scheme_stack
                    .set(bytes.contents().to_mut_ptr::<Value<'gc>>());
                break;
            }
            let m = (*x.add(CACHED_STACK_SIZE)).bits() as usize;
            let next = (*x.add(CACHED_STACK_LINK)).bits() as usize as Slot;
            if m >= n {
                n = m;
                stack.segment.set(*x.add(CACHED_STACK_SEGMENT));
                stack.scheme_stack.set(x);
                // Smaller stacks before this one are dropped too.
                stack.stack_cache.set(next);
                break;
            }
            x = next;
        }
        stack.scheme_stack_size.set(n);
        set_esp(tc);
    }
}

/// Give the thread its first stack.
pub fn ensure_stack<'gc>(ctx: MutationContext<'gc>) {
    let stack = &ctx.tc().stack;
    if !stack.scheme_stack.get().is_null() {
        return;
    }
    // SAFETY: the thread has no stack yet, so nothing refers to one.
    unsafe {
        reset_scheme_stack(
            ctx,
            0,
            if crate::runtime::sched::scheduler::is_spawned_virtual_thread() {
                THREAD_STACK_SIZE
            } else {
                DEFAULT_STACK_SIZE
            },
        );
    }
    stack.sp.set(stack.scheme_stack.get());
    stack.stack_link.set(Value::new(false));
}

/// Continue on a new stack with
/// `frame_request` more slots than the part of the live stack above the
/// split point. The live stack below the frame at `fp`, from the first frame
/// above `fp - UNDERFLOW_LIMIT`, becomes a continuation, everything above
/// that is copied to the new stack. Returns the new `fp`, `sp` moves with it.
///
/// # Safety
///
/// The stack state's `fp` is a live frame of the live stack with its return
/// address written, and `sp` is at or above it.
pub unsafe fn overflow<'gc>(ctx: MutationContext<'gc>, frame_request: usize) -> Slot<'gc> {
    unsafe {
        let tc = ctx.tc();
        let stack = &tc.stack;
        let sfp = stack.fp.get();
        let scheme_stack = stack.scheme_stack.get();
        let scheme_stack_size = stack.scheme_stack_size.get();

        let mut guard = sfp.wrapping_sub(UNDERFLOW_LIMIT);
        // leave at least stack_slop headroom in the old stack to reduce the
        // need for return-point overflow checks
        let other_guard = scheme_stack
            .wrapping_add(scheme_stack_size)
            .wrapping_sub(STACK_SLOP);
        if other_guard < guard {
            guard = other_guard;
        }

        let mut split_point = scheme_stack;
        // split only if old stack contains more than underflow_limit slots
        if guard > scheme_stack {
            // set split point to base of the frame below the current one
            let frame_size = caller_frame_size(sfp, return_address(sfp))
                .expect("a frame above the stack base has a caller");
            split_point = sfp.sub(frame_size);

            // split only if we have more than one frame
            if split_point != scheme_stack {
                // walk the stack to set split_point at first frame above
                // guard, note that first frame may have put us below the
                // guard already
                loop {
                    let frame_size = caller_frame_size(split_point, return_address(split_point))
                        .expect("a frame above the stack base has a caller");
                    let p = split_point.sub(frame_size);
                    if p < guard {
                        break;
                    }
                    split_point = p;
                }

                let split_stack_clength = split_point.offset_from(scheme_stack);

                // promote to multi-shot if current stack is shrimpy
                let split_stack_length = if scheme_stack_size < DEFAULT_STACK_SIZE / 4 {
                    promote_to_multishot(stack.stack_link.get());
                    split_stack_clength
                } else {
                    scheme_stack_size as isize
                };

                // create a continuation
                let k = Continuation::make(
                    ctx,
                    stack.segment.get(),
                    scheme_stack,
                    split_stack_length,
                    split_stack_clength,
                    stack.stack_link.get(),
                    return_address(split_point),
                    Value::new(false),
                );
                stack.stack_link.set(k.into());

                // overwrite old return address with dounderflow
                set_return_address(split_point, markers::underflow());
            }
        }

        let above_split_size = scheme_stack_size - split_point.offset_from(scheme_stack) as usize;

        // allocate a new stack, retaining same relative sfp
        let sfp_offset = sfp.offset_from(split_point) as usize;
        let sp_offset = stack.sp.get().offset_from(split_point) as usize;
        reset_scheme_stack(ctx, above_split_size + frame_request, DEFAULT_STACK_SIZE);
        let new_stack = stack.scheme_stack.get();
        let fp = new_stack.add(sfp_offset);
        stack.fp.set(fp);
        stack.sp.set(new_stack.add(sp_offset));

        // copy up everything above the split point. we don't know where the
        // current frame ends, so we copy through the end of the old stack
        std::ptr::copy_nonoverlapping(split_point, new_stack, above_split_size);
        fp
    }
}

/// Make sure `need` slots are free above `sp` in the live stack, moving the
/// top frame to a new stack if not. For use while a native procedure owns
/// the top frame.
///
/// # Safety
///
/// The stack state's `fp` and `sp` describe the top frame of the live stack.
pub unsafe fn reserve<'gc>(ctx: MutationContext<'gc>, need: usize) {
    unsafe {
        let stack = &ctx.tc().stack;
        if stack.sp.get().add(need) <= stack.end() {
            return;
        }
        overflow(ctx, need);
    }
}

/// Make the live stack below the frame
/// at `fp` a continuation object, move the live stack base up to `fp`, and
/// return the continuation. Nothing is copied. The frame at `fp` then
/// returns through `dounderflow`.
///
/// `one_shot` makes an opportunistic one-shot object (used for continuation
/// marks), reusing the cached frame, otherwise the chain below
/// is promoted to multi-shot first. When the frame already
/// returns through `dounderflow` the continuation below is returned as is.
///
/// # Safety
///
/// `fp` is a live frame of the live stack, at or above its base, with its
/// return address written.
pub unsafe fn reify_cc<'gc>(
    ctx: MutationContext<'gc>,
    fp: Slot<'gc>,
    one_shot: bool,
) -> Value<'gc> {
    unsafe {
        let stack = &ctx.tc().stack;
        let td = stack.stack_link.get();
        if !one_shot {
            // Promote existing 1-shot to multishot before reifying
            promote_to_multishot(td);
        }

        let ref_ret = return_address(fp);
        if ref_ret == markers::underflow()
            && td.downcast::<Continuation>().attachments.get() != Value::new(false)
        {
            return td;
        }

        let xp = match stack.cached_frame.get() {
            cached if one_shot && cached != Value::new(false) => {
                stack.cached_frame.set(Value::new(false));
                cached.downcast::<Continuation>()
            }
            _ => Continuation::alloc(ctx),
        };
        // Before the stores: a cached frame is an existing object (see
        // `split`).
        let wxp = Gc::write(ctx, xp);
        barrier::field!(wxp, Continuation, return_address)
            .unlock()
            .set(ref_ret);
        barrier::field!(wxp, Continuation, attachments)
            .unlock()
            .set(ctx.state().current_marks());
        set_return_address(fp, markers::underflow());
        barrier::field!(wxp, Continuation, link).unlock().set(td);
        stack.stack_link.set(xp.into());
        let base = stack.scheme_stack.get();
        stack.scheme_stack.set(fp);
        barrier::field!(wxp, Continuation, stack).unlock().set(base);
        barrier::field!(wxp, Continuation, segment)
            .unlock()
            .set(stack.segment.get());
        let n = fp.offset_from(base);
        xp.length.set(if one_shot {
            OPPORTUNISTIC_1_SHOT_FLAG
        } else {
            n
        });
        // clength will be 0 if we're reifying to hide a continuation
        // link
        xp.clength.set(n);
        stack
            .scheme_stack_size
            .set(stack.scheme_stack_size.get() - n as usize);
        xp.into()
    }
}

/// Reify the live stack below the frame at
/// `fp` (the native frame of `call/1cc`, live up to `sp`) as a one-shot
/// continuation and continue on a new live stack. The new stack is carved
/// out of the old one, `ONE_SHOT_HEADROOM` slots up, when it is large
/// enough, the continuation then owns the slots in between. Otherwise it
/// owns the whole old stack, and the new one comes from the stack cache or
/// is allocated. The native frame moves to the base of the new stack, whose
/// `fp` and `sp` the stack state then holds.
///
/// # Safety
///
/// `fp` is the stack state's `fp`, a live frame of the live stack with its
/// return address written, live up to `sp`.
pub unsafe fn call1cc<'gc>(ctx: MutationContext<'gc>, fp: Slot<'gc>, sp: Slot<'gc>) -> Value<'gc> {
    unsafe {
        let tc = ctx.tc();
        let stack = &tc.stack;
        let td = stack.stack_link.get();
        let ref_ret = return_address(fp);
        if ref_ret == markers::underflow()
            && td.downcast::<Continuation>().attachments.get() != Value::new(false)
        {
            // The existing object is handed out as it is. An opportunistic
            // one is promoted first: once merged it may be reused as the
            // cached frame, which must never happen to an object Scheme
            // holds.
            if td.downcast::<Continuation>().length.get() == OPPORTUNISTIC_1_SHOT_FLAG {
                promote_to_multishot(td);
            }
            return td;
        }
        let xp = Continuation::alloc(ctx);
        // SAFETY: new allocations do not need write barriers
        let wxp = Write::assume(&*xp);

        barrier::field!(wxp, Continuation, return_address)
            .unlock()
            .set(ref_ret);
        barrier::field!(wxp, Continuation, attachments)
            .unlock()
            .set(ctx.state().current_marks());
        barrier::field!(wxp, Continuation, link).unlock().set(td);
        stack.stack_link.set(xp.into());
        let base = stack.scheme_stack.get();
        barrier::field!(wxp, Continuation, stack).unlock().set(base);
        barrier::field!(wxp, Continuation, segment)
            .unlock()
            .set(stack.segment.get());
        xp.clength.set(fp.offset_from(base));

        // we need to get ourselves a new stack. we carve it out of the old
        // one if the old one is large enough. if not, we look for one in the
        // cache. if the cache is empty, we allocate a new stack.
        let live = sp.offset_from(fp) as usize;
        let new_fp = if fp.wrapping_add(ONE_SHOT_HEADROOM * 2) <= tc.real_esp.get() {
            let new_fp = fp.add(ONE_SHOT_HEADROOM);
            let n = new_fp.offset_from(base);
            xp.length.set(n);
            stack.scheme_stack.set(new_fp);
            stack
                .scheme_stack_size
                .set(stack.scheme_stack_size.get() - n as usize);
            new_fp
        } else {
            // set continuation length to entire stack size
            xp.length.set(stack.scheme_stack_size.get() as isize);
            // The first cached stack is taken whatever its size, because the
            // native frame is copied and needs room for it.
            reset_scheme_stack(ctx, live, DEFAULT_STACK_SIZE);
            stack.scheme_stack.get()
        };
        // The carved branch copies from `fp` to `fp + ONE_SHOT_HEADROOM`, so
        // the ranges overlap unless the live frame fits in the headroom.
        debug_assert!(live <= ONE_SHOT_HEADROOM);
        std::ptr::copy_nonoverlapping(fp, new_fp, live);
        set_return_address(new_fp, markers::underflow());
        stack.fp.set(new_fp);
        stack.sp.set(new_fp.add(live));
        xp.into()
    }
}

/// Continue `k` with the `count` values at
/// `values`, abandoning the live stack above its base. Returns the frame
/// pointer of the frame that "returns", with the values in its slots 2 and
/// up, and the return address to return to, `None` if `k` is a shot one-shot
/// continuation.
///
unsafe fn nuate_help<'gc>(
    ctx: MutationContext<'gc>,
    k: Gc<'gc, Continuation<'gc>>,
    values: Slot<'gc>,
    count: usize,
) -> Option<(Slot<'gc>, Ra)> {
    unsafe {
        let tc = ctx.tc();
        let stack = &tc.stack;
        let sfp = stack.scheme_stack.get();
        let td = k.clength.get();
        let multishot = if k.length.get() == td {
            // length and clength match, so it is either multishot or
            // shot1shot
            if td == SHOT_1_SHOT_FLAG {
                return None;
            }
            true
        } else if k.length.get() == OPPORTUNISTIC_1_SHOT_FLAG {
            // opportunistic 1-shot, so merge with the current stack if
            // possible, otherwise just treat as multishot
            if sfp == k.stack.get().offset(td) {
                // Before the frames leave `k` for the live stack (see
                // `split`).
                let _ = Gc::write(ctx, k);
                stack
                    .scheme_stack_size
                    .set(stack.scheme_stack_size.get() + td as usize);
                stack.scheme_stack.set(k.stack.get());
                stack.stack_link.set(k.link.get());
                k.clength.set(0); // in case GC sees it
                stack.cached_frame.set(k.into()); // save for fast immediate realloc
                restore_attachments(ctx, k);
                std::ptr::copy(values, CallFrame::create(sfp).arguments_ptr(), count);
                return Some((sfp, k.return_address.get()));
            }
            true
        } else {
            // general 1-shot: treat as multishot if clength + size(values)  > length
            k.length.get() < td + 2 + count as isize
        };

        if !multishot {
            // Before the frames leave `k` for the live stack (see `split`).
            let _ = Gc::write(ctx, k);
            stack.stack_link.set(k.link.get());
            // grab saved stack, keeping the old one to cache it once the
            // values are off it
            let old_stack = sfp;
            let old_size = stack.scheme_stack_size.get();
            let old_segment = stack.segment.get();
            let new_stack = k.stack.get();
            stack.scheme_stack.set(new_stack);
            stack.segment.set(k.segment.get());
            stack.scheme_stack_size.set(k.length.get() as usize);
            set_esp(tc);
            // move stack return values to top of saved stack segment
            let fp = new_stack.offset(td);
            std::ptr::copy(values, CallFrame::create(fp).arguments_ptr(), count);
            // place old stack in stack cache
            cache_stack(tc, old_segment, old_stack, old_size);
            // The attachments are left alone here: marks are popped by every
            // return through a continuation object.
            restore_attachments(ctx, k);
            // mark continuation shot
            k.length.set(SHOT_1_SHOT_FLAG);
            k.clength.set(SHOT_1_SHOT_FLAG);
            return Some((fp, k.return_address.get()));
        }

        // multishot: split if clength > underflow-limit, resize unless
        // stack-base + clength + size(values) <= esp
        if td > UNDERFLOW_LIMIT as isize || sfp.offset(td).add(2 + count) > tc.real_esp.get() {
            split_and_resize(ctx, k, count);
        }
        let td = k.clength.get();
        let sfp = stack.scheme_stack.get();
        stack.stack_link.set(k.link.get());
        // potentially pop an attachment
        restore_attachments(ctx, k);
        let fp = sfp.offset(td);
        std::ptr::copy(values, CallFrame::create(fp).arguments_ptr(), count);
        std::ptr::copy_nonoverlapping(k.stack.get(), sfp, td as usize);
        Some((fp, k.return_address.get()))
    }
}

fn restore_attachments<'gc>(ctx: MutationContext<'gc>, k: Gc<'gc, Continuation<'gc>>) {
    let attachments = k.attachments.get();
    if attachments != Value::new(false) {
        // SAFETY: these are the marks of the frames being returned into.
        unsafe { ctx.state().set_current_marks(attachments) }
    }
}

/// `dounderflow`: the frame at `fp`, the base of the
/// live stack, returned `count` values in `fp[2..]`. Continue the
/// continuation below, see [`nuate_help`].
///
/// # Safety
///
/// `fp` is the base of the live stack with `count` values in `fp[2..]`, and
/// the stack link is a [`Continuation`].
pub unsafe fn dounderflow<'gc>(
    ctx: MutationContext<'gc>,
    fp: Slot<'gc>,
    count: usize,
) -> Option<(Slot<'gc>, Ra)> {
    unsafe {
        let stack = &ctx.tc().stack;
        debug_assert!(
            fp == stack.scheme_stack.get(),
            "dounderflow away from the stack base"
        );
        let link = stack.stack_link.get();
        assert!(
            link.is::<Continuation>(),
            "return through the bottom of a stack with no continuation below"
        );
        nuate_help(
            ctx,
            link.downcast::<Continuation>(),
            CallFrame::create(fp).arguments_ptr(),
            count,
        )
    }
}

/// `nuate`: invoke the continuation object `k` with the
/// `count` values at `args`, see [`nuate_help`].
///
/// # Safety
///
/// `args` points at `count` readable values.
pub unsafe fn nuate<'gc>(
    ctx: MutationContext<'gc>,
    k: Gc<'gc, Continuation<'gc>>,
    args: Slot<'gc>,
    count: usize,
) -> Option<(Slot<'gc>, Ra)> {
    unsafe { nuate_help(ctx, k, args, count) }
}

/// Save the stack registers into `record` and start a new activation whose
/// first frame is at the returned address. The frame's return address is the
/// `scheme_exit` marker.
///
/// A callback from C runs on the same stack, so reifying in it would take in
/// the frames of the Scheme code that called out to C. Activations are
/// delimited instead: one entered with Scheme frames live
/// below it runs on a stack of its own, which [`pop_activation`] hands back
/// to the stack cache, so that a continuation reified in it never holds
/// slots the frames below will write. One entered with nothing live runs on
/// the live stack from its base.
///
/// # Safety
///
/// `record` is valid for writes and stays at the same address until the
/// matching [`pop_activation`].
pub unsafe fn push_activation<'gc>(
    ctx: MutationContext<'gc>,
    record: *mut Activation<'gc>,
    need: usize,
) -> Slot<'gc> {
    unsafe {
        ensure_stack(ctx);
        let tc = ctx.tc();
        let stack = &tc.stack;
        record.write(Activation {
            prev: stack.activation.get(),
            fp: stack.fp.get(),
            sp: stack.sp.get(),
            scheme_stack: stack.scheme_stack.get(),
            scheme_stack_size: stack.scheme_stack_size.get(),
            segment: stack.segment.get(),
            stack_link: stack.stack_link.get(),
            top_ip: stack.top_ip.get(),
            marks: ctx.state().current_marks(),
        });
        stack.activation.set(record);

        if !stack.fp.get().is_null() || stack.scheme_stack.get().add(need) > tc.real_esp.get() {
            reset_scheme_stack(ctx, need, DEFAULT_STACK_SIZE);
        }
        let fp = stack.scheme_stack.get();
        stack.stack_link.set(Value::new(false));
        stack.top_ip.set(std::ptr::null());
        stack.fp.set(fp);
        stack.sp.set(fp);
        set_return_address(fp, markers::scheme_exit());
        fp
    }
}

/// Restore the stack registers saved by [`push_activation`]. An activation
/// that ran on a stack of its own puts its live stack on the stack cache, one
/// that ran on the live stack leaves it.
///
/// # Safety
///
/// `record` was filled by [`push_activation`], and the activation it
/// started has ended.
pub unsafe fn pop_activation<'gc>(ctx: MutationContext<'gc>, record: *mut Activation<'gc>) {
    unsafe {
        let tc = ctx.tc();
        let stack = &tc.stack;
        let saved = &*record;
        stack.activation.set(saved.prev);
        if !saved.fp.is_null() {
            cache_stack(
                tc,
                stack.segment.get(),
                stack.scheme_stack.get(),
                stack.scheme_stack_size.get(),
            );
            stack.scheme_stack.set(saved.scheme_stack);
            stack.scheme_stack_size.set(saved.scheme_stack_size);
            stack.segment.set(saved.segment);
            set_esp(tc);
        }
        stack.fp.set(saved.fp);
        stack.sp.set(saved.sp);
        stack.stack_link.set(saved.stack_link);
        stack.top_ip.set(saved.top_ip);
        ctx.state().set_current_marks(saved.marks);
    }
}

/// First free slot above the frame at `fp`, for pushing a frame on top of it
/// outside the normal call protocol.
///
/// # Safety
///
/// `fp` is a live frame whose slot 1 holds the value it was called with.
pub unsafe fn frame_top<'gc>(ctx: MutationContext<'gc>, fp: Slot<'gc>) -> Slot<'gc> {
    unsafe {
        let frame = CallFrame::create(fp);
        let closure = frame.callee();
        if closure.is::<Closure>() {
            let closure = closure.downcast::<Closure>();
            if !closure.code_block.is_native() {
                return fp.add((closure.code_block.frame_size as usize).max(2));
            }
        }
        ctx.tc().stack.sp.get().max(frame.arguments_ptr())
    }
}

/// Procedures on the stack, innermost first, with the bytecode address each
/// will resume at (null for stub frames, and for the running frame when no
/// slow-path exit staged one). For backtraces.
///
/// # Safety
///
/// Called by the thread that owns the stack, with the stack state's `fp`
/// either null or a live frame whose return-address chain is intact.
pub unsafe fn backtrace<'gc>(ctx: MutationContext<'gc>, limit: usize) -> Vec<(Value<'gc>, Ip)> {
    let mut frames = Vec::new();
    unsafe {
        let tc = ctx.tc();
        let stack = &tc.stack;
        let mut fp = stack.fp.get();
        if fp.is_null() {
            return frames;
        }
        frames.push((*fp.add(1), tc.ip.get()));
        let mut ret = return_address(fp);
        while frames.len() < limit {
            let Some(size) = caller_frame_size(fp, ret) else {
                break;
            };
            let resume = if ret == markers::raise_ret() || ret == markers::subr_ret() {
                std::ptr::null()
            } else {
                resume_ip(fp)
            };
            fp = fp.sub(size);
            let closure = *fp.add(1);
            frames.push((closure, resume));
            ret = return_address(fp);
        }
    }
    frames
}
