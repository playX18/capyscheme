use super::*;

/// Continuation object. Its frames are `[stack, stack +
/// clength)` in `segment`, it is resumed by returning to `return_address`
/// with `stack + clength` as the frame pointer of the returning frame.
///
/// `length` says how it may be resumed:
///
///  * `length == clength`: multi-shot. Its frames are never written and are
///    copied onto the live stack when resumed.
///  * `length > clength`: one-shot. It owns `[stack, stack + length)` and is
///    resumed in place, which shoots it.
///  * `OPPORTUNISTIC_1_SHOT_FLAG`: resumed in place only when its frames end
///    at the base of the live stack, and treated as multi-shot otherwise.
///  * `length == clength == SHOT_1_SHOT_FLAG`: shot, invoking it is an
///    error.
///
/// For an interpreted frame (`return_address` is `INTERP_RETURN`) the
/// bytecode resume point is `stack[clength - 1]`, the top slot of the
/// frames, so it belongs to the object that owns them and travels with every
/// copy of them.
#[repr(C)]
pub struct Continuation<'gc> {
    /// The bytevector holding `stack`.
    pub segment: Lock<Value<'gc>>,
    pub link: Lock<Value<'gc>>,
    /// The continuation marks of these frames, or `#f` when returning into
    /// them leaves the marks alone.
    pub attachments: Lock<Value<'gc>>,
    pub stack: Lock<Slot<'gc>>,
    pub length: Cell<isize>,
    pub clength: Cell<isize>,
    pub return_address: Lock<Ra>,
}

extern "C" fn trace_continuation(obj: GcObject, visitor: &mut Visitor) {
    // SAFETY: `obj` is a `Continuation`, its frames lie in `segment`, which it keeps
    // alive, and are walked with the same rules as the live stack.
    unsafe {
        let continuation = obj.to_address().as_mut_ref::<Continuation>();
        visitor.trace(continuation.segment.get_mut());
        visitor.trace(continuation.link.get_mut());
        visitor.trace(continuation.attachments.get_mut());
        if continuation.length.get() != SHOT_1_SHOT_FLAG {
            let base = continuation.stack.get();
            sweep_stack(
                base,
                continuation.clength.get(),
                continuation.return_address.get(),
                &mut |slot| {
                    if crate::heap::collection::CHECK_EDGES {
                        let value = *slot;
                        if !crate::runtime::root::is_traceable_root(value) {
                            crate::heap::logging::ensure_max_level(log::LevelFilter::Error);
                            log::error!(
                                target: "capy::gc",
                                "continuation bad slot at stack+{} ({:#x}) holds {:#x} \
                                 (clength={})",
                                slot as usize - base as usize,
                                slot as usize,
                                value.bits(),
                                continuation.clength.get(),
                            );
                        }
                    }
                    crate::runtime::root::trace_slot_guarded("continuation slot", slot, visitor);
                },
            );
        }
    }
}

extern "C" fn process_weak_continuation(_obj: GcObject, _weak_processor: &mut WeakProcessor) {}

fn continuation_header_word() -> u64 {
    class_header_word(
        ClassId::new(builtin_class_ids::CONTINUATION).expect("builtin class id is nonzero"),
    )
}

impl<'gc> Continuation<'gc> {
    pub const HOOKS: AllocationHooks = AllocationHooks {
        alignment: align_of::<Continuation>(),
        compute_alignment: None,
        instance_size: size_of::<Continuation>(),
        compute_size: None,
        trace: trace_continuation,
        weak_proc: process_weak_continuation,
        type_name: "continuation",
    };

    pub(super) fn alloc(ctx: MutationContext<'gc>) -> Gc<'gc, Self> {
        // SAFETY: the allocation is `size_of::<Continuation>()` bytes and is fully
        // written, as an empty continuation, before it can be traced.
        unsafe {
            let object = ctx.allocate_raw(
                size_of::<Self>(),
                align_of::<Self>(),
                continuation_header_word(),
                Space::Default,
            );
            object
                .to_address()
                .to_mut_ptr::<Self>()
                .write(Continuation {
                    segment: Lock::new(Value::new(false)),
                    link: Lock::new(Value::new(false)),
                    attachments: Lock::new(Value::new(false)),
                    stack: Lock::new(std::ptr::null_mut()),
                    length: Cell::new(0),
                    clength: Cell::new(0),
                    return_address: Lock::new(std::ptr::null()),
                });
            Gc::from_gc_object(object)
        }
    }

    #[allow(clippy::too_many_arguments)]
    pub(super) fn make(
        ctx: MutationContext<'gc>,
        segment: Value<'gc>,
        stack: Slot<'gc>,
        length: isize,
        clength: isize,
        link: Value<'gc>,
        return_address: Ra,
        attachments: Value<'gc>,
    ) -> Gc<'gc, Self> {
        let k = Self::alloc(ctx);
        // SAFETY: new allocations do not need write barriers
        let kw = unsafe { Write::assume(&*k) };
        barrier::field!(kw, Continuation, segment)
            .unlock()
            .set(segment);
        barrier::field!(kw, Continuation, stack).unlock().set(stack);
        k.length.set(length);
        k.clength.set(clength);
        barrier::field!(kw, Continuation, link).unlock().set(link);
        barrier::field!(kw, Continuation, return_address)
            .unlock()
            .set(return_address);
        barrier::field!(kw, Continuation, attachments)
            .unlock()
            .set(attachments);
        k
    }
}

// SAFETY: Continuation stores its class ID in the heap header selected at construction time.
unsafe impl<'gc> ClassTagged for Continuation<'gc> {
    const CLASS_IDS: &'static [u32] = &[builtin_class_ids::CONTINUATION];

    const TYPE_NAME: &'static str = "continuation";
}

/// True if `continuation` (a continuation object) was resumed in place and so cannot
/// be invoked again.
pub fn is_shot<'gc>(continuation: Value<'gc>) -> bool {
    continuation
        .try_as::<Continuation>()
        .is_some_and(|continuation| continuation.length.get() == SHOT_1_SHOT_FLAG)
}

/// Promote every one-shot continuation in the chain `k` to multi-shot.
pub fn promote_to_multishot<'gc>(mut k: Value<'gc>) {
    while let Some(continuation) = k.try_as::<Continuation>() {
        if continuation.length.get() == continuation.clength.get() {
            break;
        }
        continuation.length.set(continuation.clength.get());
        k = continuation.link.get();
    }
}

/// Split `k` at `s`. `k` must be a multi-shot continuation, and `s`
/// (the split point) must be strictly between the base and end of its
/// stack.
unsafe fn split<'gc>(ctx: MutationContext<'gc>, k: Gc<'gc, Continuation<'gc>>, s: Slot<'gc>) {
    unsafe {
        // set m to size of lower piece, n to size of upper piece
        let m = s.offset_from(k.stack.get());
        let n = k.clength.get() - m;

        // insert a new continuation between k and link(k)
        let lower = Continuation::make(
            ctx,
            k.segment.get(),
            k.stack.get(),
            m,
            m,
            k.link.get(),
            return_address(s),
            Value::new(false),
        );
        // Before the stores: `k`'s frames below `s` move to `lower`, and a
        // snapshot-at-the-beginning barrier must see them still in `k`.
        let kw = Gc::write(ctx, k);

        barrier::field!(kw, Continuation, link)
            .unlock()
            .set(lower.into());
        k.length.set(n);
        k.clength.set(n);
        barrier::field!(kw, Continuation, stack).unlock().set(s);
        set_return_address(s, markers::underflow());
    }
}

/// Before copying the multi-shot (or unresumable one-shot) continuation `k`
/// and `value_count` values onto the live stack, this splits off its top
/// frames when it exceeds the copy bound, promotes it if it was a one-shot,
/// and gets a larger stack when they do not fit.
pub(super) unsafe fn split_and_resize<'gc>(
    ctx: MutationContext<'gc>,
    k: Gc<'gc, Continuation<'gc>>,
    value_count: usize,
) {
    unsafe {
        if k.clength.get() > UNDERFLOW_LIMIT as isize {
            let front_stack_ptr = k.stack.get();
            let end_stack_ptr = front_stack_ptr.offset(k.clength.get());

            let guard = end_stack_ptr.sub(UNDERFLOW_LIMIT);

            // set split point to base of top frame
            if let Some(frame_size) = caller_frame_size(end_stack_ptr, k.return_address.get()) {
                let mut split_point = end_stack_ptr.sub(frame_size);

                // split only if we have more than one frame
                if split_point != front_stack_ptr {
                    // walk the stack to set split_point at first frame above
                    // guard, note that first frame may have put us below the
                    // guard already
                    loop {
                        let frame_size =
                            caller_frame_size(split_point, return_address(split_point))
                                .expect("a continuation frame above its base has a caller");
                        let p = split_point.sub(frame_size);
                        if p < guard {
                            break;
                        }
                        split_point = p;
                    }

                    // promote to multi-shot if necessary
                    promote_to_multishot(k.into());

                    // split
                    split(ctx, k, split_point);
                }
            }
        }

        // Make sure the stack is big enough to hold the continuation. This is
        // conservative. The 2 is the returning frame's header.
        let n = k.clength.get() as usize + 2 + value_count + STACK_SLOP;
        if n >= ctx.tc().stack.scheme_stack_size.get() {
            reset_scheme_stack(ctx, n, DEFAULT_STACK_SIZE);
        }
    }
}

/// The number of frames in the continuation `k` and the continuations below
/// it.
///
/// Terminates on a shot one-shot continuation (which the null continuation
/// is) and at the end of the chain (`#f`). An empty segment, which
/// reification makes to hide a link, holds no frames and is skipped.
///
/// # Safety
///
/// `k` is a chain of [`Continuation`]s ending in `#f`, each with intact frames in
/// `[stack, stack + clength)`: every return address the walk decodes (all
/// but each segment's bottom frame) is a marker with an `RpHeader`, and
/// every interpreted frame's `fp[-1]` is a return point, as in
/// [`sweep_stack`].
pub unsafe fn continuation_depth<'gc>(mut k: Value<'gc>) -> usize {
    unsafe {
        let mut n = 0;
        // terminate on shot 1-shot, which could be null_continuation
        while let Some(continuation) = k.try_as::<Continuation>() {
            if continuation.length.get() == SHOT_1_SHOT_FLAG {
                break;
            }
            let stack_base = continuation.stack.get();
            let mut stack_ptr = stack_base.offset(continuation.clength.get());
            if stack_ptr != stack_base {
                let mut ret = continuation.return_address.get();
                let mut frame_size = caller_frame_size(stack_ptr, ret)
                    .expect("a continuation frame above its base has a caller");
                loop {
                    stack_ptr = stack_ptr.sub(frame_size);
                    n += 1;
                    if stack_ptr == stack_base {
                        break;
                    }
                    ret = return_address(stack_ptr);
                    frame_size = caller_frame_size(stack_ptr, ret)
                        .expect("a continuation frame above its base has a caller");
                }
            }
            k = continuation.link.get();
        }
        n
    }
}

/// The continuation holding frame
/// `n` of `k`, counting the top frame as 0 and linked to the frames below
/// it, or `#f` when `k` holds fewer than `n + 1` frames.
///
/// Splits `k` above and below the frame, promoting to multi-shot first, so
/// the returned object and the rest of `k` are ordinary multi-shot
/// continuations, invoking any of them behaves as invoking `k` did.
///
/// # Safety
///
/// As [`continuation_depth`]: `k` is a chain of [`Continuation`]s ending in `#f`
/// with intact frames.
pub unsafe fn single_continuation<'gc>(
    ctx: MutationContext<'gc>,
    mut k: Value<'gc>,
    mut n: usize,
) -> Value<'gc> {
    unsafe {
        // bug out on shot 1-shots, which could be null_continuation
        while let Some(continuation) = k.try_as::<Continuation>() {
            if continuation.length.get() == SHOT_1_SHOT_FLAG {
                break;
            }
            let stack_base = continuation.stack.get();
            let stack_top = stack_base.offset(continuation.clength.get());
            let mut stack_ptr = stack_top;
            if stack_ptr == stack_base {
                // An empty segment holds no frames, the frame is below it.
                k = continuation.link.get();
                continue;
            }
            let mut ret = continuation.return_address.get();
            let mut frame_size = caller_frame_size(stack_ptr, ret)
                .expect("a continuation frame above its base has a caller");
            loop {
                if n == 0 {
                    // promote to multi-shot if necessary, even if we don't end
                    // up in split, since inspector assumes multi-shot
                    promote_to_multishot(k);
                    if stack_ptr != stack_top {
                        split(ctx, continuation, stack_ptr);
                        k = continuation.link.get();
                    }
                    stack_ptr = stack_ptr.sub(frame_size);
                    if stack_ptr != stack_base {
                        split(ctx, k.downcast::<Continuation>(), stack_ptr);
                    }
                    return k;
                } else {
                    n -= 1;
                    stack_ptr = stack_ptr.sub(frame_size);
                    if stack_ptr == stack_base {
                        break;
                    }
                    ret = return_address(stack_ptr);
                    frame_size = caller_frame_size(stack_ptr, ret)
                        .expect("a continuation frame above its base has a caller");
                }
            }
            k = continuation.link.get();
        }
        Value::new(false)
    }
}
