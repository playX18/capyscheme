use std::sync::OnceLock;

use crate::Rootable;
use crate::prelude::*;
use crate::rsgc::{
    Gc, Global as GcGlobal, Trace, Visitor, Weak,
    alloc::{Array, ArrayRef},
    barrier,
    cell::Lock,
    object::{
        AllocationHooks, AllocationHooksOf, ClassId, GCObject, MAX_CLASS_ID, allocate_class_id,
        builtin_class_ids, class_header_word, drain_pending_type_classes,
        pending_hooks_for_class_id,
    },
    sync::monitor::Monitor,
    weak::WeakProcessor,
};
use crate::runtime::value::conversions::ClassTagged;
use crate::runtime::{
    Context,
    value::{Closure, NativeReturn, Value},
};
use std::fmt;

const CLASS_TABLE_PAGE_BITS: u32 = 8;
const CLASS_TABLE_PAGE_SIZE: usize = 1 << CLASS_TABLE_PAGE_BITS;
const CLASS_TABLE_SLOT_MASK: u32 = (CLASS_TABLE_PAGE_SIZE as u32) - 1;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ClassCategory {
    Abstract,
    Builtin,
    Immediate,
    Internal,
    Scheme,
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct ClassFlags(u32);

impl ClassFlags {
    const APPLICABLE: u32 = 1 << 0;
    const MALLEABLE: u32 = 1 << 1;
    const AGGREGATE: u32 = 1 << 2;
    const SEALED: u32 = 1 << 3;

    pub const fn empty() -> Self {
        Self(0)
    }

    pub const fn builtin() -> Self {
        Self(Self::SEALED)
    }

    pub const fn internal() -> Self {
        Self(Self::SEALED)
    }

    pub const fn scheme() -> Self {
        Self(Self::MALLEABLE | Self::AGGREGATE)
    }

    pub const fn scheme_applicable() -> Self {
        Self(Self::MALLEABLE | Self::AGGREGATE | Self::APPLICABLE)
    }

    pub const fn applicable(self) -> bool {
        self.0 & Self::APPLICABLE != 0
    }

    pub const fn malleable(self) -> bool {
        self.0 & Self::MALLEABLE != 0
    }

    pub const fn aggregate(self) -> bool {
        self.0 & Self::AGGREGATE != 0
    }

    pub const fn sealed(self) -> bool {
        self.0 & Self::SEALED != 0
    }

    pub const fn with_malleable(self) -> Self {
        Self(self.0 | Self::MALLEABLE)
    }

    pub const fn without_malleable(self) -> Self {
        Self(self.0 & !Self::MALLEABLE)
    }

    pub const fn with_sealed(self) -> Self {
        Self(self.0 | Self::SEALED)
    }

    pub const fn without_sealed(self) -> Self {
        Self(self.0 & !Self::SEALED)
    }
}

unsafe impl Trace for ClassFlags {
    unsafe fn trace(&mut self, visitor: &mut Visitor<'_>) {
        let _ = visitor;
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::rsgc::WeakProcessor) {
        let _ = weak_processor;
    }
}

impl ClassCategory {
    fn default_flags(self) -> ClassFlags {
        match self {
            Self::Scheme => ClassFlags::scheme(),
            Self::Builtin => ClassFlags::builtin(),
            Self::Internal => ClassFlags::internal(),
            Self::Abstract | Self::Immediate => ClassFlags::empty(),
        }
    }
}

#[derive(Clone, Copy)]
pub struct PrimitiveLayoutHooks {
    trace: extern "C" fn(GCObject, &mut Visitor),
    weak_proc: extern "C" fn(GCObject, &mut WeakProcessor),
    instance_size: usize,
    compute_size: Option<extern "C" fn(GCObject) -> usize>,
    alignment: usize,
    compute_alignment: Option<extern "C" fn(GCObject) -> usize>,
    type_name: &'static str,
}

impl PrimitiveLayoutHooks {
    pub const fn from_allocation_hooks(hooks: AllocationHooks) -> Self {
        Self {
            trace: hooks.trace,
            weak_proc: hooks.weak_proc,
            instance_size: hooks.instance_size,
            compute_size: hooks.compute_size,
            alignment: hooks.alignment,
            compute_alignment: hooks.compute_alignment,
            type_name: hooks.type_name,
        }
    }

    pub fn trace(self) -> extern "C" fn(GCObject, &mut Visitor) {
        self.trace
    }

    pub fn weak_proc(self) -> extern "C" fn(GCObject, &mut WeakProcessor) {
        self.weak_proc
    }

    pub fn instance_size(self) -> usize {
        self.instance_size
    }

    pub fn compute_size(self) -> Option<extern "C" fn(GCObject) -> usize> {
        self.compute_size
    }

    pub fn alignment(self) -> usize {
        self.alignment
    }

    pub fn compute_alignment(self) -> Option<extern "C" fn(GCObject) -> usize> {
        self.compute_alignment
    }

    pub fn type_name(self) -> &'static str {
        self.type_name
    }
}

impl core::fmt::Debug for PrimitiveLayoutHooks {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("PrimitiveLayoutHooks")
            .field("instance_size", &self.instance_size)
            .field("compute_size", &self.compute_size.is_some())
            .field("alignment", &self.alignment)
            .field("compute_alignment", &self.compute_alignment.is_some())
            .field("type_name", &self.type_name)
            .finish()
    }
}

type PrimitiveAllocateHook = for<'gc> fn(
    Context<'gc>,
    Gc<'gc, ClassDescriptor<'gc>>,
    &[(Value<'gc>, Value<'gc>)],
) -> Result<Value<'gc>, SlotInitError<'gc>>;
type PrimitivePrintHook = for<'gc, 'a, 'b> fn(
    &'b mut fmt::Formatter<'a>,
    Gc<'gc, ClassDescriptor<'gc>>,
    Value<'gc>,
) -> fmt::Result;
type PrimitiveCompareHook =
    for<'gc> fn(Gc<'gc, ClassDescriptor<'gc>>, Value<'gc>, Value<'gc>) -> Option<bool>;
type PrimitiveHashHook = for<'gc> fn(Gc<'gc, ClassDescriptor<'gc>>, Value<'gc>) -> u64;
type PrimitiveSlotRefHook = for<'gc> fn(
    Context<'gc>,
    Gc<'gc, ClassDescriptor<'gc>>,
    Value<'gc>,
    SlotAccessorDescriptor<'gc>,
) -> Result<Value<'gc>, SlotAccessError>;
type PrimitiveSlotSetHook = for<'gc> fn(
    Context<'gc>,
    Gc<'gc, ClassDescriptor<'gc>>,
    Value<'gc>,
    SlotAccessorDescriptor<'gc>,
    Value<'gc>,
) -> Result<(), SlotAccessError>;
type PrimitiveSlotBoundHook = for<'gc> fn(
    Context<'gc>,
    Gc<'gc, ClassDescriptor<'gc>>,
    Value<'gc>,
    SlotAccessorDescriptor<'gc>,
) -> Result<bool, SlotAccessError>;

#[derive(Clone, Copy, Default)]
pub struct PrimitiveOperationHooks {
    allocate: Option<PrimitiveAllocateHook>,
    print: Option<PrimitivePrintHook>,
    compare: Option<PrimitiveCompareHook>,
    hash: Option<PrimitiveHashHook>,
    slot_ref: Option<PrimitiveSlotRefHook>,
    slot_set: Option<PrimitiveSlotSetHook>,
    slot_bound: Option<PrimitiveSlotBoundHook>,
}

impl PrimitiveOperationHooks {
    pub const fn new(
        allocate: Option<PrimitiveAllocateHook>,
        print: Option<PrimitivePrintHook>,
        compare: Option<PrimitiveCompareHook>,
        hash: Option<PrimitiveHashHook>,
        slot_ref: Option<PrimitiveSlotRefHook>,
        slot_set: Option<PrimitiveSlotSetHook>,
        slot_bound: Option<PrimitiveSlotBoundHook>,
    ) -> Self {
        Self {
            allocate,
            print,
            compare,
            hash,
            slot_ref,
            slot_set,
            slot_bound,
        }
    }

    pub fn allocate(self) -> Option<PrimitiveAllocateHook> {
        self.allocate
    }

    pub fn print(self) -> Option<PrimitivePrintHook> {
        self.print
    }

    pub fn compare(self) -> Option<PrimitiveCompareHook> {
        self.compare
    }

    pub fn hash(self) -> Option<PrimitiveHashHook> {
        self.hash
    }

    pub fn slot_ref(self) -> Option<PrimitiveSlotRefHook> {
        self.slot_ref
    }

    pub fn slot_set(self) -> Option<PrimitiveSlotSetHook> {
        self.slot_set
    }

    pub fn slot_bound(self) -> Option<PrimitiveSlotBoundHook> {
        self.slot_bound
    }
}

impl core::fmt::Debug for PrimitiveOperationHooks {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("PrimitiveOperationHooks")
            .field("allocate", &self.allocate.is_some())
            .field("print", &self.print.is_some())
            .field("compare", &self.compare.is_some())
            .field("hash", &self.hash.is_some())
            .field("slot_ref", &self.slot_ref.is_some())
            .field("slot_set", &self.slot_set.is_some())
            .field("slot_bound", &self.slot_bound.is_some())
            .finish()
    }
}

pub struct ClassDescriptor<'gc> {
    id: ClassId,
    name: ArrayRef<'gc, u8>,
    category: ClassCategory,
    flags: Lock<ClassFlags>,
    primitive_layout_hooks: Option<PrimitiveLayoutHooks>,
    primitive_operation_hooks: PrimitiveOperationHooks,
    direct_supers: ArrayRef<'gc, ClassId>,
    cpl: ArrayRef<'gc, ClassId>,
    direct_slots: ArrayRef<'gc, SlotDescriptor<'gc>>,
    slots: ArrayRef<'gc, SlotDescriptor<'gc>>,
    accessors: ArrayRef<'gc, SlotAccessorDescriptor<'gc>>,
    class_slots: ArrayRef<'gc, Lock<Value<'gc>>>,
    direct_subclasses: Lock<ArrayRef<'gc, Weak<'gc, ClassDescriptor<'gc>>>>,
    direct_methods: Lock<ArrayRef<'gc, Value<'gc>>>,
    initargs: ArrayRef<'gc, Value<'gc>>,
}

struct ClassDescriptorInit<'a, 'gc> {
    id: ClassId,
    name: &'a str,
    category: ClassCategory,
    primitive_layout_hooks: Option<PrimitiveLayoutHooks>,
    primitive_operation_hooks: Option<PrimitiveOperationHooks>,
    flags: Option<ClassFlags>,
    direct_supers: &'a [ClassId],
    cpl: &'a [ClassId],
    inherited_slots: &'a [SlotDescriptor<'gc>],
    direct_slot_specs: &'a [SlotSpec<'a, 'gc>],
    direct_subclasses: Option<ArrayRef<'gc, Weak<'gc, ClassDescriptor<'gc>>>>,
    direct_methods: Option<ArrayRef<'gc, Value<'gc>>>,
}

impl core::fmt::Debug for ClassDescriptor<'_> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("ClassDescriptor")
            .field("id", &self.id)
            .field("name", &self.name())
            .field("category", &self.category)
            .field("flags", &self.flags())
            .field("primitive_layout_hooks", &self.primitive_layout_hooks)
            .field("primitive_operation_hooks", &self.primitive_operation_hooks)
            .field("direct_supers", &self.direct_supers())
            .field("cpl", &self.cpl())
            .field("direct_slots_len", &self.direct_slots.len())
            .field("slots_len", &self.slots.len())
            .field("accessors_len", &self.accessors.len())
            .field("class_slots_len", &self.class_slots.len())
            .field("direct_subclasses_len", &self.direct_subclasses.get().len())
            .field("direct_methods_len", &self.direct_methods.get().len())
            .field("initargs_len", &self.initargs.len())
            .finish()
    }
}

unsafe impl Trace for ClassDescriptor<'_> {
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        unsafe {
            self.name.trace(visitor);
            self.flags.trace(visitor);
            self.direct_supers.trace(visitor);
            self.cpl.trace(visitor);
            self.direct_slots.trace(visitor);
            self.slots.trace(visitor);
            self.accessors.trace(visitor);
            self.class_slots.trace(visitor);
            self.direct_subclasses.trace(visitor);
            self.direct_methods.trace(visitor);
            self.initargs.trace(visitor);
        }
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::rsgc::WeakProcessor) {
        unsafe {
            self.name.process_weak_refs(weak_processor);
            self.flags.process_weak_refs(weak_processor);
            self.direct_supers.process_weak_refs(weak_processor);
            self.cpl.process_weak_refs(weak_processor);
            self.direct_slots.process_weak_refs(weak_processor);
            self.slots.process_weak_refs(weak_processor);
            self.accessors.process_weak_refs(weak_processor);
            self.class_slots.process_weak_refs(weak_processor);
            self.direct_subclasses.process_weak_refs(weak_processor);
            self.direct_methods.process_weak_refs(weak_processor);
            self.initargs.process_weak_refs(weak_processor);
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum GenericDispatchError {
    Arity,
    NoApplicableMethod,
}

pub struct GenericInvocation<'gc> {
    body: Value<'gc>,
    args: ArrayRef<'gc, Value<'gc>>,
}

impl<'gc> GenericInvocation<'gc> {
    pub fn body(&self) -> Value<'gc> {
        self.body
    }

    pub fn args(&self) -> ArrayRef<'gc, Value<'gc>> {
        self.args
    }
}

#[derive(Trace)]
#[collect(no_drop)]
#[repr(C)]
pub struct GenericDescriptor<'gc> {
    name: ArrayRef<'gc, u8>,
    methods: Lock<ArrayRef<'gc, Gc<'gc, MethodDescriptor<'gc>>>>,
    dispatcher_cache: Lock<ArrayRef<'gc, GenericDispatcherCacheEntry<'gc>>>,
    max_required_dispatch_args: usize,
    fallback: Lock<Value<'gc>>,
    flags: Lock<GenericFlags>,
}

#[derive(Clone, Copy, Trace)]
pub struct GenericDispatcherCacheEntry<'gc> {
    class_ids: ArrayRef<'gc, ClassId>,
    methods: ArrayRef<'gc, Gc<'gc, MethodDescriptor<'gc>>>,
}

impl<'gc> GenericDispatcherCacheEntry<'gc> {
    pub fn class_ids(&self) -> ArrayRef<'gc, ClassId> {
        self.class_ids
    }

    pub fn methods(&self) -> ArrayRef<'gc, Gc<'gc, MethodDescriptor<'gc>>> {
        self.methods
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Trace)]
pub struct GenericFlags(u8);

impl GenericFlags {
    const SEALED: u8 = 1 << 0;

    pub const fn empty() -> Self {
        Self(0)
    }

    pub const fn sealed(self) -> bool {
        self.0 & Self::SEALED != 0
    }

    pub const fn with_sealed(self) -> Self {
        Self(self.0 | Self::SEALED)
    }

    pub const fn without_sealed(self) -> Self {
        Self(self.0 & !Self::SEALED)
    }
}

fn generic_property_entry<'gc>(
    ctx: Context<'gc>,
    key: Value<'gc>,
    value: Value<'gc>,
    rest: Value<'gc>,
) -> Value<'gc> {
    Value::cons(ctx, Value::cons(ctx, key, value), rest)
}

fn generic_descriptor_from_value<'gc>(
    ctx: Context<'gc>,
    value: Value<'gc>,
) -> Option<Gc<'gc, GenericDescriptor<'gc>>> {
    if value.is::<GenericDescriptor>() {
        return Some(value.downcast::<GenericDescriptor>());
    }

    if !value.is::<Closure>() {
        return None;
    }

    let closure = value.downcast::<Closure>();
    if closure.nfree > 1 {
        let descriptor = closure[1].get();
        if descriptor.is::<GenericDescriptor>() {
            return Some(descriptor.downcast::<GenericDescriptor>());
        }
    }

    let properties = closure.meta.get();
    if properties.is_pair() {
        let descriptor = properties.assq(ctx.intern("generic"))?.cdr();
        if descriptor.is::<GenericDescriptor>() {
            return Some(descriptor.downcast::<GenericDescriptor>());
        }
    }

    None
}

fn generic_assertion_violation<'gc>(
    ctx: Context<'gc>,
    retk: Value<'gc>,
    who: &'static str,
    message: &'static str,
    irritant: Value<'gc>,
) -> NativeReturn<'gc> {
    let assertion_violation = ctx
        .globals()
        .root_module()
        .get_str(ctx, "assertion-violation")
        .unwrap_or_else(|| {
            panic!("pre boot error at {who}: {message} with {irritant}");
        });
    ctx.return_call(
        assertion_violation,
        [ctx.intern(who), ctx.str(message), irritant],
        Some(retk),
    )
}

extern "C-unwind" fn generic_closure_proc<'gc>(
    ctx: Context<'gc>,
    rator: Value<'gc>,
    rands: *const Value<'gc>,
    num_rands: usize,
    retk: Value<'gc>,
) -> NativeReturn<'gc> {
    let generic = match generic_descriptor_from_value(ctx, rator) {
        Some(generic) => generic,
        None => {
            return generic_assertion_violation(
                ctx,
                retk,
                "generic-invoke",
                "generic procedure is missing its descriptor",
                rator,
            );
        }
    };
    // SAFETY: native procedure trampolines pass `num_rands` contiguous argument values.
    let args = unsafe { std::slice::from_raw_parts(rands, num_rands) };

    match GenericDescriptor::invocation(ctx, generic, args) {
        Ok(invocation) => ctx.return_call(
            invocation.body(),
            invocation.args().iter().copied(),
            Some(retk),
        ),
        Err(GenericDispatchError::Arity) => generic_assertion_violation(
            ctx,
            retk,
            "generic-invoke",
            "not enough dispatch arguments",
            Value::new(num_rands as i32),
        ),
        Err(GenericDispatchError::NoApplicableMethod) => {
            generic_assertion_violation(ctx, retk, "generic-invoke", "no applicable method", rator)
        }
    }
}

fn generic_procedure<'gc>(
    ctx: Context<'gc>,
    name: Value<'gc>,
    generic: Gc<'gc, GenericDescriptor<'gc>>,
) -> Gc<'gc, Closure<'gc>> {
    let properties = generic_property_entry(
        ctx,
        ctx.intern("name"),
        name,
        generic_property_entry(ctx, ctx.intern("generic"), generic.into(), Value::null()),
    );
    PROCEDURES
        .fetch(*ctx)
        .make_closure(ctx, generic_closure_proc, [generic.into()], properties)
}

#[derive(Trace)]
#[collect(no_drop)]
#[repr(C)]
pub struct MethodDescriptor<'gc> {
    generic: Gc<'gc, GenericDescriptor<'gc>>,
    specializers: ArrayRef<'gc, ClassId>,
    required_arg_count: usize,
    body: Lock<Value<'gc>>,
    flags: Lock<MethodFlags>,
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Trace)]
pub struct MethodFlags(u8);

impl MethodFlags {
    const LOCKED: u8 = 1 << 0;

    pub const fn empty() -> Self {
        Self(0)
    }

    pub const fn locked_flag() -> Self {
        Self(Self::LOCKED)
    }

    pub const fn locked(self) -> bool {
        self.0 & Self::LOCKED != 0
    }
}

#[derive(Trace)]
#[collect(no_drop)]
#[repr(C)]
pub struct NextMethodDescriptor<'gc> {
    generic: Gc<'gc, GenericDescriptor<'gc>>,
    methods: ArrayRef<'gc, Gc<'gc, MethodDescriptor<'gc>>>,
    args: ArrayRef<'gc, Value<'gc>>,
    index: usize,
}

impl<'gc> GenericDescriptor<'gc> {
    pub fn new(ctx: Context<'gc>, name: &str, max_required_dispatch_args: usize) -> Gc<'gc, Self> {
        Gc::new_with_header_word(
            *ctx,
            Self {
                name: Array::from_slice(*ctx, name.as_bytes()),
                methods: Lock::new(empty_method_metadata(ctx)),
                dispatcher_cache: Lock::new(empty_dispatcher_cache(ctx)),
                max_required_dispatch_args,
                fallback: Lock::new(Value::empty()),
                flags: Lock::new(GenericFlags::empty()),
            },
            class_header_word(builtin_id(builtin_class_ids::GENERIC)),
        )
    }

    pub fn name(&self) -> &str {
        // Generic names are copied from valid Rust strings when descriptors are built.
        unsafe { std::str::from_utf8_unchecked(self.name.as_slice()) }
    }

    pub fn methods(&self) -> ArrayRef<'gc, Gc<'gc, MethodDescriptor<'gc>>> {
        self.methods.get()
    }

    pub fn dispatcher_cache(&self) -> ArrayRef<'gc, GenericDispatcherCacheEntry<'gc>> {
        self.dispatcher_cache.get()
    }

    pub fn max_required_dispatch_args(&self) -> usize {
        self.max_required_dispatch_args
    }

    pub fn fallback(&self) -> Value<'gc> {
        self.fallback.get()
    }

    pub fn flags(&self) -> GenericFlags {
        self.flags.get()
    }

    pub fn set_sealed(ctx: Context<'gc>, generic: Gc<'gc, GenericDescriptor<'gc>>, sealed: bool) {
        let flags = if sealed {
            generic.flags().with_sealed()
        } else {
            generic.flags().without_sealed()
        };
        barrier::field!(Gc::write(*ctx, generic), GenericDescriptor, flags)
            .unlock()
            .set(flags);
    }

    pub fn set_fallback(
        ctx: Context<'gc>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        fallback: Value<'gc>,
    ) {
        barrier::field!(Gc::write(*ctx, generic), GenericDescriptor, fallback)
            .unlock()
            .set(fallback);
    }

    pub fn add_method(
        ctx: Context<'gc>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        specializers: &[ClassId],
        required_arg_count: usize,
        body: Value<'gc>,
    ) -> Gc<'gc, MethodDescriptor<'gc>> {
        Self::add_method_with_flags(
            ctx,
            generic,
            specializers,
            required_arg_count,
            body,
            MethodFlags::empty(),
        )
    }

    pub fn add_method_with_flags(
        ctx: Context<'gc>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        specializers: &[ClassId],
        required_arg_count: usize,
        body: Value<'gc>,
        flags: MethodFlags,
    ) -> Gc<'gc, MethodDescriptor<'gc>> {
        Self::add_method_in_table(
            ctx,
            class_table(ctx),
            generic,
            specializers,
            required_arg_count,
            body,
            flags,
        )
    }

    pub fn add_method_in_table(
        ctx: Context<'gc>,
        table: &ClassTable<'gc>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        specializers: &[ClassId],
        required_arg_count: usize,
        body: Value<'gc>,
        flags: MethodFlags,
    ) -> Gc<'gc, MethodDescriptor<'gc>> {
        assert!(
            specializers.len() <= required_arg_count,
            "method cannot specialize more arguments than it requires"
        );
        assert!(
            !generic.flags().sealed(),
            "sealed generic cannot accept methods"
        );
        if let Some(method) = generic.find_method(specializers, required_arg_count) {
            MethodDescriptor::set_body(ctx, method, body);
            MethodDescriptor::set_flags(ctx, method, flags);
            Self::clear_dispatcher_cache(ctx, generic);
            return method;
        }

        let method =
            MethodDescriptor::new(ctx, generic, specializers, required_arg_count, body, flags);
        Self::append_method(ctx, generic, method);
        Self::clear_dispatcher_cache(ctx, generic);

        for specializer in specializers {
            if let Some(class) = table.lookup(*specializer) {
                ClassDescriptor::add_direct_method(ctx, class, method.into());
            }
        }

        method
    }

    pub fn find_method(
        &self,
        specializers: &[ClassId],
        required_arg_count: usize,
    ) -> Option<Gc<'gc, MethodDescriptor<'gc>>> {
        self.methods().iter().copied().find(|method| {
            method.required_arg_count() == required_arg_count
                && method.specializers() == specializers
        })
    }

    pub fn select_method(
        &self,
        ctx: Context<'gc>,
        args: &[Value<'gc>],
    ) -> Result<Gc<'gc, MethodDescriptor<'gc>>, GenericDispatchError> {
        self.slow_applicable_methods(ctx, args)
            .map(|methods| methods[0])
    }

    pub fn dispatch_body(
        &self,
        ctx: Context<'gc>,
        args: &[Value<'gc>],
    ) -> Result<Value<'gc>, GenericDispatchError> {
        match self.select_method(ctx, args) {
            Ok(method) => Ok(method.body()),
            Err(GenericDispatchError::NoApplicableMethod) if !self.fallback().is_empty() => {
                Ok(self.fallback())
            }
            Err(error) => Err(error),
        }
    }

    pub fn invocation(
        ctx: Context<'gc>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        args: &[Value<'gc>],
    ) -> Result<GenericInvocation<'gc>, GenericDispatchError> {
        match Self::next_method_chain(ctx, generic, args) {
            Ok(next_method) => Ok(GenericInvocation {
                body: next_method.body(),
                args: NextMethodDescriptor::invocation_args(ctx, next_method),
            }),
            Err(GenericDispatchError::NoApplicableMethod) if !generic.fallback().is_empty() => {
                Ok(GenericInvocation {
                    body: generic.fallback(),
                    args: Array::from_slice(*ctx, args),
                })
            }
            Err(error) => Err(error),
        }
    }

    pub fn dispatch_body_cached(
        ctx: Context<'gc>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        args: &[Value<'gc>],
    ) -> Result<Value<'gc>, GenericDispatchError> {
        match Self::applicable_methods(ctx, generic, args) {
            Ok(methods) => Ok(methods[0].body()),
            Err(GenericDispatchError::NoApplicableMethod) if !generic.fallback().is_empty() => {
                Ok(generic.fallback())
            }
            Err(error) => Err(error),
        }
    }

    pub fn next_method_chain(
        ctx: Context<'gc>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        args: &[Value<'gc>],
    ) -> Result<Gc<'gc, NextMethodDescriptor<'gc>>, GenericDispatchError> {
        let methods = Self::applicable_methods(ctx, generic, args)?;
        Ok(NextMethodDescriptor::new(
            ctx,
            generic,
            methods,
            Array::from_slice(*ctx, args),
            0,
        ))
    }

    fn applicable_methods(
        ctx: Context<'gc>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        args: &[Value<'gc>],
    ) -> Result<ArrayRef<'gc, Gc<'gc, MethodDescriptor<'gc>>>, GenericDispatchError> {
        if args.len() < generic.max_required_dispatch_args {
            return Err(GenericDispatchError::Arity);
        }

        let Some(class_ids) = generic.dispatch_class_ids(ctx, args) else {
            return Err(GenericDispatchError::NoApplicableMethod);
        };

        if let Some(methods) = generic.cached_methods(class_ids.as_slice()) {
            return Ok(methods);
        }

        let methods = generic.slow_applicable_methods(ctx, args)?;
        Self::insert_dispatcher_cache(ctx, generic, class_ids, methods);
        Ok(methods)
    }

    fn slow_applicable_methods(
        &self,
        ctx: Context<'gc>,
        args: &[Value<'gc>],
    ) -> Result<ArrayRef<'gc, Gc<'gc, MethodDescriptor<'gc>>>, GenericDispatchError> {
        if args.len() < self.max_required_dispatch_args {
            return Err(GenericDispatchError::Arity);
        }

        let mut methods = Vec::new();
        for method in self.methods().iter().copied() {
            let Some(score) = method.specificity_score(ctx, args) else {
                continue;
            };
            methods.push((method, score));
        }

        if methods.is_empty() {
            return Err(GenericDispatchError::NoApplicableMethod);
        }

        methods.sort_by(|(_, left), (_, right)| left.cmp(right));
        let methods = Array::from_slice(
            *ctx,
            methods
                .iter()
                .map(|(method, _)| *method)
                .collect::<Vec<_>>(),
        );
        Ok(methods)
    }

    fn dispatch_class_ids(
        &self,
        ctx: Context<'gc>,
        args: &[Value<'gc>],
    ) -> Option<ArrayRef<'gc, ClassId>> {
        let mut class_ids = Vec::with_capacity(args.len());
        for arg in args {
            class_ids.push(arg.class(ctx)?.id());
        }
        Some(Array::from_slice(*ctx, &class_ids))
    }

    fn cached_methods(
        &self,
        class_ids: &[ClassId],
    ) -> Option<ArrayRef<'gc, Gc<'gc, MethodDescriptor<'gc>>>> {
        self.dispatcher_cache()
            .iter()
            .find(|entry| entry.class_ids.as_slice() == class_ids)
            .map(|entry| entry.methods)
    }

    fn insert_dispatcher_cache(
        ctx: Context<'gc>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        class_ids: ArrayRef<'gc, ClassId>,
        methods: ArrayRef<'gc, Gc<'gc, MethodDescriptor<'gc>>>,
    ) {
        let current = generic.dispatcher_cache.get();
        let appended = Array::with(*ctx, current.len() + 1, |_, index| {
            if index < current.len() {
                current[index]
            } else {
                GenericDispatcherCacheEntry { class_ids, methods }
            }
        });
        barrier::field!(
            Gc::write(*ctx, generic),
            GenericDescriptor,
            dispatcher_cache
        )
        .unlock()
        .set(appended);
    }

    fn clear_dispatcher_cache(ctx: Context<'gc>, generic: Gc<'gc, GenericDescriptor<'gc>>) {
        barrier::field!(
            Gc::write(*ctx, generic),
            GenericDescriptor,
            dispatcher_cache
        )
        .unlock()
        .set(empty_dispatcher_cache(ctx));
    }

    fn append_method(
        ctx: Context<'gc>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        method: Gc<'gc, MethodDescriptor<'gc>>,
    ) {
        let current = generic.methods.get();
        let appended = Array::with(*ctx, current.len() + 1, |_, index| {
            if index < current.len() {
                current[index]
            } else {
                method
            }
        });
        barrier::field!(Gc::write(*ctx, generic), GenericDescriptor, methods)
            .unlock()
            .set(appended);
    }
}

impl<'gc> MethodDescriptor<'gc> {
    fn new(
        ctx: Context<'gc>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        specializers: &[ClassId],
        required_arg_count: usize,
        body: Value<'gc>,
        flags: MethodFlags,
    ) -> Gc<'gc, Self> {
        Gc::new_with_header_word(
            *ctx,
            Self {
                generic,
                specializers: Array::from_slice(*ctx, specializers),
                required_arg_count,
                body: Lock::new(body),
                flags: Lock::new(flags),
            },
            class_header_word(builtin_id(builtin_class_ids::METHOD)),
        )
    }

    pub fn generic(&self) -> Gc<'gc, GenericDescriptor<'gc>> {
        self.generic
    }

    pub fn specializers(&self) -> &[ClassId] {
        self.specializers.as_slice()
    }

    pub fn required_arg_count(&self) -> usize {
        self.required_arg_count
    }

    pub fn body(&self) -> Value<'gc> {
        self.body.get()
    }

    pub fn flags(&self) -> MethodFlags {
        self.flags.get()
    }

    pub fn set_body(ctx: Context<'gc>, method: Gc<'gc, MethodDescriptor<'gc>>, body: Value<'gc>) {
        barrier::field!(Gc::write(*ctx, method), MethodDescriptor, body)
            .unlock()
            .set(body);
    }

    pub fn set_flags(
        ctx: Context<'gc>,
        method: Gc<'gc, MethodDescriptor<'gc>>,
        flags: MethodFlags,
    ) {
        barrier::field!(Gc::write(*ctx, method), MethodDescriptor, flags)
            .unlock()
            .set(flags);
    }

    fn specificity_score(&self, ctx: Context<'gc>, args: &[Value<'gc>]) -> Option<Vec<usize>> {
        if args.len() < self.required_arg_count || args.len() < self.specializers.len() {
            return None;
        }

        let mut score = Vec::with_capacity(args.len() + 1);
        for (index, arg) in args.iter().enumerate() {
            if let Some(specializer) = self.specializers.get(index) {
                let class = arg.class(ctx)?;
                let distance = class
                    .cpl()
                    .iter()
                    .position(|class_id| class_id == specializer)?;
                score.push(distance);
            } else {
                score.push(usize::MAX / 2);
            }
        }
        score.push(usize::MAX - self.required_arg_count);

        Some(score)
    }
}

impl<'gc> NextMethodDescriptor<'gc> {
    fn new(
        ctx: Context<'gc>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        methods: ArrayRef<'gc, Gc<'gc, MethodDescriptor<'gc>>>,
        args: ArrayRef<'gc, Value<'gc>>,
        index: usize,
    ) -> Gc<'gc, Self> {
        Gc::new_with_header_word(
            *ctx,
            Self {
                generic,
                methods,
                args,
                index,
            },
            class_header_word(builtin_id(builtin_class_ids::NEXT_METHOD)),
        )
    }

    pub fn generic(&self) -> Gc<'gc, GenericDescriptor<'gc>> {
        self.generic
    }

    pub fn args(&self) -> ArrayRef<'gc, Value<'gc>> {
        self.args
    }

    pub fn methods(&self) -> ArrayRef<'gc, Gc<'gc, MethodDescriptor<'gc>>> {
        self.methods
    }

    pub fn index(&self) -> usize {
        self.index
    }

    pub fn method(&self) -> Gc<'gc, MethodDescriptor<'gc>> {
        self.methods[self.index]
    }

    pub fn body(&self) -> Value<'gc> {
        self.method().body()
    }

    pub fn has_next(&self) -> bool {
        self.index + 1 < self.methods.len()
    }

    pub fn next(&self, ctx: Context<'gc>) -> Option<Gc<'gc, Self>> {
        self.has_next()
            .then(|| Self::new(ctx, self.generic, self.methods, self.args, self.index + 1))
    }

    pub fn invocation_args(
        ctx: Context<'gc>,
        next_method: Gc<'gc, Self>,
    ) -> ArrayRef<'gc, Value<'gc>> {
        Array::with(*ctx, next_method.args.len() + 1, |_, index| {
            if index == 0 {
                next_method.into()
            } else {
                next_method.args[index - 1]
            }
        })
    }

    pub fn next_invocation(
        ctx: Context<'gc>,
        next_method: Gc<'gc, Self>,
    ) -> Option<(Value<'gc>, ArrayRef<'gc, Value<'gc>>)> {
        next_method
            .next(ctx)
            .map(|next_method| (next_method.body(), Self::invocation_args(ctx, next_method)))
    }
}

unsafe impl<'gc> ClassTagged for GenericDescriptor<'gc> {
    const TYPE_NAME: &'static str = "generic";
    const CLASS_IDS: &'static [u32] = &[builtin_class_ids::GENERIC];
}

unsafe impl<'gc> ClassTagged for MethodDescriptor<'gc> {
    const TYPE_NAME: &'static str = "method";
    const CLASS_IDS: &'static [u32] = &[builtin_class_ids::METHOD];
}

unsafe impl<'gc> ClassTagged for NextMethodDescriptor<'gc> {
    const TYPE_NAME: &'static str = "next-method";
    const CLASS_IDS: &'static [u32] = &[builtin_class_ids::NEXT_METHOD];
}

#[derive(Trace)]
#[collect(no_drop)]
#[repr(C)]
pub struct SlotDefinitionDescriptor<'gc> {
    name: ArrayRef<'gc, u8>,
    owner: ClassId,
    index: usize,
    init_value: Value<'gc>,
    init_keyword: Value<'gc>,
    init_thunk: Value<'gc>,
    scheme_slot_ref: Value<'gc>,
    scheme_slot_set: Value<'gc>,
    scheme_slot_bound: Value<'gc>,
    getter: Value<'gc>,
    setter: Value<'gc>,
    accessor: Value<'gc>,
    flags: SlotFlags,
}

#[derive(Trace)]
#[collect(no_drop)]
#[repr(C)]
pub struct SlotAccessorDefinition<'gc> {
    name: ArrayRef<'gc, u8>,
    owner: ClassId,
    slot_index: usize,
    init_value: Value<'gc>,
    init_keyword: Value<'gc>,
    init_thunk: Value<'gc>,
    scheme_slot_ref: Value<'gc>,
    scheme_slot_set: Value<'gc>,
    scheme_slot_bound: Value<'gc>,
    getter: Value<'gc>,
    setter: Value<'gc>,
    accessor: Value<'gc>,
    flags: SlotFlags,
}

impl<'gc> SlotDefinitionDescriptor<'gc> {
    fn from_slot(ctx: Context<'gc>, slot: SlotDescriptor<'gc>) -> Gc<'gc, Self> {
        Gc::new_with_header_word(
            *ctx,
            Self {
                name: Array::from_slice(*ctx, slot.name.as_slice()),
                owner: slot.owner,
                index: slot.index,
                init_value: slot.init_value,
                init_keyword: slot.init_keyword,
                init_thunk: slot.init_thunk,
                scheme_slot_ref: slot.scheme_slot_ref,
                scheme_slot_set: slot.scheme_slot_set,
                scheme_slot_bound: slot.scheme_slot_bound,
                getter: slot.getter,
                setter: slot.setter,
                accessor: slot.accessor,
                flags: slot.flags,
            },
            class_header_word(builtin_id(builtin_class_ids::SLOT_DEFINITION)),
        )
    }

    pub fn name(&self) -> &str {
        // Slot definition names are copied from valid Rust strings.
        unsafe { std::str::from_utf8_unchecked(self.name.as_slice()) }
    }

    pub fn owner(&self) -> ClassId {
        self.owner
    }

    pub fn index(&self) -> usize {
        self.index
    }

    pub fn init_value(&self) -> Value<'gc> {
        self.init_value
    }

    pub fn init_keyword(&self) -> Value<'gc> {
        self.init_keyword
    }

    pub fn init_thunk(&self) -> Value<'gc> {
        self.init_thunk
    }

    pub fn scheme_slot_ref(&self) -> Value<'gc> {
        self.scheme_slot_ref
    }

    pub fn scheme_slot_set(&self) -> Value<'gc> {
        self.scheme_slot_set
    }

    pub fn scheme_slot_bound(&self) -> Value<'gc> {
        self.scheme_slot_bound
    }

    pub fn getter(&self) -> Value<'gc> {
        self.getter
    }

    pub fn setter(&self) -> Value<'gc> {
        self.setter
    }

    pub fn accessor(&self) -> Value<'gc> {
        self.accessor
    }

    pub fn flags(&self) -> SlotFlags {
        self.flags
    }

    pub fn class_allocated(&self) -> bool {
        self.flags.class_allocated()
    }
}

impl<'gc> SlotAccessorDefinition<'gc> {
    fn from_accessor(ctx: Context<'gc>, accessor: SlotAccessorDescriptor<'gc>) -> Gc<'gc, Self> {
        Gc::new_with_header_word(
            *ctx,
            Self {
                name: Array::from_slice(*ctx, accessor.name.as_slice()),
                owner: accessor.owner,
                slot_index: accessor.slot_index,
                init_value: accessor.init_value,
                init_keyword: accessor.init_keyword,
                init_thunk: accessor.init_thunk,
                scheme_slot_ref: accessor.scheme_slot_ref,
                scheme_slot_set: accessor.scheme_slot_set,
                scheme_slot_bound: accessor.scheme_slot_bound,
                getter: accessor.getter,
                setter: accessor.setter,
                accessor: accessor.accessor,
                flags: accessor.flags,
            },
            class_header_word(builtin_id(builtin_class_ids::SLOT_ACCESSOR)),
        )
    }

    pub fn name(&self) -> &str {
        // Accessor definition names are copied from valid Rust strings.
        unsafe { std::str::from_utf8_unchecked(self.name.as_slice()) }
    }

    pub fn owner(&self) -> ClassId {
        self.owner
    }

    pub fn slot_index(&self) -> usize {
        self.slot_index
    }

    pub fn init_value(&self) -> Value<'gc> {
        self.init_value
    }

    pub fn init_keyword(&self) -> Value<'gc> {
        self.init_keyword
    }

    pub fn init_thunk(&self) -> Value<'gc> {
        self.init_thunk
    }

    pub fn scheme_slot_ref(&self) -> Value<'gc> {
        self.scheme_slot_ref
    }

    pub fn scheme_slot_set(&self) -> Value<'gc> {
        self.scheme_slot_set
    }

    pub fn scheme_slot_bound(&self) -> Value<'gc> {
        self.scheme_slot_bound
    }

    pub fn getter(&self) -> Value<'gc> {
        self.getter
    }

    pub fn setter(&self) -> Value<'gc> {
        self.setter
    }

    pub fn accessor(&self) -> Value<'gc> {
        self.accessor
    }

    pub fn flags(&self) -> SlotFlags {
        self.flags
    }

    fn as_accessor_descriptor(&self) -> SlotAccessorDescriptor<'gc> {
        SlotAccessorDescriptor {
            name: self.name,
            owner: self.owner,
            slot_index: self.slot_index,
            init_value: self.init_value,
            init_keyword: self.init_keyword,
            init_thunk: self.init_thunk,
            slot_ref: None,
            slot_set: None,
            slot_bound: None,
            scheme_slot_ref: self.scheme_slot_ref,
            scheme_slot_set: self.scheme_slot_set,
            scheme_slot_bound: self.scheme_slot_bound,
            getter: self.getter,
            setter: self.setter,
            accessor: self.accessor,
            flags: self.flags,
        }
    }
}

unsafe impl<'gc> ClassTagged for SlotDefinitionDescriptor<'gc> {
    const TYPE_NAME: &'static str = "slot-definition";
    const CLASS_IDS: &'static [u32] = &[builtin_class_ids::SLOT_DEFINITION];
}

unsafe impl<'gc> ClassTagged for SlotAccessorDefinition<'gc> {
    const TYPE_NAME: &'static str = "slot-accessor";
    const CLASS_IDS: &'static [u32] = &[builtin_class_ids::SLOT_ACCESSOR];
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Trace)]
pub struct SlotFlags(u8);

impl SlotFlags {
    const IMMUTABLE: u8 = 1 << 0;
    const INITIALIZABLE: u8 = 1 << 1;
    const SETTABLE: u8 = 1 << 2;
    const CLASS_ALLOCATED: u8 = 1 << 3;

    pub const fn mutable() -> Self {
        Self(Self::INITIALIZABLE | Self::SETTABLE)
    }

    pub const fn immutable() -> Self {
        Self(Self::IMMUTABLE | Self::INITIALIZABLE)
    }

    pub const fn immutable_flag(self) -> bool {
        self.0 & Self::IMMUTABLE != 0
    }

    pub const fn initializable(self) -> bool {
        self.0 & Self::INITIALIZABLE != 0
    }

    pub const fn settable(self) -> bool {
        self.0 & Self::SETTABLE != 0
    }

    pub const fn class_allocated(self) -> bool {
        self.0 & Self::CLASS_ALLOCATED != 0
    }
}

#[derive(Clone, Copy, Debug)]
pub struct SlotSpec<'a, 'gc> {
    name: &'a str,
    init_value: Value<'gc>,
    init_keyword: Value<'gc>,
    init_thunk: Value<'gc>,
    slot_ref: Option<PrimitiveSlotRefHook>,
    slot_set: Option<PrimitiveSlotSetHook>,
    slot_bound: Option<PrimitiveSlotBoundHook>,
    scheme_slot_ref: Value<'gc>,
    scheme_slot_set: Value<'gc>,
    scheme_slot_bound: Value<'gc>,
    getter: Value<'gc>,
    setter: Value<'gc>,
    accessor: Value<'gc>,
    flags: SlotFlags,
}

impl<'a, 'gc> SlotSpec<'a, 'gc> {
    pub const fn mutable(name: &'a str) -> Self {
        Self {
            name,
            init_value: Value::empty(),
            init_keyword: Value::empty(),
            init_thunk: Value::empty(),
            slot_ref: None,
            slot_set: None,
            slot_bound: None,
            scheme_slot_ref: Value::empty(),
            scheme_slot_set: Value::empty(),
            scheme_slot_bound: Value::empty(),
            getter: Value::from_raw_i64(Value::VALUE_FALSE),
            setter: Value::from_raw_i64(Value::VALUE_FALSE),
            accessor: Value::from_raw_i64(Value::VALUE_FALSE),
            flags: SlotFlags::mutable(),
        }
    }

    pub const fn immutable(name: &'a str) -> Self {
        Self {
            name,
            init_value: Value::empty(),
            init_keyword: Value::empty(),
            init_thunk: Value::empty(),
            slot_ref: None,
            slot_set: None,
            slot_bound: None,
            scheme_slot_ref: Value::empty(),
            scheme_slot_set: Value::empty(),
            scheme_slot_bound: Value::empty(),
            getter: Value::from_raw_i64(Value::VALUE_FALSE),
            setter: Value::from_raw_i64(Value::VALUE_FALSE),
            accessor: Value::from_raw_i64(Value::VALUE_FALSE),
            flags: SlotFlags::immutable(),
        }
    }

    pub const fn with_init_value(mut self, value: Value<'gc>) -> Self {
        self.init_value = value;
        self
    }

    pub const fn with_init_keyword(mut self, keyword: Value<'gc>) -> Self {
        self.init_keyword = keyword;
        self
    }

    pub const fn with_init_thunk(mut self, thunk: Value<'gc>) -> Self {
        self.init_thunk = thunk;
        self
    }

    pub const fn with_native_slot_hooks(
        mut self,
        slot_ref: Option<PrimitiveSlotRefHook>,
        slot_set: Option<PrimitiveSlotSetHook>,
        slot_bound: Option<PrimitiveSlotBoundHook>,
    ) -> Self {
        self.slot_ref = slot_ref;
        self.slot_set = slot_set;
        self.slot_bound = slot_bound;
        self
    }

    pub const fn with_scheme_slot_procedures(
        mut self,
        slot_ref: Value<'gc>,
        slot_set: Value<'gc>,
        slot_bound: Value<'gc>,
    ) -> Self {
        self.scheme_slot_ref = slot_ref;
        self.scheme_slot_set = slot_set;
        self.scheme_slot_bound = slot_bound;
        self
    }

    pub const fn with_accessor_names(
        mut self,
        getter: Value<'gc>,
        setter: Value<'gc>,
        accessor: Value<'gc>,
    ) -> Self {
        self.getter = getter;
        self.setter = setter;
        self.accessor = accessor;
        self
    }

    pub const fn without_initargs(mut self) -> Self {
        self.flags.0 &= !SlotFlags::INITIALIZABLE;
        self
    }

    pub const fn without_setter(mut self) -> Self {
        self.flags.0 &= !SlotFlags::SETTABLE;
        self
    }

    pub const fn with_class_allocation(mut self) -> Self {
        self.flags.0 |= SlotFlags::CLASS_ALLOCATED;
        self
    }
}

#[derive(Clone, Copy, Debug)]
pub struct SlotDescriptor<'gc> {
    name: ArrayRef<'gc, u8>,
    owner: ClassId,
    index: usize,
    init_value: Value<'gc>,
    init_keyword: Value<'gc>,
    init_thunk: Value<'gc>,
    slot_ref: Option<PrimitiveSlotRefHook>,
    slot_set: Option<PrimitiveSlotSetHook>,
    slot_bound: Option<PrimitiveSlotBoundHook>,
    scheme_slot_ref: Value<'gc>,
    scheme_slot_set: Value<'gc>,
    scheme_slot_bound: Value<'gc>,
    getter: Value<'gc>,
    setter: Value<'gc>,
    accessor: Value<'gc>,
    flags: SlotFlags,
}

unsafe impl Trace for SlotDescriptor<'_> {
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        unsafe {
            self.name.trace(visitor);
            self.init_value.trace(visitor);
            self.init_keyword.trace(visitor);
            self.init_thunk.trace(visitor);
            self.scheme_slot_ref.trace(visitor);
            self.scheme_slot_set.trace(visitor);
            self.scheme_slot_bound.trace(visitor);
            self.getter.trace(visitor);
            self.setter.trace(visitor);
            self.accessor.trace(visitor);
        }
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::rsgc::WeakProcessor) {
        unsafe {
            self.name.process_weak_refs(weak_processor);
            self.init_value.process_weak_refs(weak_processor);
            self.init_keyword.process_weak_refs(weak_processor);
            self.init_thunk.process_weak_refs(weak_processor);
            self.scheme_slot_ref.process_weak_refs(weak_processor);
            self.scheme_slot_set.process_weak_refs(weak_processor);
            self.scheme_slot_bound.process_weak_refs(weak_processor);
            self.getter.process_weak_refs(weak_processor);
            self.setter.process_weak_refs(weak_processor);
            self.accessor.process_weak_refs(weak_processor);
        }
    }
}

impl<'gc> SlotDescriptor<'gc> {
    fn from_spec(ctx: Context<'gc>, owner: ClassId, index: usize, spec: SlotSpec<'_, 'gc>) -> Self {
        Self {
            name: Array::from_slice(*ctx, spec.name.as_bytes()),
            owner,
            index,
            init_value: spec.init_value,
            init_keyword: spec.init_keyword,
            init_thunk: spec.init_thunk,
            slot_ref: spec.slot_ref,
            slot_set: spec.slot_set,
            slot_bound: spec.slot_bound,
            scheme_slot_ref: spec.scheme_slot_ref,
            scheme_slot_set: spec.scheme_slot_set,
            scheme_slot_bound: spec.scheme_slot_bound,
            getter: spec.getter,
            setter: spec.setter,
            accessor: spec.accessor,
            flags: spec.flags,
        }
    }

    pub fn name(&self) -> &str {
        // Slot names are copied from valid Rust strings when descriptors are built.
        unsafe { std::str::from_utf8_unchecked(self.name.as_slice()) }
    }

    pub fn owner(&self) -> ClassId {
        self.owner
    }

    pub fn index(&self) -> usize {
        self.index
    }

    pub fn init_value(&self) -> Value<'gc> {
        self.init_value
    }

    pub fn init_keyword(&self) -> Value<'gc> {
        self.init_keyword
    }

    pub fn init_thunk(&self) -> Value<'gc> {
        self.init_thunk
    }

    pub fn slot_ref(&self) -> Option<PrimitiveSlotRefHook> {
        self.slot_ref
    }

    pub fn slot_set(&self) -> Option<PrimitiveSlotSetHook> {
        self.slot_set
    }

    pub fn slot_bound(&self) -> Option<PrimitiveSlotBoundHook> {
        self.slot_bound
    }

    pub fn scheme_slot_ref(&self) -> Value<'gc> {
        self.scheme_slot_ref
    }

    pub fn scheme_slot_set(&self) -> Value<'gc> {
        self.scheme_slot_set
    }

    pub fn scheme_slot_bound(&self) -> Value<'gc> {
        self.scheme_slot_bound
    }

    pub fn getter(&self) -> Value<'gc> {
        self.getter
    }

    pub fn setter(&self) -> Value<'gc> {
        self.setter
    }

    pub fn accessor(&self) -> Value<'gc> {
        self.accessor
    }

    pub fn flags(&self) -> SlotFlags {
        self.flags
    }

    pub fn immutable(&self) -> bool {
        self.flags.immutable_flag()
    }

    pub fn class_allocated(&self) -> bool {
        self.flags.class_allocated()
    }
}

#[derive(Clone, Copy, Debug)]
pub struct SlotAccessorDescriptor<'gc> {
    name: ArrayRef<'gc, u8>,
    owner: ClassId,
    slot_index: usize,
    init_value: Value<'gc>,
    init_keyword: Value<'gc>,
    init_thunk: Value<'gc>,
    slot_ref: Option<PrimitiveSlotRefHook>,
    slot_set: Option<PrimitiveSlotSetHook>,
    slot_bound: Option<PrimitiveSlotBoundHook>,
    scheme_slot_ref: Value<'gc>,
    scheme_slot_set: Value<'gc>,
    scheme_slot_bound: Value<'gc>,
    getter: Value<'gc>,
    setter: Value<'gc>,
    accessor: Value<'gc>,
    flags: SlotFlags,
}

unsafe impl Trace for SlotAccessorDescriptor<'_> {
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        unsafe {
            self.name.trace(visitor);
            self.init_value.trace(visitor);
            self.init_keyword.trace(visitor);
            self.init_thunk.trace(visitor);
            self.scheme_slot_ref.trace(visitor);
            self.scheme_slot_set.trace(visitor);
            self.scheme_slot_bound.trace(visitor);
            self.getter.trace(visitor);
            self.setter.trace(visitor);
            self.accessor.trace(visitor);
        }
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::rsgc::WeakProcessor) {
        unsafe {
            self.name.process_weak_refs(weak_processor);
            self.init_value.process_weak_refs(weak_processor);
            self.init_keyword.process_weak_refs(weak_processor);
            self.init_thunk.process_weak_refs(weak_processor);
            self.scheme_slot_ref.process_weak_refs(weak_processor);
            self.scheme_slot_set.process_weak_refs(weak_processor);
            self.scheme_slot_bound.process_weak_refs(weak_processor);
            self.getter.process_weak_refs(weak_processor);
            self.setter.process_weak_refs(weak_processor);
            self.accessor.process_weak_refs(weak_processor);
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SlotAccessError {
    Immutable,
    OutOfBounds,
    SchemeHookFailed,
    Unbound,
    Unknown,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SlotInitError<'gc> {
    UnknownKeyword(Value<'gc>),
    NotInitializable(Value<'gc>),
    InitThunkFailed(Value<'gc>),
    MissingInvokerSlot(Value<'gc>),
    InvokerNotProcedure(Value<'gc>),
}

impl<'gc> SlotAccessorDescriptor<'gc> {
    fn from_slot(ctx: Context<'gc>, slot: SlotDescriptor<'gc>) -> Self {
        Self {
            name: Array::from_slice(*ctx, slot.name.as_slice()),
            owner: slot.owner,
            slot_index: slot.index,
            init_value: slot.init_value,
            init_keyword: slot.init_keyword,
            init_thunk: slot.init_thunk,
            slot_ref: slot.slot_ref,
            slot_set: slot.slot_set,
            slot_bound: slot.slot_bound,
            scheme_slot_ref: slot.scheme_slot_ref,
            scheme_slot_set: slot.scheme_slot_set,
            scheme_slot_bound: slot.scheme_slot_bound,
            getter: slot.getter,
            setter: slot.setter,
            accessor: slot.accessor,
            flags: slot.flags,
        }
    }

    pub fn name(&self) -> &str {
        // Accessor names are copied from slot names, which originate as valid Rust strings.
        unsafe { std::str::from_utf8_unchecked(self.name.as_slice()) }
    }

    pub fn owner(&self) -> ClassId {
        self.owner
    }

    pub fn slot_index(&self) -> usize {
        self.slot_index
    }

    pub fn init_value(&self) -> Value<'gc> {
        self.init_value
    }

    pub fn init_keyword(&self) -> Value<'gc> {
        self.init_keyword
    }

    pub fn init_thunk(&self) -> Value<'gc> {
        self.init_thunk
    }

    pub fn slot_ref(&self) -> Option<PrimitiveSlotRefHook> {
        self.slot_ref
    }

    pub fn slot_set(&self) -> Option<PrimitiveSlotSetHook> {
        self.slot_set
    }

    pub fn slot_bound(&self) -> Option<PrimitiveSlotBoundHook> {
        self.slot_bound
    }

    pub fn scheme_slot_ref(&self) -> Value<'gc> {
        self.scheme_slot_ref
    }

    pub fn scheme_slot_set(&self) -> Value<'gc> {
        self.scheme_slot_set
    }

    pub fn scheme_slot_bound(&self) -> Value<'gc> {
        self.scheme_slot_bound
    }

    pub fn getter(&self) -> Value<'gc> {
        self.getter
    }

    pub fn setter(&self) -> Value<'gc> {
        self.setter
    }

    pub fn accessor(&self) -> Value<'gc> {
        self.accessor
    }

    pub fn flags(&self) -> SlotFlags {
        self.flags
    }

    pub fn class_allocated(&self) -> bool {
        self.flags.class_allocated()
    }
}

#[derive(Trace)]
#[collect(no_drop)]
#[repr(C)]
pub struct SchemeInstance<'gc> {
    class: Gc<'gc, ClassDescriptor<'gc>>,
    slots: ArrayRef<'gc, Lock<Value<'gc>>>,
}

impl<'gc> SchemeInstance<'gc> {
    pub fn new(
        ctx: Context<'gc>,
        class: Gc<'gc, ClassDescriptor<'gc>>,
        slot_count: usize,
    ) -> Gc<'gc, Self> {
        debug_assert_eq!(class.category(), ClassCategory::Scheme);
        let header_word = class_header_word(class.id());
        Gc::new_with_header_word(
            *ctx,
            Self {
                class,
                slots: Array::with(*ctx, slot_count, |_, _| Lock::new(Value::empty())),
            },
            header_word,
        )
    }

    pub fn allocate(ctx: Context<'gc>, class: Gc<'gc, ClassDescriptor<'gc>>) -> Gc<'gc, Self> {
        debug_assert_eq!(class.category(), ClassCategory::Scheme);
        let header_word = class_header_word(class.id());
        Gc::new_with_header_word(
            *ctx,
            Self {
                class,
                slots: Array::with(*ctx, class.slot_count(), |_, index| {
                    Lock::new(class.slots()[index].init_value())
                }),
            },
            header_word,
        )
    }

    pub fn allocate_with_initargs(
        ctx: Context<'gc>,
        class: Gc<'gc, ClassDescriptor<'gc>>,
        initargs: &[(Value<'gc>, Value<'gc>)],
    ) -> Result<Gc<'gc, Self>, SlotInitError<'gc>> {
        let mut resolved_initargs = Vec::with_capacity(initargs.len());
        for &(keyword, value) in initargs {
            let slot = class
                .slot_for_init_keyword(keyword)
                .ok_or(SlotInitError::UnknownKeyword(keyword))?;
            if !slot.flags().initializable() {
                return Err(SlotInitError::NotInitializable(keyword));
            }
            resolved_initargs.push((slot.index(), value));
        }

        let instance = Self::allocate(ctx, class);
        let mut explicit_slots = Vec::with_capacity(resolved_initargs.len());
        for &(slot_index, value) in &resolved_initargs {
            Self::set_slot(ctx, instance, slot_index, value);
            explicit_slots.push(slot_index);
        }

        for slot in class.slots() {
            if slot.init_thunk().is_empty() || explicit_slots.contains(&slot.index()) {
                continue;
            }

            match crate::runtime::vm::call_scheme(ctx, slot.init_thunk(), []) {
                crate::runtime::vm::VMResult::Ok(value) => {
                    Self::set_slot(ctx, instance, slot.index(), value);
                }
                crate::runtime::vm::VMResult::Err(error) => {
                    return Err(SlotInitError::InitThunkFailed(error));
                }
            }
        }

        Ok(instance)
    }

    pub fn class(&self) -> Gc<'gc, ClassDescriptor<'gc>> {
        self.class
    }

    fn migrate_to_class(
        ctx: Context<'gc>,
        instance: Gc<'gc, Self>,
        old_class: Gc<'gc, ClassDescriptor<'gc>>,
        new_class: Gc<'gc, ClassDescriptor<'gc>>,
    ) {
        let migrated_slots = Array::with(*ctx, new_class.slot_count(), |_, index| {
            let slot = new_class.slots()[index];
            let value = old_class
                .accessor_named(slot.name())
                .and_then(|old_accessor| instance.slot(old_accessor.slot_index()))
                .unwrap_or_else(|| slot.init_value());
            Lock::new(value)
        });

        Gc::write(*ctx, instance);
        instance.as_gcobj().header().set_class_id(new_class.id());
        unsafe {
            // SAFETY: the write barrier for `instance` has been triggered above.
            // The replacement fields are freshly allocated or rooted GC values
            // with the same lifetime as the instance.
            let instance_mut = instance.as_gcobj().to_address().as_mut_ref::<Self>();
            instance_mut.class = new_class;
            instance_mut.slots = migrated_slots;
        }
    }

    pub fn change_class(
        ctx: Context<'gc>,
        instance: Gc<'gc, Self>,
        new_class: Gc<'gc, ClassDescriptor<'gc>>,
    ) {
        debug_assert_eq!(new_class.category(), ClassCategory::Scheme);
        let old_class = instance.class;
        Self::migrate_to_class(ctx, instance, old_class, new_class);
    }

    pub fn touch_in_table(
        ctx: Context<'gc>,
        table: &ClassTable<'gc>,
        instance: Gc<'gc, Self>,
    ) -> bool {
        let old_class = instance.class;
        let Some(current_class) = table.lookup(old_class.id()) else {
            return false;
        };
        if current_class.as_gcobj() == old_class.as_gcobj() {
            return false;
        }

        Self::migrate_to_class(ctx, instance, old_class, current_class);
        true
    }

    pub fn touch(ctx: Context<'gc>, instance: Gc<'gc, Self>) -> bool {
        Self::touch_in_table(ctx, class_table(ctx), instance)
    }

    pub fn slot_count(&self) -> usize {
        self.slots.len()
    }

    pub fn slot(&self, index: usize) -> Option<Value<'gc>> {
        self.slots.as_slice().get(index).map(Lock::get)
    }

    pub fn slot_is_bound(&self, index: usize) -> bool {
        self.slot(index).is_some_and(|value| !value.is_empty())
    }

    pub fn read_slot(&self, index: usize) -> Result<Value<'gc>, SlotAccessError> {
        match self.slot(index) {
            Some(value) if !value.is_empty() => Ok(value),
            Some(_) => Err(SlotAccessError::Unbound),
            None => Err(SlotAccessError::OutOfBounds),
        }
    }

    pub fn set_slot(
        ctx: Context<'gc>,
        instance: Gc<'gc, SchemeInstance<'gc>>,
        index: usize,
        value: Value<'gc>,
    ) {
        assert!(
            index < instance.slots.len(),
            "scheme instance slot out of bounds"
        );
        Gc::write(*ctx, instance.slots)[index].unlock().set(value);
    }

    pub fn slot_by_accessor(
        &self,
        accessor: SlotAccessorDescriptor<'gc>,
    ) -> Result<Value<'gc>, SlotAccessError> {
        self.read_slot(accessor.slot_index())
    }

    pub fn slot_by_name(
        &self,
        name: &str,
        fallback: Option<Value<'gc>>,
    ) -> Result<Value<'gc>, SlotAccessError> {
        match self.class.accessor_named(name) {
            Some(accessor) => self.slot_by_accessor(accessor),
            None => fallback.ok_or(SlotAccessError::Unknown),
        }
    }

    pub fn slot_bound_by_accessor(
        &self,
        accessor: SlotAccessorDescriptor<'gc>,
    ) -> Result<bool, SlotAccessError> {
        if accessor.slot_index() >= self.slots.len() {
            return Err(SlotAccessError::OutOfBounds);
        }
        Ok(self.slot_is_bound(accessor.slot_index()))
    }

    pub fn slot_bound_by_name(&self, name: &str) -> Result<bool, SlotAccessError> {
        match self.class.accessor_named(name) {
            Some(accessor) => self.slot_bound_by_accessor(accessor),
            None => Err(SlotAccessError::Unknown),
        }
    }

    pub fn set_slot_by_accessor(
        ctx: Context<'gc>,
        instance: Gc<'gc, SchemeInstance<'gc>>,
        accessor: SlotAccessorDescriptor<'gc>,
        value: Value<'gc>,
    ) -> Result<(), SlotAccessError> {
        if !accessor.flags().settable() {
            return Err(SlotAccessError::Immutable);
        }
        if accessor.slot_index() >= instance.slots.len() {
            return Err(SlotAccessError::OutOfBounds);
        }
        Self::set_slot(ctx, instance, accessor.slot_index(), value);
        Ok(())
    }
}

impl<'gc> From<Gc<'gc, SchemeInstance<'gc>>> for Value<'gc> {
    fn from(instance: Gc<'gc, SchemeInstance<'gc>>) -> Self {
        Value::from_raw(instance.as_gcobj().to_address().as_usize() as u64)
    }
}

unsafe impl<'gc> ClassTagged for ClassDescriptor<'gc> {
    const TYPE_NAME: &'static str = "class";
    const CLASS_IDS: &'static [u32] = &[builtin_class_ids::CLASS];
}

pub fn try_scheme_instance<'gc>(
    ctx: Context<'gc>,
    value: Value<'gc>,
) -> Option<Gc<'gc, SchemeInstance<'gc>>> {
    let class = value.class(ctx)?;
    if class.category() != ClassCategory::Scheme {
        return None;
    }

    // SAFETY: Scheme instance objects are the only heap objects allocated with
    // dynamic Scheme class IDs. The class-category check above establishes the
    // payload layout before producing the typed GC handle.
    Some(unsafe { Gc::from_gcobj(value.as_cell_raw()) })
}

pub fn print_primitive_value(
    fmt: &mut fmt::Formatter<'_>,
    value: Value<'_>,
) -> Option<fmt::Result> {
    let class = scheme_instance_class_for_primitive_value(value)?;

    // SAFETY: Scheme class IDs are only assigned to SchemeInstance payloads.
    let instance: Gc<'_, SchemeInstance<'_>> = unsafe { Gc::from_gcobj(value.as_cell_raw()) };
    debug_assert_eq!(instance.class().id(), class.id());
    class
        .primitive_operation_hooks()
        .print()
        .map(|print| print(fmt, class, value))
}

pub fn compare_primitive_values(lhs: Value<'_>, rhs: Value<'_>) -> Option<bool> {
    let class = scheme_instance_class_for_primitive_value(lhs)?;

    // SAFETY: Scheme class IDs are only assigned to SchemeInstance payloads.
    let instance: Gc<'_, SchemeInstance<'_>> = unsafe { Gc::from_gcobj(lhs.as_cell_raw()) };
    debug_assert_eq!(instance.class().id(), class.id());
    class
        .primitive_operation_hooks()
        .compare()
        .and_then(|compare| compare(class, lhs, rhs))
}

pub fn hash_primitive_value(value: Value<'_>) -> Option<u64> {
    let class = scheme_instance_class_for_primitive_value(value)?;

    // SAFETY: Scheme class IDs are only assigned to SchemeInstance payloads.
    let instance: Gc<'_, SchemeInstance<'_>> = unsafe { Gc::from_gcobj(value.as_cell_raw()) };
    debug_assert_eq!(instance.class().id(), class.id());
    class
        .primitive_operation_hooks()
        .hash()
        .map(|hash| hash(class, value))
}

fn scheme_instance_class_for_primitive_value<'gc>(
    value: Value<'gc>,
) -> Option<Gc<'gc, ClassDescriptor<'gc>>> {
    if !value.is_cell() {
        return None;
    }

    let class_id = value.as_cell_raw().class_id();
    let table = CLASS_TABLE.get()?;
    // SAFETY: This operation-dispatch helper only looks up a rooted class
    // descriptor and returns its GC handle for immediate hook invocation. It
    // does not mutate GC-managed fields or retain borrowed table references.
    let class = unsafe { table.fetch_unchecked() }.lookup(class_id)?;
    (class.category() == ClassCategory::Scheme).then_some(class)
}

impl<'gc> ClassDescriptor<'gc> {
    pub fn new(ctx: Context<'gc>, id: ClassId, name: &str, category: ClassCategory) -> Self {
        Self::with_hierarchy(ctx, id, name, category, &[], &[id])
    }

    pub fn with_hierarchy(
        ctx: Context<'gc>,
        id: ClassId,
        name: &str,
        category: ClassCategory,
        direct_supers: &[ClassId],
        cpl: &[ClassId],
    ) -> Self {
        Self::with_primitive_layout_hooks(ctx, id, name, category, None, direct_supers, cpl)
    }

    pub fn with_primitive_layout_hooks(
        ctx: Context<'gc>,
        id: ClassId,
        name: &str,
        category: ClassCategory,
        primitive_layout_hooks: Option<PrimitiveLayoutHooks>,
        direct_supers: &[ClassId],
        cpl: &[ClassId],
    ) -> Self {
        Self::from_init(
            ctx,
            ClassDescriptorInit {
                id,
                name,
                category,
                primitive_layout_hooks,
                primitive_operation_hooks: None,
                flags: None,
                direct_supers,
                cpl,
                inherited_slots: &[],
                direct_slot_specs: &[],
                direct_subclasses: None,
                direct_methods: None,
            },
        )
    }

    fn from_init(ctx: Context<'gc>, init: ClassDescriptorInit<'_, 'gc>) -> Self {
        let inherited_len = init.inherited_slots.len();
        let primitive_layout_hooks = init
            .primitive_layout_hooks
            .or_else(|| default_primitive_layout_hooks(init.category));
        let primitive_operation_hooks = init
            .primitive_operation_hooks
            .unwrap_or_else(|| default_primitive_operation_hooks(init.category));
        let direct_slots = Array::with(*ctx, init.direct_slot_specs.len(), |ctx, index| {
            SlotDescriptor::from_spec(
                ctx.into(),
                init.id,
                inherited_len + index,
                init.direct_slot_specs[index],
            )
        });
        let slots = Array::with(
            *ctx,
            inherited_len + init.direct_slot_specs.len(),
            |ctx, index| {
                if index < inherited_len {
                    init.inherited_slots[index]
                } else {
                    SlotDescriptor::from_spec(
                        ctx.into(),
                        init.id,
                        index,
                        init.direct_slot_specs[index - inherited_len],
                    )
                }
            },
        );
        let accessors = Array::with(*ctx, slots.len(), |ctx, index| {
            SlotAccessorDescriptor::from_slot(ctx.into(), slots[index])
        });
        let class_slots = Array::with(*ctx, slots.len(), |_, index| {
            let slot = slots[index];
            if slot.class_allocated() {
                Lock::new(slot.init_value())
            } else {
                Lock::new(Value::empty())
            }
        });
        let initargs = initarg_metadata(ctx, slots.as_slice());

        Self {
            id: init.id,
            name: Array::from_slice(*ctx, init.name.as_bytes()),
            category: init.category,
            flags: Lock::new(init.flags.unwrap_or_else(|| init.category.default_flags())),
            primitive_layout_hooks,
            primitive_operation_hooks,
            direct_supers: Array::from_slice(*ctx, init.direct_supers),
            cpl: Array::from_slice(*ctx, init.cpl),
            direct_slots,
            slots,
            accessors,
            class_slots,
            direct_subclasses: Lock::new(
                init.direct_subclasses
                    .unwrap_or_else(|| empty_weak_subclasses(ctx)),
            ),
            direct_methods: Lock::new(init.direct_methods.unwrap_or_else(|| empty_metadata(ctx))),
            initargs,
        }
    }

    pub fn id(&self) -> ClassId {
        self.id
    }

    pub fn name(&self) -> &str {
        // Class names are copied from valid Rust strings when the descriptor is built.
        unsafe { std::str::from_utf8_unchecked(self.name.as_slice()) }
    }

    pub fn category(&self) -> ClassCategory {
        self.category
    }

    pub fn flags(&self) -> ClassFlags {
        self.flags.get()
    }

    pub fn seal(&self) {
        let flags = self.flags().with_sealed().without_malleable();
        // ClassFlags is a copyable scalar with no GC references, so this does
        // not need a write barrier even though the descriptor is a GC object.
        unsafe { self.flags.as_cell().set(flags) };
    }

    pub fn unseal(&self) -> bool {
        let flags = if self.category == ClassCategory::Scheme {
            self.flags().without_sealed().with_malleable()
        } else {
            return false;
        };
        // ClassFlags is a copyable scalar with no GC references, so this does
        // not need a write barrier even though the descriptor is a GC object.
        unsafe { self.flags.as_cell().set(flags) };
        true
    }

    pub fn primitive_layout_hooks(&self) -> Option<PrimitiveLayoutHooks> {
        self.primitive_layout_hooks
    }

    pub fn primitive_operation_hooks(&self) -> PrimitiveOperationHooks {
        self.primitive_operation_hooks
    }

    pub fn allocate_instance(
        &self,
        ctx: Context<'gc>,
        class: Gc<'gc, ClassDescriptor<'gc>>,
        initargs: &[(Value<'gc>, Value<'gc>)],
    ) -> Option<Result<Value<'gc>, SlotInitError<'gc>>> {
        self.primitive_operation_hooks
            .allocate()
            .map(|allocate| allocate(ctx, class, initargs))
    }

    pub fn slot_value_by_name(
        &self,
        ctx: Context<'gc>,
        class: Gc<'gc, ClassDescriptor<'gc>>,
        receiver: Value<'gc>,
        name: &str,
        fallback: Option<Value<'gc>>,
    ) -> Option<Result<Value<'gc>, SlotAccessError>> {
        let accessor = match self.accessor_named(name) {
            Some(accessor) => accessor,
            None => return Some(fallback.ok_or(SlotAccessError::Unknown)),
        };
        if !accessor.scheme_slot_ref().is_empty() {
            return Some(call_scheme_slot_ref(
                ctx,
                receiver,
                accessor.scheme_slot_ref(),
            ));
        }
        accessor
            .slot_ref()
            .or_else(|| self.primitive_operation_hooks.slot_ref())
            .map(|slot_ref| slot_ref(ctx, class, receiver, accessor))
    }

    pub fn set_slot_value_by_name(
        &self,
        ctx: Context<'gc>,
        class: Gc<'gc, ClassDescriptor<'gc>>,
        receiver: Value<'gc>,
        name: &str,
        value: Value<'gc>,
    ) -> Option<Result<(), SlotAccessError>> {
        let accessor = match self.accessor_named(name) {
            Some(accessor) => accessor,
            None => return Some(Err(SlotAccessError::Unknown)),
        };
        if !accessor.scheme_slot_set().is_empty() {
            return Some(call_scheme_slot_set(
                ctx,
                receiver,
                value,
                accessor.scheme_slot_set(),
            ));
        }
        accessor
            .slot_set()
            .or_else(|| self.primitive_operation_hooks.slot_set())
            .map(|slot_set| slot_set(ctx, class, receiver, accessor, value))
    }

    pub fn slot_bound_by_name(
        &self,
        ctx: Context<'gc>,
        class: Gc<'gc, ClassDescriptor<'gc>>,
        receiver: Value<'gc>,
        name: &str,
    ) -> Option<Result<bool, SlotAccessError>> {
        let accessor = match self.accessor_named(name) {
            Some(accessor) => accessor,
            None => return Some(Err(SlotAccessError::Unknown)),
        };
        if !accessor.scheme_slot_bound().is_empty() {
            return Some(call_scheme_slot_bound(
                ctx,
                receiver,
                accessor.scheme_slot_bound(),
            ));
        }
        accessor
            .slot_bound()
            .or_else(|| self.primitive_operation_hooks.slot_bound())
            .map(|slot_bound| slot_bound(ctx, class, receiver, accessor))
    }

    pub fn direct_supers(&self) -> &[ClassId] {
        self.direct_supers.as_slice()
    }

    pub fn cpl(&self) -> &[ClassId] {
        self.cpl.as_slice()
    }

    pub fn direct_slots(&self) -> &[SlotDescriptor<'gc>] {
        self.direct_slots.as_slice()
    }

    pub fn slots(&self) -> &[SlotDescriptor<'gc>] {
        self.slots.as_slice()
    }

    pub fn accessors(&self) -> &[SlotAccessorDescriptor<'gc>] {
        self.accessors.as_slice()
    }

    pub fn slot_count(&self) -> usize {
        self.slots.len()
    }

    pub fn accessor_named(&self, name: &str) -> Option<SlotAccessorDescriptor<'gc>> {
        self.accessors()
            .iter()
            .copied()
            .find(|accessor| accessor.name() == name)
    }

    pub fn slot_for_init_keyword(&self, keyword: Value<'gc>) -> Option<SlotDescriptor<'gc>> {
        if keyword.is_empty() {
            return None;
        }

        self.slots()
            .iter()
            .copied()
            .find(|slot| !slot.class_allocated() && slot.init_keyword() == keyword)
    }

    pub fn direct_subclasses(&self) -> ArrayRef<'gc, Weak<'gc, ClassDescriptor<'gc>>> {
        self.direct_subclasses.get()
    }

    pub fn direct_methods(&self) -> ArrayRef<'gc, Value<'gc>> {
        self.direct_methods.get()
    }

    pub fn initargs(&self) -> &[Value<'gc>] {
        self.initargs.as_slice()
    }

    pub fn class_slot(&self, index: usize) -> Option<Value<'gc>> {
        self.class_slots.as_slice().get(index).map(Lock::get)
    }

    pub fn class_slot_is_bound(&self, index: usize) -> bool {
        self.class_slot(index)
            .is_some_and(|value| !value.is_empty())
    }

    pub fn read_class_slot(
        &self,
        accessor: SlotAccessorDescriptor<'gc>,
    ) -> Result<Value<'gc>, SlotAccessError> {
        if !accessor.class_allocated() {
            return Err(SlotAccessError::OutOfBounds);
        }
        match self.class_slot(accessor.slot_index()) {
            Some(value) if !value.is_empty() => Ok(value),
            Some(_) => Err(SlotAccessError::Unbound),
            None => Err(SlotAccessError::OutOfBounds),
        }
    }

    pub fn set_class_slot(
        ctx: Context<'gc>,
        class: Gc<'gc, ClassDescriptor<'gc>>,
        accessor: SlotAccessorDescriptor<'gc>,
        value: Value<'gc>,
    ) -> Result<(), SlotAccessError> {
        if !accessor.class_allocated() || accessor.slot_index() >= class.class_slots.len() {
            return Err(SlotAccessError::OutOfBounds);
        }
        if accessor.flags().immutable_flag() || !accessor.flags().settable() {
            return Err(SlotAccessError::Immutable);
        }
        Gc::write(*ctx, class.class_slots)[accessor.slot_index()]
            .unlock()
            .set(value);
        Ok(())
    }

    pub fn class_slot_bound(
        &self,
        accessor: SlotAccessorDescriptor<'gc>,
    ) -> Result<bool, SlotAccessError> {
        if !accessor.class_allocated() || accessor.slot_index() >= self.class_slots.len() {
            return Err(SlotAccessError::OutOfBounds);
        }
        Ok(self.class_slot_is_bound(accessor.slot_index()))
    }

    pub fn is_a(&self, id: ClassId) -> bool {
        self.cpl.as_slice().contains(&id)
    }

    fn add_direct_subclass(
        ctx: Context<'gc>,
        class: Gc<'gc, ClassDescriptor<'gc>>,
        subclass: Gc<'gc, ClassDescriptor<'gc>>,
    ) {
        let current = class.direct_subclasses.get();
        if current.iter().any(|weak| {
            weak.upgrade(*ctx)
                .is_some_and(|existing| existing.id() == subclass.id())
        }) {
            return;
        }

        let appended = Array::with(*ctx, current.len() + 1, |_, index| {
            if index < current.len() {
                current[index]
            } else {
                Gc::downgrade(subclass)
            }
        });
        barrier::field!(Gc::write(*ctx, class), ClassDescriptor, direct_subclasses)
            .unlock()
            .set(appended);
    }

    fn remove_direct_subclass(
        ctx: Context<'gc>,
        class: Gc<'gc, ClassDescriptor<'gc>>,
        subclass_id: ClassId,
    ) {
        let current = class.direct_subclasses.get();
        let filtered = current
            .iter()
            .copied()
            .filter(|weak| {
                weak.upgrade(*ctx)
                    .is_none_or(|existing| existing.id() != subclass_id)
            })
            .collect::<Vec<_>>();
        if filtered.len() == current.len() {
            return;
        }

        let filtered = Array::from_slice(*ctx, &filtered);
        barrier::field!(Gc::write(*ctx, class), ClassDescriptor, direct_subclasses)
            .unlock()
            .set(filtered);
    }

    fn add_direct_method(
        ctx: Context<'gc>,
        class: Gc<'gc, ClassDescriptor<'gc>>,
        method: Value<'gc>,
    ) {
        let current = class.direct_methods.get();
        if current.contains(&method) {
            return;
        }

        let appended = Array::with(*ctx, current.len() + 1, |_, index| {
            if index < current.len() {
                current[index]
            } else {
                method
            }
        });
        barrier::field!(Gc::write(*ctx, class), ClassDescriptor, direct_methods)
            .unlock()
            .set(appended);
    }

    fn invalidate_direct_method_caches(ctx: Context<'gc>, class: Gc<'gc, ClassDescriptor<'gc>>) {
        for method in class.direct_methods().iter().copied() {
            if let Some(method) = method.try_as::<MethodDescriptor>() {
                GenericDescriptor::clear_dispatcher_cache(ctx, method.generic());
            }
        }
    }
}

fn empty_metadata<'gc>(ctx: Context<'gc>) -> ArrayRef<'gc, Value<'gc>> {
    Array::with(*ctx, 0, |_, _| {
        unreachable!("zero-length metadata arrays are never initialized")
    })
}

fn initarg_metadata<'gc>(
    ctx: Context<'gc>,
    slots: &[SlotDescriptor<'gc>],
) -> ArrayRef<'gc, Value<'gc>> {
    let initargs = slots
        .iter()
        .filter_map(|slot| {
            let keyword = slot.init_keyword();
            (!slot.class_allocated() && !keyword.is_empty() && slot.flags().initializable())
                .then_some(keyword)
        })
        .collect::<Vec<_>>();
    Array::from_slice(*ctx, &initargs)
}

fn class_id_list_to_class_objects<'gc>(ctx: Context<'gc>, ids: &[ClassId]) -> Value<'gc> {
    let classes = ids
        .iter()
        .filter_map(|id| class_table(ctx).lookup(*id).map(Value::from))
        .collect::<Vec<_>>();
    Value::list_from_slice(ctx, &classes)
}

fn class_id_to_class_object<'gc>(ctx: Context<'gc>, id: ClassId) -> Value<'gc> {
    class_table(ctx)
        .lookup(id)
        .map(Value::from)
        .unwrap_or_else(|| Value::new(false))
}

fn slot_definition_list<'gc>(ctx: Context<'gc>, slots: &[SlotDescriptor<'gc>]) -> Value<'gc> {
    let definitions = slots
        .iter()
        .map(|slot| SlotDefinitionDescriptor::from_slot(ctx, *slot).into())
        .collect::<Vec<_>>();
    Value::list_from_slice(ctx, &definitions)
}

fn slot_accessor_list<'gc>(
    ctx: Context<'gc>,
    accessors: &[SlotAccessorDescriptor<'gc>],
) -> Value<'gc> {
    let definitions = accessors
        .iter()
        .map(|accessor| SlotAccessorDefinition::from_accessor(ctx, *accessor).into())
        .collect::<Vec<_>>();
    Value::list_from_slice(ctx, &definitions)
}

fn empty_method_metadata<'gc>(ctx: Context<'gc>) -> ArrayRef<'gc, Gc<'gc, MethodDescriptor<'gc>>> {
    Array::with(*ctx, 0, |_, _| {
        unreachable!("zero-length method arrays are never initialized")
    })
}

fn empty_dispatcher_cache<'gc>(
    ctx: Context<'gc>,
) -> ArrayRef<'gc, GenericDispatcherCacheEntry<'gc>> {
    Array::with(*ctx, 0, |_, _| {
        unreachable!("zero-length dispatcher cache arrays are never initialized")
    })
}

fn empty_weak_subclasses<'gc>(ctx: Context<'gc>) -> ArrayRef<'gc, Weak<'gc, ClassDescriptor<'gc>>> {
    Array::with(*ctx, 0, |_, _| {
        unreachable!("zero-length weak subclass arrays are never initialized")
    })
}

fn default_primitive_layout_hooks(category: ClassCategory) -> Option<PrimitiveLayoutHooks> {
    match category {
        ClassCategory::Scheme => Some(PrimitiveLayoutHooks::from_allocation_hooks(
            AllocationHooksOf::<'static, SchemeInstance<'static>>::HOOKS,
        )),
        ClassCategory::Abstract
        | ClassCategory::Builtin
        | ClassCategory::Immediate
        | ClassCategory::Internal => None,
    }
}

fn default_primitive_operation_hooks(category: ClassCategory) -> PrimitiveOperationHooks {
    match category {
        ClassCategory::Scheme => PrimitiveOperationHooks::new(
            Some(scheme_instance_allocate_hook),
            Some(scheme_instance_print_hook),
            Some(scheme_instance_compare_hook),
            Some(scheme_instance_hash_hook),
            Some(scheme_instance_slot_ref_hook),
            Some(scheme_instance_slot_set_hook),
            Some(scheme_instance_slot_bound_hook),
        ),
        ClassCategory::Abstract
        | ClassCategory::Builtin
        | ClassCategory::Immediate
        | ClassCategory::Internal => PrimitiveOperationHooks::default(),
    }
}

fn call_scheme_slot_ref<'gc>(
    ctx: Context<'gc>,
    receiver: Value<'gc>,
    proc: Value<'gc>,
) -> Result<Value<'gc>, SlotAccessError> {
    match crate::runtime::vm::call_scheme(ctx, proc, [receiver]) {
        crate::runtime::vm::VMResult::Ok(value) => Ok(value),
        crate::runtime::vm::VMResult::Err(_) => Err(SlotAccessError::SchemeHookFailed),
    }
}

fn call_scheme_slot_set<'gc>(
    ctx: Context<'gc>,
    receiver: Value<'gc>,
    value: Value<'gc>,
    proc: Value<'gc>,
) -> Result<(), SlotAccessError> {
    match crate::runtime::vm::call_scheme(ctx, proc, [receiver, value]) {
        crate::runtime::vm::VMResult::Ok(_) => Ok(()),
        crate::runtime::vm::VMResult::Err(_) => Err(SlotAccessError::SchemeHookFailed),
    }
}

fn call_scheme_slot_bound<'gc>(
    ctx: Context<'gc>,
    receiver: Value<'gc>,
    proc: Value<'gc>,
) -> Result<bool, SlotAccessError> {
    match crate::runtime::vm::call_scheme(ctx, proc, [receiver]) {
        crate::runtime::vm::VMResult::Ok(value) => Ok(value != Value::new(false)),
        crate::runtime::vm::VMResult::Err(_) => Err(SlotAccessError::SchemeHookFailed),
    }
}

fn scheme_instance_allocate_hook<'gc>(
    ctx: Context<'gc>,
    class: Gc<'gc, ClassDescriptor<'gc>>,
    initargs: &[(Value<'gc>, Value<'gc>)],
) -> Result<Value<'gc>, SlotInitError<'gc>> {
    SchemeInstance::allocate_with_initargs(ctx, class, initargs).map(Value::from)
}

fn invocable_scheme_instance_allocate_hook<'gc>(
    ctx: Context<'gc>,
    class: Gc<'gc, ClassDescriptor<'gc>>,
    initargs: &[(Value<'gc>, Value<'gc>)],
) -> Result<Value<'gc>, SlotInitError<'gc>> {
    let instance = SchemeInstance::allocate_with_initargs(ctx, class, initargs)?;
    let Some(invoker) = class.accessor_named("procedure") else {
        return Err(SlotInitError::MissingInvokerSlot(class.into()));
    };
    let target = match instance.slot_by_accessor(invoker) {
        Ok(target) if target.is::<Closure>() => target,
        Ok(target) => return Err(SlotInitError::InvokerNotProcedure(target)),
        Err(_) => return Err(SlotInitError::InvokerNotProcedure(invoker.init_keyword())),
    };

    Ok(invocable_instance_procedure(ctx, class, instance, target).into())
}

extern "C-unwind" fn invocable_instance_closure_proc<'gc>(
    ctx: Context<'gc>,
    rator: Value<'gc>,
    rands: *const Value<'gc>,
    num_rands: usize,
    retk: Value<'gc>,
) -> NativeReturn<'gc> {
    if !rator.is::<Closure>() {
        return generic_assertion_violation(
            ctx,
            retk,
            "invocable-instance",
            "invocable wrapper is not a closure",
            rator,
        );
    }

    let closure = rator.downcast::<Closure>();
    if closure.nfree <= 2 {
        return generic_assertion_violation(
            ctx,
            retk,
            "invocable-instance",
            "invocable wrapper is missing its target",
            rator,
        );
    }

    let instance = closure[1].get();
    let target = closure[2].get();
    if !target.is::<Closure>() {
        return generic_assertion_violation(
            ctx,
            retk,
            "invocable-instance",
            "invocable target is not a procedure",
            target,
        );
    }

    // SAFETY: native procedure trampolines pass `num_rands` contiguous argument values.
    let args = unsafe { std::slice::from_raw_parts(rands, num_rands) };
    let mut forwarded = Vec::with_capacity(num_rands + 1);
    forwarded.push(instance);
    forwarded.extend_from_slice(args);
    ctx.return_call(target, forwarded, Some(retk))
}

fn invocable_instance_procedure<'gc>(
    ctx: Context<'gc>,
    class: Gc<'gc, ClassDescriptor<'gc>>,
    instance: Gc<'gc, SchemeInstance<'gc>>,
    target: Value<'gc>,
) -> Gc<'gc, Closure<'gc>> {
    let properties = generic_property_entry(
        ctx,
        ctx.intern("name"),
        ctx.intern(class.name()),
        generic_property_entry(
            ctx,
            ctx.intern("class"),
            class.into(),
            generic_property_entry(ctx, ctx.intern("instance"), instance.into(), Value::null()),
        ),
    );
    PROCEDURES.fetch(*ctx).make_closure(
        ctx,
        invocable_instance_closure_proc,
        [instance.into(), target],
        properties,
    )
}

fn scheme_instance_print_hook<'gc>(
    fmt: &mut fmt::Formatter<'_>,
    class: Gc<'gc, ClassDescriptor<'gc>>,
    value: Value<'gc>,
) -> fmt::Result {
    write!(fmt, "#<instance {} {:p}>", class.name(), value)
}

fn scheme_instance_compare_hook<'gc>(
    _class: Gc<'gc, ClassDescriptor<'gc>>,
    lhs: Value<'gc>,
    rhs: Value<'gc>,
) -> Option<bool> {
    Some(lhs == rhs)
}

fn scheme_instance_hash_hook<'gc>(_class: Gc<'gc, ClassDescriptor<'gc>>, value: Value<'gc>) -> u64 {
    value.as_cell_raw().hashcode()
}

fn scheme_instance_slot_ref_hook<'gc>(
    ctx: Context<'gc>,
    class: Gc<'gc, ClassDescriptor<'gc>>,
    receiver: Value<'gc>,
    accessor: SlotAccessorDescriptor<'gc>,
) -> Result<Value<'gc>, SlotAccessError> {
    if accessor.class_allocated() {
        return class.read_class_slot(accessor);
    }
    let Some(instance) = try_scheme_instance(ctx, receiver) else {
        return Err(SlotAccessError::OutOfBounds);
    };
    instance.slot_by_accessor(accessor)
}

fn scheme_instance_slot_set_hook<'gc>(
    ctx: Context<'gc>,
    class: Gc<'gc, ClassDescriptor<'gc>>,
    receiver: Value<'gc>,
    accessor: SlotAccessorDescriptor<'gc>,
    value: Value<'gc>,
) -> Result<(), SlotAccessError> {
    if accessor.class_allocated() {
        return ClassDescriptor::set_class_slot(ctx, class, accessor, value);
    }
    let Some(instance) = try_scheme_instance(ctx, receiver) else {
        return Err(SlotAccessError::OutOfBounds);
    };
    SchemeInstance::set_slot_by_accessor(ctx, instance, accessor, value)
}

fn scheme_instance_slot_bound_hook<'gc>(
    ctx: Context<'gc>,
    class: Gc<'gc, ClassDescriptor<'gc>>,
    receiver: Value<'gc>,
    accessor: SlotAccessorDescriptor<'gc>,
) -> Result<bool, SlotAccessError> {
    if accessor.class_allocated() {
        return class.class_slot_bound(accessor);
    }
    let Some(instance) = try_scheme_instance(ctx, receiver) else {
        return Err(SlotAccessError::OutOfBounds);
    };
    instance.slot_bound_by_accessor(accessor)
}

#[derive(Clone, Copy)]
pub struct BuiltinClassSpec {
    id: ClassId,
    name: &'static str,
    category: ClassCategory,
}

impl BuiltinClassSpec {
    pub fn id(self) -> ClassId {
        self.id
    }

    pub fn name(self) -> &'static str {
        self.name
    }

    pub fn category(self) -> ClassCategory {
        self.category
    }

    fn descriptor<'gc>(self, ctx: Context<'gc>) -> ClassDescriptor<'gc> {
        let (direct_supers, direct_supers_len) = builtin_direct_supers(self.id);
        let (cpl, cpl_len) = builtin_cpl(self.id);
        ClassDescriptor::with_primitive_layout_hooks(
            ctx,
            self.id,
            self.name,
            self.category,
            builtin_primitive_layout_hooks(self.id),
            &direct_supers[..direct_supers_len],
            &cpl[..cpl_len],
        )
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum ClassTableError {
    DuplicateId(ClassId),
    Exhausted,
    IdOutOfRange,
    MissingId(ClassId),
    NotMalleable(ClassId),
}

#[derive(Debug, PartialEq, Eq)]
pub enum SchemeClassSpecError<'gc> {
    SlotsNotList(Value<'gc>),
    SlotNameNotSymbol(Value<'gc>),
    SlotSpecNotList(Value<'gc>),
    SlotOptionNameNotKeyword(Value<'gc>),
    SlotOptionMissingValue(Value<'gc>),
    UnknownSlotOption(Value<'gc>),
    SupersNotList(Value<'gc>),
    SuperNotClass(Value<'gc>),
    MissingProcedureSlot(Value<'gc>),
    ClassTable(ClassTableError),
}

struct ParsedSlotSpec<'gc> {
    name: String,
    init_value: Value<'gc>,
    init_keyword: Value<'gc>,
    init_thunk: Value<'gc>,
    scheme_slot_ref: Value<'gc>,
    scheme_slot_set: Value<'gc>,
    scheme_slot_bound: Value<'gc>,
    getter: Value<'gc>,
    setter: Value<'gc>,
    accessor: Value<'gc>,
    immutable: bool,
    initializable: bool,
    settable: bool,
    class_allocated: bool,
}

impl<'gc> ParsedSlotSpec<'gc> {
    fn mutable(ctx: Context<'gc>, name: String) -> Self {
        let keyword = crate::runtime::value::Symbol::from_str(ctx, &name).into();
        Self {
            name,
            init_value: Value::empty(),
            init_keyword: keyword,
            init_thunk: Value::empty(),
            scheme_slot_ref: Value::empty(),
            scheme_slot_set: Value::empty(),
            scheme_slot_bound: Value::empty(),
            getter: Value::new(false),
            setter: Value::new(false),
            accessor: Value::new(false),
            immutable: false,
            initializable: true,
            settable: true,
            class_allocated: false,
        }
    }

    fn as_slot_spec(&self) -> SlotSpec<'_, 'gc> {
        let mut spec = if self.immutable {
            SlotSpec::immutable(self.name.as_str())
        } else {
            SlotSpec::mutable(self.name.as_str())
        }
        .with_init_value(self.init_value)
        .with_init_keyword(self.init_keyword)
        .with_init_thunk(self.init_thunk);

        if !self.initializable {
            spec = spec.without_initargs();
        }
        if !self.settable {
            spec = spec.without_setter();
        }
        if self.class_allocated {
            spec = spec.with_class_allocation();
        }
        spec = spec.with_scheme_slot_procedures(
            self.scheme_slot_ref,
            self.scheme_slot_set,
            self.scheme_slot_bound,
        );
        spec = spec.with_accessor_names(self.getter, self.setter, self.accessor);
        spec
    }
}

#[derive(Debug, PartialEq, Eq)]
enum ClassListError<'gc> {
    NotList(Value<'gc>),
    NotClass(Value<'gc>),
}

#[derive(Clone, Copy, Default)]
enum ClassSlot<'gc> {
    #[default]
    Empty,
    Class(Gc<'gc, ClassDescriptor<'gc>>),
}

unsafe impl Trace for ClassSlot<'_> {
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        if let Self::Class(class) = self {
            unsafe { class.trace(visitor) };
        }
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::rsgc::WeakProcessor) {
        if let Self::Class(class) = self {
            unsafe { class.process_weak_refs(weak_processor) };
        }
    }
}

#[derive(Clone, Copy)]
struct ClassPage<'gc> {
    slots: ArrayRef<'gc, Lock<ClassSlot<'gc>>>,
}

impl<'gc> ClassPage<'gc> {
    fn new(ctx: Context<'gc>) -> Self {
        Self {
            slots: Array::with(*ctx, CLASS_TABLE_PAGE_SIZE, |_, _| {
                Lock::new(ClassSlot::Empty)
            }),
        }
    }
}

unsafe impl Trace for ClassPage<'_> {
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        unsafe { self.slots.trace(visitor) };
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::rsgc::WeakProcessor) {
        unsafe { self.slots.process_weak_refs(weak_processor) };
    }
}

struct ClassTableInner<'gc> {
    pages: ArrayRef<'gc, Lock<Option<ClassPage<'gc>>>>,
    max_id: u32,
}

#[derive(Default)]
struct ClassRedefinitionState {
    epoch: u64,
}

unsafe impl Trace for ClassTableInner<'_> {
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        unsafe { self.pages.trace(visitor) };
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::rsgc::WeakProcessor) {
        unsafe { self.pages.process_weak_refs(weak_processor) };
    }
}

pub struct ClassTable<'gc> {
    inner: Monitor<ClassTableInner<'gc>>,
    redefinition: Monitor<ClassRedefinitionState>,
}

struct DynamicClassOptions {
    primitive_operation_hooks: Option<PrimitiveOperationHooks>,
    flags: Option<ClassFlags>,
}

struct DynamicClassRedefinition<'a> {
    id: ClassId,
    name: &'a str,
    category: ClassCategory,
}

fn invocable_scheme_class_options() -> DynamicClassOptions {
    DynamicClassOptions {
        primitive_operation_hooks: Some(PrimitiveOperationHooks::new(
            Some(invocable_scheme_instance_allocate_hook),
            Some(scheme_instance_print_hook),
            Some(scheme_instance_compare_hook),
            Some(scheme_instance_hash_hook),
            Some(scheme_instance_slot_ref_hook),
            Some(scheme_instance_slot_set_hook),
            Some(scheme_instance_slot_bound_hook),
        )),
        flags: Some(ClassFlags::scheme_applicable()),
    }
}

fn normalized_dynamic_direct_supers(
    category: ClassCategory,
    direct_supers: &[ClassId],
) -> Vec<ClassId> {
    let mut normalized = direct_supers.to_vec();
    if category == ClassCategory::Scheme {
        let object = builtin_id(builtin_class_ids::OBJECT);
        if !normalized.contains(&object) {
            normalized.push(object);
        }
    }
    normalized
}

fn push_unique_class_id(ids: &mut Vec<ClassId>, id: ClassId) {
    if !ids.contains(&id) {
        ids.push(id);
    }
}

fn collect_dynamic_hierarchy<'gc>(
    inner: &ClassTableInner<'gc>,
    id: ClassId,
    direct_supers: &[ClassId],
) -> (Vec<SlotDescriptor<'gc>>, Vec<ClassId>) {
    let mut inherited_slots = Vec::new();
    let mut cpl = vec![id];

    for super_id in direct_supers {
        if let Some(superclass) = inner.lookup(*super_id) {
            inherited_slots.extend_from_slice(superclass.slots());
            for class_id in superclass.cpl() {
                push_unique_class_id(&mut cpl, *class_id);
            }
        } else {
            push_unique_class_id(&mut cpl, *super_id);
        }
    }

    (inherited_slots, cpl)
}

unsafe impl Trace for ClassTable<'_> {
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        unsafe { self.inner.get_mut().trace(visitor) };
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::rsgc::WeakProcessor) {
        unsafe { self.inner.get_mut().process_weak_refs(weak_processor) };
    }
}

unsafe impl Send for ClassTable<'_> {}
unsafe impl Sync for ClassTable<'_> {}

type RootedClassTable = Rootable!(ClassTable<'_>);

static CLASS_TABLE: OnceLock<GcGlobal<RootedClassTable>> = OnceLock::new();

pub(crate) fn class_table_initialized() -> bool {
    CLASS_TABLE.get().is_some()
}

pub(crate) fn is_type_class_registered(id: ClassId) -> bool {
    let Some(table) = CLASS_TABLE.get() else {
        return false;
    };
    // SAFETY: The class table is a global GC root. This lookup only checks presence.
    unsafe { table.fetch_unchecked().lookup(id).is_some() }
}

pub fn init_builtin_classes<'gc>(ctx: Context<'gc>) -> &'gc ClassTable<'gc> {
    CLASS_TABLE
        .get_or_init(|| {
            let table: GcGlobal<RootedClassTable> = GcGlobal::new(ClassTable::new(ctx));
            let rooted_table = table.fetch(*ctx);
            for spec in builtin_class_specs() {
                rooted_table
                    .register_builtin(ctx, spec.descriptor(ctx))
                    .expect("built-in class IDs must be unique");
            }
            register_pending_type_classes(ctx, rooted_table);
            table
        })
        .fetch(*ctx)
}

pub fn class_table<'gc>(ctx: Context<'gc>) -> &'gc ClassTable<'gc> {
    init_builtin_classes(ctx)
}

fn class_root_binding<'gc>(ctx: Context<'gc>, name: &str) -> Option<Value<'gc>> {
    ctx.globals().root_module().get_str(ctx, name)
}

#[scheme(path=capy)]
pub mod class_ops {
    use super::{ClassListError, class_root_binding, parse_class_list};
    use crate::runtime::class::{
        ClassCategory, ClassDescriptor, GenericDescriptor, GenericDispatchError, MethodDescriptor,
        NextMethodDescriptor, SchemeClassSpecError, SchemeInstance, SlotAccessError,
        SlotAccessorDefinition, SlotDefinitionDescriptor, SlotInitError, class_id_to_class_object,
        class_table, generic_descriptor_from_value, generic_procedure, slot_accessor_list,
        slot_definition_list, try_scheme_instance,
    };
    use crate::runtime::value::{Keyword, Symbol};

    #[scheme(name = "class?")]
    pub fn class_p(value: Value<'gc>) -> bool {
        nctx.return_(value.is::<ClassDescriptor>())
    }

    #[scheme(name = "%builtin-class")]
    pub fn builtin_class(name: Gc<'gc, Symbol<'gc>>) -> Value<'gc> {
        let name_string = name.to_string();
        let Some(spec) = builtin_class_specs()
            .iter()
            .copied()
            .find(|spec| spec.name() == name_string)
        else {
            let who = nctx.ctx.intern("%builtin-class");
            let message = nctx.ctx.str("unknown built-in class");
            return nctx.raise_assertion_violation(who, message, name.into());
        };

        let Some(class) = class_table(nctx.ctx).lookup(spec.id()) else {
            let who = nctx.ctx.intern("%builtin-class");
            let message = nctx.ctx.str("built-in class is not initialized");
            return nctx.raise_assertion_violation(who, message, name.into());
        };
        nctx.return_(class.into())
    }

    #[scheme(name = "class-name")]
    pub fn class_name(class: Gc<'gc, ClassDescriptor<'gc>>) -> Value<'gc> {
        let name = nctx.ctx.str(class.name());
        nctx.return_(name)
    }

    #[scheme(name = "class-direct-supers")]
    pub fn class_direct_supers(class: Gc<'gc, ClassDescriptor<'gc>>) -> Value<'gc> {
        let supers = class_id_list_to_class_objects(nctx.ctx, class.direct_supers());
        nctx.return_(supers)
    }

    #[scheme(name = "class-precedence-list")]
    pub fn class_precedence_list(class: Gc<'gc, ClassDescriptor<'gc>>) -> Value<'gc> {
        let cpl = class_id_list_to_class_objects(nctx.ctx, class.cpl());
        nctx.return_(cpl)
    }

    #[scheme(name = "class-direct-slots")]
    pub fn class_direct_slots(class: Gc<'gc, ClassDescriptor<'gc>>) -> Value<'gc> {
        let slots = slot_definition_list(nctx.ctx, class.direct_slots());
        nctx.return_(slots)
    }

    #[scheme(name = "class-slots")]
    pub fn class_slots(class: Gc<'gc, ClassDescriptor<'gc>>) -> Value<'gc> {
        let slots = slot_definition_list(nctx.ctx, class.slots());
        nctx.return_(slots)
    }

    #[scheme(name = "class-accessors")]
    pub fn class_accessors(class: Gc<'gc, ClassDescriptor<'gc>>) -> Value<'gc> {
        let accessors = slot_accessor_list(nctx.ctx, class.accessors());
        nctx.return_(accessors)
    }

    #[scheme(name = "class-initargs")]
    pub fn class_initargs(class: Gc<'gc, ClassDescriptor<'gc>>) -> Value<'gc> {
        let initargs = Value::list_from_slice(nctx.ctx, class.initargs());
        nctx.return_(initargs)
    }

    #[scheme(name = "class-direct-methods")]
    pub fn class_direct_methods(class: Gc<'gc, ClassDescriptor<'gc>>) -> Value<'gc> {
        let methods = Value::list_from_slice(nctx.ctx, class.direct_methods().as_slice());
        nctx.return_(methods)
    }

    #[scheme(name = "class-direct-subclasses")]
    pub fn class_direct_subclasses(class: Gc<'gc, ClassDescriptor<'gc>>) -> Value<'gc> {
        let subclasses = class
            .direct_subclasses()
            .iter()
            .filter_map(|weak| weak.upgrade(*nctx.ctx).map(Value::from))
            .collect::<Vec<_>>();
        let subclasses = Value::list_from_slice(nctx.ctx, &subclasses);
        nctx.return_(subclasses)
    }

    #[scheme(name = "class-applicable?")]
    pub fn class_applicable_p(class: Gc<'gc, ClassDescriptor<'gc>>) -> bool {
        nctx.return_(class.flags().applicable())
    }

    #[scheme(name = "class-malleable?")]
    pub fn class_malleable_p(class: Gc<'gc, ClassDescriptor<'gc>>) -> bool {
        nctx.return_(class.flags().malleable())
    }

    #[scheme(name = "class-sealed?")]
    pub fn class_sealed_p(class: Gc<'gc, ClassDescriptor<'gc>>) -> bool {
        nctx.return_(class.flags().sealed())
    }

    #[scheme(name = "class-seal!")]
    pub fn class_seal(class: Gc<'gc, ClassDescriptor<'gc>>) -> Value<'gc> {
        class.seal();
        nctx.return_(class.into())
    }

    #[scheme(name = "class-unseal!")]
    pub fn class_unseal(class: Gc<'gc, ClassDescriptor<'gc>>) -> Value<'gc> {
        if class.unseal() {
            nctx.return_(class.into())
        } else {
            let who = nctx.ctx.intern("class-unseal!");
            let message = nctx.ctx.str("class cannot be unsealed");
            nctx.raise_assertion_violation(who, message, class.into())
        }
    }

    #[scheme(name = "generic?")]
    pub fn generic_p(value: Value<'gc>) -> bool {
        let result = generic_descriptor_from_value(nctx.ctx, value).is_some();
        nctx.return_(result)
    }

    #[scheme(name = "generic-name")]
    pub fn generic_name(value: Value<'gc>) -> Value<'gc> {
        let name = generic_descriptor_from_value(nctx.ctx, value)
            .map(|generic| nctx.ctx.str(generic.name()))
            .unwrap_or_else(|| Value::new(false));
        nctx.return_(name)
    }

    #[scheme(name = "generic-methods")]
    pub fn generic_methods(value: Value<'gc>) -> Value<'gc> {
        let methods = generic_descriptor_from_value(nctx.ctx, value)
            .map(|generic| {
                let methods = generic
                    .methods()
                    .iter()
                    .copied()
                    .map(Value::from)
                    .collect::<Vec<_>>();
                Value::list_from_slice(nctx.ctx, &methods)
            })
            .unwrap_or_else(|| Value::new(false));
        nctx.return_(methods)
    }

    #[scheme(name = "generic-fallback")]
    pub fn generic_fallback(value: Value<'gc>) -> Value<'gc> {
        let fallback = generic_descriptor_from_value(nctx.ctx, value)
            .map(|generic| generic.fallback())
            .unwrap_or_else(|| Value::new(false));
        nctx.return_(fallback)
    }

    #[scheme(name = "generic-required-dispatch-arg-count")]
    pub fn generic_required_dispatch_arg_count(value: Value<'gc>) -> Value<'gc> {
        let count = generic_descriptor_from_value(nctx.ctx, value)
            .map(|generic| generic.max_required_dispatch_args())
            .unwrap_or(0);
        nctx.return_(Value::new(count as i32))
    }

    #[scheme(name = "generic-sealed?")]
    pub fn generic_sealed_p(value: Value<'gc>) -> bool {
        let sealed = generic_descriptor_from_value(nctx.ctx, value)
            .map(|generic| generic.flags().sealed())
            .unwrap_or(false);
        nctx.return_(sealed)
    }

    #[scheme(name = "generic-seal!")]
    pub fn generic_seal(value: Value<'gc>) -> Value<'gc> {
        let Some(generic) = generic_descriptor_from_value(nctx.ctx, value) else {
            let who = nctx.ctx.intern("generic-seal!");
            let message = nctx.ctx.str("not a generic procedure");
            return nctx.raise_assertion_violation(who, message, value);
        };
        GenericDescriptor::set_sealed(nctx.ctx, generic, true);
        nctx.return_(Value::void())
    }

    #[scheme(name = "generic-unseal!")]
    pub fn generic_unseal(value: Value<'gc>) -> Value<'gc> {
        let Some(generic) = generic_descriptor_from_value(nctx.ctx, value) else {
            let who = nctx.ctx.intern("generic-unseal!");
            let message = nctx.ctx.str("not a generic procedure");
            return nctx.raise_assertion_violation(who, message, value);
        };
        GenericDescriptor::set_sealed(nctx.ctx, generic, false);
        nctx.return_(Value::void())
    }

    #[scheme(name = "method?")]
    pub fn method_p(value: Value<'gc>) -> bool {
        nctx.return_(value.is::<MethodDescriptor>())
    }

    #[scheme(name = "method-generic")]
    pub fn method_generic(method: Gc<'gc, MethodDescriptor<'gc>>) -> Value<'gc> {
        nctx.return_(method.generic().into())
    }

    #[scheme(name = "method-specializers")]
    pub fn method_specializers(method: Gc<'gc, MethodDescriptor<'gc>>) -> Value<'gc> {
        let specializers = class_id_list_to_class_objects(nctx.ctx, method.specializers());
        nctx.return_(specializers)
    }

    #[scheme(name = "method-required-arg-count")]
    pub fn method_required_arg_count(method: Gc<'gc, MethodDescriptor<'gc>>) -> Value<'gc> {
        nctx.return_(Value::new(method.required_arg_count() as i32))
    }

    #[scheme(name = "method-body")]
    pub fn method_body(method: Gc<'gc, MethodDescriptor<'gc>>) -> Value<'gc> {
        nctx.return_(method.body())
    }

    #[scheme(name = "method-locked?")]
    pub fn method_locked_p(method: Gc<'gc, MethodDescriptor<'gc>>) -> bool {
        nctx.return_(method.flags().locked())
    }

    #[scheme(name = "slot-definition?")]
    pub fn slot_definition_p(value: Value<'gc>) -> bool {
        nctx.return_(value.is::<SlotDefinitionDescriptor>())
    }

    #[scheme(name = "slot-definition-name")]
    pub fn slot_definition_name(slot: Gc<'gc, SlotDefinitionDescriptor<'gc>>) -> Value<'gc> {
        let name = Symbol::from_str(nctx.ctx, slot.name()).into();
        nctx.return_(name)
    }

    #[scheme(name = "slot-definition-owner")]
    pub fn slot_definition_owner(slot: Gc<'gc, SlotDefinitionDescriptor<'gc>>) -> Value<'gc> {
        let owner = class_id_to_class_object(nctx.ctx, slot.owner());
        nctx.return_(owner)
    }

    #[scheme(name = "slot-definition-index")]
    pub fn slot_definition_index(slot: Gc<'gc, SlotDefinitionDescriptor<'gc>>) -> Value<'gc> {
        nctx.return_(Value::new(slot.index() as i32))
    }

    #[scheme(name = "%slot-definition-allocation")]
    pub fn slot_definition_allocation(slot: Gc<'gc, SlotDefinitionDescriptor<'gc>>) -> Value<'gc> {
        let allocation = if slot.class_allocated() {
            nctx.ctx.keyword("class")
        } else {
            nctx.ctx.keyword("instance")
        };
        nctx.return_(allocation.into())
    }

    #[scheme(name = "slot-definition-init-keyword")]
    pub fn slot_definition_init_keyword(
        slot: Gc<'gc, SlotDefinitionDescriptor<'gc>>,
    ) -> Value<'gc> {
        nctx.return_(slot.init_keyword())
    }

    #[scheme(name = "slot-definition-init-value")]
    pub fn slot_definition_init_value(slot: Gc<'gc, SlotDefinitionDescriptor<'gc>>) -> Value<'gc> {
        nctx.return_(slot.init_value())
    }

    #[scheme(name = "slot-definition-init-thunk")]
    pub fn slot_definition_init_thunk(slot: Gc<'gc, SlotDefinitionDescriptor<'gc>>) -> Value<'gc> {
        nctx.return_(slot.init_thunk())
    }

    #[scheme(name = "slot-definition-slot-ref")]
    pub fn slot_definition_slot_ref(slot: Gc<'gc, SlotDefinitionDescriptor<'gc>>) -> Value<'gc> {
        nctx.return_(slot.scheme_slot_ref())
    }

    #[scheme(name = "slot-definition-slot-set!")]
    pub fn slot_definition_slot_set(slot: Gc<'gc, SlotDefinitionDescriptor<'gc>>) -> Value<'gc> {
        nctx.return_(slot.scheme_slot_set())
    }

    #[scheme(name = "slot-definition-slot-bound?")]
    pub fn slot_definition_slot_bound(slot: Gc<'gc, SlotDefinitionDescriptor<'gc>>) -> Value<'gc> {
        nctx.return_(slot.scheme_slot_bound())
    }

    #[scheme(name = "slot-definition-getter")]
    pub fn slot_definition_getter(slot: Gc<'gc, SlotDefinitionDescriptor<'gc>>) -> Value<'gc> {
        nctx.return_(slot.getter())
    }

    #[scheme(name = "slot-definition-setter")]
    pub fn slot_definition_setter(slot: Gc<'gc, SlotDefinitionDescriptor<'gc>>) -> Value<'gc> {
        nctx.return_(slot.setter())
    }

    #[scheme(name = "slot-definition-accessor")]
    pub fn slot_definition_accessor(slot: Gc<'gc, SlotDefinitionDescriptor<'gc>>) -> Value<'gc> {
        nctx.return_(slot.accessor())
    }

    #[scheme(name = "slot-definition-initializable?")]
    pub fn slot_definition_initializable_p(slot: Gc<'gc, SlotDefinitionDescriptor<'gc>>) -> bool {
        nctx.return_(slot.flags().initializable())
    }

    #[scheme(name = "slot-definition-settable?")]
    pub fn slot_definition_settable_p(slot: Gc<'gc, SlotDefinitionDescriptor<'gc>>) -> bool {
        nctx.return_(slot.flags().settable())
    }

    #[scheme(name = "slot-definition-immutable?")]
    pub fn slot_definition_immutable_p(slot: Gc<'gc, SlotDefinitionDescriptor<'gc>>) -> bool {
        nctx.return_(slot.flags().immutable_flag())
    }

    #[scheme(name = "slot-accessor?")]
    pub fn slot_accessor_p(value: Value<'gc>) -> bool {
        nctx.return_(value.is::<SlotAccessorDefinition>())
    }

    #[scheme(name = "slot-accessor-name")]
    pub fn slot_accessor_name(accessor: Gc<'gc, SlotAccessorDefinition<'gc>>) -> Value<'gc> {
        let name = Symbol::from_str(nctx.ctx, accessor.name()).into();
        nctx.return_(name)
    }

    #[scheme(name = "slot-accessor-owner")]
    pub fn slot_accessor_owner(accessor: Gc<'gc, SlotAccessorDefinition<'gc>>) -> Value<'gc> {
        let owner = class_id_to_class_object(nctx.ctx, accessor.owner());
        nctx.return_(owner)
    }

    #[scheme(name = "slot-accessor-index")]
    pub fn slot_accessor_index(accessor: Gc<'gc, SlotAccessorDefinition<'gc>>) -> Value<'gc> {
        nctx.return_(Value::new(accessor.slot_index() as i32))
    }

    #[scheme(name = "slot-accessor-init-keyword")]
    pub fn slot_accessor_init_keyword(
        accessor: Gc<'gc, SlotAccessorDefinition<'gc>>,
    ) -> Value<'gc> {
        nctx.return_(accessor.init_keyword())
    }

    #[scheme(name = "slot-accessor-init-value")]
    pub fn slot_accessor_init_value(accessor: Gc<'gc, SlotAccessorDefinition<'gc>>) -> Value<'gc> {
        nctx.return_(accessor.init_value())
    }

    #[scheme(name = "slot-accessor-init-thunk")]
    pub fn slot_accessor_init_thunk(accessor: Gc<'gc, SlotAccessorDefinition<'gc>>) -> Value<'gc> {
        nctx.return_(accessor.init_thunk())
    }

    #[scheme(name = "slot-accessor-slot-ref")]
    pub fn slot_accessor_slot_ref(accessor: Gc<'gc, SlotAccessorDefinition<'gc>>) -> Value<'gc> {
        nctx.return_(accessor.scheme_slot_ref())
    }

    #[scheme(name = "slot-accessor-slot-set!")]
    pub fn slot_accessor_slot_set(accessor: Gc<'gc, SlotAccessorDefinition<'gc>>) -> Value<'gc> {
        nctx.return_(accessor.scheme_slot_set())
    }

    #[scheme(name = "slot-accessor-slot-bound?")]
    pub fn slot_accessor_slot_bound(accessor: Gc<'gc, SlotAccessorDefinition<'gc>>) -> Value<'gc> {
        nctx.return_(accessor.scheme_slot_bound())
    }

    #[scheme(name = "slot-accessor-getter")]
    pub fn slot_accessor_getter(accessor: Gc<'gc, SlotAccessorDefinition<'gc>>) -> Value<'gc> {
        nctx.return_(accessor.getter())
    }

    #[scheme(name = "slot-accessor-setter")]
    pub fn slot_accessor_setter(accessor: Gc<'gc, SlotAccessorDefinition<'gc>>) -> Value<'gc> {
        nctx.return_(accessor.setter())
    }

    #[scheme(name = "slot-accessor-accessor")]
    pub fn slot_accessor_accessor(accessor: Gc<'gc, SlotAccessorDefinition<'gc>>) -> Value<'gc> {
        nctx.return_(accessor.accessor())
    }

    #[scheme(name = "slot-accessor-initializable?")]
    pub fn slot_accessor_initializable_p(accessor: Gc<'gc, SlotAccessorDefinition<'gc>>) -> bool {
        nctx.return_(accessor.flags().initializable())
    }

    #[scheme(name = "slot-accessor-settable?")]
    pub fn slot_accessor_settable_p(accessor: Gc<'gc, SlotAccessorDefinition<'gc>>) -> bool {
        nctx.return_(accessor.flags().settable())
    }

    #[scheme(name = "slot-accessor-immutable?")]
    pub fn slot_accessor_immutable_p(accessor: Gc<'gc, SlotAccessorDefinition<'gc>>) -> bool {
        nctx.return_(accessor.flags().immutable_flag())
    }

    #[scheme(name = "class-of")]
    pub fn class_of(value: Value<'gc>) -> Value<'gc> {
        match value.class(nctx.ctx) {
            Some(class) => nctx.return_(class.into()),
            None => nctx.return_(Value::new(false)),
        }
    }

    #[scheme(name = "current-class-of")]
    pub fn current_class_of(value: Value<'gc>) -> Value<'gc> {
        if let Some(instance) = try_scheme_instance(nctx.ctx, value) {
            return nctx.return_(instance.class().into());
        }
        match value.class(nctx.ctx) {
            Some(class) => nctx.return_(class.into()),
            None => nctx.return_(Value::new(false)),
        }
    }

    #[scheme(name = "%class-redefined?")]
    pub fn class_redefined_p(value: Value<'gc>) -> bool {
        let table = class_table(nctx.ctx);
        let class = if let Some(instance) = try_scheme_instance(nctx.ctx, value) {
            instance.class()
        } else if value.is::<ClassDescriptor>() {
            value.downcast::<ClassDescriptor>()
        } else {
            return nctx.return_(false);
        };
        let Some(current_class) = table.lookup(class.id()) else {
            return nctx.return_(false);
        };
        nctx.return_(current_class.as_gcobj() != class.as_gcobj())
    }

    #[scheme(name = "touch-instance!")]
    pub fn touch_instance(value: Value<'gc>) -> Value<'gc> {
        let Some(instance) = try_scheme_instance(nctx.ctx, value) else {
            let who = nctx.ctx.intern("touch-instance!");
            let message = nctx.ctx.str("not a Scheme instance");
            return nctx.raise_assertion_violation(who, message, value);
        };
        SchemeInstance::touch(nctx.ctx, instance);
        nctx.return_(value)
    }

    #[scheme(name = "change-class")]
    pub fn change_class(value: Value<'gc>, new_class: Gc<'gc, ClassDescriptor<'gc>>) -> Value<'gc> {
        let Some(instance) = try_scheme_instance(nctx.ctx, value) else {
            let who = nctx.ctx.intern("change-class");
            let message = nctx.ctx.str("not a Scheme instance");
            return nctx.raise_assertion_violation(who, message, value);
        };
        if new_class.category() != ClassCategory::Scheme {
            let who = nctx.ctx.intern("change-class");
            let message = nctx.ctx.str("new class is not a Scheme class");
            return nctx.raise_assertion_violation(who, message, new_class.into());
        }
        SchemeInstance::touch(nctx.ctx, instance);
        SchemeInstance::change_class(nctx.ctx, instance, new_class);
        nctx.return_(value)
    }

    #[scheme(name = "make-class")]
    pub fn make_class(
        name: Gc<'gc, Symbol<'gc>>,
        slot_names: Value<'gc>,
        direct_supers: Option<Value<'gc>>,
    ) -> Value<'gc> {
        match class_table(nctx.ctx).register_scheme_class_from_slot_list(
            nctx.ctx,
            name,
            slot_names,
            direct_supers.unwrap_or(Value::null()),
        ) {
            Ok(class) => nctx.return_(class.into()),
            Err(SchemeClassSpecError::SlotsNotList(value)) => {
                let who = nctx.ctx.intern("make-class");
                let message = nctx.ctx.str("slot names must be a proper list");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SlotNameNotSymbol(value)) => {
                let who = nctx.ctx.intern("make-class");
                let message = nctx.ctx.str("slot name is not a symbol");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SlotSpecNotList(value)) => {
                let who = nctx.ctx.intern("make-class");
                let message = nctx.ctx.str("slot spec options must be a proper list");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SlotOptionNameNotKeyword(value)) => {
                let who = nctx.ctx.intern("make-class");
                let message = nctx.ctx.str("slot option name is not a keyword");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SlotOptionMissingValue(value)) => {
                let who = nctx.ctx.intern("make-class");
                let message = nctx.ctx.str("slot option is missing a value");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::UnknownSlotOption(value)) => {
                let who = nctx.ctx.intern("make-class");
                let message = nctx.ctx.str("unknown slot option");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SupersNotList(value)) => {
                let who = nctx.ctx.intern("make-class");
                let message = nctx.ctx.str("direct supers must be a proper list");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SuperNotClass(value)) => {
                let who = nctx.ctx.intern("make-class");
                let message = nctx.ctx.str("direct super is not a class");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::MissingProcedureSlot(_)) => {
                unreachable!("ordinary Scheme classes do not require an invoker slot")
            }
            Err(SchemeClassSpecError::ClassTable(_)) => {
                let who = nctx.ctx.intern("make-class");
                let message = nctx.ctx.str("could not register class");
                nctx.raise_assertion_violation(who, message, name.into())
            }
        }
    }

    #[scheme(name = "make-invocable-class")]
    pub fn make_invocable_class(
        name: Gc<'gc, Symbol<'gc>>,
        slot_names: Value<'gc>,
        direct_supers: Option<Value<'gc>>,
    ) -> Value<'gc> {
        match class_table(nctx.ctx).register_invocable_scheme_class_from_slot_list(
            nctx.ctx,
            name,
            slot_names,
            direct_supers.unwrap_or(Value::null()),
        ) {
            Ok(class) => nctx.return_(class.into()),
            Err(SchemeClassSpecError::SlotsNotList(value)) => {
                let who = nctx.ctx.intern("make-invocable-class");
                let message = nctx.ctx.str("slot names must be a proper list");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SlotNameNotSymbol(value)) => {
                let who = nctx.ctx.intern("make-invocable-class");
                let message = nctx.ctx.str("slot name is not a symbol");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SlotSpecNotList(value)) => {
                let who = nctx.ctx.intern("make-invocable-class");
                let message = nctx.ctx.str("slot spec options must be a proper list");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SlotOptionNameNotKeyword(value)) => {
                let who = nctx.ctx.intern("make-invocable-class");
                let message = nctx.ctx.str("slot option name is not a keyword");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SlotOptionMissingValue(value)) => {
                let who = nctx.ctx.intern("make-invocable-class");
                let message = nctx.ctx.str("slot option is missing a value");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::UnknownSlotOption(value)) => {
                let who = nctx.ctx.intern("make-invocable-class");
                let message = nctx.ctx.str("unknown slot option");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SupersNotList(value)) => {
                let who = nctx.ctx.intern("make-invocable-class");
                let message = nctx.ctx.str("direct supers must be a proper list");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SuperNotClass(value)) => {
                let who = nctx.ctx.intern("make-invocable-class");
                let message = nctx.ctx.str("direct super is not a class");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::MissingProcedureSlot(value)) => {
                let who = nctx.ctx.intern("make-invocable-class");
                let message = nctx.ctx.str("invocable class must define a procedure slot");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::ClassTable(_)) => {
                let who = nctx.ctx.intern("make-invocable-class");
                let message = nctx.ctx.str("could not register class");
                nctx.raise_assertion_violation(who, message, name.into())
            }
        }
    }

    #[scheme(name = "redefine-class!")]
    pub fn redefine_class(
        class: Gc<'gc, ClassDescriptor<'gc>>,
        name: Gc<'gc, Symbol<'gc>>,
        slot_names: Value<'gc>,
        direct_supers: Option<Value<'gc>>,
    ) -> Value<'gc> {
        match class_table(nctx.ctx).redefine_scheme_class_from_slot_list(
            nctx.ctx,
            class,
            name,
            slot_names,
            direct_supers.unwrap_or(Value::null()),
        ) {
            Ok(class) => nctx.return_(class.into()),
            Err(SchemeClassSpecError::SlotsNotList(value)) => {
                let who = nctx.ctx.intern("redefine-class!");
                let message = nctx.ctx.str("slot names must be a proper list");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SlotNameNotSymbol(value)) => {
                let who = nctx.ctx.intern("redefine-class!");
                let message = nctx.ctx.str("slot name is not a symbol");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SlotSpecNotList(value)) => {
                let who = nctx.ctx.intern("redefine-class!");
                let message = nctx.ctx.str("slot spec options must be a proper list");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SlotOptionNameNotKeyword(value)) => {
                let who = nctx.ctx.intern("redefine-class!");
                let message = nctx.ctx.str("slot option name is not a keyword");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SlotOptionMissingValue(value)) => {
                let who = nctx.ctx.intern("redefine-class!");
                let message = nctx.ctx.str("slot option is missing a value");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::UnknownSlotOption(value)) => {
                let who = nctx.ctx.intern("redefine-class!");
                let message = nctx.ctx.str("unknown slot option");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SupersNotList(value)) => {
                let who = nctx.ctx.intern("redefine-class!");
                let message = nctx.ctx.str("direct supers must be a proper list");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SuperNotClass(value)) => {
                let who = nctx.ctx.intern("redefine-class!");
                let message = nctx.ctx.str("direct super is not a class");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::MissingProcedureSlot(_)) => {
                unreachable!("ordinary Scheme class redefinition does not require an invoker slot")
            }
            Err(SchemeClassSpecError::ClassTable(_)) => {
                let who = nctx.ctx.intern("redefine-class!");
                let message = nctx.ctx.str("could not redefine class");
                nctx.raise_assertion_violation(who, message, class.into())
            }
        }
    }

    #[scheme(name = "redefine-invocable-class!")]
    pub fn redefine_invocable_class(
        class: Gc<'gc, ClassDescriptor<'gc>>,
        name: Gc<'gc, Symbol<'gc>>,
        slot_names: Value<'gc>,
        direct_supers: Option<Value<'gc>>,
    ) -> Value<'gc> {
        match class_table(nctx.ctx).redefine_invocable_scheme_class_from_slot_list(
            nctx.ctx,
            class,
            name,
            slot_names,
            direct_supers.unwrap_or(Value::null()),
        ) {
            Ok(class) => nctx.return_(class.into()),
            Err(SchemeClassSpecError::SlotsNotList(value)) => {
                let who = nctx.ctx.intern("redefine-invocable-class!");
                let message = nctx.ctx.str("slot names must be a proper list");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SlotNameNotSymbol(value)) => {
                let who = nctx.ctx.intern("redefine-invocable-class!");
                let message = nctx.ctx.str("slot name is not a symbol");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SlotSpecNotList(value)) => {
                let who = nctx.ctx.intern("redefine-invocable-class!");
                let message = nctx.ctx.str("slot spec options must be a proper list");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SlotOptionNameNotKeyword(value)) => {
                let who = nctx.ctx.intern("redefine-invocable-class!");
                let message = nctx.ctx.str("slot option name is not a keyword");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SlotOptionMissingValue(value)) => {
                let who = nctx.ctx.intern("redefine-invocable-class!");
                let message = nctx.ctx.str("slot option is missing a value");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::UnknownSlotOption(value)) => {
                let who = nctx.ctx.intern("redefine-invocable-class!");
                let message = nctx.ctx.str("unknown slot option");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SupersNotList(value)) => {
                let who = nctx.ctx.intern("redefine-invocable-class!");
                let message = nctx.ctx.str("direct supers must be a proper list");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SuperNotClass(value)) => {
                let who = nctx.ctx.intern("redefine-invocable-class!");
                let message = nctx.ctx.str("direct super is not a class");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::MissingProcedureSlot(value)) => {
                let who = nctx.ctx.intern("redefine-invocable-class!");
                let message = nctx.ctx.str("invocable class must define a procedure slot");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::ClassTable(_)) => {
                let who = nctx.ctx.intern("redefine-invocable-class!");
                let message = nctx.ctx.str("could not redefine class");
                nctx.raise_assertion_violation(who, message, class.into())
            }
        }
    }

    #[scheme(name = "make-generic")]
    pub fn make_generic(
        name: Gc<'gc, Symbol<'gc>>,
        max_required_dispatch_args: Option<i32>,
    ) -> Value<'gc> {
        let max_required_dispatch_args = max_required_dispatch_args.unwrap_or(1);
        if max_required_dispatch_args < 0 {
            let who = nctx.ctx.intern("make-generic");
            let message = nctx
                .ctx
                .str("required dispatch argument count must be non-negative");
            return nctx.raise_assertion_violation(
                who,
                message,
                Value::new(max_required_dispatch_args),
            );
        }

        let name_value = name.into();
        let name_string = name.to_string();
        let generic =
            GenericDescriptor::new(nctx.ctx, &name_string, max_required_dispatch_args as usize);
        let proc = generic_procedure(nctx.ctx, name_value, generic);
        nctx.return_(proc.into())
    }

    #[scheme(name = "add-method!")]
    pub fn add_method(
        generic: Value<'gc>,
        specializers: Value<'gc>,
        required_arg_count: i32,
        body: Value<'gc>,
        locked: Option<bool>,
    ) -> Value<'gc> {
        if required_arg_count < 0 {
            let who = nctx.ctx.intern("add-method!");
            let message = nctx.ctx.str("required argument count must be non-negative");
            return nctx.raise_assertion_violation(who, message, Value::new(required_arg_count));
        }

        let specializers = match parse_class_list(specializers) {
            Ok(specializers) => specializers,
            Err(ClassListError::NotList(value)) => {
                let who = nctx.ctx.intern("add-method!");
                let message = nctx.ctx.str("specializers must be a proper list");
                return nctx.raise_assertion_violation(who, message, value);
            }
            Err(ClassListError::NotClass(value)) => {
                let who = nctx.ctx.intern("add-method!");
                let message = nctx.ctx.str("specializer is not a class");
                return nctx.raise_assertion_violation(who, message, value);
            }
        };
        let generic = match generic_descriptor_from_value(nctx.ctx, generic) {
            Some(generic) => generic,
            None => {
                let who = nctx.ctx.intern("add-method!");
                let message = nctx.ctx.str("not a generic procedure");
                return nctx.raise_assertion_violation(who, message, generic);
            }
        };

        if specializers.len() > required_arg_count as usize {
            let who = nctx.ctx.intern("add-method!");
            let message = nctx
                .ctx
                .str("method cannot specialize more arguments than it requires");
            return nctx.raise_assertion_violation(who, message, Value::new(required_arg_count));
        }

        if generic.flags().sealed() {
            let who = nctx.ctx.intern("add-method!");
            let message = nctx.ctx.str("generic is sealed");
            return nctx.raise_assertion_violation(who, message, generic.into());
        }

        if let Some(method) = generic.find_method(&specializers, required_arg_count as usize)
            && method.flags().locked()
        {
            let who = nctx.ctx.intern("add-method!");
            let message = nctx.ctx.str("method is locked");
            return nctx.raise_assertion_violation(who, message, method.into());
        }

        let flags = if locked.unwrap_or(false) {
            MethodFlags::locked_flag()
        } else {
            MethodFlags::empty()
        };
        let method = GenericDescriptor::add_method_with_flags(
            nctx.ctx,
            generic,
            &specializers,
            required_arg_count as usize,
            body,
            flags,
        );
        nctx.return_(method.into())
    }

    #[scheme(name = "set-generic-fallback!")]
    pub fn set_generic_fallback(generic: Value<'gc>, fallback: Value<'gc>) -> Value<'gc> {
        let generic = match generic_descriptor_from_value(nctx.ctx, generic) {
            Some(generic) => generic,
            None => {
                let who = nctx.ctx.intern("set-generic-fallback!");
                let message = nctx.ctx.str("not a generic procedure");
                return nctx.raise_assertion_violation(who, message, generic);
            }
        };
        GenericDescriptor::set_fallback(nctx.ctx, generic, fallback);
        nctx.return_(Value::undefined())
    }

    #[scheme(name = "generic-dispatch")]
    pub fn generic_dispatch(generic: Value<'gc>, args: &'gc [Value<'gc>]) -> Value<'gc> {
        let generic = match generic_descriptor_from_value(nctx.ctx, generic) {
            Some(generic) => generic,
            None => {
                let who = nctx.ctx.intern("generic-dispatch");
                let message = nctx.ctx.str("not a generic procedure");
                return nctx.raise_assertion_violation(who, message, generic);
            }
        };
        match GenericDescriptor::dispatch_body_cached(nctx.ctx, generic, args) {
            Ok(body) => nctx.return_(body),
            Err(GenericDispatchError::Arity) => {
                let who = nctx.ctx.intern("generic-dispatch");
                let message = nctx.ctx.str("not enough dispatch arguments");
                nctx.raise_assertion_violation(who, message, Value::new(args.len() as i32))
            }
            Err(GenericDispatchError::NoApplicableMethod) => {
                let who = nctx.ctx.intern("generic-dispatch");
                let message = nctx.ctx.str("no applicable method");
                nctx.raise_assertion_violation(who, message, generic.into())
            }
        }
    }

    #[scheme(name = "generic-invoke")]
    pub fn generic_invoke(generic: Value<'gc>, args: &'gc [Value<'gc>]) -> Value<'gc> {
        let generic = match generic_descriptor_from_value(nctx.ctx, generic) {
            Some(generic) => generic,
            None => {
                let who = nctx.ctx.intern("generic-invoke");
                let message = nctx.ctx.str("not a generic procedure");
                return nctx.raise_assertion_violation(who, message, generic);
            }
        };
        match GenericDescriptor::invocation(nctx.ctx, generic, args) {
            Ok(invocation) => nctx.return_call(invocation.body(), invocation.args().as_slice()),
            Err(GenericDispatchError::Arity) => {
                let who = nctx.ctx.intern("generic-invoke");
                let message = nctx.ctx.str("not enough dispatch arguments");
                nctx.raise_assertion_violation(who, message, Value::new(args.len() as i32))
            }
            Err(GenericDispatchError::NoApplicableMethod) => {
                let who = nctx.ctx.intern("generic-invoke");
                let message = nctx.ctx.str("no applicable method");
                nctx.raise_assertion_violation(who, message, generic.into())
            }
        }
    }

    #[scheme(name = "generic-next-methods")]
    pub fn generic_next_methods(generic: Value<'gc>, args: &'gc [Value<'gc>]) -> Value<'gc> {
        let generic = match generic_descriptor_from_value(nctx.ctx, generic) {
            Some(generic) => generic,
            None => {
                let who = nctx.ctx.intern("generic-next-methods");
                let message = nctx.ctx.str("not a generic procedure");
                return nctx.raise_assertion_violation(who, message, generic);
            }
        };
        match GenericDescriptor::next_method_chain(nctx.ctx, generic, args) {
            Ok(next_method) => nctx.return_(next_method.into()),
            Err(GenericDispatchError::Arity) => {
                let who = nctx.ctx.intern("generic-next-methods");
                let message = nctx.ctx.str("not enough dispatch arguments");
                nctx.raise_assertion_violation(who, message, Value::new(args.len() as i32))
            }
            Err(GenericDispatchError::NoApplicableMethod) => {
                let who = nctx.ctx.intern("generic-next-methods");
                let message = nctx.ctx.str("no applicable method");
                nctx.raise_assertion_violation(who, message, generic.into())
            }
        }
    }

    #[scheme(name = "%make-next-method")]
    pub fn make_next_method(
        generic: Value<'gc>,
        methods: Value<'gc>,
        args: Value<'gc>,
    ) -> Value<'gc> {
        let generic = match generic_descriptor_from_value(nctx.ctx, generic) {
            Some(generic) => generic,
            None => {
                let who = nctx.ctx.intern("%make-next-method");
                let message = nctx.ctx.str("not a generic procedure");
                return nctx.raise_assertion_violation(who, message, generic);
            }
        };

        let mut method_values = Vec::new();
        let mut cursor = methods;
        while !cursor.is_null() {
            if !cursor.is_pair() {
                let who = nctx.ctx.intern("%make-next-method");
                let message = nctx.ctx.str("methods must be a proper list");
                return nctx.raise_assertion_violation(who, message, cursor);
            }
            let value = cursor.car();
            let Some(method) = value.try_as::<MethodDescriptor>() else {
                let who = nctx.ctx.intern("%make-next-method");
                let message = nctx.ctx.str("method list contains a non-method");
                return nctx.raise_assertion_violation(who, message, value);
            };
            method_values.push(method);
            cursor = cursor.cdr();
        }

        let mut arg_values = Vec::new();
        let mut cursor = args;
        while !cursor.is_null() {
            if !cursor.is_pair() {
                let who = nctx.ctx.intern("%make-next-method");
                let message = nctx.ctx.str("args must be a proper list");
                return nctx.raise_assertion_violation(who, message, cursor);
            }
            arg_values.push(cursor.car());
            cursor = cursor.cdr();
        }

        if method_values.is_empty() {
            let who = nctx.ctx.intern("%make-next-method");
            let message = nctx.ctx.str("method list is empty");
            return nctx.raise_assertion_violation(who, message, methods);
        }

        let next_method = NextMethodDescriptor::new(
            nctx.ctx,
            generic,
            Array::from_slice(*nctx.ctx, &method_values),
            Array::from_slice(*nctx.ctx, &arg_values),
            0,
        );
        nctx.return_(next_method.into())
    }

    #[scheme(name = "next-method-methods")]
    pub fn next_method_methods(next_method: Gc<'gc, NextMethodDescriptor<'gc>>) -> Value<'gc> {
        let methods = next_method
            .methods()
            .iter()
            .map(|method| Value::from(*method))
            .collect::<Vec<_>>();
        let methods = Value::list_from_slice(nctx.ctx, &methods);
        nctx.return_(methods)
    }

    #[scheme(name = "next-method?")]
    pub fn next_method_p(value: Value<'gc>) -> bool {
        nctx.return_(value.is::<NextMethodDescriptor>())
    }

    #[scheme(name = "next-method-generic")]
    pub fn next_method_generic(next_method: Gc<'gc, NextMethodDescriptor<'gc>>) -> Value<'gc> {
        nctx.return_(next_method.generic().into())
    }

    #[scheme(name = "next-method-args")]
    pub fn next_method_args(next_method: Gc<'gc, NextMethodDescriptor<'gc>>) -> Value<'gc> {
        let args = Value::list_from_slice(nctx.ctx, next_method.args().as_slice());
        nctx.return_(args)
    }

    #[scheme(name = "next-method-index")]
    pub fn next_method_index(next_method: Gc<'gc, NextMethodDescriptor<'gc>>) -> Value<'gc> {
        nctx.return_(Value::new(next_method.index() as i32))
    }

    #[scheme(name = "next-method-body")]
    pub fn next_method_body(next_method: Gc<'gc, NextMethodDescriptor<'gc>>) -> Value<'gc> {
        nctx.return_(next_method.body())
    }

    #[scheme(name = "next-method-has-next?")]
    pub fn next_method_has_next_p(next_method: Gc<'gc, NextMethodDescriptor<'gc>>) -> bool {
        nctx.return_(next_method.has_next())
    }

    #[scheme(name = "next-method-invoke")]
    pub fn next_method_invoke(next_method: Gc<'gc, NextMethodDescriptor<'gc>>) -> Value<'gc> {
        match NextMethodDescriptor::next_invocation(nctx.ctx, next_method) {
            Some((body, args)) => nctx.return_call(body, args.as_slice()),
            None => {
                let who = nctx.ctx.intern("next-method-invoke");
                let message = nctx.ctx.str("no next method");
                nctx.raise_assertion_violation(who, message, next_method.into())
            }
        }
    }

    #[scheme(name = "next-method-next")]
    pub fn next_method_next(next_method: Gc<'gc, NextMethodDescriptor<'gc>>) -> Value<'gc> {
        match next_method.next(nctx.ctx) {
            Some(next_method) => nctx.return_(next_method.into()),
            None => nctx.return_(Value::new(false)),
        }
    }

    #[scheme(name = "scheme-instance?")]
    pub fn scheme_instance_p(value: Value<'gc>) -> bool {
        let is_instance = try_scheme_instance(nctx.ctx, value).is_some();
        nctx.return_(is_instance)
    }

    #[scheme(name = "make-instance")]
    pub fn make_instance(
        class: Gc<'gc, ClassDescriptor<'gc>>,
        initargs: &'gc [Value<'gc>],
    ) -> Value<'gc> {
        if class.category() != ClassCategory::Scheme {
            let who = nctx.ctx.intern("make-instance");
            let message = nctx.ctx.str("not a Scheme class");
            return nctx.raise_assertion_violation(who, message, class.into());
        }
        if !initargs.len().is_multiple_of(2) {
            let who = nctx.ctx.intern("make-instance");
            let message = nctx.ctx.str("initargs must be keyword/value pairs");
            return nctx.raise_assertion_violation(who, message, Value::new(initargs.len() as i32));
        }

        let mut parsed_initargs = Vec::new();
        for pair in initargs.chunks_exact(2) {
            let keyword_value = pair[0];
            if !keyword_value.is::<Keyword>() {
                let who = nctx.ctx.intern("make-instance");
                let message = nctx.ctx.str("initarg name is not a keyword");
                return nctx.raise_assertion_violation(who, message, keyword_value);
            }
            let keyword = keyword_value.downcast::<Keyword>().to_symbol().into();
            parsed_initargs.push((keyword, pair[1]));
        }

        let Some(allocation) = class.allocate_instance(nctx.ctx, class, &parsed_initargs) else {
            let who = nctx.ctx.intern("make-instance");
            let message = nctx.ctx.str("class is not allocatable");
            return nctx.raise_assertion_violation(who, message, class.into());
        };

        match allocation {
            Ok(instance) => nctx.return_(instance),
            Err(SlotInitError::UnknownKeyword(keyword)) => {
                let who = nctx.ctx.intern("make-instance");
                let message = nctx.ctx.str("unknown init keyword");
                nctx.raise_assertion_violation(who, message, keyword)
            }
            Err(SlotInitError::NotInitializable(keyword)) => {
                let who = nctx.ctx.intern("make-instance");
                let message = nctx.ctx.str("slot is not initializable");
                nctx.raise_assertion_violation(who, message, keyword)
            }
            Err(SlotInitError::InitThunkFailed(error)) => {
                let who = nctx.ctx.intern("make-instance");
                let message = nctx.ctx.str("slot init thunk failed");
                nctx.raise_assertion_violation(who, message, error)
            }
            Err(SlotInitError::MissingInvokerSlot(value)) => {
                let who = nctx.ctx.intern("make-instance");
                let message = nctx.ctx.str("invocable class has no procedure slot");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SlotInitError::InvokerNotProcedure(value)) => {
                let who = nctx.ctx.intern("make-instance");
                let message = nctx.ctx.str("invocable procedure slot is not a procedure");
                nctx.raise_assertion_violation(who, message, value)
            }
        }
    }

    #[scheme(name = "slot-ref")]
    pub fn slot_ref(
        instance: Value<'gc>,
        name: Gc<'gc, Symbol<'gc>>,
        fallback: Option<Value<'gc>>,
    ) -> Value<'gc> {
        let Some(instance) = try_scheme_instance(nctx.ctx, instance) else {
            let who = nctx.ctx.intern("slot-ref");
            let message = nctx.ctx.str("not a Scheme instance");
            return nctx.raise_assertion_violation(who, message, instance);
        };
        SchemeInstance::touch(nctx.ctx, instance);
        let slot_name = name.to_string();
        let class = instance.class();
        let receiver = Value::from(instance);

        let Some(result) =
            class.slot_value_by_name(nctx.ctx, class, receiver, &slot_name, fallback)
        else {
            let who = nctx.ctx.intern("slot-ref");
            let message = nctx.ctx.str("class has no slot reader");
            return nctx.raise_assertion_violation(who, message, class.into());
        };

        match result {
            Ok(value) => nctx.return_(value),
            Err(SlotAccessError::Unbound) => {
                if let Some(slot_unbound) = class_root_binding(nctx.ctx, "slot-unbound") {
                    return nctx.return_call(slot_unbound, &[class.into(), receiver, name.into()]);
                }
                let who = nctx.ctx.intern("slot-ref");
                let message = nctx.ctx.str("unbound slot");
                nctx.raise_assertion_violation(who, message, name.into())
            }
            Err(SlotAccessError::OutOfBounds) => {
                let who = nctx.ctx.intern("slot-ref");
                let message = nctx.ctx.str("stale slot accessor");
                nctx.raise_assertion_violation(who, message, name.into())
            }
            Err(SlotAccessError::Immutable) => unreachable!("slot reads cannot fail as immutable"),
            Err(SlotAccessError::SchemeHookFailed) => {
                let who = nctx.ctx.intern("slot-ref");
                let message = nctx.ctx.str("slot reader failed");
                nctx.raise_assertion_violation(who, message, name.into())
            }
            Err(SlotAccessError::Unknown) => {
                if let Some(slot_missing) = class_root_binding(nctx.ctx, "slot-missing") {
                    return nctx.return_call(slot_missing, &[class.into(), receiver, name.into()]);
                }
                let who = nctx.ctx.intern("slot-ref");
                let message = nctx.ctx.str("unknown slot");
                nctx.raise_assertion_violation(who, message, name.into())
            }
        }
    }

    #[scheme(name = "%class-slot-ref")]
    pub fn class_slot_ref(
        class: Gc<'gc, ClassDescriptor<'gc>>,
        slot: Gc<'gc, SlotDefinitionDescriptor<'gc>>,
    ) -> Value<'gc> {
        let Some(accessor) = class.accessor_named(slot.name()) else {
            let who = nctx.ctx.intern("class-slot-ref");
            let message = nctx.ctx.str("class has no slot");
            return nctx.raise_assertion_violation(who, message, slot.into());
        };
        match class.read_class_slot(accessor) {
            Ok(value) => nctx.return_(value),
            Err(SlotAccessError::Unbound) => {
                let who = nctx.ctx.intern("class-slot-ref");
                let message = nctx.ctx.str("unbound class slot");
                nctx.raise_assertion_violation(who, message, slot.into())
            }
            Err(_) => {
                let who = nctx.ctx.intern("class-slot-ref");
                let message = nctx.ctx.str("slot is not class allocated");
                nctx.raise_assertion_violation(who, message, slot.into())
            }
        }
    }

    #[scheme(name = "%class-slot-set!")]
    pub fn class_slot_set(
        class: Gc<'gc, ClassDescriptor<'gc>>,
        slot: Gc<'gc, SlotDefinitionDescriptor<'gc>>,
        value: Value<'gc>,
    ) -> Value<'gc> {
        let Some(accessor) = class.accessor_named(slot.name()) else {
            let who = nctx.ctx.intern("class-slot-set!");
            let message = nctx.ctx.str("class has no slot");
            return nctx.raise_assertion_violation(who, message, slot.into());
        };
        match ClassDescriptor::set_class_slot(nctx.ctx, class, accessor, value) {
            Ok(()) => nctx.return_(value),
            Err(SlotAccessError::Immutable) => {
                let who = nctx.ctx.intern("class-slot-set!");
                let message = nctx.ctx.str("immutable class slot");
                nctx.raise_assertion_violation(who, message, slot.into())
            }
            Err(_) => {
                let who = nctx.ctx.intern("class-slot-set!");
                let message = nctx.ctx.str("slot is not class allocated");
                nctx.raise_assertion_violation(who, message, slot.into())
            }
        }
    }

    #[scheme(name = "%class-slot-bound?")]
    pub fn class_slot_bound_p(
        class: Gc<'gc, ClassDescriptor<'gc>>,
        slot: Gc<'gc, SlotDefinitionDescriptor<'gc>>,
    ) -> Value<'gc> {
        let Some(accessor) = class.accessor_named(slot.name()) else {
            let who = nctx.ctx.intern("class-slot-bound?");
            let message = nctx.ctx.str("class has no slot");
            return nctx.raise_assertion_violation(who, message, slot.into());
        };
        match class.class_slot_bound(accessor) {
            Ok(bound) => nctx.return_(Value::new(bound)),
            Err(_) => {
                let who = nctx.ctx.intern("class-slot-bound?");
                let message = nctx.ctx.str("slot is not class allocated");
                nctx.raise_assertion_violation(who, message, slot.into())
            }
        }
    }

    #[scheme(name = "slot-set!")]
    pub fn slot_set(
        instance: Value<'gc>,
        name: Gc<'gc, Symbol<'gc>>,
        value: Value<'gc>,
    ) -> Value<'gc> {
        let Some(instance_obj) = try_scheme_instance(nctx.ctx, instance) else {
            let who = nctx.ctx.intern("slot-set!");
            let message = nctx.ctx.str("not a Scheme instance");
            return nctx.raise_assertion_violation(who, message, instance);
        };
        SchemeInstance::touch(nctx.ctx, instance_obj);
        let slot_name = name.to_string();
        let class = instance_obj.class();
        let receiver = Value::from(instance_obj);

        let Some(result) =
            class.set_slot_value_by_name(nctx.ctx, class, receiver, &slot_name, value)
        else {
            let who = nctx.ctx.intern("slot-set!");
            let message = nctx.ctx.str("class has no slot writer");
            return nctx.raise_assertion_violation(who, message, class.into());
        };

        match result {
            Ok(()) => nctx.return_(Value::undefined()),
            Err(SlotAccessError::Immutable) => {
                let who = nctx.ctx.intern("slot-set!");
                let message = nctx.ctx.str("immutable slot");
                nctx.raise_assertion_violation(who, message, name.into())
            }
            Err(SlotAccessError::OutOfBounds) => {
                let who = nctx.ctx.intern("slot-set!");
                let message = nctx.ctx.str("stale slot accessor");
                nctx.raise_assertion_violation(who, message, name.into())
            }
            Err(SlotAccessError::Unknown) => {
                let who = nctx.ctx.intern("slot-set!");
                let message = nctx.ctx.str("unknown slot");
                nctx.raise_assertion_violation(who, message, name.into())
            }
            Err(SlotAccessError::SchemeHookFailed) => {
                let who = nctx.ctx.intern("slot-set!");
                let message = nctx.ctx.str("slot writer failed");
                nctx.raise_assertion_violation(who, message, name.into())
            }
            Err(SlotAccessError::Unbound) => unreachable!("slot writes cannot fail as unbound"),
        }
    }

    #[scheme(name = "instance-slot-ref")]
    pub fn instance_slot_ref(
        instance: Value<'gc>,
        index: i32,
        fallback: Option<Value<'gc>>,
    ) -> Value<'gc> {
        if index < 0 {
            let who = nctx.ctx.intern("instance-slot-ref");
            let message = nctx.ctx.str("slot index must be non-negative");
            return nctx.raise_assertion_violation(who, message, Value::new(index));
        }
        let Some(instance) = try_scheme_instance(nctx.ctx, instance) else {
            let who = nctx.ctx.intern("instance-slot-ref");
            let message = nctx.ctx.str("not a Scheme instance");
            return nctx.raise_assertion_violation(who, message, instance);
        };
        SchemeInstance::touch(nctx.ctx, instance);

        match instance.read_slot(index as usize) {
            Ok(value) => nctx.return_(value),
            Err(SlotAccessError::Unbound) => match fallback {
                Some(value) => nctx.return_(value),
                None => {
                    let who = nctx.ctx.intern("instance-slot-ref");
                    let message = nctx.ctx.str("unbound slot");
                    nctx.raise_assertion_violation(who, message, Value::new(index))
                }
            },
            Err(SlotAccessError::OutOfBounds) => {
                let who = nctx.ctx.intern("instance-slot-ref");
                let message = nctx.ctx.str("slot index out of bounds");
                nctx.raise_assertion_violation(who, message, Value::new(index))
            }
            Err(SlotAccessError::Immutable) => {
                unreachable!("raw slot reads cannot fail as immutable")
            }
            Err(SlotAccessError::SchemeHookFailed) => {
                unreachable!("raw slot reads do not dispatch Scheme hooks")
            }
            Err(SlotAccessError::Unknown) => unreachable!("raw slot reads do not resolve names"),
        }
    }

    #[scheme(name = "instance-slot-set!")]
    pub fn instance_slot_set(instance: Value<'gc>, index: i32, value: Value<'gc>) -> Value<'gc> {
        if index < 0 {
            let who = nctx.ctx.intern("instance-slot-set!");
            let message = nctx.ctx.str("slot index must be non-negative");
            return nctx.raise_assertion_violation(who, message, Value::new(index));
        }
        let Some(instance) = try_scheme_instance(nctx.ctx, instance) else {
            let who = nctx.ctx.intern("instance-slot-set!");
            let message = nctx.ctx.str("not a Scheme instance");
            return nctx.raise_assertion_violation(who, message, instance);
        };
        SchemeInstance::touch(nctx.ctx, instance);
        if index as usize >= instance.slot_count() {
            let who = nctx.ctx.intern("instance-slot-set!");
            let message = nctx.ctx.str("slot index out of bounds");
            return nctx.raise_assertion_violation(who, message, Value::new(index));
        }

        SchemeInstance::set_slot(nctx.ctx, instance, index as usize, value);
        nctx.return_(Value::undefined())
    }

    #[scheme(name = "slot-initialize-using-accessor!")]
    pub fn slot_initialize_using_accessor(
        instance: Value<'gc>,
        accessor: Gc<'gc, SlotAccessorDefinition<'gc>>,
        initargs: Value<'gc>,
    ) -> Value<'gc> {
        let Some(instance) = try_scheme_instance(nctx.ctx, instance) else {
            let who = nctx.ctx.intern("slot-initialize-using-accessor!");
            let message = nctx.ctx.str("not a Scheme instance");
            return nctx.raise_assertion_violation(who, message, instance);
        };
        SchemeInstance::touch(nctx.ctx, instance);
        let accessor = accessor.as_accessor_descriptor();
        let accessor_name = Symbol::from_str(nctx.ctx, accessor.name()).into();
        if accessor.slot_index() >= instance.slot_count() {
            let who = nctx.ctx.intern("slot-initialize-using-accessor!");
            let message = nctx.ctx.str("stale slot accessor");
            return nctx.raise_assertion_violation(who, message, accessor_name);
        }

        let mut explicit_value = None;
        let mut cursor = initargs;
        while !cursor.is_null() {
            if !cursor.is_pair() {
                let who = nctx.ctx.intern("slot-initialize-using-accessor!");
                let message = nctx.ctx.str("initargs must be keyword/value pairs");
                return nctx.raise_assertion_violation(who, message, cursor);
            }
            let keyword_value = cursor.car();
            if !keyword_value.is::<Keyword>() {
                let who = nctx.ctx.intern("slot-initialize-using-accessor!");
                let message = nctx.ctx.str("initarg name is not a keyword");
                return nctx.raise_assertion_violation(who, message, keyword_value);
            }
            let rest = cursor.cdr();
            if !rest.is_pair() {
                let who = nctx.ctx.intern("slot-initialize-using-accessor!");
                let message = nctx.ctx.str("initargs must be keyword/value pairs");
                return nctx.raise_assertion_violation(who, message, cursor);
            }
            let keyword: Value<'gc> = keyword_value.downcast::<Keyword>().to_symbol().into();
            if keyword == accessor.init_keyword() {
                explicit_value = Some(rest.car());
            }
            cursor = rest.cdr();
        }

        let value = if let Some(value) = explicit_value {
            if !accessor.flags().initializable() {
                let who = nctx.ctx.intern("slot-initialize-using-accessor!");
                let message = nctx.ctx.str("slot is not initializable");
                return nctx.raise_assertion_violation(who, message, accessor.init_keyword());
            }
            value
        } else if !accessor.init_thunk().is_empty() {
            match crate::runtime::vm::call_scheme(nctx.ctx, accessor.init_thunk(), []) {
                crate::runtime::vm::VMResult::Ok(value) => value,
                crate::runtime::vm::VMResult::Err(error) => {
                    let who = nctx.ctx.intern("slot-initialize-using-accessor!");
                    let message = nctx.ctx.str("slot init thunk failed");
                    return nctx.raise_assertion_violation(who, message, error);
                }
            }
        } else {
            accessor.init_value()
        };

        if !value.is_empty() && accessor.class_allocated() {
            match ClassDescriptor::set_class_slot(nctx.ctx, instance.class(), accessor, value) {
                Ok(()) => {}
                Err(SlotAccessError::Immutable) => {
                    let who = nctx.ctx.intern("slot-initialize-using-accessor!");
                    let message = nctx.ctx.str("immutable class slot");
                    return nctx.raise_assertion_violation(who, message, accessor_name);
                }
                Err(_) => {
                    let who = nctx.ctx.intern("slot-initialize-using-accessor!");
                    let message = nctx.ctx.str("stale slot accessor");
                    return nctx.raise_assertion_violation(who, message, accessor_name);
                }
            }
        } else if !value.is_empty() {
            SchemeInstance::set_slot(nctx.ctx, instance, accessor.slot_index(), value);
        }
        nctx.return_(Value::undefined())
    }

    #[scheme(name = "slot-ref-using-accessor")]
    pub fn slot_ref_using_accessor(
        instance: Value<'gc>,
        accessor: Gc<'gc, SlotAccessorDefinition<'gc>>,
    ) -> Value<'gc> {
        let Some(instance) = try_scheme_instance(nctx.ctx, instance) else {
            let who = nctx.ctx.intern("slot-ref-using-accessor");
            let message = nctx.ctx.str("not a Scheme instance");
            return nctx.raise_assertion_violation(who, message, instance);
        };
        SchemeInstance::touch(nctx.ctx, instance);
        let accessor = accessor.as_accessor_descriptor();
        let class = instance.class();
        let receiver = Value::from(instance);
        let accessor_name = Symbol::from_str(nctx.ctx, accessor.name()).into();

        let Some(result) = ({
            if !accessor.scheme_slot_ref().is_empty() {
                Some(call_scheme_slot_ref(
                    nctx.ctx,
                    receiver,
                    accessor.scheme_slot_ref(),
                ))
            } else {
                accessor
                    .slot_ref()
                    .or_else(|| class.primitive_operation_hooks.slot_ref())
                    .map(|slot_ref| slot_ref(nctx.ctx, class, receiver, accessor))
            }
        }) else {
            let who = nctx.ctx.intern("slot-ref-using-accessor");
            let message = nctx.ctx.str("class has no slot reader");
            return nctx.raise_assertion_violation(who, message, class.into());
        };

        match result {
            Ok(value) => nctx.return_(value),
            Err(SlotAccessError::Unbound) => {
                let who = nctx.ctx.intern("slot-ref-using-accessor");
                let message = nctx.ctx.str("unbound slot");
                nctx.raise_assertion_violation(who, message, accessor_name)
            }
            Err(SlotAccessError::OutOfBounds) => {
                let who = nctx.ctx.intern("slot-ref-using-accessor");
                let message = nctx.ctx.str("stale slot accessor");
                nctx.raise_assertion_violation(who, message, accessor_name)
            }
            Err(SlotAccessError::Immutable) => unreachable!("slot reads cannot fail as immutable"),
            Err(SlotAccessError::SchemeHookFailed) => {
                let who = nctx.ctx.intern("slot-ref-using-accessor");
                let message = nctx.ctx.str("slot reader failed");
                nctx.raise_assertion_violation(who, message, accessor_name)
            }
            Err(SlotAccessError::Unknown) => {
                let who = nctx.ctx.intern("slot-ref-using-accessor");
                let message = nctx.ctx.str("unknown slot");
                nctx.raise_assertion_violation(who, message, accessor_name)
            }
        }
    }

    #[scheme(name = "slot-set-using-accessor!")]
    pub fn slot_set_using_accessor(
        instance: Value<'gc>,
        accessor: Gc<'gc, SlotAccessorDefinition<'gc>>,
        value: Value<'gc>,
    ) -> Value<'gc> {
        let Some(instance_obj) = try_scheme_instance(nctx.ctx, instance) else {
            let who = nctx.ctx.intern("slot-set-using-accessor!");
            let message = nctx.ctx.str("not a Scheme instance");
            return nctx.raise_assertion_violation(who, message, instance);
        };
        SchemeInstance::touch(nctx.ctx, instance_obj);
        let accessor = accessor.as_accessor_descriptor();
        let class = instance_obj.class();
        let receiver = Value::from(instance_obj);
        let accessor_name = Symbol::from_str(nctx.ctx, accessor.name()).into();

        let Some(result) = ({
            if !accessor.scheme_slot_set().is_empty() {
                Some(call_scheme_slot_set(
                    nctx.ctx,
                    receiver,
                    value,
                    accessor.scheme_slot_set(),
                ))
            } else {
                accessor
                    .slot_set()
                    .or_else(|| class.primitive_operation_hooks.slot_set())
                    .map(|slot_set| slot_set(nctx.ctx, class, receiver, accessor, value))
            }
        }) else {
            let who = nctx.ctx.intern("slot-set-using-accessor!");
            let message = nctx.ctx.str("class has no slot writer");
            return nctx.raise_assertion_violation(who, message, class.into());
        };

        match result {
            Ok(()) => nctx.return_(Value::undefined()),
            Err(SlotAccessError::Immutable) => {
                let who = nctx.ctx.intern("slot-set-using-accessor!");
                let message = nctx.ctx.str("immutable slot");
                nctx.raise_assertion_violation(who, message, accessor_name)
            }
            Err(SlotAccessError::OutOfBounds) => {
                let who = nctx.ctx.intern("slot-set-using-accessor!");
                let message = nctx.ctx.str("stale slot accessor");
                nctx.raise_assertion_violation(who, message, accessor_name)
            }
            Err(SlotAccessError::Unknown) => {
                let who = nctx.ctx.intern("slot-set-using-accessor!");
                let message = nctx.ctx.str("unknown slot");
                nctx.raise_assertion_violation(who, message, accessor_name)
            }
            Err(SlotAccessError::SchemeHookFailed) => {
                let who = nctx.ctx.intern("slot-set-using-accessor!");
                let message = nctx.ctx.str("slot writer failed");
                nctx.raise_assertion_violation(who, message, accessor_name)
            }
            Err(SlotAccessError::Unbound) => unreachable!("slot writes cannot fail as unbound"),
        }
    }

    #[scheme(name = "slot-bound?")]
    pub fn slot_bound_p(instance: Value<'gc>, name: Gc<'gc, Symbol<'gc>>) -> Value<'gc> {
        let Some(instance) = try_scheme_instance(nctx.ctx, instance) else {
            let who = nctx.ctx.intern("slot-bound?");
            let message = nctx.ctx.str("not a Scheme instance");
            return nctx.raise_assertion_violation(who, message, instance);
        };
        SchemeInstance::touch(nctx.ctx, instance);
        let slot_name = name.to_string();
        let class = instance.class();
        let receiver = Value::from(instance);

        let Some(result) = class.slot_bound_by_name(nctx.ctx, class, receiver, &slot_name) else {
            let who = nctx.ctx.intern("slot-bound?");
            let message = nctx.ctx.str("class has no slot bound checker");
            return nctx.raise_assertion_violation(who, message, class.into());
        };

        match result {
            Ok(bound) => nctx.return_(Value::new(bound)),
            Err(SlotAccessError::Unknown) => {
                let who = nctx.ctx.intern("slot-bound?");
                let message = nctx.ctx.str("unknown slot");
                nctx.raise_assertion_violation(who, message, name.into())
            }
            Err(SlotAccessError::OutOfBounds) => {
                let who = nctx.ctx.intern("slot-bound?");
                let message = nctx.ctx.str("stale slot accessor");
                nctx.raise_assertion_violation(who, message, name.into())
            }
            Err(SlotAccessError::SchemeHookFailed) => {
                let who = nctx.ctx.intern("slot-bound?");
                let message = nctx.ctx.str("slot bound checker failed");
                nctx.raise_assertion_violation(who, message, name.into())
            }
            Err(SlotAccessError::Unbound) => {
                unreachable!("slot-bound? reports unbound slots as false")
            }
            Err(SlotAccessError::Immutable) => {
                unreachable!("slot-bound? does not mutate slots")
            }
        }
    }

    #[scheme(name = "slot-bound-using-accessor?")]
    pub fn slot_bound_using_accessor_p(
        instance: Value<'gc>,
        accessor: Gc<'gc, SlotAccessorDefinition<'gc>>,
    ) -> Value<'gc> {
        let Some(instance) = try_scheme_instance(nctx.ctx, instance) else {
            let who = nctx.ctx.intern("slot-bound-using-accessor?");
            let message = nctx.ctx.str("not a Scheme instance");
            return nctx.raise_assertion_violation(who, message, instance);
        };
        SchemeInstance::touch(nctx.ctx, instance);
        let accessor = accessor.as_accessor_descriptor();
        let class = instance.class();
        let receiver = Value::from(instance);
        let accessor_name = Symbol::from_str(nctx.ctx, accessor.name()).into();

        let Some(result) = ({
            if !accessor.scheme_slot_bound().is_empty() {
                Some(call_scheme_slot_bound(
                    nctx.ctx,
                    receiver,
                    accessor.scheme_slot_bound(),
                ))
            } else {
                accessor
                    .slot_bound()
                    .or_else(|| class.primitive_operation_hooks.slot_bound())
                    .map(|slot_bound| slot_bound(nctx.ctx, class, receiver, accessor))
            }
        }) else {
            let who = nctx.ctx.intern("slot-bound-using-accessor?");
            let message = nctx.ctx.str("class has no slot bound checker");
            return nctx.raise_assertion_violation(who, message, class.into());
        };

        match result {
            Ok(bound) => nctx.return_(Value::new(bound)),
            Err(SlotAccessError::Unknown) => {
                let who = nctx.ctx.intern("slot-bound-using-accessor?");
                let message = nctx.ctx.str("unknown slot");
                nctx.raise_assertion_violation(who, message, accessor_name)
            }
            Err(SlotAccessError::OutOfBounds) => {
                let who = nctx.ctx.intern("slot-bound-using-accessor?");
                let message = nctx.ctx.str("stale slot accessor");
                nctx.raise_assertion_violation(who, message, accessor_name)
            }
            Err(SlotAccessError::SchemeHookFailed) => {
                let who = nctx.ctx.intern("slot-bound-using-accessor?");
                let message = nctx.ctx.str("slot bound checker failed");
                nctx.raise_assertion_violation(who, message, accessor_name)
            }
            Err(SlotAccessError::Unbound) => {
                unreachable!("slot-bound-using-accessor? reports unbound slots as false")
            }
            Err(SlotAccessError::Immutable) => {
                unreachable!("slot-bound-using-accessor? does not mutate slots")
            }
        }
    }
}

pub(crate) fn init_class_ops<'gc>(ctx: Context<'gc>) {
    class_ops::register(ctx);
}

pub fn primitive_layout_hooks_for_class_id(id: ClassId) -> Option<PrimitiveLayoutHooks> {
    if let Some(table) = CLASS_TABLE.get() {
        // SAFETY: The class table is a global GC root. This helper only copies
        // scalar/function-pointer hook metadata out of a class object and does
        // not retain the rooted reference across this call.
        return unsafe { table.fetch_unchecked() }
            .lookup(id)
            .and_then(|descriptor| descriptor.primitive_layout_hooks());
    }

    if let Some(hooks) = builtin_primitive_layout_hooks(id) {
        return Some(hooks);
    }

    pending_hooks_for_class_id(id).map(PrimitiveLayoutHooks::from_allocation_hooks)
}

pub(crate) fn register_internal_type_class_if_ready<'gc>(
    ctx: Context<'gc>,
    id: ClassId,
    hooks: AllocationHooks,
) {
    let Some(table) = CLASS_TABLE.get() else {
        return;
    };

    // SAFETY: The global class table is a rooted object. This fetch is scoped to
    // inserting a descriptor that carries only copied scalar/function-pointer
    // hook metadata from the type-class bootstrap registry.
    let table = unsafe { table.fetch_unchecked() };
    register_internal_type_class(ctx, table, id, hooks);
}

fn register_pending_type_classes<'gc>(ctx: Context<'gc>, table: &ClassTable<'gc>) {
    use crate::rsgc::object::{TypeClassRegistrationGuard, record_pending_type_class};

    loop {
        let pending = drain_pending_type_classes();
        if pending.is_empty() {
            break;
        }

        for (id, hooks) in pending {
            if table.lookup(id).is_some() {
                continue;
            }

            if let Some(_guard) = TypeClassRegistrationGuard::enter() {
                register_internal_type_class(ctx, table, id, hooks);
            } else {
                record_pending_type_class(id, hooks);
            }
        }
    }
}

pub(crate) fn register_internal_type_class<'gc>(
    ctx: Context<'gc>,
    table: &ClassTable<'gc>,
    id: ClassId,
    hooks: AllocationHooks,
) {
    if table.lookup(id).is_some() {
        return;
    }

    let layout_hooks = PrimitiveLayoutHooks::from_allocation_hooks(hooks);
    table
        .register_internal_type_class(ctx, id, hooks.type_name, layout_hooks)
        .expect("type class IDs must be unique while registering into class table");
}

pub fn builtin_class_specs() -> &'static [BuiltinClassSpec] {
    BUILTIN_CLASS_SPECS
}

impl<'gc> ClassTable<'gc> {
    pub fn new(ctx: Context<'gc>) -> Self {
        Self::with_max_id(ctx, MAX_CLASS_ID)
    }

    fn with_max_id(ctx: Context<'gc>, max_id: u32) -> Self {
        let page_count = (max_id >> CLASS_TABLE_PAGE_BITS) as usize + 1;
        let pages = Array::with(*ctx, page_count, |_, _| Lock::new(None));
        Self {
            inner: Monitor::new(ClassTableInner { pages, max_id }),
            redefinition: Monitor::new(ClassRedefinitionState::default()),
        }
    }

    #[cfg(test)]
    fn redefinition_epoch(&self) -> u64 {
        self.redefinition.lock().epoch
    }

    pub fn register_builtin(
        &self,
        ctx: Context<'gc>,
        descriptor: ClassDescriptor<'gc>,
    ) -> Result<(), ClassTableError> {
        let descriptor = Gc::new_with_header_word(
            *ctx,
            descriptor,
            class_header_word(builtin_id(builtin_class_ids::CLASS)),
        );
        {
            let mut inner = self.inner.lock();
            inner.register_builtin(ctx, descriptor)?;
        }
        self.link_direct_subclasses(ctx, descriptor);
        Ok(())
    }

    pub(crate) fn register_internal_type_class(
        &self,
        ctx: Context<'gc>,
        id: ClassId,
        name: &str,
        hooks: PrimitiveLayoutHooks,
    ) -> Result<Gc<'gc, ClassDescriptor<'gc>>, ClassTableError> {
        if let Some(existing) = self.lookup(id) {
            return Ok(existing);
        }

        let descriptor = Gc::new_with_header_word(
            *ctx,
            ClassDescriptor::from_init(
                ctx,
                ClassDescriptorInit {
                    id,
                    name,
                    category: ClassCategory::Internal,
                    primitive_layout_hooks: Some(hooks),
                    primitive_operation_hooks: Some(PrimitiveOperationHooks::default()),
                    flags: None,
                    direct_supers: &[],
                    cpl: &[id],
                    inherited_slots: &[],
                    direct_slot_specs: &[],
                    direct_subclasses: None,
                    direct_methods: None,
                },
            ),
            class_header_word(builtin_id(builtin_class_ids::CLASS)),
        );
        let mut inner = self.inner.lock();
        if let Some(existing) = inner.lookup(id) {
            return Ok(existing);
        }
        inner.register(ctx, descriptor)?;
        drop(inner);
        self.link_direct_subclasses(ctx, descriptor);
        Ok(descriptor)
    }

    pub fn register_dynamic(
        &self,
        ctx: Context<'gc>,
        name: &str,
        category: ClassCategory,
    ) -> Result<Gc<'gc, ClassDescriptor<'gc>>, ClassTableError> {
        self.register_dynamic_with_slots(ctx, name, category, &[], &[])
    }

    pub fn register_dynamic_with_slots(
        &self,
        ctx: Context<'gc>,
        name: &str,
        category: ClassCategory,
        direct_supers: &[ClassId],
        direct_slots: &[SlotSpec<'_, 'gc>],
    ) -> Result<Gc<'gc, ClassDescriptor<'gc>>, ClassTableError> {
        self.register_dynamic_with_slots_and_hooks(
            ctx,
            name,
            category,
            DynamicClassOptions {
                primitive_operation_hooks: None,
                flags: None,
            },
            direct_supers,
            direct_slots,
        )
    }

    fn register_dynamic_with_slots_and_hooks(
        &self,
        ctx: Context<'gc>,
        name: &str,
        category: ClassCategory,
        options: DynamicClassOptions,
        direct_supers: &[ClassId],
        direct_slots: &[SlotSpec<'_, 'gc>],
    ) -> Result<Gc<'gc, ClassDescriptor<'gc>>, ClassTableError> {
        let direct_supers = normalized_dynamic_direct_supers(category, direct_supers);
        let (id, inherited_slots, cpl) = {
            let mut inner = self.inner.lock();
            let id = inner.next_free_dynamic_id()?;
            let (inherited_slots, cpl) = collect_dynamic_hierarchy(&inner, id, &direct_supers);
            (id, inherited_slots, cpl)
        };

        let descriptor = Gc::new_with_header_word(
            *ctx,
            ClassDescriptor::from_init(
                ctx,
                ClassDescriptorInit {
                    id,
                    name,
                    category,
                    primitive_layout_hooks: None,
                    primitive_operation_hooks: options.primitive_operation_hooks,
                    flags: options.flags,
                    direct_supers: &direct_supers,
                    cpl: &cpl,
                    inherited_slots: &inherited_slots,
                    direct_slot_specs: direct_slots,
                    direct_subclasses: None,
                    direct_methods: None,
                },
            ),
            class_header_word(builtin_id(builtin_class_ids::CLASS)),
        );
        let mut inner = self.inner.lock();
        inner.register_dynamic(ctx, descriptor)?;
        drop(inner);
        self.link_direct_subclasses(ctx, descriptor);
        Ok(descriptor)
    }

    pub fn redefine_dynamic_with_slots(
        &self,
        ctx: Context<'gc>,
        id: ClassId,
        name: &str,
        category: ClassCategory,
        direct_supers: &[ClassId],
        direct_slots: &[SlotSpec<'_, 'gc>],
    ) -> Result<Gc<'gc, ClassDescriptor<'gc>>, ClassTableError> {
        self.redefine_dynamic_with_slots_and_hooks(
            ctx,
            DynamicClassRedefinition { id, name, category },
            DynamicClassOptions {
                primitive_operation_hooks: None,
                flags: None,
            },
            direct_supers,
            direct_slots,
        )
    }

    fn redefine_dynamic_with_slots_and_hooks(
        &self,
        ctx: Context<'gc>,
        spec: DynamicClassRedefinition<'_>,
        options: DynamicClassOptions,
        direct_supers: &[ClassId],
        direct_slots: &[SlotSpec<'_, 'gc>],
    ) -> Result<Gc<'gc, ClassDescriptor<'gc>>, ClassTableError> {
        let mut redefinition = self.redefinition.lock();
        let direct_supers = normalized_dynamic_direct_supers(spec.category, direct_supers);
        let (old, inherited_slots, cpl, direct_subclasses, direct_methods, removed_direct_supers) = {
            let inner = self.inner.lock();
            let old = inner
                .lookup(spec.id)
                .ok_or(ClassTableError::MissingId(spec.id))?;
            if !old.flags().malleable() {
                return Err(ClassTableError::NotMalleable(spec.id));
            }

            let (inherited_slots, cpl) = collect_dynamic_hierarchy(&inner, spec.id, &direct_supers);

            (
                old,
                inherited_slots,
                cpl,
                old.direct_subclasses(),
                old.direct_methods(),
                old.direct_supers()
                    .iter()
                    .filter(|old_super_id| !direct_supers.contains(old_super_id))
                    .filter_map(|old_super_id| inner.lookup(*old_super_id))
                    .collect::<Vec<_>>(),
            )
        };

        let descriptor = Gc::new_with_header_word(
            *ctx,
            ClassDescriptor::from_init(
                ctx,
                ClassDescriptorInit {
                    id: spec.id,
                    name: spec.name,
                    category: spec.category,
                    primitive_layout_hooks: None,
                    primitive_operation_hooks: options.primitive_operation_hooks,
                    flags: options.flags,
                    direct_supers: &direct_supers,
                    cpl: &cpl,
                    inherited_slots: &inherited_slots,
                    direct_slot_specs: direct_slots,
                    direct_subclasses: Some(direct_subclasses),
                    direct_methods: Some(direct_methods),
                },
            ),
            class_header_word(builtin_id(builtin_class_ids::CLASS)),
        );
        let mut inner = self.inner.lock();
        let descriptor = inner.redefine_dynamic(ctx, descriptor)?;
        drop(inner);
        for old_super in removed_direct_supers {
            ClassDescriptor::remove_direct_subclass(ctx, old_super, spec.id);
        }
        ClassDescriptor::invalidate_direct_method_caches(ctx, old);
        self.link_direct_subclasses(ctx, descriptor);
        ClassDescriptor::invalidate_direct_method_caches(ctx, descriptor);
        redefinition.epoch += 1;
        Ok(descriptor)
    }

    pub fn register_scheme_class_from_slot_list(
        &self,
        ctx: Context<'gc>,
        name: Gc<'gc, crate::runtime::value::Symbol<'gc>>,
        slot_names: Value<'gc>,
        direct_supers: Value<'gc>,
    ) -> Result<Gc<'gc, ClassDescriptor<'gc>>, SchemeClassSpecError<'gc>> {
        let (name, direct_supers, parsed_slots) =
            parse_scheme_class_shape(ctx, name, slot_names, direct_supers)?;
        let direct_slots = parsed_slots
            .iter()
            .map(ParsedSlotSpec::as_slot_spec)
            .collect::<Vec<_>>();

        self.register_dynamic_with_slots(
            ctx,
            &name,
            ClassCategory::Scheme,
            &direct_supers,
            &direct_slots,
        )
        .map_err(SchemeClassSpecError::ClassTable)
    }

    pub fn register_invocable_scheme_class_from_slot_list(
        &self,
        ctx: Context<'gc>,
        name: Gc<'gc, crate::runtime::value::Symbol<'gc>>,
        slot_names: Value<'gc>,
        direct_supers: Value<'gc>,
    ) -> Result<Gc<'gc, ClassDescriptor<'gc>>, SchemeClassSpecError<'gc>> {
        let (name, direct_supers, parsed_slots) =
            parse_scheme_class_shape(ctx, name, slot_names, direct_supers)?;
        if !parsed_slots.iter().any(|slot| slot.name == "procedure") {
            return Err(SchemeClassSpecError::MissingProcedureSlot(
                ctx.intern(&name),
            ));
        }
        let direct_slots = parsed_slots
            .iter()
            .map(ParsedSlotSpec::as_slot_spec)
            .collect::<Vec<_>>();

        self.register_dynamic_with_slots_and_hooks(
            ctx,
            &name,
            ClassCategory::Scheme,
            invocable_scheme_class_options(),
            &direct_supers,
            &direct_slots,
        )
        .map_err(SchemeClassSpecError::ClassTable)
    }

    pub fn redefine_scheme_class_from_slot_list(
        &self,
        ctx: Context<'gc>,
        class: Gc<'gc, ClassDescriptor<'gc>>,
        name: Gc<'gc, crate::runtime::value::Symbol<'gc>>,
        slot_names: Value<'gc>,
        direct_supers: Value<'gc>,
    ) -> Result<Gc<'gc, ClassDescriptor<'gc>>, SchemeClassSpecError<'gc>> {
        let (name, direct_supers, parsed_slots) =
            parse_scheme_class_shape(ctx, name, slot_names, direct_supers)?;
        let direct_slots = parsed_slots
            .iter()
            .map(ParsedSlotSpec::as_slot_spec)
            .collect::<Vec<_>>();

        self.redefine_dynamic_with_slots(
            ctx,
            class.id(),
            &name,
            ClassCategory::Scheme,
            &direct_supers,
            &direct_slots,
        )
        .map_err(SchemeClassSpecError::ClassTable)
    }

    pub fn redefine_invocable_scheme_class_from_slot_list(
        &self,
        ctx: Context<'gc>,
        class: Gc<'gc, ClassDescriptor<'gc>>,
        name: Gc<'gc, crate::runtime::value::Symbol<'gc>>,
        slot_names: Value<'gc>,
        direct_supers: Value<'gc>,
    ) -> Result<Gc<'gc, ClassDescriptor<'gc>>, SchemeClassSpecError<'gc>> {
        let (name, direct_supers, parsed_slots) =
            parse_scheme_class_shape(ctx, name, slot_names, direct_supers)?;
        if !parsed_slots.iter().any(|slot| slot.name == "procedure") {
            return Err(SchemeClassSpecError::MissingProcedureSlot(
                ctx.intern(&name),
            ));
        }
        let direct_slots = parsed_slots
            .iter()
            .map(ParsedSlotSpec::as_slot_spec)
            .collect::<Vec<_>>();

        self.redefine_dynamic_with_slots_and_hooks(
            ctx,
            DynamicClassRedefinition {
                id: class.id(),
                name: &name,
                category: ClassCategory::Scheme,
            },
            invocable_scheme_class_options(),
            &direct_supers,
            &direct_slots,
        )
        .map_err(SchemeClassSpecError::ClassTable)
    }

    pub fn lookup(&self, id: ClassId) -> Option<Gc<'gc, ClassDescriptor<'gc>>> {
        self.inner.lock().lookup(id)
    }

    pub fn allocated_page_count(&self) -> usize {
        self.inner.lock().allocated_page_count()
    }

    pub fn is_subclass(&self, class_id: ClassId, superclass_id: ClassId) -> bool {
        self.inner.lock().is_subclass(class_id, superclass_id)
    }

    fn link_direct_subclasses(&self, ctx: Context<'gc>, descriptor: Gc<'gc, ClassDescriptor<'gc>>) {
        let (direct_supers, existing_subclasses) = {
            let inner = self.inner.lock();
            let direct_supers = descriptor
                .direct_supers()
                .iter()
                .filter_map(|super_id| inner.lookup(*super_id))
                .collect::<Vec<_>>();
            let mut existing_subclasses = Vec::new();
            inner.for_each_class(|existing| {
                if existing.id() != descriptor.id()
                    && existing.direct_supers().contains(&descriptor.id())
                {
                    existing_subclasses.push(existing);
                }
            });
            (direct_supers, existing_subclasses)
        };

        for superclass in direct_supers {
            ClassDescriptor::add_direct_subclass(ctx, superclass, descriptor);
        }
        for subclass in existing_subclasses {
            ClassDescriptor::add_direct_subclass(ctx, descriptor, subclass);
        }
    }
}

impl<'gc> ClassTableInner<'gc> {
    fn register_builtin(
        &mut self,
        ctx: Context<'gc>,
        descriptor: Gc<'gc, ClassDescriptor<'gc>>,
    ) -> Result<(), ClassTableError> {
        self.register(ctx, descriptor)
    }

    fn register(
        &mut self,
        ctx: Context<'gc>,
        descriptor: Gc<'gc, ClassDescriptor<'gc>>,
    ) -> Result<(), ClassTableError> {
        let (page, slot_index) = self.empty_slot(ctx, descriptor.id())?;
        Gc::write(*ctx, page.slots)[slot_index]
            .unlock()
            .set(ClassSlot::Class(descriptor));
        Ok(())
    }

    fn for_each_class(&self, mut f: impl FnMut(Gc<'gc, ClassDescriptor<'gc>>)) {
        for page in self.pages.iter().filter_map(|page| page.get()) {
            for slot in page.slots.iter() {
                if let ClassSlot::Class(class) = slot.get() {
                    f(class);
                }
            }
        }
    }

    fn register_dynamic(
        &mut self,
        ctx: Context<'gc>,
        descriptor: Gc<'gc, ClassDescriptor<'gc>>,
    ) -> Result<(), ClassTableError> {
        self.register(ctx, descriptor)
    }

    fn redefine_dynamic(
        &mut self,
        ctx: Context<'gc>,
        descriptor: Gc<'gc, ClassDescriptor<'gc>>,
    ) -> Result<Gc<'gc, ClassDescriptor<'gc>>, ClassTableError> {
        let old = self
            .lookup(descriptor.id())
            .ok_or(ClassTableError::MissingId(descriptor.id()))?;
        if !old.flags().malleable() {
            return Err(ClassTableError::NotMalleable(descriptor.id()));
        }

        let (page, slot_index) = self.existing_slot(descriptor.id())?;
        Gc::write(*ctx, page.slots)[slot_index]
            .unlock()
            .set(ClassSlot::Class(descriptor));
        Ok(descriptor)
    }

    fn lookup(&self, id: ClassId) -> Option<Gc<'gc, ClassDescriptor<'gc>>> {
        if id.bits() > self.max_id {
            return None;
        }

        let (page_index, slot_index) = indexes(id);
        let page = self.pages.get(page_index)?.get()?;
        match page.slots[slot_index].get() {
            ClassSlot::Empty => None,
            ClassSlot::Class(descriptor) => Some(descriptor),
        }
    }

    fn allocated_page_count(&self) -> usize {
        self.pages
            .iter()
            .filter(|page| page.get().is_some())
            .count()
    }

    fn is_subclass(&self, class_id: ClassId, superclass_id: ClassId) -> bool {
        self.lookup(class_id)
            .is_some_and(|class| class.is_a(superclass_id))
    }

    fn empty_slot(
        &mut self,
        ctx: Context<'gc>,
        id: ClassId,
    ) -> Result<(ClassPage<'gc>, usize), ClassTableError> {
        if id.bits() > self.max_id {
            return Err(ClassTableError::IdOutOfRange);
        }

        let (page_index, slot_index) = indexes(id);
        let page = match self.pages[page_index].get() {
            Some(page) => page,
            None => {
                let page = ClassPage::new(ctx);
                Gc::write(*ctx, self.pages)[page_index]
                    .unlock()
                    .set(Some(page));
                page
            }
        };

        match page.slots[slot_index].get() {
            ClassSlot::Empty => Ok((page, slot_index)),
            ClassSlot::Class(_) => Err(ClassTableError::DuplicateId(id)),
        }
    }

    fn existing_slot(&self, id: ClassId) -> Result<(ClassPage<'gc>, usize), ClassTableError> {
        if id.bits() > self.max_id {
            return Err(ClassTableError::IdOutOfRange);
        }

        let (page_index, slot_index) = indexes(id);
        let Some(page) = self.pages[page_index].get() else {
            return Err(ClassTableError::MissingId(id));
        };

        match page.slots[slot_index].get() {
            ClassSlot::Empty => Err(ClassTableError::MissingId(id)),
            ClassSlot::Class(_) => Ok((page, slot_index)),
        }
    }

    fn next_free_dynamic_id(&mut self) -> Result<ClassId, ClassTableError> {
        let id = allocate_class_id();
        if id.bits() > self.max_id {
            return Err(ClassTableError::Exhausted);
        }
        if self.lookup(id).is_some() {
            return Err(ClassTableError::DuplicateId(id));
        }
        Ok(id)
    }
}

fn indexes(id: ClassId) -> (usize, usize) {
    let raw = id.bits();
    (
        (raw >> CLASS_TABLE_PAGE_BITS) as usize,
        (raw & CLASS_TABLE_SLOT_MASK) as usize,
    )
}

fn parse_slot_spec_list<'gc>(
    ctx: Context<'gc>,
    values: Value<'gc>,
) -> Result<Vec<ParsedSlotSpec<'gc>>, SchemeClassSpecError<'gc>> {
    let mut cursor = values;
    let mut specs = Vec::new();

    while !cursor.is_null() {
        if !cursor.is_pair() {
            return Err(SchemeClassSpecError::SlotsNotList(cursor));
        }

        specs.push(parse_slot_spec(ctx, cursor.car())?);
        cursor = cursor.cdr();
    }

    Ok(specs)
}

fn parse_slot_spec<'gc>(
    ctx: Context<'gc>,
    value: Value<'gc>,
) -> Result<ParsedSlotSpec<'gc>, SchemeClassSpecError<'gc>> {
    if let Some(symbol) = value.try_as::<crate::runtime::value::Symbol>() {
        return Ok(ParsedSlotSpec::mutable(ctx, symbol.to_string()));
    }

    if !value.is_pair() {
        return Err(SchemeClassSpecError::SlotNameNotSymbol(value));
    }

    let name_value = value.car();
    let Some(name) = name_value.try_as::<crate::runtime::value::Symbol>() else {
        return Err(SchemeClassSpecError::SlotNameNotSymbol(name_value));
    };
    let mut spec = ParsedSlotSpec::mutable(ctx, name.to_string());
    let mut options = value.cdr();

    while !options.is_null() {
        if !options.is_pair() {
            return Err(SchemeClassSpecError::SlotSpecNotList(options));
        }

        let option = options.car();
        let Some(keyword) = option.try_as::<crate::runtime::value::Keyword>() else {
            return Err(SchemeClassSpecError::SlotOptionNameNotKeyword(option));
        };
        let rest = options.cdr();
        if !rest.is_pair() {
            return Err(SchemeClassSpecError::SlotOptionMissingValue(option));
        }
        let value = rest.car();
        options = rest.cdr();

        match keyword.to_symbol().to_string().as_str() {
            "init-value" => spec.init_value = value,
            "init-keyword" => spec.init_keyword = slot_init_keyword_value(value),
            "init-thunk" => spec.init_thunk = value,
            "slot-ref" => spec.scheme_slot_ref = value,
            "slot-set!" => spec.scheme_slot_set = value,
            "slot-bound?" => spec.scheme_slot_bound = value,
            "getter" => spec.getter = value,
            "setter" => spec.setter = value,
            "accessor" => spec.accessor = value,
            "allocation" => match slot_allocation_value(value).as_deref() {
                Some("instance") => spec.class_allocated = false,
                Some("class") => spec.class_allocated = true,
                _ => return Err(SchemeClassSpecError::UnknownSlotOption(value)),
            },
            "immutable" => {
                spec.immutable = value.as_bool();
                if spec.immutable {
                    spec.settable = false;
                }
            }
            "initializable" => spec.initializable = value.as_bool(),
            "settable" => spec.settable = value.as_bool(),
            _ => return Err(SchemeClassSpecError::UnknownSlotOption(option)),
        }
    }

    Ok(spec)
}

fn slot_allocation_value<'gc>(value: Value<'gc>) -> Option<String> {
    let raw = if let Some(keyword) = value.try_as::<crate::runtime::value::Keyword>() {
        keyword.to_symbol().to_string()
    } else {
        value
            .try_as::<crate::runtime::value::Symbol>()
            .map(|symbol| symbol.to_string())?
    };
    Some(raw.strip_prefix(':').unwrap_or(raw.as_str()).to_owned())
}

fn slot_init_keyword_value<'gc>(value: Value<'gc>) -> Value<'gc> {
    if let Some(keyword) = value.try_as::<crate::runtime::value::Keyword>() {
        keyword.to_symbol().into()
    } else {
        value
    }
}

fn parse_class_list<'gc>(values: Value<'gc>) -> Result<Vec<ClassId>, ClassListError<'gc>> {
    let mut cursor = values;
    let mut ids = Vec::new();

    while !cursor.is_null() {
        if !cursor.is_pair() {
            return Err(ClassListError::NotList(cursor));
        }

        let value = cursor.car();
        let Some(class) = value.try_as::<ClassDescriptor>() else {
            return Err(ClassListError::NotClass(value));
        };
        ids.push(class.id());
        cursor = cursor.cdr();
    }

    Ok(ids)
}

fn parse_scheme_class_shape<'gc>(
    ctx: Context<'gc>,
    name: Gc<'gc, crate::runtime::value::Symbol<'gc>>,
    slot_specs: Value<'gc>,
    direct_supers: Value<'gc>,
) -> Result<(String, Vec<ClassId>, Vec<ParsedSlotSpec<'gc>>), SchemeClassSpecError<'gc>> {
    let slot_specs = parse_slot_spec_list(ctx, slot_specs)?;
    let direct_supers = parse_class_list(direct_supers).map_err(|error| match error {
        ClassListError::NotList(value) => SchemeClassSpecError::SupersNotList(value),
        ClassListError::NotClass(value) => SchemeClassSpecError::SuperNotClass(value),
    })?;
    let name = name.to_string();

    Ok((name, direct_supers, slot_specs))
}

fn builtin_direct_supers(id: ClassId) -> ([ClassId; 1], usize) {
    let raw_super: &[u32] = match id.bits() {
        builtin_class_ids::TOP => &[],
        builtin_class_ids::BOTTOM => &[builtin_class_ids::TOP],
        builtin_class_ids::OBJECT => &[builtin_class_ids::TOP],
        builtin_class_ids::TYPE => &[builtin_class_ids::OBJECT],
        builtin_class_ids::CLASS => &[builtin_class_ids::TYPE],
        builtin_class_ids::NUMBER => &[builtin_class_ids::OBJECT],
        builtin_class_ids::FIXNUM
        | builtin_class_ids::FLONUM
        | builtin_class_ids::BIGINT
        | builtin_class_ids::RATIONAL
        | builtin_class_ids::COMPLEX => &[builtin_class_ids::NUMBER],
        builtin_class_ids::UNINTERNED_SYMBOL => &[builtin_class_ids::SYMBOL],
        _ => &[builtin_class_ids::OBJECT],
    };

    let mut direct_supers = [id; 1];
    for (index, raw_id) in raw_super.iter().copied().enumerate() {
        direct_supers[index] = builtin_id(raw_id);
    }
    (direct_supers, raw_super.len())
}

fn builtin_cpl(id: ClassId) -> ([ClassId; 4], usize) {
    let mut cpl = [id; 4];
    let mut cpl_len = 0;
    let mut next = Some(id);

    while let Some(class_id) = next {
        cpl[cpl_len] = class_id;
        cpl_len += 1;

        let (direct_supers, direct_supers_len) = builtin_direct_supers(class_id);
        next = if direct_supers_len == 0 {
            None
        } else {
            Some(direct_supers[0])
        }
    }

    (cpl, cpl_len)
}

fn builtin_primitive_layout_hooks(id: ClassId) -> Option<PrimitiveLayoutHooks> {
    let hooks = match id.bits() {
        builtin_class_ids::PAIR => {
            AllocationHooksOf::<'static, crate::runtime::value::Pair<'static>>::HOOKS
        }
        builtin_class_ids::VARIABLE => {
            AllocationHooksOf::<'static, crate::runtime::modules::Variable<'static>>::HOOKS
        }
        builtin_class_ids::CLOSURE_PROC | builtin_class_ids::CLOSURE_K => {
            crate::runtime::value::CLOSURE_HOOKS
        }
        builtin_class_ids::MUTABLE_VECTOR | builtin_class_ids::IMMUTABLE_VECTOR => {
            crate::runtime::value::Vector::<'static>::HOOKS
        }
        builtin_class_ids::TUPLE => crate::runtime::value::Tuple::<'static>::HOOKS,
        builtin_class_ids::BIGINT => crate::runtime::value::BigInt::<'static>::HOOKS,
        builtin_class_ids::RATIONAL => {
            AllocationHooksOf::<'static, crate::runtime::value::Rational<'static>>::HOOKS
        }
        builtin_class_ids::COMPLEX => {
            AllocationHooksOf::<'static, crate::runtime::value::Complex<'static>>::HOOKS
        }
        builtin_class_ids::SYMBOL | builtin_class_ids::UNINTERNED_SYMBOL => {
            AllocationHooksOf::<'static, crate::runtime::value::Symbol<'static>>::HOOKS
        }
        builtin_class_ids::KEYWORD => {
            AllocationHooksOf::<'static, crate::runtime::value::Keyword<'static>>::HOOKS
        }
        builtin_class_ids::STRING | builtin_class_ids::IMMUTABLE_STRING => {
            AllocationHooksOf::<'static, crate::runtime::value::Str<'static>>::HOOKS
        }
        builtin_class_ids::STRINGBUF_WIDE | builtin_class_ids::STRINGBUF_NARROW => {
            crate::runtime::value::string::Stringbuf::HOOKS
        }
        builtin_class_ids::MUTABLE_BYTEVECTOR | builtin_class_ids::IMMUTABLE_BYTEVECTOR => {
            crate::runtime::value::ByteVector::HOOKS
        }
        builtin_class_ids::MAPPED_BYTEVECTOR => crate::runtime::value::ByteVector::MAPPING_HOOKS,
        builtin_class_ids::HASHTABLE | builtin_class_ids::IMMUTABLE_HASHTABLE => {
            AllocationHooksOf::<'static, crate::runtime::value::HashTable<'static>>::HOOKS
        }
        builtin_class_ids::WEAK_SET => {
            AllocationHooksOf::<'static, crate::runtime::value::WeakSet<'static>>::HOOKS
        }
        builtin_class_ids::WEAK_TABLE => {
            AllocationHooksOf::<'static, crate::runtime::value::WeakTable<'static>>::HOOKS
        }
        builtin_class_ids::WEAK_MAPPING => {
            AllocationHooksOf::<'static, crate::runtime::value::WeakMapping<'static>>::HOOKS
        }
        builtin_class_ids::EPHEMERON => {
            AllocationHooksOf::<'static, crate::runtime::vm::gc::Ephemeron<'static>>::HOOKS
        }
        builtin_class_ids::BOX => {
            AllocationHooksOf::<'static, crate::runtime::value::Boxed<'static>>::HOOKS
        }
        builtin_class_ids::FLUID => {
            AllocationHooksOf::<'static, crate::runtime::fluids::Fluid<'static>>::HOOKS
        }
        builtin_class_ids::DYNAMIC_STATE => {
            AllocationHooksOf::<'static, crate::runtime::fluids::DynamicStateObject<'static>>::HOOKS
        }
        builtin_class_ids::NATIVE_PROCEDURE | builtin_class_ids::NATIVE_CONTINUATION => {
            AllocationHooksOf::<'static, crate::runtime::value::NativeProc>::HOOKS
        }
        builtin_class_ids::CODE_BLOCK => {
            AllocationHooksOf::<'static, crate::runtime::value::CodeBlock<'static>>::HOOKS
        }
        builtin_class_ids::RELOCATABLE_CODE_BLOCK => {
            crate::runtime::value::RelocatableCodeBlock::<'static>::HOOKS
        }
        builtin_class_ids::MODULE => {
            AllocationHooksOf::<'static, crate::runtime::modules::Module<'static>>::HOOKS
        }
        builtin_class_ids::SYNTAX => {
            AllocationHooksOf::<'static, crate::runtime::vm::syntax::Syntax<'static>>::HOOKS
        }
        builtin_class_ids::SYNTAX_TRANSFORMER => AllocationHooksOf::<
            'static,
            crate::runtime::vm::syntax::SyntaxTransformer<'static>,
        >::HOOKS,
        builtin_class_ids::SOCKET => {
            AllocationHooksOf::<'static, crate::runtime::value::port::Socket>::HOOKS
        }
        builtin_class_ids::POLLER => {
            AllocationHooksOf::<'static, crate::runtime::vm::io::Poller>::HOOKS
        }
        builtin_class_ids::POINTER => {
            AllocationHooksOf::<'static, crate::runtime::vm::ffi::Pointer>::HOOKS
        }
        builtin_class_ids::CIF => {
            AllocationHooksOf::<'static, crate::runtime::vm::ffi::CIF<'static>>::HOOKS
        }
        builtin_class_ids::THREAD => AllocationHooksOf::<
            'static,
            crate::runtime::vm::threading::ThreadObject<'static>,
        >::HOOKS,
        builtin_class_ids::MUTEX => {
            AllocationHooksOf::<'static, crate::runtime::vm::threading::Mutex>::HOOKS
        }
        builtin_class_ids::CONDITION => {
            AllocationHooksOf::<'static, crate::runtime::vm::threading::Condition>::HOOKS
        }
        builtin_class_ids::ANNOTATION => {
            #[cfg(feature = "bootstrap")]
            {
                AllocationHooksOf::<'static, crate::frontend::reader::Annotation<'static>>::HOOKS
            }
            #[cfg(not(feature = "bootstrap"))]
            {
                return None;
            }
        }
        builtin_class_ids::CONTINUATION_MARKS => AllocationHooksOf::<
            'static,
            crate::runtime::vm::control::ContinuationMarks<'static>,
        >::HOOKS,
        builtin_class_ids::CLASS => AllocationHooksOf::<'static, ClassDescriptor<'static>>::HOOKS,
        builtin_class_ids::GENERIC => {
            AllocationHooksOf::<'static, GenericDescriptor<'static>>::HOOKS
        }
        builtin_class_ids::METHOD => AllocationHooksOf::<'static, MethodDescriptor<'static>>::HOOKS,
        builtin_class_ids::NEXT_METHOD => {
            AllocationHooksOf::<'static, NextMethodDescriptor<'static>>::HOOKS
        }
        builtin_class_ids::SLOT_DEFINITION => {
            AllocationHooksOf::<'static, SlotDefinitionDescriptor<'static>>::HOOKS
        }
        builtin_class_ids::SLOT_ACCESSOR => {
            AllocationHooksOf::<'static, SlotAccessorDefinition<'static>>::HOOKS
        }
        _ => return None,
    };

    Some(PrimitiveLayoutHooks::from_allocation_hooks(hooks))
}

const fn builtin_id(raw: u32) -> ClassId {
    match ClassId::new(raw) {
        Some(id) => id,
        None => panic!("invalid built-in class id"),
    }
}

macro_rules! builtin_specs {
    ($($name:literal => $id:expr, $category:ident;)*) => {
        const BUILTIN_CLASS_SPECS: &[BuiltinClassSpec] = &[
            $(
                BuiltinClassSpec {
                    id: builtin_id($id),
                    name: $name,
                    category: ClassCategory::$category,
                },
            )*
        ];
    };
}

builtin_specs! {
    "pair" => builtin_class_ids::PAIR, Builtin;
    "variable" => builtin_class_ids::VARIABLE, Builtin;
    "closure" => builtin_class_ids::CLOSURE_PROC, Builtin;
    "continuation-closure" => builtin_class_ids::CLOSURE_K, Builtin;
    "vector" => builtin_class_ids::MUTABLE_VECTOR, Builtin;
    "immutable-vector" => builtin_class_ids::IMMUTABLE_VECTOR, Builtin;
    "tuple" => builtin_class_ids::TUPLE, Builtin;
    "top" => builtin_class_ids::TOP, Abstract;
    "bottom" => builtin_class_ids::BOTTOM, Abstract;
    "type" => builtin_class_ids::TYPE, Abstract;
    "class" => builtin_class_ids::CLASS, Builtin;
    "object" => builtin_class_ids::OBJECT, Abstract;
    "bool" => builtin_class_ids::BOOL, Immediate;
    "char" => builtin_class_ids::CHAR, Immediate;
    "null" => builtin_class_ids::NULL, Immediate;
    "eof" => builtin_class_ids::EOF, Immediate;
    "void" => builtin_class_ids::VOID, Immediate;
    "unspecified" => builtin_class_ids::UNSPECIFIED, Immediate;
    "undefined" => builtin_class_ids::UNDEFINED, Immediate;
    "fixnum" => builtin_class_ids::FIXNUM, Immediate;
    "flonum" => builtin_class_ids::FLONUM, Immediate;
    "bigint" => builtin_class_ids::BIGINT, Builtin;
    "rational" => builtin_class_ids::RATIONAL, Builtin;
    "complex" => builtin_class_ids::COMPLEX, Builtin;
    "number" => builtin_class_ids::NUMBER, Abstract;
    "symbol" => builtin_class_ids::SYMBOL, Builtin;
    "uninterned-symbol" => builtin_class_ids::UNINTERNED_SYMBOL, Builtin;
    "keyword" => builtin_class_ids::KEYWORD, Builtin;
    "string" => builtin_class_ids::STRING, Builtin;
    "immutable-string" => builtin_class_ids::IMMUTABLE_STRING, Builtin;
    "stringbuf-wide" => builtin_class_ids::STRINGBUF_WIDE, Builtin;
    "stringbuf-narrow" => builtin_class_ids::STRINGBUF_NARROW, Builtin;
    "bytevector" => builtin_class_ids::MUTABLE_BYTEVECTOR, Builtin;
    "immutable-bytevector" => builtin_class_ids::IMMUTABLE_BYTEVECTOR, Builtin;
    "mapped-bytevector" => builtin_class_ids::MAPPED_BYTEVECTOR, Builtin;
    "hash-table" => builtin_class_ids::HASHTABLE, Builtin;
    "immutable-hash-table" => builtin_class_ids::IMMUTABLE_HASHTABLE, Builtin;
    "weak-set" => builtin_class_ids::WEAK_SET, Builtin;
    "weak-table" => builtin_class_ids::WEAK_TABLE, Builtin;
    "weak-mapping" => builtin_class_ids::WEAK_MAPPING, Builtin;
    "ephemeron" => builtin_class_ids::EPHEMERON, Builtin;
    "box" => builtin_class_ids::BOX, Builtin;
    "fluid" => builtin_class_ids::FLUID, Builtin;
    "dynamic-state" => builtin_class_ids::DYNAMIC_STATE, Builtin;
    "native-procedure" => builtin_class_ids::NATIVE_PROCEDURE, Builtin;
    "native-continuation" => builtin_class_ids::NATIVE_CONTINUATION, Builtin;
    "code-block" => builtin_class_ids::CODE_BLOCK, Builtin;
    "relocatable-code-block" => builtin_class_ids::RELOCATABLE_CODE_BLOCK, Builtin;
    "module" => builtin_class_ids::MODULE, Builtin;
    "environment" => builtin_class_ids::ENVIRONMENT, Abstract;
    "syntax" => builtin_class_ids::SYNTAX, Builtin;
    "syntax-transformer" => builtin_class_ids::SYNTAX_TRANSFORMER, Builtin;
    "port" => builtin_class_ids::PORT, Abstract;
    "socket" => builtin_class_ids::SOCKET, Builtin;
    "poller" => builtin_class_ids::POLLER, Builtin;
    "ffi-pointer" => builtin_class_ids::POINTER, Builtin;
    "cif" => builtin_class_ids::CIF, Builtin;
    "thread" => builtin_class_ids::THREAD, Builtin;
    "mutex" => builtin_class_ids::MUTEX, Builtin;
    "condition" => builtin_class_ids::CONDITION, Builtin;
    "annotation" => builtin_class_ids::ANNOTATION, Builtin;
    "continuation-marks" => builtin_class_ids::CONTINUATION_MARKS, Builtin;
    "generic" => builtin_class_ids::GENERIC, Builtin;
    "method" => builtin_class_ids::METHOD, Builtin;
    "next-method" => builtin_class_ids::NEXT_METHOD, Builtin;
    "slot-definition" => builtin_class_ids::SLOT_DEFINITION, Builtin;
    "slot-accessor" => builtin_class_ids::SLOT_ACCESSOR, Builtin;
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashSet;

    struct GcOnlyDummy {
        value: usize,
    }

    struct PendingRegistrationDummy {
        value: usize,
    }

    unsafe impl Trace for GcOnlyDummy {
        unsafe fn trace(&mut self, visitor: &mut Visitor) {
            let _ = visitor;
        }

        unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::rsgc::WeakProcessor) {
            let _ = weak_processor;
        }
    }

    unsafe impl Trace for PendingRegistrationDummy {
        unsafe fn trace(&mut self, visitor: &mut Visitor) {
            let _ = visitor;
        }

        unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::rsgc::WeakProcessor) {
            let _ = weak_processor;
        }
    }

    fn id(raw: u32) -> ClassId {
        ClassId::new(raw).unwrap()
    }

    fn symbol<'gc>(ctx: Context<'gc>, name: &str) -> Value<'gc> {
        crate::runtime::value::Symbol::from_str(ctx, name).into()
    }

    extern "C-unwind" fn constant_17_thunk<'gc>(
        _ctx: Context<'gc>,
        _rator: Value<'gc>,
        _rands: *const Value<'gc>,
        _num_rands: usize,
        _retk: Value<'gc>,
    ) -> crate::runtime::value::NativeReturn<'gc> {
        crate::runtime::value::NativeReturn {
            code: crate::runtime::value::ReturnCode::ReturnOk,
            value: Value::new(17),
        }
    }

    extern "C-unwind" fn slot_reader_proc<'gc>(
        _ctx: Context<'gc>,
        _rator: Value<'gc>,
        _rands: *const Value<'gc>,
        num_rands: usize,
        _retk: Value<'gc>,
    ) -> crate::runtime::value::NativeReturn<'gc> {
        let value = if num_rands == 1 {
            Value::new(911)
        } else {
            Value::new(-1)
        };
        crate::runtime::value::NativeReturn {
            code: crate::runtime::value::ReturnCode::ReturnOk,
            value,
        }
    }

    extern "C-unwind" fn slot_writer_proc<'gc>(
        _ctx: Context<'gc>,
        _rator: Value<'gc>,
        rands: *const Value<'gc>,
        num_rands: usize,
        _retk: Value<'gc>,
    ) -> crate::runtime::value::NativeReturn<'gc> {
        let value = if num_rands == 2 {
            // SAFETY: the VM passes `num_rands` contiguous argument values.
            unsafe { *rands.add(1) }
        } else {
            Value::new(-1)
        };
        crate::runtime::value::NativeReturn {
            code: crate::runtime::value::ReturnCode::ReturnOk,
            value,
        }
    }

    extern "C-unwind" fn slot_bound_proc<'gc>(
        _ctx: Context<'gc>,
        _rator: Value<'gc>,
        _rands: *const Value<'gc>,
        num_rands: usize,
        _retk: Value<'gc>,
    ) -> crate::runtime::value::NativeReturn<'gc> {
        crate::runtime::value::NativeReturn {
            code: crate::runtime::value::ReturnCode::ReturnOk,
            value: Value::new(num_rands == 1),
        }
    }

    fn test_slot_ref_hook<'gc>(
        _ctx: Context<'gc>,
        _class: Gc<'gc, ClassDescriptor<'gc>>,
        _receiver: Value<'gc>,
        _accessor: SlotAccessorDescriptor<'gc>,
    ) -> Result<Value<'gc>, SlotAccessError> {
        Ok(Value::new(901))
    }

    fn test_slot_set_hook<'gc>(
        _ctx: Context<'gc>,
        _class: Gc<'gc, ClassDescriptor<'gc>>,
        _receiver: Value<'gc>,
        _accessor: SlotAccessorDescriptor<'gc>,
        value: Value<'gc>,
    ) -> Result<(), SlotAccessError> {
        if value == Value::new(902) {
            Ok(())
        } else {
            Err(SlotAccessError::Immutable)
        }
    }

    fn test_slot_bound_hook<'gc>(
        _ctx: Context<'gc>,
        _class: Gc<'gc, ClassDescriptor<'gc>>,
        _receiver: Value<'gc>,
        _accessor: SlotAccessorDescriptor<'gc>,
    ) -> Result<bool, SlotAccessError> {
        Ok(true)
    }

    fn slot_ref_hook_addr(hook: PrimitiveSlotRefHook) -> usize {
        hook as *const () as usize
    }

    fn slot_set_hook_addr(hook: PrimitiveSlotSetHook) -> usize {
        hook as *const () as usize
    }

    fn slot_bound_hook_addr(hook: PrimitiveSlotBoundHook) -> usize {
        hook as *const () as usize
    }

    #[test]
    fn builtin_specs_have_unique_ids_and_names() {
        let mut ids = HashSet::new();
        let mut names = HashSet::new();

        for spec in builtin_class_specs() {
            assert!(ids.insert(spec.id()));
            assert!(names.insert(spec.name()));
        }

        assert!(ids.contains(&id(builtin_class_ids::PAIR)));
        assert!(ids.contains(&id(builtin_class_ids::BOOL)));
        assert!(ids.contains(&id(builtin_class_ids::CLASS)));
        assert!(ids.contains(&id(builtin_class_ids::THREAD)));
        assert!(ids.contains(&id(builtin_class_ids::GENERIC)));
        assert!(ids.contains(&id(builtin_class_ids::METHOD)));
        assert!(ids.contains(&id(builtin_class_ids::NEXT_METHOD)));
    }

    #[test]
    fn global_class_table_contains_builtin_specs() {
        crate::runtime::thread::Scheme::new_uninit().enter(|ctx| {
            let table = class_table(ctx);

            for spec in builtin_class_specs() {
                let descriptor = table.lookup(spec.id()).unwrap();
                assert_eq!(descriptor.id(), spec.id());
                assert_eq!(descriptor.name(), spec.name());
                assert_eq!(descriptor.category(), spec.category());
            }
        });
    }

    #[test]
    fn builtin_classes_publish_primitive_layout_hooks_from_class_metadata() {
        crate::runtime::thread::Scheme::new_uninit().enter(|ctx| {
            let table = class_table(ctx);
            let cases = [
                builtin_class_ids::PAIR,
                builtin_class_ids::VARIABLE,
                builtin_class_ids::CLOSURE_PROC,
                builtin_class_ids::CLOSURE_K,
                builtin_class_ids::MUTABLE_VECTOR,
                builtin_class_ids::IMMUTABLE_VECTOR,
                builtin_class_ids::TUPLE,
                builtin_class_ids::BIGINT,
                builtin_class_ids::RATIONAL,
                builtin_class_ids::COMPLEX,
                builtin_class_ids::SYMBOL,
                builtin_class_ids::UNINTERNED_SYMBOL,
                builtin_class_ids::KEYWORD,
                builtin_class_ids::STRING,
                builtin_class_ids::IMMUTABLE_STRING,
                builtin_class_ids::STRINGBUF_WIDE,
                builtin_class_ids::STRINGBUF_NARROW,
                builtin_class_ids::MUTABLE_BYTEVECTOR,
                builtin_class_ids::IMMUTABLE_BYTEVECTOR,
                builtin_class_ids::MAPPED_BYTEVECTOR,
                builtin_class_ids::HASHTABLE,
                builtin_class_ids::IMMUTABLE_HASHTABLE,
                builtin_class_ids::WEAK_SET,
                builtin_class_ids::WEAK_TABLE,
                builtin_class_ids::WEAK_MAPPING,
                builtin_class_ids::EPHEMERON,
                builtin_class_ids::BOX,
                builtin_class_ids::FLUID,
                builtin_class_ids::DYNAMIC_STATE,
                builtin_class_ids::NATIVE_PROCEDURE,
                builtin_class_ids::NATIVE_CONTINUATION,
                builtin_class_ids::CODE_BLOCK,
                builtin_class_ids::RELOCATABLE_CODE_BLOCK,
                builtin_class_ids::MODULE,
                builtin_class_ids::SYNTAX,
                builtin_class_ids::SYNTAX_TRANSFORMER,
                builtin_class_ids::SOCKET,
                builtin_class_ids::POLLER,
                builtin_class_ids::POINTER,
                builtin_class_ids::CIF,
                builtin_class_ids::THREAD,
                builtin_class_ids::MUTEX,
                builtin_class_ids::CONDITION,
                builtin_class_ids::ANNOTATION,
                builtin_class_ids::CONTINUATION_MARKS,
                builtin_class_ids::CLASS,
                builtin_class_ids::GENERIC,
                builtin_class_ids::METHOD,
                builtin_class_ids::NEXT_METHOD,
            ];

            for raw_id in cases {
                let hooks = table
                    .lookup(id(raw_id))
                    .unwrap()
                    .primitive_layout_hooks()
                    .expect("heap class should carry primitive hooks");
                assert!(
                    table
                        .lookup(id(raw_id))
                        .unwrap()
                        .primitive_operation_hooks()
                        .allocate()
                        .is_none(),
                    "built-in class {raw_id} should not get the Scheme allocate hook"
                );
                assert!(
                    table
                        .lookup(id(raw_id))
                        .unwrap()
                        .primitive_operation_hooks()
                        .print()
                        .is_none(),
                    "built-in class {raw_id} should not get the Scheme print hook"
                );
                assert!(
                    table
                        .lookup(id(raw_id))
                        .unwrap()
                        .primitive_operation_hooks()
                        .compare()
                        .is_none(),
                    "built-in class {raw_id} should not get the Scheme compare hook"
                );
                assert!(
                    table
                        .lookup(id(raw_id))
                        .unwrap()
                        .primitive_operation_hooks()
                        .hash()
                        .is_none(),
                    "built-in class {raw_id} should not get the Scheme hash hook"
                );
                assert!(
                    table
                        .lookup(id(raw_id))
                        .unwrap()
                        .primitive_operation_hooks()
                        .slot_ref()
                        .is_none(),
                    "built-in class {raw_id} should not get the Scheme slot-ref hook"
                );
                assert!(
                    table
                        .lookup(id(raw_id))
                        .unwrap()
                        .primitive_operation_hooks()
                        .slot_set()
                        .is_none(),
                    "built-in class {raw_id} should not get the Scheme slot-set hook"
                );
                assert!(
                    table
                        .lookup(id(raw_id))
                        .unwrap()
                        .primitive_operation_hooks()
                        .slot_bound()
                        .is_none(),
                    "built-in class {raw_id} should not get the Scheme slot-bound hook"
                );
                let expected_hooks = builtin_primitive_layout_hooks(id(raw_id))
                    .expect("built-in hook lookup should find heap class hooks");

                assert_eq!(
                    hooks.trace() as usize,
                    expected_hooks.trace() as usize,
                    "trace hook mismatch for class id {raw_id}"
                );
                assert_eq!(
                    hooks.weak_proc() as usize,
                    expected_hooks.weak_proc() as usize,
                    "weak hook mismatch for class id {raw_id}"
                );
                assert_eq!(hooks.instance_size(), expected_hooks.instance_size());
                assert_eq!(
                    hooks.compute_size().is_some(),
                    expected_hooks.compute_size().is_some()
                );
                assert_eq!(hooks.alignment(), expected_hooks.alignment());
                assert_eq!(
                    hooks.compute_alignment().is_some(),
                    expected_hooks.compute_alignment().is_some()
                );
                assert_eq!(hooks.type_name(), expected_hooks.type_name());

                let global_hooks = primitive_layout_hooks_for_class_id(id(raw_id))
                    .expect("global class hook lookup should find heap class hooks");
                assert_eq!(global_hooks.instance_size(), hooks.instance_size());
                assert_eq!(global_hooks.alignment(), hooks.alignment());
                assert_eq!(global_hooks.type_name(), hooks.type_name());
            }

            let descriptorless_cases = [
                builtin_class_ids::TOP,
                builtin_class_ids::BOTTOM,
                builtin_class_ids::TYPE,
                builtin_class_ids::OBJECT,
                builtin_class_ids::BOOL,
                builtin_class_ids::CHAR,
                builtin_class_ids::NULL,
                builtin_class_ids::EOF,
                builtin_class_ids::VOID,
                builtin_class_ids::UNSPECIFIED,
                builtin_class_ids::UNDEFINED,
                builtin_class_ids::FIXNUM,
                builtin_class_ids::FLONUM,
                builtin_class_ids::NUMBER,
                builtin_class_ids::ENVIRONMENT,
                builtin_class_ids::PORT,
            ];

            for raw_id in descriptorless_cases {
                let class = table.lookup(id(raw_id)).unwrap();
                assert!(
                    matches!(
                        class.category(),
                        ClassCategory::Abstract | ClassCategory::Immediate
                    ),
                    "descriptorless class {raw_id} should be an abstract or immediate class"
                );
                assert!(
                    class.primitive_layout_hooks().is_none(),
                    "descriptorless class {raw_id} should not carry primitive hooks"
                );
            }
        });
    }

    #[test]
    fn builtin_classes_have_cpl_for_family_predicates() {
        crate::runtime::thread::Scheme::new_uninit().enter(|ctx| {
            let table = class_table(ctx);
            let fixnum = table.lookup(id(builtin_class_ids::FIXNUM)).unwrap();
            let number = id(builtin_class_ids::NUMBER);
            let object = id(builtin_class_ids::OBJECT);
            let top = id(builtin_class_ids::TOP);

            assert_eq!(fixnum.direct_supers(), &[number]);
            assert!(fixnum.is_a(number));
            assert!(fixnum.is_a(object));
            assert!(fixnum.is_a(top));
            assert!(table.is_subclass(id(builtin_class_ids::COMPLEX), number));
            assert!(table.is_subclass(id(builtin_class_ids::PAIR), object));
            assert!(table.is_subclass(id(builtin_class_ids::CLASS), id(builtin_class_ids::TYPE)));
            assert!(table.is_subclass(
                id(builtin_class_ids::UNINTERNED_SYMBOL),
                id(builtin_class_ids::SYMBOL)
            ));
            assert!(!table.is_subclass(id(builtin_class_ids::PAIR), number));
        });
    }

    #[test]
    fn builtin_classes_populate_weak_direct_subclasses() {
        crate::runtime::thread::Scheme::new_uninit().enter(|ctx| {
            let table = class_table(ctx);
            let object = table.lookup(id(builtin_class_ids::OBJECT)).unwrap();
            let number = table.lookup(id(builtin_class_ids::NUMBER)).unwrap();

            assert!(
                object.direct_subclasses().iter().any(|subclass| {
                    subclass.upgrade(*ctx).is_some_and(|class| {
                        class.id() == id(builtin_class_ids::PAIR)
                            || class.id() == id(builtin_class_ids::NUMBER)
                    })
                }),
                "object should have subclasses registered before and after object itself"
            );
            assert!(
                number.direct_subclasses().iter().any(|subclass| {
                    subclass
                        .upgrade(*ctx)
                        .is_some_and(|class| class.id() == id(builtin_class_ids::FIXNUM))
                }),
                "number should have numeric direct subclasses"
            );
        });
    }

    #[test]
    fn table_roots_classes_and_lookup_returns_descriptor() {
        crate::runtime::thread::Scheme::new_uninit().enter(|ctx| {
            let table = ClassTable::with_max_id(ctx, 512);

            table
                .register_builtin(
                    ctx,
                    ClassDescriptor::new(ctx, id(1), "pair", ClassCategory::Builtin),
                )
                .unwrap();

            let found = table.lookup(id(1)).unwrap();
            assert_eq!(found.name(), "pair");
            assert_eq!(found.category(), ClassCategory::Builtin);
        });
    }

    #[test]
    fn registration_updates_superclass_weak_subclasses() {
        crate::runtime::thread::Scheme::new_uninit().enter(|ctx| {
            let table = ClassTable::with_max_id(ctx, 512);

            table
                .register_builtin(
                    ctx,
                    ClassDescriptor::with_hierarchy(
                        ctx,
                        id(1),
                        "object",
                        ClassCategory::Abstract,
                        &[],
                        &[id(1)],
                    ),
                )
                .unwrap();
            table
                .register_builtin(
                    ctx,
                    ClassDescriptor::with_hierarchy(
                        ctx,
                        id(2),
                        "pair",
                        ClassCategory::Builtin,
                        &[id(1)],
                        &[id(2), id(1)],
                    ),
                )
                .unwrap();

            let object = table.lookup(id(1)).unwrap();
            let subclasses = object.direct_subclasses();

            assert_eq!(subclasses.len(), 1);
            assert_eq!(subclasses[0].upgrade(*ctx).unwrap().id(), id(2));
        });
    }

    #[test]
    fn class_descriptors_reserve_gc_managed_metadata_arrays() {
        crate::runtime::thread::Scheme::new_uninit().enter(|ctx| {
            let descriptor = ClassDescriptor::new(ctx, id(1), "pair", ClassCategory::Builtin);

            assert!(descriptor.direct_slots().is_empty());
            assert!(descriptor.slots().is_empty());
            assert!(descriptor.accessors().is_empty());
            assert!(descriptor.direct_subclasses().is_empty());
            assert!(descriptor.direct_methods().is_empty());
            assert!(descriptor.initargs().is_empty());
        });
    }

    #[test]
    fn class_descriptors_store_typed_slot_metadata() {
        crate::runtime::thread::Scheme::new_uninit().enter(|ctx| {
            let descriptor = ClassDescriptor::from_init(
                ctx,
                ClassDescriptorInit {
                    id: id(1),
                    name: "point",
                    category: ClassCategory::Scheme,
                    primitive_layout_hooks: None,
                    primitive_operation_hooks: None,
                    flags: None,
                    direct_supers: &[],
                    cpl: &[id(1)],
                    inherited_slots: &[],
                    direct_slot_specs: &[
                        SlotSpec::mutable("x")
                            .with_init_value(Value::new(11))
                            .with_init_keyword(Value::new(101))
                            .with_init_thunk(Value::new(201))
                            .with_scheme_slot_procedures(
                                Value::new(301),
                                Value::new(302),
                                Value::new(303),
                            )
                            .with_native_slot_hooks(
                                Some(test_slot_ref_hook),
                                Some(test_slot_set_hook),
                                Some(test_slot_bound_hook),
                            ),
                        SlotSpec::immutable("y")
                            .with_init_value(Value::new(12))
                            .with_init_keyword(Value::new(102))
                            .with_init_thunk(Value::new(202)),
                        SlotSpec::mutable("locked")
                            .with_init_keyword(Value::new(103))
                            .without_initargs(),
                    ],
                    direct_subclasses: None,
                    direct_methods: None,
                },
            );

            assert_eq!(descriptor.direct_slots().len(), 3);
            assert_eq!(descriptor.slots().len(), 3);
            assert_eq!(descriptor.accessors().len(), 3);
            assert_eq!(descriptor.slots()[0].name(), "x");
            assert_eq!(descriptor.slots()[0].owner(), id(1));
            assert_eq!(descriptor.slots()[0].index(), 0);
            assert_eq!(descriptor.slots()[0].init_value(), Value::new(11));
            assert_eq!(descriptor.slots()[0].init_keyword(), Value::new(101));
            assert_eq!(descriptor.slots()[0].init_thunk(), Value::new(201));
            assert_eq!(descriptor.slots()[0].scheme_slot_ref(), Value::new(301));
            assert_eq!(descriptor.slots()[0].scheme_slot_set(), Value::new(302));
            assert_eq!(descriptor.slots()[0].scheme_slot_bound(), Value::new(303));
            assert_eq!(
                slot_ref_hook_addr(descriptor.slots()[0].slot_ref().unwrap()),
                slot_ref_hook_addr(test_slot_ref_hook)
            );
            assert_eq!(
                slot_set_hook_addr(descriptor.slots()[0].slot_set().unwrap()),
                slot_set_hook_addr(test_slot_set_hook)
            );
            assert_eq!(
                slot_bound_hook_addr(descriptor.slots()[0].slot_bound().unwrap()),
                slot_bound_hook_addr(test_slot_bound_hook)
            );
            assert!(descriptor.slots()[0].flags().settable());
            assert_eq!(descriptor.slots()[1].name(), "y");
            assert_eq!(descriptor.slots()[1].init_value(), Value::new(12));
            assert_eq!(descriptor.slots()[1].init_keyword(), Value::new(102));
            assert_eq!(descriptor.slots()[1].init_thunk(), Value::new(202));
            assert!(descriptor.slots()[1].scheme_slot_ref().is_empty());
            assert!(descriptor.slots()[1].slot_ref().is_none());
            assert!(descriptor.slots()[1].immutable());
            assert!(!descriptor.slots()[1].flags().settable());
            assert_eq!(descriptor.initargs(), &[Value::new(101), Value::new(102)]);
            let x = descriptor.accessor_named("x").unwrap();
            let y = descriptor.accessor_named("y").unwrap();
            assert_eq!(x.slot_index(), 0);
            assert_eq!(x.init_value(), Value::new(11));
            assert_eq!(x.init_keyword(), Value::new(101));
            assert_eq!(x.init_thunk(), Value::new(201));
            assert_eq!(x.scheme_slot_ref(), Value::new(301));
            assert_eq!(x.scheme_slot_set(), Value::new(302));
            assert_eq!(x.scheme_slot_bound(), Value::new(303));
            assert_eq!(
                slot_ref_hook_addr(x.slot_ref().unwrap()),
                slot_ref_hook_addr(test_slot_ref_hook)
            );
            assert_eq!(
                slot_set_hook_addr(x.slot_set().unwrap()),
                slot_set_hook_addr(test_slot_set_hook)
            );
            assert_eq!(
                slot_bound_hook_addr(x.slot_bound().unwrap()),
                slot_bound_hook_addr(test_slot_bound_hook)
            );
            assert_eq!(y.slot_index(), 1);
            assert_eq!(y.init_value(), Value::new(12));
            assert_eq!(y.init_keyword(), Value::new(102));
            assert_eq!(y.init_thunk(), Value::new(202));
            assert!(y.scheme_slot_ref().is_empty());
            assert!(y.slot_ref().is_none());
            assert!(descriptor.accessor_named("missing").is_none());
        });
    }

    #[test]
    fn class_descriptors_have_default_flags() {
        crate::runtime::thread::Scheme::new_uninit().enter(|ctx| {
            let builtin = ClassDescriptor::new(ctx, id(1), "pair", ClassCategory::Builtin);
            let scheme = ClassDescriptor::new(ctx, id(2), "user-class", ClassCategory::Scheme);
            let immediate = ClassDescriptor::new(ctx, id(3), "fixnum", ClassCategory::Immediate);
            let internal = ClassDescriptor::new(ctx, id(4), "gc-only", ClassCategory::Internal);

            assert!(builtin.flags().sealed());
            assert!(!builtin.flags().malleable());

            assert!(internal.flags().sealed());
            assert!(!internal.flags().malleable());

            assert!(scheme.flags().malleable());
            assert!(scheme.flags().aggregate());
            assert!(!scheme.flags().sealed());

            assert_eq!(immediate.flags(), ClassFlags::empty());
        });
    }

    #[test]
    fn generic_and_method_descriptors_are_gc_objects() {
        crate::runtime::thread::Scheme::new_uninit().enter(|ctx| {
            let table = ClassTable::with_max_id(ctx, 512);
            let class = table
                .register_dynamic(ctx, "receiver", ClassCategory::Scheme)
                .unwrap();
            let generic = GenericDescriptor::new(ctx, "describe", 1);
            let method = GenericDescriptor::add_method_in_table(
                ctx,
                &table,
                generic,
                &[class.id()],
                1,
                Value::new(77),
                MethodFlags::empty(),
            );

            assert_eq!(
                generic.as_gcobj().header().class_id(),
                id(builtin_class_ids::GENERIC)
            );
            assert_eq!(
                method.as_gcobj().header().class_id(),
                id(builtin_class_ids::METHOD)
            );
            assert!(Value::from(generic).is::<GenericDescriptor>());
            assert!(Value::from(method).is::<MethodDescriptor>());
            assert_eq!(generic.name(), "describe");
            assert_eq!(generic.max_required_dispatch_args(), 1);
            assert!(generic.fallback().is_empty());
            assert!(generic.dispatcher_cache().is_empty());
            assert_eq!(method.generic().name(), "describe");
            assert_eq!(method.specializers(), &[class.id()]);
            assert_eq!(method.required_arg_count(), 1);
            assert_eq!(method.body(), Value::new(77));
            assert_eq!(generic.methods().len(), 1);
            assert_eq!(generic.methods()[0].body(), Value::new(77));
            assert_eq!(class.direct_methods().len(), 1);
            assert_eq!(class.direct_methods()[0], method.into());
        });
    }

    #[test]
    fn next_method_descriptors_are_gc_objects() {
        crate::runtime::thread::Scheme::new_uninit().enter(|ctx| {
            let table = class_table(ctx);
            let class = table
                .register_dynamic(ctx, "receiver", ClassCategory::Scheme)
                .unwrap();
            let generic = GenericDescriptor::new(ctx, "describe", 1);
            let method = GenericDescriptor::add_method_in_table(
                ctx,
                &table,
                generic,
                &[class.id()],
                1,
                Value::new(77),
                MethodFlags::empty(),
            );
            let instance = SchemeInstance::allocate(ctx, class);
            let next_method =
                GenericDescriptor::next_method_chain(ctx, generic, &[instance.into()]).unwrap();

            assert_eq!(
                next_method.as_gcobj().header().class_id(),
                id(builtin_class_ids::NEXT_METHOD)
            );
            assert!(Value::from(next_method).is::<NextMethodDescriptor>());
            assert_eq!(next_method.generic().as_gcobj(), generic.as_gcobj());
            assert_eq!(next_method.methods().len(), 1);
            assert_eq!(next_method.method().as_gcobj(), method.as_gcobj());
            assert_eq!(next_method.body(), Value::new(77));
            assert_eq!(next_method.args().len(), 1);
            assert_eq!(next_method.index(), 0);
            assert!(!next_method.has_next());
            assert!(next_method.next(ctx).is_none());
        });
    }

    #[test]
    fn generic_slow_dispatch_selects_most_specific_method() {
        crate::runtime::thread::Scheme::new_uninit().enter(|ctx| {
            let table = class_table(ctx);
            let base = table
                .register_dynamic(ctx, "base", ClassCategory::Scheme)
                .unwrap();
            let derived = table
                .register_dynamic_with_slots(
                    ctx,
                    "derived",
                    ClassCategory::Scheme,
                    &[base.id()],
                    &[],
                )
                .unwrap();
            let generic = GenericDescriptor::new(ctx, "classify", 1);
            let base_method = GenericDescriptor::add_method_in_table(
                ctx,
                &table,
                generic,
                &[base.id()],
                1,
                Value::new(10),
                MethodFlags::empty(),
            );
            let derived_method = GenericDescriptor::add_method_in_table(
                ctx,
                &table,
                generic,
                &[derived.id()],
                1,
                Value::new(20),
                MethodFlags::empty(),
            );
            let base_instance = SchemeInstance::allocate(ctx, base);
            let derived_instance = SchemeInstance::allocate(ctx, derived);

            assert_eq!(
                generic
                    .select_method(ctx, &[base_instance.into()])
                    .unwrap()
                    .body(),
                base_method.body()
            );
            assert_eq!(
                generic
                    .select_method(ctx, &[derived_instance.into()])
                    .unwrap()
                    .body(),
                derived_method.body()
            );
            assert_eq!(base.direct_methods().len(), 1);
            assert_eq!(derived.direct_methods().len(), 1);
        });
    }

    #[test]
    fn next_method_chain_orders_applicable_methods_by_specificity() {
        crate::runtime::thread::Scheme::new_uninit().enter(|ctx| {
            let generic = GenericDescriptor::new(ctx, "classify", 1);
            let number_method = GenericDescriptor::add_method(
                ctx,
                generic,
                &[id(builtin_class_ids::NUMBER)],
                1,
                Value::new(10),
            );
            let fixnum_method = GenericDescriptor::add_method(
                ctx,
                generic,
                &[id(builtin_class_ids::FIXNUM)],
                1,
                Value::new(20),
            );
            let arg = Value::new(1);

            let chain = GenericDescriptor::next_method_chain(ctx, generic, &[arg]).unwrap();
            assert_eq!(chain.methods().len(), 2);
            assert_eq!(chain.method().as_gcobj(), fixnum_method.as_gcobj());
            assert_eq!(chain.body(), Value::new(20));
            assert!(chain.has_next());

            let next = chain.next(ctx).unwrap();
            assert_eq!(next.methods().len(), 2);
            assert_eq!(next.method().as_gcobj(), number_method.as_gcobj());
            assert_eq!(next.body(), Value::new(10));
            assert_eq!(next.index(), 1);
            assert!(!next.has_next());
        });
    }

    #[test]
    fn generic_dispatcher_cache_stores_applicable_method_chain() {
        crate::runtime::thread::Scheme::new_uninit().enter(|ctx| {
            let generic = GenericDescriptor::new(ctx, "classify", 1);
            let number_method = GenericDescriptor::add_method(
                ctx,
                generic,
                &[id(builtin_class_ids::NUMBER)],
                1,
                Value::new(10),
            );
            let fixnum_method = GenericDescriptor::add_method(
                ctx,
                generic,
                &[id(builtin_class_ids::FIXNUM)],
                1,
                Value::new(20),
            );

            assert!(generic.dispatcher_cache().is_empty());
            let first =
                GenericDescriptor::next_method_chain(ctx, generic, &[Value::new(1)]).unwrap();
            assert_eq!(first.methods().len(), 2);
            assert_eq!(generic.dispatcher_cache().len(), 1);

            let entry = generic.dispatcher_cache()[0];
            assert_eq!(
                entry.class_ids().as_slice(),
                &[id(builtin_class_ids::FIXNUM)]
            );
            assert_eq!(entry.methods()[0].as_gcobj(), fixnum_method.as_gcobj());
            assert_eq!(entry.methods()[1].as_gcobj(), number_method.as_gcobj());

            let second =
                GenericDescriptor::next_method_chain(ctx, generic, &[Value::new(2)]).unwrap();
            assert_eq!(generic.dispatcher_cache().len(), 1);
            assert_eq!(second.methods().as_gcobj(), entry.methods().as_gcobj());
        });
    }

    #[test]
    fn generic_dispatcher_cache_is_cleared_when_methods_change() {
        crate::runtime::thread::Scheme::new_uninit().enter(|ctx| {
            let generic = GenericDescriptor::new(ctx, "classify", 1);
            GenericDescriptor::add_method(
                ctx,
                generic,
                &[id(builtin_class_ids::NUMBER)],
                1,
                Value::new(10),
            );
            let first =
                GenericDescriptor::next_method_chain(ctx, generic, &[Value::new(1)]).unwrap();
            assert_eq!(first.body(), Value::new(10));
            assert_eq!(generic.dispatcher_cache().len(), 1);

            GenericDescriptor::add_method(
                ctx,
                generic,
                &[id(builtin_class_ids::FIXNUM)],
                1,
                Value::new(20),
            );
            assert!(generic.dispatcher_cache().is_empty());

            let second =
                GenericDescriptor::next_method_chain(ctx, generic, &[Value::new(1)]).unwrap();
            assert_eq!(second.body(), Value::new(20));
            assert_eq!(generic.dispatcher_cache().len(), 1);

            let replacement = GenericDescriptor::add_method(
                ctx,
                generic,
                &[id(builtin_class_ids::FIXNUM)],
                1,
                Value::new(30),
            );
            assert!(generic.dispatcher_cache().is_empty());
            assert_eq!(replacement.body(), Value::new(30));

            let replaced =
                GenericDescriptor::next_method_chain(ctx, generic, &[Value::new(1)]).unwrap();
            assert_eq!(replaced.body(), Value::new(30));
        });
    }

    #[test]
    fn class_redefinition_preserves_direct_methods_and_clears_generic_cache() {
        crate::runtime::thread::Scheme::new_uninit().enter(|ctx| {
            let table = class_table(ctx);
            let class = table
                .register_dynamic_with_slots(
                    ctx,
                    "receiver",
                    ClassCategory::Scheme,
                    &[],
                    &[SlotSpec::mutable("old")],
                )
                .unwrap();
            let generic = GenericDescriptor::new(ctx, "describe", 1);
            let method = GenericDescriptor::add_method_in_table(
                ctx,
                &table,
                generic,
                &[class.id()],
                1,
                Value::new(77),
                MethodFlags::empty(),
            );
            let instance = SchemeInstance::allocate(ctx, class);

            let selected =
                GenericDescriptor::next_method_chain(ctx, generic, &[instance.into()]).unwrap();
            assert_eq!(selected.body(), Value::new(77));
            assert_eq!(generic.dispatcher_cache().len(), 1);

            let redefined = table
                .redefine_dynamic_with_slots(
                    ctx,
                    class.id(),
                    "receiver-v2",
                    ClassCategory::Scheme,
                    &[],
                    &[SlotSpec::mutable("new")],
                )
                .unwrap();

            assert_eq!(redefined.id(), class.id());
            assert_eq!(redefined.name(), "receiver-v2");
            assert_eq!(redefined.slot_count(), 1);
            assert_eq!(redefined.slots()[0].name(), "new");
            assert_eq!(redefined.direct_methods().len(), 1);
            assert_eq!(redefined.direct_methods()[0], method.into());
            assert!(generic.dispatcher_cache().is_empty());
            assert_eq!(
                table.lookup(class.id()).unwrap().as_gcobj(),
                redefined.as_gcobj()
            );

            let replacement_instance = SchemeInstance::allocate(ctx, redefined);
            let selected =
                GenericDescriptor::next_method_chain(ctx, generic, &[replacement_instance.into()])
                    .unwrap();
            assert_eq!(selected.body(), Value::new(77));
            assert_eq!(generic.dispatcher_cache().len(), 1);
        });
    }

    #[test]
    fn class_redefinition_touches_stale_scheme_instances_by_slot_name() {
        crate::runtime::thread::Scheme::new_uninit().enter(|ctx| {
            let table = ClassTable::with_max_id(ctx, 512);
            let class = table
                .register_dynamic_with_slots(
                    ctx,
                    "record",
                    ClassCategory::Scheme,
                    &[],
                    &[
                        SlotSpec::mutable("kept").with_init_value(Value::new(1)),
                        SlotSpec::mutable("removed").with_init_value(Value::new(2)),
                    ],
                )
                .unwrap();
            let kept = class.accessor_named("kept").unwrap();
            let removed = class.accessor_named("removed").unwrap();
            let instance = SchemeInstance::allocate(ctx, class);
            SchemeInstance::set_slot_by_accessor(ctx, instance, kept, Value::new(10)).unwrap();
            SchemeInstance::set_slot_by_accessor(ctx, instance, removed, Value::new(20)).unwrap();

            let redefined = table
                .redefine_dynamic_with_slots(
                    ctx,
                    class.id(),
                    "record-v2",
                    ClassCategory::Scheme,
                    &[],
                    &[
                        SlotSpec::mutable("added").with_init_value(Value::new(30)),
                        SlotSpec::mutable("kept").with_init_value(Value::new(100)),
                    ],
                )
                .unwrap();
            let added = redefined.accessor_named("added").unwrap();
            let kept_after_redefinition = redefined.accessor_named("kept").unwrap();

            assert_eq!(instance.class().as_gcobj(), class.as_gcobj());
            assert!(SchemeInstance::touch_in_table(ctx, &table, instance));
            assert_eq!(instance.class().as_gcobj(), redefined.as_gcobj());
            assert_eq!(instance.slot_count(), redefined.slot_count());
            assert_eq!(instance.slot_by_accessor(added), Ok(Value::new(30)));
            assert_eq!(
                instance.slot_by_accessor(kept_after_redefinition),
                Ok(Value::new(10))
            );
            assert!(redefined.accessor_named("removed").is_none());
        });
    }

    #[test]
    fn scheme_instance_change_class_updates_header_and_preserves_named_slots() {
        crate::runtime::thread::Scheme::new_uninit().enter(|ctx| {
            let table = ClassTable::with_max_id(ctx, 512);
            let source = table
                .register_dynamic_with_slots(
                    ctx,
                    "source",
                    ClassCategory::Scheme,
                    &[],
                    &[
                        SlotSpec::mutable("kept").with_init_value(Value::new(1)),
                        SlotSpec::mutable("removed").with_init_value(Value::new(2)),
                    ],
                )
                .unwrap();
            let target = table
                .register_dynamic_with_slots(
                    ctx,
                    "target",
                    ClassCategory::Scheme,
                    &[],
                    &[
                        SlotSpec::mutable("added").with_init_value(Value::new(30)),
                        SlotSpec::mutable("kept").with_init_value(Value::new(100)),
                    ],
                )
                .unwrap();
            let instance = SchemeInstance::allocate(ctx, source);
            let kept = source.accessor_named("kept").unwrap();
            SchemeInstance::set_slot_by_accessor(ctx, instance, kept, Value::new(10)).unwrap();

            SchemeInstance::change_class(ctx, instance, target);

            assert_eq!(instance.as_gcobj().header().class_id(), target.id());
            assert_eq!(instance.class().as_gcobj(), target.as_gcobj());
            assert_eq!(instance.slot_count(), target.slot_count());
            assert_eq!(
                instance.slot_by_accessor(target.accessor_named("added").unwrap()),
                Ok(Value::new(30))
            );
            assert_eq!(
                instance.slot_by_accessor(target.accessor_named("kept").unwrap()),
                Ok(Value::new(10))
            );
            assert!(target.accessor_named("removed").is_none());
        });
    }

    #[test]
    fn class_redefinition_rejects_sealed_classes() {
        crate::runtime::thread::Scheme::new_uninit().enter(|ctx| {
            let table = ClassTable::with_max_id(ctx, 512);
            let class = ClassDescriptor::new(ctx, id(1), "pair", ClassCategory::Builtin);
            table.register_builtin(ctx, class).unwrap();

            let err = table
                .redefine_dynamic_with_slots(
                    ctx,
                    id(1),
                    "pair-v2",
                    ClassCategory::Builtin,
                    &[],
                    &[],
                )
                .unwrap_err();

            assert_eq!(err, ClassTableError::NotMalleable(id(1)));
        });
    }

    #[test]
    fn scheme_class_sealing_toggles_malleability() {
        crate::runtime::thread::Scheme::new_uninit().enter(|ctx| {
            let table = ClassTable::with_max_id(ctx, 512);
            let class = table
                .register_dynamic_with_slots(ctx, "record", ClassCategory::Scheme, &[], &[])
                .unwrap();

            assert!(class.flags().malleable());
            assert!(!class.flags().sealed());

            class.seal();
            assert!(!class.flags().malleable());
            assert!(class.flags().sealed());
            assert_eq!(
                table
                    .redefine_dynamic_with_slots(
                        ctx,
                        class.id(),
                        "record-v2",
                        ClassCategory::Scheme,
                        &[],
                        &[],
                    )
                    .unwrap_err(),
                ClassTableError::NotMalleable(class.id())
            );

            assert!(class.unseal());
            assert!(class.flags().malleable());
            assert!(!class.flags().sealed());
            table
                .redefine_dynamic_with_slots(
                    ctx,
                    class.id(),
                    "record-v2",
                    ClassCategory::Scheme,
                    &[],
                    &[],
                )
                .unwrap();
        });
    }

    #[test]
    fn class_redefinition_epoch_tracks_successful_replacements() {
        crate::runtime::thread::Scheme::new_uninit().enter(|ctx| {
            let table = ClassTable::with_max_id(ctx, 512);
            let class = table
                .register_dynamic_with_slots(ctx, "record", ClassCategory::Scheme, &[], &[])
                .unwrap();

            assert_eq!(table.redefinition_epoch(), 0);

            class.seal();
            assert_eq!(
                table
                    .redefine_dynamic_with_slots(
                        ctx,
                        class.id(),
                        "record-v2",
                        ClassCategory::Scheme,
                        &[],
                        &[],
                    )
                    .unwrap_err(),
                ClassTableError::NotMalleable(class.id())
            );
            assert_eq!(table.redefinition_epoch(), 0);

            class.unseal();
            table
                .redefine_dynamic_with_slots(
                    ctx,
                    class.id(),
                    "record-v2",
                    ClassCategory::Scheme,
                    &[],
                    &[],
                )
                .unwrap();
            assert_eq!(table.redefinition_epoch(), 1);
        });
    }

    #[test]
    fn builtin_class_unseal_does_not_make_class_malleable() {
        crate::runtime::thread::Scheme::new_uninit().enter(|ctx| {
            let builtin = ClassDescriptor::new(ctx, id(1), "pair", ClassCategory::Builtin);
            let table = ClassTable::with_max_id(ctx, 512);
            table.register_builtin(ctx, builtin).unwrap();
            let class = table.lookup(id(1)).unwrap();

            assert!(class.flags().sealed());
            assert!(!class.unseal());
            assert!(class.flags().sealed());
            assert!(!class.flags().malleable());
        });
    }

    #[test]
    fn generic_invocation_prepares_method_body_with_next_method_arg() {
        crate::runtime::thread::Scheme::new_uninit().enter(|ctx| {
            let generic = GenericDescriptor::new(ctx, "classify", 1);
            let method = GenericDescriptor::add_method(
                ctx,
                generic,
                &[id(builtin_class_ids::FIXNUM)],
                1,
                Value::new(20),
            );
            let arg = Value::new(1);

            let invocation = GenericDescriptor::invocation(ctx, generic, &[arg]).unwrap();
            let invoke_args = invocation.args();
            let next_method = invoke_args[0].try_as::<NextMethodDescriptor>().unwrap();

            assert_eq!(invocation.body(), method.body());
            assert_eq!(invoke_args.len(), 2);
            assert_eq!(invoke_args[1], arg);
            assert_eq!(next_method.body(), method.body());
            assert_eq!(next_method.args()[0], arg);
        });
    }

    #[test]
    fn generic_invocation_prepares_fallback_with_original_args() {
        crate::runtime::thread::Scheme::new_uninit().enter(|ctx| {
            let generic = GenericDescriptor::new(ctx, "fallback", 1);
            let arg = Value::new(1);

            GenericDescriptor::set_fallback(ctx, generic, Value::new(90));

            let invocation = GenericDescriptor::invocation(ctx, generic, &[arg]).unwrap();
            assert_eq!(invocation.body(), Value::new(90));
            assert_eq!(invocation.args().as_slice(), &[arg]);
        });
    }

    #[test]
    fn next_method_invocation_advances_to_remaining_chain() {
        crate::runtime::thread::Scheme::new_uninit().enter(|ctx| {
            let generic = GenericDescriptor::new(ctx, "classify", 1);
            let number_method = GenericDescriptor::add_method(
                ctx,
                generic,
                &[id(builtin_class_ids::NUMBER)],
                1,
                Value::new(10),
            );
            GenericDescriptor::add_method(
                ctx,
                generic,
                &[id(builtin_class_ids::FIXNUM)],
                1,
                Value::new(20),
            );
            let arg = Value::new(1);
            let chain = GenericDescriptor::next_method_chain(ctx, generic, &[arg]).unwrap();

            let (body, invoke_args) = NextMethodDescriptor::next_invocation(ctx, chain).unwrap();
            let next_method = invoke_args[0].try_as::<NextMethodDescriptor>().unwrap();

            assert_eq!(body, number_method.body());
            assert_eq!(invoke_args.len(), 2);
            assert_eq!(invoke_args[1], arg);
            assert_eq!(next_method.body(), number_method.body());
            assert!(!next_method.has_next());
        });
    }

    #[test]
    fn generic_add_method_replaces_existing_specializers() {
        crate::runtime::thread::Scheme::new_uninit().enter(|ctx| {
            let table = class_table(ctx);
            let class = table
                .register_dynamic(ctx, "receiver", ClassCategory::Scheme)
                .unwrap();
            let generic = GenericDescriptor::new(ctx, "describe", 1);
            let old_method = GenericDescriptor::add_method_in_table(
                ctx,
                &table,
                generic,
                &[class.id()],
                1,
                Value::new(11),
                MethodFlags::empty(),
            );
            let replacement = GenericDescriptor::add_method_in_table(
                ctx,
                &table,
                generic,
                &[class.id()],
                1,
                Value::new(12),
                MethodFlags::empty(),
            );
            let instance = SchemeInstance::allocate(ctx, class);

            assert_eq!(old_method.as_gcobj(), replacement.as_gcobj());
            assert_eq!(generic.methods().len(), 1);
            assert_eq!(class.direct_methods().len(), 1);
            assert_eq!(old_method.body(), Value::new(12));
            assert_eq!(
                generic
                    .select_method(ctx, &[instance.into()])
                    .unwrap()
                    .body(),
                Value::new(12)
            );
        });
    }

    #[test]
    fn generic_add_method_records_locked_flag() {
        crate::runtime::thread::Scheme::new_uninit().enter(|ctx| {
            let table = ClassTable::with_max_id(ctx, 512);
            let class = table
                .register_dynamic(ctx, "receiver", ClassCategory::Scheme)
                .unwrap();
            let generic = GenericDescriptor::new(ctx, "describe", 1);
            let method = GenericDescriptor::add_method_in_table(
                ctx,
                &table,
                generic,
                &[class.id()],
                1,
                Value::new(11),
                MethodFlags::locked_flag(),
            );

            assert!(method.flags().locked());
        });
    }

    #[test]
    fn generic_seal_flag_is_mutable() {
        crate::runtime::thread::Scheme::new_uninit().enter(|ctx| {
            let generic = GenericDescriptor::new(ctx, "sealed", 1);
            assert!(!generic.flags().sealed());

            GenericDescriptor::set_sealed(ctx, generic, true);
            assert!(generic.flags().sealed());

            GenericDescriptor::set_sealed(ctx, generic, false);
            assert!(!generic.flags().sealed());
        });
    }

    #[test]
    fn generic_dispatch_body_uses_gc_managed_fallback() {
        crate::runtime::thread::Scheme::new_uninit().enter(|ctx| {
            let table = ClassTable::with_max_id(ctx, 512);
            let class = table
                .register_dynamic(ctx, "receiver", ClassCategory::Scheme)
                .unwrap();
            let generic = GenericDescriptor::new(ctx, "describe", 1);
            let instance = SchemeInstance::allocate(ctx, class);

            assert_eq!(
                generic.dispatch_body(ctx, &[instance.into()]),
                Err(GenericDispatchError::NoApplicableMethod)
            );

            GenericDescriptor::set_fallback(ctx, generic, Value::new(90));
            assert_eq!(generic.fallback(), Value::new(90));
            assert_eq!(
                generic.dispatch_body(ctx, &[instance.into()]),
                Ok(Value::new(90))
            );

            GenericDescriptor::set_fallback(ctx, generic, Value::new(91));
            assert_eq!(
                generic.dispatch_body(ctx, &[instance.into()]),
                Ok(Value::new(91))
            );
        });
    }

    #[test]
    fn generic_dispatch_body_prefers_applicable_method_over_fallback() {
        crate::runtime::thread::Scheme::new_uninit().enter(|ctx| {
            let table = class_table(ctx);
            let class = table
                .register_dynamic(ctx, "receiver", ClassCategory::Scheme)
                .unwrap();
            let generic = GenericDescriptor::new(ctx, "describe", 1);
            let instance = SchemeInstance::allocate(ctx, class);

            GenericDescriptor::set_fallback(ctx, generic, Value::new(90));
            GenericDescriptor::add_method_in_table(
                ctx,
                &table,
                generic,
                &[class.id()],
                1,
                Value::new(12),
                MethodFlags::empty(),
            );

            assert_eq!(
                generic.dispatch_body(ctx, &[instance.into()]),
                Ok(Value::new(12))
            );
        });
    }

    #[test]
    fn generic_slow_dispatch_handles_multi_argument_specificity() {
        crate::runtime::thread::Scheme::new_uninit().enter(|ctx| {
            let table = class_table(ctx);
            let base = table
                .register_dynamic(ctx, "base", ClassCategory::Scheme)
                .unwrap();
            let derived = table
                .register_dynamic_with_slots(
                    ctx,
                    "derived",
                    ClassCategory::Scheme,
                    &[base.id()],
                    &[],
                )
                .unwrap();
            let generic = GenericDescriptor::new(ctx, "combine", 2);
            let generic_method = GenericDescriptor::add_method_in_table(
                ctx,
                &table,
                generic,
                &[base.id(), base.id()],
                2,
                Value::new(30),
                MethodFlags::empty(),
            );
            let second_specific = GenericDescriptor::add_method_in_table(
                ctx,
                &table,
                generic,
                &[base.id(), derived.id()],
                2,
                Value::new(40),
                MethodFlags::empty(),
            );
            let base_instance = SchemeInstance::allocate(ctx, base);
            let derived_instance = SchemeInstance::allocate(ctx, derived);

            assert_eq!(
                generic
                    .select_method(ctx, &[base_instance.into(), derived_instance.into()])
                    .unwrap()
                    .body(),
                second_specific.body()
            );
            match generic.select_method(ctx, &[base_instance.into()]) {
                Err(GenericDispatchError::Arity) => {}
                _ => panic!("short argument list should reject with arity error"),
            }
            match generic.select_method(ctx, &[Value::empty(), base_instance.into()]) {
                Err(GenericDispatchError::NoApplicableMethod) => {}
                _ => panic!("unclassified argument should leave no applicable method"),
            }
            assert_eq!(generic_method.body(), Value::new(30));
        });
    }

    #[test]
    fn dynamic_scheme_classes_compute_inherited_slots() {
        crate::runtime::thread::Scheme::new_uninit().enter(|ctx| {
            let table = ClassTable::with_max_id(ctx, 512);
            let base = table
                .register_dynamic_with_slots(
                    ctx,
                    "base",
                    ClassCategory::Scheme,
                    &[],
                    &[SlotSpec::mutable("base-slot").with_init_keyword(Value::new(301))],
                )
                .unwrap();
            let derived = table
                .register_dynamic_with_slots(
                    ctx,
                    "derived",
                    ClassCategory::Scheme,
                    &[base.id()],
                    &[SlotSpec::mutable("derived-slot").with_init_keyword(Value::new(302))],
                )
                .unwrap();

            let object = builtin_id(builtin_class_ids::OBJECT);
            assert_eq!(base.direct_supers(), &[object]);
            assert_eq!(base.cpl(), &[base.id(), object]);
            assert_eq!(derived.direct_supers(), &[base.id(), object]);
            assert_eq!(derived.cpl(), &[derived.id(), base.id(), object]);
            assert_eq!(derived.direct_slots().len(), 1);
            assert_eq!(derived.slots().len(), 2);
            assert_eq!(derived.slots()[0].name(), "base-slot");
            assert_eq!(derived.slots()[0].owner(), base.id());
            assert_eq!(derived.slots()[0].index(), 0);
            assert_eq!(derived.slots()[1].name(), "derived-slot");
            assert_eq!(derived.slots()[1].owner(), derived.id());
            assert_eq!(derived.slots()[1].index(), 1);
            assert_eq!(derived.initargs(), &[Value::new(301), Value::new(302)]);
            assert_eq!(
                derived.accessor_named("derived-slot").unwrap().slot_index(),
                1
            );
        });
    }

    #[test]
    fn type_class_allocations_are_registered_in_rooted_class_table() {
        crate::runtime::thread::Scheme::new_uninit().enter(|ctx| {
            let table = class_table(ctx);
            let value = Gc::new(*ctx, GcOnlyDummy { value: 7 });
            let class_id = value.as_gcobj().class_id();
            let class = table
                .lookup(class_id)
                .expect("type-class allocation should register class metadata into class table");
            let hooks = class
                .primitive_layout_hooks()
                .expect("registered type class should carry primitive layout hooks");

            assert_eq!(value.value, 7);
            assert!(class_id.bits() > builtin_class_ids::MAX);
            assert_eq!(class.id(), class_id);
            assert_eq!(class.category(), ClassCategory::Internal);
            assert_eq!(
                class.name(),
                AllocationHooksOf::<'static, GcOnlyDummy>::HOOKS.type_name
            );
            assert_eq!(
                hooks.type_name(),
                AllocationHooksOf::<'static, GcOnlyDummy>::HOOKS.type_name
            );
            assert_eq!(hooks.instance_size(), size_of::<GcOnlyDummy>());
        });
    }

    #[test]
    fn class_table_initialization_registers_pending_type_classes() {
        crate::runtime::thread::Scheme::new_uninit().enter(|ctx| {
            let hooks = AllocationHooksOf::<'static, PendingRegistrationDummy>::HOOKS;
            let class_id = crate::rsgc::object::allocate_class_id();
            crate::rsgc::object::record_pending_type_class(class_id, hooks);
            let table = ClassTable::new(ctx);

            assert!(table.lookup(class_id).is_none());
            register_pending_type_classes(ctx, &table);

            let class = table
                .lookup(class_id)
                .expect("pre-existing type classes should register into class table");
            let hooks = class
                .primitive_layout_hooks()
                .expect("registered type class should carry primitive layout hooks");

            assert_eq!(class.category(), ClassCategory::Internal);
            assert_eq!(
                class.name(),
                AllocationHooksOf::<'static, PendingRegistrationDummy>::HOOKS.type_name
            );
            assert_eq!(
                hooks.type_name(),
                AllocationHooksOf::<'static, PendingRegistrationDummy>::HOOKS.type_name
            );
        });
    }

    #[test]
    fn initialized_class_table_disables_pending_type_class_fallback() {
        crate::runtime::thread::Scheme::new_uninit().enter(|ctx| {
            let _table = class_table(ctx);
            let unregistered_id = crate::rsgc::object::allocate_class_id();

            assert!(_table.lookup(unregistered_id).is_none());
            assert!(primitive_layout_hooks_for_class_id(unregistered_id).is_none());
        });
    }

    #[test]
    fn type_classes_do_not_route_scheme_instance_operations() {
        crate::runtime::thread::Scheme::new_uninit().enter(|ctx| {
            let table = class_table(ctx);
            let value = Gc::new(*ctx, GcOnlyDummy { value: 7 });
            let raw_value = Value::from_raw(value.as_gcobj().to_address().as_usize() as u64);

            assert!(raw_value.is_cell());
            assert!(table.lookup(value.as_gcobj().class_id()).is_some());
            assert!(try_scheme_instance(ctx, raw_value).is_none());
            assert_eq!(hash_primitive_value(raw_value), None);
            assert_eq!(compare_primitive_values(raw_value, raw_value), None);
        });
    }

    #[test]
    fn class_descriptors_are_class_heap_objects() {
        crate::runtime::thread::Scheme::new_uninit().enter(|ctx| {
            let table = ClassTable::with_max_id(ctx, 512);
            let descriptor = table
                .register_dynamic(ctx, "user-class", ClassCategory::Scheme)
                .unwrap();

            assert_eq!(
                descriptor.as_gcobj().header().class_id(),
                id(builtin_class_ids::CLASS)
            );
            assert!(Value::from(descriptor).is::<ClassDescriptor>());
            let hooks = descriptor.primitive_layout_hooks().unwrap();
            assert_eq!(hooks.instance_size(), std::mem::size_of::<SchemeInstance>());
            assert_eq!(hooks.alignment(), std::mem::align_of::<SchemeInstance>());
            assert_eq!(
                hooks.type_name(),
                std::any::type_name::<SchemeInstance<'static>>()
            );
            assert_eq!(
                table
                    .lookup(descriptor.id())
                    .unwrap()
                    .primitive_layout_hooks()
                    .unwrap()
                    .instance_size(),
                std::mem::size_of::<SchemeInstance>()
            );
        });
    }

    #[test]
    fn scheme_instances_allocate_from_class_slots_and_accessors() {
        crate::runtime::thread::Scheme::new_uninit().enter(|ctx| {
            let table = ClassTable::with_max_id(ctx, 512);
            let class = table
                .register_dynamic_with_slots(
                    ctx,
                    "point",
                    ClassCategory::Scheme,
                    &[],
                    &[
                        SlotSpec::mutable("x").with_init_value(Value::new(3)),
                        SlotSpec::immutable("y").with_init_value(Value::new(4)),
                    ],
                )
                .unwrap();
            let x = class.accessor_named("x").unwrap();
            let y = class.accessor_named("y").unwrap();
            let instance = SchemeInstance::allocate(ctx, class);

            assert_eq!(instance.slot_count(), class.slot_count());
            assert_eq!(instance.slot_by_accessor(x), Ok(Value::new(3)));
            assert_eq!(instance.slot_by_accessor(y), Ok(Value::new(4)));

            SchemeInstance::set_slot_by_accessor(ctx, instance, x, Value::new(7)).unwrap();
            assert_eq!(instance.slot_by_accessor(x), Ok(Value::new(7)));
            assert_eq!(
                SchemeInstance::set_slot_by_accessor(ctx, instance, y, Value::new(8)),
                Err(SlotAccessError::Immutable)
            );
            assert_eq!(instance.slot_by_accessor(y), Ok(Value::new(4)));

            let stale = SlotAccessorDescriptor {
                slot_index: instance.slot_count(),
                ..x
            };
            assert_eq!(
                instance.slot_by_accessor(stale),
                Err(SlotAccessError::OutOfBounds)
            );
        });
    }

    #[test]
    fn scheme_instance_slots_without_init_values_stay_unbound() {
        crate::runtime::thread::Scheme::new_uninit().enter(|ctx| {
            let table = ClassTable::with_max_id(ctx, 512);
            let class = table
                .register_dynamic_with_slots(
                    ctx,
                    "partial",
                    ClassCategory::Scheme,
                    &[],
                    &[
                        SlotSpec::mutable("bound").with_init_value(Value::new(5)),
                        SlotSpec::mutable("unbound"),
                    ],
                )
                .unwrap();
            let bound = class.accessor_named("bound").unwrap();
            let unbound = class.accessor_named("unbound").unwrap();
            let instance = SchemeInstance::allocate(ctx, class);

            assert_eq!(instance.slot(unbound.slot_index()), Some(Value::empty()));
            assert_eq!(instance.slot_by_accessor(bound), Ok(Value::new(5)));
            assert_eq!(
                instance.slot_by_accessor(unbound),
                Err(SlotAccessError::Unbound)
            );
            assert_eq!(instance.slot_bound_by_accessor(bound), Ok(true));
            assert_eq!(instance.slot_bound_by_accessor(unbound), Ok(false));
            assert_eq!(instance.slot_bound_by_name("bound"), Ok(true));
            assert_eq!(instance.slot_bound_by_name("unbound"), Ok(false));
            assert!(instance.slot_is_bound(bound.slot_index()));
            assert!(!instance.slot_is_bound(unbound.slot_index()));

            let stale = SlotAccessorDescriptor {
                slot_index: instance.slot_count(),
                ..bound
            };
            assert_eq!(
                instance.slot_bound_by_accessor(stale),
                Err(SlotAccessError::OutOfBounds)
            );
        });
    }

    #[test]
    fn scheme_instance_slot_name_lookup_supports_missing_slot_fallback() {
        crate::runtime::thread::Scheme::new_uninit().enter(|ctx| {
            let table = ClassTable::with_max_id(ctx, 512);
            let class = table
                .register_dynamic_with_slots(
                    ctx,
                    "partial",
                    ClassCategory::Scheme,
                    &[],
                    &[
                        SlotSpec::mutable("bound").with_init_value(Value::new(5)),
                        SlotSpec::mutable("unbound"),
                    ],
                )
                .unwrap();
            let instance = SchemeInstance::allocate(ctx, class);

            assert_eq!(instance.slot_by_name("bound", None), Ok(Value::new(5)));
            assert_eq!(
                instance.slot_by_name("missing", None),
                Err(SlotAccessError::Unknown)
            );
            assert_eq!(
                instance.slot_by_name("missing", Some(Value::new(99))),
                Ok(Value::new(99))
            );
            assert_eq!(
                instance.slot_bound_by_name("missing"),
                Err(SlotAccessError::Unknown)
            );
            assert_eq!(
                instance.slot_by_name("unbound", Some(Value::new(99))),
                Err(SlotAccessError::Unbound)
            );
        });
    }

    #[test]
    fn scheme_instance_allocation_applies_initargs() {
        crate::runtime::thread::Scheme::new_uninit().enter(|ctx| {
            let table = ClassTable::with_max_id(ctx, 512);
            let x_keyword = Value::new(101);
            let y_keyword = Value::new(102);
            let class = table
                .register_dynamic_with_slots(
                    ctx,
                    "point",
                    ClassCategory::Scheme,
                    &[],
                    &[
                        SlotSpec::mutable("x")
                            .with_init_value(Value::new(1))
                            .with_init_keyword(x_keyword),
                        SlotSpec::mutable("y")
                            .with_init_value(Value::new(2))
                            .with_init_keyword(y_keyword),
                    ],
                )
                .unwrap();
            let x = class.accessor_named("x").unwrap();
            let y = class.accessor_named("y").unwrap();
            let instance =
                SchemeInstance::allocate_with_initargs(ctx, class, &[(x_keyword, Value::new(9))])
                    .unwrap();

            assert_eq!(class.slot_for_init_keyword(x_keyword).unwrap().name(), "x");
            assert_eq!(instance.slot_by_accessor(x), Ok(Value::new(9)));
            assert_eq!(instance.slot_by_accessor(y), Ok(Value::new(2)));
        });
    }

    #[test]
    fn scheme_instance_allocation_runs_init_thunks_for_defaulted_slots() {
        crate::runtime::thread::Scheme::new_uninit().enter(|ctx| {
            let table = ClassTable::with_max_id(ctx, 512);
            let x_keyword = Value::new(101);
            let thunk = crate::runtime::value::PROCEDURES
                .fetch(*ctx)
                .register_static_closure(ctx, constant_17_thunk, Value::null());
            let class = table
                .register_dynamic_with_slots(
                    ctx,
                    "point",
                    ClassCategory::Scheme,
                    &[],
                    &[
                        SlotSpec::mutable("x")
                            .with_init_value(Value::new(1))
                            .with_init_keyword(x_keyword)
                            .with_init_thunk(thunk.into()),
                        SlotSpec::mutable("y").with_init_value(Value::new(2)),
                    ],
                )
                .unwrap();
            let x = class.accessor_named("x").unwrap();
            let y = class.accessor_named("y").unwrap();

            let defaulted = SchemeInstance::allocate_with_initargs(ctx, class, &[]).unwrap();
            assert_eq!(defaulted.slot_by_accessor(x), Ok(Value::new(17)));
            assert_eq!(defaulted.slot_by_accessor(y), Ok(Value::new(2)));

            let explicit =
                SchemeInstance::allocate_with_initargs(ctx, class, &[(x_keyword, Value::new(9))])
                    .unwrap();
            assert_eq!(explicit.slot_by_accessor(x), Ok(Value::new(9)));
        });
    }

    #[test]
    fn scheme_class_operation_hooks_allocate_instances() {
        crate::runtime::thread::Scheme::new_uninit().enter(|ctx| {
            let table = ClassTable::with_max_id(ctx, 512);
            let x_keyword = Value::new(111);
            let class = table
                .register_dynamic_with_slots(
                    ctx,
                    "point",
                    ClassCategory::Scheme,
                    &[],
                    &[SlotSpec::mutable("x")
                        .with_init_value(Value::new(1))
                        .with_init_keyword(x_keyword)],
                )
                .unwrap();
            let x = class.accessor_named("x").unwrap();

            let value = class
                .allocate_instance(ctx, class, &[(x_keyword, Value::new(9))])
                .expect("Scheme classes should carry an allocate operation hook")
                .unwrap();
            assert_eq!(value.class_id(), Some(class.id()));
            // SAFETY: the dynamic class ID check above proves the Scheme-instance payload layout.
            let instance: Gc<'_, SchemeInstance<'_>> =
                unsafe { Gc::from_gcobj(value.as_cell_raw()) };

            assert_eq!(instance.class().id(), class.id());
            assert_eq!(instance.slot_by_accessor(x), Ok(Value::new(9)));
        });
    }

    #[test]
    fn scheme_class_operation_hooks_print_instances() {
        crate::runtime::thread::Scheme::new_uninit().enter(|ctx| {
            let table = class_table(ctx);
            let class = table
                .register_dynamic_with_slots(ctx, "point", ClassCategory::Scheme, &[], &[])
                .unwrap();
            let instance = SchemeInstance::allocate(ctx, class);
            let printed = format!("{}", Value::from(instance));

            assert!(printed.starts_with("#<instance point "));
            assert!(printed.ends_with(")>"));
        });
    }

    #[test]
    fn scheme_class_operation_hooks_hash_instances() {
        #[derive(Default)]
        struct OneU64Hasher(u64);

        impl std::hash::Hasher for OneU64Hasher {
            fn finish(&self) -> u64 {
                self.0
            }

            fn write(&mut self, bytes: &[u8]) {
                for byte in bytes {
                    self.0 = self.0.wrapping_mul(257).wrapping_add(u64::from(*byte));
                }
            }

            fn write_u64(&mut self, i: u64) {
                self.0 = i;
            }
        }

        crate::runtime::thread::Scheme::new_uninit().enter(|ctx| {
            let table = class_table(ctx);
            let class = table
                .register_dynamic_with_slots(ctx, "point", ClassCategory::Scheme, &[], &[])
                .unwrap();
            let instance = SchemeInstance::allocate(ctx, class);
            let value = Value::from(instance);
            let expected = instance.as_gcobj().hashcode();
            let hook = class.primitive_operation_hooks().hash().unwrap();

            assert_eq!(hook(class, value), expected);

            let mut hasher = OneU64Hasher::default();
            std::hash::Hash::hash(&value, &mut hasher);
            assert_eq!(std::hash::Hasher::finish(&hasher), expected);
        });
    }

    #[test]
    fn scheme_class_operation_hooks_compare_instances() {
        crate::runtime::thread::Scheme::new_uninit().enter(|ctx| {
            let table = class_table(ctx);
            let class = table
                .register_dynamic_with_slots(ctx, "point", ClassCategory::Scheme, &[], &[])
                .unwrap();
            let lhs = Value::from(SchemeInstance::allocate(ctx, class));
            let rhs = Value::from(SchemeInstance::allocate(ctx, class));
            let hook = class.primitive_operation_hooks().compare().unwrap();

            assert_eq!(hook(class, lhs, lhs), Some(true));
            assert_eq!(hook(class, lhs, rhs), Some(false));
            assert!(lhs.eqv(lhs));
            assert!(!lhs.eqv(rhs));
        });
    }

    #[test]
    fn scheme_class_operation_hooks_access_slots() {
        crate::runtime::thread::Scheme::new_uninit().enter(|ctx| {
            let table = class_table(ctx);
            let reader = crate::runtime::value::PROCEDURES
                .fetch(*ctx)
                .register_static_closure(ctx, slot_reader_proc, Value::null());
            let writer = crate::runtime::value::PROCEDURES
                .fetch(*ctx)
                .register_static_closure(ctx, slot_writer_proc, Value::null());
            let bound = crate::runtime::value::PROCEDURES
                .fetch(*ctx)
                .register_static_closure(ctx, slot_bound_proc, Value::null());
            let class = table
                .register_dynamic_with_slots(
                    ctx,
                    "point",
                    ClassCategory::Scheme,
                    &[],
                    &[
                        SlotSpec::mutable("x").with_init_value(Value::new(4)),
                        SlotSpec::mutable("unset"),
                        SlotSpec::mutable("native").with_native_slot_hooks(
                            Some(test_slot_ref_hook),
                            Some(test_slot_set_hook),
                            Some(test_slot_bound_hook),
                        ),
                        SlotSpec::mutable("procedure")
                            .with_init_value(Value::new(4))
                            .with_scheme_slot_procedures(reader.into(), writer.into(), bound.into())
                            .with_native_slot_hooks(
                                Some(test_slot_ref_hook),
                                Some(test_slot_set_hook),
                                Some(test_slot_bound_hook),
                            ),
                    ],
                )
                .unwrap();
            let instance = SchemeInstance::allocate(ctx, class);
            let receiver = Value::from(instance);

            assert_eq!(
                class
                    .slot_value_by_name(ctx, class, receiver, "x", None)
                    .unwrap(),
                Ok(Value::new(4))
            );
            assert_eq!(
                class.slot_bound_by_name(ctx, class, receiver, "x").unwrap(),
                Ok(true)
            );
            assert_eq!(
                class
                    .slot_bound_by_name(ctx, class, receiver, "unset")
                    .unwrap(),
                Ok(false)
            );
            assert_eq!(
                class
                    .set_slot_value_by_name(ctx, class, receiver, "unset", Value::new(6))
                    .unwrap(),
                Ok(())
            );
            assert_eq!(
                class
                    .slot_value_by_name(ctx, class, receiver, "unset", None)
                    .unwrap(),
                Ok(Value::new(6))
            );
            assert_eq!(
                class
                    .slot_value_by_name(ctx, class, receiver, "missing", Some(Value::new(9)))
                    .unwrap(),
                Ok(Value::new(9))
            );
            assert_eq!(
                class
                    .set_slot_value_by_name(ctx, class, receiver, "missing", Value::new(10))
                    .unwrap(),
                Err(SlotAccessError::Unknown)
            );
            assert_eq!(
                class
                    .slot_value_by_name(ctx, class, receiver, "native", None)
                    .unwrap(),
                Ok(Value::new(901))
            );
            assert_eq!(
                class
                    .slot_bound_by_name(ctx, class, receiver, "native")
                    .unwrap(),
                Ok(true)
            );
            assert_eq!(
                class
                    .set_slot_value_by_name(ctx, class, receiver, "native", Value::new(902))
                    .unwrap(),
                Ok(())
            );
            assert_eq!(
                class
                    .set_slot_value_by_name(ctx, class, receiver, "native", Value::new(903))
                    .unwrap(),
                Err(SlotAccessError::Immutable)
            );
            assert_eq!(
                class
                    .slot_value_by_name(ctx, class, receiver, "procedure", None)
                    .unwrap(),
                Ok(Value::new(911))
            );
            assert_eq!(
                class
                    .slot_bound_by_name(ctx, class, receiver, "procedure")
                    .unwrap(),
                Ok(true)
            );
            assert_eq!(
                class
                    .set_slot_value_by_name(ctx, class, receiver, "procedure", Value::new(912))
                    .unwrap(),
                Ok(())
            );
            assert_eq!(
                instance.slot_by_accessor(class.accessor_named("procedure").unwrap()),
                Ok(Value::new(4)),
                "Scheme accessor procedures override native/raw slot storage"
            );
        });
    }

    #[test]
    fn scheme_instance_initargs_reject_unknown_or_uninitializable_slots() {
        crate::runtime::thread::Scheme::new_uninit().enter(|ctx| {
            let table = ClassTable::with_max_id(ctx, 512);
            let x_keyword = Value::new(201);
            let locked_keyword = Value::new(202);
            let missing_keyword = Value::new(203);
            let class = table
                .register_dynamic_with_slots(
                    ctx,
                    "point",
                    ClassCategory::Scheme,
                    &[],
                    &[
                        SlotSpec::mutable("x").with_init_keyword(x_keyword),
                        SlotSpec::mutable("locked")
                            .with_init_keyword(locked_keyword)
                            .without_initargs(),
                    ],
                )
                .unwrap();

            match SchemeInstance::allocate_with_initargs(
                ctx,
                class,
                &[(missing_keyword, Value::new(1))],
            ) {
                Err(SlotInitError::UnknownKeyword(keyword)) => {
                    assert_eq!(keyword, missing_keyword);
                }
                _ => panic!("unknown init keyword should be rejected"),
            }
            match SchemeInstance::allocate_with_initargs(
                ctx,
                class,
                &[(locked_keyword, Value::new(1))],
            ) {
                Err(SlotInitError::NotInitializable(keyword)) => {
                    assert_eq!(keyword, locked_keyword);
                }
                _ => panic!("non-initializable slot should reject initarg"),
            }
            assert!(class.slot_for_init_keyword(Value::empty()).is_none());
        });
    }

    #[test]
    fn scheme_instance_allocation_reports_init_thunk_failure() {
        crate::runtime::thread::Scheme::new_uninit().enter(|ctx| {
            let table = ClassTable::with_max_id(ctx, 512);
            let bad_thunk = Value::new(44);
            let class = table
                .register_dynamic_with_slots(
                    ctx,
                    "bad-default",
                    ClassCategory::Scheme,
                    &[],
                    &[SlotSpec::mutable("x").with_init_thunk(bad_thunk)],
                )
                .unwrap();

            match SchemeInstance::allocate_with_initargs(ctx, class, &[]) {
                Err(SlotInitError::InitThunkFailed(error)) => {
                    assert_eq!(error, bad_thunk);
                }
                _ => panic!("non-callable init thunk should be reported as a thunk failure"),
            }
        });
    }

    #[test]
    fn scheme_instances_use_dynamic_class_id_and_gc_slots() {
        crate::runtime::thread::Scheme::new_uninit().enter(|ctx| {
            let table = class_table(ctx);
            let class = table
                .register_dynamic(ctx, "user-class", ClassCategory::Scheme)
                .unwrap();
            let instance = SchemeInstance::new(ctx, class, 2);

            assert_eq!(instance.as_gcobj().header().class_id(), class.id());
            assert_eq!(instance.class().id(), class.id());
            assert_eq!(instance.slot_count(), 2);
            assert_eq!(instance.slot(0), Some(Value::empty()));
            assert!(!instance.slot_is_bound(0));

            SchemeInstance::set_slot(ctx, instance, 0, Value::new(42));

            assert_eq!(instance.slot(0), Some(Value::new(42)));
            assert!(instance.slot_is_bound(0));

            let value: Value = instance.into();
            assert_eq!(value.class_id(), Some(class.id()));
            assert!(value.is_a(ctx, class.id()));
        });
    }

    #[test]
    fn scheme_instance_dynamic_values_can_be_recovered_for_slot_ops() {
        crate::runtime::thread::Scheme::new_uninit().enter(|ctx| {
            let table = class_table(ctx);
            let class = table
                .register_dynamic_with_slots(
                    ctx,
                    "point",
                    ClassCategory::Scheme,
                    &[],
                    &[SlotSpec::mutable("x").with_init_value(Value::new(4))],
                )
                .unwrap();
            let instance = SchemeInstance::allocate(ctx, class);
            let value: Value = instance.into();
            let recovered = try_scheme_instance(ctx, value).unwrap();

            assert_eq!(recovered.class().id(), class.id());
            assert_eq!(
                recovered.slot_by_accessor(class.accessor_named("x").unwrap()),
                Ok(Value::new(4))
            );
            assert!(try_scheme_instance(ctx, Value::new(17)).is_none());
        });
    }

    #[test]
    fn scheme_class_registration_accepts_symbol_slot_list() {
        crate::runtime::thread::Scheme::new_uninit().enter(|ctx| {
            let table = ClassTable::with_max_id(ctx, 512);
            let name = crate::runtime::value::Symbol::from_str(ctx, "point");
            let x = crate::runtime::value::Symbol::from_str(ctx, "x");
            let y = crate::runtime::value::Symbol::from_str(ctx, "y");
            let slots = Value::cons(ctx, x.into(), Value::cons(ctx, y.into(), Value::null()));
            let class = table
                .register_scheme_class_from_slot_list(ctx, name, slots, Value::null())
                .unwrap();

            assert_eq!(class.name(), "point");
            assert_eq!(class.category(), ClassCategory::Scheme);
            assert_eq!(
                class.direct_supers(),
                &[builtin_id(builtin_class_ids::OBJECT)]
            );
            assert_eq!(
                class.cpl(),
                &[class.id(), builtin_id(builtin_class_ids::OBJECT)]
            );
            assert_eq!(class.direct_slots().len(), 2);
            assert_eq!(class.slots()[0].name(), "x");
            assert_eq!(class.slots()[1].name(), "y");
            assert_eq!(class.accessor_named("x").unwrap().slot_index(), 0);
            assert_eq!(class.accessor_named("y").unwrap().slot_index(), 1);
            assert_eq!(table.lookup(class.id()).unwrap().id(), class.id());
            assert_eq!(
                class
                    .slot_for_init_keyword(symbol(ctx, "x"))
                    .unwrap()
                    .name(),
                "x"
            );

            let instance = SchemeInstance::allocate(ctx, class);
            SchemeInstance::set_slot_by_accessor(
                ctx,
                instance,
                class.accessor_named("x").unwrap(),
                Value::new(10),
            )
            .unwrap();
            assert_eq!(
                instance.slot_by_accessor(class.accessor_named("x").unwrap()),
                Ok(Value::new(10))
            );

            let initialized = SchemeInstance::allocate_with_initargs(
                ctx,
                class,
                &[(symbol(ctx, "y"), Value::new(11))],
            )
            .unwrap();
            assert_eq!(
                initialized.slot_by_accessor(class.accessor_named("y").unwrap()),
                Ok(Value::new(11))
            );
        });
    }

    #[test]
    fn scheme_class_registration_accepts_direct_supers() {
        crate::runtime::thread::Scheme::new_uninit().enter(|ctx| {
            let table = ClassTable::with_max_id(ctx, 512);
            let base_name = crate::runtime::value::Symbol::from_str(ctx, "base");
            let base_slot = crate::runtime::value::Symbol::from_str(ctx, "base-slot");
            let base_slots = Value::cons(ctx, base_slot.into(), Value::null());
            let base = table
                .register_scheme_class_from_slot_list(ctx, base_name, base_slots, Value::null())
                .unwrap();

            let derived_name = crate::runtime::value::Symbol::from_str(ctx, "derived");
            let derived_slot = crate::runtime::value::Symbol::from_str(ctx, "derived-slot");
            let derived_slots = Value::cons(ctx, derived_slot.into(), Value::null());
            let direct_supers = Value::cons(ctx, base.into(), Value::null());
            let derived = table
                .register_scheme_class_from_slot_list(
                    ctx,
                    derived_name,
                    derived_slots,
                    direct_supers,
                )
                .unwrap();

            let object = builtin_id(builtin_class_ids::OBJECT);
            assert_eq!(base.direct_supers(), &[object]);
            assert_eq!(base.cpl(), &[base.id(), object]);
            assert_eq!(derived.direct_supers(), &[base.id(), object]);
            assert_eq!(derived.cpl(), &[derived.id(), base.id(), object]);
            assert!(derived.is_a(base.id()));
            assert!(derived.is_a(object));
            assert_eq!(derived.direct_slots().len(), 1);
            assert_eq!(derived.slots().len(), 2);
            assert_eq!(derived.slots()[0].name(), "base-slot");
            assert_eq!(derived.slots()[1].name(), "derived-slot");
            assert_eq!(derived.accessor_named("base-slot").unwrap().slot_index(), 0);
            assert_eq!(
                derived.accessor_named("derived-slot").unwrap().slot_index(),
                1
            );

            let instance = SchemeInstance::allocate_with_initargs(
                ctx,
                derived,
                &[
                    (symbol(ctx, "base-slot"), Value::new(10)),
                    (symbol(ctx, "derived-slot"), Value::new(11)),
                ],
            )
            .unwrap();
            assert_eq!(
                instance.slot_by_accessor(derived.accessor_named("base-slot").unwrap()),
                Ok(Value::new(10))
            );
            assert_eq!(
                instance.slot_by_accessor(derived.accessor_named("derived-slot").unwrap()),
                Ok(Value::new(11))
            );
        });
    }

    #[test]
    fn scheme_class_registration_rejects_bad_slot_lists() {
        crate::runtime::thread::Scheme::new_uninit().enter(|ctx| {
            let table = ClassTable::with_max_id(ctx, 512);
            let name = crate::runtime::value::Symbol::from_str(ctx, "bad");
            let slot = crate::runtime::value::Symbol::from_str(ctx, "slot");
            let improper = Value::cons(ctx, slot.into(), Value::new(1));

            match table.register_scheme_class_from_slot_list(ctx, name, improper, Value::null()) {
                Err(SchemeClassSpecError::SlotsNotList(value)) => assert_eq!(value, Value::new(1)),
                _ => panic!("improper slot-name list should be rejected"),
            }
            match table.register_scheme_class_from_slot_list(
                ctx,
                name,
                Value::cons(ctx, Value::new(2), Value::null()),
                Value::null(),
            ) {
                Err(SchemeClassSpecError::SlotNameNotSymbol(value)) => {
                    assert_eq!(value, Value::new(2));
                }
                _ => panic!("non-symbol slot name should be rejected"),
            }
            let super_name = crate::runtime::value::Symbol::from_str(ctx, "super");
            let valid_super = table
                .register_scheme_class_from_slot_list(ctx, super_name, Value::null(), Value::null())
                .unwrap();
            match table.register_scheme_class_from_slot_list(
                ctx,
                name,
                Value::null(),
                Value::cons(ctx, valid_super.into(), Value::new(4)),
            ) {
                Err(SchemeClassSpecError::SupersNotList(value)) => assert_eq!(value, Value::new(4)),
                _ => panic!("improper direct-super list should be rejected"),
            }
            match table.register_scheme_class_from_slot_list(
                ctx,
                name,
                Value::null(),
                Value::cons(ctx, Value::new(5), Value::null()),
            ) {
                Err(SchemeClassSpecError::SuperNotClass(value)) => {
                    assert_eq!(value, Value::new(5));
                }
                _ => panic!("non-class direct super should be rejected"),
            }
        });
    }

    #[test]
    fn dynamic_classes_are_rooted_by_the_table() {
        crate::runtime::thread::Scheme::new_uninit().enter(|ctx| {
            let table = ClassTable::with_max_id(ctx, 512);
            let descriptor = table
                .register_dynamic(ctx, "user-class", ClassCategory::Scheme)
                .unwrap();
            let class_id = descriptor.id();

            assert!(table.lookup(class_id).is_some());
            let _ = descriptor;
            assert!(table.lookup(class_id).is_some());
        });
    }

    #[test]
    fn missing_class_id_returns_none() {
        crate::runtime::thread::Scheme::new_uninit().enter(|ctx| {
            let table = ClassTable::with_max_id(ctx, 512);

            assert!(table.lookup(id(10)).is_none());
            assert!(primitive_layout_hooks_for_class_id(id(10)).is_none());
        });
    }

    #[test]
    fn table_allocates_pages_lazily() {
        crate::runtime::thread::Scheme::new_uninit().enter(|ctx| {
            let table = ClassTable::with_max_id(ctx, 512);

            table
                .register_builtin(
                    ctx,
                    ClassDescriptor::new(ctx, id(1), "first", ClassCategory::Builtin),
                )
                .unwrap();
            table
                .register_builtin(
                    ctx,
                    ClassDescriptor::new(ctx, id(257), "second", ClassCategory::Builtin),
                )
                .unwrap();

            assert_eq!(table.allocated_page_count(), 2);
        });
    }

    #[test]
    fn duplicate_id_is_rejected() {
        crate::runtime::thread::Scheme::new_uninit().enter(|ctx| {
            let table = ClassTable::with_max_id(ctx, 512);

            table
                .register_builtin(
                    ctx,
                    ClassDescriptor::new(ctx, id(1), "first", ClassCategory::Builtin),
                )
                .unwrap();
            let err = table
                .register_builtin(
                    ctx,
                    ClassDescriptor::new(ctx, id(1), "second", ClassCategory::Builtin),
                )
                .unwrap_err();

            assert_eq!(err, ClassTableError::DuplicateId(id(1)));
        });
    }

    #[test]
    fn dynamic_registration_keeps_live_slots() {
        crate::runtime::thread::Scheme::new_uninit().enter(|ctx| {
            let table = ClassTable::with_max_id(ctx, MAX_CLASS_ID);
            let first = table
                .register_dynamic(ctx, "first", ClassCategory::Scheme)
                .unwrap();
            let first_id = first.id();
            let _ = first;

            {
                let mut inner = table.inner.lock();
                inner.max_id = first_id.bits();
            }

            let err = table
                .register_dynamic(ctx, "second", ClassCategory::Scheme)
                .unwrap_err();

            assert_eq!(err, ClassTableError::Exhausted);
            assert!(first_id.bits() > builtin_class_ids::MAX);
            assert_eq!(table.lookup(first_id).unwrap().name(), "first");
        });
    }

    #[test]
    fn dynamic_registration_reports_exhaustion() {
        crate::runtime::thread::Scheme::new_uninit().enter(|ctx| {
            let next_id = crate::rsgc::object::peek_next_class_id();
            let table = ClassTable::with_max_id(ctx, next_id.saturating_sub(1));
            let err = table
                .register_dynamic(ctx, "first", ClassCategory::Scheme)
                .unwrap_err();

            assert_eq!(err, ClassTableError::Exhausted);
        });
    }

    #[test]
    fn dynamic_registration_uses_unified_id_allocator() {
        crate::runtime::thread::Scheme::new_uninit().enter(|ctx| {
            let table = ClassTable::with_max_id(ctx, MAX_CLASS_ID);
            let descriptor = table
                .register_dynamic(ctx, "dynamic", ClassCategory::Scheme)
                .unwrap();

            assert!(descriptor.id().bits() > builtin_class_ids::MAX);
        });
    }
}
