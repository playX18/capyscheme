use super::descriptor::ClassDescriptor;
use super::table::class_table;
use crate::prelude::*;
use crate::rsgc::alloc::{Array, ArrayRef};
use crate::rsgc::cell::Lock;
use crate::rsgc::object::{ClassId, builtin_class_ids, class_header_word};
use crate::rsgc::{Gc, Trace};
use crate::runtime::value::conversions::ClassTagged;
use crate::runtime::{
    Context,
    value::{Closure, NativeReturn, PROCEDURES, Value},
};

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

pub(crate) fn generic_property_entry<'gc>(
    ctx: Context<'gc>,
    key: Value<'gc>,
    value: Value<'gc>,
    rest: Value<'gc>,
) -> Value<'gc> {
    Value::cons(ctx, Value::cons(ctx, key, value), rest)
}

pub fn generic_descriptor_from_value<'gc>(
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

pub fn generic_assertion_violation<'gc>(
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
    // SAFETY: Pointer is valid for the given element count
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

pub fn generic_procedure<'gc>(
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
            class_header_word(super::builtin::builtin_id(builtin_class_ids::GENERIC)),
        )
    }

    pub fn name(&self) -> &str {
        // SAFETY: The byte content is known to be valid UTF-8
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
        table: &super::table::ClassTable<'gc>,
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

    pub(crate) fn clear_dispatcher_cache(
        ctx: Context<'gc>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
    ) {
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
            class_header_word(super::builtin::builtin_id(builtin_class_ids::METHOD)),
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
    pub(crate) fn new(
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
            class_header_word(super::builtin::builtin_id(builtin_class_ids::NEXT_METHOD)),
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

// SAFETY: `gc` for `GenericDescriptor` upholds all trait invariants
unsafe impl<'gc> ClassTagged for GenericDescriptor<'gc> {
    const TYPE_NAME: &'static str = "generic";
    const CLASS_IDS: &'static [u32] = &[builtin_class_ids::GENERIC];
}

// SAFETY: `gc` for `MethodDescriptor` upholds all trait invariants
unsafe impl<'gc> ClassTagged for MethodDescriptor<'gc> {
    const TYPE_NAME: &'static str = "method";
    const CLASS_IDS: &'static [u32] = &[builtin_class_ids::METHOD];
}

// SAFETY: `gc` for `NextMethodDescriptor` upholds all trait invariants
unsafe impl<'gc> ClassTagged for NextMethodDescriptor<'gc> {
    const TYPE_NAME: &'static str = "next-method";
    const CLASS_IDS: &'static [u32] = &[builtin_class_ids::NEXT_METHOD];
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
