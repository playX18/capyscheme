use crate::runtime::class::builtin::*;
use crate::runtime::class::descriptor::*;
use crate::runtime::class::flags::*;
use crate::runtime::class::generic::*;
use crate::runtime::class::hooks::*;
use crate::runtime::class::instance::*;
use crate::runtime::class::slot::*;
use crate::runtime::class::table::*;

use crate::rsgc::object::{AllocationHooksOf, ClassId, MAX_CLASS_ID, builtin_class_ids};
use crate::rsgc::{Gc, Trace, Visitor};
use crate::runtime::{Context, value::Value};
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
            .register_dynamic_with_slots(ctx, "derived", ClassCategory::Scheme, &[base.id()], &[])
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
        let first = GenericDescriptor::next_method_chain(ctx, generic, &[Value::new(1)]).unwrap();
        assert_eq!(first.methods().len(), 2);
        assert_eq!(generic.dispatcher_cache().len(), 1);

        let entry = generic.dispatcher_cache()[0];
        assert_eq!(
            entry.class_ids().as_slice(),
            &[id(builtin_class_ids::FIXNUM)]
        );
        assert_eq!(entry.methods()[0].as_gcobj(), fixnum_method.as_gcobj());
        assert_eq!(entry.methods()[1].as_gcobj(), number_method.as_gcobj());

        let second = GenericDescriptor::next_method_chain(ctx, generic, &[Value::new(2)]).unwrap();
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
        let first = GenericDescriptor::next_method_chain(ctx, generic, &[Value::new(1)]).unwrap();
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

        let second = GenericDescriptor::next_method_chain(ctx, generic, &[Value::new(1)]).unwrap();
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
            .redefine_dynamic_with_slots(ctx, id(1), "pair-v2", ClassCategory::Builtin, &[], &[])
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
            .register_dynamic_with_slots(ctx, "derived", ClassCategory::Scheme, &[base.id()], &[])
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
        let instance: Gc<'_, SchemeInstance<'_>> = unsafe { Gc::from_gcobj(value.as_cell_raw()) };

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
        match SchemeInstance::allocate_with_initargs(ctx, class, &[(locked_keyword, Value::new(1))])
        {
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
            .register_scheme_class_from_slot_list(ctx, derived_name, derived_slots, direct_supers)
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
