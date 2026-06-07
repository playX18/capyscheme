use super::*;
use crate::{rsgc::barrier, runtime::Scheme};

static TEST_LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());

extern "C-unwind" fn test_native_proc<'gc>(
    _ctx: Context<'gc>,
    _rator: Value<'gc>,
    _rands: *const Value<'gc>,
    _num_rands: usize,
    _retk: Value<'gc>,
) -> NativeReturn<'gc> {
    NativeReturn {
        code: ReturnCode::ReturnOk,
        value: Value::undefined(),
    }
}

fn with_ctx(f: impl for<'gc> FnOnce(Context<'gc>)) {
    let _guard = TEST_LOCK
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner());
    let scm = Scheme::new_uninit();
    scm.enter(f);
}

#[test]
fn closure_inherits_metadata_from_code_block() {
    with_ctx(|ctx| {
        let metadata = Value::cons(ctx, ctx.intern("name"), ctx.str("closure-a"));
        let code_block = CodeBlock::new_aot(
            ctx,
            Address::from_ptr(test_native_proc as *const ()),
            CodeArity::new(0),
            false,
            metadata,
        );
        let closure = Closure::new(ctx, code_block, &[], false);

        assert_eq!(closure.code, code_block.entrypoint);
        assert!(Gc::ptr_eq(closure.code_block, code_block));
        assert_eq!(closure.meta.get(), code_block.metadata.get());
    });
}

#[test]
fn mutating_closure_metadata_does_not_mutate_code_block_default() {
    with_ctx(|ctx| {
        let metadata = Value::cons(ctx, ctx.intern("name"), ctx.str("prototype"));
        let code_block = CodeBlock::new_aot(
            ctx,
            Address::from_ptr(test_native_proc as *const ()),
            CodeArity::new(0),
            false,
            metadata,
        );

        let closure_a = Closure::new(ctx, code_block, &[], false);
        let updated = Value::cons(ctx, ctx.intern("name"), ctx.str("updated"));
        let wclosure = Gc::write(*ctx, closure_a);
        barrier::field!(wclosure, Closure, meta)
            .unlock()
            .set(updated);

        let closure_b = Closure::new(ctx, code_block, &[], false);

        assert_eq!(code_block.metadata.get(), metadata);
        assert_eq!(closure_a.meta.get(), updated);
        assert_eq!(closure_b.meta.get(), metadata);
    });
}

#[test]
fn native_closure_keeps_native_proc_in_first_free_slot() {
    with_ctx(|ctx| {
        let closure = PROCEDURES
            .fetch(*ctx)
            .procedure(test_native_proc)
            .free_vars([Value::new(true)])
            .metadata(Value::null())
            .build(ctx);

        assert_eq!(closure.code, closure.code_block.entrypoint);
        assert!(closure.code_block.metadata.get().is_null());
        assert!(closure.is_foreign());
        assert!(closure[0].get().is::<NativeProc>());
        assert_eq!(closure[1].get(), Value::new(true));
    });
}

#[test]
fn static_native_closure_is_cached_by_function_address() {
    with_ctx(|ctx| {
        let procs = PROCEDURES.fetch(*ctx);
        let first = procs
            .procedure(test_native_proc)
            .metadata(Value::new(false))
            .cached_static()
            .build(ctx);
        let second = procs
            .procedure(test_native_proc)
            .metadata(Value::null())
            .cached_static()
            .build(ctx);

        assert!(Gc::ptr_eq(first, second));
        assert!(first[0].get().is::<NativeProc>());
    });
}

#[test]
fn loaded_code_block_owns_span_and_unlinked_code() {
    with_ctx(|ctx| {
        let mut memory = crate::runtime::code_memory::CodeMemory::new();
        let loaded = memory.allocate_copy(&[0xc3]).expect("allocate code span");
        let entrypoint = loaded.entrypoint;
        let unlinked = RelocatableCodeBlock::new(ctx, &[0xc3], 0, &[], &[]);
        let metadata = Value::cons(ctx, ctx.intern("name"), ctx.str("loaded"));
        let code_block = CodeBlock::new_loaded_with_data(
            ctx,
            LoadedCodeBlockInit {
                entrypoint,
                arity: CodeArity::new(2),
                is_continuation: true,
                metadata,
                unlinked,
                span: loaded.into_span(),
                loaded_data_base: Address::ZERO,
                loaded_data_slot_count: 0,
            },
        );

        assert!(matches!(code_block.kind, CodeBlockKind::Loaded));
        assert!(code_block.has_live_span());
        assert_eq!(code_block.unlinked.code(), [0xc3]);
        assert!(code_block.flags.contains(CodeBlockFlags::CONTINUATION));
        assert_eq!(code_block.arity.fixed_arity(), 2);
        assert_eq!(code_block.metadata.get(), metadata);

        let span = unsafe {
            code_block
                .take_span_for_finalization()
                .expect("span is present")
        };
        assert!(!code_block.has_live_span());
        assert!(unsafe { code_block.take_span_for_finalization() }.is_none());
        memory.release_span(span).expect("release code span");
    });
}

#[test]
fn native_code_block_does_not_initialize_span() {
    with_ctx(|ctx| {
        let code_block = CodeBlock::new_native(
            ctx,
            Address::from_ptr(test_native_proc as *const ()),
            CodeArity::new(0),
            false,
            Value::new(false),
        );

        assert!(!code_block.has_live_span());
        assert!(code_block.unlinked.code().is_empty());
    });
}

#[test]
fn code_block_span_can_be_taken_for_finalization_once() {
    with_ctx(|ctx| {
        let mut memory = crate::runtime::code_memory::CodeMemory::new();
        let loaded = memory.allocate_copy(&[0xc3]).expect("allocate code");
        let entrypoint = loaded.entrypoint;
        let unlinked = RelocatableCodeBlock::new(ctx, &[0xc3], 0, &[], &[]);
        let code_block = CodeBlock::new_loaded_with_data(
            ctx,
            LoadedCodeBlockInit {
                entrypoint,
                arity: CodeArity::new(0),
                is_continuation: false,
                metadata: Value::new(false),
                unlinked,
                span: loaded.into_span(),
                loaded_data_base: Address::ZERO,
                loaded_data_slot_count: 0,
            },
        );

        let span = unsafe {
            code_block
                .take_span_for_finalization()
                .expect("span is present")
        };
        assert!(unsafe { code_block.take_span_for_finalization() }.is_none());
        memory.release_span(span).expect("release code span");
    });
}

#[test]
fn relocatable_code_block_preserves_original_code_relocations_and_bitmap() {
    with_ctx(|ctx| {
        let code = [0xde, 0xad, 0xbe, 0xef, 0x01];
        let relocations = [
            CodeRelocation {
                offset: 4,
                kind_tag: 1,
                target_tag: 2,
                target_payload: 3,
                addend: -8,
                aux_tag: 5,
            },
            CodeRelocation {
                offset: 12,
                kind_tag: 6,
                target_tag: 7,
                target_payload: 8,
                addend: 16,
                aux_tag: 9,
            },
        ];
        let bitmap = [0b101usize, usize::MAX];

        let code_block = RelocatableCodeBlock::new(ctx, &code, 2, &relocations, &bitmap);

        assert_eq!(code_block.entry_offset, 2);
        assert_eq!(code_block.code(), code);
        assert_eq!(code_block.relocations(), relocations);
        assert_eq!(code_block.loaded_data_value_bitmap(), bitmap);
    });
}

#[test]
fn code_blocks_allocate_with_class_only_headers() {
    with_ctx(|ctx| {
        let unlinked = RelocatableCodeBlock::new(ctx, &[0xc3], 0, &[], &[]);
        assert_eq!(
            unlinked.as_gcobj().header().class_id(),
            ClassId::new(builtin_class_ids::RELOCATABLE_CODE_BLOCK).unwrap()
        );

        let code_block = CodeBlock::new_native(
            ctx,
            Address::from_ptr(test_native_proc as *const ()),
            CodeArity::new(0),
            false,
            Value::new(false),
        );
        assert_eq!(
            code_block.as_gcobj().header().class_id(),
            ClassId::new(builtin_class_ids::CODE_BLOCK).unwrap()
        );
    });
}
