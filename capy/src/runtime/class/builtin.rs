use crate::rsgc::object::{
    AllocationHooksOf, ClassId, builtin_class_ids,
};
use super::descriptor::ClassDescriptor;
use super::flags::ClassCategory;
use super::generic::{GenericDescriptor, MethodDescriptor, NextMethodDescriptor};
use super::hooks::PrimitiveLayoutHooks;
use super::slot::{SlotAccessorDefinition, SlotDefinitionDescriptor};

pub const CLASS_TABLE_PAGE_BITS: u32 = 8;
pub const CLASS_TABLE_PAGE_SIZE: usize = 1 << CLASS_TABLE_PAGE_BITS;
pub const CLASS_TABLE_SLOT_MASK: u32 = (CLASS_TABLE_PAGE_SIZE as u32) - 1;

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

    pub(crate) fn descriptor<'gc>(self, ctx: crate::runtime::Context<'gc>) -> ClassDescriptor<'gc> {
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

pub fn builtin_class_specs() -> &'static [BuiltinClassSpec] {
    BUILTIN_CLASS_SPECS
}

pub(crate) const fn builtin_id(raw: u32) -> ClassId {
    match ClassId::new(raw) {
        Some(id) => id,
        None => panic!("invalid built-in class id"),
    }
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

pub fn builtin_primitive_layout_hooks(id: ClassId) -> Option<PrimitiveLayoutHooks> {
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
