pub(crate) mod builtin;
pub(crate) mod descriptor;
pub(crate) mod flags;
pub(crate) mod generic;
pub(crate) mod hooks;
pub(crate) mod instance;
mod ops;
pub(crate) mod parse;
pub(crate) mod slot;
pub(crate) mod storage;
pub(crate) mod table;

pub use descriptor::ClassDescriptor;
pub use flags::{ClassCategory, ClassFlags};
pub use generic::{
    GenericDescriptor, GenericDispatchError, GenericFlags, GenericInvocation, MethodDescriptor,
    MethodFlags, NextMethodDescriptor, SLOT_PROTOCOL_GENERICS, fire_slot_protocol,
    generic_descriptor_from_value, generic_procedure, is_slot_protocol_generic,
};
pub(crate) use generic::{OP_BOUND, OP_REF, OP_SET};
pub use hooks::{PrimitiveLayoutHooks, PrimitiveOperationHooks};
pub use instance::{
    SchemeInstance, compare_primitive_values, hash_primitive_value, print_primitive_value,
    try_scheme_instance,
};
pub use ops::class_ops;
pub use slot::{
    SlotAccessError, SlotAccessorDefinition, SlotAccessorDescriptor, SlotDefinitionDescriptor,
    SlotDescriptor, SlotFlags, SlotInitError, SlotSpec,
};
pub use table::{
    ClassTable, ClassTableError, SchemeClassSpecError, class_table, init_builtin_classes,
    primitive_layout_hooks_for_class_id,
};

pub(crate) use builtin::builtin_class_specs;
pub(crate) use descriptor::{PROTOCOL_CUSTOM, PROTOCOL_PLAIN};
pub(crate) use descriptor::{
    call_scheme_slot_bound, call_scheme_slot_ref, call_scheme_slot_set,
    class_id_list_to_class_objects, class_id_to_class_object, slot_accessor_list,
    slot_definition_list,
};
pub(crate) use ops::init_class_ops;
pub(crate) use parse::parse_class_list;
pub(crate) use table::{
    ClassListError, class_table_initialized, is_type_class_registered,
    register_internal_type_class_if_ready,
};
