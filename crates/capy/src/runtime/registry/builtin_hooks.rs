//! Layout hooks for builtin classes defined outside the object model.

use crate::heap::object::{AllocationHooksOf, builtin_class_ids};
use crate::runtime::objectmodel::class::PrimitiveLayoutHooks;
use crate::runtime::objectmodel::class::builtin::{builtin_id, set_layout_hooks};

macro_rules! register {
    ($hooks:ident, $raw:expr, $ty:ty) => {
        $hooks.push((
            builtin_id($raw),
            PrimitiveLayoutHooks::from_allocation_hooks(
                AllocationHooksOf::<'static, $ty>::HOOKS,
            ),
        ));
    };
    ($hooks:ident, $raw:expr, $ty:ty, direct) => {
        $hooks.push((
            builtin_id($raw),
            PrimitiveLayoutHooks::from_allocation_hooks(<$ty>::HOOKS),
        ));
    };
}

/// Register every builtin class whose heap-object type lives above the object
/// model. Called once, before [`crate::runtime::objectmodel::class::init_builtin_classes`].
pub fn register_builtin_hooks() {
    let mut hooks = Vec::new();
    register!(hooks, builtin_class_ids::VARIABLE, crate::runtime::variable::Variable<'static>);
    register!(hooks, builtin_class_ids::FLUID, crate::runtime::fluids::Fluid<'static>);
    register!(hooks, 
        builtin_class_ids::DYNAMIC_STATE,
        crate::runtime::fluids::DynamicStateObject<'static>
    );
    register!(hooks, builtin_class_ids::CONTINUATION, crate::runtime::interp::stack::Continuation<'static>, direct);
    register!(hooks, builtin_class_ids::LINKLET, crate::runtime::linklet::Linklet<'static>);
    register!(hooks, builtin_class_ids::INSTANCE, crate::runtime::linklet::LibraryInstance<'static>);
    register!(hooks, builtin_class_ids::REGISTRY, crate::runtime::linklet::LibraryRegistry<'static>);
    register!(hooks, builtin_class_ids::RTD, crate::runtime::record::Rtd<'static>);
    register!(hooks, builtin_class_ids::RCD, crate::runtime::record::Rcd<'static>);
    register!(hooks, builtin_class_ids::SYNTAX, crate::runtime::vm::syntax::Syntax<'static>);
    register!(hooks, 
        builtin_class_ids::SYNTAX_TRANSFORMER,
        crate::runtime::vm::syntax::SyntaxTransformer<'static>
    );
    register!(hooks, builtin_class_ids::POLLER, crate::runtime::objectmodel::io::Poller);
    register!(hooks, builtin_class_ids::POINTER, crate::runtime::objectmodel::ffi::Pointer);
    register!(hooks, builtin_class_ids::CIF, crate::runtime::objectmodel::ffi::Cif<'static>);
    register!(hooks, builtin_class_ids::THREAD, crate::runtime::objectmodel::threading::ThreadObject<'static>);
    register!(hooks, builtin_class_ids::MUTEX, crate::runtime::objectmodel::threading::Mutex);
    register!(hooks, builtin_class_ids::CONDITION, crate::runtime::objectmodel::threading::Condition);
    #[cfg(feature = "bootstrap")]
    register!(hooks, builtin_class_ids::ANNOTATION, crate::frontend::reader::Annotation<'static>);
    register!(hooks, 
        builtin_class_ids::CONTINUATION_MARKS,
        crate::runtime::objectmodel::control::ContinuationMarks<'static>
    );
    set_layout_hooks(hooks);
}

#[cfg(test)]
mod tests {
    #[test]
    fn external_classes_have_hooks_after_init() {
        let scheme = crate::runtime::Scheme::new_uninit();
        scheme.enter(|_| {
            for (name, id) in [
                ("thread", crate::heap::object::builtin_class_ids::THREAD),
                ("mutex", crate::heap::object::builtin_class_ids::MUTEX),
                ("condition", crate::heap::object::builtin_class_ids::CONDITION),
                ("continuation", crate::heap::object::builtin_class_ids::CONTINUATION),
                ("syntax", crate::heap::object::builtin_class_ids::SYNTAX),
            ] {
                assert!(
                    crate::runtime::objectmodel::class::builtin::builtin_primitive_layout_hooks(
                        crate::runtime::objectmodel::class::builtin::builtin_id(id)
                    )
                    .is_some(),
                    "{name} ({id}) has no layout hooks"
                );
            }
        });
    }
}
