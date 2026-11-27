//! Method combinations.

use super::builtin::builtin_id;
use super::flags::LayoutFamily;
use crate::heap::object::{builtin_class_ids, class_header_word};
use crate::heap::{Gc, Trace};
use crate::prelude::*;
use crate::runtime::MutationContext;
use crate::runtime::objectmodel::conversions::ClassTagged;
use crate::runtime::objectmodel::{Symbol, Value};

/// Method combination.
#[derive(Trace)]
#[collect(no_drop)]
#[repr(C)]
pub struct MethodCombination<'gc> {
    name: Gc<'gc, Symbol<'gc>>,
    /// Options.
    options: Value<'gc>,
    /// Builder.
    builder: Value<'gc>,
    /// Standard flag.
    standard: bool,
}

// SAFETY: `gc` for `MethodCombination` upholds all trait invariants
unsafe impl<'gc> ClassTagged for MethodCombination<'gc> {
    const TYPE_NAME: &'static str = "method-combination";
    const CLASS_IDS: &'static [u32] = &[builtin_class_ids::METHOD_COMBINATION];
    const FAMILY: Option<LayoutFamily> = Some(LayoutFamily::MethodCombination);
}

// The standard combination, shared by every generic that does not name one.
global!(pub(crate) STANDARD_COMBINATION<'gc>: Gc<'gc, MethodCombination<'gc>> = (ctx)
    MethodCombination::new(
        ctx,
        Symbol::from_str(ctx, "standard"),
        Value::null(),
        Value::new(false),
        true,
    ););

impl<'gc> MethodCombination<'gc> {
    pub fn new(
        ctx: MutationContext<'gc>,
        name: Gc<'gc, Symbol<'gc>>,
        options: Value<'gc>,
        builder: Value<'gc>,
        standard: bool,
    ) -> Gc<'gc, Self> {
        ctx.allocate_with(
            Self {
                name,
                options,
                builder,
                standard,
            },
            class_header_word(builtin_id(builtin_class_ids::METHOD_COMBINATION)),
        )
    }

    pub fn name(&self) -> Gc<'gc, Symbol<'gc>> {
        self.name
    }

    pub fn options(&self) -> Value<'gc> {
        self.options
    }

    pub fn builder(&self) -> Value<'gc> {
        self.builder
    }

    pub fn is_standard(&self) -> bool {
        self.standard
    }
}
