//! Helpers to work with Scheme records from Rust.
//!
//! This module does not on its own expose any functions to Scheme code but
//! merely provides utilities for other parts of the VM to work with records.
//!
//! A record instance is a tuple whose first slot holds its [`Rtd`]
use crate::{
    runtime::{
        prelude::*,
        record::{Rcd, Rtd},
    },
    static_symbols,
};

static_symbols!(SYM_TYPE_RECORD = "type:record-type");

impl<'gc> Value<'gc> {
    pub fn is_record_type_descriptor(&self) -> bool {
        self.is::<Rtd>()
    }

    pub fn is_record_constructor_descriptor(&self) -> bool {
        self.is::<Rcd>()
    }

    pub fn is_record(&self) -> bool {
        self.record_rtd().is_some()
    }

    pub fn is_record_type(&self, ctx: MutationContext<'gc>) -> bool {
        self.is::<Tuple>() && self.downcast::<Tuple>()[0].get() == sym_type_record(ctx).into()
    }

    /// The type descriptor of a record instance, or `None` for any other value.
    pub fn record_rtd(&self) -> Option<Gc<'gc, Rtd<'gc>>> {
        let tuple = self.try_as::<Tuple>()?;
        if tuple.len() == 0 {
            return None;
        }
        tuple[0].get().try_as::<Rtd>()
    }

    pub fn record_type_rtd(&self, ctx: MutationContext<'gc>) -> Value<'gc> {
        assert!(self.is_record_type(ctx));
        self.downcast::<Tuple>()[2].get()
    }

    /// Whether this is an instance of `rtd` or of one of its subtypes.
    pub fn is_record_of(&self, rtd: Gc<'gc, Rtd<'gc>>) -> bool {
        self.record_rtd().is_some_and(|own| own.is_subtype_of(rtd))
    }
}
