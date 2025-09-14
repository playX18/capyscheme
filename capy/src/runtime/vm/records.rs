//! Helpers to work with Scheme records from Rust. 
//! 
//! This module does not on its own expose any functions to Scheme code but
//! merely provides utilities for other parts of the VM to work with records.


use crate::{runtime::prelude::*, static_symbols};

static_symbols!(
    SYM_TYPE_RTD = "type:record-type-descriptor"
    SYM_TYPE_RCD = "type:record-constructor-descriptor"
    SYM_TYPE_RECORD = "type:record-type"
);

pub const RTD_TYPE: usize = 0;
pub const RTD_NAME: usize = 1;
pub const RTD_PARENT: usize = 2;
pub const RTD_UID: usize = 3;
pub const RTD_SEALED: usize = 4;
pub const RTD_OPAQUE: usize = 5;
pub const RTD_FIELDS: usize = 6;



impl<'gc> Value<'gc> {
    pub fn is_recod_type_descriptor(&self, ctx: Context<'gc>) -> bool {
        self.is::<Tuple>() && self.downcast::<Tuple>()[0].get() == sym_type_rtd(ctx).into()
    }

    pub fn is_record(&self, ctx: Context<'gc>) -> bool {
        self.is::<Tuple>() && self.downcast::<Tuple>()[0].get().is_recod_type_descriptor(ctx)
    }

    pub fn is_record_constructor_descriptor(&self, ctx: Context<'gc>) -> bool {
        self.is::<Tuple>() && self.downcast::<Tuple>()[0].get() == sym_type_rcd(ctx).into()
    }

    pub fn is_record_type(&self, ctx: Context<'gc>) -> bool {
        self.is::<Tuple>() && self.downcast::<Tuple>()[0].get() == sym_type_record(ctx).into()
    }

    pub fn record_rtd(&self, ctx: Context<'gc>) -> Option<Gc<'gc, Tuple<'gc>>> {
        if self.is_record(ctx) {
            Some(self.downcast::<Tuple>()[0].get().downcast())
        } else {
            None
        }
    }

    pub fn rtd_parent(&self, ctx: Context<'gc>) -> Value<'gc> {
        assert!(self.is_recod_type_descriptor(ctx));
        self.downcast::<Tuple>()[RTD_PARENT].get()
    }

    pub fn rtd_ancestor(&self, ctx: Context<'gc>, parent: Value<'gc>) -> bool {
        let mut rtd = *self;
        loop {
            
            if rtd == parent {
                return true;
            }

            let rtd_parent = rtd.rtd_parent(ctx);
            if rtd_parent == Value::new(false) {
                return false;
            }

            rtd = rtd_parent;
        }
    }

    pub fn record_type_rtd(&self, ctx: Context<'gc>) -> Value<'gc> {
        assert!(self.is_record_type(ctx));
        self.downcast::<Tuple>()[2].get()
    }

    pub fn is_record_of(&self, ctx: Context<'gc>, rtd: Value<'gc>) -> bool {
        self.is::<Tuple>()
            && (self.downcast::<Tuple>()[0].get() == rtd
        || self.downcast::<Tuple>()[0].get().rtd_ancestor(ctx, rtd))
    }
    
}