//! Scheme-visible record type and record constructor descriptor primitives.
//!
//! These are the native counterparts of `lib/boot/records.scm`. Record
//! instances are still tuples `[rtd, field...]`; only the descriptors are
//! native objects.

use crate::heap::{Gc, barrier};
use crate::prelude::*;
use crate::runtime::record::{Rcd, Rtd};
use crate::runtime::value::{Closure, Tuple, Vector};

/// Raises an assertion violation attributed to `who`, the way the Scheme
/// `assertion-violation` helper does.
fn violation<'gc>(
    call: &Call<'_, 'gc>,
    who: &str,
    message: &str,
    irritant: Value<'gc>,
) -> Value<'gc> {
    let ctx = call.ctx;
    let message = Str::new(ctx, message, true).into();
    call.raise_assertion_violation(ctx.intern(who), message, irritant)
}

/// The default parent constructor descriptor for `rtd`: `#f` for a root type,
/// otherwise a default descriptor for the parent, itself built the same way.
fn default_parent_rcd<'gc>(ctx: MutationContext<'gc>, rtd: Gc<'gc, Rtd<'gc>>) -> Value<'gc> {
    match rtd.parent() {
        Some(parent) => {
            let prcd = default_parent_rcd(ctx, parent);
            Rcd::new(ctx, parent, prcd, Value::new(false)).into()
        }
        None => Value::new(false),
    }
}

const MAKE_RTD: &str = "make-record-type-descriptor";

/// Whether `obj` is a tuple whose slot 0 is a record type descriptor.
fn is_record_value(obj: Value<'_>) -> bool {
    obj.is::<Tuple>() && {
        let tuple = obj.downcast::<Tuple>();
        tuple.len() > 0 && tuple[0].get().is::<Rtd>()
    }
}

#[scheme(path=capy)]
pub mod record_ops {

    #[scheme(name = "make-record-type-descriptor")]
    pub fn make_record_type_descriptor(
        name: Value<'gc>,
        parent: Value<'gc>,
        uid: Value<'gc>,
        sealed: bool,
        opaque: bool,
        fields: Value<'gc>,
    ) -> Value<'gc> {
        if !name.is::<Symbol>() {
            return violation(call, MAKE_RTD, "expected a symbol for name", name);
        }
        if !fields.is::<Vector>() {
            return violation(call, MAKE_RTD, "expected a vector for fields", fields);
        }
        let parent_rtd = if parent == Value::new(false) {
            None
        } else {
            if !parent.is::<Rtd>() {
                return violation(
                    call,
                    MAKE_RTD,
                    "expected a record-type descriptor or #f for parent",
                    parent,
                );
            }
            let parent_rtd = parent.downcast::<Rtd>();
            if parent_rtd.is_sealed() {
                return violation(call, MAKE_RTD, "cannot extend a sealed record type", parent);
            }
            Some(parent_rtd)
        };
        if uid != Value::new(false) && !uid.is::<Symbol>() {
            return violation(call, MAKE_RTD, "expected a symbol or #f for uid", uid);
        }

        // Validate every field spec before allocating anything.
        let field_specs = fields.downcast::<Vector>();
        let field_specs = field_specs.as_slice();
        let mutable_sym = ctx.intern("mutable");
        for spec in field_specs {
            if !(spec.is_pair() && spec.cdr().is_pair()) {
                return violation(call, MAKE_RTD, "expected a field specification", *spec);
            }
        }

        // Normalize to `(mutable? . name)` pairs, in declaration order.
        let mut fields_list = Value::null();
        for spec in field_specs.iter().rev() {
            let mutable = spec.car() == mutable_sym;
            let name = spec.cdr().car();
            fields_list = Value::cons(
                ctx,
                Value::cons(ctx, Value::new(mutable), name),
                fields_list,
            );
        }
        let own_field_count = field_specs.len() as u32;

        if uid == Value::new(false) {
            let rtd = Rtd::new(
                ctx,
                name,
                parent_rtd,
                uid,
                sealed,
                opaque,
                fields_list,
                own_field_count,
            );
            return rtd.into();
        }

        let table = ctx
            .globals()
            .nongenerative_rtds
            .get()
            .downcast::<HashTable>();
        match table.get(ctx, uid) {
            Some(current) => {
                let current = current.downcast::<Rtd>();
                if current.nongenerative_compatible(parent_rtd, fields_list, sealed, opaque) {
                    current.into()
                } else {
                    let irritant = crate::list!(
                        ctx,
                        name,
                        parent,
                        uid,
                        Value::new(sealed),
                        Value::new(opaque),
                        fields_list
                    );
                    violation(
                        call,
                        MAKE_RTD,
                        "mismatched subsequent call for nongenerative record-type",
                        irritant,
                    )
                }
            }
            None => {
                let rtd = Rtd::new(
                    ctx,
                    name,
                    parent_rtd,
                    uid,
                    sealed,
                    opaque,
                    fields_list,
                    own_field_count,
                );
                table.put(ctx, uid, Value::from(rtd));
                rtd.into()
            }
        }
    }

    #[scheme(name = "record-type-descriptor?")]
    pub fn is_record_type_descriptor(obj: Value<'gc>) -> bool {
        obj.is::<Rtd>()
    }

    #[scheme(name = "rtd-name")]
    pub fn rtd_name(rtd: Gc<'gc, Rtd<'gc>>) -> Value<'gc> {
        rtd.name
    }

    #[scheme(name = "rtd-parent")]
    pub fn rtd_parent(rtd: Gc<'gc, Rtd<'gc>>) -> Value<'gc> {
        match rtd.parent() {
            Some(parent) => parent.into(),
            None => Value::new(false),
        }
    }

    #[scheme(name = "rtd-uid")]
    pub fn rtd_uid(rtd: Gc<'gc, Rtd<'gc>>) -> Value<'gc> {
        rtd.uid
    }

    #[scheme(name = "rtd-sealed?")]
    pub fn rtd_sealed(rtd: Gc<'gc, Rtd<'gc>>) -> bool {
        rtd.is_sealed()
    }

    #[scheme(name = "rtd-opaque?")]
    pub fn rtd_opaque(rtd: Gc<'gc, Rtd<'gc>>) -> bool {
        rtd.is_opaque()
    }

    /// The fields this type declares, not including inherited ones.
    #[scheme(name = "rtd-fields")]
    pub fn rtd_fields(rtd: Gc<'gc, Rtd<'gc>>) -> Value<'gc> {
        rtd.fields
    }

    /// The total number of fields, including inherited ones.
    #[scheme(name = "rtd-field-count")]
    pub fn rtd_field_count(rtd: Gc<'gc, Rtd<'gc>>) -> usize {
        rtd.field_count as usize
    }

    #[scheme(name = "rtd-printer")]
    pub fn rtd_printer(rtd: Gc<'gc, Rtd<'gc>>) -> Value<'gc> {
        rtd.printer.get()
    }

    #[scheme(name = "set-rtd-printer!")]
    pub fn set_rtd_printer(rtd: Gc<'gc, Rtd<'gc>>, printer: Value<'gc>) -> Value<'gc> {
        barrier::field!(Gc::write(ctx, rtd), Rtd, printer)
            .unlock()
            .set(printer);
        Value::undefined()
    }

    /// Whether `rtd` is `parent` or one of its descendants.
    #[scheme(name = "rtd-ancestor?")]
    pub fn rtd_ancestor(parent: Gc<'gc, Rtd<'gc>>, rtd: Value<'gc>) -> bool {
        rtd.is::<Rtd>() && rtd.downcast::<Rtd>().is_subtype_of(parent)
    }

    #[scheme(name = "make-record-constructor-descriptor")]
    pub fn make_record_constructor_descriptor(
        rtd: Gc<'gc, Rtd<'gc>>,
        parent: Value<'gc>,
        protocol: Value<'gc>,
    ) -> Value<'gc> {
        let who = "make-record-constructor-descriptor";
        if protocol != Value::new(false) && !protocol.is::<Closure>() {
            return violation(
                call,
                who,
                "expected a procedure or #f for protocol",
                protocol,
            );
        }
        let prcd = if parent == Value::new(false) {
            default_parent_rcd(ctx, rtd)
        } else {
            if !parent.is::<Rcd>() {
                return violation(
                    call,
                    who,
                    "expected a record-constructor-descriptor or #f for parent",
                    parent,
                );
            }
            let parent_rcd = parent.downcast::<Rcd>();
            let parent_matches = match rtd.parent() {
                Some(rtd_parent) => Gc::ptr_eq(rtd_parent, parent_rcd.rtd),
                None => false,
            };
            if !parent_matches {
                return violation(
                    call,
                    who,
                    "parent record-constructor-descriptor does not match the record type's parent",
                    parent,
                );
            }
            parent
        };
        Rcd::new(ctx, rtd, prcd, protocol).into()
    }

    #[scheme(name = "record-constructor-descriptor?")]
    pub fn is_record_constructor_descriptor(obj: Value<'gc>) -> bool {
        obj.is::<Rcd>()
    }

    #[scheme(name = "rcd-rtd")]
    pub fn rcd_rtd(rcd: Gc<'gc, Rcd<'gc>>) -> Value<'gc> {
        rcd.rtd.into()
    }

    #[scheme(name = "rcd-parent")]
    pub fn rcd_parent(rcd: Gc<'gc, Rcd<'gc>>) -> Value<'gc> {
        rcd.prcd
    }

    /// The custom protocol, or `#f` when the descriptor uses the default.
    #[scheme(name = "rcd-protocol")]
    pub fn rcd_protocol(rcd: Gc<'gc, Rcd<'gc>>) -> Value<'gc> {
        rcd.protocol
    }

    /// Whether `obj` is a record instance: a tuple whose slot 0 is a record
    /// type descriptor. Opaque types count, as in `lib/boot/records.scm`.
    #[scheme(name = "record?")]
    pub fn is_record(obj: Value<'gc>) -> bool {
        is_record_value(obj)
    }

    #[scheme(name = "record-rtd")]
    pub fn record_rtd(obj: Value<'gc>) -> Value<'gc> {
        if is_record_value(obj) {
            obj.downcast::<Tuple>()[0].get()
        } else {
            violation(call, "record-rtd", "expected a record, but got", obj)
        }
    }
}
