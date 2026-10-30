//! The R6RS condition type hierarchy

use crate::{
    heap::{Gc, Trace},
    runtime::{
        MutationContext,
        record::{Rcd, Rtd},
        value::{Tuple, Value},
    },
};

/// `(name, parent, fields)` for every condition type
pub const CONDITION_TYPES: &[(&str, Option<&str>, &[&str])] = &[
    ("&condition", None, &[]),
    ("&message", Some("&condition"), &["message"]),
    ("&warning", Some("&condition"), &[]),
    ("&serious", Some("&condition"), &[]),
    ("&error", Some("&serious"), &[]),
    ("&violation", Some("&serious"), &[]),
    ("&assertion", Some("&violation"), &[]),
    ("&irritants", Some("&condition"), &["irritants"]),
    ("&who", Some("&condition"), &["who"]),
    ("&marks", Some("&condition"), &["marks"]),
    ("&non-continuable", Some("&violation"), &[]),
    ("&implementation-restriction", Some("&violation"), &[]),
    ("&lexical", Some("&violation"), &[]),
    ("&syntax", Some("&violation"), &["form", "subform"]),
    ("&source", Some("&condition"), &["file", "line", "column"]),
    ("&expansion-trace", Some("&condition"), &["frames"]),
    ("&undefined", Some("&violation"), &[]),
    ("&i/o", Some("&error"), &[]),
    ("&i/o-read", Some("&i/o"), &[]),
    ("&i/o-write", Some("&i/o"), &[]),
    ("&i/o-invalid-position", Some("&i/o"), &["position"]),
    ("&i/o-filename", Some("&i/o"), &["filename"]),
    ("&i/o-file-protection", Some("&i/o-filename"), &[]),
    ("&i/o-file-is-read-only", Some("&i/o-file-protection"), &[]),
    ("&i/o-file-already-exists", Some("&i/o-filename"), &[]),
    ("&i/o-file-does-not-exist", Some("&i/o-filename"), &[]),
    ("&i/o-port", Some("&i/o"), &["port"]),
    ("&i/o-decoding", Some("&i/o-port"), &[]),
    ("&i/o-encoding", Some("&i/o-port"), &["char"]),
    ("&no-infinities", Some("&implementation-restriction"), &[]),
    ("&no-nans", Some("&implementation-restriction"), &[]),
    (
        "&continuable-exception",
        Some("&implementation-restriction"),
        &["continuation", "base"],
    ),
];

/// The record types the runtime builds conditions from, plus every wrapper.
#[derive(Trace)]
pub struct ConditionTypes<'gc> {
    pub serious: Gc<'gc, Rtd<'gc>>,
    pub message: Gc<'gc, Rtd<'gc>>,
    pub warning: Gc<'gc, Rtd<'gc>>,
    pub error: Gc<'gc, Rtd<'gc>>,
    pub violation: Gc<'gc, Rtd<'gc>>,
    pub irritants: Gc<'gc, Rtd<'gc>>,
    pub who: Gc<'gc, Rtd<'gc>>,
    pub marks: Gc<'gc, Rtd<'gc>>,
    pub non_continuable: Gc<'gc, Rtd<'gc>>,
    pub implementation_restriction: Gc<'gc, Rtd<'gc>>,
    pub lexical: Gc<'gc, Rtd<'gc>>,
    pub syntax: Gc<'gc, Rtd<'gc>>,
    pub source: Gc<'gc, Rtd<'gc>>,
    pub expansion_trace: Gc<'gc, Rtd<'gc>>,
    pub undefined: Gc<'gc, Rtd<'gc>>,
    pub assertion: Gc<'gc, Rtd<'gc>>,
    /// `(type:record-type name rtd rcd)` for each entry of [`CONDITION_TYPES`],
    /// in the same order.
    pub wrappers: Vec<Value<'gc>>,
}

impl<'gc> ConditionTypes<'gc> {
    /// Creates every condition record type and its constructor descriptor.
    pub fn new(ctx: MutationContext<'gc>) -> Self {
        // Each entry: (name, rtd, rcd), filled in table order.
        let mut built: Vec<(&str, Gc<'gc, Rtd<'gc>>, Gc<'gc, Rcd<'gc>>)> =
            Vec::with_capacity(CONDITION_TYPES.len());
        let mut wrappers = Vec::with_capacity(CONDITION_TYPES.len());
        let record_type_tag = ctx.intern("type:record-type");

        for &(name, parent, fields) in CONDITION_TYPES {
            let parent_entry = parent.map(|parent_name| {
                built
                    .iter()
                    .find(|(n, _, _)| *n == parent_name)
                    .unwrap_or_else(|| panic!("condition parent {parent_name} is not defined yet"))
            });
            let parent_rtd = parent_entry.map(|(_, rtd, _)| *rtd);
            let parent_rcd: Value<'gc> = match parent_entry {
                Some((_, _, rcd)) => (*rcd).into(),
                None => Value::new(false),
            };

            // `(mutable? . name)` pairs in declaration order, all immutable.
            let mut fields_list = Value::null();
            for field in fields.iter().rev() {
                let field_name = ctx.intern(field);
                fields_list = Value::cons(
                    ctx,
                    Value::cons(ctx, Value::new(false), field_name),
                    fields_list,
                );
            }

            let rtd = Rtd::new(
                ctx,
                ctx.intern(name),
                parent_rtd,
                Value::new(false),
                false,
                false,
                fields_list,
                fields.len() as u32,
            );
            let rcd = Rcd::new(ctx, rtd, parent_rcd, Value::new(false));
            let wrapper: Value<'gc> = Tuple::from_slice(
                ctx,
                &[record_type_tag, ctx.intern(name), rtd.into(), rcd.into()],
            )
            .into();

            built.push((name, rtd, rcd));
            wrappers.push(wrapper);
        }

        let rtd_of = |name: &str| -> Gc<'gc, Rtd<'gc>> {
            built
                .iter()
                .find(|(n, _, _)| *n == name)
                .map(|(_, rtd, _)| *rtd)
                .unwrap_or_else(|| panic!("condition type {name} is missing"))
        };

        Self {
            serious: rtd_of("&serious"),
            message: rtd_of("&message"),
            warning: rtd_of("&warning"),
            error: rtd_of("&error"),
            violation: rtd_of("&violation"),
            irritants: rtd_of("&irritants"),
            who: rtd_of("&who"),
            marks: rtd_of("&marks"),
            non_continuable: rtd_of("&non-continuable"),
            implementation_restriction: rtd_of("&implementation-restriction"),
            lexical: rtd_of("&lexical"),
            syntax: rtd_of("&syntax"),
            source: rtd_of("&source"),
            expansion_trace: rtd_of("&expansion-trace"),
            undefined: rtd_of("&undefined"),
            assertion: rtd_of("&assertion"),
            wrappers,
        }
    }
}
