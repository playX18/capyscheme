//! Scheme-visible record type and record constructor descriptor primitives.
//!
//! These are the native counterparts of `lib/boot/records.scm`. Record
//! instances are still tuples `[rtd, field...]`; only the descriptors are
//! native objects.

use crate::heap::{Gc, barrier};
use crate::prelude::*;
use crate::runtime::hooks::Hook;
use crate::runtime::objectmodel::{Closure, Tuple, Vector};
use crate::runtime::record::{Rcd, Rtd};

pub(crate) const RECORD_META_KIND: i32 = -0x5245;
pub(crate) const RECORD_META_RTD: i32 = -0x5246;
pub(crate) const RECORD_META_OFFSET: i32 = -0x5247;
pub(crate) const RECORD_META_DEPTH: i32 = -0x5248;

pub(crate) const RECORD_KIND_ACCESSOR: i32 = 1;
pub(crate) const RECORD_KIND_MUTATOR: i32 = 2;
pub(crate) const RECORD_KIND_PREDICATE: i32 = 3;
pub(crate) const RECORD_KIND_CONSTRUCTOR: i32 = 4;

/// The descriptor a record procedure's code block metadata carries.
#[derive(Clone, Copy, Debug)]
pub(crate) struct RecordOp<'gc> {
    pub kind: i32,
    pub rtd: Value<'gc>,
    pub offset: i32,
    /// Depth of `rtd` in its ancestry tuple. `u32::MAX` means "exact
    /// identity only"
    pub depth: i32,
}

pub(crate) fn record_op_from_meta<'gc>(meta: Value<'gc>) -> Option<RecordOp<'gc>> {
    let field = |key: i32| meta.assq(Value::from_i32(key)).map(|pair| pair.cdr());
    let kind = field(RECORD_META_KIND)?.int32()?;
    let rtd = field(RECORD_META_RTD)?;
    let offset = field(RECORD_META_OFFSET)?.int32()?;
    let depth = field(RECORD_META_DEPTH)?.int32()?;
    Some(RecordOp {
        kind,
        rtd,
        offset,
        depth,
    })
}

fn record_rtd_of<'gc>(obj: Value<'gc>) -> Option<Gc<'gc, Rtd<'gc>>> {
    if !obj.is::<Tuple>() {
        return None;
    }
    let tuple = obj.downcast::<Tuple>();
    if tuple.len() == 0 {
        return None;
    }
    let first = tuple[0].get();
    first.is::<Rtd>().then(|| first.downcast::<Rtd>())
}

fn captured<'gc>(call: &NativeCall<'_, 'gc>) -> Option<(Gc<'gc, Rtd<'gc>>, Option<usize>)> {
    // SAFETY: a native procedure is only entered through a closure, whose
    // rator slot holds it.
    let closure = unsafe { call.rator.downcast_unchecked::<Closure>() };
    if closure.nfree < 2 {
        return None;
    }
    let rtd = closure[1].get();
    if !rtd.is::<Rtd>() {
        return None;
    }
    let rtd = rtd.downcast::<Rtd>();
    let offset = if closure.nfree > 2 {
        Some(closure[2].get().int32()? as usize)
    } else {
        None
    };
    Some((rtd, offset))
}

fn raise_assertion<'gc>(
    call: &NativeCall<'_, 'gc>,
    who: Value<'gc>,
    message: &str,
    irritants: &[Value<'gc>],
) -> ReturnCode {
    let hook = call.ctx.hook(Hook::AssertionViolation);
    let message = Str::new(call.ctx, message, true).into();
    let mut args = Vec::with_capacity(2 + irritants.len());
    args.push(who);
    args.push(message);
    args.extend_from_slice(irritants);
    call.ctx.stage_tail_call(hook, &args)
}

fn wrong_arity<'gc>(call: &NativeCall<'_, 'gc>, who: &str, expected: usize) -> ReturnCode {
    let who = Value::new(Symbol::from_str(call.ctx, who));
    let message = format!("expected {expected} argument(s), got {}", call.args.len());
    let irritant = Value::new(call.args.len() as i32);
    raise_assertion(call, who, &message, &[irritant])
}

extern "C" fn record_accessor_proc<'gc>(ctx: MutationContext<'gc>, sfp: Slot<'gc>) -> ReturnCode {
    NativeCall::invoke(ctx, sfp, |call| {
        let Some((rtd, Some(offset))) = captured(call) else {
            return raise_assertion(
                call,
                Value::new(Symbol::from_str(ctx, "record-accessor")),
                "malformed record accessor",
                &[],
            );
        };
        if call.args.len() != 1 {
            return wrong_arity(call, "record-accessor", 1);
        }
        let obj = call.args[0];
        let Some(obj_rtd) = record_rtd_of(obj) else {
            return raise_assertion(
                call,
                Value::new(Symbol::from_str(ctx, "record-accessor")),
                "expected a record",
                &[obj],
            );
        };
        if Gc::ptr_eq(obj_rtd, rtd) || obj_rtd.is_subtype_of(rtd) {
            let tuple = obj.downcast::<Tuple>();
            if offset < tuple.len() {
                return call.return_one(tuple[offset].get());
            }
        }
        let who = Str::new(ctx, "record accessor", true).into();
        raise_assertion(call, who, "invalid record type", &[obj, rtd.into()])
    })
}

extern "C" fn record_mutator_proc<'gc>(ctx: MutationContext<'gc>, sfp: Slot<'gc>) -> ReturnCode {
    NativeCall::invoke(ctx, sfp, |call| {
        let Some((rtd, Some(offset))) = captured(call) else {
            return raise_assertion(
                call,
                Value::new(Symbol::from_str(ctx, "record-mutator")),
                "malformed record mutator",
                &[],
            );
        };
        if call.args.len() != 2 {
            return wrong_arity(call, "record-mutator", 2);
        }
        let (obj, value) = (call.args[0], call.args[1]);
        let Some(obj_rtd) = record_rtd_of(obj) else {
            return raise_assertion(
                call,
                Str::new(ctx, "record mutator", true).into(),
                "expected a record",
                &[obj],
            );
        };
        if Gc::ptr_eq(obj_rtd, rtd) || obj_rtd.is_subtype_of(rtd) {
            let tuple = obj.downcast::<Tuple>();
            if offset < tuple.len() {
                Gc::write(ctx, tuple)[offset].unlock().set(value);
                return call.return_one(Value::undefined());
            }
        }
        let who = Str::new(ctx, "record mutator", true).into();
        raise_assertion(
            call,
            who,
            "expected a record of the accessor's type",
            &[obj, value],
        )
    })
}

extern "C" fn record_constructor_proc<'gc>(
    ctx: MutationContext<'gc>,
    sfp: Slot<'gc>,
) -> ReturnCode {
    NativeCall::invoke(ctx, sfp, |call| {
        let Some((rtd, Some(argc))) = captured(call) else {
            return raise_assertion(
                call,
                Value::new(Symbol::from_str(ctx, "record-constructor")),
                "malformed record constructor",
                &[],
            );
        };
        if call.args.len() != argc {
            return wrong_arity(call, "record-constructor", argc);
        }
        let tuple = Tuple::new(ctx, argc + 1, Value::from(rtd));
        for (index, arg) in call.args.iter().enumerate() {
            Gc::write(ctx, tuple)[index + 1].unlock().set(*arg);
        }
        call.return_one(tuple.into())
    })
}

extern "C" fn record_predicate_proc<'gc>(ctx: MutationContext<'gc>, sfp: Slot<'gc>) -> ReturnCode {
    NativeCall::invoke(ctx, sfp, |call| {
        let Some((rtd, _)) = captured(call) else {
            return raise_assertion(
                call,
                Value::new(Symbol::from_str(ctx, "record-predicate")),
                "malformed record predicate",
                &[],
            );
        };
        if call.args.len() != 1 {
            return wrong_arity(call, "record-predicate", 1);
        }
        let obj = call.args[0];
        let matches = record_rtd_of(obj)
            .is_some_and(|obj_rtd| Gc::ptr_eq(obj_rtd, rtd) || obj_rtd.is_subtype_of(rtd));
        call.return_one(Value::new(matches))
    })
}

/// The descriptor alist a record procedure's closure and code block carry.
fn record_meta<'gc>(
    ctx: MutationContext<'gc>,
    kind: i32,
    rtd: Value<'gc>,
    offset: i32,
    depth: i32,
    name: Value<'gc>,
) -> Value<'gc> {
    let entry = |key: i32, value: Value<'gc>| Value::cons(ctx, Value::from_i32(key), value);
    let mut meta = Value::null();
    meta = Value::cons(ctx, entry(RECORD_META_DEPTH, Value::from_i32(depth)), meta);
    meta = Value::cons(
        ctx,
        entry(RECORD_META_OFFSET, Value::from_i32(offset)),
        meta,
    );
    meta = Value::cons(ctx, entry(RECORD_META_RTD, rtd), meta);
    meta = Value::cons(ctx, entry(RECORD_META_KIND, Value::from_i32(kind)), meta);
    meta = Value::cons(ctx, Value::cons(ctx, ctx.intern("name"), name), meta);
    meta
}

/// Raises an assertion violation attributed to `who`, the way the Scheme
/// `assertion-violation` helper does.
fn violation<'gc>(
    call: &NativeCall<'_, 'gc>,
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

    /// A record accessor closure for `(rtd, flat-offset)`.
    #[scheme(name = "%make-record-accessor")]
    pub fn make_record_accessor(rtd: Gc<'gc, Rtd<'gc>>, offset: i32) -> Value<'gc> {
        let depth = rtd.depth() as i32;
        let name = ctx.intern("record-accessor");
        let meta = record_meta(ctx, RECORD_KIND_ACCESSOR, rtd.into(), offset, depth, name);
        let closure = PROCEDURES.fetch(ctx).make_closure(
            ctx,
            record_accessor_proc,
            [rtd.into(), Value::from_i32(offset)],
            meta,
        );
        closure.into()
    }

    /// A record mutator closure
    #[scheme(name = "%make-record-mutator")]
    pub fn make_record_mutator(rtd: Gc<'gc, Rtd<'gc>>, offset: i32) -> Value<'gc> {
        let depth = rtd.depth() as i32;
        let name = ctx.intern("record-mutator");
        let meta = record_meta(ctx, RECORD_KIND_MUTATOR, rtd.into(), offset, depth, name);
        let closure = PROCEDURES.fetch(ctx).make_closure(
            ctx,
            record_mutator_proc,
            [rtd.into(), Value::from_i32(offset)],
            meta,
        );
        closure.into()
    }

    /// A record constructor closure taking exactly `argc` fields
    #[scheme(name = "%make-record-constructor")]
    pub fn make_record_constructor(rtd: Gc<'gc, Rtd<'gc>>, argc: i32) -> Value<'gc> {
        let depth = rtd.depth() as i32;
        let name = ctx.intern("record-constructor");
        let meta = record_meta(ctx, RECORD_KIND_CONSTRUCTOR, rtd.into(), argc, depth, name);
        let closure = PROCEDURES.fetch(ctx).make_closure(
            ctx,
            record_constructor_proc,
            [rtd.into(), Value::from_i32(argc)],
            meta,
        );
        closure.into()
    }

    /// A record predicate closure for `rtd`
    #[scheme(name = "%make-record-predicate")]
    pub fn make_record_predicate(rtd: Gc<'gc, Rtd<'gc>>) -> Value<'gc> {
        let depth = rtd.depth() as i32;
        let name = ctx.intern("record-predicate");
        let meta = record_meta(ctx, RECORD_KIND_PREDICATE, rtd.into(), 0, depth, name);
        let closure =
            PROCEDURES
                .fetch(ctx)
                .make_closure(ctx, record_predicate_proc, [rtd.into()], meta);
        closure.into()
    }
}
